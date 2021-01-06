open Import

type t = Cmd.spawn

let opam_binary = Path.of_string "opam"

let ocaml_env_binary = Path.of_string "ocaml-env"

let ocaml_env_setting =
  Settings.create ~scope:Global ~key:"ocaml.useOcamlEnv"
    ~of_json:Jsonoo.Decode.bool ~to_json:Jsonoo.Encode.bool

let make () =
  let spawn =
    match (Platform.t, Settings.get ocaml_env_setting) with
    | Win32, Some true ->
      { Cmd.bin = ocaml_env_binary; args = [ "exec"; "--"; "opam" ] }
    | _ -> { Cmd.bin = opam_binary; args = [] }
  in
  let open Promise.Syntax in
  let+ spawn = Cmd.check_spawn spawn in
  match spawn with
  | Error _ -> None
  | Ok cmd -> Some cmd

module Opam_parser = struct
  let rec string name = function
    | [] -> None
    | OpamParserTypes.Variable (_, s, String (_, v)) :: _
      when String.equal s name ->
      Some v
    | _ :: rest -> string name rest

  let rec list name = function
    | [] -> None
    | OpamParserTypes.Variable (_, s, List (_, l)) :: _ when String.equal s name
      ->
      let rec aux acc = function
        | [] -> acc |> List.rev
        | OpamParserTypes.String (_, v) :: rest -> aux (v :: acc) rest
        | _ -> assert false
      in
      Some (aux [] l)
    | _ :: rest -> list name rest
end

module Switch = struct
  type t =
    | Local of Path.t  (** if switch name is directory name where it's stored *)
    | Named of string  (** if switch is stored in ~/.opam *)

  let of_string = function
    | "" -> None
    | switch_name ->
      let switch_name = String.strip switch_name in
      if Char.equal switch_name.[0] '/' then
        Some (Local (Path.of_string switch_name))
      else
        Some (Named switch_name)

  let name = function
    | Named s -> s
    | Local p -> Path.to_string p

  let path opam t =
    let open Promise.Result.Syntax in
    match t with
    | Local p -> Promise.return (Ok Path.(p / "_opam"))
    | Named n ->
      let cmd = Cmd.Spawn (Cmd.append opam [ "var"; "root" ]) in
      let+ output = Cmd.output cmd in
      let root = String.strip output |> Path.of_string in
      Path.(root / n)

  let equal x y =
    match (x, y) with
    | Local x, Local y -> Path.equal x y
    | Named x, Named y -> String.equal x y
    | _, _ -> false
end

module Switch_state : sig
  type opam = t

  type t

  val of_switch : opam -> Switch.t -> (t, string) result Promise.t

  val compilers : t -> string list option

  val root : t -> string list option

  val installed : t -> string list option
end = struct
  type opam = t

  type t = OpamParserTypes.opamfile_item list

  let of_switch opam switch =
    let open Promise.Result.Syntax in
    let* path = Switch.path opam switch in
    let switch_state_filepath =
      Path.(path / ".opam-switch" / "switch-state") |> Path.to_string
    in
    let+ file_content =
      Fs.readFile switch_state_filepath |> Promise.map Result.return
    in
    let OpamParserTypes.{ file_contents; _ } =
      OpamParser.FullPos.string file_content switch_state_filepath
      |> OpamParser.FullPos.to_opamfile
    in
    file_contents

  let compilers = Opam_parser.list "compiler"

  let root = Opam_parser.list "roots"

  let installed = Opam_parser.list "installed"
end

module Package = struct
  type t =
    { path : Path.t
    ; items : OpamParserTypes.opamfile_item list
    ; name : string
    ; version : string
    }

  let of_path path =
    match String.split (Path.basename path) ~on:'.' with
    | [] -> Promise.return None
    | name :: version_parts ->
      let open Promise.Syntax in
      (* TODO: check if file path exists *)
      let opam_filepath = Path.(path / "opam") |> Path.to_string in
      let+ file_content = Fs.readFile opam_filepath in
      let OpamParserTypes.{ file_contents; _ } =
        OpamParser.FullPos.string file_content opam_filepath
        |> OpamParser.FullPos.to_opamfile
      in
      let version = String.concat version_parts ~sep:"." in
      Some { path; name; version; items = file_contents }

  let make ~name ~version ~opam_path =
    let path =
      Path.(opam_path / ".opam-switch" / "install" / (name ^ "." ^ version))
    in
    of_path path

  let path t = t.path

  let name t = t.name

  let version t = t.version

  let documentation t = Opam_parser.string "doc" t.items

  let synopsis t = Opam_parser.string "synopsis" t.items

  let depends t =
    let rec parser = function
      | [] -> None
      | OpamParserTypes.Variable (_, s, List (_, l)) :: _
        when String.equal s "depends" ->
        let rec aux acc = function
          | [] -> acc |> List.rev
          | OpamParserTypes.String (_, v) :: rest -> aux (v :: acc) rest
          | _ :: rest -> aux acc rest
        in
        Some (aux [] l)
      | _ :: rest -> parser rest
    in
    parser t.items
end

let switch_arg switch = "--switch=" ^ Switch.name switch

let exec t switch args =
  Cmd.Spawn (Cmd.append t ("exec" :: switch_arg switch :: "--" :: args))

let parse_switch_list out =
  let lines = String.split_on_chars ~on:[ '\n' ] out in
  let result = lines |> List.filter_map ~f:Switch.of_string in
  log "%d switches" (List.length result);
  result

let switch_list t =
  let command = Cmd.append t [ "switch"; "list"; "-s" ] in
  let open Promise.Syntax in
  let+ output = Cmd.output (Spawn command) in
  match output with
  | Error _ ->
    show_message `Warn "Unable to read the list of switches.";
    []
  | Ok out -> parse_switch_list out

let switch_exists t switch =
  let open Promise.Syntax in
  let+ switches = switch_list t in
  List.exists switches ~f:(Switch.equal switch)

let switch_path t switch = Switch.path t switch

let equal o1 o2 = Cmd.equal_spawn o1 o2

let switch_show ?cwd t =
  let command = Cmd.append t [ "switch"; "show" ] in
  let open Promise.Syntax in
  let+ output = Cmd.output ?cwd (Spawn command) in
  match output with
  | Ok out -> Switch.of_string out
  | Error _ ->
    show_message `Warn "Unable to read the current switch.";
    None

let switch_remove t switch =
  let name = Switch.name switch in
  Cmd.Spawn (Cmd.append t [ "switch"; "remove"; name; "-y" ])

let switch_compiler t switch =
  let open Promise.Syntax in
  let+ switch_state = Switch_state.of_switch t switch in
  match switch_state with
  | Error _ -> None
  | Ok switch_state ->
    let compilers = Switch_state.compilers switch_state in
    Option.bind compilers ~f:List.hd

let packages t switch =
  let open Promise.Result.Syntax in
  let* switch_state = Switch_state.of_switch t switch in
  match Switch_state.installed switch_state with
  | None ->
    Promise.return
      (Error "Could not get the installed packaged from the switch state")
  | Some l ->
    let* path = Switch.path t switch in
    Promise.List.filter_map
      (fun name ->
        let packages_path = Path.(path / ".opam-switch/" / "packages" / name) in
        Package.of_path packages_path)
      l
    |> Promise.map Result.return

let root_packages t switch =
  let open Promise.Result.Syntax in
  let* switch_state = Switch_state.of_switch t switch in
  match Switch_state.root switch_state with
  | None ->
    Promise.return
      (Error "Could not get the root packaged from the switch state")
  | Some l ->
    let* path = Switch.path t switch in
    Promise.List.filter_map
      (fun name ->
        let packages_path = Path.(path / ".opam-switch/" / "packages" / name) in
        Package.of_path packages_path)
      l
    |> Promise.map Result.return

let package_uninstall t switch package =
  exec t switch [ "uninstall"; Package.name package ]

let get_switch_package name ~package_path =
  let open Promise.Syntax in
  let* l = Node.Fs.readDir (Path.to_string package_path) in
  match l with
  | Error _ -> Promise.return None
  | Ok l ->
    Promise.List.find_map
      (fun fpath ->
        let basename = Filename.basename fpath in
        if String.is_prefix basename ~prefix:name then
          Package.of_path Path.(package_path / fpath)
        else
          Promise.return None)
      l

let package_dependencies package =
  match Package.depends package with
  | None ->
    Promise.return
      (Error "Could not get the root packaged from the switch state")
  | Some l ->
    Promise.List.filter_map
      (fun pkg ->
        let package_path =
          (* The package path is never the root, so it's safe to use [value_exn] *)
          Path.parent (Package.path package) |> fun x -> Option.value_exn x
        in
        get_switch_package pkg ~package_path)
      l
    |> Promise.map Result.return
