open Import

module Dependency = struct
  type t =
    | Switch of Opam.Switch.t
    | Dependency of string

  let sexp_of_t =
    let open Sexp in
    function
    | Switch (Named name) ->
      List [ Atom "switch"; List [ Atom "named"; Atom name ] ]
    | Switch (Local path) ->
      let name = Path.to_string path in
      List [ Atom "switch"; List [ Atom "local"; Atom name ] ]
    | Dependency dep -> List [ Atom "dependency"; Atom dep ]

  let t_of_sexp =
    let open Sexp in
    function
    | List [ Atom "switch"; List [ Atom "named"; Atom name ] ] ->
      Switch (Named name)
    | List [ Atom "switch"; List [ Atom "local"; Atom name ] ] ->
      let path = Path.of_string name in
      Switch (Local path)
    | List [ Atom "dependency"; Atom dep ] -> Dependency dep
    | _ -> assert false

  let to_string t = t |> sexp_of_t |> Sexp.to_string

  let of_string s = s |> Sexp.of_string |> t_of_sexp
end

let make_switch_item ~name ~extension_path =
  let icon =
    `LightDark
      TreeItem.
        { light = `String (extension_path ^ "/assets/dependency-light.svg")
        ; dark = `String (extension_path ^ "/assets/dependency-dark.svg")
        }
  in
  let label = Vscode.TreeItemLabel.create ~label:name () in
  let item = TreeItem.make ~label () in
  TreeItem.set_iconPath item icon;
  item

let getTreeItem ~element = Promise.return element

let get_dependencies _element = Some []

let register extension =
  let open Promise.Syntax in
  let extension_path = Vscode.ExtensionContext.extensionPath extension in
  let* opam = Opam.make () in
  match opam with
  | None -> Promise.return ()
  | Some opam ->
    let+ switches = Opam.switch_list opam in
    let items =
      List.filter_map switches ~f:(function
        | Opam.Switch.Named name
          when String.is_prefix name ~prefix:"vscode-ocaml-toolchain" ->
          None
        | Opam.Switch.Named name ->
          Some (make_switch_item ~name ~extension_path)
        | Opam.Switch.Local path ->
          let name = Path.to_string path in
          Some (make_switch_item ~name ~extension_path))
    in
    let getChildren ~element =
      match element with
      | None -> `Promise (Promise.return (Some items))
      | Some element -> (
        let item = TreeItem.t_of_js element in
        match get_dependencies item with
        | None -> `Promise (Promise.return None)
        | Some l ->
          `Promise (Promise.return (Some (List.map l ~f:TreeItem.t_to_js))) )
    in
    let treeDataProvider =
      Vscode.TreeDataProvider.create ~getTreeItem ~getChildren ()
    in
    let _ =
      Vscode.Window.registerTreeDataProvider ~viewId:"ocaml-packages"
        ~treeDataProvider
    in
    ()
