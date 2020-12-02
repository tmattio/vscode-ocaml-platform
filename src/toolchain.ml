open Import

type t = Opam.t * Opam.Switch.t

let get_command ?(args = []) (opam, switch) bin =
  Opam.exec opam ~switch ~args:(bin :: args)

module Tool = struct
  let lsp_command ?args toolchain : Cmd.t =
    get_command toolchain "ocamllsp" ?args

  let dune_command toolchain args : Cmd.t = get_command toolchain "dune" ~args

  let ocamlformat_command ?args toolchain : Cmd.t =
    get_command toolchain "ocamlformat" ?args

  let ocamlc_command ?args toolchain : Cmd.t =
    get_command toolchain "ocamlc" ?args

  let all toolchain =
    [ ("ocaml-base-compiler", ocamlc_command toolchain ~args:[ "--version" ])
    ; ("ocaml-lsp-server", lsp_command toolchain ~args:[ "--version" ])
    ; ("ocamlformat", ocamlformat_command toolchain ~args:[ "--version" ])
    ]

  let missing_command_opt binary cmd =
    let open Promise.Syntax in
    let+ output =
      let open Promise.Result.Syntax in
      let* command = Cmd.check cmd in
      Cmd.output command
    in
    match output with
    | Ok _ -> None
    | Error _ -> Some binary

  let detect_missing toolchain =
    all toolchain
    |> List.map ~f:(fun (binary, cmd) -> missing_command_opt binary cmd)
    |> Promise.all_list
    |> Promise.map List.filter_opt
end

let sandbox_name = "vscode-ocaml-toolchain"

let sandbox_opt =
  let open Promise.Option.Syntax in
  let* opam = Opam.make () in
  let+ switch = Opam.Switch.of_string sandbox_name |> Promise.return in
  (opam, switch)

let is_sandbox_installed (opam, switch) =
  let open Promise.Syntax in
  let cmd = Opam.exec opam ~switch ~args:[ "true" ] in
  let+ result = Cmd.output cmd in
  match result with
  | Ok _ -> true
  | Error _ -> false

let setup_toolchain_sandbox () =
  let open Promise.Syntax in
  let* opam_opt = Opam.make () in
  match opam_opt with
  | None -> Promise.return (Error `Opam_not_available)
  | Some opam -> (
    let+ result =
      Opam.switch_create opam ~name:sandbox_name
        ~args:[ "ocaml-base-compiler.4.11.1" ]
      |> Cmd.output
    in
    match result with
    | Ok _ -> Ok ()
    | Error err -> Error (`Switch_create_failed err) )

let get_install_command (opam, switch) tools =
  Opam.install opam ~switch ~args:("-y" :: tools)

let _install_tools_with_cancel ~progress:_ ~token sandbox tools =
  Promise.make @@ fun ~resolve ~reject ->
  let cmd = get_install_command sandbox tools in
  let child_process = Cmd.run cmd in
  let _ =
    (CancellationToken.onCancellationRequested token) ~listener:(fun _ ->
        let _ = ChildProcess.kill child_process "SIGTERM" in
        ())
  in
  ChildProcess.on child_process "close" (fun _ ->
      show_message `Info "The platform tools have been successfully installed";
      resolve ());
  ChildProcess.on child_process "error" (fun err ->
      show_message `Error "The installation of the Platform tools failed";
      reject err);
  ()

let install_tools ~progress:_ ~token:_ t tools =
  let open Promise.Syntax in
  let* is_installed = is_sandbox_installed t in
  let* _ =
    match is_installed with
    | false -> setup_toolchain_sandbox ()
    | true -> Promise.return (Ok ())
  in
  let cmd = get_install_command t tools in
  let+ result = Cmd.output cmd in
  match result with
  | Ok _ ->
    show_message `Info "The platform tools have been successfully installed";
    Some t
  | Error err ->
    show_message `Error "The installation of the Platform tools failed: %s" err;
    None

let install_missing_tools t tools =
  let open Promise.Syntax in
  match tools with
  | [] -> Promise.return (Some t)
  | tools -> (
    let select_pm_button_text = "Install toolchain" in
    let missing_str = String.concat ~sep:", " tools in
    let* selection =
      Window.showInformationMessage
        ~message:
          (Printf.sprintf
             "OCaml Platform toolchain is not installed. Do you want to \
              install it?\n\
              Missing: %s" missing_str)
        ~choices:[ (select_pm_button_text, ()) ]
        ()
    in
    let progress_options =
      ProgressOptions.create ~location:(`ProgressLocation Window)
        ~title:"Installing Platform Tools" ~cancellable:false ()
    in
    let task = install_tools t tools in
    match selection with
    | Some () -> Window.withProgress ~options:progress_options ~task
    | None ->
      (* We should remove that *)
      show_message `Error "Some tools are missing, won't install";
      Promise.return None )

let setup () =
  let open Promise.Syntax in
  let* sandbox_opt = sandbox_opt in
  match sandbox_opt with
  | None ->
    show_message `Error
      "Opam is required to install the toolchain. Install it manually to use \
       the extension.";
    Promise.return None
  | Some t ->
    let* is_installed = is_sandbox_installed t in
    let* missing_tools =
      match is_installed with
      | false -> Tool.all t |> List.map ~f:fst |> Promise.return
      | true -> Tool.detect_missing t
    in
    install_missing_tools t missing_tools

let get_lsp_command = Tool.lsp_command

let get_dune_command = Tool.dune_command

let get_ocamlformat_command = Tool.ocamlformat_command

let get_ocamlc_command = Tool.ocamlc_command
