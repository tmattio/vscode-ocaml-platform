open Import

module Dependency = struct
  type t = Sandbox.Package.t

  let t_of_js : Ojs.t -> t = Obj.magic

  let t_to_js : t -> Ojs.t = Obj.magic

  let label t = Sandbox.Package.name t

  let description t = Promise.return (Some (Sandbox.Package.version t))

  let tooltip t = Sandbox.Package.synopsis t

  let context_value t =
    match Sandbox.Package.documentation t with
    | Some _ -> "package-with-doc"
    | None -> "package"

  let icon _ =
    TreeItem.LightDarkIcon.
      { light = `String (Node.__filename () ^ "/../../assets/number-light.svg")
      ; dark = `String (Node.__filename () ^ "/../../assets/number-dark.svg")
      }

  let collapsible_state t =
    match Sandbox.Package.depends t with
    | Some []
    | None ->
      TreeItemCollapsibleState.None
    | _ -> TreeItemCollapsibleState.Collapsed

  let to_treeitem dependency =
    let open Promise.Syntax in
    let icon = `LightDark (icon dependency) in
    let collapsibleState = collapsible_state dependency in
    let label =
      `TreeItemLabel (Vscode.TreeItemLabel.create ~label:(label dependency) ())
    in
    let item = Vscode.TreeItem.make_label ~label ~collapsibleState () in
    Vscode.TreeItem.set_iconPath item icon;
    TreeItem.set_contextValue item (context_value dependency);
    let+ _ =
      Promise.Option.iter
        (fun desc -> TreeItem.set_description item (`String desc))
        (description dependency)
    in
    Option.iter (tooltip dependency) ~f:(fun desc ->
        TreeItem.set_tooltip item (`String desc));
    item

  let get_dependencies t =
    let open Promise.Syntax in
    let+ deps = Sandbox.package_dependencies t in
    match deps with
    | Error _ -> None
    | Ok packages -> Some packages
end

module Command = struct
  let _open_documentation =
    let handler (_ : Extension_instance.t) ~args =
      let (_ : unit Promise.t) =
        let arg = List.hd_exn args in
        let dep = Dependency.t_of_js arg in
        let open Promise.Syntax in
        let doc = Sandbox.Package.documentation dep in
        match doc with
        | None -> Promise.return ()
        | Some doc ->
          let+ _ =
            Vscode.Commands.executeCommand
              (module Ojs)
              ~command:"vscode.open"
              ~args:[ Vscode.Uri.parse doc () |> Vscode.Uri.t_to_js ]
          in
          ()
      in
      ()
    in
    Extension_commands.register
      Extension_consts.Commands.open_sandbox_documentation handler
end

let getTreeItem ~element = `Promise (Dependency.to_treeitem element)

let getChildren ~instance ?element () =
  let sandbox = Extension_instance.sandbox instance in
  match element with
  | Some element -> `Promise (Dependency.get_dependencies element)
  | None ->
    let open Promise.Syntax in
    let items =
      let+ packages = Sandbox.root_packages sandbox in
      match packages with
      | Ok packages -> Some packages
      | Error _ -> None
    in
    `Promise items

let register extension instance =
  let getChildren = getChildren ~instance in
  let module EventEmitter =
    Vscode.EventEmitter.Make (Interop.Js.Or_undefined (Dependency)) in
  let event_emitter = EventEmitter.make () in
  let event = EventEmitter.event event_emitter in
  let module TreeDataProvider = Vscode.TreeDataProvider.Make (Dependency) in
  let treeDataProvider =
    TreeDataProvider.create ~getTreeItem ~getChildren ~onDidChangeTreeData:event
      ()
  in
  let disposable =
    Vscode.Window.registerTreeDataProvider
      (module Dependency)
      ~viewId:"ocaml-sandbox" ~treeDataProvider
  in
  ExtensionContext.subscribe extension ~disposable;

  let disposable =
    Commands.registerCommand ~command:Extension_consts.Commands.refresh_sandbox
      ~callback:(fun ~args:_ -> EventEmitter.fire event_emitter None)
  in
  ExtensionContext.subscribe extension ~disposable
