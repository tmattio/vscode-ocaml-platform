let items = []

let getTreeItem ~element = Promise.return element

let getChildren ~element =
  match element with
  | None -> `Promise (Promise.return (Some items))
  | Some _ -> `Promise (Promise.return (Some []))

let register () =
  let treeDataProvider =
    Vscode.TreeDataProvider.create ~getTreeItem ~getChildren ()
  in
  let _ =
    Vscode.Window.registerTreeDataProvider ~viewId:"ocaml-commands"
      ~treeDataProvider
  in
  Promise.return ()
