let items = []

let getTreeItem ~element = Promise.return element

let getChildren ~element =
  match element with
  | None -> `Promise (Promise.return (Some items))
  | Some _ -> `Promise (Promise.return (Some []))

let provider (_sandbox : Sandbox.t) =
  Vscode.TreeDataProvider.create ~getTreeItem ~getChildren ()
