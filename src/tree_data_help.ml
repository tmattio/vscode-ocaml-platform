let discord_item ~extension_path =
  let icon =
    `LightDark
      Vscode.TreeItem.
        { light = `String (extension_path ^ "/assets/discord-light.svg")
        ; dark = `String (extension_path ^ "/assets/discord-dark.svg")
        }
  in
  let label = Vscode.TreeItemLabel.create ~label:"Chat on Discord" () in
  let item = Vscode.TreeItem.make ~label () in
  let command =
    Vscode.Command.create ~title:"Open" ~command:"vscode.open"
      ~arguments:
        [ Vscode.Uri.parse "https://github.com/ocamllabs/vscode-ocaml-platform"
            ()
          |> Vscode.Uri.t_to_js
        ]
      ()
  in
  Vscode.TreeItem.set_iconPath item icon;
  Vscode.TreeItem.set_command item command;
  item

let github_item ~extension_path =
  let icon =
    `LightDark
      Vscode.TreeItem.
        { light = `String (extension_path ^ "/assets/github-light.svg")
        ; dark = `String (extension_path ^ "/assets/github-dark.svg")
        }
  in
  let label = Vscode.TreeItemLabel.create ~label:"OCaml on Github" () in
  let item = Vscode.TreeItem.make ~label () in
  let command =
    Vscode.Command.create ~title:"Open" ~command:"vscode.open"
      ~arguments:
        [ Vscode.Uri.parse "https://github.com/ocamllabs/vscode-ocaml-platform"
            ()
          |> Vscode.Uri.t_to_js
        ]
      ()
  in
  Vscode.TreeItem.set_iconPath item icon;
  Vscode.TreeItem.set_command item command;
  item

let items ~extension_path =
  [ discord_item ~extension_path; github_item ~extension_path ]

let getTreeItem ~extension_path:_ ~element = Promise.return element

let getChildren ~extension_path ~element =
  match element with
  | None -> `Promise (Promise.return (Some (items ~extension_path)))
  | Some _ -> `Promise (Promise.return (Some []))

let provider extension =
  let extension_path = Vscode.ExtensionContext.extensionPath extension in
  Vscode.TreeDataProvider.create
    ~getTreeItem:(getTreeItem ~extension_path)
    ~getChildren:(getChildren ~extension_path)
    ()
