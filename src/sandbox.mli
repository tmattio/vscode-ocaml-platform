(** Sandbox.ml exposes functions that let us

    1. Run initial checks in the environment looking for a reliable sandbox
    ([Sandbox.init])

    2. Run a setup that would setup the sandbox provided that basic requirements
    are met ([Sandbox.run_setup])

    3. Helper functions that extract the tools from the setup sandbox. This
    includes just [ocamllsp] right now, but in future could include others like
    debuggers, REPLs etc that could be shipped with [vscode-ocaml-platform]
    plugin itself

    The separation between [init], [run_setup] and extraction helpers exist so
    that we can handle missing tools gracefully (ie provide degraded
    performance, direct user to install missing tools etc). Having a single
    [Sandbox.make()], for instance, would not make it this flexible. *)

type t =
  | Opam of Opam.t * Opam.Switch.t
  | Esy of Esy.t * Path.t
  | Global
  | Custom of string

val equal : t -> t -> bool

val to_string : t -> string

val to_pretty_string : t -> string

val of_settings : unit -> t option Promise.t

val detect : unit -> t option Promise.t

val of_settings_or_detect : unit -> t option Promise.t

val save_to_settings : t -> unit Promise.t

(** [select_sandbox_and_save] requires the process environment the plugin is
    being run in (ie VSCode's process environment) and the project root and
    produces a promise of resources available that can later be passed on to
    [run_setup] that can be called to install the sandbox. *)
val select_sandbox_and_save : unit -> t option Promise.t

(** [select_sandbox] is the same as [select_sandbox_and_save] but does not save
    the sandbox configuration *)
val select_sandbox : unit -> t option Promise.t

(* Helper utils *)

val has_command : t -> string -> bool Promise.t

(** Extract command to run with the sandbox *)
val get_command : t -> string -> string list -> Cmd.t

(** Command to install dependencies in the sandbox *)
val get_install_command : t -> string list -> Cmd.t option

(** Command to exec a process in the sandbox *)
val get_exec_command : t -> string list -> Cmd.t option
