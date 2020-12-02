(** Manages OCaml Platform toolchain

    The toolchain is installed in a private sandbox, managed by VSCode only,
    where the tools we depend on are installed automatically.

    The sandbox uses Opam, but it does not conflict with users who use Esy as
    their package managers, since the tools are separated from the project. *)

type t

val setup : unit -> t option Promise.t

(* Helper utils *)

(** Extract command to run with the sandbox *)
val get_command : ?args:string list -> t -> string -> Cmd.t

(** Extract lsp command and arguments *)
val get_lsp_command : ?args:string list -> t -> Cmd.t

(** Extract a dune command *)
val get_dune_command : t -> string list -> Cmd.t

(** Extract a ocamlformat command and arguments *)
val get_ocamlformat_command : ?args:string list -> t -> Cmd.t

(** Extract a ocamlc command and arguments *)
val get_ocamlc_command : ?args:string list -> t -> Cmd.t
