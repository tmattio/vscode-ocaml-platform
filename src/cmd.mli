(** Interface for running system commands *)

open Import

type shell = string

type spawn =
  { bin : Path.t
  ; args : string list
  }

type t =
  | Shell of shell
  | Spawn of spawn

type stdout = string

type stderr = string

val append : spawn -> string list -> spawn

val check_spawn : spawn -> spawn Or_error.t Promise.t

val check : t -> t Or_error.t Promise.t

val log : ?result:ChildProcess.return -> t -> unit

val output :
     ?cwd:Path.t
  -> ?env:string Interop.Dict.t
  -> ?stdin:string
  -> t
  -> (stdout, stderr) result Promise.t

val equal_spawn : spawn -> spawn -> bool

val run :
     ?cwd:Path.t
  -> ?env:string Interop.Dict.t
  -> ?stdin:stderr
  -> t
  -> ChildProcess.return Promise.t
