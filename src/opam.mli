module Switch : sig
  type t =
    | Local of Path.t
    | Named of string

  val of_string : string -> t option

  val name : t -> string

  val equal : t -> t -> bool
end

module Package : sig
  type t

  val name : t -> string
end

type t

val make : unit -> t option Promise.t

val switch_list : t -> Switch.t list Promise.t

val switch_show : ?cwd:Path.t -> t -> Switch.t option Promise.t

val switch_create : t -> name:string -> args:string list -> Cmd.t

val exec : t -> switch:Switch.t -> args:string list -> Cmd.t

val install : t -> switch:Switch.t -> args:string list -> Cmd.t

val exists : t -> switch:Switch.t -> bool Promise.t

val equal : t -> t -> bool

val get_switch_packages : Switch.t -> (Package.t list, string) result Promise.t
