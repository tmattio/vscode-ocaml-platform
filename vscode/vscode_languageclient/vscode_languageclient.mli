module RevealOutputChannelOn : sig
  type t =
    | Info
    | Warn
    | Error
    | Never

  val t_of_js : Ojs.t -> t

  val t_to_js : t -> Ojs.t
end

module ServerCapabilities : sig
  type t

  val experimental : t -> Jsonoo.t option

  val create : ?experimental:Jsonoo.t -> unit -> t

  (** {4 Converters} *)

  (** Get a [ServerCapabilities.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [ServerCapabilities.t]. *)
  val t_to_js : t -> Ojs.t
end

module InitializeResult : sig
  type t

  val capabilities : t -> ServerCapabilities.t

  type serverInfo =
    { name : string
    ; version : string option
    }

  val serverInfo : t -> serverInfo option

  (** {4 Converters} *)

  (** Get a [InitializeResult.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [InitializeResult.t]. *)
  val t_to_js : t -> Ojs.t
end

module DocumentFilter : sig
  type t

  val language : t -> string option

  val scheme : t -> string option

  val pattern : t -> string option

  val createLanguage :
    language:string -> ?scheme:string -> ?pattern:string -> unit -> t

  val createScheme :
    ?language:string -> scheme:string -> ?pattern:string -> unit -> t

  val createPattern :
    ?language:string -> ?scheme:string -> pattern:string -> unit -> t

  (** {4 Converters} *)

  (** Get a [DocumentFilter.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [DocumentFilter.t]. *)
  val t_to_js : t -> Ojs.t
end

module DocumentSelector : sig
  type t = selector array

  and selector =
    [ `Filter of DocumentFilter.t
    | `String of string
    ]

  val t_of_js : Ojs.t -> t

  val t_to_js : t -> Ojs.t

  val language : ?scheme:string -> ?pattern:string -> string -> selector
end

module ClientOptions : sig
  type t

  val documentSelector : t -> DocumentSelector.t option

  val outputChannel : t -> Vscode.OutputChannel.t option

  val revealOutputChannelOn : t -> RevealOutputChannelOn.t

  val create :
       ?documentSelector:DocumentSelector.t
    -> ?outputChannel:Vscode.OutputChannel.t
    -> ?revealOutputChannelOn:RevealOutputChannelOn.t
    -> unit
    -> t

  (** {4 Converters} *)

  (** Get a [ClientOptions.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [ClientOptions.t]. *)
  val t_to_js : t -> Ojs.t
end

module ExecutableOptions : sig
  type t

  val cwd : t -> string option

  val env : t -> string Interop.Dict.t option

  val detached : t -> bool option

  val shell : t -> bool option

  val create :
       ?cwd:string
    -> ?env:string Interop.Dict.t
    -> ?detached:bool
    -> ?shell:bool
    -> unit
    -> t

  (** {4 Converters} *)

  (** Get a [ExecutableOptions.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [ExecutableOptions.t]. *)
  val t_to_js : t -> Ojs.t
end

module Executable : sig
  type t

  val command : t -> string

  val args : t -> string list option

  val options : t -> ExecutableOptions.t option

  val create :
       command:string
    -> ?args:string list
    -> ?options:ExecutableOptions.t
    -> unit
    -> t

  (** {4 Converters} *)

  (** Get a [Executable.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [Executable.t]. *)
  val t_to_js : t -> Ojs.t
end

module ServerOptions = Executable

module LanguageClient : sig
  type t

  val make :
       id:string
    -> name:string
    -> serverOptions:ServerOptions.t
    -> clientOptions:ClientOptions.t
    -> ?forceDebug:bool
    -> unit
    -> t

  val start : t -> unit

  val stop : t -> unit

  val onReady : t -> Promise.void

  val initializeResult : t -> InitializeResult.t

  val readyInitializeResult : t -> InitializeResult.t Promise.t

  val sendRequest :
       t
    -> meth:string
    -> data:Jsonoo.t
    -> ?token:Vscode.CancellationToken.t
    -> unit
    -> Jsonoo.t Promise.t

  (** {4 Converters} *)

  (** Get a [LanguageClient.t] from a JavaScript object. *)
  val t_of_js : Ojs.t -> t

  (** Get a JavaScript object from a [LanguageClient.t]. *)
  val t_to_js : t -> Ojs.t
end
