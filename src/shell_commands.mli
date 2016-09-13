module Arg : sig
  type 'a t

  val flag : ?doc:string -> string list -> bool t

  val opt : ?doc:string -> ?predict:(string -> string list) ->
    ?default:string -> string list -> string t

  val pos : ?doc:string -> ?predict:(string -> string list) ->
    ?default:string -> unit -> string t

  val pos_required : ?doc:string -> ?predict:(string -> string list) ->
    unit -> string t

  module Opt : sig
    type t
    val is_flag : t -> bool
  end
  module Pos : sig type t end
  type arg = [`Opt of Opt.t | `Pos of Pos.t]
  val doc : arg -> string
  val predict : string -> arg -> string list
end

module Command : sig
  type 'a t

  type full = unit Lwt.t t

  val name : 'a t -> string

  val rename : string -> 'a t -> 'a t

  val create : ?doc:string -> string -> 'a -> 'a t

  val ($) : ('a -> 'b) t -> 'a Arg.t -> 'b t

  val const : 'a -> 'a Arg.t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val eval : (Arg.arg * string) list -> 'a t -> 'a
  val doc : 'a t -> string
  val lookup_opt : string -> 'a t -> Arg.Opt.t option
  val lookup_pos : int -> 'a t -> Arg.Pos.t option
  val opt_bindings : 'a t -> (string * Arg.Opt.t) list
  val should_exit : 'a t -> bool
end

module Map : sig
  type t

  val empty : t

  val add : Command.full -> t -> t

  val get : string -> t -> Command.full option
  val bindings : t -> (string * Command.full) list
end
