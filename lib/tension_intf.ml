type act = Action.t -> (unit, Rresult.R.msg) Result.t

module type S = sig
  (* type t *)

  (* val of_string : string -> t *)

  val name : string

  val description : string

  val act : act
end
