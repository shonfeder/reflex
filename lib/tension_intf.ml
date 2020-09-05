type err =
  [ Rresult.R.msg
  | `Unsupported of Action.t
  ]

type act = Context.t -> Action.t -> (unit, err) Result.t

let unsupported action = Error (`Unsupported action)

module type S = sig
  (* type t *)

  (* val of_string : string -> t *)

  val name : string

  val description : string

  val act : act
end
