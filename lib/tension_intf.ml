type err =
  [ Rresult.R.msg
  | `Unsupported of Action.t
  ]

type act =
  ctxt:Context.t -> ?data:string -> Action.t -> (unit, err) Lwt_result.t

let unsupported action = Lwt_result.fail (`Unsupported action)

module type S = sig
  (* type t *)

  (* val of_string : string -> t *)

  val name : string

  val description : string

  val act : act
end
