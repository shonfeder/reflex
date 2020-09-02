open Base

module Action = struct
  type t =
    | Dict
    | Spect
  [@@deriving sexp]

  let of_string : string -> t =
   fun s -> Sexplib.Std.sexp_of_string s |> t_of_sexp

  let describe = function
    | Dict  -> ""
    | Spect -> "a survey or review of a past course of events or period of time"
end

module Tension = struct
  type act = Action.t -> (unit, Rresult.R.msg) Result.t

  module type T = sig
    (* type t *)

    (* val of_string : string -> t *)

    val name : string

    val description : string

    val act : act
  end

  module Retro : T = struct
    (* type t = Act of Action.t [@@deriving sexp]
     *
     * let of_string : string -> t =
     *  fun s -> Sexplib.Std.sexp_of_string s |> t_of_sexp *)

    let name = "retro"

    let description = "towards the back"

    let act : act = function
      | Dict  -> Ok (Retro.Dict.actualize ())
      | Spect -> Ok (Retro.Spect.actualize ())
  end

  (* TODO Implement *)
  (* module Pro
   * module Intro
   * module Pre
   * module Post
   * module Inter *)
end
