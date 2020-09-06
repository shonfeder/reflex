open Base

(* FIXME *)
open Dev

let name = "re"

let description = "tending back to the original place; again, anew, once more"

module Mark = struct
  module Data = struct
    type t =
      { project : string list
      ; topic : string
      ; dynamic : Dynamic.t [@default Dynamic.Problem]
      ; content : string [@default ""]
      }
    [@@deriving sexp]

    let of_string s =
      match Sexplib.Sexp.of_string_conv s t_of_sexp with
      | `Error parse_err -> Error (`Invalid_data parse_err)
      | `Result r        -> Ok r
  end

  let info Context.{ user; _ } fmt = Irmin_unix.info ~author:user fmt

  let lift_write_result result =
    let f = function
      | `Conflict msg -> `Msg msg
      | `Test_was _
      | `Too_many_retries _ ->
          `Msg "unkonwn error"
    in
    Result.map_error ~f result

  let actualize (ctxt : Context.t) data =
    let open Lwt.Syntax in
    let+ result =
      match Data.of_string data with
      | Error e -> Lwt.return (Error e)
      | Ok { project; topic; dynamic; content } -> (
          let author = ctxt.user in
          let remark = Remark.v ~author ~content in
          let note = Note.v ~remark ~topic ~dynamic ~author () in
          let* proj =
            Context.Store.find ctxt.store project
            |> Lwt.map (Option.value ~default:Project.empty)
          in
          match Project.add_speculation proj note with
          | Error e -> Lwt.return (Error e)
          | Ok proj ->
              let info = info ctxt "Adding a new remark" in
              let+ result = Context.Store.set ctxt.store ~info project proj in
              lift_write_result result )
    in
    Result.map_error result ~f:(function
      | `Msg m -> `Msg m
      | `Speculation_already_exists _ -> `Msg "TODO note exists error"
      | `Invalid_data _ -> `Msg "TODO data parse  error")
end

(* TODO *)
(* module Ply = struct end *)

let act : Tension_intf.act =
 fun ~ctxt ?(data = "") -> function
  | Mark  -> Mark.actualize ctxt data
  | Spect -> raise (Failure "TODO respect")
  | a     -> Tension_intf.unsupported a
