open Base

(** FIXME *)
open! Dev

let name = "retro"

let description = "tending towards the back"

module Dict = struct
  let actualize _ctxt = raise (Failure "TODO")
end

module Spect = struct
  let info fmt =
    let author = Option.value (Sys.getenv "USER") ~default:"unknown user" in
    Irmin_unix.info ~author fmt

  let lift_write_result result =
    let f = function
      | `Conflict msg -> `Conflict msg
      | `Test_was _
      | `Too_many_retries _ ->
        `Unknown
    in
    Result.map_error ~f result

  let new_project store path =
    let open Lwt.Syntax in
    let info = info "Creating a new project" in
    let+ result = Context.Store.set store ~info path Project.empty in
    lift_write_result result

  let add_speculation store path spec =
    let open Lwt.Syntax in
    let info = info "Adding a new speculation" in
    let* result = Context.Store.find store path in
    let proj = Option.value result ~default:Project.empty in
    match Project.add_speculation proj spec with
    | Error e -> Lwt.return (Error e)
    | Ok proj ->
      let+ result = Context.Store.set store ~info path proj in
      lift_write_result result

  let add_remark store path topic remark =
    let open Lwt.Syntax in
    let* proj = Context.Store.find store path in
    match proj with
    | None -> Lwt.return (Error (`Remark_not_found_at_path path))
    | Some proj -> (
      let info = info "Adding new remark to speculation topic %s" topic in
      match Project.add_remark proj topic remark with
      | Error err -> Lwt_result.fail err
      | Ok proj ->
        let+ result = Context.Store.set store ~info path proj in
        lift_write_result result )

  let actualize _ctxt = raise (Failure "TODO")
end

let act : Tension_intf.act = function
  | Action.Dict -> Ok (Dict.actualize ())
  | Action.Spect -> Ok (Spect.actualize ())
  | a -> Tension_intf.unsupported a
