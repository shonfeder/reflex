open Base

open! Dev
(** FIXME *)

module Dict = struct
  let actualize _ctxt = raise (Failure "TODO")
end

module Spect = struct
  module Store = Irmin_unix.Git.FS.KV (Project)

  let config = Irmin_git.config ~bare:true "/tmp/reflex/test"

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
    let+ result = Store.set store ~info path [] in
    lift_write_result result

  let add_speculation store path spec =
    let open Lwt.Syntax in
    let info = info "Adding a new speculation" in
    let* result = Store.find store path in
    let proj = Option.value result ~default:[] in
    match Project.add_speculation proj spec with
    | Error e -> Lwt.return (Error e)
    | Ok proj ->
        let+ result = Store.set store ~info path proj in
        lift_write_result result

  let add_remark store path topic remark =
    let open Lwt.Syntax in
    let* proj = Store.find store path in
    match proj with
    | None      -> Lwt.return (Error (`Remark_not_found_at_path path))
    | Some proj -> (
        let info = info "Adding new remark to speculation topic %s" topic in
        match Project.add_remark proj topic remark with
        | Error err -> Lwt_result.fail err
        | Ok proj   ->
            let+ result = Store.set store ~info path proj in
            lift_write_result result )

  let load path =
    let open Lwt.Syntax in
    let* repo = Irmin_git.config ~bare:true path |> Store.Repo.v in
    let+ master = Store.master repo in
    Ok master

  let actualize _ctxt = raise (Failure "TODO")
end
