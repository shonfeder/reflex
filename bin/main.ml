open Lib

(* FIXME *)
open Dev

let author = "Shon Feder"

let test_path = [ "prototyping" ]

let test (context : Context.t) =
  (*FIXME: Handle error *)
  let open Lwt_result.Syntax in
  let store = context.store in
  let topic = "Implemented basic read/write via Irmin library" in
  let spec = Note.v ~topic ~dynamic:Dynamic.Celebration ~author () in
  let* () = Retro.Spect.add_speculation store test_path spec in
  let* store' =
    Context.Store.clone ~src:store ~dst:"test_branch" |> Lwt_result.ok
  in
  let remark =
    Remark.v ~author
      ~content:"This is good, but it's not as much progress as I'd hoped."
  in
  let* () = Retro.Spect.add_remark store test_path topic remark in
  let remark' =
    Remark.v ~author
      ~content:
        "We should celebrate the postive outcome, but note the factors causing \
         it to be slower than expected"
  in
  let* () = Retro.Spect.add_remark store' test_path topic remark' in
  let info = Retro.Spect.info "Merging test_branch into master" in
  Context.Store.merge_into ~info ~into:store store'
  |> Lwt.map Retro.Spect.lift_write_result

open Cmdliner

let action_conv = Arg.enum Action.name_map

let tension_cmd (module T : Tension.S) =
  let open T in
  Kwdcmd.(
    cmd ~name ~doc:description
      (Term.const T.act $ Required.pos "ACTION" ~conv:action_conv ~nth:0 ()))

let retrodict () = raise (Failure "TODO")

let cmds = [ tension_cmd (module Tension.Retro) ]

let handle_result result =
  let report_err err =
    match err with
    | `Conflict msg -> Printf.eprintf "conlict: %s\n" msg
    | `Invalid_topic msg -> Printf.eprintf "invalid topic: %s\n" msg
    | `Remark_not_found_at_path path ->
      Printf.eprintf "remark not found at path: %s\n" (String.concat "/" path)
    | `Speculation_already_exists Note.{ topic; _ } ->
      Printf.eprintf "speculation exists: %s\n" topic
    | `Unknown -> Printf.eprintf "unknown\n"
  in
  match result with
  | Ok () -> ()
  | Error err ->
    report_err err;
    exit 3

let main () =
  let open Lwt_result.Syntax in
  let* context = Context.load "/tmp/reflex/test" in
  let+ () = test context in
  Kwdcmd.Exec.select ~name:"reflex" cmds

let () = Lwt_main.run (main ()) |> handle_result
