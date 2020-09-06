open Base
open Lib

(* FIXME *)
open Dev

let test_path = [ "prototyping" ]

let test Context.{ store; user = author } =
  (*FIXME: Handle error *)
  let open Lwt_result.Syntax in
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

let tension_cmd ctxt (module T : Tension.S) =
  let handle_err : Tension.err -> Rresult.R.msg = function
    | `Msg m              -> `Msg m
    | `Unsupported action ->
        let msg =
          Printf.sprintf "Action %s unsupported for tension %s"
            (Action.to_string action) T.name
        in
        `Msg msg
  in
  let action a data =
    let open Lwt.Syntax in
    let+ result = T.act ~ctxt ?data a in
    Result.map_error result ~f:handle_err
  in
  let open T in
  Kwdcmd.(
    cmd ~name ~doc:description
      ( Term.const action
      $ Required.pos "ACTION" ~conv:action_conv ~nth:0 ()
      $ Optional.pos "DATA" ~conv:Arg.string ~nth:1 () ))

let retrodict () = raise (Failure "TODO")

let cmds ctxt =
  [ tension_cmd ctxt (module Tension.Retro)
  ; tension_cmd ctxt (module Tension.Re)
  ]

let handle_result result =
  let report_err err =
    match err with
    | `Conflict msg -> Stdio.eprintf "conlict: %s\n" msg
    | `Invalid_topic msg -> Stdio.eprintf "invalid topic: %s\n" msg
    | `Remark_not_found_at_path path ->
        Stdio.eprintf "remark not found at path: %s\n"
          (String.concat ~sep:"/" path)
    | `Speculation_already_exists Note.{ topic; _ } ->
        Stdio.eprintf "speculation exists: %s\n" topic
    | `Unknown -> Stdio.eprintf "unknown\n"
  in
  match result with
  | Ok ()     -> ()
  | Error err ->
      Stdio.eprintf "[error] ";
      report_err err;
      Caml.exit 3

let main () =
  let open Lwt_result.Syntax in
  let* context = Context.load "/tmp/reflex/test" in
  let+ () = test context in
  Kwdcmd.Exec.select ~name:"reflex" (cmds context)

let () = Lwt_main.run (main ()) |> handle_result
