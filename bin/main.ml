open Lib

(* FIXME *)
open Dev

let author = "Shon Feder"

let test_path = [ "reflex"; "retro" ]

let test () =
  (*FIXME: Handle error *)
  let open Lwt_result.Syntax in
  let* store = Retro.Spect.load "/tmp/reflex/test" in
  let topic = "Implemented basic read/write via Irmin library" in
  let spec = Speculation.v ~topic ~dynamic:Dynamic.Celebration ~author () in
  let* () = Retro.Spect.add_speculation store test_path spec in
  let* store' =
    Retro.Spect.Store.clone ~src:store ~dst:"test_branch" |> Lwt_result.ok
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
  Retro.Spect.Store.merge_into ~info ~into:store store'
  |> Lwt.map Retro.Spect.lift_write_result

open Cmdliner

let action_conv =
  let open Element in
  Arg.enum [ ("dict", Action.Dict); ("spect", Action.Spect) ]

let tension_cmd (module T : Element.Tension.T) =
  let open T in
  Kwdcmd.(
    cmd ~name ~doc:description
      (Term.const T.act $ Required.pos "ACTION" ~conv:action_conv ~nth:0 ()))

let retrodict () = raise (Failure "TODO")

let cmds = [ tension_cmd (module Element.Tension.Retro) ]

let handle_result result =
  let report_err err =
    match err with
    | `Conflict msg -> Printf.eprintf "conlict: %s\n" msg
    | `Invalid_topic msg -> Printf.eprintf "invalid topic: %s\n" msg
    | `Remark_not_found_at_path path ->
        Printf.eprintf "remark not found at path: %s\n" (String.concat "/" path)
    | `Speculation_already_exists Speculation.{ topic; _ } ->
        Printf.eprintf "speculation exists: %s\n" topic
    | `Unknown -> Printf.eprintf "unknown\n"
  in
  match result with
  | Ok ()     -> ()
  | Error err ->
      report_err err;
      exit 3

let main () =
  let open Lwt.Syntax in
  let* result = test () in
  match result with
  | Error e -> Lwt_result.fail e
  | Ok ()   -> Lwt_result.return @@ Kwdcmd.Exec.select ~name:"reflex" cmds

let () = Lwt_main.run (main ()) |> handle_result
