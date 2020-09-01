open Reflex

let author = "Shon Feder"

let test_path = [ "reflex"; "retro" ]

let _ =
  (*FIXME: Handle error *)
  let open Lwt_result.Syntax in
  Lwt_main.run
  @@ let* store = Retro.Spect.load "/tmp/reflex/test" in
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
           "We should celebrate the postive outcome, but note the factors \
            causing it to be slower than expected"
     in
     let* () = Retro.Spect.add_remark store' test_path topic remark' in
     let info = Retro.Spect.info "Merging test_branch into master" in
     Retro.Spect.Store.merge_into ~info ~into:store store'
     |> Lwt.map Retro.Spect.lift_write_result
