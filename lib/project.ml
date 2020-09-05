open Base
module Assoc = List.Assoc
open Dev

let find = Assoc.find ~equal:String.equal

let add = Assoc.add ~equal:String.equal

type notes = (Topic.t * Note.t) list [@@deriving irmin]

type t = { notes : notes } [@@deriving irmin]

let empty = { notes = [] }

let ( let* ), ( let+ ) = Irmin.Merge.Infix.(( >>=* ), ( >|=* ))

let merge_or_add_notes notes ((topic : Topic.t), (s : Note.t)) =
  let equal = String.equal in
  let f t =
    match find t topic with
    | None -> Irmin.Merge.ok (Assoc.add t ~equal topic s)
    | Some s' ->
      let* s'' = Note.merge s s' in
      Irmin.Merge.ok (add t s.topic s'')
  in
  let* notes' = notes in
  f notes'

let merge_notes (old : notes) (t1 : notes) (t2 : notes) =
  let t = List.fold ~init:(Irmin.Merge.ok old) ~f:merge_or_add_notes t1 in
  List.fold ~init:t ~f:merge_or_add_notes t2

let merge ~old t1 t2 =
  let* old = old () in
  let old = Option.value old ~default:empty in
  let+ notes = merge_notes old.notes t1.notes t2.notes in
  { notes }

let merge = Irmin.Merge.(option (v t merge))

let add_remark (t : t) topic remark =
  match find t.notes topic with
  | None -> Error (`Invalid_topic topic)
  | Some s -> Ok { notes = add t.notes topic (Note.add_remark s remark) }

let add_speculation (t : t) (spec : Note.t) =
  match find t.notes spec.topic with
  | None -> Ok { notes = add t.notes spec.topic spec }
  | Some s -> Error (`Speculation_already_exists s)
