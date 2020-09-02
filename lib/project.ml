open Base
module Assoc = List.Assoc
open Dev

let find = Assoc.find ~equal:String.equal

let add = Assoc.add ~equal:String.equal

type t = (Topic.t * Speculation.t) list [@@deriving irmin]

let ( let* ), ( let+ ) = Irmin.Merge.Infix.(( >>=* ), ( >|=* ))

let merge_or_add t ((topic : Topic.t), (s : Speculation.t)) =
  let equal = String.equal in
  let f t =
    match find t topic with
    | None    -> Irmin.Merge.ok (Assoc.add t ~equal topic s)
    | Some s' ->
        let* s'' = Speculation.merge s s' in
        Irmin.Merge.ok (add t s.topic s'')
  in
  let* t' = t in
  f t'

let merge ~old t1 t2 =
  let* old = old () in
  let old = Option.value old ~default:[] |> Irmin.Merge.ok in
  let t = List.fold ~init:old ~f:merge_or_add t1 in
  List.fold ~init:t ~f:merge_or_add t2

let merge = Irmin.Merge.(option (v t merge))

let add_remark t topic remark =
  match find t topic with
  | None   -> Error (`Invalid_topic topic)
  | Some s -> Ok (add t topic (Speculation.add_remark s remark))

let add_speculation t (spec : Speculation.t) =
  match find t spec.topic with
  | None   -> Ok (add t spec.topic spec)
  | Some s -> Error (`Speculation_already_exists s)
