open Base

(* FIXME *)
open Dev

type t =
  { topic : Topic.t
  ; dynamic : Dynamic.t
  ; provenance : Provenance.t
  ; remarks : Remark.t list
  ; support : int
  }
[@@deriving irmin, eq]

let compare t t' =
  let provenance_cmp = Provenance.compare t.provenance t'.provenance in
  if provenance_cmp = 0 then
    String.compare t.topic t'.topic
  else
    provenance_cmp

let add_remark t remark =
  let remarks = remark :: t.remarks |> List.sort ~compare:Remark.compare in
  { t with remarks }

let v ?remark ~topic ~dynamic ~author () =
  let provenance = Provenance.v author in
  let remarks =
    match remark with
    | None   -> []
    | Some r -> [ r ]
  in
  { topic; dynamic; provenance; remarks; support = 0 }

let merge t1 t2 =
  match Ordering.of_int (compare t1 t2) with
  | Greater
  | Less ->
      Irmin.Merge.conflict "TODO"
  (* Error (`Speculation_conflict (t1, t2)) *)
  | Equal ->
      Irmin.Merge.ok
        { t1 with
          remarks = List.merge ~compare:Remark.compare t1.remarks t2.remarks
        }
