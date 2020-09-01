(* TODO open CalendarLib *)
(* TODO Add keep/stop/etc. *)

open Base

type topic = string [@@deriving irmin, eq]

type author = string [@@deriving irmin, eq]

module Provenance = struct
  type t =
    { author : author
    ; timestamp : float
    }
  [@@deriving irmin, eq]

  let compare t t' =
    let time_cmp = Float.compare t.timestamp t'.timestamp in
    if time_cmp = 0 then
      String.compare t.author t'.author
    else
      time_cmp

  let v author = { author; timestamp = Unix.time () }
end

module Remark = struct
  type t =
    { content : string
    ; provenance : Provenance.t
    }
  [@@deriving irmin, eq]

  let compare t t' =
    let provenance_cmp = Provenance.compare t.provenance t'.provenance in
    if provenance_cmp = 0 then
      String.compare t.content t'.content
    else
      provenance_cmp

  let v ~author ~content = { provenance = Provenance.v author; content }
end

module Dynamic = struct
  type t =
    | Problem
    | Celebration
    | Other (* FIXME? *)
  [@@deriving irmin, eq, sexp]

  let of_string s = s |> Sexplib.Std.sexp_of_string |> t_of_sexp
end

module Speculation = struct
  type t =
    { topic : topic
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
end

module Project = struct
  module Assoc = List.Assoc

  let find = Assoc.find ~equal:String.equal

  let add = Assoc.add ~equal:String.equal

  type t = (topic * Speculation.t) list [@@deriving irmin]

  let ( let* ), ( let+ ) = Irmin.Merge.Infix.(( >>=* ), ( >|=* ))

  let merge_or_add t ((topic : topic), (s : Speculation.t)) =
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

  (* List.Assoc.find ~equal:String.equal *)
  (* List.merge ~compare:Speculation.compare t1 t2 *)

  (* let merge ~old t1 t2 =
   *   let open Irmin.Merge.Infix in
   *   old () >>=* fun old ->
   *   let old = Option.value old ~default:List.(hd (sort compare [ t1; t2 ])) in
   *   if not (same_speculation old t1 && same_speculation t1 t2) then
   *     Irmin.Merge.conflict "speculation conflict: (%s, %s) (%s, %s) (%s, %s)"
   *       old.topic old.provenance.author t1.topic t1.provenance.author t2.topic
   *       t2.provenance.author
   *   else
   *     (\* TODO Perf *\)
   *     let remarks =
   *       List.concat [ old.remarks; t1.remarks; t2.remarks ]
   *       |> List.sort_uniq Remark.compare
   *     in
   *     (\* TODO Should we preserve the timestamp or anything from t1 and t2? *\)
   *     Irmin.Merge.ok { old with remarks } *)

  let merge = Irmin.Merge.(option (v t merge))

  let add_remark t topic remark =
    match find t topic with
    | None   -> Error (`Invalid_topic topic)
    | Some s -> Ok (add t topic (Speculation.add_remark s remark))

  let add_speculation t (spec : Speculation.t) =
    match find t spec.topic with
    | None   -> Ok (add t spec.topic spec)
    | Some s -> Error (`Speculation_already_exists s)
end

module type Spect = sig
  include module type of T
end

module Retro = struct
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
  end
end

(* module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.V1) *)
