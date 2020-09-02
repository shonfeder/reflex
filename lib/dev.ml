(** For WIP code
 **
 ** {1} TODOs
 **
 ** - TODO Should be removed before v1 release *)

open Base

module Topic = struct
  type t = string [@@deriving irmin, eq]
end

module Author = struct
  type t = string [@@deriving irmin, eq]
end

module Provenance = struct
  type t =
    { author : Author.t
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
