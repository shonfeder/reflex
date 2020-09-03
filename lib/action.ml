open! Base

type t =
  | Dict
  | Spect
[@@deriving sexp, eq]

let name_map = [ ("dict", Dict); ("spect", Spect) ]

let of_string : string -> t option =
 fun s -> List.Assoc.find name_map ~equal:String.equal s

let to_string : t -> string =
 fun t -> List.Assoc.(inverse name_map |> fun m -> find_exn m ~equal t)

let describe = function
  | Dict  -> ""
  | Spect -> "a survey or review of a past course of events or period of time"
