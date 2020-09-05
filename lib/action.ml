open! Base

type t =
  | Dict
  | Spect
  | Mark
[@@deriving sexp, eq]

let name_map = [ ("dict", Dict); ("spect", Spect); ("mark", Mark) ]

let of_string : string -> t option =
 fun s -> List.Assoc.find name_map ~equal:String.equal s

let to_string : t -> string =
 fun t -> List.Assoc.(inverse name_map |> fun m -> find_exn m ~equal t)

let describe = function
  | Dict -> "a saying"
  | Spect -> "to observe"
  | Mark -> "to trace out boundaries"
