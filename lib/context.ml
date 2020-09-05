module Store = Irmin_unix.Git.FS.KV (Project)

(* TODO Provide access to the store without having to manually use the Store and
   store *)
type t = { store : Store.t (* ; user : string *) }

let load path =
  let open Lwt.Syntax in
  (* let* user = Lwt_unix. in *)
  let* repo = Irmin_git.config ~bare:true path |> Store.Repo.v in
  let+ store = Store.master repo in
  Ok { store }
