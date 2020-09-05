open Base
module Store = Irmin_unix.Git.FS.KV (Project)

(* TODO Provide access to the store without having to manually use the Store and
   store *)
type t =
  { store : Store.t
  ; user : string
  }

let get_user = function
  | None -> Lwt_unix.getlogin ()
  | Some u -> Lwt.return u

let load ?user path =
  let open Lwt.Syntax in
  let* user = get_user user in
  let* repo = Irmin_git.config ~bare:true path |> Store.Repo.v in
  let+ store = Store.master repo in
  Ok { store; user }
