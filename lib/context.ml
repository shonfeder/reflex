module Store = Irmin_unix.Git.FS.KV (Project)

type t = { store : Store.t }

let load path =
  let open Lwt.Syntax in
  let* repo = Irmin_git.config ~bare:true path |> Store.Repo.v in
  let+ store = Store.master repo in
  Ok { store }
