open! Core
open! Async
open! Odup

let main tol dir =
  let%bind dups = Walk.images dir |> Walk.hashes |> Walk.find_duplicates ~tol in
  return
  @@ List.iter dups ~f:(fun group ->
         Fmt.pr "%a\n" Fmt.Dump.(list Walk.Image_ref.pp) group)

let () =
  let cmd =
    let open Command.Let_syntax in
    let%map_open dir = anon ("dir" %: string)
    and tol =
      flag "-t" (optional_with_default 0.0 float) ~doc:" comparison tolerance"
    in
    fun () -> main tol dir
  in
  Command.async ~summary:"" cmd |> Command.run
