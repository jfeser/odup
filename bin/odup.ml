open Core
open Async
open Odup
open Walk

let pipe_progress r =
  let ct = ref 0 in
  Pipe.create_reader ~close_on_exception:false
    (Pipe.transfer' ~max_queue_length:100 r ~f:(fun vs ->
         ct := Queue.fold ~init:!ct ~f:(fun ct (l, _) -> ct + List.length l) vs;
         Print.printf "%d " !ct;
         return vs))

let main ~nworkers ~tol ~dir =
  let%bind working_dir = Sys.getcwd () in
  let config =
    Rpc_parallel.Map_reduce.Config.create ~cd:working_dir ~local:nworkers
      ~redirect_stderr:`Dev_null ~redirect_stdout:`Dev_null ()
  in
  let archives = find_archives dir in
  let%bind images_reader =
    Rpc_parallel.Map_reduce.map_unordered config archives
      ~m:(module Hash_images_map_function)
      ~param:()
  in
  Print.printf "Hashed: ";
  let%bind images =
    pipe_progress images_reader
    |> Pipe.to_list
    >>| List.map ~f:(fun (x, _) -> x)
    >>| List.concat
  in
  Print.printf "\n";
  let%bind duplicates_reader =
    Rpc_parallel.Map_reduce.map_unordered config
      (List.chunks_of images ~length:100 |> Pipe.of_list)
      ~m:(module Find_duplicates_map_function)
      ~param:Find_duplicates_map_impl.Param.{ tol; all_images = images }
  in
  Print.printf "Duplicates: ";
  let%bind dupes =
    pipe_progress duplicates_reader
    |> Pipe.to_list
    >>| List.map ~f:(fun (x, _) -> x)
    >>| List.concat
  in
  Print.printf "\n";
  print_s [%message (dupes : (Image.t * Image.t) list)];
  Deferred.unit

let command =
  let spec =
    let open Command.Let_syntax in
    let%map_open dir = anon ("dir" %: string)
    and tol =
      flag "-t" (optional_with_default 0.0 float) ~doc:" comparison tolerance"
    and nworkers =
      flag "nworkers" (optional_with_default 4 int) ~doc:" Number of workers"
    in
    fun () -> main ~nworkers ~tol ~dir
  in
  Command.async ~summary:"" spec

let () = Rpc_parallel.start_app command
