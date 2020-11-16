open! Core
open! Async

module Image_ref = struct
  type 'a t = {
    filename : string;
    archive : string option;
    kind : [ `Png | `Jpeg ];
    hash : 'a; [@ignore]
  }
  [@@deriving compare]

  let pp ?pp_hash fmt x =
    match pp_hash with
    | Some pp_hash -> Fmt.pf fmt "%s/%a" x.filename pp_hash x.hash
    | None -> Fmt.pf fmt "%s" x.filename
end

module Image = struct
  type t = { image_ref : unit Image_ref.t; contents : string }
end

let images dir =
  let open Async.Let_syntax in
  let classify name =
    let _, ext = Filename.split_extension name in
    match ext with
    | Some "jpg" | Some "jpeg" -> `Image `Jpeg
    | Some "png" -> `Image `Png
    | Some "zip" | Some "cbz" -> `Zip
    | _ -> `Other
  in
  let filter (name, _) =
    match classify name with
    | `Image _ | `Zip -> return true
    | `Other -> return false
  in
  let options = Async_find.{ Options.default with filter = Some filter } in
  let find = Async_find.create ~options dir in
  Pipe.create_reader ~close_on_exception:true (fun pipe ->
      Async_find.iter find ~f:(fun (fn, _) ->
          match classify fn with
          | `Other -> return ()
          | `Image kind ->
              Pipe.write pipe
                Image.
                  {
                    image_ref =
                      { filename = fn; archive = None; kind; hash = () };
                    contents = In_channel.read_all fn;
                  }
          | `Zip ->
              let zip = Zip.open_in fn in
              let%bind () =
                Zip.entries zip
                |> Deferred.List.iter ~f:(fun entry ->
                       match classify entry.Zip.filename with
                       | `Image kind ->
                           let image_ref =
                             Image_ref.
                               {
                                 filename = entry.filename;
                                 archive = Some fn;
                                 kind;
                                 hash = ();
                               }
                           in
                           Pipe.write pipe
                             Image.
                               {
                                 image_ref;
                                 contents = Zip.read_entry zip entry;
                               }
                       | `Zip | `Other -> return ())
              in
              Zip.close_in zip;
              return ()))

let hashes images =
  Pipe.map images ~f:(fun Image.{ image_ref; contents } ->
      let hash =
        Option.value_exn ~message:"hashing failed"
          (Phash.dct_hash contents image_ref.kind)
      in
      { image_ref with hash })

module Hash_tree = Bst.Bisec_tree.Make (struct
  type t = Unsigned.UInt64.t Image_ref.t

  let dist _ _ = failwith ""
end)

let hamming_dist x x' =
  Unsigned.UInt64.(logxor x x' |> to_int64 |> Int64.popcount)

let dist h h' = hamming_dist h.Image_ref.hash h'.Image_ref.hash |> Float.of_int

let find_duplicates ~tol hashes =
  let pp_ref = Image_ref.pp ~pp_hash:Unsigned.UInt64.pp in
  let%bind hashes = Pipe.to_list hashes in
  Fmt.pr "Got %d hashes: %a@." (List.length hashes)
    (Fmt.Dump.list (Image_ref.pp ~pp_hash:Unsigned.UInt64.pp))
    hashes;
  return
  @@ List.filter_map hashes ~f:(fun h ->
         let dups =
           List.filter hashes ~f:(fun h' ->
               let d = Float.(dist h h') in
               Fmt.pr "h=%a, h'=%a, dist=%f\n" pp_ref h pp_ref h' d;
               (not ([%compare.equal: _ Image_ref.t] h h')) && Float.(d <= tol))
         in
         if List.length dups <= 1 then None else Some dups)
