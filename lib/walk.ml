open! Core
open! Async

let classify name =
  let _, ext = Filename.split_extension name in
  match ext with
  | Some "jpg" | Some "jpeg" -> `Jpeg
  | Some "png" -> `Png
  | _ -> `Other

module Image = struct
  type t = {
    filename : string;
    archive : string;
    kind : [ `Png | `Jpeg ];
    hash : int64;
  }
  [@@deriving bin_io, compare, sexp]

  let create ~kind ~filename ~archive ~hash = { filename; archive; kind; hash }
end

open Image

module Hash_images_map_function =
Rpc_parallel.Map_reduce.Make_map_function (struct
  module Input = struct
    type t = string [@@deriving bin_io]
  end

  module Output = struct
    type t = Image.t list [@@deriving bin_io]
  end

  let map archive =
    let zip = Zip.open_in archive in
    let hashes =
      Exn.protect
        ~f:(fun () ->
          List.filter_map (Zip.entries zip) ~f:(fun entry ->
              let filename = entry.Zip.filename in
              match classify filename with
              | (`Jpeg | `Png) as kind ->
                  let hash =
                    let contents = Zip.read_entry zip entry in
                    Option.value_exn ~message:"hashing failed"
                      (Phash.dct_hash contents kind)
                  in
                  Some (Image.create ~kind ~filename ~archive ~hash)
              | `Other -> None))
        ~finally:(fun () -> Zip.close_in zip)
    in
    return hashes
end)

module Find_duplicates_map_impl = struct
  module Param = struct
    type t = { tol : float; all_images : Image.t list } [@@deriving bin_io]
  end

  module Input = struct
    type t = Image.t list [@@deriving bin_io]
  end

  module Output = struct
    type t = (Image.t * Image.t) list [@@deriving bin_io]
  end

  type state_type = Param.t

  let init = return

  let hamming_dist x x' =
    Unsigned.UInt64.(logxor x x' |> to_int64 |> Int64.popcount)

  let dist h h' =
    let to_uint64 = Unsigned.UInt64.of_int64 in
    hamming_dist (to_uint64 h.hash) (to_uint64 h'.hash) |> Float.of_int

  let map Param.{ tol; all_images } images =
    return
    @@ List.concat_map all_images ~f:(fun img ->
           List.filter_map images ~f:(fun img' ->
               if
                 (not ([%compare.equal: Image.t] img img'))
                 && [%compare: string] img.archive img'.archive < 0
                 && Float.(dist img img' <= tol)
               then Some (img, img')
               else None))
end

module Find_duplicates_map_function =
  Rpc_parallel.Map_reduce.Make_map_function_with_init (Find_duplicates_map_impl)

let find_archives dir =
  let classify name =
    let _, ext = Filename.split_extension name in
    match ext with Some "zip" | Some "cbz" -> `Zip | _ -> `Other
  in
  let filter (name, _) =
    match classify name with `Zip -> return true | `Other -> return false
  in
  let options = Async_find.{ Options.default with filter = Some filter } in
  let find = Async_find.create ~options dir in
  Pipe.create_reader ~close_on_exception:false @@ fun w ->
  Async_find.iter find ~f:(fun (fn, _) -> Pipe.write w fn)
