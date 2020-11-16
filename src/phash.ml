open Ctypes
open Foreign

let libphash =
  Dl.dlopen ~flags:[ Dl.RTLD_LAZY ] ~filename:"/usr/local/lib/libpHash.so"

let ph_dct_imagehash_jpeg =
  foreign ~from:libphash ~release_runtime_lock:true "ph_dct_imagehash_jpeg"
    (ptr void @-> ptr uint64_t @-> returning int)

let ph_dct_imagehash_png =
  foreign ~from:libphash ~release_runtime_lock:true "ph_dct_imagehash_png"
    (ptr void @-> ptr uint64_t @-> returning int)

let fmemopen =
  foreign "fmemopen" (string @-> size_t @-> string @-> returning (ptr void))

let fclose = foreign "fclose" (ptr void @-> returning int)

let dct_image_hash hasher img =
  let file = fmemopen img (Unsigned.Size_t.of_int @@ String.length img) "r" in
  let hash = allocate uint64_t Unsigned.UInt64.zero in
  let ret = hasher file hash in
  fclose file |> ignore;
  if ret = 0 then Some !@hash else None

let dct_jpeg_hash = dct_image_hash ph_dct_imagehash_jpeg

let dct_png_hash = dct_image_hash ph_dct_imagehash_png

let dct_hash img kind =
  match kind with `Png -> dct_png_hash img | `Jpeg -> dct_jpeg_hash img
