open Ctypes
include Phash_bindings.Bindings (Phash_generated_stubs)

let dct_image_hash hasher img =
  let file = fmemopen img (Unsigned.Size_t.of_int @@ String.length img) "r" in
  let hash = allocate int64_t 0L in
  let ret = hasher file hash in
  (fclose file : _) |> ignore;
  if ret = 0 then Some !@hash else None

let dct_jpeg_hash = dct_image_hash ph_dct_imagehash_jpeg

let dct_png_hash = dct_image_hash ph_dct_imagehash_png

let dct_hash img kind =
  match kind with `Png -> dct_png_hash img | `Jpeg -> dct_jpeg_hash img
