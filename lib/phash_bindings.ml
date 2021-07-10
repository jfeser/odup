open Ctypes

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

  let ph_dct_imagehash_jpeg =
    foreign "ph_dct_imagehash_jpeg" (ptr void @-> ptr int64_t @-> returning int)

  let ph_dct_imagehash_png =
    foreign "ph_dct_imagehash_png" (ptr void @-> ptr int64_t @-> returning int)

  let fmemopen =
    foreign "fmemopen" (string @-> size_t @-> string @-> returning (ptr void))

  let fclose = foreign "fclose" (ptr void @-> returning int)
end
