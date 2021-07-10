let () =
  Cstubs.write_c Format.std_formatter ~prefix:"phash_stub"
    (module Phash_bindings.Bindings)
