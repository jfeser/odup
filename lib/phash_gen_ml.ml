let () =
  Cstubs.write_ml Format.std_formatter ~prefix:"phash_stub"
    (module Phash_bindings.Bindings)
