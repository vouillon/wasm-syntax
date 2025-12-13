(subdir wasm-formatted
 (rule
  (target array.wat.gen)
  (deps ../wasm-source/array.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/array.wat)))
 (rule
  (alias runtest)
  (action
   (diff array.wat array.wat.gen))))

(subdir wax
 (rule
  (target array.wax.gen)
  (deps ../wasm-source/array.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/array.wat)))
 (rule
  (alias runtest)
  (action
   (diff array.wax array.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target array.wat.gen)
  (deps ../wax/array.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/array.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff array.wat array.wat.gen))))

(subdir wasm-formatted
 (rule
  (target backtrace.wat.gen)
  (deps ../wasm-source/backtrace.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/backtrace.wat)))
 (rule
  (alias runtest)
  (action
   (diff backtrace.wat backtrace.wat.gen))))

(subdir wax
 (rule
  (target backtrace.wax.gen)
  (deps ../wasm-source/backtrace.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/backtrace.wat)))
 (rule
  (alias runtest)
  (action
   (diff backtrace.wax backtrace.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target backtrace.wat.gen)
  (deps ../wax/backtrace.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/backtrace.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff backtrace.wat backtrace.wat.gen))))

(subdir wasm-formatted
 (rule
  (target bigarray.wat.gen)
  (deps ../wasm-source/bigarray.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/bigarray.wat)))
 (rule
  (alias runtest)
  (action
   (diff bigarray.wat bigarray.wat.gen))))

(subdir wax
 (rule
  (target bigarray.wax.gen)
  (deps ../wasm-source/bigarray.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/bigarray.wat)))
 (rule
  (alias runtest)
  (action
   (diff bigarray.wax bigarray.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target bigarray.wat.gen)
  (deps ../wax/bigarray.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/bigarray.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff bigarray.wat bigarray.wat.gen))))

(subdir wasm-formatted
 (rule
  (target bigstring.wat.gen)
  (deps ../wasm-source/bigstring.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/bigstring.wat)))
 (rule
  (alias runtest)
  (action
   (diff bigstring.wat bigstring.wat.gen))))

(subdir wax
 (rule
  (target bigstring.wax.gen)
  (deps ../wasm-source/bigstring.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/bigstring.wat)))
 (rule
  (alias runtest)
  (action
   (diff bigstring.wax bigstring.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target bigstring.wat.gen)
  (deps ../wax/bigstring.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/bigstring.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff bigstring.wat bigstring.wat.gen))))

(subdir wasm-formatted
 (rule
  (target compare.wat.gen)
  (deps ../wasm-source/compare.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/compare.wat)))
 (rule
  (alias runtest)
  (action
   (diff compare.wat compare.wat.gen))))

(subdir wax
 (rule
  (target compare.wax.gen)
  (deps ../wasm-source/compare.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/compare.wat)))
 (rule
  (alias runtest)
  (action
   (diff compare.wax compare.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target compare.wat.gen)
  (deps ../wax/compare.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/compare.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff compare.wat compare.wat.gen))))

(subdir wasm-formatted
 (rule
  (target custom.wat.gen)
  (deps ../wasm-source/custom.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/custom.wat)))
 (rule
  (alias runtest)
  (action
   (diff custom.wat custom.wat.gen))))

(subdir wax
 (rule
  (target custom.wax.gen)
  (deps ../wasm-source/custom.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/custom.wat)))
 (rule
  (alias runtest)
  (action
   (diff custom.wax custom.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target custom.wat.gen)
  (deps ../wax/custom.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/custom.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff custom.wat custom.wat.gen))))

(subdir wasm-formatted
 (rule
  (target domain.wat.gen)
  (deps ../wasm-source/domain.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/domain.wat)))
 (rule
  (alias runtest)
  (action
   (diff domain.wat domain.wat.gen))))

(subdir wax
 (rule
  (target domain.wax.gen)
  (deps ../wasm-source/domain.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/domain.wat)))
 (rule
  (alias runtest)
  (action
   (diff domain.wax domain.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target domain.wat.gen)
  (deps ../wax/domain.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/domain.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff domain.wat domain.wat.gen))))

(subdir wasm-formatted
 (rule
  (target dynlink.wat.gen)
  (deps ../wasm-source/dynlink.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/dynlink.wat)))
 (rule
  (alias runtest)
  (action
   (diff dynlink.wat dynlink.wat.gen))))

(subdir wax
 (rule
  (target dynlink.wax.gen)
  (deps ../wasm-source/dynlink.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/dynlink.wat)))
 (rule
  (alias runtest)
  (action
   (diff dynlink.wax dynlink.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target dynlink.wat.gen)
  (deps ../wax/dynlink.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/dynlink.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff dynlink.wat dynlink.wat.gen))))

(subdir wasm-formatted
 (rule
  (target effect.wat.gen)
  (deps ../wasm-source/effect.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/effect.wat)))
 (rule
  (alias runtest)
  (action
   (diff effect.wat effect.wat.gen))))

(subdir wax
 (rule
  (target effect.wax.gen)
  (deps ../wasm-source/effect.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/effect.wat)))
 (rule
  (alias runtest)
  (action
   (diff effect.wax effect.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target effect.wat.gen)
  (deps ../wax/effect.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/effect.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff effect.wat effect.wat.gen))))

(subdir wasm-formatted
 (rule
  (target fail.wat.gen)
  (deps ../wasm-source/fail.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/fail.wat)))
 (rule
  (alias runtest)
  (action
   (diff fail.wat fail.wat.gen))))

(subdir wax
 (rule
  (target fail.wax.gen)
  (deps ../wasm-source/fail.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/fail.wat)))
 (rule
  (alias runtest)
  (action
   (diff fail.wax fail.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target fail.wat.gen)
  (deps ../wax/fail.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/fail.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff fail.wat fail.wat.gen))))

(subdir wasm-formatted
 (rule
  (target float.wat.gen)
  (deps ../wasm-source/float.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/float.wat)))
 (rule
  (alias runtest)
  (action
   (diff float.wat float.wat.gen))))

(subdir wax
 (rule
  (target float.wax.gen)
  (deps ../wasm-source/float.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/float.wat)))
 (rule
  (alias runtest)
  (action
   (diff float.wax float.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target float.wat.gen)
  (deps ../wax/float.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/float.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff float.wat float.wat.gen))))

(subdir wasm-formatted
 (rule
  (target fs.wat.gen)
  (deps ../wasm-source/fs.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/fs.wat)))
 (rule
  (alias runtest)
  (action
   (diff fs.wat fs.wat.gen))))

(subdir wax
 (rule
  (target fs.wax.gen)
  (deps ../wasm-source/fs.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/fs.wat)))
 (rule
  (alias runtest)
  (action
   (diff fs.wax fs.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target fs.wat.gen)
  (deps ../wax/fs.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/fs.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff fs.wat fs.wat.gen))))

(subdir wasm-formatted
 (rule
  (target gc.wat.gen)
  (deps ../wasm-source/gc.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/gc.wat)))
 (rule
  (alias runtest)
  (action
   (diff gc.wat gc.wat.gen))))

(subdir wax
 (rule
  (target gc.wax.gen)
  (deps ../wasm-source/gc.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/gc.wat)))
 (rule
  (alias runtest)
  (action
   (diff gc.wax gc.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target gc.wat.gen)
  (deps ../wax/gc.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/gc.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff gc.wat gc.wat.gen))))

(subdir wasm-formatted
 (rule
  (target hash.wat.gen)
  (deps ../wasm-source/hash.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/hash.wat)))
 (rule
  (alias runtest)
  (action
   (diff hash.wat hash.wat.gen))))

(subdir wax
 (rule
  (target hash.wax.gen)
  (deps ../wasm-source/hash.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/hash.wat)))
 (rule
  (alias runtest)
  (action
   (diff hash.wax hash.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target hash.wat.gen)
  (deps ../wax/hash.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/hash.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff hash.wat hash.wat.gen))))

(subdir wasm-formatted
 (rule
  (target int32.wat.gen)
  (deps ../wasm-source/int32.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/int32.wat)))
 (rule
  (alias runtest)
  (action
   (diff int32.wat int32.wat.gen))))

(subdir wax
 (rule
  (target int32.wax.gen)
  (deps ../wasm-source/int32.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/int32.wat)))
 (rule
  (alias runtest)
  (action
   (diff int32.wax int32.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target int32.wat.gen)
  (deps ../wax/int32.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/int32.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff int32.wat int32.wat.gen))))

(subdir wasm-formatted
 (rule
  (target int64.wat.gen)
  (deps ../wasm-source/int64.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/int64.wat)))
 (rule
  (alias runtest)
  (action
   (diff int64.wat int64.wat.gen))))

(subdir wax
 (rule
  (target int64.wax.gen)
  (deps ../wasm-source/int64.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/int64.wat)))
 (rule
  (alias runtest)
  (action
   (diff int64.wax int64.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target int64.wat.gen)
  (deps ../wax/int64.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/int64.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff int64.wat int64.wat.gen))))

(subdir wasm-formatted
 (rule
  (target ints.wat.gen)
  (deps ../wasm-source/ints.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/ints.wat)))
 (rule
  (alias runtest)
  (action
   (diff ints.wat ints.wat.gen))))

(subdir wax
 (rule
  (target ints.wax.gen)
  (deps ../wasm-source/ints.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/ints.wat)))
 (rule
  (alias runtest)
  (action
   (diff ints.wax ints.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target ints.wat.gen)
  (deps ../wax/ints.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/ints.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff ints.wat ints.wat.gen))))

(subdir wasm-formatted
 (rule
  (target io.wat.gen)
  (deps ../wasm-source/io.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/io.wat)))
 (rule
  (alias runtest)
  (action
   (diff io.wat io.wat.gen))))

(subdir wax
 (rule
  (target io.wax.gen)
  (deps ../wasm-source/io.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/io.wat)))
 (rule
  (alias runtest)
  (action
   (diff io.wax io.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target io.wat.gen)
  (deps ../wax/io.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/io.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff io.wat io.wat.gen))))

(subdir wasm-formatted
 (rule
  (target jslib.wat.gen)
  (deps ../wasm-source/jslib.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/jslib.wat)))
 (rule
  (alias runtest)
  (action
   (diff jslib.wat jslib.wat.gen))))

(subdir wax
 (rule
  (target jslib.wax.gen)
  (deps ../wasm-source/jslib.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/jslib.wat)))
 (rule
  (alias runtest)
  (action
   (diff jslib.wax jslib.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target jslib.wat.gen)
  (deps ../wax/jslib.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/jslib.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff jslib.wat jslib.wat.gen))))

(subdir wasm-formatted
 (rule
  (target jslib_js_of_ocaml.wat.gen)
  (deps ../wasm-source/jslib_js_of_ocaml.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/jslib_js_of_ocaml.wat)))
 (rule
  (alias runtest)
  (action
   (diff jslib_js_of_ocaml.wat jslib_js_of_ocaml.wat.gen))))

(subdir wax
 (rule
  (target jslib_js_of_ocaml.wax.gen)
  (deps ../wasm-source/jslib_js_of_ocaml.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/jslib_js_of_ocaml.wat)))
 (rule
  (alias runtest)
  (action
   (diff jslib_js_of_ocaml.wax jslib_js_of_ocaml.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target jslib_js_of_ocaml.wat.gen)
  (deps ../wax/jslib_js_of_ocaml.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/jslib_js_of_ocaml.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff jslib_js_of_ocaml.wat jslib_js_of_ocaml.wat.gen))))

(subdir wasm-formatted
 (rule
  (target jsstring.wat.gen)
  (deps ../wasm-source/jsstring.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/jsstring.wat)))
 (rule
  (alias runtest)
  (action
   (diff jsstring.wat jsstring.wat.gen))))

(subdir wax
 (rule
  (target jsstring.wax.gen)
  (deps ../wasm-source/jsstring.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/jsstring.wat)))
 (rule
  (alias runtest)
  (action
   (diff jsstring.wax jsstring.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target jsstring.wat.gen)
  (deps ../wax/jsstring.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/jsstring.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff jsstring.wat jsstring.wat.gen))))

(subdir wasm-formatted
 (rule
  (target lexing.wat.gen)
  (deps ../wasm-source/lexing.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/lexing.wat)))
 (rule
  (alias runtest)
  (action
   (diff lexing.wat lexing.wat.gen))))

(subdir wax
 (rule
  (target lexing.wax.gen)
  (deps ../wasm-source/lexing.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/lexing.wat)))
 (rule
  (alias runtest)
  (action
   (diff lexing.wax lexing.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target lexing.wat.gen)
  (deps ../wax/lexing.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/lexing.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff lexing.wat lexing.wat.gen))))

(subdir wasm-formatted
 (rule
  (target marshal.wat.gen)
  (deps ../wasm-source/marshal.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/marshal.wat)))
 (rule
  (alias runtest)
  (action
   (diff marshal.wat marshal.wat.gen))))

(subdir wax
 (rule
  (target marshal.wax.gen)
  (deps ../wasm-source/marshal.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/marshal.wat)))
 (rule
  (alias runtest)
  (action
   (diff marshal.wax marshal.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target marshal.wat.gen)
  (deps ../wax/marshal.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/marshal.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff marshal.wat marshal.wat.gen))))

(subdir wasm-formatted
 (rule
  (target md5.wat.gen)
  (deps ../wasm-source/md5.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/md5.wat)))
 (rule
  (alias runtest)
  (action
   (diff md5.wat md5.wat.gen))))

(subdir wax
 (rule
  (target md5.wax.gen)
  (deps ../wasm-source/md5.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/md5.wat)))
 (rule
  (alias runtest)
  (action
   (diff md5.wax md5.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target md5.wat.gen)
  (deps ../wax/md5.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/md5.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff md5.wat md5.wat.gen))))

(subdir wasm-formatted
 (rule
  (target nat.wat.gen)
  (deps ../wasm-source/nat.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/nat.wat)))
 (rule
  (alias runtest)
  (action
   (diff nat.wat nat.wat.gen))))

(subdir wax
 (rule
  (target nat.wax.gen)
  (deps ../wasm-source/nat.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/nat.wat)))
 (rule
  (alias runtest)
  (action
   (diff nat.wax nat.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target nat.wat.gen)
  (deps ../wax/nat.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/nat.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff nat.wat nat.wat.gen))))

(subdir wasm-formatted
 (rule
  (target obj.wat.gen)
  (deps ../wasm-source/obj.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/obj.wat)))
 (rule
  (alias runtest)
  (action
   (diff obj.wat obj.wat.gen))))

(subdir wax
 (rule
  (target obj.wax.gen)
  (deps ../wasm-source/obj.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/obj.wat)))
 (rule
  (alias runtest)
  (action
   (diff obj.wax obj.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target obj.wat.gen)
  (deps ../wax/obj.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/obj.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff obj.wat obj.wat.gen))))

(subdir wasm-formatted
 (rule
  (target parsing.wat.gen)
  (deps ../wasm-source/parsing.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/parsing.wat)))
 (rule
  (alias runtest)
  (action
   (diff parsing.wat parsing.wat.gen))))

(subdir wax
 (rule
  (target parsing.wax.gen)
  (deps ../wasm-source/parsing.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/parsing.wat)))
 (rule
  (alias runtest)
  (action
   (diff parsing.wax parsing.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target parsing.wat.gen)
  (deps ../wax/parsing.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/parsing.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff parsing.wat parsing.wat.gen))))

(subdir wasm-formatted
 (rule
  (target printexc.wat.gen)
  (deps ../wasm-source/printexc.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/printexc.wat)))
 (rule
  (alias runtest)
  (action
   (diff printexc.wat printexc.wat.gen))))

(subdir wax
 (rule
  (target printexc.wax.gen)
  (deps ../wasm-source/printexc.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/printexc.wat)))
 (rule
  (alias runtest)
  (action
   (diff printexc.wax printexc.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target printexc.wat.gen)
  (deps ../wax/printexc.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/printexc.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff printexc.wat printexc.wat.gen))))

(subdir wasm-formatted
 (rule
  (target prng.wat.gen)
  (deps ../wasm-source/prng.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/prng.wat)))
 (rule
  (alias runtest)
  (action
   (diff prng.wat prng.wat.gen))))

(subdir wax
 (rule
  (target prng.wax.gen)
  (deps ../wasm-source/prng.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/prng.wat)))
 (rule
  (alias runtest)
  (action
   (diff prng.wax prng.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target prng.wat.gen)
  (deps ../wax/prng.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/prng.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff prng.wat prng.wat.gen))))

(subdir wasm-formatted
 (rule
  (target runtime_events.wat.gen)
  (deps ../wasm-source/runtime_events.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/runtime_events.wat)))
 (rule
  (alias runtest)
  (action
   (diff runtime_events.wat runtime_events.wat.gen))))

(subdir wax
 (rule
  (target runtime_events.wax.gen)
  (deps ../wasm-source/runtime_events.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/runtime_events.wat)))
 (rule
  (alias runtest)
  (action
   (diff runtime_events.wax runtime_events.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target runtime_events.wat.gen)
  (deps ../wax/runtime_events.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/runtime_events.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff runtime_events.wat runtime_events.wat.gen))))

(subdir wasm-formatted
 (rule
  (target stdlib.wat.gen)
  (deps ../wasm-source/stdlib.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/stdlib.wat)))
 (rule
  (alias runtest)
  (action
   (diff stdlib.wat stdlib.wat.gen))))

(subdir wax
 (rule
  (target stdlib.wax.gen)
  (deps ../wasm-source/stdlib.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/stdlib.wat)))
 (rule
  (alias runtest)
  (action
   (diff stdlib.wax stdlib.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target stdlib.wat.gen)
  (deps ../wax/stdlib.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/stdlib.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff stdlib.wat stdlib.wat.gen))))

(subdir wasm-formatted
 (rule
  (target str.wat.gen)
  (deps ../wasm-source/str.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/str.wat)))
 (rule
  (alias runtest)
  (action
   (diff str.wat str.wat.gen))))

(subdir wax
 (rule
  (target str.wax.gen)
  (deps ../wasm-source/str.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/str.wat)))
 (rule
  (alias runtest)
  (action
   (diff str.wax str.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target str.wat.gen)
  (deps ../wax/str.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/str.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff str.wat str.wat.gen))))

(subdir wasm-formatted
 (rule
  (target string.wat.gen)
  (deps ../wasm-source/string.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/string.wat)))
 (rule
  (alias runtest)
  (action
   (diff string.wat string.wat.gen))))

(subdir wax
 (rule
  (target string.wax.gen)
  (deps ../wasm-source/string.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/string.wat)))
 (rule
  (alias runtest)
  (action
   (diff string.wax string.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target string.wat.gen)
  (deps ../wax/string.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/string.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff string.wat string.wat.gen))))

(subdir wasm-formatted
 (rule
  (target sync.wat.gen)
  (deps ../wasm-source/sync.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/sync.wat)))
 (rule
  (alias runtest)
  (action
   (diff sync.wat sync.wat.gen))))

(subdir wax
 (rule
  (target sync.wax.gen)
  (deps ../wasm-source/sync.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/sync.wat)))
 (rule
  (alias runtest)
  (action
   (diff sync.wax sync.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target sync.wat.gen)
  (deps ../wax/sync.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/sync.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff sync.wat sync.wat.gen))))

(subdir wasm-formatted
 (rule
  (target sys.wat.gen)
  (deps ../wasm-source/sys.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/sys.wat)))
 (rule
  (alias runtest)
  (action
   (diff sys.wat sys.wat.gen))))

(subdir wax
 (rule
  (target sys.wax.gen)
  (deps ../wasm-source/sys.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/sys.wat)))
 (rule
  (alias runtest)
  (action
   (diff sys.wax sys.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target sys.wat.gen)
  (deps ../wax/sys.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/sys.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff sys.wat sys.wat.gen))))

(subdir wasm-formatted
 (rule
  (target toplevel.wat.gen)
  (deps ../wasm-source/toplevel.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/toplevel.wat)))
 (rule
  (alias runtest)
  (action
   (diff toplevel.wat toplevel.wat.gen))))

(subdir wax
 (rule
  (target toplevel.wax.gen)
  (deps ../wasm-source/toplevel.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/toplevel.wat)))
 (rule
  (alias runtest)
  (action
   (diff toplevel.wax toplevel.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target toplevel.wat.gen)
  (deps ../wax/toplevel.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/toplevel.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff toplevel.wat toplevel.wat.gen))))

(subdir wasm-formatted
 (rule
  (target unix.wat.gen)
  (deps ../wasm-source/unix.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/unix.wat)))
 (rule
  (alias runtest)
  (action
   (diff unix.wat unix.wat.gen))))

(subdir wax
 (rule
  (target unix.wax.gen)
  (deps ../wasm-source/unix.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/unix.wat)))
 (rule
  (alias runtest)
  (action
   (diff unix.wax unix.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target unix.wat.gen)
  (deps ../wax/unix.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/unix.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff unix.wat unix.wat.gen))))

(subdir wasm-formatted
 (rule
  (target weak.wat.gen)
  (deps ../wasm-source/weak.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/weak.wat)))
 (rule
  (alias runtest)
  (action
   (diff weak.wat weak.wat.gen))))

(subdir wax
 (rule
  (target weak.wax.gen)
  (deps ../wasm-source/weak.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/weak.wat)))
 (rule
  (alias runtest)
  (action
   (diff weak.wax weak.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target weak.wat.gen)
  (deps ../wax/weak.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/weak.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff weak.wat weak.wat.gen))))

(subdir wasm-formatted
 (rule
  (target zstd.wat.gen)
  (deps ../wasm-source/zstd.wat)
  (action
   (run wax --validate --input-format wat --format wat -o %{target} ../wasm-source/zstd.wat)))
 (rule
  (alias runtest)
  (action
   (diff zstd.wat zstd.wat.gen))))

(subdir wax
 (rule
  (target zstd.wax.gen)
  (deps ../wasm-source/zstd.wat)
 (action
  (run wax --validate --input-format wat --format wax -o %{target} ../wasm-source/zstd.wat)))
 (rule
  (alias runtest)
  (action
   (diff zstd.wax zstd.wax.gen))))

(subdir wasm-round-trip
 (rule
  (target zstd.wat.gen)
  (deps ../wax/zstd.wax.gen)
 (action
  (run wax --validate --input-format wax --format wat -o %{target} ../wax/zstd.wax.gen)))
 (rule
  (alias runtest)
  (action
   (diff zstd.wat zstd.wat.gen))))

