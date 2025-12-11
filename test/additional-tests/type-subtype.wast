(assert_invalid
 (module
  (rec
   (type $b (sub $a (struct)))
   (type $a (sub (struct)))))
 "forward use")

(assert_invalid
 (module
  (rec
   (type $a (sub $a (struct)))))
 "forward use")
