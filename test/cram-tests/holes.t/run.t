Well placed holes

  $ wax good.wax -o /dev/null

A constant before the last hole

  $ wax bad1.wax -o /dev/null
  2 _ + (2 + _)
  wax: internal error, uncaught exception:
       File "src/lib-wax/typing.ml", line 2189, characters 8-14: Assertion failed
       
  [125]

A binary operation before the last hole

  $ wax bad2.wax -o /dev/null
  3 _ + _ + _
  wax: internal error, uncaught exception:
       File "src/lib-wax/typing.ml", line 2189, characters 8-14: Assertion failed
       
  [125]
