Well placed holes

  $ wax good.wax -o /dev/null

A constant before the last hole

  $ wax bad1.wax -o /dev/null
  File "bad1.wax", line 3, characters 14-15:
  Error: This expression occurs before a hole '_'.
  2 │ fn f () -> i32 {
  3 │   1; 3;  _ + (2 + _);
                    ^
  [128]

A binary operation before the last hole

  $ wax bad2.wax -o /dev/null
  File "bad2.wax", line 3, characters 12-17:
  Error: This expression occurs before a hole '_'.
  2 │ fn f () -> i32 {
  3 │   1; 2; 3;  _ + _ + _;
                  ^^^^^
  [128]

Struct fields are ordered according to the type

  $ wax bad3.wax -o /dev/null
  File "bad3.wax", line 4, characters 17-18:
  Error: This expression occurs before a hole '_'.
  3 │ fn i () -> &t {
  4 │   1; {t| y:_, x: 1};
                       ^
  [128]


Function arguments are before the function itself

  $ wax bad4.wax -o /dev/null
  File "bad4.wax", line 7, characters 7-8:
  Error: This expression occurs before a hole '_'.
  6 │ fn j () -> i32 {
  7 │   f; _(1);
             ^
  [128]

Select condition is considered after the two branches

  $ wax bad5.wax -o /dev/null
  File "bad5.wax", line 2, characters 7-8:
  Error: This expression occurs before a hole '_'.
  1 │ fn k (x: i32) -> i32 {
  2 │   x; _?1:2
             ^
  [128]
