#!perl

use Test::More tests => 46;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

### ~*

# (def-format-test format.*.1
#   "~A~*~A" (1 2 3) "13")

def_format_test 'format.*.1' =>
  "~A~*~A",
  [ 1, 2, 3 ],
  "13";

# (def-format-test format.*.2
#   "~A~0*~A" (1 2 3) "12" 1)

def_format_test 'format.*.2' =>
  "~A~0*~A",
  [ 1, 2, 3 ],
  "12",
  1;

# (def-format-test format.*.3
#   "~A~v*~A" (1 0 2) "12")

def_format_test 'format.*.3' =>
  "~A~v*~A",
  [ 1, 0, 2 ],
  "12";

# (def-format-test format.*.4
#   "~A~v*~A" (1 1 2 3) "13")

def_format_test 'format.*.4' =>
  "~A~v*~A",
  [ 1, 1, 2, 3 ],
  "13";

# (def-format-test format.*.5
#   "~A~v*~A" (1 nil 2 3) "13")

def_format_test 'format.*.5' =>
  "~A~v*~A",
  [ 1, undef, 2, 3 ],
  "13";

SKIP: {
  my $count = 4;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;
# (def-format-test format.*.6
#   "~A~1{~A~*~A~}~A" (0 '(1 2 3) 4) "0134")

def_format_test 'format.*.6' =>
  "~A~1{~A~*~A~}~A",
  [ 0, [ 1, 2, 3 ],  4 ],
  "0134";

# (def-format-test format.*.7
#   "~A~1{~A~0*~A~}~A" (0 '(1 2 3) 4) "0124")

def_format_test 'format.*.7' =>
  "~A~1{~A~0*~A~}~A",
  [ 0, [ 1, 2, 3 ],  4 ],
  "0124";

# (def-format-test format.*.8
#   "~A~{~A~*~A~}~A" (0 '(1 2 3 4 5 6) 7) "013467")

def_format_test 'format.*.8' =>
  "~A~{~A~*~A~}~A",
  [ 0, [ 1, 2, 3, 4, 5, 6 ],  7 ],
  "013467";

# (def-format-test format.*.9
#   "~A~{~A~A~A~A~v*~^~A~A~A~A~}~A" (0 '(1 2 3 4 nil 6 7 8 9 #\A) 5)
#   "01234789A5")

def_format_test 'format.*.9' =>
  "~A~{~A~A~A~A~v*~^~A~A~A~A~}~A",
  [ 0, [ 1, 2, 3, 4, undef, 6, 7, 8, 9, 'A' ],  5 ],
  "01234789A5";
}

### ~:*

# (def-format-test format.\:*.1
#   "~A~:*~A" (1 2 3) "11" 2)

def_format_test 'format.\:*.1' =>
  "~A~:*~A",
  [ 1, 2, 3 ],
  "11",
  2;

# (def-format-test format.\:*.2
#   "~A~A~:*~A" (1 2 3) "122" 1)

def_format_test 'format.\:*.2' =>
  "~A~A~:*~A",
  [ 1, 2, 3 ],
  "122",
  1;

# (def-format-test format.\:*.3
#   "~A~A~0:*~A" (1 2 3) "123")

def_format_test 'format.\:*.3' =>
  "~A~A~0:*~A",
  [ 1, 2, 3 ],
  "123";

# (def-format-test format.\:*.4
#   "~A~A~2:*~A" (1 2 3) "121" 2)

def_format_test 'format.\:*.4' =>
  "~A~A~2:*~A",
  [ 1, 2, 3 ],
  "121",
  2;

# (def-format-test format.\:*.5
#   "~A~A~v:*~A" (1 2 0 3) "123")

def_format_test 'format.\:*.5' =>
  "~A~A~v:*~A",
  [ 1, 2, 0, 3 ],
  "123";

# (def-format-test format.\:*.6
#   "~A~A~v:*~A" (6 7 2 3) "677" 2)

def_format_test 'format.\:*.6' =>
  "~A~A~v:*~A",
  [ 6, 7, 2, 3 ],
  "677",
  2;

# (def-format-test format.\:*.7
#   "~A~A~v:*~A" (6 7 nil 3) "67NIL" 1)

def_format_test 'format.\:*.7' =>
  "~A~A~v:*~A",
  [ 6, 7, undef, 3 ],
  "67UNDEF",
  1;

SKIP: {
  my $count = 28;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;
# (def-format-test format.\:*.8
#   "~A~1{~A~:*~A~}~A" (0 '(1 2 3) 4) "0114")

def_format_test 'format.\:*.8' =>
  "~A~1{~A~:*~A~}~A",
  [ 0, [ 1, 2, 3 ],  4 ],
  "0114";

# (def-format-test format.\:*.9
#   "~A~1{~A~A~A~:*~A~}~A" (0 '(1 2 3 4) 5) "012335")

def_format_test 'format.\:*.9' =>
  "~A~1{~A~A~A~:*~A~}~A",
  [ 0, [ 1, 2, 3, 4 ],  5 ],
  "012335";

# (def-format-test format.\:*.10
#   "~A~1{~A~A~A~2:*~A~A~}~A" (0 '(1 2 3 4) 5) "0123235")

def_format_test 'format.\:*.10' =>
  "~A~1{~A~A~A~2:*~A~A~}~A",
  [ 0, [ 1, 2, 3, 4 ],  5 ],
  "0123235";

# (def-format-test format.\:*.11
#   "~A~{~A~A~A~3:*~A~A~A~A~}~A" (0 '(1 2 3 4) 5) "012312345")

def_format_test 'format.\:*.11' =>
  "~A~{~A~A~A~3:*~A~A~A~A~}~A",
  [ 0, [ 1, 2, 3, 4 ],  5 ],
  "012312345";

# (def-format-test format.\:*.12
#   "~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A" (0 '(1 2 3 4) 5) "0123412345")

def_format_test 'format.\:*.12' =>
  "~A~{~A~A~A~A~4:*~^~A~A~A~A~}~A",
  [ 0, [ 1, 2, 3, 4 ],  5 ],
  "0123412345";

# (def-format-test format.\:*.13
#   "~A~{~A~A~A~A~v:*~^~A~}~A" (0 '(1 2 3 4 nil) 5) "01234NIL5")

def_format_test 'format.\:*.13' =>
  "~A~{~A~A~A~A~v:*~^~A~}~A",
  [ 0, [ 1, 2, 3, 4, undef ],  5 ],
  "01234NIL5";

### ~@*

# (def-format-test format.@*.1
#   "~A~A~@*~A~A" (1 2 3 4) "1212" 2)

def_format_test 'format.@*.1' =>
  '~A~A~@*~A~A',
  [ 1, 2, 3, 4 ],
  "1212",
  2;

# (def-format-test format.@*.2
#   "~A~A~1@*~A~A" (1 2 3 4) "1223" 1)

def_format_test 'format.@*.2' =>
  '~A~A~1@*~A~A',
  [ 1, 2, 3, 4 ],
  "1223",
  1;

# (def-format-test format.@*.3
#   "~A~A~2@*~A~A" (1 2 3 4) "1234")

def_format_test 'format.@*.3' =>
  '~A~A~2@*~A~A',
  [ 1, 2, 3, 4 ],
  "1234";

# (def-format-test format.@*.4
#   "~A~A~3@*~A~A" (1 2 3 4 5) "1245")

def_format_test 'format.@*.4' =>
  '~A~A~3@*~A~A',
  [ 1, 2, 3, 4, 5 ],
  "1245";

# (def-format-test format.@*.5
#   "~A~A~v@*~A~A" (1 2 nil 3 4) "1212" 3)

def_format_test 'format.@*.5' =>
  '~A~A~v@*~A~A',
  [ 1, 2, undef, 3, 4 ],
  "1212",
  3;

# (def-format-test format.@*.6
#   "~A~A~v@*~A~A" (1 2 1 3 4) "1221" 2)

def_format_test 'format.@*.6' =>
  '~A~A~v@*~A~A',
  [ 1, 2, 1, 3, 4 ],
  "1221",
  2;

# (def-format-test format.@*.7
#   "~A~A~v@*~A~A" (6 7 2 3 4) "6723" 1)

def_format_test 'format.@*.7' =>
  '~A~A~v@*~A~A',
  [ 6, 7, 2, 3, 4 ],
  "6723",
  1;

# (def-format-test format.@*.8
#   "~A~{~A~A~@*~A~A~}~A" (0 '(1 2) 9) "012129")

def_format_test 'format.@*.8' =>
  '~A~{~A~A~@*~A~A~}~A',
  [ 0, [ 1, 2 ], 9 ],
  "012129";

# (def-format-test format.@*.9
#   "~A~{~A~A~0@*~A~A~}~A" (0 '(1 2) 9) "012129")

def_format_test 'format.@*.9' =>
  '~A~{~A~A~0@*~A~A~}~A',
  [ 0, [ 1, 2 ], 9 ],
  "012129";

# (def-format-test format.@*.10
#   "~A~1{~A~A~v@*~A~A~}~A" (0 '(1 2 nil) 9) "012129")

def_format_test 'format.@*.10' =>
  '~A~1{~A~A~v@*~A~A~}~A',
  [ 0, [ 1, 2, undef ], 9 ],
  "012129";

# (def-format-test format.@*.11
#   "~A~{~A~A~1@*~A~}~A" (0 '(1 2) 9) "01229")

def_format_test 'format.@*.11' =>
  '~A~{~A~A~1@*~A~}~A',
  [ 0, [ 1, 2 ], 9 ],
  "01229";
}
