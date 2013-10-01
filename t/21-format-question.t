#!perl

use Test::More tests => 12;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

# (def-format-test format.?.1
#   "~?" ("" nil) "")

def_format_test 'format.?.1' =>
  "~?",
  [ "", undef ],
  "";

# (def-format-test format.?.2
#   "~?" ("~A" '(1)) "1")

def_format_test 'format.?.2' =>
  "~?",
  [ "~A", [ 1 ] ],
  "1";

# (def-format-test format.?.3
#   "~?" ("" '(1)) "")

def_format_test 'format.?.3' =>
  "~?",
  [ "", [ 1 ] ],
  "";

# (def-format-test format.?.4
#   "~? ~A" ("" '(1) 2) " 2")

def_format_test 'format.?.4' =>
  "~? ~A",
  [ "", [ 1 ], 2 ],
  " 2";

# (def-format-test format.?.5
#   "a~?z" ("b~?y" '("c~?x" ("~A" (1)))) "abc1xyz")

def_format_test 'format.?.5' =>
  "a~?z",
  [ "b~?y", [ "c~?x", [ "~A", [ 1 ] ] ] ],
  "abc1xyz";

### Tests of ~@?

# (def-format-test format.@?.1
#   "~@?" ("") "")

def_format_test 'format.@?.1' =>
  "~@?",
  [ "" ],
  "";

# (def-format-test format.@?.2
#   "~@?" ("~A" 1) "1")

def_format_test 'format.@?.2' =>
  "~@?",
  [ "~A", 1 ],
  "1";

# (def-format-test format.@?.3
#   "~@? ~A" ("<~A>" 1 2) "<1> 2")

def_format_test 'format.@?.3' =>
  "~@? ~A",
  [ "<~A>", 1, 2 ],
  "<1> 2";

# (def-format-test format.@?.4
#   "a~@?z" ("b~@?y" "c~@?x" "~A" 1) "abc1xyz")

def_format_test 'format.@?.4' =>
  "a~@?z",
  [ "b~@?y", "c~@?x", "~A", 1 ],
  "abc1xyz";

SKIP: {
  my $count = 1;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;
# (def-format-test format.@?.5
#   "~{~A~@?~A~}" ('(1 "~4*" 2 3 4 5 6)) "16")

def_format_test 'format.@?.5' =>
  "~{~A~@?~A~}",
  [ [ 1, "~4*", 2, 3, 4, 5, 6 ] ],
  "16";
}
