#!perl

use Test::More tests => 61;

BEGIN {
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

# (def-format-test format.{.1
#   (concatenate 'string "~{~" (string #\Newline) "~}")
#   (nil) "")

def_format_test 'format.{.1' =>
  concatenate( "~{~", "\n", "~}" ),
  [ [ undef ] ],
  "";

# (def-format-test format.{.1a
#   "~{~}" ("" nil) "")

def_format_test 'format.{.1a' =>
  "~{~}",
  [ "", undef ],
  "";

# (def-format-test format.{.1b
#   "~0{~}" ("" '(1 2 3)) "")

def_format_test 'format.{.1b' =>
  "~0{~}",
  [ "", [ 1, 2, 3 ] ],
  "";

# (def-format-test format.{.2
#   "~{ ~}" (nil) "")

def_format_test 'format.{.2' =>
  "~{ ~}",
  [ undef ],
  "";

# (def-format-test format.{.3
#   "~{X Y Z~}" (nil) "")

def_format_test 'format.{.3' =>
  "~{X Y Z~}",
  [ undef ],
  "";

# (def-format-test format.{.4
#   "~{~A~}" ('(1 2 3 4)) "1234")

def_format_test 'format.{.4' =>
  "~{~A~}",
  [ [ 1 , 2, 3, 4 ] ],
  "1234";

# (def-format-test format.{.5
#   "~{~{~A~}~}" ('((1 2 3)(4 5)(6 7 8))) "12345678")

def_format_test 'format.{.5' =>
  "~{~{~A~}~}",
  [ [ [ 1, 2, 3 ], [ 4, 5 ], [ 6, 7, 8 ] ] ],
  "12345678";

# (def-format-test format.{.6
#   "~{~1{~A~}~}" ('((1 2 3)(4 5)(6 7 8))) "146")

def_format_test 'format.{.6' =>
  "~{~1{~A~}~}",
  [ [ [ 1, 2, 3 ], [ 4, 5 ], [ 6, 7, 8 ] ] ],
  "146";

# (def-format-test format.{.7
#   (concatenate 'string "~1{~" (string #\Newline) "~}") (nil) "")

def_format_test 'format.{.7' =>
  concatenate( "~1{~", "\n", "~}" ),
  [ undef ],
  "";

# (deftest format.{.8
#   (loop for i from 0 to 10
#         for s = (format nil "~v{~A~}" i '(1 2 3 4 5 6 7 8 9 0))
#         unless (string= s (subseq "1234567890" 0 i))
#         collect (list i s))
#   nil)

deftest 'format.{.8' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( 0 .. 10 ) {
    my $s = $f->format( undef, "~v{~A~}", $i, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 ] );
    unless ( $s eq subseq( "1234567890", 0, $i ) ) {
      collect( $remainder, $i, $s );
    }
  }
  $remainder;
}, [];

# (deftest formatter.{.8
#   (let ((fn (formatter "~V{~A~}")))
#     (loop for i from 0 to 10
#           for s = (formatter-call-to-string fn i '(1 2 3 4 5 6 7 8 9 0))
#           unless (string= s (subseq "1234567890" 0 i))
#           collect (list i s)))
#   nil)

deftest 'formatter.{.8' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~V{~A~}" );
  my $remainder = [];
  for my $i ( 0 .. 10 ) {
    my $s = formatter_call_to_string( $fn,
      $i, [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 ] );
    unless ( $s eq subseq( "1234567890", 0, $i ) ) {
      collect( $remainder, $i, $s );
    }
  }
  $remainder;
}, [];

# (def-format-test format.{.9
#   "~#{~A~}" ('(1 2 3 4 5 6 7) nil nil nil) "1234" 3)

def_format_test 'format.{.9' =>
  "~#{~A~}",
  [ [ 1, 2, 3, 4, 5, 6, 7 ], undef, undef, undef ],
  "1234",
  3;

### (missing tests involved ~^ and have been moved to format-circumflex.lsp
###  and renamed.)

# (def-format-test format.{.15
#   "~0{~}" ("~A" '(1 2 3)) "")

def_format_test 'format.{.15' =>
  "~0{~}",
  [ "~A", [ 1, 2, 3 ] ],
  "";

# (def-format-test format.{.16
#   "~1{~}" ("~A" '(4 5 6)) "4")

def_format_test 'format.{.16' =>
  "~1{~}",
  [ "~A", [ 4, 5, 6 ] ],
  "4";

# (deftest format.{.17
#   (format nil "~{~}" (formatter "") nil)
#   "")

deftest 'format.{.17' => sub {
  my $f = JGoff::Lisp::Format->new;
  $f->format( undef, "~{~}", $f->formatter( "" ), undef );
}, "";

# (deftest format.{.18
#   (format nil "~1{~}" (formatter "") '(1 2 3 4))
#   "")

deftest 'format.{.18' => sub { # XXX Vet the arguments
  my $f = JGoff::Lisp::Format->new;
  $f->format( undef, "~1{~}", $f->formatter( "" ), [ 1, 2, 3, 4 ] );
}, "";

# (deftest format.{.19
#   (format nil "~{~}" (formatter "~A") '(1 2 3 4))
#   "1234")

deftest 'format.{.19' => sub {
  my $f = JGoff::Lisp::Format->new;
  $f->format( undef, "~{~}", $f->formatter( "~A" ), [ 1, 2, 3, 4 ] );
}, "1234";

# (deftest format.{.20
#   (format nil "~3{~}" (formatter "~A") '(1 2 3 4))
#   "123")

deftest 'format.{.20' => sub {
  my $f = JGoff::Lisp::Format->new;
  $f->format( undef, "~3{~}", $f->formatter( "~A" ), [ 1, 2, 3, 4 ] );
}, "123";

# (def-format-test format.{.21
#   "~V{~}" (2 "~A" '(1 2 3 4 5)) "12")

def_format_test 'format.{.21' =>
  "~V{~}",
  [ 2, "~A", [ 1, 2, 3, 4, 5 ] ],
  "12";

# (def-format-test format.{.22
#   "~#{~}" ("~A" '(1 2 3 4 5)) "12")

def_format_test 'format.{.22' =>
  "~#{~}",
  [ "~A", [ 1, 2, 3, 4, 5 ] ],
  "12";

# (def-format-test format.{.23
#   "~{FOO~:}" (nil) "FOO")

def_format_test 'format.{.23' =>
  "~{FOO~:}",
  [ undef ],
  "FOO";

# (def-format-test format.{.24
#   "~{~A~:}" ('(1)) "1")

def_format_test 'format.{.24' =>
  "~{~A~:}",
  [ [ 1 ] ],
  "1";

# (def-format-test format.{.25
#   "~{~A~:}" ('(1 2)) "12")

def_format_test 'format.{.25' =>
  "~{~A~:}",
  [ [ 1, 2 ] ],
  "12";

# (def-format-test format.{.26
#   "~{~A~:}" ('(1 2 3)) "123")

def_format_test 'format.{.26' =>
  "~{~A~:}",
  [ [ 1, 2, 3 ] ],
  "123";

# (def-format-test format.{.27
#   "~0{FOO~:}" (nil) "")

def_format_test 'format.{.27' =>
  "~0{FOO~:}",
  [ undef ],
  "";

# (def-format-test format.{.28
#   "~V{FOO~:}" (0 nil) "")

def_format_test 'format.{.28' =>
  "~V{FOO~:}",
  [ 0, undef ],
  "";

# (def-format-test format.{.29
#   "~1{FOO~:}" (nil) "FOO")

def_format_test 'format.{.29' =>
  "~1{FOO~:}",
  [ undef ],
  "FOO";

# (def-format-test format.{.30
#   "~2{FOO~:}" (nil) "FOO")

def_format_test 'format.{.30' =>
  "~2{FOO~:}",
  [ undef ],
  "FOO";

# (def-format-test format.{.31
#   (concatenate 'string "~2{~" (string #\Newline) "~:}")
#   (nil) "")

def_format_test 'format.{.31' =>
  concatenate( "~2{~", "\n", "~:}" ),
  [ undef ],
  "";

# (def-format-test format.{.32
#   "~2{FOO~}" (nil) "")

def_format_test 'format.{.32' =>
  "~2{FOO~}",
  [ undef ],
  "";

# (def-format-test format.{.33
#   "~v{~a~}" (nil '(1 2 3 4 5 6 7)) "1234567")

def_format_test 'format.{.33' =>
  "~v{~a~}",
  [ undef, [ 1, 2, 3, 4, 5, 6, 7 ] ],
  "1234567";

### ~:{ ... ~}

SKIP: {
  diag "Make these tests work";
  skip 'Not ready yet', 4;
# (def-format-test format.\:{.1
#   "~:{(~A ~A)~}" ('((1 2 3)(4 5)(6 7 8))) "(1 2)(4 5)(6 7)")

def_format_test 'format.\:{.1' =>
  "~:{[~A ~A]~}",
  [ [ [ 1, 2, 3 ], [ 4, 5 ], [ 6, 7, 8 ] ] ], # It *is* 3 layers deep.
  "[1 2][4 5][6 7]";

# (def-format-test format.\:{.2
#   (concatenate 'string "~:{~" (string #\Newline) "~}")
#   (nil) "")

def_format_test 'format.\:{.2' =>
  concatenate( "~:{~", "\n", "~}" ),
  [ undef ],
  "";

# (def-format-test format.\:{.3
#   "~:{~}" ("" nil) "")

def_format_test 'format.\:{.3' =>
  "~:{~}",
  [ "", undef ],
  "";

# (def-format-test format.\:{.4
#   "~:{~}" ("~A" nil) "")

def_format_test 'format.\:{.4' =>
  "~:{~}",
  [ "~A", undef ],
  "";

# (def-format-test format.\:{.5
#   "~:{~}" ("X" '(nil (1 2) (3))) "XXX")

def_format_test 'format.\:{.5' =>
  "~:{~}",
  [ "X", [ undef, [ 1, 2 ], [ 3 ] ] ],
  "XXX";

# (deftest format.\:{.6
#   (format nil "~:{~}" (formatter "~A") '((1 2) (3) (4 5 6)))
#   "134")

deftest 'format.\:{.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  $f->format(
    undef,
    "~:{~}",
    $f->formatter( "~A" ), [ [ [ 1, 2 ], [ 3 ], [ 4, 5, 6 ] ] ]
  );
}, "134";

# (def-format-test format.\:{.7
#   "~0:{XYZ~}" ('((1))) "")

def_format_test 'format.\:{.7' =>
  "~0:{XYZ~}",
  [ [ [ 1 ] ] ],
  "";

# (def-format-test format.\:{.8
#   "~2:{XYZ~}" ('((1))) "XYZ")

def_format_test 'format.\:{.8' =>
  "~2:{XYZ~}",
  [ [ [ 1 ] ] ],
  "XYZ";

# (def-format-test format.\:{.9
#   "~2:{~A~}" ('((1) (2))) "12")

def_format_test 'format.\:{.9' =>
  "~2:{~A~}",
  [ [ [ 1 ], [ 2 ] ] ],
  "12";

# XXX Testing a property-list, close to a hash.
# (def-format-test format.\:{.10
#   "~2:{~A~}" ('((1 X) (2 Y) (3 Z))) "12")

def_format_test 'format.\:{.10' =>
  "~2:{~A~}",
  [ [ [ 1, 'X' ], [ 2, 'Y' ], [ 3, 'Z' ] ] ],
  "12";

# (deftest format.\:{.11
#   (loop for i from 0 to 10 collect
#         (format nil "~v:{~A~}" i '((1) (2) (3 X) (4 Y Z) (5) (6))))
#   ("" "1" "12" "123" "1234" "12345"
#    "123456" "123456" "123456" "123456" "123456"))

deftest 'format.\:{.11' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( 0 .. 10 ) {
    collect( $remainder,
      $f->format(
        undef,
        "~v:{~A~}",
        [ $i, [ [ 1 ], [ 2 ], [ 3, 'X' ], [ 4, 'Y', 'Z' ], [ 5 ], [ 6 ] ] ]
      )
    );
  }
  return $remainder;
}, [
  '',
  '1',
  '12',
  '123',
  '1234',
  '12345',
  '123456',
  '123456',
  '123456',
  '123456',
  '123456',
];

# (deftest formatter.\:{.11
#   (let ((fn (formatter "~v:{~A~}")))
#     (loop for i from 0 to 10 collect
#           (formatter-call-to-string fn i '((1) (2) (3 X) (4 Y Z) (5) (6)))))
#   ("" "1" "12" "123" "1234" "12345"
#    "123456" "123456" "123456" "123456" "123456"))

deftest 'format.\:{.11' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $fn = $f->formatter( "~v:{~A~}" );
  for my $i ( 0 .. 10 ) {
    collect( $remainder,
      formatter_call_to_list( $fn, $i,
        [ $i, [ [ 1 ], [ 2 ], [ 3, 'X' ], [ 4, 'Y', 'Z' ], [ 5 ], [ 6 ] ] ]
      )
    );
  }
  return $remainder;
}, [
  '',
  '1',
  '12',
  '123',
  '1234',
  '12345',
  '123456',
  '123456',
  '123456',
  '123456',
  '123456',
];

# (def-format-test format.\:{.12
#   "~V:{X~}" (nil '((1) (2) (3) nil (5))) "XXXXX")

def_format_test 'format.\:{.12' =>
  "~V:{X~}",
  [ undef, [ [ 1 ], [ 2 ], [ 3 ], undef, [ 5 ] ] ],
  "XXXXX";

# (def-format-test format.\:{.13
#   "~#:{~A~}" ('((1) (2) (3) (4) (5)) 'foo 'bar) "123" 2)

def_format_test 'format.\:{.13' =>
  "~#:{~A~}",
  [ [ [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 5 ] ], "foo", "bar" ],
  "123",
  2;

# (def-format-test format.\:{.14
#   "~:{~A~:}" ('((1 X) (2 Y) (3) (4 A B))) "1234")

def_format_test 'format.\:{.14' =>
  "~:{~A~:}",
  [ [ [ 1, 'X' ], [ 2, 'Y' ], [ 3 ], [ 4, 'A', 'B' ] ] ],
  "1234";

# (deftest format.\:{.15
#   (loop for i from 0 to 10 collect
#         (format nil "~v:{~A~:}" i '((1 X) (2 Y) (3) (4 A B))))
#   ("" "1" "12" "123" "1234" "1234"
#    "1234" "1234" "1234" "1234" "1234"))

deftest 'format.\:{.15' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( 0 .. 10 ) {
    collect( $remainder,
      $f->format(
        undef,
        "~v:{~A~:}",
        [ $i, [ [ 1, 'X' ], [ 2, 'Y' ], [ 3 ], [ 4, 'A', 'B' ] ] ]
      )
    );
  }
  return $remainder;
}, [
  '',
  '1',
  '12',
  '123',
  '1234',
  '1234',
  '1234',
  '1234',
  '1234',
  '1234',
  '1234',
];

# (deftest formatter.\:{.15
#   (let ((fn (formatter "~v:{~A~:}")))
#     (loop for i from 0 to 10 collect
#           (formatter-call-to-string fn i '((1 X) (2 Y) (3) (4 A B)))))
#   ("" "1" "12" "123" "1234" "1234"
#    "1234" "1234" "1234" "1234" "1234"))

deftest 'formatter.\:{.15' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  my $fn = $f->formatter( "~v:{~A~:}" );
  for my $i ( 0 .. 10 ) {
    collect( $remainder,
      formatter_call_to_string(
        $fn, $i, [ [ 1, 'X' ], [ 2, 'Y' ], [ 3 ], [ 4, 'A', 'B' ] ]
      )
    );
  }
  return $remainder;
}, [
  '',
  '1',
  '12',
  '123',
  '1234',
  '1234',
  '1234',
  '1234',
  '1234',
  '1234',
  '1234',
];

# (def-format-test format.\:{.16
#   "~:{ABC~:}" ('(nil)) "ABC")

def_format_test 'format.\:{.16' =>
  "~:{ABC~:}",
  [ [ undef ] ],
  "ABC";

# (def-format-test format.\:{.17
#   "~v:{ABC~:}" (nil '(nil)) "ABC")

def_format_test 'format.\:{.17' =>
  "~v:{ABC~:}",
  [ undef, [ undef ] ],
  "ABC";

### Tests of ~@{ ... ~}

# (def-format-test format.@{.1
#   (concatenate 'string "~@{~" (string #\Newline) "~}")
#   nil "")

def_format_test 'format.@{.1' =>
  concatenate( '~@{~', "\n", "~}" ),
  undef,
  "";

# (def-format-test format.@{.1A
#   "~@{~}" ("") "")

def_format_test 'format.@{.1A' =>
  '~@{~}',
  [ "" ],
  "";

# (def-format-test format.@{.2
#   "~@{ ~}" nil "")

def_format_test 'format.@{.2' =>
  '~@{ ~}',
  undef,
  "";

# (def-format-test format.@{.3
#   "~@{X ~A Y Z~}" (nil) "X NIL Y Z")

def_format_test 'format.@{.3' =>
  '~@{X ~A Y Z~}',
  [ undef ],
  "X UNDEF Y Z";

# (def-format-test format.@{.4
#   "~@{~A~}" (1 2 3 4) "1234")

def_format_test 'format.@{.4' =>
  '~@{~A~}',
  [ 1, 2, 3, 4 ],
  "1234";

# XXX This is a list of quoted lists here, maybe something going on?
# (def-format-test format.@{.5
#   "~@{~{~A~}~}" ('(1 2 3) '(4 5) '(6 7 8)) "12345678")

def_format_test 'format.@{.5' =>
  '~@{~{~A~}~}',
  [ [ 1, 2, 3 ], [ 4, 5 ], [ 6, 7, 8 ] ],
  "12345678";

# (def-format-test format.@{.6
#   "~@{~1{~A~}~}" ('(1 2 3) '(4 5) '(6 7 8)) "146")

def_format_test 'format.@{.6' =>
  '~@{~1{~A~}~}',
  [ [ 1, 2, 3 ], [ 4, 5 ], [ 6, 7, 8 ] ],
  "146";

# (def-format-test format.@{.7
#   "~1@{FOO~}" nil "")

def_format_test 'format.@{.7' =>
  '~1@{FOO~}',
  undef,
  "";

# (def-format-test format.@{.8
#   "~v@{~A~}" (nil 1 4 7) "147")

def_format_test 'format.@{.8' =>
  '~v@{~A~}',
  [ undef, 1, 4, 7 ],
  "147";

# (def-format-test format.@{.9
#   "~#@{~A~}" (1 2 3) "123")

def_format_test 'format.@{.9' =>
  '~#@{~A~}',
  [ 1, 2, 3 ],
  "123";

SKIP: {
  my $count = 2;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

# (deftest format.@{.10
#   (loop for i from 0 to 10
#         for x = nil then (cons i x)
#         collect (apply #'format nil "~v@{~A~}" i (reverse x)))
#   ("" "1" "12" "123" "1234" "12345"
#    "123456" "1234567" "12345678" "123456789" "12345678910"))

#(deftest formatter.@{.10
#  (let ((fn (formatter "~v@{~A~}")))
#    (loop for i from 0 to 10
#          for x = nil then (cons i x)
#          for rest = (list 'a 'b 'c)
#          collect
#          (with-output-to-string
#            (s)
#            (assert (equal (apply fn s i (append (reverse x) rest)) rest)))))
#  ("" "1" "12" "123" "1234" "12345"
#   "123456" "1234567" "12345678" "123456789" "12345678910"))
}

# (def-format-test format.@{.11
#   "~@{X~:}" nil "X")

def_format_test 'format.@{.11' =>
  '~@{X~:}',
  undef,
  'X';

#def_format_test format.@{.12
#  "~@{~}" ((formatter "X~AY") 1) "X1Y")

def_format_test 'format.@{.12' => # XXX double-check
  '~@{~}',
  #[ $f->formatter( "X~AY" ), 1 ],
  [ JGoff::Lisp::Format->new->formatter( "X~AY" ), 1 ],
  "X1Y";

#def_format_test format.@{.13
#  "~v@{~}" (1 (formatter "X") 'foo) "X" 1)

def_format_test 'format.@{.13' => # XXX double-check
  '~v@{~}',
  #[ 1, $f->formatter( "X" ), "foo" ],
  [ 1, JGoff::Lisp::Format->new->formatter( "X" ), "foo" ],
  "X",
  1;
}

### ~:@{

# (def-format-test format.\:@{.1
#   (concatenate 'string "~:@{~" (string #\Newline) "~}")
#   nil "")

def_format_test 'format.\:@{.1' => # }
  concatenate( '~:@{~', "\n", "~}" ),
  undef,
  "";

SKIP: {
  my $count = 8;
  my $str = "$count tests not working yet";
  diag $str; skip $str, $count;

# (def-format-test format.\:@{.2 ;;; }
#   "~:@{~A~}" ('(1 2) '(3) '(4 5 6)) "134")

def_format_test 'format.\:@{.2' => # }
  '~:@{~A~}',
  [ [ 1, 2 ], [ 3 ], [ 4, 5, 6 ] ],
  "134";

# XXX Almost forgot, another '(1 2) '(3 4) list.
# (def-format-test format.\:@{.3 ;;; }
#   "~:@{(~A ~A)~}" ('(1 2 4) '(3 7) '(4 5 6)) "(1 2)(3 7)(4 5)")

def_format_test 'format.\:@{.3' => # }
  '~:@{(~A ~A)~}',
  [ [ 1, 2, 4 ], [ 3, 7 ], [ 4, 5, 6 ] ],
  "(1 2)(3 7)(4 5)";

# (def-format-test format.\:@{.4 ;;; }
#   "~:@{~}" ("(~A ~A)" '(1 2 4) '(3 7) '(4 5 6)) "(1 2)(3 7)(4 5)")

def_format_test 'format.\:@{.4' => # }
  '~:@{~}',
  [ "(~A ~A)", [ 1, 2, 4 ], [ 3, 7 ], [ 4, 5, 6 ] ],
  "(1 2)(3 7)(4 5)";

# (def-format-test format.\:@{.5 ;;; }
#   "~:@{~}" ((formatter "(~A ~A)") '(1 2 4) '(3 7) '(4 5 6)) "(1 2)(3 7)(4 5)")

def_format_test 'format.\:@{.5' => # }
  '~:@{~}',
  [ [ JGoff::Lisp::Format->new->formatter( "(~A ~A)" ) ],
    [ 1, 2, 4 ], [ 3, 7 ], [ 4, 5, 6 ] ],
  "(1 2)(3 7)(4 5)";

# (def-format-test format.\:@.6
#   "~:@{~A~:}" ('(1 A) '(2 B) '(3) '(4 C D)) "1234")

def_format_test 'format.\:@.6' =>
  '~:@{~A~:}',
  [ [ 1, 'A' ], [ 2, 'B' ], [ 3 ], [ 4, 'C', 'D' ] ],
  "1234";

# (def-format-test format.\:@.7
#   "~0:@{~A~:}" ('(1 A) '(2 B) '(3) '(4 C D)) "" 4)

def_format_test 'format.\:@.7' =>
  '~0:@{~A~:}',
  [ [ 1, 'A' ], [ 2, 'B' ], [ 3 ], [ 4, 'C', 'D' ] ],
  "",
  4;

# (def-format-test format.\:@.8
#   "~#:@{A~:}" (nil nil nil) "AAA")

def_format_test 'format.\:@.8' =>
  '~#:@{A~:}',
  [ undef, undef, undef ],
  "AAA";

# (def-format-test format.\:@.9
#   "~v:@{~A~}" (nil '(1) '(2) '(3)) "123")

def_format_test 'format.\:@.9' =>
  '~v:@{~A~}',
  [ undef, [ 1 ], [ 2 ], [ 3 ] ],
  "123";
}

SKIP: {
  my $count = 16;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.\:@.10
#  (loop for i from 0 to 10
#        for x = nil then (cons (list i) x)
#        collect
#        (apply #'format nil "~V:@{~A~}" i (reverse x)))
#  ("" "1" "12" "123" "1234" "12345" "123456" "1234567" "12345678"
#   "123456789" "12345678910"))

#(deftest formatter.\:@.10
#  (let ((fn (formatter "~V@:{~A~}")))
#    (loop for i from 0 to 10
#          for x = nil then (cons (list i) x)
#          for rest = (list 'a 'b)
#          collect
#          (with-output-to-string
#            (s)
#            (assert (equal (apply fn s i (append (reverse x) rest)) rest)))))
#  ("" "1" "12" "123" "1234" "12345" "123456" "1234567" "12345678"
#   "123456789" "12345678910"))

### Error tests

#(deftest format.{.error.1 ;;; }
#  (signals-type-error x 'A (format nil "~{~A~}" x))
#  t)

#(deftest format.{.error.2 ;;; }
#  (signals-type-error x 1 (format nil "~{~A~}" x))
#  t)

#(deftest format.{.error.3 ;;; }
#  (signals-type-error x "foo" (format nil "~{~A~}" x))
#  t)

#(deftest format.{.error.4 ;;; }
#  (signals-type-error x #*01101 (format nil "~{~A~}" x))
#  t)

#(deftest format.{.error.5 ;;; }
#  (signals-error (format nil "~{~A~}" '(x y . z)) type-error)
#  t)

#(deftest format.\:{.error.1 ;;; }
#  (signals-error (format nil "~:{~A~}" '(x)) type-error)
#  t)

#(deftest format.\:{.error.2 ;;; }
#  (signals-type-error x 'x (format nil "~:{~A~}" x))
#  t)

#(deftest format.\:{.error.3 ;;; }
#  (signals-error (format nil "~:{~A~}" '((x) . y)) type-error)
#  t)

#(deftest format.\:{.error.4 ;;; }
#  (signals-error (format nil "~:{~A~}" '("X")) type-error)
#  t)

#(deftest format.\:{.error.5 ;;; }
#  (signals-error (format nil "~:{~A~}" '(#(X Y Z))) type-error)
#  t)

#(deftest format.\:@{.error.1 ;;; }
#  (signals-type-error x 'x (format nil "~:@{~A~}" x))
#  t)

#(deftest format.\:@{.error.2 ;;; }
#  (signals-type-error x 0 (format nil "~:@{~A~}" x))
#  t)

#(deftest format.\:@{.error.3 ;;; }
#  (signals-type-error x #*01101 (format nil "~:@{~A~}" x))
#  t)

#(deftest format.\:@{.error.4 ;;; }
#  (signals-type-error x "abc" (format nil "~:@{~A~}" x))
#  t)

#(deftest format.\:@{.error.5 ;;; }
#  (signals-error (format nil "~:@{~A ~A~}" '(x . y)) type-error)
#  t)
}
