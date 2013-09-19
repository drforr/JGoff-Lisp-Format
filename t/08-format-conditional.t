#!perl

use Test::More tests => 39;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

SKIP: {
  my $count = 6;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

# (def-format-test format.cond.1
#   "~[~]" (0) "")

def_format_test 'format.cond.1' =>
  "~[~]",
   [ 0 ],
   "";

# (def-format-test format.cond.2
#   "~[a~]" (0) "a")

def_format_test 'format.cond.2' =>
  "~[a~]",
   [ 0 ],
   "a";

# (def-format-test format.cond.3
#   "~[a~]" (-1) "")

def_format_test 'format.cond.3' =>
  "~[a~]",
   [ -1 ],
   "";

# (def-format-test format.cond.4
#   "~[a~]" ((1- most-negative-fixnum)) "")

def_format_test 'format.cond.4' =>
  "~[a~]",
   [ $JGoff::Lisp::Format::most_negative_fixnum - 1 ],
   "";

# (def-format-test format.cond.5
#   "~[a~]" (1) "")

def_format_test 'format.cond.5' =>
  "~[a~]",
   [ 1 ],
   "";

# (def-format-test format.cond.6
#   "~[a~]" ((1+ most-positive-fixnum)) "")

def_format_test 'format.cond.6' =>
  "~[a~]",
   [ $JGoff::Lisp::Format::most_positive_fixnum + 1 ],
   "";
}

SKIP: {
  my $count = 2;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.cond.7
#  (loop for i from -1 to 10
#        collect (format nil "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i))
#  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

deftest 'format.cond.7' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             $f->format( undef, "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]", $i ) );
  }
  return $remainder;
}, [
  "",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "",
  ""
];

#(deftest formatter.cond.7
#  (let ((fn (formatter "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
#    (loop for i from -1 to 10
#          collect (formatter-call-to-string fn i)))
#  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

deftest 'formatter.cond.7' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]" );
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             formatter_call_to_strng( $fn, $i ) );
  }
  return $remainder;
}, [
  "",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "",
  ""
];

}

SKIP: {
  my $count = 5;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

# (def-format-test format.cond.8
#   "~0[a~;b~;c~;d~]" (3) "a" 1)

def_format_test 'format.cond.8' =>
  "~0[a~;b~;c~;d~]",
   [ 3 ],
   "a",
   1;

# (def-format-test format.cond.9
#   "~-1[a~;b~;c~;d~]" (3) "" 1)

def_format_test 'format.cond.9' =>
  "~-1[a~;b~;c~;d~]",
   [ 3 ],
   "",
   1;

# (def-format-test format.cond.10
#   "~1[a~;b~;c~;d~]" (3) "b" 1)

def_format_test 'format.cond.10' =>
  "~1[a~;b~;c~;d~]",
   [ 3 ],
   "b",
   1;

# (def-format-test format.cond.11
#   "~4[a~;b~;c~;d~]" (3) "" 1)

def_format_test 'format.cond.11' =>
  "~4[a~;b~;c~;d~]",
   [ 3 ],
   "",
   1;

# (def-format-test format.cond.12
#   "~100000000000000000000000000000000[a~;b~;c~;d~]" (3) "" 1)

def_format_test 'format.cond.12' =>
  "~100000000000000000000000000000000[a~;b~;c~;d~]",
   [ 3 ],
   "",
   1;
}

SKIP: {
  my $count = 4;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.cond.13
#  (loop for i from -1 to 10
#        collect (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i nil))
#  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

deftest 'format.cond.13' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             $f->format( undef, "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]", $i, undef ) );
  }
  return $remainder;
}, [
  "",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "",
  ""
];

#(deftest formatter.cond.13
#  (let ((fn (formatter "~V[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
#    (loop for i from -1 to 10
#          collect (formatter-call-to-string fn i)))
#  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))


deftest 'formatter.cond.13' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~V[a~;b~;c~;d~;e~;f~;g~;h~;i~]" );
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             formatter_call_to_strng( $fn, $i ) );
  }
  return $remainder;
}, [
  "",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "",
  ""
];

#(deftest format.cond.14
#  (loop for i from -1 to 10
#        collect (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" nil i))
#  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

deftest 'format.cond.14' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             $f->format( undef, "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]", undef, $i ) );
  }
  return $remainder;
}, [
  "",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "",
  ""
];

#(deftest formatter.cond.14
#  (let ((fn (formatter "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
#    (loop for i from -1 to 10
#          collect (formatter-call-to-string fn nil i)))
#  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

deftest 'formatter.cond.14' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" );
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             formatter_call_to_strng( $fn, undef, $i ) );
  }
  return $remainder;
}, [
  "",
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "",
  ""
];
}

SKIP: {
  my $count = 2;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

# (def-format-test format.cond.15
#   "~#[A~;B~]" nil "A")

def_format_test 'format.cond.15' =>
  "~#[A~;B~]",
   undef,
   "A";

# (def-format-test format.cond.16
#   "~#[A~;B~]" (nil) "B" 1)

def_format_test 'format.cond.16' =>
  "~#[A~;B~]",
   [ undef ],
   "B",
   1;
}

### ~[ .~:;  ~]

SKIP: {
  my $count = 2;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.cond\:.1
#  (loop for i from -100 to 100
#        for s = (format nil "~[~:;a~]" i)
#        unless (or (zerop i) (string= s "a"))
#        collect (list i s))
#  nil)

deftest 'format.cond\:.1' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -100 .. 100 ) {
    my $s = $f->format( undef, "~[~:;a~]", $i );
    unless ( ( $i == 0 ) or
             ( $s eq "a" ) ) {
      collect( $remainder, list( $i, $s ) );
    }
  }
  return $remainder;
}, [];

#(deftest formatter.cond\:.1
#  (let ((fn (formatter "~[~:;a~]")))
#    (loop for i from -100 to 100
#          for s = (formatter-call-to-string fn i)
#          unless (or (zerop i) (string= s "a"))
#          collect (list i s)))
#  nil)

deftest 'formatter.cond\:.1' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~[~:;a~]" );
  my $remainder = [];
  for my $i ( -100 .. 100 ) {
    my $s = formatter_call_to_string( $fn, $i );
    unless ( ( $i == 0 ) or
             ( $s eq "a" ) ) {
      collect( $remainder, list( $i, $s ) );
    }
  }
  return $remainder;
}, [];

}

SKIP: {
  my $count = 3;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

# (def-format-test format.cond\:.2
#   "~[a~:;b~]" (0) "a")

def_format_test 'format.cond\:.2' =>
  "~[a~:;b~]",
   [ 0 ],
   "a";

# (def-format-test format.cond\:.3
#   "~[a~:;b~]" ((1- most-negative-fixnum)) "b")

def_format_test 'format.cond\:.3' =>
  "~[a~:;b~]",
   [ $JGoff::Lisp::Format::most_negative_fixnum - 1 ],
   "b";

# (def-format-test format.cond\:.4
#   "~[a~:;b~]" ((1+ most-positive-fixnum)) "b")

def_format_test 'format.cond\:.4' =>
  "~[a~:;b~]",
   [ $JGoff::Lisp::Format::most_positive_fixnum + 1 ],
   "b";
}

SKIP: {
  my $count = 6;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.cond\:.5
#  (loop for i from -1 to 10
#        collect (format nil "~[a~;b~;c~;d~:;e~]" i))
#  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

deftest 'format.cond\:.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder, $f->format( undef, "~[a~;b~;c~;d~:;e~]", $i ) );
  }
  return $remainder;
}, [ "e", "a", "b", "c", "d", "e", "e", "e", "e", "e", "e", "e" ];

#(deftest formatter.cond\:.5
#  (let ((fn (formatter "~[a~;b~;c~;d~:;e~]")))
#    (loop for i from -1 to 10
#          collect (formatter-call-to-string fn i)))
#  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

deftest 'formatter.cond\:.5' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~[a~;b~;c~;d~:;e~]" );
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder, formatter_call_to_string( $fn, $i ) );
  }
  return $remainder;
}, [ "e", "a", "b", "c", "d", "e", "e", "e", "e", "e", "e", "e" ];

#(deftest format.cond\:.6
#  (loop for i from -1 to 10
#        collect (format nil "~v[a~;b~;c~;d~:;e~]" i nil))
#  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

deftest 'format.cond\:.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             $f->format( undef, "~v[a~;b~;c~;d~:;e~]", $i, undef ) );
  }
  return $remainder;
}, [ "e", "a", "b", "c", "d", "e", "e", "e", "e", "e", "e", "e" ];

#(deftest formatter.cond\:.6
#  (let ((fn (formatter "~v[a~;b~;c~;d~:;e~]")))
#    (loop for i from -1 to 10
#          collect (formatter-call-to-string fn i)))
#  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

deftest 'formatter.cond\:.6' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~v[a~;b~;c~;d~:;e~]" );
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder, formatter_call_to_string( $fn, $i ) );
  }
  return $remainder;
}, [ "e", "a", "b", "c", "d", "e", "e", "e", "e", "e", "e", "e" ];

#(deftest format.cond\:.7
#  (loop for i from -1 to 10
#        collect (format nil "~v[a~;b~;c~;d~:;e~]" nil i))
#  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

deftest 'format.cond\:.7' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder,
             $f->format( undef, "~v[a~;b~;c~;d~:;e~]", undef, $i ) );
  }
  return $remainder;
}, [ "e", "a", "b", "c", "d", "e", "e", "e", "e", "e", "e", "e" ];

#(deftest formatter.cond\:.7
#  (let ((fn (formatter "~v[a~;b~;c~;d~:;e~]")))
#    (loop for i from -1 to 10
#          collect (formatter-call-to-string fn nil i)))
#  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

deftest 'formatter.cond\:.7' => sub {
  my $f = JGoff::Lisp::Format->new;
  my $fn = $f->formatter( "~v[a~;b~;c~;d~:;e~]" );
  my $remainder = [];
  for my $i ( -1 .. 10 ) {
    collect( $remainder, formatter_call_to_string( $fn, undef, $i ) );
  }
  return $remainder;
}, [ "e", "a", "b", "c", "d", "e", "e", "e", "e", "e", "e", "e" ];

}

SKIP: {
  my $count = 3;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

# (def-format-test format.cond\:.8
#   "~#[A~:;B~]" nil "A")

def_format_test 'format.cond\:.8' =>
  "~#[A~:;B~]",
   undef,
   "A";

# (def-format-test format.cond\:.9
#   "~#[A~:;B~]" (nil nil) "B" 2)

def_format_test 'format.cond\:.9' =>
  "~#[A~:;B~]",
   [ undef, undef ],
   "B",
   2;

### ~:[...~]

# (def-format-test format.\:cond.1
#   "~:[a~;b~]" (nil) "a")

def_format_test 'format.\:cond.1' =>
  "~:[a~;b~]",
   [ undef ],
   "a";
}

SKIP: {
  my $count = 2;
  my $str = "$count tests not implemented yet";
  diag $str; skip $str, $count;

#(deftest format.\:cond.2
#  (loop for x in *mini-universe*
#        for s = (format nil "~:[a~;b~]" x)
#        when (and x (not (string= s "b")))
#        collect (list x s))
#  nil)

#(deftest formatter.\:cond.2
#  (let ((fn (formatter "~:[a~;b~]")))
#    (loop for x in *mini-universe*
#          for s = (formatter-call-to-string fn x)
#          when (and x (not (string= s "b")))
#          collect (list x s)))
#  nil)
}

### ~@[ ... ~]

SKIP: {
  my $count = 2;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

# (def-format-test format.@cond.1
#   "~@[X~]Y~A" (1) "XY1")

def_format_test 'format.@cond.1' =>
  "~@[X~]Y~A",
   [ 1 ],
   "XY1";

# (def-format-test format.@cond.2
#   "~@[X~]Y~A" (nil 2) "Y2")

def_format_test 'format.@cond.2' =>
   "~@[X~]Y~A" ,
   [ undef, 2 ],
   "Y2";

}
