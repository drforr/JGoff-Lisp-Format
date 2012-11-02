#!perl

use Test::More tests => 1;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

my $f = JGoff::Lisp::Format->new;

def_format_test 'format.a.1' =>
  "~a", [ undef ], "UNDEF";

deftest 'format.a.2' => sub {
  # with-standard-io-syntax
  # XXX strictly speaking, *print-case* should be somewhere in core perl.
  # XXX but since it doesn't exist, make one up.
  # XXX
  local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::downcase;
  $f->format( undef, "~A", undef );
}, "undef";

deftest 'formatter.a.2' => sub {
  # with-standard-io-syntax
  local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::downcase;
  $f->formatter_call_to_string(
    $f->formatter( "~A" ),
    undef );
}, "nil";

deftest 'format.a.3' => sub {
  # with-standard-io-syntax
  local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::capitalize;
  $f->format( undef, "~a", undef );
}, "Undef";

deftest 'formatter.a.3' => sub {
  # with-standard-io-syntax
  local $JGoff::Lisp::Format::print_case = $JGoff::Lisp::Format::capitalize;
  $f->formatter_call_to_string(
    $f->formatter( "~a" ),
    undef );
}, "Undef";

def_format_test 'format.a.4' =>
  "~;a", [ undef ], "()";

def_format_test 'format.a.5' =>
  "~:A", [ [ undef ] ], "[UNDEF]";

#def_format_test 'format.a.6' => "~:A", [ [ undef ] ], "[UNDEF]";
#  "~:A" (#(nil)) "#(NIL)") # Perl doesn't really have the notion of symbols

def_format_test 'format.a.15' =>
  "~va", [ undef, undef ], "UNDEF";

def_format_test 'format.a.16' =>
  "~v:A", [ undef, undef ], "()";

def_format_test 'format.a.17' =>
  '~@a', [ undef ], "UNDEF";

def_format_test 'format.a.18' =>
  '~v@A', [ undef, undef ], "UNDEF";

def_format_test 'format.a.19' =>
  '~v:@a', [ undef, undef ], "()";

### With colinc specified

def_format_test 'format.a.20' =>
  '~v@:a', [ undef, undef ], "()";

def_format_test 'format.a.21' =>
  "~3,1a", [ undef ], "NIL";

def_format_test 'format.a.22' =>
  "~4,3a", [ undef ], "NIL   ";

def_format_test 'format.a.23' =>
  '~3,3@a', [ undef ], "NIL";

def_format_test 'format.a.24' =>
  '~4,4@a', [ undef ], "    NIL";

def_format_test 'format.a.25' =>
  '~5,3@a', [ undef ], "   NIL";

def_format_test 'format.a.26' =>
  "~5,3A", [ undef ], "NIL   ";

def_format_test 'format.a.27' =>
  '~7,3@a', [ undef ], "      NIL";

def_format_test 'format.a.28' =>
  "~7,3A", [ undef ], "NIL      ";

### With padchar

def_format_test 'format.a.30' =>
  "~3,,+2A",  [ "ABC" ], "ABC  ";

def_format_test 'format.a.31' =>
  "~3,,0A", [ "ABC" ], "ABC";

def_format_test 'format.a.32' =>
  "~3,,-1A", [ "ABC" ], "ABC";

def_format_test 'format.a.33' =>
  "~3,,0A", [ "ABCD" ], "ABCD";

def_format_test 'format.a.34' =>
  "~3,,-1A", [ "ABCD" ], "ABCD";

### With padchar

def_format_test 'format.a.35' =>
  "~4,,,'XA", [ "AB" ], "ABXX";

def_format_test 'format.a.36' =>
  "~4,,,a", [ "AB" ], "AB  ";

def_format_test 'format.a.37' =>
  q{~4,,,'X@a}, [ "AB" ], "XXAB";

def_format_test 'format.a.38' =>
  '~4,,,@A', [ "AB" ], "  AB";

def_format_test 'format.a.39' =>
  "~10,,,vA", [ undef, "abcde" ], "abcde     ";

def_format_test 'format.a.40' =>
  '~10,,,v@A', [ undef, "abcde" ], "     abcde";

def_format_test 'format.a.41' =>
  "~10,,,va", [ '*', "abcde" ], "abcde*****";

def_format_test 'format.a.42' =>
  '~10,,,v@a', [ '*', "abcde" ], "*****abcde";

### Other tests

def_format_test 'format.a.43' =>
  "~3,,vA", [ undef, "ABC" ], "ABC";

def_format_test 'format.a.45' =>
  "~4,,va", [ -1, "abcd" ], "abcd";

def_format_test 'format.a.46' =>
  "~5,vA", [ undef, "abc" ], "abc  ";

def_format_test 'format.a.47' =>
  "~5,vA", [ 3, "abc" ], "abc   ";

def_format_test 'format.a.48' =>
  '~5,v@A', [ 3, "abc" ], "   abc";

### # parameters

def_format_test 'format.a.49' =>
  "~#A", [ "abc", undef, undef, undef ], "abc ", 3;

def_format_test 'format.a.50' =>
  '~#@a', [ "abc", undef, undef, undef, undef, undef ], "   abc", 5;

def_format_test 'format.a.51' =>
  "~5,#a", [ "abc", undef, undef, undef ], "abc    ", 3;

def_format_test 'format.a.52' =>
  '~5,#@A', [ "abc", undef, undef, undef ], "    abc", 3;

def_format_test 'format.a.53' =>
  "~4,#A", [ "abc", undef, undef ], "abc   ", 2;

def_format_test 'format.a.54' =>
  '~4,#@A', [ "abc", undef, undef ], "   abc", 2;

def_format_test 'format.a.55' =>
  "~#,#A", [ "abc", undef, undef, undef ], "abc    ", 3;

def_format_test 'format.a.56' =>
  '~#,#@A', [ "abc", undef, undef, undef ], "    abc", 3;

def_format_test 'format.a.57' =>
  "~-100A", [ "xyz" ], "xyz";

def_format_test 'format.a.57' =>
  "~-100000000000000000000a", [ "xyz" ], "xyz";

=pod

(deftest format.a.7
  (let ((fn (formatter "~a")))
    (loop for c across +standard-chars+
          for s1 = (string c)
          for s2 = (format nil "~a" s1)
          for s3 = (formatter-call-to-string fn s1)
          unless (and (string= s1 s2) (string= s2 s3))
          collect (list c s1 s2 s3)))
  nil)

(deftest format.a.8
  (let ((fn (formatter "~A")))
    (loop with count = 0
          for i from 0 below (min #x10000 char-code-limit)
          for c = (code-char i)
          for s1 = (and c (string c))
          for s2 = (and c (format nil "~A" s1))
          for s3 = (and c (formatter-call-to-string fn s1))
          unless (or (null c) (string= s1 s2) (string= s2 s3))
          do (incf count) and collect (list c s1 s2 s3)
          when (> count 100) collect "count limit exceeded" and do (loop-finish)))
  nil)

(deftest format.a.9
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~d@a" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "NIL"
  "NIL"
  "NIL"
  " NIL"
  "  NIL"
  "   NIL"
  "    NIL"
  "     NIL"
  "      NIL"
  "       NIL")

(deftest format.a.10
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~da" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "NIL"
  "NIL"
  "NIL"
  "NIL "
  "NIL  "
  "NIL   "
  "NIL    "
  "NIL     "
  "NIL      "
  "NIL       ")

(deftest format.a.11
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~d@:A" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "()"
  "()"
  " ()"
  "  ()"
  "   ()"
  "    ()"
  "     ()"
  "      ()"
  "       ()"
  "        ()")

(deftest format.a.12
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~d:a" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "()"
  "()"
  "() "
  "()  "
  "()   "
  "()    "
  "()     "
  "()      "
  "()       "
  "()        ")

(deftest format.a.13
  (with-standard-io-syntax
   (apply
    #'values
    (let ((fn (formatter "~V:a")))
      (loop for i from 1 to 10
            for s = (format nil "~v:A" i nil)
            for s2 = (formatter-call-to-string fn i nil)
            do (assert (string= s s2))
            collect s))))
  "()"
  "()"
  "() "
  "()  "
  "()   "
  "()    "
  "()     "
  "()      "
  "()       "
  "()        ")

(deftest format.a.14
  (with-standard-io-syntax
   (apply
    #'values
    (let ((fn (formatter "~V@:A")))
      (loop for i from 1 to 10
            for s = (format nil "~v:@a" i nil)
            for s2 = (formatter-call-to-string fn i nil)
            do (assert (string= s s2))
            collect s))))
  "()"
  "()"
  " ()"
  "  ()"
  "   ()"
  "    ()"
  "     ()"
  "      ()"
  "       ()"
  "        ()")

;;; With padchar

(deftest format.a.29
  (let ((fn (formatter "~v,,2A")))
    (loop for i from -4 to 10
          for s = (format nil "~v,,2A" i "ABC")
          for s2 = (formatter-call-to-string fn i "ABC")
          do (assert (string= s s2))
          collect s))
  ("ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "
   "ABC       "))

(deftest format.a.44
  (let ((fn (formatter "~3,,vA")))
    (loop for i from 0 to 6
          for s =(format nil "~3,,vA" i "ABC")
          for s2 = (formatter-call-to-string fn i "ABC")
          do (assert (string= s s2))
          collect s))
  ("ABC"
   "ABC "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "))

(deftest format.a.44a
  (let ((fn (formatter "~3,,v@A")))
    (loop for i from 0 to 6
          for s = (format nil "~3,,v@A" i "ABC")
          for s2 = (formatter-call-to-string fn i "ABC")
          do (assert (string= s s2))
          collect s))
  ("ABC"
   " ABC"
   "  ABC"
   "   ABC"
   "    ABC"
   "     ABC"
   "      ABC"))

=cut
