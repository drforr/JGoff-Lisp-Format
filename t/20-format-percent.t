#!perl

use Test::More tests => 5;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

#SKIP: {
#  diag "Make these tests work";
#  skip 'Not ready yet', 3;
def_format_test 'format.%.1' =>
  "~%",
  undef,
  "\n";

#(deftest format.%.2
#  (loop for i from 0 to 100
#        for s1 = (make-string i :initial-element #\Newline)
#        for format-string = (format nil "~~~D%" i)
#        for s2 = (format nil format-string)
#        for fn = (eval `(formatter ,s2))
#        for s3 = (formatter-call-to-string fn)
#        unless (and (string= s1 s2) (string= s1 s3))
#        collect i)
#  nil)

def_format_test 'format.%.3' =>
  "~v%",
  [ undef ],
  "\n";

def_format_test 'format.%.4' =>
  "~V%",
  [ 1 ],
  "\n";

#(deftest format.%.5
#  (loop for i from 0 to 100
#        for s1 = (make-string i :initial-element #\Newline)
#        for s2 = (format nil "~v%" i)
#        unless (string= s1 s2)
#        collect i)
#  nil)

#(deftest formatter.%.5
#  (let ((fn (formatter "~v%")))
#    (loop for i from 0 to 100
#          for s1 = (make-string i :initial-element #\Newline)
#          for s2 = (formatter-call-to-string fn i)
#          unless (string= s1 s2)
#          collect i))
#  nil)

#(deftest format.%.6
#  (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#        for args = (make-list i)
#        for s1 = (make-string i :initial-element #\Newline)
#        for s2 = (apply #'format nil "~#%" args)
#        unless (string= s1 s2)
#        collect i)
#  nil)

#(deftest formatter.%.6
#  (let ((fn (formatter "~#%")))
#    (loop for i from 0 to (min (- call-arguments-limit 3) 100)
#          for args = (make-list i)
#          for s1 = (make-string i :initial-element #\Newline)
#          for s2 = (with-output-to-string
#                     (stream)
#                     (assert (equal (apply fn stream args) args)))
#          unless (string= s1 s2)
#          collect i))
#  nil)

#}
