#!perl

use Test::More tests => 1;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

def_format_test 'format.p.1' =>
  "~p",
  [ 1 ],
  "";

def_format_test 'format.p.2' =>
  "~P",
  [ 2 ],
  "s";

def_format_test 'format.p.3' =>
  "~p",
  [ 0 ],
  "s";

def_format_test 'format.p.4' =>
  "~P",
  [ 1.0 ],
  "s";

#(deftest format.p.5
#  (loop for x in *universe*
#        for s = (format nil "~p" x)
#        unless (or (eql x 1) (string= s "s"))
#        collect (list x s))
#  nil)

#(deftest formatter.p.5
#  (let ((fn (formatter "~p")))
#    (loop for x in *universe*
#          for s = (formatter-call-to-string fn x)
#          unless (or (eql x 1) (string= s "s"))
#          collect (list x s)))
#  nil)

### :p

def_format_test 'format.p.6' =>
  "~D cat~:P",
  [ 1 ],
  "1 cat";

def_format_test 'format.p.7' =>
  "~D cat~:p",
  [ 2 ],
  "2 cats";

def_format_test 'format.p.8' =>
  "~D cat~:P",
  [ 0 ],
  "0 cats";

def_format_test 'format.p.9' =>
  "~D cat~:p",
  [ "No" ],
  "No cats";

### :@p

def_format_test 'format.p.10' =>
  '~D penn~:@P',
  [ 1 ],
  "1 penny";

def_format_test 'format.p.11' =>
  '~D penn~:@p',
  [ 2 ],
  "2 pennies";

def_format_test 'format.p.12' =>
  '~D penn~@:P',
  [ 0 ],
  "0 pennies";

def_format_test 'format.p.13' =>
  '~D penn~@:p',
  [ "No" ],
  "No pennies";

### @p

def_format_test 'format.p.14' =>
  '~@p',
  [ 1 ],
  "y";

def_format_test 'format.p.15' =>
  '~@P',
  [ 2 ],
  "ies";

def_format_test 'format.p.16' =>
  '~@p',
  [ 0 ],
  "ies";

def_format_test 'format.p.17' =>
  '~@P',
  [ 1.0 ],
  "ies";

#(deftest format.p.18
#  (loop for x in *universe*
#        for s = (format nil "~@p" x)
#        unless (or (eql x 1) (string= s "ies"))
#        collect (list x s))
#  nil)

#(deftest formatter.p.18
#  (let ((fn (formatter "~@P")))
#    (loop for x in *universe*
#          for s = (formatter-call-to-string fn x)
#          unless (or (eql x 1) (string= s "ies"))
#          collect (list x s)))
#  nil)
