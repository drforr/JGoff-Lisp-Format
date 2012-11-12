#!perl

use Test::More tests => 1;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

my $f = JGoff::Lisp::Format->new;

def_format_test 'format.page.1' =>
  "~0|",
  undef,
  "";

(deftest format.page.2
  (let ((s (format nil "~|")))
    (cond
     ((string= s "") nil)
     ((> (length s) 1) (values s :too-long))
     (t
      (let ((c (elt s 0)))
        (loop for i from 2 to 100
              for s = (format nil (format nil "~~~D|" i))
              unless (and (= (length s) i)
                          (every #'(lambda (c2) (char= c c2)) s))
              collect i)))))
  nil)

(deftest format.page.3
  (let ((s (format nil "~|")))
    (cond
     ((string= s "") nil)
     ((> (length s) 1) (values s :too-long))
     (t
      (let ((c (elt s 0)))
        (loop for i from 2 to 100
              for s = (format nil "~v|" i)
              unless (and (= (length s) i)
                          (every #'(lambda (c2) (char= c c2)) s))
              collect i)))))
  nil)

def_format_test 'format.page.4' =>
  "~V|",
  [ 0 ],
  "";

def_format_test 'format.page.5' =>
  "~v|",
  [ nil ],
  $f->format( undef, "~|" );
