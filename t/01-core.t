#!perl

use Test::More tests => 223;

BEGIN {
    use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!\n";
}

use strict;
use warnings;

my $p = JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );

sub parse_deeply {
  my ( $str, $res ) = @_;
  is_deeply( $p->from_string( $str ), [ $res ], qq{q{$str}} );
}

parse_deeply q{~a} => { format => '~a' };
parse_deeply q{~A} => { format => '~a' };
parse_deeply q{~0a} => { format => '~a', arguments => [ 0 ] };
parse_deeply q{~1a} => { format => '~a', arguments => [ 1 ] };
parse_deeply q{~9a} => { format => '~a', arguments => [ 9 ] };
parse_deeply q{~99a} => { format => '~a', arguments => [ 99 ] };
parse_deeply q{~+0a} => { format => '~a', arguments => [ 0 ] };
parse_deeply q{~+1a} => { format => '~a', arguments => [ 1 ] };
parse_deeply q{~+9a} => { format => '~a', arguments => [ 9 ] };
parse_deeply q{~+99a} => { format => '~a', arguments => [ 99 ] };
parse_deeply q{~-0a} => { format => '~a', arguments => [ 0 ] };
parse_deeply q{~-1a} => { format => '~a', arguments => [ -1 ] };
parse_deeply q{~-9a} => { format => '~a', arguments => [ -9 ] };
parse_deeply q{~-99a} => { format => '~a', arguments => [ -99 ] };
parse_deeply q{~,a} => { format => '~a', arguments => [ undef, undef ] };
parse_deeply q{~-0,a} => { format => '~a', arguments => [ 0, undef ] };
parse_deeply q{~-1,a} => { format => '~a', arguments => [ -1, undef ] };
parse_deeply q{~-9,a} => { format => '~a', arguments => [ -9, undef ] };
parse_deeply q{~-99,a} => { format => '~a', arguments => [ -99, undef ] };
parse_deeply q{~-0,0a} => { format => '~a', arguments => [ 0, 0 ] };
parse_deeply q{~-1,0a} => { format => '~a', arguments => [ -1, 0 ] };
parse_deeply q{~-9,0a} => { format => '~a', arguments => [ -9, 0 ] };
parse_deeply q{~-99,0a} => { format => '~a', arguments => [ -99, 0 ] };
parse_deeply q{~-0,-0a} => { format => '~a', arguments => [ 0, 0 ] };
parse_deeply q{~-1,-0a} => { format => '~a', arguments => [ -1, 0 ] };
parse_deeply q{~-9,-0a} => { format => '~a', arguments => [ -9, 0 ] };
parse_deeply q{~-99,-0a} => { format => '~a', arguments => [ -99, 0 ] };
parse_deeply q{~-0,-9a} => { format => '~a', arguments => [ 0, -9 ] };
parse_deeply q{~-1,-9a} => { format => '~a', arguments => [ -1, -9 ] };
parse_deeply q{~-9,-9a} => { format => '~a', arguments => [ -9, -9 ] };
parse_deeply q{~-99,-9a} => { format => '~a', arguments => [ -99, -9 ] };
parse_deeply q{~-99,-99,-99a} => {
  format => '~a', arguments => [ -99, -99, -99 ] };
parse_deeply q{~-99,-99,-99,-99a} => {
  format => '~a', arguments => [ -99, -99, -99, -99 ] };

parse_deeply q{~:A} => { format => '~a', colon => 1 };
parse_deeply q{~@A} => { format => '~a', at => 1 };
parse_deeply q{~@:A} => { format => '~a', at => 1, colon => 1 };
parse_deeply q{~:@A} => { format => '~a', at => 1, colon => 1 };

parse_deeply q{~#A} => { format => '~a', arguments => [ '#' ] };
parse_deeply q{~#@A} => {
  format => '~a', at => 1, arguments => [ '#' ] };
parse_deeply q{~#:A} => {
  format => '~a', colon => 1, arguments => [ '#' ] };
parse_deeply q{~#@:A} => {
  format => '~a', at => 1, colon => 1, arguments => [ '#' ] };
parse_deeply q{~#:@A} => {
  format => '~a', at => 1, colon => 1, arguments => [ '#' ] };

parse_deeply q{~vA} => { format => '~a', arguments => [ 'v' ] };
parse_deeply q{~VA} => { format => '~a', arguments => [ 'V' ] };
parse_deeply q{~v@A} => {
  format => '~a', at => 1, arguments => [ 'v' ] };
parse_deeply q{~v:A} => {
  format => '~a', colon => 1, arguments => [ 'v' ] };
parse_deeply q{~v@:A} => {
  format => '~a', at => 1, colon => 1, arguments => [ 'v' ] };
parse_deeply q{~v:@A} => {
  format => '~a', at => 1, colon => 1, arguments => [ 'v' ] };

# Edge cases for 'x
#
parse_deeply q{~':A} => { format => '~a', arguments => [ q{':} ] };
parse_deeply q{~'@A} => { format => '~a', arguments => [ q{'@} ] };
parse_deeply q{~'#A} => { format => '~a', arguments => [ q{'#} ] };
parse_deeply q{~'vA} => { format => '~a', arguments => [ q{'v} ] };
parse_deeply q{~'VA} => { format => '~a', arguments => [ q{'V} ] };
parse_deeply q{~''A} => { format => '~a', arguments => [ q{''} ] };
parse_deeply q{~',A} => { format => '~a', arguments => [ q{',} ] };
parse_deeply q{~'0A} => { format => '~a', arguments => [ q{'0} ] };
parse_deeply q{~'-A} => { format => '~a', arguments => [ q{'-} ] };
parse_deeply q{~'+A} => { format => '~a', arguments => [ q{'+} ] };
parse_deeply q{~' A} => { format => '~a', arguments => [ q{' } ] };
parse_deeply q{~':@A} => {
  format => '~a', at => 1, arguments => [ q{':} ] };
parse_deeply q{~'::A} => {
  format => '~a', colon => 1, arguments => [ q{':} ] };
parse_deeply q{~'@@A} => {
  format => '~a', at => 1, arguments => [ q{'@} ] };
parse_deeply q{~'@:A} => {
  format => '~a', colon => 1, arguments => [ q{'@} ] };

# Run permutations of { (null), #, v, \d+, -\d+, +\d+, ', } on 2..4 args
#
parse_deeply q{~,A} => { format => '~a', arguments => [ undef, undef ] };
parse_deeply q{~,#A} => { format => '~a', arguments => [ undef, '#' ] };
parse_deeply q{~,vA} => { format => '~a', arguments => [ undef, 'v' ] };
parse_deeply q{~,9A} => { format => '~a', arguments => [ undef, 9 ] };
parse_deeply q{~,-9A} => { format => '~a', arguments => [ undef, -9 ] };
parse_deeply q{~,+9A} => { format => '~a', arguments => [ undef, 9 ] };
parse_deeply q{~,',A} => { format => '~a', arguments => [ undef, q{',} ] };

parse_deeply q{~#,A} => { format => '~a', arguments => [ '#', undef ] };
parse_deeply q{~#,#A} => { format => '~a', arguments => [ '#', '#' ] };
parse_deeply q{~#,vA} => { format => '~a', arguments => [ '#', 'v' ] };
parse_deeply q{~#,9A} => { format => '~a', arguments => [ '#', 9 ] };
parse_deeply q{~#,-9A} => { format => '~a', arguments => [ '#', -9 ] };
parse_deeply q{~#,+9A} => { format => '~a', arguments => [ '#', 9 ] };
parse_deeply q{~#,',A} => { format => '~a', arguments => [ '#', q{',} ] };

parse_deeply q{~v,A} => { format => '~a', arguments => [ 'v', undef ] };
parse_deeply q{~v,#A} => { format => '~a', arguments => [ 'v', '#' ] };
parse_deeply q{~v,vA} => { format => '~a', arguments => [ 'v', 'v' ] };
parse_deeply q{~v,9A} => { format => '~a', arguments => [ 'v', 9 ] };
parse_deeply q{~v,-9A} => { format => '~a', arguments => [ 'v', -9 ] };
parse_deeply q{~v,+9A} => { format => '~a', arguments => [ 'v', 9 ] };
parse_deeply q{~v,',A} => { format => '~a', arguments => [ 'v', q{',} ] };

parse_deeply q{~9,A} => { format => '~a', arguments => [ 9, undef ] };
parse_deeply q{~9,#A} => { format => '~a', arguments => [ 9, '#' ] };
parse_deeply q{~9,vA} => { format => '~a', arguments => [ 9, 'v' ] };
parse_deeply q{~9,9A} => { format => '~a', arguments => [ 9, 9 ] };
parse_deeply q{~9,-9A} => { format => '~a', arguments => [ 9, -9 ] };
parse_deeply q{~9,+9A} => { format => '~a', arguments => [ 9, 9 ] };
parse_deeply q{~9,',A} => { format => '~a', arguments => [ 9, q{',} ] };

parse_deeply q{~-9,A} => { format => '~a', arguments => [ -9, undef ] };
parse_deeply q{~-9,#A} => { format => '~a', arguments => [ -9, '#' ] };
parse_deeply q{~-9,vA} => { format => '~a', arguments => [ -9, 'v' ] };
parse_deeply q{~-9,9A} => { format => '~a', arguments => [ -9, 9 ] };
parse_deeply q{~-9,-9A} => { format => '~a', arguments => [ -9, -9 ] };
parse_deeply q{~-9,+9A} => { format => '~a', arguments => [ -9, 9 ] };
parse_deeply q{~-9,',A} => { format => '~a', arguments => [ -9, q{',} ] };

parse_deeply q{~+9,A} => { format => '~a', arguments => [ 9, undef ] };
parse_deeply q{~+9,#A} => { format => '~a', arguments => [ 9, '#' ] };
parse_deeply q{~+9,vA} => { format => '~a', arguments => [ 9, 'v' ] };
parse_deeply q{~+9,9A} => { format => '~a', arguments => [ 9, 9 ] };
parse_deeply q{~+9,-9A} => { format => '~a', arguments => [ 9, -9 ] };
parse_deeply q{~+9,+9A} => { format => '~a', arguments => [ 9, 9 ] };
parse_deeply q{~+9,',A} => { format => '~a', arguments => [ 9, q{',} ] };

parse_deeply q{~',,A} => { format => '~a', arguments => [ q{',}, undef ] };
parse_deeply q{~',,#A} => { format => '~a', arguments => [ q{',}, '#' ] };
parse_deeply q{~',,vA} => { format => '~a', arguments => [ q{',}, 'v' ] };
parse_deeply q{~',,9A} => { format => '~a', arguments => [ q{',}, 9 ] };
parse_deeply q{~',,-9A} => { format => '~a', arguments => [ q{',}, -9 ] };
parse_deeply q{~',,+9A} => { format => '~a', arguments => [ q{',}, 9 ] };
parse_deeply q{~',,',A} => { format => '~a', arguments => [ q{',}, q{',} ] };

=pod

sub ok_parse {
  my ( $str ) = @_;
  eval { $p->from_string( $str ) };
  ok( !$@, qq{q{$str}} ) or
    diag( "q{$str} : $@" );
}

ok_parse( q{~#,#a} );
ok_parse( q{~#,#,#a} );
ok_parse( q{~#,#,#,#a} );
ok_parse( q{~va} );
ok_parse( q{~v,va} );
ok_parse( q{~v,v,va} );
ok_parse( q{~v,v,v,va} );
ok_parse( q{~v@a} );
ok_parse( q{~v,v@a} );
ok_parse( q{~v,v,v@a} );
ok_parse( q{~v,v,v,v@a} );
ok_parse( q{~v@:a} );
ok_parse( q{~v,v@:a} );
ok_parse( q{~v,v,v@:a} );
ok_parse( q{~v,v,v,v@:a} );
ok_parse( q{~,@:a} );
ok_parse( q{~,,@:a} );
ok_parse( q{~,,,@:a} );
ok_parse( q{~,'@@:a} );
ok_parse( q{~,,'@@:a} );
ok_parse( q{~,,,'@@:a} );
ok_parse( q{~,'@:a} );
ok_parse( q{~,,'@:a} );
ok_parse( q{~,,,'@:a} );

=cut

=pod

ok_parse( q{~-1a}  );
ok_parse( q{~+1a}  );
ok_parse( q{~'xa}  );
ok_parse( q{~' a}  );
ok_parse( q{~':a}  );
ok_parse( q{~'@a}  );
ok_parse( q{~'x:a} );
ok_parse( q{~' :a} );
ok_parse( q{~'::a} );
ok_parse( q{~'@:a} );
ok_parse( q{~'x@a} );
ok_parse( q{~' @a} );
ok_parse( q{~':@a} );
ok_parse( q{~'@@a} );

ok_parse( q{~0,0a} );

=cut

=pod

# Naked
#
is_deeply( $p->from_string( q{~a} ), [ {
  format => '~a'
} ] );
is_deeply( $p->from_string( q{~A} ), [ {
  format => '~a'
} ] );

# Modifiers
#
is_deeply( $p->from_string( q{~:A} ), [ {
  format => '~a',
  colon => 1
} ] );
is_deeply( $p->from_string( q{~@A} ), [ {
  format => '~a',
  at => 1
} ] );
is_deeply( $p->from_string( q{~:@A} ), [ {
  format => '~a',
  at => 1,
  colon => 1
} ] );
is_deeply( $p->from_string( q{~@:A} ), [ {
  format => '~a',
  at => 1,
  colon => 1
} ] );

# Single argument
#
is_deeply( $p->from_string( q{~0a} ), [ {
  format => '~a',
  argument => [ 0 ]
} ] );
is_deeply( $p->from_string( q{~1a} ), [ {
  format => '~a',
  argument => [ 1 ]
} ] );
is_deeply( $p->from_string( q{~9a} ), [ {
  format => '~a',
  argument => [ 9 ]
} ] );
is_deeply( $p->from_string( q{~99a} ), [ {
  format => '~a',
  argument => [ 99 ]
} ] );
is_deeply( $p->from_string( q{~-0a} ), [ {
  format => '~a',
  argument => [ 0 ]
} ] );
is_deeply( $p->from_string( q{~-1a} ), [ {
  format => '~a',
  argument => [ -1 ]
} ] );
is_deeply( $p->from_string( q{~-9a} ), [ {
  format => '~a',
  argument => [ -9 ]
} ] );
is_deeply( $p->from_string( q{~-99a} ), [ {
  format => '~a',
  argument => [ -99 ]
} ] );
is_deeply( $p->from_string( q{~#a} ), [ {
  format => '~a',
  argument => [ '#' ]
} ] );
is_deeply( $p->from_string( q{~va} ), [ {
  format => '~a',
  argument => [ 'v' ]
} ] );

=cut

=pod


#
# Test parsing raw format strings, leave the higher-level tests for later.
#
# t/02-format-a
#
ok_parse( q{~va} );
ok_parse( q{~v:A} );
ok_parse( q{~v@A} );
ok_parse( q{~v:@a} );
ok_parse( q{~v@:a} );
ok_parse( q{~5,1a} );
ok_parse( q{~6,5a} );
ok_parse( q{~5,5@a} );
ok_parse( q{~6,6@a} );
ok_parse( q{~9,5@a} );
ok_parse( q{~9,5A} );
ok_parse( q{~11,5@a} );
ok_parse( q{~11,5A} );
ok_parse( q{~v,,2A} );
ok_parse( q{~3,,+2A} );
ok_parse( q{~3,,0A} );
ok_parse( q{~3,,-1A} );
ok_parse( q{~3,,0A} );
ok_parse( q{~3,,-1A} );
ok_parse( q{~4,,,'XA} );
ok_parse( q{~4,,,a} );
ok_parse( q{~4,,,'X@a} );
ok_parse( q{~4,,,@A} );
ok_parse( q{~10,,,vA} );
ok_parse( q{~10,,,v@A} );
ok_parse( q{~10,,,va} );
ok_parse( q{~10,,,v@a} );
ok_parse( q{~3,,vA} );
ok_parse( q{~4,,va} );
ok_parse( q{~3,,vA} );
ok_parse( q{~3,,v@A} );
ok_parse( q{~5,vA} );
ok_parse( q{~5,v@A} );
ok_parse( q{~#A} );
ok_parse( q{~#@a} );
ok_parse( q{~5,#a} );
ok_parse( q{~5,#@A} );
ok_parse( q{~4,#A} );
ok_parse( q{~4,#@A} );
ok_parse( q{~#,#A} );
ok_parse( q{~#,#@A} );
ok_parse( q{~-100A} );
ok_parse( q{~-100000000000000000000a} );
ok_parse( q{~~~d@a} ); # format( "~~~d@a", 3 ) => "~3@a" so it *is* legitimate.
ok_parse( q{~~~da} );
ok_parse( q{~~~d@:A} );
ok_parse( q{~~~d:a} );
ok_parse( q{~V:a} );
ok_parse( q{~V@:A} );

# t/03-format-ampersand
ok_parse( q{~0&} );
ok_parse( q{~&} );
ok_parse( q{X~&} );
ok_parse( q{X~%~&} );
ok_parse( q{~v&} );
ok_parse( q{X~v&} );
ok_parse( q{X~V%} );
ok_parse( q{X~#%} );
ok_parse( q{X~#%} );
ok_parse( q{~~~D&} );
ok_parse( q{~~~D&} );
ok_parse( q{X~~~D&} );
ok_parse( q{X~~~D&} );
ok_parse( q{~V&} );
ok_parse( q{~#&} );

# t/04-format-b
#
ok_parse( q{~:B} );
ok_parse( q{~b} );
ok_parse( q{~:b} );
ok_parse( q{~vb} );
ok_parse( q{~6,vB} );
ok_parse( q{~,,v:b} );
ok_parse( q{~,,'*,v:B} );
ok_parse( q{~+10b} );
ok_parse( q{~+10@B} );
ok_parse( q{~-1b} );
ok_parse( q{~-1000000000000000000B} );
ok_parse( q{~b} );
ok_parse( q{~B} );
ok_parse( q{~@b} );
ok_parse( q{~b} );
ok_parse( q{~~~db} );
ok_parse( q{~~~d@b} );
ok_parse( q{~@B} );
ok_parse( q{~~~d,'~c~c} );
ok_parse( q{~v,vB} );
ok_parse( q{~v,vb} );
ok_parse( q{~v,v@B} );
ok_parse( q{~:b} );
ok_parse( q{~:B} );
ok_parse( q{~,,v:B} );
ok_parse( q{~,,v:b} );
ok_parse( q{~~,,'~c:~c} );
ok_parse( q{~,,V,V:b} );
ok_parse( q{~,,v,v:B} );
ok_parse( q{~,,V,V@:B} );
ok_parse( q{~@B} );
ok_parse( q{~,,v,v:@b} );
ok_parse( q{~:b} );
ok_parse( q{~:@b} );
ok_parse( q{~@:B} );
ok_parse( q{~#B} );
ok_parse( q{~#b} );
ok_parse( q{~,,,#:b} );
ok_parse( q{~,,,#:B} );
ok_parse( q{~,,,#@:B} );
ok_parse( q{~,,,#@:B} );
ok_parse( q{~V,V,V,VB} );
ok_parse( q{~~~d,} );
ok_parse( q{'~c,} );
ok_parse( q{'~c,} );
ok_parse( q{~db} );
ok_parse( q{~v,v,v,vb} );

# t/05-format-brace.t
#
ok_parse( qq{~{~\n~}} );
ok_parse( q{~{~}} );
ok_parse( q{~0{~}} );
ok_parse( q{~{ ~}} );
ok_parse( q{~{X Y Z~}} );
ok_parse( q{~{~A~}} );
ok_parse( q{~{~{~A~}~}} );
ok_parse( q{~{~1{~A~}~}} );
ok_parse( qq{~1{~\n~}} );
ok_parse( q{~#{~A~}} );
ok_parse( q{~0{~}} );
ok_parse( q{~1{~}} );
ok_parse( q{~{~}} );
ok_parse( q{} );
ok_parse( q{~1{~}} );
ok_parse( q{~1{~}} );
ok_parse( q{~3{~}} );
ok_parse( q{~V{~}} );
ok_parse( q{~#{~}} );
ok_parse( q{~{FOO~:}} );
ok_parse( q{~{~A~:}} );
ok_parse( q{~0{FOO~:}} );
ok_parse( q{~V{FOO~:}} );
ok_parse( q{~1{FOO~:}} );
ok_parse( q{~2{FOO~:}} );
ok_parse( qq{~2{~\n~:}} );
ok_parse( q{~2{FOO~}} );
ok_parse( q{~v{~a~}} );
ok_parse( q{~:{(~A ~A)~}} );
ok_parse( qq{~:{~\n~}} );
ok_parse( q{~:{~}} );
ok_parse( q{~0:{XYZ~}} );
ok_parse( q{~2:{XYZ~}} );
ok_parse( q{~2:{~A~}} );
ok_parse( q{~V:{X~}} );
ok_parse( q{~#:{~A~}} );
ok_parse( q{~:{~A~:}} );
ok_parse( q{~:{ABC~:}} );
ok_parse( q{~v:{ABC~:}} );
ok_parse( qq{~\@{~\n~}} );
ok_parse( q{~@{~}} );
ok_parse( q{~@{ ~}} );
ok_parse( q{~@{X ~A Y Z~}} );
ok_parse( q{~@{~A~}} );
ok_parse( q{~@{~{~A~}~}} );
ok_parse( q{~@{~1{~A~}~}} );
ok_parse( q{~1@{FOO~}} );
ok_parse( q{~v@{~A~}} );
ok_parse( q{~#@{~A~}} );
ok_parse( q{~@{X~:}} );
ok_parse( q{~@{~}} );
ok_parse( q{X~AY} );
ok_parse( q{~@{~}'} );
ok_parse( q{~v@{~}} );
ok_parse( q{X} );
ok_parse( q{~:@{~}} );
ok_parse( q{~:@{~A~}} );
ok_parse( q{~:@{(~A ~A)~}} );
ok_parse( q{~:@{~}} );
ok_parse( q{~:@{~}} );
ok_parse( q{(~A ~A)} );
ok_parse( q{~:@{~A~:}} );
ok_parse( q{~0:@{~A~:}} );
ok_parse( q{~#:@{A~:}} );
ok_parse( q{~v:@{~A~}} );
ok_parse( q{~v{~A~}} );
ok_parse( q{~V{~A~}} );
ok_parse( q{~v:{~A~}} );
ok_parse( q{~v:{~A~}} );
ok_parse( q{~v:{~A~:}} );
ok_parse( q{~v:{~A~:}} );
ok_parse( q{~v@{~A~}} );
ok_parse( q{~V@:{~A~}} );
ok_parse( q{~{~A~}} );
ok_parse( q{~:{~A~}} );
ok_parse( q{~:@{~A~}} );
ok_parse( q{~:@{~A ~A~}} );

=cut
