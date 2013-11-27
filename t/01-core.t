#!perl

use Test::More tests => 23;
use YAML;

BEGIN {
  use_ok( 'JGoff::Lisp::Format' ) || print "Bail out!\n";
  use lib 't/lib';
  use_ok( 'JGoff::Lisp::Format::Utils' ) || print "Bail out!";
}

use strict;
use warnings;

subtest 'Utility state defaults correctly' => sub {
  plan tests => 7;

  my @multibyte_chars =
     grep { ord( $_->toString ) > 0x7f }
          @JGoff::Lisp::Format::Utils::standard_chars;

  ok $JGoff::Lisp::Format::Utils::most_positive_fixnum > 0,
     'most_positive_fixnum is positive';
  ok $JGoff::Lisp::Format::Utils::most_negative_fixnum < 0,
     'most_negative_fixnum is negative';
  ok @JGoff::Lisp::Format::Utils::standard_chars > 0,
     'standard_chars has chars';
  isa_ok $JGoff::Lisp::Format::Utils::standard_chars[0],
         'JGoff::Lisp::Format::Utils::Character',
         'standard_chars are typed objects for testing purposes';
  ok @multibyte_chars == 0,
     'standard_chars objects are all ASCII';
  ok $JGoff::Lisp::Format::Utils::char_code_limit > 0,
     'char_code_limit > 0';
  ok $JGoff::Lisp::Format::Utils::call_arguments_limit > 0,
     'call_char_limit > 0';
};

SKIP: {
  diag 'Missing stuff';
  skip 'Not ready yet', 1;
ok @JGoff::Lisp::Format::Utils::mini_universe > 0,
   'mini_universe populated';
is char_int( ' ' ) => 32,
   'XXX Should this be the correct return value?';
}

subtest 'char_names' => sub {
  plan tests => 3;

  is char_name( ' ' ) => 'Space';
  is char_name( "\n" ) => 'Newline';
  is char_name( 'n' ) => 'n';
};

is char_code( ' ' ), 32;

SKIP: {
  diag 'Missing stuff';
  skip 'Not ready yet', 1;
is char_int( ' ' ) => 32, 'XXX Should this be the correct return value?';
}

subtest 'strings' => sub {
  plan tests => 2;

  is string( 'f' ) => 'f';
  is string( JGoff::Lisp::Format::Utils::Character->new( character => 'f' ) ) =>
     'f';
};

subtest 'subseqs' => sub {
  plan tests => 2;

  is subseq( 'foo', 1, 2 ) => 'o';
  is subseq( 'foo', 1, 3 ) => 'oo';
};

is with_standard_io_syntax { 'foo' } => 'foo';

is concatenate( 'foo', 'bar' ) => 'foobar';

my $foo = [];
collect( $foo, 1 );
is_deeply $foo => [ 1 ];

is_deeply list( 'a', 'b' ) => [ 'a', 'b' ];

# XXX formatter_call_to_string

def_format_test 'core.2' => 
  "42",
  undef,
  42;

# XXX def_pprint_test

# Regardless of (format), deftest() should run
{
  my $side_effect = 0;
  deftest 'core.1' => sub { $side_effect++; 42 }, 42;
  ok $side_effect == 1, 'deftest() had required side effect';
}

is code_char( 32 ) => ' ';
is make_string( 3, initial_element => 'x' ) => 'xxx';

is random_from_seq( 'x' ) => 'x';

is_deeply [ remove_duplicates( 'a', 'b', 'a', 'c', 'c', 'd' ) ] =>
          [ 'a', 'b', 'c', 'd' ];

# XXX graphic_char_p

# XXX read_from_string

# XXX search

is_deeply make_list( 3 ) => [ undef, undef, undef ];

is apply( sub { scalar @_ }, undef, undef, undef ) => 3;

is_deeply cons( 3, [ undef, undef, undef ] ) => [ undef, undef, undef, 3 ];

# XXX signals_type_error

# XXX signals_error

SKIP: {
  diag "Rethink these tests";
  skip 'Not ready yet', 0;

my $p = JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );

sub parse_deeply {
  my ( $str, $expected ) = @_;
  my ( $package, $filename, $line ) = caller();
  my $actual = $p->from_string( $str );
  is_deeply( $actual => [ $expected ], qq{q{$str}} ) or
      diag( "  at test file $filename line $line\n" . Dump( $actual ) );
}

parse_deeply q{~a} => { format => q{~a} };
parse_deeply q{~A} => { format => q{~a} };

# {{{ Single parameter
#
parse_deeply q{~#a} => { format => q{~a}, arguments => [ q{#} ] };
parse_deeply q{~va} => { format => q{~a}, arguments => [ q{v} ] };
parse_deeply q{~Va} => { format => q{~a}, arguments => [ q{v} ] };

# (potential edge cases)
#
parse_deeply q{~' a} => { format => q{~a}, arguments => [ q{' } ] };
parse_deeply q{~''a} => { format => q{~a}, arguments => [ q{''} ] };
parse_deeply q{~',a} => { format => q{~a}, arguments => [ q{',} ] };
parse_deeply q{~'aa} => { format => q{~a}, arguments => [ q{'a} ] };
parse_deeply q{~':a} => { format => q{~a}, arguments => [ q{':} ] };
parse_deeply q{~'@a} => { format => q{~a}, arguments => [ q{'@} ] };
parse_deeply q{~'va} => { format => q{~a}, arguments => [ q{'v} ] };
parse_deeply q{~'Va} => { format => q{~a}, arguments => [ q{'V} ] };
parse_deeply q{~'#a} => { format => q{~a}, arguments => [ q{'#} ] };
parse_deeply q{~'-a} => { format => q{~a}, arguments => [ q{'-} ] };
parse_deeply q{~'+a} => { format => q{~a}, arguments => [ q{'+} ] };
parse_deeply q{~'0a} => { format => q{~a}, arguments => [ q{'0} ] };
parse_deeply q{~'9a} => { format => q{~a}, arguments => [ q{'9} ] };

# (numeric arguments)
#
parse_deeply q{~0a} => { format => q{~a}, arguments => [ 0 ] };
parse_deeply q{~1a} => { format => q{~a}, arguments => [ 1 ] };
parse_deeply q{~9a} => { format => q{~a}, arguments => [ 9 ] };
parse_deeply q{~99a} => { format => q{~a}, arguments => [ 99 ] };
parse_deeply q{~+0a} => { format => q{~a}, arguments => [ 0 ] };
parse_deeply q{~+1a} => { format => q{~a}, arguments => [ 1 ] };
parse_deeply q{~+9a} => { format => q{~a}, arguments => [ 9 ] };
parse_deeply q{~+99a} => { format => q{~a}, arguments => [ 99 ] };
parse_deeply q{~-0a} => { format => q{~a}, arguments => [ 0 ] };
parse_deeply q{~-1a} => { format => q{~a}, arguments => [ -1 ] };
parse_deeply q{~-9a} => { format => q{~a}, arguments => [ -9 ] };
parse_deeply q{~-99a} => { format => q{~a}, arguments => [ -99 ] };
#
# }}}

# {{{ Single parameter with ':' modifier
#
parse_deeply q{~#:a} => { format => q{~a}, colon => 1, arguments => [ q{#} ] };
parse_deeply q{~v:a} => { format => q{~a}, colon => 1, arguments => [ q{v} ] };
parse_deeply q{~V:a} => { format => q{~a}, colon => 1, arguments => [ q{v} ] };

# (potential edge cases)
#
parse_deeply q{~' :a} =>
  { format => q{~a}, colon => 1, arguments => [ q{' } ] };
parse_deeply q{~'':a} =>
  { format => q{~a}, colon => 1, arguments => [ q{''} ] };
parse_deeply q{~',:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{',} ] };
parse_deeply q{~'a:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'a} ] };
parse_deeply q{~'::a} =>
  { format => q{~a}, colon => 1, arguments => [ q{':} ] };
parse_deeply q{~'@:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'@} ] };
parse_deeply q{~'v:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'v} ] };
parse_deeply q{~'V:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'V} ] };
parse_deeply q{~'#:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'#} ] };
parse_deeply q{~'-:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'-} ] };
parse_deeply q{~'+:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'+} ] };
parse_deeply q{~'0:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'0} ] };
parse_deeply q{~'9:a} =>
  { format => q{~a}, colon => 1, arguments => [ q{'9} ] };

# (numeric colon => 1, arguments)
#
parse_deeply q{~0:a}   => { format => q{~a}, colon => 1, arguments => [ 0 ] };
parse_deeply q{~1:a}   => { format => q{~a}, colon => 1, arguments => [ 1 ] };
parse_deeply q{~9:a}   => { format => q{~a}, colon => 1, arguments => [ 9 ] };
parse_deeply q{~99:a}  => { format => q{~a}, colon => 1, arguments => [ 99 ] };
parse_deeply q{~+0:a}  => { format => q{~a}, colon => 1, arguments => [ 0 ] };
parse_deeply q{~+1:a}  => { format => q{~a}, colon => 1, arguments => [ 1 ] };
parse_deeply q{~+9:a}  => { format => q{~a}, colon => 1, arguments => [ 9 ] };
parse_deeply q{~+99:a} => { format => q{~a}, colon => 1, arguments => [ 99 ] };
parse_deeply q{~-0:a}  => { format => q{~a}, colon => 1, arguments => [ 0 ] };
parse_deeply q{~-1:a}  => { format => q{~a}, colon => 1, arguments => [ -1 ] };
parse_deeply q{~-9:a}  => { format => q{~a}, colon => 1, arguments => [ -9 ] };
parse_deeply q{~-99:a} => { format => q{~a}, colon => 1, arguments => [ -99 ] };
#
# }}}

# {{{ Single parameter with ':@' modifier
#
parse_deeply q{~#:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{#} ] };
parse_deeply q{~v:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{v} ] };
parse_deeply q{~V:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{v} ] };

# (potential edge cases)
#
parse_deeply q{~' :@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{' } ] };
parse_deeply q{~'':@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{''} ] };
parse_deeply q{~',:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{',} ] };
parse_deeply q{~'a:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'a} ] };
parse_deeply q{~'::@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{':} ] };
parse_deeply q{~'@:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'@} ] };
parse_deeply q{~'v:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'v} ] };
parse_deeply q{~'V:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'V} ] };
parse_deeply q{~'#:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'#} ] };
parse_deeply q{~'-:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'-} ] };
parse_deeply q{~'+:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'+} ] };
parse_deeply q{~'0:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'0} ] };
parse_deeply q{~'9:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'9} ] };

# (numeric colon => 1, at => 1, arguments)
#
parse_deeply q{~0:@a}   =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 0 ] };
parse_deeply q{~1:@a}   =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 1 ] };
parse_deeply q{~9:@a}   =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 9 ] };
parse_deeply q{~99:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 99 ] };
parse_deeply q{~+0:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 0 ] };
parse_deeply q{~+1:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 1 ] };
parse_deeply q{~+9:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 9 ] };
parse_deeply q{~+99:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 99 ] };
parse_deeply q{~-0:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 0 ] };
parse_deeply q{~-1:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ -1 ] };
parse_deeply q{~-9:@a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ -9 ] };
parse_deeply q{~-99:@a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ -99 ] };
#
# }}}

# {{{ Single parameter with '@' modifier
#
parse_deeply q{~#@a} => { format => q{~a}, at => 1, arguments => [ q{#} ] };
parse_deeply q{~v@a} => { format => q{~a}, at => 1, arguments => [ q{v} ] };
parse_deeply q{~V@a} => { format => q{~a}, at => 1, arguments => [ q{v} ] };

# (potential edge cases)
#
parse_deeply q{~' @a} => { format => q{~a}, at => 1, arguments => [ q{' } ] };
parse_deeply q{~''@a} => { format => q{~a}, at => 1, arguments => [ q{''} ] };
parse_deeply q{~',@a} => { format => q{~a}, at => 1, arguments => [ q{',} ] };
parse_deeply q{~'a@a} => { format => q{~a}, at => 1, arguments => [ q{'a} ] };
parse_deeply q{~':@a} => { format => q{~a}, at => 1, arguments => [ q{':} ] };
parse_deeply q{~'@@a} => { format => q{~a}, at => 1, arguments => [ q{'@} ] };
parse_deeply q{~'v@a} => { format => q{~a}, at => 1, arguments => [ q{'v} ] };
parse_deeply q{~'V@a} => { format => q{~a}, at => 1, arguments => [ q{'V} ] };
parse_deeply q{~'#@a} => { format => q{~a}, at => 1, arguments => [ q{'#} ] };
parse_deeply q{~'-@a} => { format => q{~a}, at => 1, arguments => [ q{'-} ] };
parse_deeply q{~'+@a} => { format => q{~a}, at => 1, arguments => [ q{'+} ] };
parse_deeply q{~'0@a} => { format => q{~a}, at => 1, arguments => [ q{'0} ] };
parse_deeply q{~'9@a} => { format => q{~a}, at => 1, arguments => [ q{'9} ] };

# (numeric colon => 1, arguments)
#
parse_deeply q{~0@a}   => { format => q{~a}, at => 1, arguments => [ 0 ] };
parse_deeply q{~1@a}   => { format => q{~a}, at => 1, arguments => [ 1 ] };
parse_deeply q{~9@a}   => { format => q{~a}, at => 1, arguments => [ 9 ] };
parse_deeply q{~99@a}  => { format => q{~a}, at => 1, arguments => [ 99 ] };
parse_deeply q{~+0@a}  => { format => q{~a}, at => 1, arguments => [ 0 ] };
parse_deeply q{~+1@a}  => { format => q{~a}, at => 1, arguments => [ 1 ] };
parse_deeply q{~+9@a}  => { format => q{~a}, at => 1, arguments => [ 9 ] };
parse_deeply q{~+99@a} => { format => q{~a}, at => 1, arguments => [ 99 ] };
parse_deeply q{~-0@a}  => { format => q{~a}, at => 1, arguments => [ 0 ] };
parse_deeply q{~-1@a}  => { format => q{~a}, at => 1, arguments => [ -1 ] };
parse_deeply q{~-9@a}  => { format => q{~a}, at => 1, arguments => [ -9 ] };
parse_deeply q{~-99@a} => { format => q{~a}, at => 1, arguments => [ -99 ] };
#
# }}}

# {{{ Single parameter with '@:' modifier
#
parse_deeply q{~#@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{#} ] };
parse_deeply q{~v@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{v} ] };
parse_deeply q{~V@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{v} ] };

# (potential edge cases)
#
parse_deeply q{~' @:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{' } ] };
parse_deeply q{~''@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{''} ] };
parse_deeply q{~',@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{',} ] };
parse_deeply q{~'a@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'a} ] };
parse_deeply q{~':@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{':} ] };
parse_deeply q{~'@@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'@} ] };
parse_deeply q{~'v@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'v} ] };
parse_deeply q{~'V@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'V} ] };
parse_deeply q{~'#@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'#} ] };
parse_deeply q{~'-@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'-} ] };
parse_deeply q{~'+@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'+} ] };
parse_deeply q{~'0@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'0} ] };
parse_deeply q{~'9@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ q{'9} ] };

# (numeric colon => 1, at => 1, arguments)
#
parse_deeply q{~0@:a}   =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 0 ] };
parse_deeply q{~1@:a}   =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 1 ] };
parse_deeply q{~9@:a}   =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 9 ] };
parse_deeply q{~99@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 99 ] };
parse_deeply q{~+0@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 0 ] };
parse_deeply q{~+1@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 1 ] };
parse_deeply q{~+9@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 9 ] };
parse_deeply q{~+99@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 99 ] };
parse_deeply q{~-0@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ 0 ] };
parse_deeply q{~-1@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ -1 ] };
parse_deeply q{~-9@:a}  =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ -9 ] };
parse_deeply q{~-99@:a} =>
  { format => q{~a}, colon => 1, at => 1, arguments => [ -99 ] };
#
# }}}

# {{{ Two parameters from { (null), #, v, \d+, -\d+, +\d+, ', }
#
parse_deeply q{~,A} => { format => q{~a}, arguments => [ undef, undef ] };
parse_deeply q{~,#A} => { format => q{~a}, arguments => [ undef, '#' ] };
parse_deeply q{~,vA} => { format => q{~a}, arguments => [ undef, 'v' ] };
parse_deeply q{~,9A} => { format => q{~a}, arguments => [ undef, 9 ] };
parse_deeply q{~,-9A} => { format => q{~a}, arguments => [ undef, -9 ] };
parse_deeply q{~,+9A} => { format => q{~a}, arguments => [ undef, 9 ] };
parse_deeply q{~,',A} => { format => q{~a}, arguments => [ undef, q{',} ] };

parse_deeply q{~#,A} => { format => q{~a}, arguments => [ '#', undef ] };
parse_deeply q{~#,#A} => { format => q{~a}, arguments => [ '#', '#' ] };
parse_deeply q{~#,vA} => { format => q{~a}, arguments => [ '#', 'v' ] };
parse_deeply q{~#,9A} => { format => q{~a}, arguments => [ '#', 9 ] };
parse_deeply q{~#,-9A} => { format => q{~a}, arguments => [ '#', -9 ] };
parse_deeply q{~#,+9A} => { format => q{~a}, arguments => [ '#', 9 ] };
parse_deeply q{~#,',A} => { format => q{~a}, arguments => [ '#', q{',} ] };

parse_deeply q{~v,A} => { format => q{~a}, arguments => [ 'v', undef ] };
parse_deeply q{~v,#A} => { format => q{~a}, arguments => [ 'v', '#' ] };
parse_deeply q{~v,vA} => { format => q{~a}, arguments => [ 'v', 'v' ] };
parse_deeply q{~v,9A} => { format => q{~a}, arguments => [ 'v', 9 ] };
parse_deeply q{~v,-9A} => { format => q{~a}, arguments => [ 'v', -9 ] };
parse_deeply q{~v,+9A} => { format => q{~a}, arguments => [ 'v', 9 ] };
parse_deeply q{~v,',A} => { format => q{~a}, arguments => [ 'v', q{',} ] };

parse_deeply q{~9,A} => { format => q{~a}, arguments => [ 9, undef ] };
parse_deeply q{~9,#A} => { format => q{~a}, arguments => [ 9, '#' ] };
parse_deeply q{~9,vA} => { format => q{~a}, arguments => [ 9, 'v' ] };
parse_deeply q{~9,9A} => { format => q{~a}, arguments => [ 9, 9 ] };
parse_deeply q{~9,-9A} => { format => q{~a}, arguments => [ 9, -9 ] };
parse_deeply q{~9,+9A} => { format => q{~a}, arguments => [ 9, 9 ] };
parse_deeply q{~9,',A} => { format => q{~a}, arguments => [ 9, q{',} ] };

parse_deeply q{~-9,A} => { format => q{~a}, arguments => [ -9, undef ] };
parse_deeply q{~-9,#A} => { format => q{~a}, arguments => [ -9, '#' ] };
parse_deeply q{~-9,vA} => { format => q{~a}, arguments => [ -9, 'v' ] };
parse_deeply q{~-9,9A} => { format => q{~a}, arguments => [ -9, 9 ] };
parse_deeply q{~-9,-9A} => { format => q{~a}, arguments => [ -9, -9 ] };
parse_deeply q{~-9,+9A} => { format => q{~a}, arguments => [ -9, 9 ] };
parse_deeply q{~-9,',A} => { format => q{~a}, arguments => [ -9, q{',} ] };

parse_deeply q{~+9,A} => { format => q{~a}, arguments => [ 9, undef ] };
parse_deeply q{~+9,#A} => { format => q{~a}, arguments => [ 9, '#' ] };
parse_deeply q{~+9,vA} => { format => q{~a}, arguments => [ 9, 'v' ] };
parse_deeply q{~+9,9A} => { format => q{~a}, arguments => [ 9, 9 ] };
parse_deeply q{~+9,-9A} => { format => q{~a}, arguments => [ 9, -9 ] };
parse_deeply q{~+9,+9A} => { format => q{~a}, arguments => [ 9, 9 ] };
parse_deeply q{~+9,',A} => { format => q{~a}, arguments => [ 9, q{',} ] };

parse_deeply q{~',,A} => { format => q{~a}, arguments => [ q{',}, undef ] };
parse_deeply q{~',,#A} => { format => q{~a}, arguments => [ q{',}, '#' ] };
parse_deeply q{~',,vA} => { format => q{~a}, arguments => [ q{',}, 'v' ] };
parse_deeply q{~',,9A} => { format => q{~a}, arguments => [ q{',}, 9 ] };
parse_deeply q{~',,-9A} => { format => q{~a}, arguments => [ q{',}, -9 ] };
parse_deeply q{~',,+9A} => { format => q{~a}, arguments => [ q{',}, 9 ] };
parse_deeply q{~',,',A} => { format => q{~a}, arguments => [ q{',}, q{',} ] };
#
# }}}

# {{{ Three parameters, all the same term
#
parse_deeply q{~,,A} =>
  { format => q{~a}, arguments => [ undef, undef, undef ] };
parse_deeply q{~#,#,#A} =>
  { format => q{~a}, arguments => [ q{#}, q{#}, q{#} ] };
parse_deeply q{~v,v,vA} =>
  { format => q{~a}, arguments => [ q{v}, q{v}, q{v} ] };
parse_deeply q{~V,V,VA} =>
  { format => q{~a}, arguments => [ q{v}, q{v}, q{v} ] };
parse_deeply q{~-9,-9,-9A} =>
  { format => q{~a}, arguments => [ -9, -9, -9 ] };
#
# }}}

# {{{ Four parameters, all the same term
#
parse_deeply q{~,,,A} =>
  { format => q{~a}, arguments => [ undef, undef, undef, undef ] };
parse_deeply q{~#,#,#,#A} =>
  { format => q{~a}, arguments => [ q{#}, q{#}, q{#}, q{#} ] };
parse_deeply q{~v,v,v,vA} =>
  { format => q{~a}, arguments => [ q{v}, q{v}, q{v}, q{v} ] };
parse_deeply q{~V,V,V,VA} =>
  { format => q{~a}, arguments => [ q{v}, q{v}, q{v}, q{v} ] };
parse_deeply q{~-9,-9,-9,-9A} =>
  { format => q{~a}, arguments => [ -9, -9, -9, -9 ] };
#
# }}}

=pod

sub ok_parse {
  my ( $str ) = @_;
  eval { $p->from_string( $str ) };
  ok( !$@, qq{q{$str}} ) or
    diag( "q{$str} : $@" );
}

#
# Test parsing format strings from the wild,
# Leave the higher-level tests for later.
#
# t/02-format-a
#
ok_parse( q{~v:@a} );
ok_parse( q{~v@:a} );
ok_parse( q{~5,5@a} );
ok_parse( q{~6,6@a} );
ok_parse( q{~9,5@a} );
ok_parse( q{~11,5@a} );
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
ok_parse( q{~5,v@A} );
ok_parse( q{~5,#@A} );
ok_parse( q{~4,#@A} );
ok_parse( q{~#,#@A} );
ok_parse( q{~~~d@a} ); # format( "~~~d@a", 3 ) => "~3@a" so it *is* legitimate.
ok_parse( q{~~~da} );
ok_parse( q{~~~d@:A} );
ok_parse( q{~~~d:a} );
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

}
