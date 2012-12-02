package JGoff::Lisp::Format::Tokens::Circumflex;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
}

1;
