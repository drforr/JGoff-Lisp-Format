package JGoff::Lisp::Format::Tokens::Circumflex;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has parameters => ( is => 'rw' );
has colon => ( is => 'ro', isa => 'Bool' );
has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::Circumflex - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
}

1;
