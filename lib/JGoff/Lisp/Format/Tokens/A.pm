package JGoff::Lisp::Format::Tokens::A;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has parameters => ( is => 'rw' );
has colon => ( is => 'ro', isa => 'Bool' );
has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::A - Internal token for parser

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 METHODS

=head2 format( $core )

=cut

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_parameters(
    $core, [
      [ 'mincol' => 0 ],
      [ 'colinc' => 1 ],
      [ 'minpad' => 0 ],
      [ 'padchar' => ' ' ]
    ]
  );

  die "no more arguments" if $core->num_arguments == 0;

  my $argument = $core->forward_argument;

# Strip escape characters

  if ( !defined $argument ) {
    if ( $self->colon ) {
      return '[]';
    }
    $argument = $self->_padding( 'undef' );
    return $self->_print_case( $core->print_case, $argument );
  }
  elsif ( ref( $argument ) and
          ref( $argument ) eq 'ARRAY' ) {
   my $sub = $core->new(
     stream => $core->stream,
     format => '~a',
     arguments => $argument,

     print_case => $core->print_case,
   );
   return '[' . $sub->apply . ']';
  }
  elsif ( ref( $argument ) and
          ref( $argument ) =~ /Character/ ) {
    return $argument->toString;
  }
  else {
    $argument = $self->_padding( $argument );
  }
  
  return $argument;
}

1;
