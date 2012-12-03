package JGoff::Lisp::Format::Tokens::A;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $self->_resolve_arguments(
    $core, [
      [ 'mincol' => 0 ],
      [ 'colinc' => 1 ],
      [ 'minpad' => 0 ],
      [ 'padchar' => ' ' ]
    ]
  );

  my $argument = $core->current_argument;
  $core->forward_argument;

# Strip escape characters

  if ( !defined $argument ) {
    if ( $self->colon ) {
      return '[]';
    }
    $argument = $self->_padding( 'undef' );
    return $self->_print_case( $core->print_case, $argument );
  }
  elsif ( ref( $argument ) and ref( $argument ) eq 'ARRAY' ) {
   my $sub = $core->new(
     stream => $core->stream,
     format => '~a',
     arguments => $argument,

     print_case => $core->print_case,
   );
   return '[' . $sub->apply . ']';
  }
  elsif ( ref( $argument ) and ref( $argument ) =~ /Character/ ) {
    return $argument->toString;
  }
  else {
    $argument = $self->_padding( $argument );
  }
  
  return $argument;
}

1;
