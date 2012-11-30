package JGoff::Lisp::Format::Tokens::S;

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
      [ 'padchar' => ' ' ] ] );

  my $argument = $core->increment_argument;

# Strip escape characters

  if ( !defined $argument ) {
    if ( $self->{colon} ) {
      return '[]';
    }
    $argument = $self->_padding( 'undef' );
    return $self->_print_case( $core, $argument );
  }
  elsif ( ref( $argument ) and ref( $argument ) eq 'ARRAY' ) {
   my $sub = $core->new(
     stream => $core->stream,
     format => $self->{format},
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
    return $argument;
  }
  
  return $argument;
}

1;
