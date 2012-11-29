package JGoff::Lisp::Format::Tokens::S;

use Moose;

has arguments => ( is => 'rw' );
has colon => ( is => 'ro' );
has at => ( is => 'ro' );

sub format {
  my $self = shift;
  my ( $core ) = @_;
  $core->_resolve_arguments(
    $self, [
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
    $argument = $core->_padding( $self, 'undef' );
    return $core->_print_case( $argument );
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
    $argument = $core->_padding( $self, $argument );
    return $argument;
  }
  
  return $argument;
}

1;
