package JGoff::Lisp::Format::Token;

use Moose;

sub _padding {
  my $self = shift;
  my ( $argument ) = @_;
  my ( $padchar, $minpad, $mincol, $colinc ) =
    @{ $self }{qw( padchar minpad mincol colinc )};

  my $padding = '';
  if ( $minpad and $minpad > 0 ) {
    $padding .= $padchar x $minpad;
  }
  if ( $padchar and $padchar =~ /./ ) {
    if ( $mincol and $mincol > length( $argument ) ) {
      if ( $colinc and $colinc > 0 ) {
        while ( length( $argument ) + length( $padding ) < $mincol ) {
          $padding .= $padchar x $colinc;
        }
      }
    }
  }

  if ( $self->at ) {
    $argument = $padding . $argument;
  }
  else {
    $argument = $argument . $padding;
  }

  if ( $argument and
       $self->{mincol} and
       $self->{mincol} > 0 and
       length( $argument ) < $self->{mincol} ) {
    $argument = ' ' x ( $self->{mincol} - length( $argument ) ) . $argument;
  }
  return $argument;
}

sub char_name {
  my $self = shift;
  my ( $char ) = @_;
  return 'Space' if $char eq ' ';
  return $char;
}

1;
