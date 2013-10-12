package JGoff::Lisp::Format::Tokens::F;

use Moose;

extends 'JGoff::Lisp::Format::Token';

has parameters => ( is => 'rw' );
has colon => ( is => 'ro', isa => 'Bool' );
has at => ( is => 'ro', isa => 'Bool' );

=head1 NAME

JGoff::Lisp::Format::Tokens::F - Internal token for parser

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
      [ w => undef ],
      [ d => 0 ],
      [ k => 0 ],
      [ overflowchar => ',' ],
      [ padchar => ',' ],
    ]
  );

  my $argument = $core->forward_argument;
  $argument = sprintf "%1f", $argument;

  $argument = $self->_commify( $argument );
  if ( $argument !~ /[.]/ ) {
    $argument .= '.0';
  }

  if ( $self->{w} and
       $self->{w} > 0 ) {
    $argument =~ s{ ^ 0+ }{}x;
    $argument =~ s{ 0+ $ }{}x;
  }

  if ( $self->{w} and
       $self->{w} > 1 and
       length( $argument ) > $self->{w} ) {
    my ( $l, $r ) = split /\./, $argument;
    my $max_digits = $self->{w} - length( $l ) - 1;
    if ( $max_digits > 0 ) {
      $r = substr( $r, 0, $max_digits );
    }
    else {
      if ( '0.' . $r >= 0.5 ) {
        $l++;
      }
      $r = '';
    }
    $argument = $l . '.' . $r;
  }

  return $argument;
}

1;
