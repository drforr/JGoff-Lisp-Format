package JGoff::Lisp::Format::Tokens::X;

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
      [ 'padchar' => ' ' ],
      [ 'commachar' => ',' ],
      [ 'comma-interval' => 3 ],
    ]
  );

  return $core->_argument_to_base( 16, $self );
}

# }}}
1;
