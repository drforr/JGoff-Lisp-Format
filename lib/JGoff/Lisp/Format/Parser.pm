package JGoff::Lisp::Format::Parser;

use Moose;
use Readonly;
use Carp qw( croak );

use JGoff::Lisp::Format::Tokens::Text;
use JGoff::Lisp::Format::Tokens::A;
use JGoff::Lisp::Format::Tokens::Ampersand;
use JGoff::Lisp::Format::Tokens::Asterisk;
use JGoff::Lisp::Format::Tokens::B;
use JGoff::Lisp::Format::Tokens::C;
use JGoff::Lisp::Format::Tokens::Circumflex;
use JGoff::Lisp::Format::Tokens::D;
use JGoff::Lisp::Format::Tokens::F;
use JGoff::Lisp::Format::Tokens::Newline;
use JGoff::Lisp::Format::Tokens::O;
use JGoff::Lisp::Format::Tokens::Open_Brace;
use JGoff::Lisp::Format::Tokens::P;
use JGoff::Lisp::Format::Tokens::Percent;
use JGoff::Lisp::Format::Tokens::Question;
use JGoff::Lisp::Format::Tokens::R;
use JGoff::Lisp::Format::Tokens::S;
use JGoff::Lisp::Format::Tokens::Text;
use JGoff::Lisp::Format::Tokens::Tilde;
use JGoff::Lisp::Format::Tokens::Vertical_Bar;
use JGoff::Lisp::Format::Tokens::X;

extends 'Parser::MGC';

=head1 NAME

JGoff::Lisp::Format::Parser - Parser internals

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Internal format-string parser for Lisp::Format::Parser.

    use JGoff::Lisp::Format::Parser;

    my $p = JGoff::Lisp::Format::Parser->new( patterns => { ws => undef } );
    my $tree = $p->from_string( '~~d@:a' );

=head1 METHODS

=head2 parse( $text )

=cut

# Regular expressions for later use.
#
Readonly my $MODIFIERS  => qr{ ( [:] | [@] | [:][@] | [@][:] ) }x;
Readonly my $INTEGER    => qr{ [-+]?\d+ }x;
Readonly my $PARAMETER  => qr{ ( $INTEGER | '. | [vV] | [#] ) }x;
Readonly my $PARAMETERS => qr{ ( $PARAMETER? , ) }x;

# {{{ ___parse_token

# Tear apart the token to get at the component args.
# This will probably be replaced with REs when I figure out a nice way to do so.
#
sub ___parse_token {
  my $self = shift;
  my ( $match ) = @_;
  my $rv = {};

  $match =~ s{^~}{}; # Remove the tilde
  while ( $match =~ s{ ^ $PARAMETER? , }{}x ) {
    my $value = $1;
    #if ( $value ) {
    if ( defined $value ) {
      $value = $value + 0 if $value =~ m{ ^ [-+] }x; # Numify numbers
      $value = lc $value if $value eq 'V';           # Canonicalize 'V'
    }
    push @{ $rv->{arguments} }, $value;
  }
  if ( $match =~ s{ ^ $PARAMETER }{}x ) {
    my $value = $1;
    if ( $value ) {
      $value = $value + 0 if $value =~ m{ ^ [-+] }x;  # Numify numbers
      $value = lc $value if $value eq 'V'; # Canonicalie 'V'
    }
    push @{ $rv->{arguments} }, $value;
  }
  elsif ( $rv->{arguments} ) {
    push @{ $rv->{arguments} }, undef;
  }
  while ( $match =~ s{ ^ ( [:@] ) }{}x ) {
    $rv->{colon} = 1 if $1 eq ':';
    $rv->{at} = 1 if $1 eq '@';
  }
  croak "format has too many characters remaining! ($match)" if
    length($match) > 1;
  $match = q{\n} if $match eq qq{\n};
  $rv->{format} = q{~} . lc( $match );
  return $rv;
}

# }}}

# Basic Output - c % & | ~
# Radix Control - r d b o x
# Floating-point printers - f e g $
# Printer operations - a s w
# Pretty-printer operations - _ < i /
# Layout control - t < >
# Control flow - * [ ] { } ?
# Miscellaneous operations - ( ) p
# Miscellaneous pseudo-operations ; ^ \n

# Upper-level token types.
#
sub __token_a_b_d_o_s_x {
  my $self = shift;
  my $match =
    $self->expect( qr{
      ~ $PARAMETERS{0,3} $PARAMETER? $MODIFIERS?
        [aAbBdDoOsSxX]
    }x );
  my $rv = $self->___parse_token( $match );

  return JGoff::Lisp::Format::Tokens::A->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~a};

  return JGoff::Lisp::Format::Tokens::B->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~b};

  return JGoff::Lisp::Format::Tokens::D->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~d};

  return JGoff::Lisp::Format::Tokens::O->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~o};

  return JGoff::Lisp::Format::Tokens::S->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~s};

  return JGoff::Lisp::Format::Tokens::X->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  );
}

sub __token_f_r {
  my $self = shift;
  my $match =
    $self->expect( qr{ ~ $PARAMETERS{0,4} $PARAMETER? $MODIFIERS? [fFrR] }x );
  my $rv = $self->___parse_token( $match );

  return JGoff::Lisp::Format::Tokens::F->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~f};
  
  return JGoff::Lisp::Format::Tokens::R->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  );
}

sub __token_ampersand_percent_vertical_bar_tilde {
  my $self = shift;
  my $match = $self->expect( qr{ ~ $PARAMETER? [&%|~] }x );
  my $rv = $self->___parse_token( $match );

  return JGoff::Lisp::Format::Tokens::Ampersand->new
    if $rv->{format} eq q{~&};

  return JGoff::Lisp::Format::Tokens::Percent->new(
    n => defined $rv->{arguments} &&
         @{ $rv->{arguments} } ? $rv->{arguments}->[0] : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~%};

  return JGoff::Lisp::Format::Tokens::Tilde->new(
    n => defined $rv->{arguments} &&
         @{ $rv->{arguments} } ? $rv->{arguments}->[0] : undef,
  ) if $rv->{format} eq q{~~};

  return JGoff::Lisp::Format::Tokens::Vertical_Bar->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  );
}

sub __token_asterisk_open_bracket {
  my $self = shift;
  my $match = $self->expect( qr{ ~ $PARAMETER? $MODIFIERS? [*\[] }x );
  my $rv = $self->___parse_token( $match );
  return JGoff::Lisp::Format::Tokens::Asterisk->new(
    n => defined $rv->{arguments} &&
         @{ $rv->{arguments} } ? $rv->{arguments}->[0] : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~*};

  return $rv;
}

sub __token_c_newline_open_paren_p_question_semi {
  my $self = shift;
  my $match = $self->expect( qr{ ~ $MODIFIERS? [cC\(\npP?;] }x );
  my $rv = $self->___parse_token( $match );
  return JGoff::Lisp::Format::Tokens::C->new(
    colon => $rv->{colon}
  ) if $rv->{format} eq q{~c};

  return JGoff::Lisp::Format::Tokens::P->new(
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~p};

  return JGoff::Lisp::Format::Tokens::Question->new(
    at => $rv->{at}
  ) if $rv->{format} eq q{~?};

  return JGoff::Lisp::Format::Tokens::Newline->new(
    colon => $rv->{colon},
    at => $rv->{at}
  ) if $rv->{format} eq q{~\n};

  return $rv;
}

sub __token_circumflex {
  my $self = shift;
  my $match =
    $self->expect( qr{ ~ $PARAMETERS{0,2} $PARAMETER? $MODIFIERS? \^ }x );
  my $rv = $self->___parse_token( $match );
  return JGoff::Lisp::Format::Tokens::Circumflex->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  );
}

sub __token_close_bracket_close_paren {
  my $self = shift;
  my $match = $self->expect( qr{ ~ [\]\)] }x );
  my $rv = {
    format => $match
  };
  return $rv;
}

sub __token_open_brace {
  my $self = shift;
  my $match = $self->expect( qr{ ~ $PARAMETER? $MODIFIERS? \{ }x );
  my $rv = $self->___parse_token( $match );
  return JGoff::Lisp::Format::Tokens::Open_Brace->new(
    arguments => defined $rv->{arguments} &&
                 @{ $rv->{arguments} } ? $rv->{arguments} : undef,
    colon => $rv->{colon},
    at => $rv->{at}
  );
}

sub __token_close_brace {
  my $self = shift;
  my $match = $self->expect( qr{ ~ $PARAMETER? $MODIFIERS? \} }x );
  return $self->___parse_token( $match );
}

# {{{ __token_text

sub __token_text {
  my $self = shift;
  return JGoff::Lisp::Format::Tokens::Text->new( text =>
    $self->any_of(
      sub { $self->expect( '!@#$%^&*this' ) }, # XXX
      sub { $self->expect( qr{
        ,,' | ,' | [a-zA-Z0-9.()<>]+ | [@][ab] | :a | [@]:A | \[ | \] | [ ]+
            | [,':&]
      }x ) },
    )
  );
}

# }}}

# {{{ _atom

sub _atom {
  my $self = shift;

  $self->any_of(
    sub { [
      $self->__token_open_brace,
      $self->_atoms,
      $self->__token_close_brace
    ] },
    sub { $self->__token_text },
    sub { $self->__token_asterisk_open_bracket },
    sub { $self->__token_a_b_d_o_s_x },
    sub { $self->__token_ampersand_percent_vertical_bar_tilde },
    sub { $self->__token_c_newline_open_paren_p_question_semi },
    sub { $self->__token_close_bracket_close_paren },
    sub { $self->__token_circumflex },
    sub { $self->__token_f_r },
  );
}

# }}}

sub _atoms {
  my $self = shift;

  $self->sequence_of( sub { $self->_atom } );
}

sub parse {
  my $self = shift;
  $self->_atoms;
}
sub pattern_ws { qr{} }

=head1 AUTHOR

Jeff Goff, C<< <jgoff at cpan.org> >>

 =head1 BUGS

Please report any bugs or feature requests to C<bug-jgoff-lisp-format at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JGoff-Lisp-Format>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.




=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc JGoff::Lisp::Format


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JGoff-Lisp-Format>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/JGoff-Lisp-Format>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/JGoff-Lisp-Format>

=item * Search CPAN

L<http://search.cpan.org/dist/JGoff-Lisp-Format/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2012 Jeff Goff.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of JGoff::Lisp::Format
