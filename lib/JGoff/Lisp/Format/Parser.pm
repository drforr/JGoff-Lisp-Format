package JGoff::Lisp::Format::Parser;

use Moose;
use Readonly;

extends 'Parser::MGC';

=head1 NAME

JGoff::Lisp::Format::Parser - Parser internals

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

Quick summary of what the module does.

    use JGoff::Lisp::Format::Parser;

    my $foo = JGoff::Lisp::Format->new();
    ...

=head1 EXPORT

A list of functions that can be exported.  You can delete this section
if you don't export anything, such as for a purely object-oriented module.

=head1 METHODS

=head2 parse( $text )

=cut

Readonly my $MODIFIERS => qr{ [:] | [@] | [:][@] | [@][:] }x;
Readonly my $INTEGER   => qr{ [-+]?\d+ }x;
Readonly my $PARAMETER => qr{ $INTEGER | '. | [vV] | [#] }x;

sub ___parse_token {
  my $self = shift;
  my ( $format, $match ) = @_;
  my $rv = { format => $format };

  $match =~ s{^~}{}; # Remove the tilde
  while ( $match =~ s{ ^ ( $PARAMETER )? , }{}x ) {
    my $value = $1;
    $value = $value + 0 if $value and $value =~ m{ ^ [-+] }x; # Numify numbers
    $value = lc $value if $value and $value eq 'V';           # Canonicalize 'V'
    push @{ $rv->{arguments} }, $value;
  }
  if ( $match =~ s{ ^ ( $PARAMETER ) }{}x ) {
    my $value = $1;
    $value = $value + 0 if $value =~ m{ ^ [-+] }x;  # Numify numbers
    $value = lc $value if $value and $value eq 'V'; # Canonicalie 'V'
    push @{ $rv->{arguments} }, $value;
  }
  elsif ( $rv->{arguments} ) {
    push @{ $rv->{arguments} }, undef;
  }
  while ( $match =~ s{ ^ ( [:@] ) }{}x ) {
    $rv->{colon} = 1 if $1 eq ':';
    $rv->{at} = 1 if $1 eq '@';
  }
  return $rv;
}

sub __token_a {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,3}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [aA]
  }x );
  return $self->___parse_token( q{~a}, $match );
}

sub __token_ampersand {
  my $self = shift;
  my $match = $self->expect( qr{ ~ (?: ( $PARAMETER )? ) [&] }x );
  return $self->___parse_token( q{~&}, $match );
}

sub __token_asterisk {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: $INTEGER [vV]
        | $PARAMETER
      )?
      ( $MODIFIERS )?
    [*]
  }x );
  return $self->___parse_token( q{~*}, $match );
}

sub __token_b {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,3}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [bB]
  }x );
  return $self->___parse_token( q{~b}, $match );
}

sub __token_c {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( $MODIFIERS )?
    [cC]
  }x );
  return $self->___parse_token( q{~c}, $match );
}

sub __token_circumflex {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,2} # XXX Not sure if it's only 2 params, but...
      ( $PARAMETER )?
      ( $MODIFIERS )?
    \^
  }x );
  return $self->___parse_token( q{~^}, $match );
}

sub __token_close_brace {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: 
      )
      ( $MODIFIERS )?
    \}
  }x );
  my $rv = {
    format => '~}',
  };
  return $rv;
}

sub __token_close_bracket {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
    \]
  }x );
  my $rv = {
    format => '~]',
  };
  return $rv;
}

sub __token_close_paren {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
    \)
  }x );
  my $rv = {
    format => '~)',
  };
  return $rv;
}

sub __token_d {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,3}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [dD]
  }x );
  return $self->___parse_token( q{~d}, $match );
}

sub __token_f {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,4} # XXX Aha, something with 4 ','s
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [fF]
  }x );
  return $self->___parse_token( q{~f}, $match );
}

sub __token_newline {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
      ( $MODIFIERS )?
    \n
  }x );
  my $rv = {
    format => '~\\n',
  };
  return $rv;
}

sub __token_o {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,3}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [oO]
  }x );
  return $self->___parse_token( q{~o}, $match );
}

sub __token_open_brace {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: $PARAMETER
      )?
      ( $MODIFIERS )?
    \{
  }x );
  my $rv = {
    format => '~{',
  };
  return $rv;
}

sub __token_open_bracket {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: $PARAMETER
      )?
      ( $MODIFIERS )?
    \[
  }x );
  my $rv = {
    format => '~[',
  };
  return $rv;
}

sub __token_open_paren {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
      ( $MODIFIERS )?
    \(
  }x );
  my $rv = {
    format => '~(',
  };
  return $rv;
}

sub __token_p {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
      ( $MODIFIERS )?
    [pP]
  }x );
  my $rv = {
    format => '~p',
  };
  return $rv;
}

sub __token_percent {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:$PARAMETER
      )?
    [%]
  }x );
  my $rv = {
    format => '~%',
  };
  return $rv;
}

sub __token_pipe {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: $PARAMETER
      )?
    [|]
  }x );
  my $rv = {
    format => '~|',
  };
  return $rv;
}

sub __token_question {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
      ( $MODIFIERS )?
    [?]
  }x );
  my $rv = {
    format => '~?',
  };
  return $rv;
}

sub __token_r {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,4}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [rR]
  }x );
  return $self->___parse_token( q{~r}, $match );
}

sub __token_s {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,3}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [sS]
  }x );
  return $self->___parse_token( q{~s}, $match );
}

sub __token_semicolon {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?:
      )
      ( $MODIFIERS )?
    [;]
  }x );
  my $rv = {
    format => '~;',
  };
  return $rv;
}

sub __token_tilde {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ (?: | [vV]
      )
    ~
  }x );
  my $rv = {
    format => '~~',
  };
  return $rv;
}

sub __token_x {
  my $self = shift;
  my $match = $self->expect( qr{
    ~ ( (?: $PARAMETER )? , ){0,3}
      ( $PARAMETER )?
      ( $MODIFIERS )?
    [xX]
  }x );
  return $self->___parse_token( q{~x}, $match );
}

sub parse {
  my $self = shift;

  $self->sequence_of( sub {
    $self->any_of(
      sub { $self->expect( '!@#$%^&*this' ) },
      sub { $self->expect( qr{
        ABC | ,,' | ,' | cat | penn | XXyy | uuVV | this | is7a | is | TEST[.]
            | [@][ab] | :a | [@]:A | [a-zA-Z()] | NO | FOO | XYZ | \[ | \]
            | [,':&]
      }x ) },
      sub { $self->__token_p },
      sub { $self->__token_asterisk },
      sub { $self->__token_semicolon },
      sub { $self->__token_a },
      sub { $self->__token_ampersand },
      sub { $self->__token_percent },
      sub { $self->__token_b },
      sub { $self->__token_open_brace },
      sub { $self->__token_open_bracket },
      sub { $self->__token_c },
      sub { $self->__token_open_paren },
      sub { $self->__token_newline },
      sub { $self->__token_close_brace },
      sub { $self->__token_close_bracket },
      sub { $self->__token_close_paren },
      sub { $self->__token_circumflex },
      sub { $self->__token_d },
      sub { $self->__token_question },
      sub { $self->__token_f },
      sub { $self->__token_asterisk },
      sub { $self->__token_o },
      sub { $self->__token_pipe },
      sub { $self->__token_r },
      sub { $self->__token_s },
      sub { $self->__token_tilde },
      sub { $self->__token_x },
    );
  } );
}

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
