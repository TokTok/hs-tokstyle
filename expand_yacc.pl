#!/usr/bin/env perl

# SPDX-License-Identifier: GPL-3.0-or-later
#
# This program expands the parameterised productions in a Happy (Haskell parser
# generator) grammar. Happy supports parameterised productions, but no types for
# them, so we do the expansion in a preprocessing step where we can support
# types. We want types because it speeds up compilation of the generated parser
# by about 50%.
#
# See https://www.haskell.org/happy/doc/html/sec-compilation-time.html.

use strict;
use warnings FATAL => 'all';
no warnings 'experimental';
use utf8;
use 5.010;

use constant {
   PREAMBLE             => 0,
   PRODUCTIONS          => 1,
   MULTILINE_PRODUCTION => 2,
   POSTAMBLE            => 3,
};

##############################################################################
#
# :: Print grammar data structure as string.
#
##############################################################################

sub show_production {
   my ($res, $prod) = @_;

   my $line = join " ", @{ $prod->{defn} };
   push @$res, "\t" . $line . $prod->{tabs} . "{ $prod->{code} }\n";
}

sub show_nonterm {
   my ($res, $name, $nonterm) = @_;

   push @$res, "$name :: { $nonterm->{type} }";
   my @prod_res;
   for my $prod (@{ $nonterm->{productions} }) {
      show_production \@prod_res, $prod;
   }
   push @$res, ':' . join '|', @prod_res;
}

sub show_nonterms {
   my ($res, $start, $nonterms) = @_;

   show_nonterm $res, $start, $nonterms->{$start};
   delete $nonterms->{$start};

   for my $nt (sort keys %$nonterms) {
      show_nonterm $res, $nt, $nonterms->{$nt};
   }
}

sub print_grammar {
   my ($grammar, $file) = @_;
   open my $fh, '>', $file or die "$file: $!";

   print $fh "$_\n" for @{ $grammar->{preamble} };
   print $fh "%%\n";

   my @res;
   show_nonterms \@res, $grammar->{start}, $grammar->{nonterms};
   print $fh "$_\n" for @res;

   print $fh "$_\n" for @{ $grammar->{postamble} };
}

##############################################################################
#
# :: Parse a grammar file into a grammar data structure.
#
##############################################################################

sub parse_production {
   my ($prod) = @_;
   return [] unless $prod;
   [$prod =~ m/('[^']+'|[A-Za-z_()%]+)+/g]
}

sub parse {
   my ($file) = @_;

   my @lines = do { open my $fh, '<', $file or die "$file: $!"; <$fh> };
   chomp for @lines;

   my $state = PREAMBLE;

   # Global state.
   my @preamble;  # Code preamble, configs, and tokens.
   my $start;     # Name of the starting non-terminal.
   my %funcs;     # Parameterised rules (i.e. functions generating rules).
   my %nonterms;  # All the nonterminals in the parser.
   my @postamble; # Code post-amble, at the end of the parser program.

   # Local state.
   my $nonterm;   # Current non-terminal we're processing.
   my $type;      # Type of the upcoming non-terminal.
   my $prod;      # Current multi-line production.

   for my $line (@lines) {
      given ($state) {
         when (PREAMBLE) {
            if ($line eq "%%") {
               $state = PRODUCTIONS;
               next;
            }
            push @preamble, $line;
         }
         when (PRODUCTIONS) {
            next unless $line;

            if ($line eq "{") {
               push @postamble, $line;
               $state = POSTAMBLE;
               next;
            }

            if ($line =~ m/^(\w+) :: \{ (.*) \}$/) {
               $type = $2;
               next;
            }
            
            if ($line =~ m/^(\w+)(\((\w+)\))?$/) {
               if ($3) {
                  $nonterm = $funcs{$1} = { param => $3 };
               } else {
                  $nonterm = $nonterms{$1} = {};
               }
               $nonterm->{type} = $type;
               $start = $1 unless $start;
               next;
            }

            if ($line =~ m/^[:|]\t([^\t]+)?(\t+)\{ (.*) \}/) {
               push @{ $nonterm->{productions} }, {
                  defn => parse_production ($1),
                  tabs => $2,
                  code => $3,
               };
               next;
            }

            if ($line =~ m/^[:|]\t([^\t]+)$/) {
               $state = MULTILINE_PRODUCTION;
               $prod = $1;
               next;
            }

            if ($line =~ m/^-- .*/) {
               next;
            }

            die "Unhandled: '$line'";
         }
         when (MULTILINE_PRODUCTION) {
            if ($line =~ m/^\t([^\t]+)(\t+)\{ (.*) \}/) {
               $prod .= " $1";
               push @{ $nonterm->{productions} }, {
                  defn => parse_production ($prod),
                  tabs => $2,
                  code => $3,
               };
               $state = PRODUCTIONS;
               next;
            }

            if ($line =~ m/^\t([^\t]+)$/) {
               $prod .= " $1";
               next;
            }

            die "Unhandled: '$line'";
         }
         when (POSTAMBLE) {
            push @postamble, $line;
         }
         default {
            die "Invalid state: $state"
         }
      }
   }

   {
      preamble  => \@preamble,
      start     => $start,
      funcs     => \%funcs,
      nonterms  => \%nonterms,
      postamble => \@postamble,
   }
}

##############################################################################
#
# :: Parameterised rule instantiation.
#
##############################################################################

sub instantiate_prod_defn {
   my ($defn, $param, $arg) = @_;
   die unless $arg;

   my @instantiated;
   for my $word (@$defn) {
      die unless $word;
      if ($word eq $param) {
         push @instantiated, $arg;
      } elsif ($word =~ m/^(\w+)\($param\)$/) {
         push @instantiated, "$1($arg)";
      } else {
         push @instantiated, $word;
      }
   }

   \@instantiated
}

sub type {
   my ($funcname) = @_;
   return "Maybe (Node String)" if $funcname eq "Opt";
   'Node String'
}

sub instantiate_func {
   my ($grammar, $funcname, $arg) = @_;
   my $mangled_name = "${funcname}_${arg}_";

   # If we already instantiated it, we don't do it again.
   return $mangled_name if $grammar->{nonterms}{$mangled_name};

   my $func = $grammar->{funcs}{$funcname};

   my %nonterm = (type => type $funcname);
   for my $prod (@{ $func->{productions} }) {
      push @{ $nonterm{productions} }, {
         defn => instantiate_prod_defn ($prod->{defn}, $func->{param}, $arg),
         tabs => $prod->{tabs},
         code => $prod->{code},
      };
   }

   $grammar->{nonterms}{$mangled_name} = \%nonterm;

   $mangled_name
}

sub instantiate_rule {
   my ($grammar, $word) = @_;

   if (my ($funcname, $arg) = $word =~ m/(\w+)\((\w+)\)/) {
      $word = instantiate_func $grammar, $funcname, $arg;
   }

   $word
}

sub instantiate_production {
   my ($grammar, $defn) = @_;

   my @res;
   for my $word (@$defn) {
      push @res, instantiate_rule $grammar, $word;
   }

   \@res
}

sub instantiate_grammar {
   my ($grammar) = @_;

   my $nonterms = $grammar->{nonterms};

   my $continue = 1;
   while ($continue) {
      $continue = 0;
      for my $nt (sort keys %$nonterms) {
         for my $prod (@{ $nonterms->{$nt}{productions} }) {
            if (grep { /\)$/ } @{ $prod->{defn} }) {
               $prod->{defn} = instantiate_production $grammar, $prod->{defn};
               $continue = 1;
            }
         }
      }
   }
}

##############################################################################
#
# :: Main program.
#
##############################################################################

my $grammar = parse $ARGV[0];

instantiate_grammar $grammar;
print_grammar $grammar, $ARGV[1];
