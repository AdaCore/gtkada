#! /usr/bin/env perl

use strict;
## parse all mapping comments in the .ads files
##
##  The syntax of mapping comments is :
##  --  mapping: Ada_Entity C_Header_File C_Entity
##  or
##  --  mapping: Ada_Entity C_Header_File \
##  --  mapping: C_Entity
##  (ie continuation lines are allowed)
##  or
##  --  mapping: NOT_IMPLEMENTED C_Header_File C_Entity
##  (for functions not yet implemented)
##  or
##  --  mapping: INTERNAL C_Header_File C_Entity
##  (for functions internal to gtk, which won't be implemented


##  FIXME add some checking on NOT_IMPLEMENTED (a entity can not be
##    both NOT_IMPLEMENTED and mapped. check that there are not two
##    contradictory pragma

## syntax = (c_entity => (c_file ada_file ada_entity))

my (%c2ada) = ();
my ($file);
foreach $file (<*.ads>)
{
  open (FILE, $file);
  while (<FILE>)
  {
    if (/--\s*mapping:\s*(.*)$/)
    {
      my ($map) = $1;
      if ($map =~ /\\$/)
      {
	my ($cont);
	$cont = <FILE>;
	$map =~ s/\\$//;
	$cont =~ s/^\s*--\s*mapping:\s*//;
	$map .= $cont;
      }
      $map =~ /(\S+)\s+(\S+)\s+(\S+)/;
      push (@{$c2ada{$3}}, [$2, $file, $1]);
    }
  }
  close (FILE);
}


## parse command line

my (@requests) = ();

while ($_ = shift @ARGV)
{
  if (/^-h$/) {
    &write_usage;
  }
  elsif (/^[^-]/) {
    push (@requests, $_);
  }
  else {
    &write_usage;
  }
}

&print_results;

##########################################

sub write_usage
{
  print "mapping.pl [-h] [-c files] [c_functions]\n";
  print "checks the mapping between gtk (c-library) and the "
  . "Ada binding\n";
  print " [c-functions] : prints the mapping for the c-functions\n";
  print " -h            : prints this help page\n\n";
  exit;
}

##########################################

sub print_results
{
  my ($request, $ref);
  foreach $request (@requests)
  {
    foreach $ref (@{$c2ada{$request}})
    {
      print $request, " (file ", $ref->[0],
      ") is mapped to ", $ref->[2], " (file ", $ref->[1], ")\n";
    }
  }
}

