#! /usr/bin/env perl

use strict;

## syntax = (c_entity => (ada_file ada_entity))

my (%c2ada) = ();
my ($file);

my ($function) = "";

foreach $file (<*.ad[bs]>)
{
    open (FILE, $file);
    while (<FILE>)
    {
	if (/(procedure|function)\s+([\w_.]+)/
	    &&
	    $2 !~ /Internal/i)
	{
	    $function = $2;
	}
	elsif (/pragma\s+Import\s+\(C,[^,]+,\s+\"([^\"]+)\"/)
	{
	    push (@{$c2ada{$1}}, [$file, $function]);
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
      print $request, " is mapped to ", $ref->[1], " (file ", $ref->[0], ")\n";
    }
  }
}

