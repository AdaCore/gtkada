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
##  FIXME try to align the arguments on ':' (pretty printing :-)

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
my ($check_mode) = 0;
my ($unimplemented) = 0;

while ($_ = shift @ARGV)
{
  if (/^-h$/) {
    &write_usage;
  }
  elsif (/^-c$/) {
    $check_mode = 1;
  }
  elsif (/^-u$/) {
    $check_mode = 1;
    $unimplemented = 1;
  }
  elsif (/^[^-]/) {
    push (@requests, $_);
  }
  else {
    &write_usage;
  }
}

if ($check_mode == 0) {
  &print_results;
} else {
  &check_undefined ($unimplemented);
}

##########################################

sub write_usage
{
  print "mapping.pl [-h] [-c files] [c_functions]\n";
  print "checks the mapping between gtk (c-library) and the "
  . "Ada binding\n";
  print " [c-functions] : prints the mapping for the c-functions\n";
  print " -c files      : outputs the Ada binding for every function not "
  . "mapped\n";
  print " -h            : prints this help page\n\n";
  print " -u files      : outputs a NOT IMPLEMENTED comment for every "
  . "function not mapped\n";
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

###########################################

sub check_undefined
{
  my ($not_implemented) = shift;
  my ($func_name) = "";
  my ($return) = "";
  my ($arguments) = "";
  my (@list) = grep (/\.h$/,@requests);
  
  ##  only looks in valid .h files
  foreach $file (@list)
  {
    print "opening $file\n" if (scalar (@list) > 1);
    ##  Looks for c-functions in the .h file
    ##  We suppose that the files are coded in gth-like style
    open (FILE, $file);

    while (<FILE>)
    {
      chop;
      if (/^(\S+)\s+(g[td]k_\S+)\s+\((.*)/)
      {
	$func_name = $2;
	$return = $1;
	$arguments = $3;
	if ($3 !~ /\);/)
	{
	  while (<FILE>)
	  {
	    chop;
	    /\s+(.*)/;
	    $arguments .= $1;
	    last if ($1 =~ /\);/);
	  }
	}
	$arguments =~ s/\s+/ /g;
	$arguments =~ s/\);//;

	### Check if the function is mapped
	my ($is_mapped) = 0;
	my ($ref);
	foreach $ref (@{$c2ada{$func_name}})
	{
	  $is_mapped = 1 if (&file_name ($file) eq $ref->[0]);
	}

	if ($is_mapped == 0)
	{
	  unless ($not_implemented)
	  {
	    my ($string) = "-- " . &ada_style ($func_name) . " --";
	    
	    print '#' x 50, "\n";
	    &print_function (&file_name ($file), $return,
			     $func_name, $arguments, 0);
	    print '-' x length ($string),
	    "\n$string\n", '-' x length ($string), "\n\n";
	    &print_function (&file_name ($file), $return,
			     $func_name, $arguments, 1);
	  }
	  else
	  {
	    &print_not_implemented (&file_name ($file), $func_name);
	  }
	}
      }
    }
    close (FILE);
  }
}


#######################################################

sub convert_c
{
  my ($type) = shift;

  if ($type eq "gint") {
    return "Gint";
  } elsif ($type eq "guint") {
    return "Guint";
  } elsif ($type eq "constgchar*") {
    return "String";
  } elsif ($type =~ /G[dt]k([^*]+)\*/) {
    return "System.Address";
  } else {
    return $type;
  }
}

#######################################################

sub convert_type
{
  my ($type) = shift;

  if ($type eq "gint") {
    return "Gint";
  } elsif ($type eq "guint") {
    return "Guint";
  } elsif ($type eq "constgchar*") {
    return "String";
  } elsif ($type =~ /(G[dt]k)([^*]+)\*/) {
    my ($t) = $2;
    my ($prefix) = $1;
    return "Object'Class" if ($t eq "Object");
    $t =~ s/(.)([A-Z])/$1_$2/g;
    return "$prefix.$t.$t\'Class";
  } else {
    $type =~ s/(.)([A-Z])/$1_$2/g;
    return $type;
  }
}

########################################################

sub print_arguments
{
  my ($arguments) = shift;
  my ($indent) = shift;
  my ($convert) = shift;
  my ($first) = 1;

  print " (";
  $indent = $indent . '  ';
    
  foreach (split (",", $arguments))
  {
    my ($type, $name) = /^(.*[ *])(\w+)$/;
    $type =~ s/\s//g;
    print ";\n$indent" unless ($first);
    print &ada_style ($name), " : in ", &{$convert} ($type);
    $first = 0;
  }
  print ")";
}

#######################################################

sub file_name
{
  my ($name) = shift;
  $name =~ /^.*\/([^\/]+)/;
  return $1;
}

########################################################

sub ada_style
{
  my ($entity) = shift;
  my ($i);

  substr ($entity, 0, 1) = uc (substr ($entity, 0, 1));
  
  for ($i = 1; $i < length ($entity); $i ++)
  {
    if (substr ($entity, $i, 1) eq '_')
    {
      substr ($entity, $i + 1, 1) = uc (substr ($entity, $i + 1, 1));
    }
  }
  $entity =~ s/^G[td]k_//;
  return $entity;
}

########################################################

sub print_function
{
  my ($file)   = shift;
  my ($return) = shift;
  my ($func_name) = shift;
  my ($arguments) = shift;
  my ($is_body) = shift;
  my ($string);
  
  $string = ($return eq "void" ? "procedure " : "function ");
  $string .= &ada_style ($func_name);
  print $string;
  if ($arguments ne "void")
  {
    print_arguments ($arguments, ' ' x length ($string), \&convert_type);
  }
  if ($return ne "void")
  {
    print " return ", &convert_type ($return);
  }

  if (!$is_body)
  {
    print ";\n--  mapping: ", &ada_style ($func_name), " $file $func_name\n";
    return;
  }

  print "\nis\n";
  $string = "   " . ($return eq "void" ? "procedure " : "function ")
  . "Internal";
  print $string;
  if ($arguments ne "void")
  {
    print_arguments ($arguments, ' ' x length ($string), \&convert_c);
  }
  if ($return ne "void")
  {
    print " return ", &convert_c ($return);
  }
  print ";\n   pragma Import (C, Internal, \"$func_name\");\nbegin\n   Internal";
  if ($arguments ne "void")
  {
    my ($first) = 1;
    print " (";
    foreach (split (",", $arguments))
    {
      my ($type, $name) = /^(.*[ *])(\w+)$/;
      print ", " unless ($first);
      if ( $type =~ /G[td]k([^*]+)\*/)
      {
	print "Get_Object (", &ada_style ($name), ")";
      }
      else
      {
	print &ada_style ($name);
      }
      $first = 0;
    }
    print ")";
  }
  print ";\nend ", &ada_style ($func_name), ";\n";
}

##########################################

sub print_not_implemented
{
  my ($file) = shift;
  my ($func_name) = shift;

  print "   --  mapping: NOT_IMPLEMENTED $file $func_name\n";
}
