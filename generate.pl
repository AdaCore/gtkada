#! /usr/bin/env perl

use strict;

if ($#ARGV < 0) {
  print "Syntax : generate.pl file_name [definition-file unit_name]\n";
  print "   file_name : name of the C file to parse\n";
  print "   definition_file : file to parse for the struct definition\n";
  print "   unit_name : if present, only functions include this name will be \n";
  print "               generated\n";
  print " ex/  generate.pl ../include/gdk/gdk.h ../include/gdk/gdktypes.h window\n";
  print " ex/  generate.pl ../include/gtk/gtkframe.h\n";
  exit;
}

my ($file) = $ARGV [0] || die "must give a filename!!\n";

open (FILE, $file);
my (@cfile) = <FILE>;
close (FILE);


my ($directory);
my ($unit_name) = "";
my ($definition_file) = $file;

($directory, $file) = ($file =~ /^(.*)\/([^\/]+)\.h$/);
my ($hfile) = $file . ".h";
my ($prefix) = ($file =~ /(g[dt]k)/);
substr ($prefix, 0, 1) = uc (substr ($prefix, 0, 1));

if ($#ARGV >= 1) {
  $definition_file = $ARGV [1];
}
if ($#ARGV >= 2) {
  $unit_name = $ARGV [2];
  substr ($unit_name, 0, 1) = uc (substr ($unit_name, 0, 1));
  $file = $prefix.$unit_name;
}

my (@output) = ();
my (%with_list) = ();

my ($current_package) = &create_ada_name ($file);
$file = uc ($file);

$file = "GTKFILESELECTION" if ($file eq "GTKFILESEL");
$file = "GTKFONTSELECTION" if ($file eq "GTKFONTSEL");
$file = "GTK$1BUTTONBOX" if ($file =~ /GTK([VH]?)BBOX/);


my (%functions) = &parse_functions;
my ($abstract) = 0;
# 1 if the type is an abstract type
# Set 1 on the previous line if you want to eventually create abstract types.

## Create some new functions for every field to get
my ($ctype_package) = "$prefix$current_package*";
$ctype_package =~ s/_//g; 

my ($parent) = "Root_Type";
my (%fields) = ();
&parse_definition_file ($definition_file);

if ($parent eq "gpointer") {
  $parent = "Root_Type";
}

foreach (keys %fields)
  {
    push (@{$functions{"ada_" . lc ($current_package) . "_get_$_"}},
	  $fields{$_}, "$ctype_package Widget");
  }

&print_copyright_notice;
&generate_specifications;
&print_copyright_notice;
&generate_body;
&generate_c_functions;

### END ###############

######################################
## Prints the copyright notice
######################################

sub print_copyright_notice
{
    print <<'EOF';
-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------
EOF
}


######################################
## Parse the definition file
## PARAMS : name of the file to parse
## modifies  $parent, %fields
######################################

sub parse_definition_file
  {
    my ($filename) = shift;
    local (*FILE);
    my (@deffile);
    open (FILE, $filename);
    @deffile = <FILE>;
    close (FILE);
    
    my ($line) = 0;
    for ($line = 0; $line < $#deffile; $line ++)
      {
	last if ($deffile[$line] =~ /^STRUCT\s+_$file\s*$/i);
      }
    
    if ($line < $#deffile) {
      $deffile[$line] =~ /struct\s+_(.*)/;
      $current_package = &create_ada_name ($1);
      
      my ($in_comment) = 0;
      $line += 2;  ## skip the '{' line
      if ($deffile[$line] =~ /\/\*/) ## If we have a comment, skip it
	{
	  1 while ($deffile[++$line] !~ /\*\//);
	  $line++;
	}
      
      ### Look for the parent widget
      
      $parent = (split (/\s+/, $deffile[$line])) [1];
  
      $line++;
      while ($deffile[$line] !~ /\}/)
	{
	  if ($deffile[$line] =~ /\/\*/) ## If we have a comment, skip it
	    {
	      $line++ while ($deffile[$line] !~ /\*\//);
	      $line++;
	    }
	  $line++ while ($deffile[$line] =~ /^\s*$/);
	  chop ($deffile[$line]);
	  $deffile[$line] =~ s/\s+/ /g;
	  $deffile[$line] =~ s/ $//;
	  $deffile[$line] =~ s/^ //;
	  $deffile[$line] =~ s/ \*/\* /;  ## attach the pointer to the type not to the field
	  $deffile[$line] =~ s/;//;
	  my ($type, $field) = split (/ /, $deffile[$line]);
	  
	  print STDERR "Create a function for the field $field (of type $type) [n]?";
	  my ($answer) = scalar (<STDIN>);
	  if ($answer =~ /^y/)
	    {
	      $fields {$field} = $type;
	    }
	  $line ++;
	}
    }
  }
    
###################################
##  Generates the package specification file
###################################
sub generate_specifications
  {
    %with_list = ();
    @output = ();
    $with_list {"with " . &package_name ($parent)} ++;
    foreach (sort {&ada_func_name ($a) cmp &ada_func_name ($b)} keys %functions)
      {
	&print_declaration ($_, @{$functions{$_}});
      }
    my ($parent_string) = &package_name ($parent). ".$prefix\_" . &create_ada_name ($parent);
    $parent_string = "Root_Type" if ($parent eq "Root_Type");

    unshift (@output, "   type $prefix\_$current_package is "
	     . ($abstract ? "abstract " : "")
	     . "new " . $parent_string . " with private;\n\n");
    unshift (@output, "package $prefix.$current_package is\n\n");
    
    push (@output, "\nprivate\n");
    push (@output, "   type $prefix\_$current_package is "
	  . ($abstract ? "abstract " : "")
	  . "new ".
	  &package_name ($parent). ".$prefix\_", &create_ada_name ($parent).
	  " with null record;\n\n");

    ## Since beta 0.2.1, we no longer print the mapping comments at the end
    ## of the files
#    foreach (sort {&ada_func_name ($a) cmp &ada_func_name ($b)} keys %functions)
#      {
#	&print_comment ($_, @{$functions{$_}});
#      }
    
    
    push (@output, "end $prefix.$current_package;\n");
    
    print "\n", join (";\n", sort keys %with_list), ";\n"; 
    print "\n", join ("", @output);
  }

###################################
##  Generates the package body file
###################################
sub generate_body
  {
    %with_list = ("with Gdk; use Gdk" => 1,
		  "with System" => 1);
    @output = ();
    push (@output, "package body $prefix.$current_package is\n\n");
    
    foreach (sort {&ada_func_name ($a) cmp &ada_func_name ($b)} keys %functions)
      {
	&print_body ($_, @{$functions{$_}});
      }
    
    push (@output, "end $prefix.$current_package;\n");

    ## If there is indeed a body
    if ($#output > 3) {
      print "\n", join (";\n", sort keys %with_list), ";\n"; 
      print "\n", join ("", @output);
    }
  }

#######################
## Generates the C functions needed to retrieve the fields
#######################
sub generate_c_functions
  {
    return if (scalar (keys %fields) == 0);

    @output = ();
    push (@output, "/******************************************\n");
    push (@output, " ** Functions for $current_package\n");
    push (@output, " ******************************************/\n\n");

    foreach (sort keys %fields)
      {
	my ($ctype) = $current_package;
	$ctype =~ s/_//g;
	push (@output, $fields{$_} . "\n");
	push (@output, "ada_" . lc ($current_package) . "_get_$_ (");
	push (@output, "$prefix$ctype* widget)\n{\n   return widget->$_;\n}\n\n");
      }
    print "\n\n", join ("", @output);
  }

#######################
## Create a valid Ada type name
## Entry : the C type name
#######################
sub create_ada_name
  {
    my ($entity) = shift;
    my ($i);
    my ($char);

    $entity =~ s/([^_])([A-Z])/$1_$2/g;
    
    substr ($entity, 0, 1) = uc (substr ($entity, 0, 1));
    
    for ($i = 1; $i < length ($entity); $i ++)
      {
	$char = substr ($entity, $i, 1); 
	if ($char eq '_')
	  {
	    substr ($entity, $i + 1, 1) = uc (substr ($entity, $i + 1, 1));
	  }
      }
    $entity =~ s/^G[dt]k_?//;

    return "The_Type" if ($entity eq "Type");
    return "The_End" if ($entity eq "End");
    return "The_Digits" if ($entity eq "Digits");    
    return $entity;
  }

#######################
## Return a valid package name
## Entries: the C type implemented in the package
#######################
sub package_name
  {
    my ($entity) = shift;
    
    $entity =~ s/(G[td]k)/$1./;
    $entity =~ s/([a-z])([A-Z])/$1_$2/g;
    $entity =~ s/Gtk\.Range/Gtk\.Gtk_Range/;
    $entity =~ s/Gtk\.Entry/Gtk\.Gtk_Entry/;    
    return $entity;
  }

#######################
## Look through the C file for every function definition
## This is based on the gtk coding style, and will probably won't work
## correctly with any C file
#######################
sub parse_functions
  {
    my ($func_name);
    my ($return);
    my (@arguments);
    my (%functions);
    
    while ($_ = shift @cfile)
      {
	chop;
	s/ \*/\* /g;
	if (/^(\S+)\s+(g[dt]k_\S+)\s*\((.*)/)
	  {
	    $func_name = $2;
	    $return = $1;
	    my ($tmp) = $3;
	    $tmp =~ s/,\s*$//;
	    @arguments = split (/,/, $tmp);
	    if ($3 !~ /\);/)
	      {
		while ($_ = shift @cfile)
		  {
		    chop;
		    /\s+(.*)/;
		    push (@arguments, grep ($_ !~ /^$/, split (/,/, $1)));
		    last if ($1 =~ /\);/);
		  }
	      }
	    if ($unit_name eq ""
	       || $func_name =~ /$prefix\_$unit_name/i) {
	      push (@{$functions{$func_name}},
		    $return, @arguments);
	    }
	  }
      }
    return %functions;
  }



#########################
## Print the declaration for a subprogram corresponding to a gtk_..._new
## Entries : C function name
##           An array containing the arguments to the function
#########################
sub print_new_declaration
  {
    my ($func_name) = shift;
    my (@arguments) = @_;
    my ($string);
    my ($indent) = "";

    $string = "   procedure $prefix\_New";
    push (@output, $string);
    $indent = ' ' x (length ($string));
    &print_arguments ($indent, "void", \&convert_ada_type,
		      3, 1, @arguments);
    $abstract = 0;
  }

#########################
## Print the argument list for a subprogram, and the return statement if any
## Entries : A string of blank space indicating the position on the current
##               line
##           C return type for the function
##           A reference to the function to be used for type conversion
##           The base indentation of the line (3 for Ada fonction, 6 for Internal)
##           1 if we are generating the list for Gtk_New, 0 sinon
##           An array containing the arguments to the function
#########################
sub print_arguments
  {
    my ($indent) = shift;
    my ($return) = shift;
    my ($convert) = shift;
    my ($baseindent) = shift;
    my ($for_gtk_new) = shift;
    my (@arguments) = @_;
    my ($longest) = ($return ne "void") ? 6 : 0;

    if ($#arguments != 0
	|| ($for_gtk_new && $#arguments > 0))   ## More than one argument ?
      {
	$indent = (' ' x $baseindent) . "   ";
	    push (@output, "\n$indent");
      }
    elsif ($arguments[0] !~ /void/ || $for_gtk_new || $return ne "void")
      {
	push (@output, " ");
	$indent .= ' ';
      }
    
    if ($arguments[0] !~ /void/ || $for_gtk_new)
      {
	push (@output, "(");
	$indent .= ' ';
	my (@variables) = ();
	my (@types) = ();
	if ($for_gtk_new)
	  {
	    push (@variables, "Widget");
	    push (@types, "out $prefix\_$current_package");
	  }
	
	foreach (@arguments)
	  {
	    last if (/void/);
	    s/\s+/ /g;
	    s/,//;
	    s/\);//;
	    my ($type, $name) = /^(.*[ *])(\w+)\s*$/;
	    $type =~ s/\s//g;
	    push (@variables, &create_ada_name ($name));
	    $type = &{$convert} ($type);
	    $type =~ s/\'Class//;
	    push (@types, "in " . $type);
	  }

	foreach (@variables)
	  {
	    $longest = length ($_) if (length ($_) > $longest);
	  }
	my ($first) = 1;
	foreach (@variables)
	  {
	    push (@output, ";\n$indent") unless ($first);
	    push (@output, $_);
	    push (@output, ' ' x ($longest - length ($_) + 1) . ": ");
	    push (@output, shift @types);
	    $first = 0;
	  }

	push (@output, ")");
      }
    if ($return ne "void")
      {
	push (@output, "\n$indent") if ($arguments[0] !~ /void/);
	push (@output, "return ");
	push (@output, ' ' x ($longest - 1)) if  ($arguments[0] !~ /void/);
	if ($convert == \&convert_c_type) {
	  if (&{$convert} ($return) eq "String") {
	    push (@output, "Interfaces.C.Strings.chars_ptr");
	    $with_list {"with Interfaces.C.Strings"} ++;
	  }
	  else {
	    push (@output, &{$convert} ($return));
	  }
	}
	else {
	  push (@output, &{$convert} ($return));
	}
      }
  }

#########################
## Print the call to the Internal function
## Entries : A string of blank space indicating the position on the current
##               line
##           An array containing the arguments to the function
#########################
sub print_arguments_call
  {
    my ($indent) = shift;
    my (@arguments) = @_;
    my ($first) = 1;

    if ($arguments[0] !~ /void/)
      {
	push (@output, " (");
	$indent .= '  ';
	foreach (@arguments)
	  {
	    s/\s+/ /g;
	    s/,//;
	    s/\);//;
	    my ($type, $name) = /^(.*[ *])(\w+)\s*$/;
	    $type =~ s/\s//g;
	    push (@output, ",\n$indent") unless ($first);
	    $first = 0;
	    if (&convert_c_type ($type) eq "System.Address")
	      {
		push (@output, "Get_Object ("
		      .  &create_ada_name ($name) . ")");
	      }
	    elsif (&convert_c_type ($type) eq "String")
	      {
		push (@output, &create_ada_name ($name)
		     . " & Ascii.NUL");
	      }
	    elsif (&convert_c_type ($type) eq "Gint"
		  && lc ($type) ne "gint")
	      {
		push (@output, &convert_ada_type ($type)
		     . "'Pos (" . &create_ada_name ($name)
		     . ")");
	      }
	    else
	      {
		push (@output, &create_ada_name ($name));
	      }
	  }
	push (@output, ")");
      }
  }

#########################
## Print the declaration for a subprogram
## Entries : C function name
##           return value C type
##           array of arguments
#########################
sub print_declaration
  {
    my ($func_name) = shift;
    my ($return) = shift;
    my (@arguments) = @_;
    my ($string);
    my ($indent);
    my ($adaname) = &ada_func_name ($func_name); 

    return if ($adaname eq "Get_Type");

    if ($adaname =~ /New/)
      {
	&print_new_declaration ($func_name, @arguments);
      }
    else
      {
	$string = ($return eq "void" ? "procedure" : "function");
	$string .=  " $adaname";
	push (@output, "   $string");
	$indent = ' ' x (length ($string) + 3);
	
	&print_arguments ($indent, $return, \&convert_ada_type,
			  3, 0, @arguments);
      }
    push (@output, ";\n");
  }

#########################
## Print the body for a subprogram
## Entries : C function name
##           return value C type
##           array of arguments
#########################
sub print_body
  {
    my ($func_name) = shift;
    my ($return) = shift;
    my (@arguments) = @_;
    my ($first) = 1;
    my ($string);
    my ($indent);
    my ($adaname) = &ada_func_name ($func_name); 

    return if ($adaname eq "Get_Type");

    $string = "-- $adaname --";
    push (@output, "   " . '-' x length ($string) . "\n");
    push (@output, "   " . $string . "\n");
    push (@output, "   " . '-' x length ($string) . "\n\n");

    if ($adaname =~ /New/)
      {
	&print_new_declaration ($func_name, @arguments);
      }
    else
      {
	$string = ($return eq "void" ? "procedure" : "function");
	$string .=  " $adaname";
	push (@output, "   $string");
	$indent = ' ' x (length ($string) + 3);
	&print_arguments ($indent, $return, \&convert_ada_type,
			  3, 0, @arguments);
      }
    push (@output, "\n   is\n");

    $string = "      ";
    $string .= ($return eq "void" ? "procedure Internal" :
	       "function Internal");
    push (@output, $string);
    $indent = ' ' x (length ($string));
    &print_arguments ($indent, $return, \&convert_c_type,
		      6, 0, @arguments);
    push (@output, ";\n");

    push (@output, "      pragma Import (C, Internal, \"$func_name\");\n");
    if ($adaname =~ /New/)
      {
	push (@output, "   begin\n");
	$string = "      Set_Object (Widget, Internal";
	push (@output, $string);
	&print_arguments_call (' ' x length ($string), @arguments);
	push (@output, ");\n");
      }
    else
      {
	if (&convert_ada_type ($return) =~ /\'Class/)
	  {
	    my ($tmp) = &convert_ada_type ($return);
	    $tmp =~ s/\'Class//;
	    push (@output, "      Tmp : $tmp;\n   begin\n");
	    $string = "      Set_Object (Tmp, Internal";
	  }
	elsif (&convert_ada_type ($return) eq "String")
	  {
	    push (@output, "   begin\n");
	    $string = "      return Interfaces.C.Strings.Value (Internal";
	  }
	elsif ($return =~ /^G[dt]k/) {
	  push (@output, "   begin\n");
	  $string = "      return " . &convert_ada_type ($return)
	    . "'Val (Internal";
	}
	elsif ($return ne "void")
	  {
	    push (@output, "   begin\n");
	    $string = "      return Internal";
	  }
	else
	  {
	    push (@output, "   begin\n");
	    $string = "      Internal";
	  }
	push (@output, $string);
	
	&print_arguments_call (' ' x length ($string), @arguments);
	if (&convert_ada_type ($return) =~ /\'Class/) {
	  push (@output, ");\n      return Tmp;\n");
	}
	elsif (&convert_ada_type ($return) eq "String"
	      || $return =~ /^G[dt]k/) {
	  push (@output, ");\n");
	}
	else {
	  push (@output, ";\n");
	}
      }
    push (@output, "   end $adaname;\n\n");
  }


##########################
## Generates a name for a function
## Entry : the C function name
##########################
sub ada_func_name
  {
    my ($c_func_name) = shift;
    my ($type) = lc ($current_package);

    $c_func_name =~ s/^(g[dt]k|ada)_?//;
    $c_func_name =~ s/^$type\_//;

    $type = &create_ada_name ($c_func_name);
    return "Gtk_Select" if ($type eq "Select");
    return "$prefix\_New" if ($type =~ /New/);
    return $type;
  }
    
###########################
## Generates the name for a type
## The current package does not appear in the generated type
## Entry: the C type name
###########################
sub convert_ada_type
  {
    my ($type) = shift;

    if ($type =~ /^gint([^*]*)(\*?)/) {
      if ($2 ne "") {
	return "out Gint$1";
      }
      return "Gint$1";
    } elsif ($type =~ /^int(\*?)/) {
      if ($1 ne "") {
	return "out Integer";
      }
      return "Integer";
    } elsif ($type =~ /^guint([^*]*)(\*?)/) {
      if ($2 ne "") {
	return "out Guint$1";
      }
      return "Guint$1";
    } elsif ($type eq "gboolean") {
      return "Boolean";
    } elsif ($type eq "gfloat") {
      return "Gfloat";
    } elsif ($type =~ /(const)?(g?)char\*/) {
      return "String";
    } elsif ($type eq "Root_Type") {
	return "Root_Type";
    } elsif ($type =~ /GSList\*/) {
      $with_list {"with Glib.GSList"};
      return "GSList";
    } elsif ($type =~ /(G[td]k)([^*]+)\*/) {
      my ($t) = $2;
      my ($prefix) = $1;
      return "Object'Class" if ($t eq "Object");
      if ($t ne "GC") {
	$t =~ s/(.)([A-Z])/$1_$2/g;
      }
      if ("$prefix\_$t" eq "$prefix\_$current_package") {
	return "$prefix\_$t\'Class";
      }
      else {
	$with_list {"with $prefix.$t"} ++;
	return "$prefix.$t.$prefix\_$t\'Class";
      }
    } else {
      if ($type ne "$prefix\_$current_package") {
	$with_list {"with Gtk.Enums; use Gtk.Enums"} ++;
      }
      $type =~ s/([^_])([A-Z])/$1_$2/g;
      return "$type";
    }
  }

###########################
## Generates the name for a type use in a "Internal" function
## The current package does not appear in the generated type
## Entry: the C type name
###########################
sub convert_c_type
  {
    my ($type) = shift;
    
    if ($type =~ /gint([^*]*)(\*?)/) {
      if ($2 ne "") {
	return "out Gint$1";
      }
      return "Gint$1";
    } elsif ($type eq "gboolean") {
      return "Gint";
    } elsif ($type =~ /guint([^*]*)(\*?)/) {
      if ($2 ne "") {
	return "out Guint$1";
      }
      return "Guint$1";
    } elsif ($type =~ /int(\*?)/) {
      if ($1 ne "") {
	return "out Integer";
      }
      return "Integer";
    } elsif ($type eq "gfloat") {
      return "Gfloat";
    } elsif ($type =~ /(const)?g?char\*/) {
      return "String";
    } else {
      ## If it is a pointer, we have an object, otherwise we have an enum
      if ($type =~ /\*/) {
	return "System.Address";
      }
      return "Gint";
    }
  }

###########################
sub print_comment
  {
    my ($func_name) = shift;
    my ($ada) = &ada_func_name ($func_name);
    my ($string) = "";
    
    $string = "   --  mapping: ";
    if ($ada eq "Get_Type")
      {
	$string .= "NOT_IMPLEMENTED";
      }
    else
      {
	$string .= &ada_func_name ($func_name);
      }
    $string .= " $hfile ";
    if ($func_name =~ /^g[td]k/)
      {
	if (length ($string) + length ($func_name) > 79) {
	  $string .= "\\\n   --  mapping:      ";
	}
	$string .= "$func_name\n";
      }
    else
      {
	my ($entity) = $current_package;
	my ($field) = ($func_name =~ /get_(.*)$/);
	$entity =~ s/_//g;
	if (length ($string) + length ("$prefix$entity->$field") > 79) {
	  $string .= "\\\n   --  mapping:      ";
	}
	$string .= "$prefix$entity->$field\n";
      }
    push (@output, $string);
  }
