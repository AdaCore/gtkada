#! /usr/bin/env perl

use strict;

if ($#ARGV < 0) {
  print "Syntax : generate.pl [switches] file_name [definition-file unit_name]\n";
  print "   -l  : simply list the functions defined in the file (c or Ada)\n";
  print "         The arguments are then   file_name_1  file_name_2]\n";
  print "         If file_name_2 is present, the output will only show the difference\n";
  print "         between the two\n";
  print "         The c file should be the first one\n";
  print "   file_name : name of the C file to parse\n";
  print "   definition_file : file to parse for the struct definition\n";
  print "   unit_name : if present, only the functions including this name will be \n";
  print "               generated\n";
  print " ex/  generate.pl ../include/gdk/gdk.h ../include/gdk/gdktypes.h window\n";
  print " ex/  generate.pl ../include/gtk/gtkframe.h\n";
  exit;
}

my ($list_mode) = 0;
if ($ARGV[0] eq "-l") {
    $list_mode = 1;
    shift @ARGV;
}


my ($file) = $ARGV [0] || die "must give a filename!!\n";
my ($directory);
my ($unit_name) = "";
my ($definition_file) = $file;
my ($has_get_type_subprogram) = 0;
my ($enumerates) = "";   # Extra string to print for enumeration types
my ($enumerates_clauses) = "";  # Representation clauses to add
my ($signals) = ""; # List of all the signals for the widget

($directory, $file) = ($file =~ /^(.*\/)?([^\/]+)\.h$/);
my ($hfile) = $file . ".h";
my ($prefix) = ($file =~ /(g[dt]k|gnome)/);
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
$file =~ s/-//g;  # Handle Gnome naming convention, e.g gnome-dock-band

my (@cfile);

if ($list_mode) {
    $current_package = "progress_bar" if ($current_package eq "progressbar");
    $current_package = "aspect_frame" if ($current_package eq "aspectframe");
    $current_package = "check_button" if ($current_package eq "checkbutton");
    $current_package = "check_menu_item" if ($current_package eq "checkmenuitem");
    $current_package = "drawing_area" if ($current_package eq "drawingarea");
    $current_package = "event_box" if ($current_package eq "eventbox");
    $current_package = "handle_box" if ($current_package eq "handlebox");
    $current_package = "list_item" if ($current_package eq "listitem");
    $current_package = "menu_bar" if ($current_package eq "menubar");
    $current_package = "menu_shell" if ($current_package eq "menushell");
    $current_package = "option_menu" if ($current_package eq "optionmenu");
    $current_package = "radio_menu_item" if ($current_package eq "radio_menu_item");
    $current_package = "scrolled_window" if ($current_package eq "scrolledwindow");
    $current_package = "spin_button" if ($current_package eq "spinbutton");
    $current_package = "tips_query" if ($current_package eq "tipsquery");
    $current_package = "toggle_button" if ($current_package eq "togglebutton");
    $current_package = "tree_item" if ($current_package eq "treeitem");

    my (%list);
    my (%from);

    while (@ARGV) {
	open (FILE, $ARGV [0]);
	@cfile = <FILE>;
	close (FILE);

	if ($ARGV [0] =~ /\.h$/) {
	    my (%functions) = &parse_functions;
	    foreach (keys %functions) {
		my ($tmp) = &ada_func_name ($_);
		if ($tmp eq '_New') {
		    $tmp = "Gtk_New";
		}
		$list{$tmp}++;
		$from{$tmp} = $ARGV[0] . " $_";
	    }
	}
	elsif ($ARGV [0] =~ /\.ad[bs]$/) {
	    my ($last);
	    foreach (@cfile)
	    {
		if (/(procedure|function)\s+([\w_.]+)/
		    &&
		    $2 !~ /Internal/i)
		{
		    $list {$2}++;
		    $from {$2} = $ARGV[0];
		    $last = $2;
		}
		elsif (/pragma\s+Import\s+\(C,[^,]+,\s+\"([^\"]+)\"/)
		{
		    if ($1 =~ /^ada_/) {
			$from {$last} .= "  from misc.c";
		    }
		}
	    }
	}
 	shift @ARGV;
    }

    foreach (sort keys %list) {
	print $_, "  (from ", $from{$_}, ")\n" if ($list{$_} == 1);
    }
    exit 0;
}


open (FILE, $ARGV [0]);
@cfile = <FILE>;
close (FILE);


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
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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
## Parse a list of parameters, and return
##   Arg 1 = string: list of parameters as read in the C file
##   Return= array that contains one element per parameter. See arg_to_ada
##           for printing it to Ada.
######################################

sub parse_param_list {
  my ($args) = shift;

  # Associate pointers (return value) with the type instead of the
  # name. Preserve comments indications (*/)
  $args =~ s/(\w+)\s*\[\]/* $1/g;   # "*argv[]" arguments (see gnome-client.h)
  $args =~ s/\s*\*(([^\/]|$))/* $1/g;

  # Arrays are not fully supported (we would need to create a separate
  # type. Instead, we just make sure that the file can be
  # gnatchop-ed, and leave the responsability to the user for creating
  # the type.
  $args =~ s/(\w+)\s*(\w+)\s*\[(\d+)\]/Array_$1_$3 $2/g;

  # Cleanup initial spaces and remove comments from the arguments list
  $args =~ s/\/\*.*?\*\///g;
  $args =~ s/\s+/ /g;
  $args =~ s/(^|, ?)const /$1/g; # gnome-calculator.h

  # Access-To-Subprograms are not supported, but at least we want to
  # get some code that can be gnatchop-ed (gnome-procbar.h)
  $args =~ s/, (\w+) \(\* *(\w+)\) *\([^\)]*\)/, Function_$1 $2/g;

  return grep ($_ !~ /^\s*$/, split (/, ?/, $args));
}

#######################################
##  Convert a C declaration for one parameter ("int a") to the equivalent
##  Ada string
##    Arg 1 = the string representing the C declaration
##    Arg 2 = parameter number
##    Arg 3 = conversion function to use
##    return= ($name, $type)
#######################################

sub arg_to_ada {
  $_ = shift;
  my ($arg_num) = shift;
  my ($convert) = shift;

  return "" if ($_ eq "void");
  s/\s+/ /g;
  s/,//;
  s/\);//;
  my ($type, $name) = &parse_c_decl ($_, $arg_num);

  $arg_num ++;

  $type = &{$convert} ($type);

  # Handling of widget parameters

  if ($type =~ /\'Class/) {
    $type =~ s/\'Class//;
    if ($type =~ /$prefix\_$current_package/i) {
      $type = "access $type\_Record";
    }
    else {
      $type = "access $type\_Record'Class";
    }

    # Pointers arguments in C map to "out" parameters in Ada.
    # However, pointers to pointers (char**) map to "out" parameters
    # to the access type.
  } elsif ($type =~ /(.*)\*$/) {
    $type = $1;
    if ($type !~ /^Gdk/) {
      $type = "$1_Access" if ($type =~ /(.*)\*$/);
      $type = "out $type";
    }
  }

  return (&create_ada_name ($name), $type);
}

######################################
## Parse the list of signals
##   arg 1 = first line in the Class structure
##   arg 2 = contents of the file (array)
##   return= last line we reached while parsing the structure
######################################

sub parse_signals {
  my ($line) = shift;
  my (@deffile) = @_;
  my ($return, $name, $params);

  $signals = "   -------------\n"
    . "   -- Signals --\n"
    . "   -------------\n\n"
    . "   --  <signals>\n"
    . "   --  The following new signals are defined for this widget:\n"
    . "   --\n";

  while ($deffile[$line] !~ /};/) {
    if ($deffile[$line] =~ /^\s*(\w+(\s*\*)?)\s*\(\*\s*(\w+)\)\s*\(([^\)]+)/)
      {
	($return, $name, $params) = ($1, $3, $4);
	while ($deffile[$line] !~ /\);/) {
	  $line++;
	  $params .= $deffile[$line];
	}

	$params =~ s/\s+/ /g;
	$params =~ s/\);\s*$//;
	$signals .= "   --  - \"$name\"\n   --    ";
	$signals .= ($return eq "void")? "procedure " : "function ";
	$signals .=
	  "Handler (Widget : access $prefix\_$current_package\_Record'Class";
	$params =~ s/^[^,]+//;

	if ($params ne "") {
	  my ($arg_num) = 1;
	  foreach (&parse_param_list ($params)) {
	    my ($var, $type) = &arg_to_ada ($_, $arg_num, \&convert_ada_type);
	    $arg_num++;
	    $signals .= ";\n   --       $var : $type";
	  }
	}

        $signals .= ")";
	if ($return ne "void") {
	  $signals .= "\n   --       "
	    . "return " . &create_ada_name ($return);
	}

	$signals .= ";\n   --\n";
      }
    $line++;
  }

  $signals .= "   --  </signals>\n";

  return $line;
}

######################################
## Parse the enumeration type pointed to by the first parameters,
##   arg 1 = first line in the Class structure
##   arg 2 = contents of the file (array)
##   return: line at which we stopped
######################################

sub parse_enums
  {
    my ($line) = shift;
    my (@deffile) = @_;
    my ($enum, $enum_name);

    $enum = $deffile[$line];
    while ($deffile[$line] !~ /;/) {
      $line++;
      $enum .= $deffile[$line];
      chop $enum;  # so that multi-lines comments are correctly handled
    }

    # Remove comments
    $enum =~ s/\/\*.*?\*\///g;
    $enum =~ s/\s+/ /g;

    ($enum, $enum_name) =
      ($enum =~ /typedef enum {([^\}]*)}\s*([\w_]+)/);
    $enum_name = &create_ada_name ($enum_name);
    $enumerates .= "   type $enum_name is ";
    $enum =~ s/\s//g;

    # If there is a representation clause, use an Integer type instead
    if ($enum =~ /<</) {
      $enumerates .= "mod 2 ** 32;\n";
      foreach (split (/,/, $enum)) {
	my ($value, $num) = lc ($_);
	$value =~ s/G[td]k_|Gnome_//i;
	$value =~ s/^$current_package\_//i;
	($value, $num) = ($value =~ /^([^=]+)=(.*)/);
	$num = "2 ** $1" if ($num =~ /<<(\d*)\)?/);

	$enumerates .= "   " . &create_ada_name ($value)
	  . " : constant $enum_name := $num;\n";
      }
      $enumerates .= "\n";

      # Else create an enumeration to match the C enum
    } else {
      $enumerates .= "(\n";
      my ($first) = 1;
      my ($has_representation) = 0;
      foreach (split (/,/, $enum)) {
	my ($value) = lc ($_);
	$has_representation = 1 if ($value =~ s/=.*//);
	$value =~ s/G[td]k_|Gnome_//i;
	$value =~ s/^$current_package\_//i;
	$enumerates .= ",\n" unless ($first);
	$first = 0;
	$enumerates .= "      " . &create_ada_name ($value);
      }
      $enumerates .= ");\n\n";

      # Print the representation clause if necessary
      if ($has_representation) {
	$enumerates_clauses .= "   for $enum_name use (\n";
	$first = 0;
	foreach (split (/,/, $enum)) {
	  my ($value, $repres) = lc ($_);
	  $value =~ s/=([^,\)]*)//;
	  $repres = $1 || $first;
	  $value =~ s/G[td]k|Gnome_//i;
	  $value =~ s/^$current_package\_//i;
	  $enumerates_clauses .= ",\n" unless ($first == 0);
	  $first = $value + 1;
	  $enumerates_clauses .= "      " . &create_ada_name ($value)
	    . " => $repres";
	}
	$enumerates_clauses .= ");\n\n";
      }
    }
    return $line;
  }

######################################
## Parse the definition of the widget, and check which fields should be
## mapped to Ada
##   arg 1 = first line in the Class structure
##   arg 2 = contents of the file (array)
##   return: line at which we stopped
######################################

sub parse_fields {
  my ($line) = shift;
  my (@deffile) = @_;

  $line++ while ($deffile[$line] !~ /\{/);
  $line++;
  if ($deffile[$line] =~ /\/\*/) ## If we have a comment, skip it
    {
      while ($deffile[++$line] !~ /\*\//) {};
      $line++;
    }

  ## Look for the parent widget.
  ## This is the first non-blank line in the definition of the widget
  ## structure. Note that this will be converted later on to Ada style,
  ## we currently only store the C name

  $line++ while ($deffile[$line] =~ /^\s*$/);
  $parent = (split (/\s+/, $deffile[$line])) [1];

  $line++;

  ## Check all the fields (ie until the end of the structure)

  while ($deffile[$line] !~ /\}/) {
    my ($field);

    # Search for the new field. Ignore empty lines and comments
    while ($field =~ /^\s*$/) {
      $field = $deffile[$line];

      if ($field =~ /\/\*/) {
	while ($deffile[$line] !~ /\*\//) {
	  $line++;
	  $field .= $deffile[$line];
	}
	$field =~ s$/\*.*\*/$$g;
      }
      $line++;
    }

    $field =~ s/\s+/ /g;
    $field =~ s/ $//;
    $field =~ s/^ //;
    $field =~ s/\s\*/\* /g; ## attach the pointer to the type not to the field
    $field =~ s/;//;
    $field =~ s/:\s*\d+//;  ##  <type> <name> : <width>

    # Types must be a single word, or split below will be confused
    $field =~ s/unsigned ((g)?int)/$1/;
    my ($type, $field) = split (/\s+/, $field);

    print STDERR
      "Create a function for the field $field (of type $type) [n]?";
    my ($answer) = scalar (<STDIN>);
    $fields {$field} = $type if ($answer =~ /^y/);
  }
  return $line;
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
    my (@deffile, $line);
    open (FILE, $filename);
    @deffile = <FILE>;
    close (FILE);

    while ($line < $#deffile) {

      # The Class record defines signals
      if ($deffile[$line] =~ /^STRUCT\s+_${file}Class($|\s)/i) {
	$line = &parse_signals ($line, @deffile);
      }

      # Enumeration types should also be visible in the Ada specs
      elsif ($deffile[$line] =~ /^typedef enum/) {
	$line = &parse_enums ($line, @deffile);
      }

      # The widget record defines all the fields
      elsif ($deffile[$line] =~ /^struct\s+_($file)($|\s)/i) {

	# Get the package name from the name of the structure. Given the
	# various casing used in Gtk+/Gnome, there are a few exceptions that
	# we need to handle manually

	$current_package = $1;
	$current_package = "Dentry_Edit" if ($1 eq "GnomeDEntryEdit");
	$current_package = "Ui_Info"     if ($1 eq "GnomeUIInfo");
	$current_package = "Mdi"         if ($1 eq "GnomeMDI");
	$current_package = "Mdi_$1"      if ($1 =~ /GnomeMDI(.+)/);
	$current_package = &create_ada_name ($current_package);

	$line = &parse_fields ($line, @deffile);
      }

      $line ++;
    }
  }

###################################
##  Sorting function used to generate the subprograms in a correct order
###################################

sub func_sort () {
  # "_newv" must be supported as well (gnome-canvas.h)
  return (-1) if ($a =~ /_newv?$/ || $a =~ /_newv?_/);
  return (1)  if ($b =~ /_newv?$/ || $b =~ /_newv?_/);

  return &ada_func_name ($a) cmp &ada_func_name ($b);
}

###################################
##  Generates the package specification file
###################################

sub generate_specifications
  {
    my ($parent_prefix);

    %with_list = ("with Gtk" => 1);
    @output = ();
    $with_list {"with " . &package_name ($parent)} ++;
    foreach (sort func_sort keys %functions)
      {
	&print_declaration ($_, @{$functions{$_}});
      }

    $parent_prefix = "Gtk"   if ($parent =~ /^gtk/i);
    $parent_prefix = "Gdk"   if ($parent =~ /^gdk/i);
    $parent_prefix = "Gnome" if ($parent =~ /^gnome/i);

    my ($parent_string) = &package_name ($parent). ".$parent_prefix\_"
	. &create_ada_name ($parent);
    $parent_string = ($parent eq "Root_Type") ?
	"Root_Type" : "$parent_string\_Record";

    unshift (@output, $enumerates);

    unshift (@output, "   type $prefix\_$current_package is access all "
	     . "$prefix\_$current_package\_Record'Class;\n\n");
    unshift (@output, "   type $prefix\_$current_package\_Record is "
	     . ($abstract ? "abstract " : "")
	     . "new " . $parent_string . " with private;\n");
    unshift (@output, "package $prefix.$current_package is\n\n");

    push (@output, $signals);
    push (@output, "\nprivate\n");
    push (@output, "   type $prefix\_$current_package\_Record is "
	  . ($abstract ? "abstract " : "")
	  . "new ",
	  $parent_string = ($parent eq "Root_Type") ?
	      "Root_Type" : "$parent_string\_Record",
	  " with null record;\n\n");

    push (@output, $enumerates_clauses);

    if ($has_get_type_subprogram) {
      push (@output,
	    "   pragma Import (C, Get_Type, \""
	    . lc ("$prefix\_$current_package\_get_type") . "\");"
	    . "\n");
    }

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

    foreach (sort func_sort keys %functions)
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

    $entity =~ s/-/_/g;

    # Put an underscore before each upper-case letter
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
    $entity =~ s/^(G[dt]k|Gnome)_?//;

    return "The_Type" if ($entity eq "Type");
    return "The_End" if ($entity eq "End");
    return "The_Digits" if ($entity eq "Digits");
    return "GRange" if ($entity eq "Range");     # gtk-range.h
    return "GEntry" if ($entity eq "Entry");     # gnome-entry.h
    return "Accepted" if ($entity eq "Accept");  # gnome-icon-item.h
    return "Modifier" if ($entity eq "Mod");     # gnome-stock.h
    return "Sub_Type" if ($entity eq "Subtype"); # gnome-stock.h

    return $entity;
  }

#######################
## Return a valid package name
## Entries: the C type implemented in the package
#######################
sub package_name
  {
    my ($entity) = shift;

    $entity =~ s/(G[dt]k|Gnome)/$1./;
    $entity =~ s/([a-z])([A-Z])/$1_$2/g;
    return $entity;
  }

#######################
## Look through the C file for every function definition
## This is based on the gtk coding style, and will probably won't work
## correctly with any C file
#######################
sub parse_functions
  {
    my ($func_name, $return);
    my (@arguments);
    my (%functions);

    while ($_ = shift @cfile)
      {
	# If this looks like a subprogram start (<return_type>  <name>)

	if (/^(\w\S+(\s+\*)?)\s*((g[dt]k|gnome)_\S+)/) {
	  chop;

	  # Read the whole subprogram definition at once. This makes it
	  # easier to ignore comments, to handle subprograms defined on
	  # multiple lines, ...

	  while ($_ !~ /\);/) {
	    $_ .= shift @cfile;
	    chop;
	  }

	  /^(\w\S+(\s+\*)?)\s*(\S+)\s*\((.*)\);/;
	  $return = (&parse_param_list ($1)) [0];
	  $func_name = $3;
	  @arguments = &parse_param_list ($4);

	  # We do not want to generate bindings for some functions
	  # We need to filter out "constructv" as well (gnome-canvas.h)
	  $func_name = "" if ($func_name =~ /$unit_name\_constructv?$/);

	  # Get_Type subprograms are handled specially, since they are
	  # implemented with a simple pragma Import.
	  if ($func_name =~ /$unit_name\_get_type/) {
	    $func_name = "";
	    $has_get_type_subprogram = 1;
	  }

	  if (($unit_name eq "" && $func_name ne "")
	      || $func_name =~ /$prefix\_$unit_name/i) {
	    push (@{$functions{$func_name}}, $return, @arguments);
	  }
	}
      }
    return %functions;
  }


#########################
## Print the declaration for a subprogram corresponding to a gtk_..._new
## The second function is used to generate the equivalent Initialize function
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

sub print_initialize_declaration
{
    my ($func_name) = shift;
    my ($in_spec) = shift;
    my (@arguments) = @_;
    my ($string);
    my ($indent) = "";

    $string = "   procedure Initialize";
    push (@output, $string);
    $indent = ' ' x (length ($string));
    &print_arguments ($indent, "void", \&convert_ada_type,
		      3, 2, @arguments);
    if ($in_spec) {
      push (@output,
	    ";\n   --  Internal initialization function.\n",
	    "   --  See the section \"Creating your own widgets\" in "
	    . "the documentation.\n\n");
    }
}

#########################
## Return the type and name of a C parameter/variable declaration
##   Arg 1 = type definition
##   Arg 2 = argument number
##   Result= ($type, $name)
#########################

sub parse_c_decl {
  my ($decl, $arg_num) = (shift, shift);

  my ($type, $name) = ($decl =~ /^(.*[ *])(\w+)\s*$/);
  if ($type eq "") {
    $type = $_;
    $name = "arg$arg_num";
  }
  $type =~ s/\s//g;

  return ($type, $name);
}

#########################
## Print the argument list for a subprogram, and the return statement if any
## Entries : A string of blank space indicating the position on the current
##               line
##           C return type for the function
##           A reference to the function to be used for type conversion
##           The base indentation of the line (3 for Ada fonction, 6 for Internal)
##           1 if we are generating the list for Gtk_New, 2 for Initialize, 0 otherwise
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
    my ($arg_num) = 1;

    if ($#arguments != 0
	|| ($for_gtk_new && $#arguments > 0))   ## More than one argument ?
      {
	$indent = (' ' x $baseindent) . "  ";
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
	if ($for_gtk_new == 1)
	  {
	    push (@variables, "Widget");
	    push (@types, "out $prefix\_$current_package");
	  }
	elsif ($for_gtk_new == 2)
	  {
	    push (@variables, "Widget");
	    push (@types, "access $prefix\_$current_package\_Record'Class");
	  }

	if ($arguments[0] !~ /void/) {
	  foreach (@arguments)
	    {
	      my ($var, $type) = &arg_to_ada ($_, $arg_num, $convert);
	      $arg_num ++;
	      push (@variables, $var);
	      $longest = length ($var) if (length ($var) > $longest);
	      push (@types, $type);
	    }
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
	if ($convert == \&convert_c_type
	    && &{$convert} ($return) eq "String")
	{
	    push (@output, "Interfaces.C.Strings.chars_ptr");
	    $with_list {"with Interfaces.C.Strings"} ++;
	}
	else {
	  my ($t) = &{$convert} ($return);
	  $t =~ s/'Class$//;
	  push (@output, $t);
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
    my ($arg_num) = 1;

    if ($arguments[0] !~ /void/)
      {
	push (@output, " (");
	$indent .= '  ';
	foreach (@arguments)
	  {
	    s/\s+/ /g;
	    s/,//;
	    s/\);//;
	    my ($type, $name) = &parse_c_decl ($_, $arg_num);
	    push (@output, ",\n$indent") unless ($arg_num == 1);
	    $arg_num++;
	    if (&convert_c_type ($type) eq "System.Address")
	      {
		push (@output, "Get_Object ("
		      .  &create_ada_name ($name) . ")");
	      }
	    elsif (&convert_c_type ($type) eq "String")
	      {
		push (@output, &create_ada_name ($name)
		     . " & ASCII.NUL");
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

sub print_arguments_call_for_gtk_new
{
    my (@arguments) = @_;
    my ($arg_num) = 1;

    if ($arguments[0] !~ /void/)
    {
	foreach (@arguments)
	  {
	    s/\s+/ /g;
	    s/,//;
	    s/\);//;
	    my ($type, $name) = &parse_c_decl ($_, $arg_num);
	    $arg_num++;
	    push (@output, ",");
	    push (@output, " " . &create_ada_name ($name));
	  }
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

    if (grep (/\.\.\./, @arguments)) {
      print STDERR "\n\n!!!No code generated for $func_name (variable number",
	" of arguments\n";
      push (@output,
	    "   --  $func_name not bound: variable number of arguments\n\n");

    } elsif ($adaname =~ /New/) {
      &print_new_declaration ($func_name, @arguments);
      push (@output, ";\n\n");
      &print_initialize_declaration ($func_name, 1, @arguments);
      if ($has_get_type_subprogram) {
	push (@output, "   function Get_Type return Gtk.Gtk_Type;\n");
	push (@output, "   --  Return the internal value associated with"
	      . " this widget.\n\n");
      }

    } else {
      $string = ($return eq "void" ? "procedure" : "function");
      $string .=  " $adaname";
      push (@output, "   $string");
      $indent = ' ' x (length ($string) + 3);
      &print_arguments ($indent, $return, \&convert_ada_type,
			3, 0, @arguments);
      push (@output, ";\n\n");
    }
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

    return if (grep (/\.\.\./, @arguments));

    $string = "-- $adaname --";
    push (@output, "   " . '-' x length ($string) . "\n");
    push (@output, "   " . $string . "\n");
    push (@output, "   " . '-' x length ($string) . "\n\n");

    if ($adaname =~ /New/)
      {
	&print_new_declaration ($func_name, @arguments);
	push (@output, "\n   is\n   begin\n");
	push (@output, "      Widget := new $prefix\_$current_package\_Record;\n");
	push (@output, "      Initialize (Widget");
	&print_arguments_call_for_gtk_new (@arguments);
	push (@output, ");\n");
	push (@output, "   end $prefix\_New;\n\n");

 	$string = "-- Initialize --";
 	push (@output, "   " . '-' x length ($string) . "\n");
 	push (@output, "   " . $string . "\n");
 	push (@output, "   " . '-' x length ($string) . "\n\n");
 	&print_initialize_declaration ($func_name, 0, @arguments);
 	$adaname = "Initialize";
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
    if ($adaname eq "Initialize")
      {
	push (@output, "   begin\n");
	$string = "      Set_Object (Widget, Internal";
	push (@output, $string);
	&print_arguments_call (' ' x length ($string), @arguments);
	push (@output, ");\n");
      }
    else
      {
	my ($terminate) = ";\n";

	if (&convert_ada_type ($return) =~ /\'Class/)
	  {
	    my ($tmp) = &convert_ada_type ($return);
	    $tmp =~ s/\'Class//;
	    push (@output, "   begin\n");
	    if ($tmp eq "Gtk.Widget.Gtk_Widget") {
	      $string = "      return Convert (Internal";
	      $terminate = ");\n";
	    } else {
	      $string = "      return $tmp (Convert (Internal";
	      $terminate = "));\n";
	      }
	    $with_list{"with Gtk.Widget; use Gtk.Widget"}++;
	  }
	elsif (&convert_ada_type ($return) eq "String")
	  {
	    push (@output, "   begin\n");
	    $string = "      return Interfaces.C.Strings.Value (Internal";
	    $terminate = ");\n";
	  }
	elsif ($return =~ /^(G[dt]k|Gnome)/) {
	  push (@output, "   begin\n");
	  $string = "      return " . &convert_ada_type ($return)
	    . "'Val (Internal";
	  $terminate = ");\n";
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
	push (@output, $terminate);
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

    $c_func_name =~ s/^(g[dt]k|gnome|ada)_?//;
    $c_func_name =~ s/^$type\_//;

    $type = &create_ada_name ($c_func_name);
    return "Gtk_Select" if ($type eq "Select");
    return "Gtk_Abort" if ($type eq "Abort");
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
      return ($2 ne "") ? "out Gint$1" : "Gint$1";

    } elsif ($type =~ /^guint([^*]*)(\*?)/) {
      return ($2 ne "") ? "out Guint$1" : "Guint$1";

    } elsif ($type =~ /^gdouble([^*]*)(\*?)/) {
      return ($2 ne "") ? "out Gdouble$1" : "Gdouble$1";

    } elsif ($type =~ /^double([^*]*)(\*?)/) {
      return ($2 ne "") ? "out Double$1" : "Double$1";

    } elsif ($type =~ /^int(\*?)/) {
      return ($1 ne "") ? "out Integer" : "Integer";

    } elsif ($type eq "gboolean") {
      return "Boolean";

    } elsif ($type eq "gfloat") {
      return "Gfloat";

    } elsif ($type =~ /(const)?(g?)char\*(\*?)/) {
      return ($3 eq "*") ? "Chars_Ptr_Array" : "String";

    } elsif ($type eq "Root_Type") {
	return "Root_Type";
    } elsif ($type =~ /GSList\*/) {
      $with_list {"with Glib.GSList"} ++;
      return "GSList";
    } elsif ($type =~ /Gdk([^*]+)\*/) {
      $with_list {"with Gdk.$1"} ++;
      return "Gdk.$1.Gdk_$1";
    } elsif ($type =~ /(Gtk|Gnome)([^*]+)\*/) {
      my ($t) = $2;
      my ($prefix) = $1;
      return "Object'Class" if ($t eq "Object");

      if ($t eq "DEntryEdit") {
	$t = "Dentry_Edit";
      } elsif ($t eq "MDI") {
	$t = "MDI";
      } elsif ($t eq "UIInfo") {
	$t = "UI_Info";
      } elsif ($t =~ /MDI(.+)/) {
	$t = "MDI_$1";
      } elsif ($t eq "Entry") {
	$t = "GEntry";
      } elsif ($t ne "GC") {
	$t =~ s/(.)([A-Z])/$1_$2/g;
      }
      if ($t =~ /$current_package/i) {
	return "$prefix\_$t\'Class";
      } else {
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
      return ($2 ne "") ? "out Gint$1" : "Gint$1";

    } elsif ($type eq "gboolean") {
      return "Gint";

    } elsif ($type =~ /guint([^*]*)(\*?)/) {
      return ($2 ne "") ? "out Guint$1" : "Guint$1";

    } elsif ($type =~ /double([^*]*)(\*?)/) {
      return ($2 ne "") ? "out Gdouble$1" : "Gdouble$1";

    } elsif ($type =~ /int(\*?)/) {
      return ($1 ne "") ? "out Integer" : "Integer";

    } elsif ($type eq "gfloat") {
      return "Gfloat";

    } elsif ($type =~ /^(Gdk[^*]*)\*/) {
      return $1;

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
    $string .= &ada_func_name ($func_name);
    $string .= " $hfile ";
    if ($func_name =~ /^(g[dt]k|gnome)/)
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
