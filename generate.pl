#!/opt/gnu/bin/perl 

use strict;


my ($file) = shift @ARGV || die "must give a filename!!\n";


open (FILE, $file);
my (@cfile) = <FILE>;
close (FILE);

my (@output) = ();
my (%with_list) = ();

($file) = ($file =~ /\/([^\/]+)\.h$/);
my ($hfile) = $file . ".h";
$file = uc ($file);

### Look for the structure representing the unit in the C file
my ($line) = 0;
for ($line = 0; $line < $#cfile; $line ++)
{
  last if (uc ($cfile[$line]) =~ /^STRUCT _$file/);
}
my ($current_package);
$cfile[$line] =~ /struct _(.*)/;
$current_package = &create_ada_name ($1);

#print "Name of the created package ? [$current_package] :";
#my ($real_name) = scalar (<STDIN>);
#$current_package = $real_name if ($real_name !~ /^$/);


my ($in_comment) = 0;
$line += 2;  ## skip the '{' line
if ($cfile[$line] =~ /\/\*/) ## If we have a comment, skip it
  {
    1 while ($cfile[++$line] !~ /\*\//);
    $line++;
  }

### Look for the parent widget

my ($parent) = (split (/\s+/, $cfile[$line])) [1];
my (%functions) = &parse_functions;
    

&generate_specifications;
&generate_body;

### END ###############


###################################
##  Generates the package specification file
###################################
sub generate_specifications
  {
    %with_list = ();
    @output = ();
    $with_list {"with " . &package_name ($parent)} ++;
    push (@output, "package Gtk.$current_package is\n\n");
    push (@output, "   type Gtk_$current_package is new ".
	  &package_name ($parent). ".Gtk_", &create_ada_name ($parent).
	  " with private;\n\n");
    
    foreach (sort keys %functions)
      {
	&print_declaration ($_, @{$functions{$_}});
      }
    push (@output, "\nprivate\n");
    push (@output, "   type Gtk_$current_package is new ".
	  &package_name ($parent). ".Gtk_", &create_ada_name ($parent).
	  " with null record;\n\n");
    
    foreach (sort keys %functions)
      {
	&print_comment ($_, @{$functions{$_}});
      }
    
    
    push (@output, "end Gtk.$current_package;\n");
    
    print "\n", join (";\n", sort keys %with_list), ";\n"; 
    print "\n", join ("", @output);
  }

###################################
##  Generates the package body file
###################################
sub generate_body
  {
    %with_list = ();
    @output = ();
    push (@output, "package body Gtk.$current_package is\n\n");
    
    foreach (sort keys %functions)
      {
	&print_body ($_, @{$functions{$_}});
      }
    
    push (@output, "end Gtk.$current_package;\n");

    print "\n";
#    if (scalar (keys %with_list) > 0)
#      {
#	print join (";\n", sort keys %with_list), ";\n";
#      }
    print "\n", join ("", @output);
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
    $entity =~ s/^Gtk_?//;
    return $entity;
  }

#######################
## Return a valid package name
## Entries: the C type implemented in the package
#######################
sub package_name
  {
    my ($entity) = shift;
    
    $entity =~ s/(.)([A-Z])/$1.$2/g;
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
	if (/^(\S+)\s+(gtk_\S+)\s+\((.*)/)
	  {
	    $func_name = $2;
	    $return = $1;
	    @arguments = ($3);
	    if ($3 !~ /\);/)
	      {
		while ($_ = shift @cfile)
		  {
		    chop;
		    /\s+(.*)/;
		    push (@arguments, $1);
		    last if ($1 =~ /\);/);
		  }
	      }
	    push (@{$functions{$func_name}},
		  $return, @arguments);
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

    $string = "   function Gtk_New ";
    push (@output, $string);
    $indent = ' ' x (length ($string));
    &print_arguments ($indent, "Gtk_$current_package", \&convert_ada_type,
		      3, @arguments);
  }

#########################
## Print the argument list for a subprogram, and the return statement if any
## Entries : A string of blank space indicating the position on the current
##               line
##           C return type for the function
##           A reference to the function to be used for type conversion
##           The base indentation of the line (3 for Ada fonction, 6 for Internal)
##           An array containing the arguments to the function
#########################
sub print_arguments
  {
    my ($indent) = shift;
    my ($return) = shift;
    my ($convert) = shift;
    my ($baseindent) = shift;
    my (@arguments) = @_;
    my ($longest) = 6;

    if ($arguments[0] !~ /void/)
      {
	if ($#arguments != 0)   ## More than one argument ?
	  {
	    $indent = (' ' x $baseindent) . "   ";
	    push (@output, "\n$indent");
	  }
	push (@output, "(");
	$indent .= ' ';
	my (@variables) = ();
	my (@types) = ();
	foreach (@arguments)
	  {
	    s/\s+/ /g;
	    s/,//;
	    s/\);//;
	    my ($type, $name) = /^(.*[ *])(\w+)\s*$/;
	    $type =~ s/\s//g;
	    push (@variables, &create_ada_name ($name));
	    push (@types, "in " . &{$convert} ($type));
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
	push (@output, "return");
	push (@output, ' ' x ($longest) . &{$convert} ($return));
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
	$string = ($return eq "void" ? "procedure " : "function ");
	$string .=  "$adaname ";
	push (@output, "   $string");
	$indent = ' ' x (length ($string) + 3);
	
	&print_arguments ($indent, $return, \&convert_ada_type,
			  3, @arguments);
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
	$string = ($return eq "void" ? "procedure " : "function ");
	$string .=  "$adaname ";
	push (@output, "   $string");
	$indent = ' ' x (length ($string) + 3);
	&print_arguments ($indent, $return, \&convert_ada_type,
			  3, @arguments);
      }
    push (@output, "\n   is\n");

    $string = "      ";
    $string .= ($return eq "void" ? "procedure Internal " :
	       "function Internal ");
    push (@output, $string);
    $indent = ' ' x (length ($string));
    &print_arguments ($indent, $return, \&convert_c_type,
		      6, @arguments);
    push (@output, ";\n");

    push (@output, "      pragma Import (C, Internal, $func_name);\n");
    if ($adaname =~ /New/)
      {
	push (@output, "      Widget : Gtk_$current_package;\n");
	push (@output, "   begin\n");
	$string = "      Set_Object (Widget, Internal";
	push (@output, $string);
	&print_arguments_call (' ' x length ($string), @arguments);
	push (@output, ");\n      return Widget;\n");
      }
    else
      {
	push (@output, "   begin\n");
	if ($return ne "void")
	  {
	    $string = "      return Internal";
	  }
	else
	  {
	    $string = "      Internal";
	  }
	push (@output, $string);
	
	&print_arguments_call (' ' x length ($string), @arguments);
	push (@output, ";\n");
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

    $c_func_name =~ s/^gtk_?//;
    $c_func_name =~ s/^$type\_//;

    $type = &create_ada_name ($c_func_name);
    return "Gtk_New" if ($type =~ /New/);
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
    
    if ($type eq "gint") {
      return "Gint";
    } elsif ($type eq "guint") {
      return "Guint";
    } elsif ($type eq "gfloat") {
      return "Gfloat";
    } elsif ($type eq "constgchar*") {
      return "String";
    } elsif ($type =~ /(Gtk|Gdk)([^*]+)\*/) {
      my ($t) = $2;
      my ($prefix) = $1;
      return "Object'Class" if ($t eq "Object");
      $t =~ s/(.)([A-Z])/$1_$2/g;
      if ($t ne $current_package)
	{
	  $with_list {"with $prefix.$t"} ++;
	  return "$prefix.$t.$prefix\_$t\'Class";
	}
      else
	{
	  return "$prefix\_$t\'Class";
	}
    } else {
      $type =~ s/([^_])([A-Z])/$1_$2/g;
      return $type;
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
    
    if ($type eq "gint") {
      return "Gint";
    } elsif ($type eq "guint") {
      return "Guint";
    } elsif ($type eq "gfloat") {
      return "Gfloat";
    } elsif ($type =~ /(const)?gchar*/) {
      return "String";
    } else {
      return "System.Address";
    }
  }

###########################
sub print_comment
  {
    my ($func_name) = shift;
    my ($ada) = &ada_func_name ($func_name);

    push (@output, "   --  mapping: ");
    if ($ada eq "Get_Type")
      {
	push (@output, "NOT_IMPLEMENTED");
      }
    else
      {
	push (@output, &ada_func_name ($func_name));
      }
    push (@output," $hfile $func_name\n");
  }
