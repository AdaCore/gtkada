#! /usr/bin/env perl

## This script generates the GtkAda documentation from the sources
## All the .ads in the src/ subdirectory are searched for relevant
## tags, and the documentation is automatically generated.
## The tags are looked for anywhere in the file.
##
## SETUP NOTES:
##    This script should be run from the $prefix/doc/automatic
##    directory in the GtkAda package. It expects to find the gtk+
##    sources in $prefix/gtk+-1.2.6/ (the version number can vary).
##
## The following tags are known:
##
## <description>...</description>
##     This is the description for a package. It can span multiple
##     lines, and contain any kind of text
##     It no such tag is found, no documentation is generated for
##     the package.
##
## <c_version>...</c_version>
##     Version number of gtk+ that was used to generate this file.
##     ex/ 1.2.6
##     The package should be fully synchronized with the c header
##     when this tag is used.
##     If not such tag is found, the package is assumed to be more
##     or less synchronized with gtk+1.2. Some functions might not
##     have an Ada binding.
##
## <screenshot>...</screenshot>
##     Specifies the name of a file that shows a screenshot of the
##     widget. The name should not include any extension (it will
##     be ".eps" for the latex version of the documentation and
##     ".jpg" for the html version.
##     This tag won't be shown in the info file.
##     The pixmap should be in the directory where you are compiling
##     the documentation.
##
## <example>...</example>
##     Gives an example of use of the widget. The keywords and comments
##     are automatically highlighted. Unless the example is short, we
##     recommended using <include> to include its code from an external
##     compilable source file.
##
## <include>...</include>
##     Includes an external source file. The file is included inline, thus
##     any enclosing tag is taken into account, as in
##      <example><include>file</include></example>
##     Note that comment starts are added at the beginning of each line.
##     You can use relative pathnames (relative to the directory of the file
##     containing this file).
##     Both tags (opening and closing) should be on the same line.
##
## The functions and procedures are automatically associated with
## the comments that follow them, and that stops at the first blank
## line.
## No documentation is generated for the following special subprograms:
##   Initialize, Generate
##

## TO BE DONE:
## Print the signals (from the C file)


$src_dir = "../../src/";
# name of the source directory for GtkAda

$gtk_src_dir = "../..";
# Name of the source directory for gtk+ (base directory for now,
# the exact subdirectory is found below).

$output_file_name  = "generated.texi";
$menu_file_name    = "generated_menu.texi";
$submenu_file_name = "generated_submenu.texi";

@source_files = @ARGV;

opendir (DIR, $gtk_src_dir);
$gtk_src_dir .= "/" . (grep (/gtk\+-/, readdir (DIR)))[0];
closedir (DIR);

local (@Ada_keywords) = ('abort', 'abs', 'accept', 'access', 'all', 'and',
			 'array', 'at', 'begin', 'body', 'case', 'constant',
			 'declare', 'delay', 'delta', 'digits', 'do', 'else',
			 'elsif', 'end', 'entry', 'exception', 'exit', 'for',
			 'function', 'generic', 'goto', 'if', 'in', 'is',
			 'limited', 'loop', 'mod', 'new', 'not', 'null', 'of',
			 'or', 'others', 'out', 'package', 'pragma', 'private',
			 'procedure', 'raise', 'range', 'record', 'rem',
			 'renames', 'return', 'reverse', 'select', 'separate',
			 'subtype', 'task', 'terminate', 'then', 'type',
			 'until', 'use', 'when', 'while', 'with', 'xor');
local (@Ada95_keywords) = ('abstract', 'aliased', 'protected', 'requeue',
			'tagged');
local ($keywords_reg) = join ("|", @Ada95_keywords, @Ada_keywords);

## Contains the parent of each widget
%parent = ();

## The output file (stored here so that we can sort the output)
## It is indexed on the package name
%output = ();

# Prepares the menu

foreach $source_file (@source_files) {

    open (FILE, $source_file);
    @content = &expand_include (<FILE>);
    close (FILE);

    # Only generate documentation if we have a <description> tag
    if (grep (/\<description\>/, @content)) {

	# Get the package name
	my ($package_name) = &get_package_name (@content);
	my ($cfile) = &get_c_file ($package_name);

	push (@{$output{$package_name}},
	      "\@cindex $package_name\n",
	      "\@node Package $package_name\n",
	      "\@section Package $package_name\n",
	      "\n\@noindent\n");

	my ($description) = &clean_comment_marks
	    (&get_tag_value ("description", @content), 0);
	$description =~ s/^\s*//;
	    
	push (@{$output{$package_name}},
	      "\n",
	      "See also \@uref{http://www.gtk.org/docs/$cfile.html,$cfile.html} ",
	      "for more information.\n");

	if (&get_tag_value ("screenshot", @content)) {
	    push (@{$output{$package_name}},
		  "\@iftex\n\@image{", &get_tag_value ("screenshot", @content),
		  ",}\n\@end iftex\n\n");
	}

	my (%signals) = &find_signals ($cfile . ".h");

	## Prepare the submenu
	
	push (@{$output{$package_name}},
	      "\@menu\n* $package_name Widget Hierarchy::\n");
	push (@{$output{$package_name}},
	      "* $package_name Signals::\n")             if (keys %signals);
	push (@{$output{$package_name}},
	      "* $package_name Subprograms::\n");
	
	if (&get_tag_value ("example", @content)) {
	    push (@{$output{$package_name}},
		  "* $package_name Example::\n");
	}
	push (@{$output{$package_name}}, "\@end menu\n\n");

	## Widget hierarchy
	
	push (@{$output{$package_name}},
	      "\@node $package_name Widget Hierarchy\n",
	      "\@subsection Widget Hierarchy\n",
	      "\@multitable \@columnfractions .4 .6\n",
	      "\@item \@b{Gtk_Object}\@tab (\@ref{Package Gtk.Object})\n");
	
	my (@hierarchy) = &find_hierarchy (@content);
	for ($level = 1; $level < @hierarchy; $level ++) {
	    push (@{$output{$package_name}},
		  "\@item ",
		  "\@ " x ($level * 3 - 1), "\\_",
		  "\@b{", $hierarchy[$level], "}\@tab (\@ref{Package ",
		  &package_from_type ($hierarchy[$level]), "})\n");
	}
	
	push (@{$output{$package_name}},
	      "\@end multitable\n\n");

	## List of signals

	if (keys %signals) {
	    push (@{$output{$package_name}},
		  "\@node $package_name Signals\n",
		  "\@subsection Signals\n\n",
		  "\@itemize \@bullet\n\n");
	    
	    foreach $signal (sort keys %signals) {
		push (@{$output{$package_name}},  "\@item $signal\n");
	    }
	    push (@{$output{$package_name}}, "\@end itemize\n\n");
	}

	## List of subprograms (sorted)

	push (@{$output{$package_name}},
	      "\@node $package_name Subprograms\n",
	      "\@subsection Subprograms\n\n",
	      "\@itemize \@bullet\n\n");

	my (@subprogs) = &get_subprograms (@content);

        foreach $subprog (sort {$$a[1] cmp $$b[1]} @subprogs) {
	    my ($name, $return, $comment, @params)
		= ($$subprog[1], $$subprog[0], $$subprog[2], @{$$subprog[3]});

	    next if ($name eq "Generate" || $name eq "Initialize");

	    push (@{$output{$package_name}},
		  "\@need ", (@params + 1), "\n",
		  "\@findex $name (\@i{in} $package_name)\n",
		  "\@item \@b{",

		  ($return eq "")? "procedure $name} "  : "function $name} ",

		  );

	    if (scalar (@params) > 0) {
		push (@{$output{$package_name}}, "(\@*\n");
		my ($i);
		for ($i=0; $i<@params; $i++) {
		    push (@{$output{$package_name}},
			  "\@	 		\@var{",
			  $params[$i][0], "} : ",
			  $params[$i][1], " ",
			  $params[$i][2],

			  ($i == $#params) ? ");@*\n" : ";\@*\n");
		}
	    } else {
		push (@{$output{$package_name}}, "\n");
	    }
	    
	    if ($comments ne "") {
		push (@{$output{$package_name}},
		      "\@ifhtml\n<BR>\n\@end ifhtml\n");
	    }
	    push (@{$output{$package_name}},
		  &clean_comment_marks ($comment, 1), "\n\n",
		  "\@ifhtml\n<BR><BR>\n\@end ifhtml\n");
	}

	push (@{$output{$package_name}},  "\@end itemize\n\n");


	## Examples if any

	if (&get_tag_value ("example", @content)) {
	    push (@{$output{$package_name}},
		  "\@node $package_name Example\n",
		  "\@subsection Example\n\n\@example\n",
		  &highlight_keywords
		  (&clean_comment_marks (&get_tag_value
					 ("example", @content),
					 0)),
		  "\n\@end example\n");
	}
    }
}

## Creates empty nodes for the packages that are referenced but not
## already created.

$parent{"Gtk_Object"} = "";
foreach $package_name (keys %parent) {
    $package_name = &package_from_type ($package_name);
    unless (defined $output{$package_name}) {

	push (@{$output{$package_name}},
	      "\@cindex $package_name\n",
	      "\@node Package $package_name\n",
	      "\@section Package $package_name\n\n");
    }
}


## Outputs the files

my (%packages) = ();

foreach (keys %parent) {
    $packages{&package_from_type ($_)} = "";
}

open (MENU, ">$menu_file_name");
print MENU "* Package ", join ("::\n* Package ", sort keys %packages), "::\n";
close MENU;

open (SUBMENU, ">$submenu_file_name");
print SUBMENU "\@menu\n",
      "* Package ", join ("::\n* Package ", sort keys %packages),
      "::\n\@end menu\n";
close SUBMENU;

open (OUT, ">$output_file_name");
foreach $package_name (sort keys %output) {
    print OUT join ("", @{$output{$package_name}}), "\n";
}
close OUT;




# Returns the name of the widget defined in the package whose contents is
# @1.
# The htable %parent is initialized, as a side effect
sub find_type_in_package () {
    my (@content) = @_;
    my ($line, $origin, $p);

    # Look into the private part only
    while (@content && $line !~ /^private/) {
	$line = shift @content;
    }
    
    # Find the name of the type in the package @content
    
    while (@content) {
	$line = shift @content;
	if ($line =~ /type ([^ \t]+)_Record/) {
	    $origin = $1;

	    # Do not keep the package name
	    $origin =~ s/.*\.([^.]+)/$1/;

	    unless ($parent{$origin}) {
		if ($line =~ /is new ([^\s]+)/) {
		    if ($1 eq "") {
			$line = shift @content;
			$line =~ /\s+([^\s]+)/;
		    }
		    $p = $1;
		} else {
		    $line = shift @content;
		    $line =~ /((is)? new)? ([^\s]+)/;
		    $p = $3;
		}
		$p =~ s/_Record//;
		$p =~ s/.*\.([^.]+)/$1/;
		$parent{$origin} = $p;
	    }
	}
    }
    return $origin;
}

# Expands a given <include>...</include> and replace it with the content of
# the file
# Parameter: array of strings to check and possibly expand
# Returns the new array where tags have been expanded.

sub expand_include () {
    my (@content) = @_;
    my (@strings);
    my ($line);

    foreach $line (@_) {
	if ($line =~ /(.*)\<include\>(.*)\<\/include\>(.*)/) {
	    push (@strings, $1);
	    my ($file, $rest) = ($2, $3);

	    open (INCLUDE_FILE, "$src_dir/$file") ||
		die "Could not open $src_dir/$file";
	    push (@strings, "\n--  " . join ("--  ", <INCLUDE_FILE>) . "\n" . $rest);
	    close (INCLUDE_FILE);

	} else {
	    push (@strings, $line);
	}
    }
    
    return @strings;
}

# Returns the list of signals defined in the C file $1.
# ( signal_name => profile, signal_name => profile)
sub find_signals () {
    my ($cfile) = shift;
    my (%signals);
    my ($gtk) = "gtk";

    if ($cfile =~ /^gdk/) {
	$gtk = "gdk";
    }

    open (FILE, $gtk_src_dir . "/$gtk/" . $cfile);
    my (@content) = <FILE>;
    close (FILE);

    my ($in_struct) = 0;
    my ($line);
    while (@content) {
	$line = shift @content;
	if ($line =~ /^\s*struct\s/) {
	    $in_struct = 1;
	}

	if ($in_struct) {

	    # A pointer to function ?
	    if ($line =~ /\(\*\s*([^\)]+)/) {
		$signals{$1} = "OK";
	    }
	}
	if ($line =~ /\s*\}/) {
	    $in_struct = 0;
	}
    }
    return %signals;
}

# Returns the type hierarchy of the main type defined in the file whose
# content is @1

sub find_hierarchy () {
    my (@content) = @_;
    my ($origin) = &find_type_in_package (@content);
    return &find_hierarchy_array ($origin);
}

sub find_hierarchy_array () {
    my ($type) = shift;

    if ($type =~ /Gtk_Object/) {
	return ($type);
    };

    unless ($parent{$type}) {
	my ($filename) = $type;
	$filename =~ s/Gtk_/Gtk-/;
	$filename =~ tr/A-Z/a-z/;
	$filename .= ".ads";

	open (FILE, $src_dir . "/" . $filename);
	my ($origin) = &find_type_in_package (<FILE>);
	close (FILE);
	return (&find_hierarchy_array ($parent{$type}), $type);
    }
    return (&find_hierarchy_array ($parent{$type}), $type);
}


# Returns the name of the Ada package that contains the type $1

sub package_from_type () {
    my ($string) = shift;
    $string =~ s/Gtk_/Gtk./;
    return $string;
}

# Prepare the string $1 for output (highlight keywords and comments)
sub highlight_keywords () {
    my ($string) = shift;
    $string =~ s/--([^\n]*)/-\@:-\@i{$1}/g;
    $string =~ s/\b($keywords_reg)\b/\@b{$1}/g;

    # Do not highlight keywords in comments !
    while (($string =~ s/\@i(.*)\@b{([^\}]+)}(.*)/\@i$1$2$3/g)){};
    
    return $string;
}

# Returns the name of the C file corresponding to the Ada package $1
sub get_c_file () {
    my ($file) = shift;
    $file =~ tr/A-Z/a-z/;
    $file =~ s/[_.]//g;
    return $file;
}

# Returns the package name from the file whose content is $1
sub get_package_name () {
    my (@content) = @_;

    ($_) = grep (/^package /, @content);
    $_ =~ /^package\s*([^\s]+)/;
    return $1;
}

# Eliminates the comment marks in the string (i.e suppress the -- at
# the beginning of each line in $1)
# When there is an empty line, replaces it with @*
# And the first line is ended with @* if $2 is 1
sub clean_comment_marks () {
    my ($string) = shift;
    my ($first_line) = shift;

    # get rid of comments
    $string =~ s/^[ \t]*-- ?//;
    $string =~ s/\n[ \t]*-- ?/\n/g;

    # If needed, separate the first line from the rest (if there is more
    # than one line)
    if ($first_line) {
	if ($string =~ /\n.*\n/) {
	    $string =~ s/\n/\@*\n/;
	}
    }
    return $string;
}

# Returns the value of the tag $1, in the file whose content is $2
# All the matching tags found in the documentation are concatenated,
# with a blank line added between them.
sub get_tag_value () {
  my ($tag) = shift;
  my (@content) = @_;
  my ($value) = "";
  my ($line, $add);

  while (@content) {
    $line = shift @content;
    if ($line =~ /\<$tag\>/) {
      if ($value ne "") {
	$value .= "\n";
      }

      $line =~ /\<$tag\>(.*)/;
      $value .= $1;

      while ($line !~ /<\/$tag\>/) {
	$line = shift @content;
	$value .= $line;
      }

      $value =~ s/\<\/$tag\>.*//;
    }
  }
  return $value;
}

# Returns a list of all the subprograms defined in the file whose
# content is $1.
# The format of the list is the following:
# ( (subprogram_return_type,  # "" for a procedure
#    subprogram_name,
#    comments
#    (param1_name,
#     param1_access,          # "in", "in out", "access", ""
#     param1_type)
#    (param2_name,
#     param2_access,
#     param2_type),
#    ...
#   )
#   (subprogram_return_type, ....
#
sub get_subprograms () {
  my (@content) = @_;
  my ($line);
  my (@result);

  while (@content) {
    $line = shift @content;

    if ($line =~ /^\s*(procedure|function)\s+(\w+)\s*(.*)/) {
      my ($type)   = $1;
      my ($name)   = $2;
      my ($params) = $3;

      # If the line was over multiple lines
      if ($params !~ /;/) {
	  $params .= shift @content;
      }

      # Get the whole parameter list
      if ($params =~ /\(/) {
	while ($params !~ /\)/) {
	  $params .= shift @content;
	}
      }

      $params =~ s/\s+/ /g;

      my ($profile, $return) = ($params =~ /^\s*\(\s*([^\)]+)\)(.*)/);

      # If we expect a return type, make sure we have it
      if ($type eq "function") {
	while ($return !~ /;/) {
	  $return .= shift @content;
	}
      }

      my ($ret_type) = ($return =~ /return ([^\s;]+)/);

      # Memorizes the comments
      my ($comments) = "";
      $line = shift @content;
      while ($line =~ /^\s*--/) {
	$comments .= $line;
	$line = shift @content;
      }

      # Parses the profile
      my (@param_list) = ();

      while ($profile !~ /^ *$/) {
	  my ($all, $tname, $taccess, $ttype)
	      = ($profile =~ /((\w+)\s*:\s*(in out|in|out|access)?\s*([^;]+);?)/);
	  push (@param_list, [$tname, $taccess, $ttype]);
	  $profile =~ s/$all//;
      }

      push (@result, [ $ret_type, $name, $comments, \@param_list]);
    }
  }
  return @result;
}
