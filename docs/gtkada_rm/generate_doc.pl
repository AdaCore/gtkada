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
##     Multiple such tags are concatenated.
##     Lists are automatically replaced by @itemize items (a list
##     is a set of paragraphs that start with '-'; the list ends
##     at the first empty-line not followed by a paragraph that
##     starts with '-').
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
## <signals>
##    - "signal_name"
##      signal description
##    - "signal_name" ....
## </signals>
##     This tag describes all the new signals defined by a widget.
##
## <doc_ignore>...</doc_ignore>
##     Indicates a section which should not be included in the documentation.
##     All the subprograms inside the section will not be documented. The lines
##     on which these tags are are completly deleted.
##
## The functions and procedures are automatically associated with
## the comments that follow them, and that stops at the first blank
## line.
## No documentation is generated for the following special subprograms:
##   Initialize, Generate
##

$src_dir = "../../src/";
# name of the source directory for GtkAda

$gtk_src_dir = "../..";
# Name of the source directory for gtk+ (base directory for now,
# the exact subdirectory is found below).

$output_file_name  = "generated.texi";
$menu_file_name    = "generated_menu.texi";

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

# List of pointer to functions that are not signals in the C files.
%signals_exceptions = ("gtkcheckbutton.h/draw_indicator" => 0,
		       "gtkmenushell.h/selection_done"   => 0,
		       "gtkcontainer.h/forall"           => 0,
		       "gtkcontainer.h/composite_name"   => 0,
		       "gtkcontainer.h/child_type"       => 0,
		       "gtkcontainer.h/set_child_arg"    => 0,
		       "gtkcontainer.h/get_child_arg"    => 0,
		       "gtkcontainer.h/set_focus_child"  => 0,
		       "gtkobject.h/finalize"            => 0,
		       "gtkobject.h/shutdown"            => 0,
		       "gtkobject.h/set_arg"             => 0,
		       "gtkobject.h/get_arg"             => 0,
		       "gtkwidget.h/hide_all"            => 0,
		       "gtkwidget.h/show_all"            => 0,
		       "gtkwidget.h/debug_msg"           => 0,  # is a signal
		       "gtkclist.h/set_scroll_adjustments" => 0,
		       "gtkclist.h/refresh"              => 0,
		       "gtkclist.h/cell_size_request"    => 0,
		       "gtkclist.h/draw_drag_highlight"  => 0,
		       "gtkclist.h/sort_list"            => 0,
		       "gtkclist.h/remove_row"           => 0,
		       "gtkclist.h/draw_row"             => 0,
		       "gtkclist.h/selection_find"       => 0,
		       "gtkclist.h/clear"                => 0,
		       "gtkclist.h/set_scroll_adjustments" => 0,
		       "gtkclist.h/insert_row"           => 0,
		       "gtkclist.h/fake_unselect_all"    => 0,
		       "gtkclist.h/set_cell_contents"    => 0,
		       "gtkclist.h/resync_selection"     => 0
		       );

## Contains the parent of each widget
%parent = ();

## The output file (stored here so that we can sort the output)
## It is indexed on the package name
%output = ();

## Name of the package that is currently being proposed.
$package_name="";

## Colors used
$chapter_bg="#FFF0D0";
$chapter_fg="#000000";
$section_bg="#FFD0D0";
$section_fg="#000000";
$subprog_bg="#D6E8FF";
$hierarchy_bg="#FFF0D0";
$tab1_width="7%";
$tab2_width="30%";
$tab3_width="63%";
$tab23_width="93%";  # $tab2_width + $tab3_witdh

# Prepares the menu

foreach $source_file (@source_files) {

    open (FILE, $source_file);
    @content = <FILE>;
    close (FILE);

    # Only generate documentation if we have a <description> tag
    if (grep (/\<description\>/, @content)) {

	@content = &delete_ignore(&expand_include (@content));
	print "Generating doc for $source_file\n";
	
	# Get the package name
	$package_name = &get_package_name (@content);
	my ($cfile) = &get_c_file ($package_name);

	# Underscores are incorrectly written in the table of contents
	# because of the font that is used in there. We thus use another
	# font just for them...
	local ($pack) = $package_name;
	$pack =~ s/_/\@code{_}/g;

	&output ("\@page\n",
		 "\@cindex $package_name\n");
	&output ("\@node Package $package_name\n",
		 "\@chapter Package $pack\n");
	&output ("\n\@noindent\n");

	my ($description) = &clean_comment_marks
	    (&get_tag_value ("description", @content), 0);
	$description =~ s/^\s*//;
	$description = &process_list ($description);
	    
	&output ("$description\n");

	if (&get_tag_value ("screenshot", @content)) {
	    &output ("\@iftex\n\@image{",
		     &get_tag_value ("screenshot", @content),
		     ",}\n\@end iftex\n\n");
	}

	my (%signals) = &find_signals ($cfile . ".h", @content);
	my (@hierarchy) = &find_hierarchy (@content);
	my (@subprogs) = &get_subprograms (@content);


	## Prepare the submenu
	
	&output ("\@menu\n");
	&output ("* $package_name Widget Hierarchy::\n") if (@hierarchy);
	&output ("* $package_name Signals::\n")          if (keys %signals);
	&output ("* $package_name Subprograms::\n")      if (@subprogs);
	
	if (&get_tag_value ("example", @content)) {
	    &output ("* $package_name Example::\n");
	}
	&output ("\@end menu\n\n");

	## Widget hierarchy
	
	if (@hierarchy) {
	    &color_output ($section_bg, $section_fg,
			   "\@node $package_name Widget Hierarchy\n",
			   "\@section Widget Hierarchy\n");
	    &html_output ("<TABLE WIDTH=100%><TR><TD WIDTH=$tab1_width></TD>",
			  "<TD BGCOLOR=$hierarchy_bg>");
	    &output ("\@multitable \@columnfractions .4 .6\n",
		     "\@item \@b{Gtk_Object}\@tab (\@ref{Package Gtk.Object})\n");
	    
	    for ($level = 1; $level < @hierarchy; $level ++) {
		&output ("\@item ",
			 "\@ " x ($level * 3 - 1), "\\_",
			 "\@b{", $hierarchy[$level], "}\@tab (\@ref{Package ",
			 &package_from_type ($hierarchy[$level]), "})\n");
	    }
	
	    &output ("\@end multitable\n\n");
	    &html_output ("</TD></TR></TABLE>");
	} else {
	    $parent{$package_name} = "<>";
	}

	## List of signals

	if (keys %signals) {
	    &color_output ($section_bg, $section_fg,
			   "\@node $package_name Signals\n",
			   "\@section Signals\n\n");
	    &output ("\@itemize \@bullet\n\n");
	    
	    foreach $signal (sort keys %signals) {
		&output ("\@item \"\@b{$signal}\"\n\n",
			 $signals{$signal}, "\n");
	    }
	    &output ("\@end itemize\n\n");
	}

	## List of subprograms (sorted)

	if (@subprogs) {
	    my ($has_itemize) = 0;
	    &color_output ($section_bg, $section_fg,
			   "\@node $package_name Subprograms\n",
			   "\@section Subprograms\n\n");

	    &html_output ("<TABLE width=100% border=0 ",
			  "CELLSPACING=0>");
	    foreach $subprog (@subprogs) {
		my ($name, $return, $comment, @params)
		    = ($$subprog[1], $$subprog[0], $$subprog[2],
		       @{$$subprog[3]});
		if ($return eq "--") {
		    if ($has_itemize == 1) {
			&output ("\@end itemize\n");
			$has_itemize = 0;
		    }
		    &html_output ("<TR><TD colspan=3>");
		    &output ("\@subsection $name\n\n");
		    &html_output ("</TD></TR>");
		    $comment =~ s/^\s*//;
		    $comment = &process_list
			(&clean_comment_marks ($comment, 1));
		    &output ($comment, "\n\n");
		    next;
		}

		if ($has_itemize == 0) {
		    &output ("\@itemize \@bullet\n\n");
		    $has_itemize = 1;
		}
		&html_output ("<TR>",
			      "<TD WIDTH=$tab1_width></TD>",
			      "<TD BGCOLOR=$subprog_bg valign=top WIDTH=$tab2_width>");
#		&tex_output ("\\settabs 2 \\columns\n\\+");
		&output ("\@findex $name (\@i{in} $package_name)\n",
			 "\@item \@b{",
			 ($return eq "")? "procedure $name} " : "function $name} ");
		if (scalar (@params) == 0) {
		    &output ("\n");
		}
		&html_output ("</TD><TD BGCOLOR=$subprog_bg valign=top WIDTH=$tab3_width>");
#		&tex_output (" & ");
		
		if (scalar (@params) > 0) {
		    my ($i);
		    &output ("(");
		    &output ("\n\@tex\n\\hfil\\break\n\@end tex\n");
		    &output ("\@ifinfo\n\@*  \n\@end ifinfo\n");
		    for ($i=0; $i<@params; $i++) {
			&output ("\@	 		\@var{",
				 $params[$i][0], "} : ",
				 $params[$i][1], " ",
				 $params[$i][2],
				 ($i == $#params) ? ")" : ";\@*\n");
		    }
		}
		if ($return eq "") {
		    &output (";\@*\n");
		} else {
		    if (scalar (@params) > 0) {
			&output ("\@*\n");
		    }
		    &output ("\@	 		\@b{return} $return;\@*\n");
		}
		&html_output ("</TD></TR>");
#		&tex_output ("&\\cr");
		
		$comment =~ s/^\s*//;
		$comment = &process_list (&clean_comment_marks ($comment, 1));
		&html_output ("<TR>",
			      "<TD WIDTH=$tab1_width></TD>",
			      "<TD colspan=2 WIDTH=$tab23_width>");
		&output ($comment, "\n\n",
			 "\@ifhtml\n<BR><BR>\n\@end ifhtml\n");
		&html_output ("</TD></TR>");
	    }
	    if ($has_itemize == 1)  {
		&output ("\@end itemize\n\n");
	    }
	    &html_output ("</TABLE>");
	}

	## Examples if any

	if (&get_tag_value ("example", @content)) {
	    &color_output ($section_bg, $section_fg,
			   "\@node $package_name Example\n",
			   "\@section Example\n");
	    &output ("\n\@example\n",
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

	&output ("\@cindex $package_name\n",
		 "\@node Package $package_name\n",
		 "\@chapter Package $package_name\n\n");
    }
}


## Outputs the files
## Requires the global variables $output and $package_name

my (%packages) = ();

foreach (keys %parent) {
    $packages{&package_from_type ($_)} = "";
}

open (MENU, ">$menu_file_name");
print MENU "\@menu\n";
print MENU "* Package ", join ("::\n* Package ", sort keys %packages), "::\n";
print MENU "* Index::\n";
print MENU "\@end menu\n";
close MENU;

open (OUT, ">$output_file_name");
foreach $package_name (sort keys %output) {
    print OUT join ("", @{$output{$package_name}}), "\n";
}
close OUT;


# Outputs some lines
sub output () {
    push (@{$output{$package_name}},
	  @_);
}

# Outputs the block, only for html

sub html_output () {
    &output ("\n\@ifhtml\n", @_, "\n\@end ifhtml\n");
}

# Outputs the block, only for tex

sub tex_output () {
    &output ("\n\@tex\n", @_, "\n\@end tex\n");
}

# Print some output with special colors
#   $1 = background color
#   $2 = foreground color
#   $3,.. = text (array of lines)

sub color_output ()
{
    my ($bg) = shift;
    my ($fg) = shift;

    &html_output ("<TABLE WIDTH=\"100%\"><TR>",
		  "<TH BGCOLOR=\"$bg\"><FONT COLOR=\"$fg\">");
    &output (@_);
    &html_output ("</FONT></TH></TR></TABLE>");
}


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

    foreach $line (@content) {
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
# The Ada file is defined in $2
# If the Ada file has a "<signals>" tag, then this is used instead
# ( signal_name => profile, signal_name => profile)
sub find_signals () {
    my ($cfile) = shift;
    my (@content) = @_;
    my (%c_signals);
    my (%signals);

    ## Parses the C file to find all the signals
    my ($gtk) = "gtk";
    
    if ($cfile =~ /^gdk/) {
	$gtk = "gdk";
    }
    
    open (FILE, $gtk_src_dir . "/$gtk/" . $cfile);
    my (@c_content) = <FILE>;
    close (FILE);
    
    my ($in_struct) = 0;
    my ($line);
    foreach $line (@c_content) {
	if ($line =~ /^\s*struct\s/) {
	    $in_struct = 1;
	}
	
	if ($in_struct) {
	    
	    # A pointer to function ?
	    if ($line =~ /\(\*\s*([^\)]+)/) {
		$c_signals{$1} = "";
	    }
	}
	if ($line =~ /\s*\}/) {
	    $in_struct = 0;
	}
    }
    
    my ($ada_signals) = &get_tag_value ("signals", @content);

    # If the tag is found in the Ada file, use it
    if ($ada_signals ne "") {
	my ($signal, $descr);
	my (@lines) = split (/\n/, $ada_signals);
	my ($in_handler) = 0;
	foreach $line (@lines) {
	    $line =~ s/^\s*--\s*//;
	    if ($line =~ /- \"([^\"]+)\"/) {
		$signals{$signal} = $descr if ($signal ne "");
		$signal = $1;
		$descr = "";
		$in_handler = 1;
	    } elsif ($line =~ /^\s*$/) {
		if ($in_handler == 1) {
		    $in_handler = 0;
		    $descr .= "";
		}
		$descr .= "\n";
	    } else {
		if ($in_handler) {
		    if ($line !~ /^procedure|function/) {
			$line = ("\@ " x 27) . $line;
		    }
		    $descr .= $line . "\@*\n";
		} else {
		    $descr .= $line . "\n";
		}
	    }
	}
	$signals{$signal} = $descr if ($signal ne "");

	# Check that all the signals are documented
	foreach (keys %c_signals) {
	    if (! defined $signals{$_}
		&& ! defined $signals_exceptions{$cfile . "/" . $_})
	    {
		
		print "  The signal $_ is not documented in the Ada file!\n";
		$signals{$_} = "";
	    }
	}
	
    } else {
	print "  Signals loaded from the C header file.\n"
	    if (keys %c_signals);
	return %c_signals;
    }

    return %signals;
}

# Returns the type hierarchy of the main type defined in the file whose
# content is @1

sub find_hierarchy () {
    my (@content) = @_;
    my ($origin) = &find_type_in_package (@content);
    if ($origin =~ /^Gtk/) {
	return &find_hierarchy_array ($origin);
    } else {
	return ();
    }
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
    $string =~ s/(G[td]k)_/$1./;
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

# Delete all the sections that should be ignored in the documentation
sub delete_ignore () {
    my (@content) = @_;
    my (@output) = ();
    my ($ignore)=0;

    foreach $line (@content) {
	if ($line =~ /\<doc_ignore\>/) {
	    $ignore=1;
	}
	push (@output, $line) if (! $ignore);
	if ($line =~ /\<\/doc_ignore\>/) {
	    $ignore=0;
	}
    }
    return @output;
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

# Processes the lists, and replaces them with some @itemize instructions.
# A list is recognized when at least one line starts with '-' as the first
# non-white character, and ends after the next empty line.

sub process_list () {
    my ($value) = shift;
    my (@lines) = split (/\n/, $value);
    my ($output);
    my ($in_list) = 0;
    my ($terminate_list) = 0;

    foreach $line (@lines) {
	if ($line =~ /^\s*-\s/) {
	    $output .= "\@itemize \@bullet\n" if (! $in_list);
	    $in_list = 1;
	    $terminate_list = 0;
	    $line =~ s/^\s*-/\@item /;
	} elsif ($in_list && $line =~ /^\s*$/) {
	    $terminate_list = 1;
	} elsif ($terminate_list) {
	    $output .= "\@end itemize\n";
	    $terminate_list = 0;
	    $in_list = 0;
	}
	
	$output .= $line . "\n";
    }
    $output .= "\@end itemize\n" if ($in_list);
	
    return $output;
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
# ( (subprogram_return_type,  # "" for a procedure  "--" for a separator
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
  my ($saw_package_start) = 0;
  my ($last_was_section) = 0;

  while (@content) {
    $line = shift @content;

    if ($line =~ /^package /) {
	$saw_package_start = 1;
	next;
    }

    if ($saw_package_start
	&& $line =~ /^\s*------*/
	&& $last_was_section == 0)
    {

	$line = shift @content;
	$line =~ /-- (.*) --/;
	my ($comments) = "";
	my ($section) = $1;
	$line = shift @content;

	while ($content [0] =~ /^\s*--/) {
	    $comments .= shift @content;
	}
	
	push (@result, ['--', $section, "$comments", ()]);
	$last_was_section = 1;
	
    } elsif ($line =~ /^\s*(procedure|function)\s+(\w+)\s*(.*)/) {
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

      my ($dummy, $profile, $return) = ($params =~ /^\s*(\(\s*([^\)]+)\))?(.*)/);

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
	      = ($profile =~ /(([\w\s,]+)\s*:\s*(in out|in|out|access)?\s*([^;]+);?)/);
	  push (@param_list, [$tname, $taccess, $ttype]);
	  $profile =~ s/$all//;
      }

      # Ignore the special subprogram "Generate" and "Initialize"
      if ($name ne "Generate" && $name ne "Initialize") {
	  push (@result, [ $ret_type, $name, $comments, \@param_list]);
	  $last_was_section = 0;
      }
    }

  }
  if ($last_was_section == 1) {
      pop @result;
  }
  return @result;
}
