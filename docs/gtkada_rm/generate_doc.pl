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
		       "gtkclist.h/resync_selection"     => 0,
		       "gtkctree.h/change_focus_row_expansion" => 0,  # is a signal
		       "gtkcheckitem.h/draw_indicator"   => 0,
		       "gtkeditable.h/set_position"      => 0,
		       "gtkeditable.h/set_editable"      => 0,
		       "gtkeditable.h/update_text"       => 0,
		       "gtkeditable.h/get_chars"         => 0,
		       "gtkeditable.h/set_selection"     => 0
		       );
%package_from_type = ("Gtk_Plot"         => "Gtk.Extra.Plot",
		      "Gtk_Check_Item"   => "Gtk.Extra.Check_Item",
		      "Gtk_Plot_Layout"  => "Gtk.Extra.Plot_Layout",
		      "Gtk_Plot_Canvas"  => "Gtk.Extra.Plot_Canvas",
		      "Gtk_Combo_Box"    => "Gtk.Extra.Combo_Box",
		      "Gtk_Sheet"        => "Gtk.Extra.Sheet",
		      "Gtk_Color_Combo"  => "Gtk.Extra.Color_Combo",
		      "Gtk_Border_Combo" => "Gtk.Extra.Border_Combo",
		      "Gtk_Font_Combo"   => "Gtk.Extra.Font_Combo",
		      "Gtk_IEntry"       => "Gtk.Extra.Item_Entry",
		      "Gtk_Entry"        => "Gtk.Gentry",
		      "Gtk_Range"        => "Gtk.Grange");

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
$subsection_bg="#FFF0D0";
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
	&output ("\@node Package_$package_name\n",
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
	my (%types) = &get_types (@content);


	## Prepare the submenu
	
	&output ("\@menu\n");
	&output ("* $package_name Widget Hierarchy::\n") if (@hierarchy);
	&output ("* $package_name Signals::\n")          if (keys %signals);
	&output ("* $package_name Types::\n")            if (%types);
	&output ("* $package_name Subprograms::\n")      if (@subprogs);
	
	if (&get_tag_value ("example", @content)) {
	    &output ("* $package_name Example::\n");
	}
	&output ("\@end menu\n\n");

	## Widget hierarchy
	
	if (@hierarchy) {
	    my ($hierarchy, $hierarchy_short) = ("", "");
	    $hierarchy = sprintf ("\@b{%-30s (\@pxref{Package_%s})\n",
				  "Gtk_Object}", "Gtk.Object");
	    $hierarchy_short = sprintf ("%-30s (Package %s)\n",
					"Gtk_Object", "Gtk.Object");
	    for ($level = 1; $level < @hierarchy; $level ++) {
		$hierarchy .= " " x ($level * 3)
		    . sprintf ("\\___ \@b{%-" . (25 - $level * 3)
			          . "s (\@pxref{Package_%s})\n",
			       $hierarchy[$level] . "}",
			       &package_from_type ($hierarchy[$level]));
		my ($line) = " " x ($level * 3)
		    . sprintf ("\\___ %-" . (25 - $level * 3)
			       . "s (Package %s)\n",
			       $hierarchy[$level],
			       &package_from_type ($hierarchy[$level]));
		if (length ($line) > 75) {
		    my ($length) = " " x ($level * 3);
		    $line =~ s/\(/\n$length        \(/;
		}
		$hierarchy_short .= $line;		
	    }
	    &color_output ($section_bg, $section_fg,
			   "\@node $package_name Widget Hierarchy\n",
			   "\@section Widget Hierarchy\n");
	    &html_output ("<TABLE WIDTH=100%><TR><TD WIDTH=$tab1_width></TD>",
			  "<TD BGCOLOR=$hierarchy_bg>");
	    &output ("\n\@ifnottex\n");
	    &output ("\n\@smallexample\n$hierarchy\n\@end smallexample\n");
	    &output ("\@end ifnottex\n");
	    &output ("\@ifnothtml\n\@iftex\n\@smallexample\n$hierarchy_short\n",
		     "\@end smallexample\n\@end iftex\n\@end ifnothtml\n");
#	    &output ("\@ifinfo\n\@smallexample\n$hierarchy\n",
#		     "\@end smallexample\n\@end ifinfo\n");
#	    &output ("\@iftex\n\@smallexample\n$hierarchy_short",
#		     "\n\@end smallexample\n\@end iftex");
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

	## List of types (sorted)

	if (%types) {
	    &color_output ($section_bg, $section_fg,
			   "\@node $package_name Types\n",
			   "\@section Types\n\n");
#	    &output ("\@itemize \@bullet\n");
	    &html_output ("<TABLE width=100% border=0 ",
			  "CELLSPACING=0>");

	    foreach $type (sort keys %types) {
		&html_output ("<TR>",
			      "<TD WIDTH=$tab1_width></TD>",
			      "<TD BGCOLOR=$subprog_bg valign=top>");
#		&output ("\@item \n");
		&output ("\@smallexample\n\@exdent ",
			 $types{$type}[0],
			 "\n\@end smallexample");
		&html_output ("</TD></TR><TR>",
			      "<TD WIDTH=$tab1_width></TD>\n<TD>");
		&output ("\@noindent\n",
			 $types{$type}[1], "\@*\n",
			 "\@ifhtml\n<BR><BR>\n\@end ifhtml\n");
		&html_output ("</TD></TR>");
	    }
	    &html_output ("</TABLE>");
#	    &output ("\@end itemize\n");
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
#			&output ("\@end itemize\n");
			$has_itemize = 0;
		    }
		    &html_output ("<TR><TD colspan=3 BGCOLOR=$subsection_bg>");
		    &output ("\@subsection $name\n\n");
		    &html_output ("</TD></TR><TR><TD><BR></TD></TR>");
		    $comment =~ s/^\s*//;
		    if ($comment ne "") {
			$comment = &process_list
			    (&clean_comment_marks ($comment, 1));
			&html_output ("<TR><TD colspan=3>");
			&output ($comment, "\n\n");
			&html_output ("<BR></TD></TR>");
		    }
		    next;
		}

		if ($has_itemize == 0) {
#		    &output ("\@itemize \@bullet\n\n");
		    $has_itemize = 1;
		}

		my ($profile) = "";
		if ($return eq ""  && scalar (@params) == 0) {
		    $name .= ";";
		}
		$profile = sprintf ("%-35s",
				    ($return eq "") ? "\@b{procedure} $name"
				                    : "\@b{function} $name");
		if (scalar (@params) > 0) {
		    $profile .= "\n  (";
		    for ($i=0; $i<=$#params; $i++) {
			$profile .= " " x 3     if ($i != 0);
			my ($type) = $params[$i][2];
			my ($default) = "";
			if ($params[$i][2] =~ /:=/
			    && length ($params[$i][2]) > 30) {
			    $type =~ s/\s*:=.*//;
			    $default = $params[$i][2];			    
			    $default =~ s/.*:=/:=/;
			}
			
			$profile .= sprintf ("%-18s : \@b{%-6s} %s",
					     $params[$i][0],
					     $params[$i][1],
					     $type);
			if ($default ne '') {
			    $profile .= "\n" . (' ' x 23) . $default;
			}
			
			$profile .= (($i == $#params) ? ")" : ";\n");
		    }
		}
		if ($return eq "") {
		    $profile .= ";" if (scalar (@params) != 0);
		} else {
		    $profile .= "\n" . " " x 3 if (scalar (@params) > 0);
		    $profile .=  "\@b{return} $return;";
		}
		    
		&html_output ("<TR>",
			      "<TD WIDTH=$tab1_width></TD>",
			      "<TD BGCOLOR=$subprog_bg valign=top WIDTH=$tab23_width>");
		&output ("\@smallexample\n\@exdent $profile\n\@end smallexample");
		
		$comment =~ s/^\s*//;
		$comment = &process_list (&clean_comment_marks ($comment, 1));
		&html_output ("</TD></TR><TR>",
			      "<TD WIDTH=$tab1_width></TD>",
			      "<TD colspan=2 WIDTH=$tab23_width>");
		&output ("\@noindent\n",
			 $comment, "\@*\n",
			 "\@ifhtml\n<BR><BR>\n\@end ifhtml\n");
		&html_output ("</TD></TR>");
	    }
	    if ($has_itemize == 1)  {
#		&output ("\@end itemize\n\n");
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
    } else {  # No <description> tag
#	print "no description tag for $source_file\n";
    }
}

## Creates empty nodes for the packages that are referenced but not
## already created.

$parent{"Gtk_Object"} = "";
foreach $package_name (keys %parent) {
    $package_name = &package_from_type ($package_name);
    unless (defined $output{$package_name}) {

	&output ("\@cindex $package_name\n",
		 "\@node Package_$package_name\n",
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
print MENU "* Package_", join ("::\n* Package_", sort keys %packages), "::\n";
print MENU "* Index::\n";
print MENU "\@end menu\n";
close MENU;

open (OUT, ">$output_file_name");
foreach $package_name (sort
		       { if ($a =~ /Glib/ && $b =~ /Glib/) {
			   return $a cmp $b;
		       } elsif ($a =~ /Glib/) {
			   return -1;
		       } elsif ($b =~ /Glib/) {
			   return 1;
		       } else {
			   if ($a =~ /GtkAda/i && $b =~ /GtkAda/i) {
			       return $a cmp $b;
			   } elsif ($a =~ /GtkAda/i) {
			       return -1;
			   } elsif ($b =~ /GtkAda/i) {
			       return 1;
			   } else {
			       return $a cmp $b;
			   }
		       }} keys %output) {
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
	    }
	}
    }
    if ($p ne "") {
	$parent{$origin} = $p;
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

	    if (open (INCLUDE_FILE, "$src_dir/$file")) {
		push (@strings, "\n--  " . join ("--  ", <INCLUDE_FILE>) . "\n" . $rest);
		close (INCLUDE_FILE);
	    }

	} else {
	    push (@strings, $line);
	}
    }
    
    return @strings;
}


# Return the list of types defined in the Ada file $1.
# This function does not return the type defined in the private part of the
# package.
# It returns a hash-table:
#  return ::= (type_name => (string_to_display, comment), ...)

sub get_types () {
    my (@content) = @_;
    my (%types) = ();
    my ($current) = "";
    my ($description);
    
    while (($line = shift @content)) {

	# Skip the private part
	if ($line =~ /^\s*private/) {
	    return %types;
	}
	
	if ($line =~ /^\s*((sub)?type)\s*(\S+)\s+(.)/) {
	    $current = $3;
#	    print "======$current====\n";
	    $description = "$1 $current is ";

	    # Be sure that we have on the same line the "is" and the following
	    # character or word (open parenthesis or new).
	    if ($line =~ /\sis\s*$/ || $line !~ /\sis/) {
		$line .= shift @content;
	    }

	    # Likewise for an access type, which could be an access to
	    # subprogram
	    if ($line =~ /access\s*$/) {
		$line .= shift @content;
	    }
	    
	    $line =~ s/^\s+/ /g;

	    # For an access to function type
	    if ($line =~ s/access\s+(function|procedure)/access $1/) {
		my ($is_function) = ($1 eq "function");
		$line =~ s/\(/\n    \(/;
		$line =~ /(access (.|\n)*)/;
		$description .= $1;
		my ($length) = "    ";

		# If there is a non-empty argument list, include it.
		if ($line =~ /\(/) {
		    while ($line !~ /\)/) {
			$line = shift @content;
			$line =~ s/^\s+/ /;
			$description .= $length . $line;
		    }
		}

		# Add the 'return' line
		if ($is_function && $line !~ /\Wreturn\W/) {
		    $line = shift @content;
		    $line =~ s/^\s+/ /;
		    $description .= "   $line";
		}
	    }

	    # Else for a record type
	    elsif ($line =~ /\Wrecord\W/) {
		$description .= "record\n";
		while ($line !~ /record;/) {
		    $line = shift @content;
		    chop $line;
		    $line =~ s/^\s+//;
		    if ($line =~ s/^\s*--\s*//) {
			$description .= "      -- $line\n";
		    } else {
			$description .= "    $line\n";
		    }			
		}
	    }
	    
	    # Else if we have an enumeration type, get all the possible values
	    # and the associated comments
	    elsif ($line =~ s/^.*\sis\s+\(\s*(.*)//) {
		# There can be multiple values on the same line, separated
		# by commas.

		my ($length) = ' ' x (length ($description) - 0);
		$description .= "\n    ($1\n";
		$length = "    ";
		while ($description !~ /\)/) {
		    $line = shift @content;

		    # If we have a comment
		    if ($line =~ /^\s*--\s*(.*)$/) {
			$description .= $length . "    -- $1\n";
		    }

		    # Else we have one or more possible values
		    else {
			$line =~ s/\s+/ /g;
			$description .= "$length$line\n";
		    }
		}
	    }

	    # Else for a simple type, skip to the end of its definition
	    else {
		$line =~ /\sis\s+(.*)$/;
		$description .= $1;
		while ($line !~ /;/) {
		    $line = shift @content;
		    $line =~ s/\s+/ /g;
		    $description .= $line;
		}
	    }
	    
#	    print $description;

	    $line = shift @content;

	    # If we have a comment associated with the type, read it.
	    my ($comment) = "";
	    if ($line =~ /^\s*--/) {
		while ($line !~ /^\s*$/) {
		    $line =~ s/^\s*--\s*/ /;
		    $comment .= $line;
		    $line = shift @content;
		}
	    } else {
		unshift (@content, $line);
	    }
#	    print "\n===>\n$comment\n==============\n";
#	    print $description, "\n";
#	    print &highlight_keywords ($description), "\n";

	    # Do not keep the widgets themselves
	    if ($current !~ /_Record/
		&& $description !~ /access all .*_Record/) {
		$types{$current} = [&highlight_keywords ($description),
				    $comment];
	    }
	}
    }
    return %types;
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
	my ($filename) = &file_from_package (&package_from_type($type));
	if (-f $src_dir . "/gtk-" . $filename) {
	    open (FILE, $src_dir . "/gtk-" . $filename);
	} elsif (-f $src_dir . "/gtk-extra-" . $filename) {
	    open (FILE, $src_dir . "/gtk-extra-" . $filename);
	} else {
	    die "file not found for type $type ($filename)\n";
	}
	my ($origin) = &find_type_in_package (<FILE>);
	close (FILE);
	return (&find_hierarchy_array ($parent{$type}), $type);
    }
    return (&find_hierarchy_array ($parent{$type}), $type);
}

# Returns the name of the file that contains a given package
sub file_from_package () {
    my ($package) = shift;
    $package =~ s/G[dt]k\.//;
    $package =~ s/\./-/;
    $package =~ tr/A-Z/a-z/;
    return $package . ".ads";
}

# Returns the name of the Ada package that contains the type $1

sub package_from_type () {
    my ($string) = shift;
    if (defined $package_from_type{$string}) {
	return $package_from_type{$string};
    } else {
	$string =~ s/(G[td]k)_/$1./;
	return $string;
    }
}

# Prepare the string $1 for output (highlight keywords and comments)
sub highlight_keywords () {
    my ($string) = shift;

    # Protect texi special characters
    $string =~ s/([{}])/\@$1/g;

    # Highlight keywords and commands
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
  my (@result) = ();
  my ($saw_package_start) = 0;
  my ($last_was_section) = 0;

  while (@content) {
    $line = shift @content;

    if ($line =~ /^package /) {
	$saw_package_start = 1;
	next;
    }

    if ($saw_package_start
	&& $line =~ /^\s*------*/)
    {
	# Delete the previous section if it was empty
	if ($last_was_section != 0) {
	    pop (@result);
	}

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

    # Else if we have a subprogram or operator definition	
    } elsif ($line =~ /^\s*(procedure|function)\s+([\w\"+\-*\/&]+)\s*(.*)/) {
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
	      = ($profile =~ /(\s*([\w\s,]+)\s*:\s*(in out|in|out|access)?\s*([^;]+);?)/);
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
