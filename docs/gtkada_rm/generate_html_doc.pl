#! /usr/bin/env perl

use warnings;
use strict;

## This script generates the GtkAda documentation from the sources
## All the .ads in the src/ subdirectory are searched for relevant
## tags, and the documentation is automatically generated.
## The tags are looked for anywhere in the file.
##
## SETUP NOTES:
##    This script should be run from the $prefix/doc/gtkada_ug
##    directory in the GtkAda package.
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
## <properties>
##   -- Name: name
##   -- Type: type
##   -- Descr: descr
## </properties>
##     Describes the properties of the widget
##
## <testgtk>file.adb</testgtk>
##     Specifies the name of the source file in testgtk that contains example
##     for the widget
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
##   Generate
##

# The list of files to parse
our @source_files = @ARGV;

# The special keywords to highlight in profiles and examples
our @Ada_keywords = ('abort', 'abs', 'accept', 'access', 'all', 'and',
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
our @Ada95_keywords = ('abstract', 'aliased', 'protected', 'requeue',
                       'tagged');
our $keywords = join ("|", @Ada95_keywords, @Ada_keywords);

## List of special sections in the Ada files
our @xml_sections = ("description", "example", "screenshot", "c_version",
                     "see", "signals", "properties", "child_properties",
                     "style_properties", "testgtk");

## Will contain, for each widget, the name of its parent type. The parent
## doesn't contain a package name.
our %parents;

## Will contain the name of packages for each widget. Indexed on widget name
## The second variable contains the name of html file for each widget.
our %packages;
our %files_from_widget;
our %files_from_package;

## List of all known entities. Key is the entity name, prefixed by package,
## contents is
##   [$file, $a_name]   ("a_name" is the name of the HTML anchor
our (%entities);

## Will contain info about each of the source files (subprograms,...). Indexed
## on file name.
our %files;

#####################
## Name of the HTML output file (suitable for links) for an Ada file
#####################

sub html_from_ada() {
   my ($file) = shift;
   $file =~ s/^.*\/([^\/]+).ads/$1.html/;
   return $file;
}

#####################
## Parse an Ada file, and extra each of the relevant special sections
#####################

our $subprogram_re =
   '\n[ \t]*(?:(?:procedure|function)\s+(\w+|".")\s*(?:\([^)]+\))?[^;]*?;)';
our $empty_comment_re = '(?:[ \t]+--\n)';
our $non_empty_comment_re = '(?:[ \t]+--[^\n]+\n)';
our $non_empty_comment_block_re = '(' . $non_empty_comment_re . '*)';
our $comment_block_re = '((?:[ \t]*--[^\n]*\n)*)';
our $section_and_comment_re =
   "\n[ \t]+--[ \t]+([^\n]+?)[ \t]+--\n[ \t]+--+\n" . $non_empty_comment_block_re;
our $subprogram_and_comment_re =
   "((?:$subprogram_re)+)\n" . $comment_block_re;
our $widget_re =
   '\n\s*type\s+(\w+_Record)\s+is\s+(?:tagged private|new\s+([\w\.]+)\s+with)';
our $type_re = 
   '\s*(type\s+(\w+)\sis(?:[^;]+);)\n' . $comment_block_re;

sub extract_sections() {
   my ($file) = shift;
   my ($html_file);
   my ($tags) = join ("|", @xml_sections);
   my (%tags, @subprograms, $package, @widgets);
   my ($section, $section_pushed, @sections, %types);
   my ($count) = 1;

   open (FILE, $file) || die "File not found: $file";
   my ($contents) = join ("", <FILE>);
   close (FILE);

   # Find package name
   ($package) = ($contents =~ /^package ([\w\.]+) is/m);
    return if (!defined $package);

   $html_file = &html_from_ada($file);

   $files_from_package{$package} = $html_file;
   $entities{$package} = [$html_file, ""];

   # Remove sections that must be ignored
   $contents =~ s/<doc_ignore>.*?<\/doc_ignore>/\n/gs;

   # Find all special tags
   while ($contents =~ /<($tags)>(.*?)<\/\1>/osg) {
      my ($tag, $value) = ($1, $2);
      if (defined $tags{$tag}) {
        $tags{$tag} .= "\n$value";
      } else {
        $tags{$tag} = $value;
      }
   }

   # Remove private part, after finding special tags
   $contents =~ s/\n\s*private\s.*//s;

   # Remove these special tags so that they get ignored when looking for
   # subprograms (think of examples and/or signal descriptions). In fact, this
   # might not be needed since these other subprograms will be inside comments
   # and will therefore not match $subprogram_re.
   #$contents =~ s/<($tags)>.*?<\/\1>//osg;

   # Find widget names
   while ($contents =~ /$widget_re/og) {
       my ($widget, $parent) = ($1, $2);
       if (defined $parent) {
          $parent =~ s/^.*?\.(\w+)$/$1/;
          $parents{$widget} = $parent;
       }
       $entities{"$package.$widget"} = [$html_file, ""];
       my ($no_record) = $widget;
       $no_record =~ s/_Record$//;
       #$entities{"$package.$no_record"} = [$html_file, ""];
       $packages{$widget} = $package;
       $files_from_widget{$widget} = $html_file;
       push (@widgets, $widget);
   }

   # Find types
   while ($contents =~ /$type_re/og) {
       my ($definition, $name, $comment) = ($1, $2, $3);
       $entities{"$package.$name"} = [$html_file, ""];
       $types{$name} = [$definition, $comment];
   }

   # Find subprograms
   $section = "General";
   push (@sections, [$section, ""]);
   $section_pushed = 0;

   while ($contents =~ /(?:$subprogram_and_comment_re)|(?:$section_and_comment_re)/og) {
      if (defined $1) {
         my ($description, $comment) =  ($1, $3);
         push (@subprograms, [$section, $description, $comment]);
         $section_pushed = 0;

         ## Store the subprograms in the list of entities
         while ($description =~ /($subprogram_re)/og) {
            my ($sname) = ($2);
            $entities {"$package.$sname"} = [$html_file, "${sname}_${count}_"];
            $count ++;
         }

      } else {
         pop (@sections) if ($section_pushed);  ## No contents => ignore
         $section = $4;
         push (@sections, [$section, $5]);
         $section_pushed = 1;
      }
   }
   pop (@sections) if ($section_pushed);  ## No contents => ignore

   $files{$file} = [$package, \%tags, \@subprograms, \@widgets, \@sections, \%types];
}

#####################
## Return a processed version of the comment
#####################

sub process_comment() {
   my ($comment) = shift;
   my ($package) = shift;
   my ($params)  = shift;
   my (%params)  = ();
   %params  = %{$params} if (defined $params);
   $comment =~ s/^\s*--(  )?//gm;

   # Empty line => Force a new paragraph
   $comment =~ s/\n\n/\n<p>\n/g;

   # Highlight URLs
   $comment =~ s,(http://\S+),<a href="$1">$1<\/a>,g;

   # Highlight internal cross-refs. This is done by detecting words starting
   # with an upper case that reference a known package or widget type
   $comment =~ s/([A-Z](?:\.?\w+)*)/
                  my ($name) = $1;
                  my ($file, $anchor);
                  if (defined $params{$name}) {
                     "<tt>$name<\/tt>";
                  } else {
                    if (defined $entities{$name}) {
                       ($file, $anchor) = @{$entities{$name}};
                    } elsif (defined $entities{"${name}_Record"}) {
                       ($file, $anchor) = @{$entities{"${name}_Record"}};
                    } elsif (defined $entities{"$package.${name}_Record"}) {
                       ($file, $anchor) = @{$entities{"$package.${name}_Record"}};
                    } elsif (defined $entities{"$package.$name"}) {
                       ($file, $anchor) = @{$entities{"$package.$name"}};
                    }

                    if (defined $file) {
                       if ($anchor ne "") {
                          "<a href='$file#$anchor'>$name<\/a>";
                       } else {
                        "<a href='$file'>$name<\/a>";
                       }
                    } else {
                       $name;
                    }
                 }
               /xeg;

   return $comment;
}

#####################
## Display the profile of a subprogram, including xref
#####################

sub process_profile() {
   my ($profile) = shift;

   # Unindent as much as possible
   $profile =~ s/^[ \t]*\n//mg;
   my ($spaces) = ($profile =~ /^(\s*)/);
   $profile =~ s/^$spaces//gm;

   # Remove empty lines
   $profile =~ s/\s*$//;

   # Create xref for types
   $profile =~ s/(:\s*(?:access|in|out)?\s*)([\w.]+)((?:'Class)?(\s*:=\s*\w+)?[;)])/
                 if (defined $entities{$2}) {
                    "$1<a href='$entities{$2}->[0]'>$2<\/a>$3";
                 } else {
                    "$1$2$3";
                 } 
                /xeg;
   $profile =~ s/(return\s+|is\s+new\s+)([\w.]+)/
                 if (defined $entities{$2}) {
                    "$1<a href='$entities{$2}->[0]'>$2<\/a>";
                 } else {
                    "$1$2";
                 } 
                /xeg;

   return &highlight_syntax ($profile);
}

sub highlight_syntax() {
   my ($profile) = shift;

   # Highlight comments
   $profile =~ s/^([ \t]*--.*)/<i>$1<\/i>/mg;

   # Highlight subprogram name (for subprograms section, not examples)
   $profile =~ s/^(procedure|function)\s+(\w+|".")/$1 <span class='name'>$2<\/span>/gi;

   # Highlight keywords, not in comments
   $profile =~ s/\b($keywords)\b/<b>$1<\/b>/og;
   while (($profile =~ s/<i>(.*)<b>(\w+)<\/b>/<i>$1$2/g)){};
   return $profile;
}

######################
## Parse the signals section
######################

our $non_empty_non_signal_comment_re = '(?:[ \t]+--  [^-][^\n]*\n)';
our $non_empty_comment_non_signal_block_re =
   '(' . $non_empty_non_signal_comment_re . '*)';
our $signal_re = '--[ \t]+-[ \t]*"(\w+)"\n'  # Signal name
   . '[ \t]+--[ \t]+((?:procedure|function) Handler[\s-]+\([^)]+\)[\s-]*'
   . '(?:return [\w.]+)?;)\n'
   . $empty_comment_re . '?' # Optional blank line between profile and comment
   . $non_empty_comment_non_signal_block_re;  # comment
                
sub parse_signals() {
   my ($section) = shift;
   my (%signals);

   while ($section =~ /$signal_re/goi) {
      my ($name, $profile, $comment) = ($1, $2, $3);
      $profile =~ s/^\s+--//mg if (defined $profile);
      $signals{$name} = [$profile, $comment];
   }

   return %signals;
}

######################
## Parse the properties section
######################

our $properties_re = '--[ \t]+(?:- )?Name:[ \t]*(.+)\n'
   . '[ \t]+--[ \t]+(?:- )?Type:[ \t]*(.+)\n'
   . '(?:[ \t]+--[ \t]+(?:- )?Flags:[ \t]*(.+\n))?'
   . '(?:[ \t]+--[ \t]+(?:- )?Descr:[ \t]*(.+\n(?:--[ \t]{4,})*))?'
   . '(?:[ \t]+--[ \t]+(?:- )?See also:[ \t]*(.+)\n)?';

sub parse_properties() {
   my ($section) = shift;
   my (%properties);

   while ($section =~ /$properties_re/goi) {
      my ($name, $type, $descr, $see) = ($1, $2, $4, $5);
      $properties{$name} = [$type, $descr, $see];
   }
   return %properties;
}

######################
## Generate HTML for a package
######################

sub generate_html() {
   my ($filename) = shift;
   my ($output)   = &html_from_ada ($filename);
   my ($package)  = $files{$filename}[0];
   my (%tags)     = %{$files{$filename}[1]};
   my (@subprograms) = @{$files{$filename}[2]};
   my (@widgets)  = @{$files{$filename}[3]};
   my (@sections) = @{$files{$filename}[4]};
   my (%types)    = %{$files{$filename}[5]};
   my ($w);
   my ($parent_package, $parent_file);
   my ($count) = 1;
   my ($has_types) = scalar (keys %types) > 0;

   # Start generating the output

   open (OUTPUT, ">gtkada_rm/$output");

   print OUTPUT "<?xml version='1.0' encoding='utf-8' />\n";
   print OUTPUT '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" ',
                ' "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">', "\n";
   print OUTPUT "<html><head>\n";
   print OUTPUT " <title>GtkAda: $package</title>\n";
   print OUTPUT " <link rel='stylesheet' href='gtkada_rm.css' type='text/css'>\n";
   print OUTPUT " <script src='gtkada_rm.js' type='text/javascript'></script>\n";
   print OUTPUT "</head><body>\n";

   $w = $widgets[0];
   if (defined $w && defined $parents{$w}) {
      $parent_package = $packages{$parents{$w}};
      $parent_file    = $files_from_widget{$parents{$w}};
   }

   ## Output the object name
   print OUTPUT "<div id='objectName'>\n";
   print OUTPUT " <span><a href='index.html'><img src='home.png' alt='Index' title='Index'/></a>";
   if (defined $parent_package) {
      print OUTPUT " <a href='$parent_file'><img src='parent.png' alt='Parent widget' title='Parent widget'/></a>";
   }
   print OUTPUT " <a href='gallery.html'><img src='gallery.png' alt='Gallery' title='Widgets gallery'/></a>\n";
   print OUTPUT "  </span>\n$package\n";
   print OUTPUT "</div> <!--  objectName -->\n\n";

   ## Left side

   print OUTPUT "<div id='leftSide'>\n";

   ## Screenshot
   if (defined $tags{"screenshot"}) {
      my ($screenshot) = $tags{"screenshot"};
      $screenshot .= ".jpg" if (-f "$screenshot.jpg");
      $screenshot .= ".png" if (-f "$screenshot.png");
      print OUTPUT "  <div id='screenshot'>\n";
      print OUTPUT "    <h2>Screenshot</h2>\n";
      print OUTPUT "    <img src='$screenshot' alt='No screeshot'/>\n";
      print OUTPUT "  </div>\n\n";
   }

   ## Class hierarchy
   if (defined $widgets[0]) {
      my (@hierarchy);
      my ($index, $index2) = 0;
      print OUTPUT "  <div id='classHierarchy'>\n";
      print OUTPUT "   <h2>Hierarchy</h2>\n";
      print OUTPUT "    <ul>\n";

      $w = $widgets[0];
      unshift (@hierarchy, $w);
      $w = $parents{$w};
      while (defined $w) {
         unshift (@hierarchy, $w);
         $w = $parents{$w};
      }

      foreach $w (@hierarchy) {
          print OUTPUT "      <li>";

          $index2 = $index - 1;
          while ($index2 >= 0) {
             print OUTPUT "<img src='childtree2.png' alt='  ' />";
             $index2--;
          }

          print OUTPUT "<img src='childtree.png' alt='\\_'/>" if ($index > 0);
          if (!defined $packages{$w} || $w eq $widgets[0]) {
             print OUTPUT "$w</li>\n";
          } else {
             print OUTPUT "<a href='$files_from_widget{$w}'>$packages{$w}.$w</a></li>\n";
          }
          $index ++;
       }

       print OUTPUT "    </ul>\n";
       print OUTPUT "  </div> <!--  classHierarchy -->\n\n";
   }

   ## Navigation
   print OUTPUT "  <div id='navigation'>\n";
   print OUTPUT "   <h2>Navigation</h2>\n";
   print OUTPUT "   <ul>\n";

   print OUTPUT "     <li><a href='#Description' onclick='return switchPage(\"page1\")'>Description</a></li>\n"
      if (defined $tags{"description"});
   print OUTPUT "     <li><a href='#Types' onclick='return switchPage(\"page1\")'>Types</a></li>\n"
      if ($has_types);
   print OUTPUT "     <li><a href='#Subprograms' onclick='return switchPage(\"page1\")'>Subprograms</a>\n";
   print OUTPUT "       <ul>\n";
   foreach (@sections) {
      my ($current, $section) = ($_->[0], $_->[0]);
      $section =~ s/[ \t]/_/g;
      print OUTPUT "        <li><a href='#Subprograms__$section' onclick='return switchPage(\"page1\")'>",
                   $current,
                   "</a></li>\n";

   }
   print OUTPUT "       </ul>\n";
   print OUTPUT "   </li>\n";

   print OUTPUT "     <li><a href='#Signals' onclick='return switchPage(\"page2\")'>Signals</a></li>\n"
      if (defined $tags{"signals"});
   print OUTPUT "     <li><a href='#Properties' onclick='return switchPage(\"page3\")'>Properties</a></li>\n"
      if (defined $tags{"properties"});
   print OUTPUT "     <li><a href='#StyleProperties' onclick='return switchPage(\"page3\")'>Style Properties</a></li>\n"
      if (defined $tags{"style_properties"});
   print OUTPUT "     <li><a href='#ChildProperties' onclick='return switchPage(\"page3\")'>Child Properties</a></li>\n"
      if (defined $tags{"child_properties"});
   print OUTPUT "     <li><a href='#Examples' onclick='return switchPage(\"page4\")'>Examples</a></li>\n"
      if (defined $tags{"example"});
   print OUTPUT "     <li><a href='#Testgtk' onclick='return switchPage(\"page5\")'>Testgtk</a></li>\n"
      if (defined $tags{"testgtk"});

   print OUTPUT "   </ul>\n";
   print OUTPUT "  </div> <!--  navigation -->\n\n";

   ## See also
   if (defined $tags{"see"}) {
      print OUTPUT "  <div id='seeAlso'>\n";
      print OUTPUT "   <h2>See Also</h2>\n";
      print OUTPUT "   <ul>\n";
      foreach $w (split ("\n", $tags{'see'})) {
         my ($file) = $files_from_package{$w};
         print OUTPUT "     <li><a href='$file'>$w</a></li>\n";
      }
      print OUTPUT "   </ul>\n";
      print OUTPUT "  </div>  <!-- seeAlso --> \n\n";
   }

   ## Finish left side
   print OUTPUT "</div>  <!--  leftSide -->\n\n";

   ## Documentation
   print OUTPUT "<div id='documentation'>\n";

   ## Notebook
   print OUTPUT "  <ul id='notebook'>\n";
   print OUTPUT "   <li id='tab_page1' class='current'><a href='' ",
                "onclick='return !switchPage(\"page1\")'>Documentation</a></li>\n";
   print OUTPUT "   <li id='tab_page2'><a href='#Signals' ",
                "onclick='return !switchPage(\"page2\")'>Signals</a></li>\n"
      if (defined $tags{"signals"});
   print OUTPUT "   <li id='tab_page3'><a href='#Properties' ",
                "onclick='return !switchPage(\"page3\")'>Properties</a></li>\n"
      if (defined $tags{"properties"}
          || defined $tags{"style_properties"}
          || defined $tags{"child_properties"});
   print OUTPUT "  <li id='tab_page4'><a href='#Examples' ",
                "onclick='return !switchPage(\"page4\")'>Examples</a></li>\n"
      if (defined $tags{"example"});
   print OUTPUT "  <li id='tab_page5'><a href='#Testgtk' ",
                "onclick='return !switchPage(\"page5\")'>Testgtk</a></li>\n"
      if (defined $tags{"testgtk"});
   print OUTPUT "  </ul>  <!-- notebook --> \n\n";

   ## First notebook page
   print OUTPUT "  <div id='notebook_page1' class='notebookPage'>\n";

   ## Description of package
   if (defined $tags{'description'}) {
      print OUTPUT "  <a name='Description'></a>\n";
      print OUTPUT "  <div class='description'>\n";
      print OUTPUT "   <h2>Description</h2>\n";
      print OUTPUT &process_comment ($tags{'description'}, $package);
      print OUTPUT "  </div> <!-- description -->\n\n";
   }

   ## Page1 => Types
   if ($has_types) {
      print OUTPUT "  <a name='Types'></a>\n";
      print OUTPUT "  <div id='types'>\n";
      print OUTPUT "   <h2>Types</h2>\n";
      print OUTPUT "   <ul>\n";

      foreach (sort keys %types) {
        my ($name, $def, $comment) = ($_, $types{$_}->[0], $types{$_}->[1]);
        print OUTPUT "     <li><a name='${name}_'></a>\n";
        print OUTPUT "         <div class='profile'>",
                     &process_profile($def),
                     "</div>\n";
        print OUTPUT "         <div class='comment'>",
                     &process_comment($comment, $package),
                     "</div></li>\n"; 
      }

      print OUTPUT "   </ul>\n";
      print OUTPUT "  </div> <!-- types -->\n\n";
   }

   ## Page1 => Subprograms
   print OUTPUT "  <a name='Subprograms'> </a>\n";
   print OUTPUT "  <a name='Subprograms__General'> </a>\n";
   print OUTPUT "  <div id='subprograms'>\n";
   print OUTPUT "   <h2>Subprograms</h2>\n";
   print OUTPUT "   <ul>\n";

   my (%names);
   my ($current_section) = "General";
   $count = 1;

   foreach $w (@subprograms) {
      my ($section, $description, $comment) = ($w->[0], $w->[1], $w->[2]);
      if ($section ne $current_section) {
         $current_section = $section;
         $section =~ s/[ \t]/_/g;

         print OUTPUT "  <a name='Subprograms__$section'></a>\n";
         print OUTPUT "  <h3>$current_section</h3>\n";

         foreach (@sections) {
            my ($name, $comment) = ($_->[0], $_->[1]);
            if ($name eq $current_section) {
               print OUTPUT "  <div class='description'>",
                      &process_comment ($comment, $package),
                      "</div>\n\n";
            }
         }
      }

      print OUTPUT "     <li>";
      my (%params) = ();
      while ($description =~ /($subprogram_re)/og) {
         my ($sname, $sprofile) = ($2, $1);

         # Output profile for each subprograms in the group
         print OUTPUT "<a name='${sname}_${count}_'></a>\n";
         print OUTPUT "<div class='profile'>",
                      &process_profile ($sprofile),
                      "</div>\n";

         # Save the name for the index
         $names{"${sname}_${count}_"} = $sname;
         $count++;
      }

      # Highlight parameters
      while ($description =~ /[(;]\s*(?:--)?\s*(\w+)\s*:/og) {
         my ($pname) = $1;
         $params{$pname}++;
      }

      # Output the common comment for all subprograms in the group
      print OUTPUT "<div class='comment'>",
                   &process_comment ($comment, $package, \%params),
                   "</div></li>\n";
   }

   print OUTPUT "    </ul>\n";
   print OUTPUT "   </div> <!--  subprograms -->\n\n";

   ## End of first notebook page
   print OUTPUT "  </div> <!--  notebook_page1 --> \n";

   ## Second notebook page (signals)
   if (defined $tags{'signals'}) {
      print OUTPUT "  <div id='notebook_page2' class='notebookPage'>\n";
      print OUTPUT "    <a name='Signals'></a>\n";
      print OUTPUT "    <div id='signals'>\n";
      print OUTPUT "      <h2>Signals</h2>\n";

      my (%signals) = &parse_signals ($tags{'signals'});
      print OUTPUT "   <ul>\n";
      foreach (sort keys %signals) {
         my ($name, $profile, $comment) = ($_, $signals{$_}->[0], $signals{$_}->[1]);
         print OUTPUT "    <li><div class='name'>$name</div>\n";
         print OUTPUT "        <div class='profile'>",
                      &process_profile ($profile),
                      "</div>\n";
         print OUTPUT "        <div class='comment'>",
                      &process_comment ($comment, $package),
                      "<div></li>\n";
      }
      print OUTPUT "   </ul>\n";
   }

   print OUTPUT "    </div> <!-- signals -->\n";
   print OUTPUT "  </div> <!--  notebook_page2 -->\n\n";

   ## Third notebook page (properties)
   if (defined $tags{'properties'}) {
      print OUTPUT "  <div id='notebook_page3' class='notebookPage'>\n";
      print OUTPUT "    <a name='Properties'></a>\n";
      print OUTPUT "    <div id='properties'>\n";
      print OUTPUT "      <h2>Properties</h2>\n";

      my (%props) = &parse_properties ($tags{'properties'});
      print OUTPUT "      <ul>\n";
      foreach (sort keys %props) {
         my ($name, $type, $descr, $see) = ($_, $props{$_}->[0],
                                            $props{$_}->[1],
                                            $props{$_}->[2],
                                            $props{$_}->[3]);
        print OUTPUT "        <li><div class='name'>$name</div>\n";
        print OUTPUT "            <div class='profile'>$type</div>\n"; 
        print OUTPUT "            <div class='comment'>",
                     (defined $descr ? &process_comment ($descr, $package) : ""),
                     (defined $see ? "<br><b>See:</b> " . &process_comment ($see, $package) : ""),
                     "</div></li>\n";
      }

      print OUTPUT "      </ul>\n";
      print OUTPUT "    </div> <!-- Properties -->\n";
      print OUTPUT "  </div> <!-- notebook_page3 -->\n\n";
   }

   ## Fourth page (example)
   if (defined $tags{'example'}) {
      print OUTPUT "  <div id='notebook_page4' class='notebookPage'>\n";
      print OUTPUT "   <a name='Example'></a>\n";
      print OUTPUT "   <div id='example'>";
      print OUTPUT "<h2>Example</h2>";
      my ($example) = $tags{'example'};
      $example =~ s/^\s*--//mg;
      print OUTPUT &highlight_syntax ($example);
      print OUTPUT "</div> <!-- example -->\n";
      print OUTPUT "  </div> <!-- notebook_page4 -->\n\n";
   }

   ## Fifth page (testgtk)
   if (defined $tags{'testgtk'}) {
      print OUTPUT "  <div id='notebook_page5' class='notebookPage'>\n";
      print OUTPUT "   <a name='Testgtk'></a>\n";
      print OUTPUT "   <div id='testgtk'>";
      print OUTPUT "<h2>Testgtk source code</h2>";
      print OUTPUT "<div class='description'>This code is part of testgtk, a demo application",
                   " packaged with GtkAda. Testgtk demonstrates the various",
                   " widgets of GtkAda</div>";
      open (TESTGTK, "../../testgtk/$tags{'testgtk'}");
      print OUTPUT &highlight_syntax (join ("", <TESTGTK>));
      close (TESTGTK);
      print OUTPUT "</div> <!-- testgtk -->\n";
      print OUTPUT "  </div> <!-- notebook_page4 -->\n\n";
   }

   ## Finish documentation
   print OUTPUT "</div> <!-- documentation -->\n\n";

   ## Start right side
   print OUTPUT "<div id='rightSide'>\n";
   print OUTPUT " <div id='Index'>\n";
   print OUTPUT "  <h2>Alphabetical Index</h2>\n";
   print OUTPUT "  <ul>\n";

  foreach $w (sort keys %names) {
      print OUTPUT "   <li><a href='#${w}' onclick='return switchPage(\"page1\")'>$names{$w}</a></li>\n";
   }

   print OUTPUT "  </ul>\n";
   print OUTPUT " </div> <!-- Index -->\n";
   print OUTPUT "</div> <!-- rightSide -->\n\n";

   print OUTPUT <<EOF
   <script language='javascript'>switchPage('page1');
adjust_height()</script>
 </body>
</html>
EOF
;

   close (OUTPUT);
}

#######################
## Main
#######################

## Parse all source files, to get info on type hierarchy

our ($source);
foreach $source (@source_files) {
   &extract_sections ($source);
}

## Then generate HTML for each source file
foreach $source (sort keys %files) {
   &generate_html ($source);
}
