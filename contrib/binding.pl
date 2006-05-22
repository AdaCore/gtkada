#!/usr/bin/env perl
use warnings;
use strict;

our ($ada_dir)="/home/briot/Ada/GtkAda/src/";
#$c_dir  ="/home/briot/gtk/gtk+-2.9/gtk+-2.9.0/";
our ($c_dir)  ="/home/briot/gtk/gtk+-2.8/gtk+-2.8.17/";

our ($debug) = 0;

## parameters are of the form "gtkbutton", "gtkbutton.h"
## They shouldn't contain directory indication
our (@c_files)=@ARGV;

## Find out the name of the Ada unit containing the binding to a specific
## c file

sub replace_word() {
  my ($file) = shift;
  my ($word) = shift;
  $file =~ s/([-_])($word)(.+)/$1$2_$3/;
  return $file;
}

sub ada_unit_from_c_file() {
  my ($cfile) = shift;
  my ($adafile) = $cfile;
  my (@words);
  $adafile =~ s,.*?/([^/]+?)(\.[ch])?$,$1,;  ## basename, no extension
  $adafile =~ s/^gtk(.+)/gtk-$1/;

  ## Order matters in this array
  @words = ("about", "accel", "action", "aspect", "button", "cell", "check",
            "color", "combo", "drawing", "event", "file", "recent", "chooser",
            "font", "handle", "icon", "input", "item", "list", "option",
            "renderer", "radio", "tearoff", "separator",
            "menu", "system", "size", "status", "toggle", "text",
            "tag", "tree", "view", "model", "printer", "operation", "page", "paper",
            "progress", "scrolled", "message", "action");
  for (@words) {
     $adafile = &replace_word ($adafile, $_);
  }
  $adafile =~ s/-bbox/-button_box/;
  $adafile =~ s/([^-])entry$/$1_entry/;
  $adafile =~ s/-entry$/-gentry/;
  $adafile =~ s/-entry(.+)/-entry_$1/;
  $adafile =~ s/_seldialog/_selection_dialog/;
  $adafile =~ s/_sel$/_selection/;
  $adafile =~ s/([-_])toolbutton/$1tool_button/;
  $adafile =~ s/([-_])toolitem/$1tool_item/;
  $adafile =~ s/view_port/viewport/;
  $adafile =~ s/^gmain$/glib-main/;
  $adafile =~ s/-tipsquery/-tips_query/;
  $adafile =~ s/-spinbutton/-spin_button/;
  return $adafile;
}

## Find out all C functions bound by a given Ada unit (.ads and .adb files).
## Returns a hash-table indexed by C functions, containing the name of the Ada
## subprogram
our $import_re      = 'pragma\s+Import\s+\(C\s*,\s*(\w+)\s*,\s*"(\w+)"\s*\)';
our $obsolescent_re = '(pragma\s+Obsolescent).*?(?:--  (\w+))?';
our $not_bound_re   = '--  No binding: (\w+)';
our $ada_prop_re    = '\b(\w+)_Property\s+:\s+constant ';
our $ada_signal_re  = '\bSignal_(\w+)\s+:\s+constant String := "([^"]+)"';

sub ada_bindings_in_unit() {
  my ($unit) = shift;
  my ($contents);
  my (%binding, %obsolescent, %properties, %signals);
  my ($whole_obsolesent) = 0;
  my ($subprogram_seen) = 0;

  if (-f "$ada_dir/$unit.ads") {
     open (FILE, "$ada_dir/$unit.ads");
     $contents = join ("", <FILE>);
     close (FILE);
     print STDERR "Ada Unit $unit\n" if ($debug);
  } else {
     print STDERR "Ada unit doesn't exist yet: $unit.ads\n";
     return (0, %binding, %obsolescent);
  }

  if (-f "$ada_dir/$unit.adb") {
     open (FILE, "$ada_dir/$unit.adb");
     $contents .= join ("", <FILE>);
     close (FILE);
  }

  while ($contents =~ /($import_re)|($obsolescent_re)|($not_bound_re)|($ada_prop_re)|($ada_signal_re)/iog) {
     if (defined $12) {
        $signals{$12} = $13;  ## Indexed on Ada name, value is C name
     } elsif (defined $10) {
        $properties{$10} ++;
     } elsif (defined $8) {
        $binding{$8} = "No binding";
     } elsif (defined $5) {
        if (!$subprogram_seen) {
           $whole_obsolesent = 1;
        } else {
           $obsolescent{$6} = 1;
        }
     } else {
        $binding{$3} = $2;
        $subprogram_seen = 1;

        ## If the whole package is obsolete, this subprogram in particular is
        ## also obsolete. We do this so that we do not have to repeat the
        ## pragma Obsolescent for every entity.
        $obsolescent{&ada_entity_from_c ($unit, $3)} = 1
          if ($whole_obsolesent);
     }
  }

  return (1, \%binding, \%obsolescent, \%properties, \%signals);
}

## Return all the properties defined for the current widget, as well as the
## signals

our $property_re = 'g_object_class_install_property\s*\(\s*\w+\s*,\s*\w+\s*,\s*g_param_spec_(\w+)\s*\("([^"]+)"';
our $property_descr_re = '\s*,\s*P_\("[^"]+"\),\s*P_\("([^"]+)"';
our $c_signal_re = 'g(tk)?_signal_new\s*\("([^"]+)"';
our $c_doc_re = '/\*\*\s*\* (\w+):\s*(.*?)\*/';

sub properties_in_c_file() {
   my ($fullname) = shift;
   my (%properties, %signals, %docs, $cname);
   my ($func, $descr, $name, $type, $sname);

   $fullname =~ s/\.h$/.c/g;

   my ($content) = &get_c_file_content ($fullname);

   while ($content =~ /($property_re$property_descr_re)|($c_signal_re)|($c_doc_re)/ogs) {
      if (defined $9) {
         ($func, $descr) = ($9, $10);
         $descr =~ s/^\s*\* */   --  /gm;
         $docs {$func} = $descr;

      } elsif (defined $2) {
         ($type, $name, $descr) = ($2, $3, $4);
         $cname = $name;
         $name =~ s/-/_/g;
         $name = &capitalize ($name);
         $properties{$name} = [$type, $descr, $cname];
      } else {
         ($sname) = $7;
         $cname = $sname;
         $sname =~ s/-/_/g;
         $sname = &capitalize ($sname);
         $signals {$sname} = $cname;
      }
   }
   return (\%properties, \%signals, \%docs);
}

## Output the descr
sub output_properties() {
   my (%properties) = @_;
   my ($prop, $type, $descr, $cname);
   my (@list) = sort keys %properties;

   if ($#list >= 0) {
      print "   ----------------\n";
      print "   -- Properties --\n";
      print "   ----------------\n";
      print "\n";
      print "   --  <properties>\n";

      foreach $prop (@list) {
         ($type, $descr, $cname) = @{$properties{$prop}};
         print "   --  Name:  " . $prop . "_Property\n";
         print "   --  Type:  " . &capitalize ($type) . "\n";
         print "   --  Descr: $descr\n";
         print "   --\n";
      }

      print "\n";
      print "   --  </properties>\n";

      print "\n";
      foreach $prop (@list) {
         ($type, $descr, $cname) = @{$properties{$prop}};
         print "   ${prop}_Property : constant Glib.Properties.Property_",
               &capitalize ($type), ";\n";
      }

      print "\n";
      foreach $prop (@list) {
         ($type, $descr, $cname) = @{$properties{$prop}};
         print "   ${prop}_Property : constant Glib.Properties.Property_",
               &capitalize ($type), " :=\n",
               "     Glib.Properties.Build (\"$cname\");\n";
      }
   }
}

## Output signals
sub output_signals() {
   my (%signals) = @_;
   my (@list) = sort keys %signals;
   my ($name);

   if ($#list >= 0) {
      print "\n";
      print "   -------------\n";
      print "   -- Signals --\n";
      print "   -------------\n";
      print "\n";
      print "   --  <signals>\n";
      print "   --  </signals>\n";

      foreach $name (@list) {
         print "   Signal_$name : constant String := \"$signals{$name}\";\n";
      }
      print "\n";
   }
}

## Read the content of a C file. As a special case, it might include the
## contents of other C files, for instance gtkseparator.c also includes
## gtkvseparator.c and gtkhseparator.c

sub get_c_file_content () {
  my ($fullname) = shift;
  my ($contents);

  # print "--  C file: $fullname\n";
  open (FILE, $fullname);
  $contents = join ("", <FILE>);
  close (FILE);

  $fullname =~ s,/gtk([^/]+)$,/gtkv$1,;
  if (-f $fullname) {
     # print "--  C file: $fullname\n";
     open (FILE, $fullname);
     $contents .= join ("", <FILE>);
     close (FILE);
  }

  $fullname =~ s,/gtkv([^/]+)$,/gtkh$1,;
  if (-f $fullname) {
     # print "--  C file: $fullname\n";
     open (FILE, $fullname);
     $contents .= join ("", <FILE>);
     close (FILE);
  }
  return $contents;
}

## Find out all C functions defined in a C file.
## Return a hash table indexed on the functions
our $c_function_re = '\b(\w+(\s*\*|\s))\s*(\w+)\s*\(([^)]*\))(\s*G_GNUC_CONST)?;';
sub functions_from_c_file() {
  my ($fullname) = shift;
  my (%funcs, $contents);
  my ($deprecated) = 0;

  $contents = &get_c_file_content ($fullname);

  while ($contents =~ /(ifndef|endif).*GTK_DISABLE_DEPRECATED|$c_function_re/og) {
     if (defined $1 && $1 eq "ifndef") {
        $deprecated = 1;
     } elsif (defined $1 && $1 eq "endif") {
        $deprecated = 0;
     } else {
        my ($returns, $args) = ($2, $5);
        ## Ignore internal gtk+ functions:
        if (substr($4,0,1) ne '_') {
           $funcs{$4} = [$args, $returns, $deprecated];
        }
    }
  }

  return %funcs;
}

## Capitalize a string (every letter after _)
sub capitalize () {
  my ($str) = shift;
  $str =~ s/^(.)/\u$1/;
  $str =~ s/_(.)/_\u$1/g;
  return $str;
}

## Return the Ada widget from a C widget.
## This doesn't return the final _Record
sub c_widget_to_ada () {
  my ($c) = shift;
  $c =~ s/\s//g;
  $c =~ s/([^_])([A-Z])/$1_$2/g;  ## Split on upper cases
  $c =~ s/\*$//;                  ## Ignore pointers for now
  return $c;
}

## Return the Ada type to use for a given C type
sub c_to_ada() {
   my ($c_type) = shift;
   my ($param_index) = shift;  ## -1 for return type
   $c_type =~ s/([^_])([A-Z])/$1_$2/g;  ## Split on upper cases

   return "Boolean"            if ($c_type eq "gboolean");
#   return "Gtk_Relief_Style"   if ($c_type eq "GtkReliefStyle");
#   return "Gtk_Orientation"    if ($c_type eq "GtkOrientation");
#   return "Gtk_Toolbar_Style"  if ($c_type eq "GtkToolbarStyle");
#   return "Gtk_Icon_Size"      if ($c_type eq "GtkIconSize");
   return "Gdk_Event"          if ($c_type eq "Gdk_Event*");
   return "Gfloat"             if ($c_type eq "gfloat");
   return "String"             if ($c_type eq "gchar*");
   return "out Gfloat"         if ($c_type eq "gfloat*");

   if ($c_type =~ /Gtk(.+)\*/ || $c_type =~ /PangoLayout/) {
      if ($param_index == -1) {
         return &c_widget_to_ada ($c_type);
      } elsif ($param_index == 0) {
         return "access " . &c_widget_to_ada ($c_type) . "_Record";
      } else {
         return "access " . &c_widget_to_ada ($c_type) . "_Record'Class";
      }
   }
   return &capitalize ($c_type);
}

## Same as c_to_ada, but return the type to use in the function
## that directly imports the C function
sub c_to_low_ada() {
   my ($c_type) = shift;
   my ($param_index) = shift; ## -1 for return type
   return "Gboolean"       if ($c_type eq "gboolean");
   $c_type =~ s/([^_])([A-Z])/$1_$2/g;  ## Split on upper cases
#   return "Gtk_Relief_Style" if ($c_type eq "GtkReliefStyle");
#   return "Gtk_Orientation"  if ($c_type eq "GtkOrientation");
#   return "Gtk_Toolbar_Style"  if ($c_type eq "GtkToolbarStyle");
#   return "Gtk_Icon_Size"  if ($c_type eq "GtkIconSize");
   return "Gfloat"         if ($c_type eq "gfloat");
   return "Gdk_Event"      if ($c_type eq "Gdk_Event*");
   return "out Gfloat"     if ($c_type eq "gfloat*");
   return "String"         if ($c_type eq "gchar*" && $param_index >= 0);
   return "Interfaces.C.Strings.chars_ptr"
                           if ($c_type eq "gchar*" && $param_index == -1);
   return "System.Address" if ($c_type =~ /Gtk(.+)\*/ || $c_type =~ /PangoLayout/);
   return &capitalize ($c_type);
}

## Return the code to pass or convert back an Ada type to a
## C type
sub c_to_call_ada() {
   my ($name) = shift;
   my ($type) = shift;
   return "Boolean'Pos ($name)" if ($type eq "gboolean");
   return "$name & ASCII.NUL"   if ($type eq "gchar*");
   if ($type =~ /Gtk(.+)\*/) {
      return "Get_Object ($name)";
   }
   return $name;
}

## Return the length of the longuest name in the array
sub longuest() {
   my ($longuest) = 0;
   foreach (@_) {
      $longuest=length($_) if (length ($_) > $longuest);
   }
   return $longuest;
}

## Output the list of parameters for the function
sub output_params() {
   my ($convert) = shift;
   my ($indent) = shift;
   my ($rargs) = shift;
   my ($rarg_types) = shift;
   my ($returns) = shift;
   my ($subprogram_name) = shift;
   my (@args) = @$rargs;
   my (@arg_types) = @$rarg_types;
   my ($longuest) = &longuest (@args);

   if ($#args >= 0) {
      print $indent, "  (";
      my ($index) = 0;
      while ($index <= $#args) {
         my ($n) = &capitalize ($args[$index]);
         print $indent, "   " if ($index != 0);
         print $n;
         print ' ' x ($longuest - length ($n)), " : ";
         if ($index == 0 && $subprogram_name =~ /^Gtk_New/) {
            print "out ", &$convert ($arg_types[$index], -1);
         } elsif ($index == 0 && $subprogram_name =~ /^Initialize/) {
            print &$convert ($arg_types[$index], $index), "'Class";
         } else {
            print &$convert ($arg_types[$index], $index);
         }
         print "", (($index != $#args) ? ";\n" : ")");
         $index++;
      }
      print "\n" if ($returns ne "void");
   }
   if ($returns ne "void") {
      print "$indent   return ", &$convert ($returns, -1);
   }
}

## Name of the Ada entity, given its C name
sub ada_entity_from_c() {
   my ($ada_pkg) = shift;
   my ($func) = shift;
   my ($name) = $func;

   $ada_pkg =~ s/-/_/g;
   $name =~ s/${ada_pkg}_//i;

   if ($ada_pkg eq "gtk_main") {
      $name =~ s/^gtk_//;
   } elsif ($ada_pkg eq "glib_main") {
      $name =~ s/^g_//;
   } elsif ($ada_pkg eq "gtk_gentry") {
      $name =~ s/^gtk_entry_//;
   }

   return &capitalize ($name);
}

## Output a single subprogram

sub output_subprogram() {
   my ($name) = shift;
   my ($func) = shift;
   my ($args) = shift;  # reference to array
   my ($arg_types) = shift; # reference to array
   my ($returns) = shift;
   my ($specs_only) = shift;
   my ($deprecated) = shift;
   my ($c_docs) = shift;
   my (@args) = @$args;
   my (@arg_types) = @$arg_types;
   my ($longuest_param) = 0;
   my ($index);
   my ($is_gobject);

   if (!$specs_only && $name eq "Get_Type") {
      ## Nothing to do, this is just a pragma Import
      return;
   }

   if (!$specs_only) {
      # Subprogram box
      print "\n   ", '-' x (length($name) + 6), "\n";
      print "   -- $name --\n";
      print "   ", '-' x (length($name) + 6), "\n\n";
   }

   ## Prototype of Ada subprogram
   print (($returns eq "void") ? "   procedure " : "   function ");
   print $name, "\n";
   &output_params (\&c_to_ada, "   ", \@args, \@arg_types, $returns, $name);

   if ($specs_only) {
     print ";\n";
     if ($deprecated) {
        print "   pragma Obsolescent;\n";
     }
     if ($name eq "Get_Type") {
        print "   pragma Import (C, $name, \"$func\");\n";
     }

     if (defined $c_docs->{$func}) {
        print $c_docs->{$func}, "\n";
     }
     return;
   }

   print "\n";
   print "   is\n";

   if ($name =~ /^Gtk_New(.*)/) {
      print "   begin\n";
      print "      Widget := new ", &c_to_ada ($arg_types[0], -1), "_Record;\n";
      print "      Initialize$1\n         ";
      $index = 0;
      while ($index <= $#args) {
         print ",\n         " if ($index > 0);
         print "", (($index == 0) ? "(" : " ");
         print &capitalize ($args[$index]);
         $index++;
      }
      print ");\n";

   } else {
      my ($old_name);
      if ($name =~ /^Initialize/) {
         $returns = shift @arg_types;
         $old_name = shift @args;
      }

      ## Prototype for Internal
      print "      ", (($returns eq "void") ? "procedure " : "function ");
      print "Internal\n";
      &output_params
        (\&c_to_low_ada, "      ", \@args, \@arg_types, $returns, $name);
      print ";\n";
      print "      pragma Import (C, Internal, \"$func\");\n";

      ## If we are returning a complex Widget type
      $is_gobject = ($returns =~ /Gtk(.+)\*/ || $returns =~ /PangoLayout/);
      if ($name !~ /^Initialize/ && $is_gobject)
      {
         print "      Stub : ". &c_widget_to_ada ($returns) . "_Record;\n";
      }

      ## The body of the Ada subprogram
      print "   begin\n";

      if ($name =~ /^Initialize/) {
         print "      Set_Object\n",
               "        (", &capitalize ($old_name), ",\n   ";
      }

      ## Call to Internal
      $index = 0;
      if ($name !~ /^Initialize/ && $returns ne "void") {
         if ($is_gobject) {
            print "      return ", &c_widget_to_ada ($returns) . "\n";
            print "        (Get_User_Data\n";
            print "          (";
         } elsif ($returns eq "gboolean") {
            print "      return Boolean'Val (";
         } elsif ($returns eq "gchar*") {
            print "      return Value (";
         } else {
            print "      return ";
         }
      } else {
         print "      ";
      }
      print "Internal";

      while ($index <= $#args) {
         if ($index == 0) {
            print " (";
         } else {
            print ", ";
         }
         print &c_to_call_ada (&capitalize ($args[$index]), $arg_types[$index]);
         $index++;
      }
      if ($#args >= 0) {
         if ($name !~ /^Initialize/ && $is_gobject) {
             print "), Stub));\n";
         } elsif ($name =~ /^Initialize/) {
             print "));\n";
         } elsif ($returns eq "gboolean") {
            print "));\n";
         } elsif ($returns eq "gchar*") {
            print "));\n";
         } else {
            print ");\n";
         }
     }
   }

   ## End of Ada subprogram
   print "   end $name;\n";
}

## Show the binding for a specific C function. This is only a rough binding,
## and needs to be reviewed manually
sub create_binding() {
   my ($ada_pkg) = shift;
   my ($func) = shift;
   my ($args) = shift;
   my ($returns) = shift;
   my ($specs_only) = shift;
   my ($deprecated) = shift;
   my ($c_docs) = shift;
   my (@args, @arg_types);
   my ($longuest_param) = 0;

   my ($name) = &ada_entity_from_c ($ada_pkg, $func);

   $returns =~ s/\s//;

   $args =~ s/\s+/ /g;
   $args =~ s/ \* ?/\* /g;

   while ($args =~ / ?(\w+\*?) (\w+) ?[,)]/g) {
      push (@args, $2);
      $longuest_param = (length ($2) > $longuest_param) ? length ($2) : $longuest_param;
      push (@arg_types, $1);
   }

   if ($name =~ /^New/) {
     unshift (@args, "Widget");
     unshift (@arg_types, $returns);
     $returns = "void";

     my ($end) = $name;
     $end =~ s/^New//;

     &output_subprogram ("Gtk_New$end", $func, \@args, \@arg_types, $returns,
                         $specs_only, $deprecated, $c_docs);
     $name = "Initialize$end";
   }

   &output_subprogram ($name, $func, \@args, \@arg_types, $returns,
                       $specs_only, $deprecated, $c_docs);
}


## Process a specific C file
sub process_c_file() {
  my ($c_file) = shift;
  my ($with_dir);
  my (%funcs, %binding, %obsolescent, $func, $ada_unit, $success);
  my ($binding, $obsolescent, $c_properties, $c_signals, $ada_properties);
  my ($args, $returns, $deprecated, $ada_signals, $c_docs);

  if (-f $c_file) {
    $with_dir = $c_file;
  } elsif (-f "$c_dir/gtk/$c_file") {
    $with_dir = "$c_dir/gtk/$c_file";
  } elsif (-f "$c_dir/gtk/$c_file.h") {
    $with_dir = "$c_dir/gtk/$c_file.h";
  }

  if (!defined $with_dir) {
     print STDERR "File $c_file not found\n";
     return;
  }

  %funcs = &functions_from_c_file ($with_dir);
  ($c_properties, $c_signals, $c_docs) = &properties_in_c_file ($with_dir);
  $ada_unit = &ada_unit_from_c_file ($c_file);
  ($success, $binding, $obsolescent, $ada_properties, $ada_signals) =
      &ada_bindings_in_unit ($ada_unit);
  %binding = %$binding;
  %obsolescent = %$obsolescent;
  return if (!$success);

  ## Ignore properties already defined in Ada
  my ($prop);
  foreach $prop (keys %$ada_properties) {
     delete $c_properties->{$prop};
  }

  ## Ignore signals already defiend in Ada
  my ($signal);
  foreach $signal (keys %$ada_signals) {
     delete $c_signals->{$signal};
  }

  foreach $func (sort keys %funcs) {
     if (!defined $binding{$func}) {
        print "   --  No binding: $func\n";
     }
  }
  foreach $func (sort keys %binding) {
     ## functions generated for the sake of GtkAda always start with
     ## ada_, so we ignore these for now
     if (substr($func,0,4) ne "ada_" && !defined $funcs{$func}) {
        print "No longer valid: $func\n";
     }
  }

  ## Output specs
  foreach $func (sort keys %funcs) {
     if (!defined $binding{$func}) {
        ($args, $returns, $deprecated) = ($funcs{$func}->[0], $funcs{$func}->[1], $funcs{$func}->[2]);
        &create_binding ($ada_unit, $func, $args, $returns, 1, $deprecated, $c_docs);
     }
  }

   &output_properties (%$c_properties);
   &output_signals (%$c_signals);

  ## Output list of deprecated subprograms
  foreach $func (sort keys %funcs) {
     ($args, $returns, $deprecated) =
       ($funcs{$func}->[0], $funcs{$func}->[1], $funcs{$func}->[2]);
     if ($deprecated) {
        my ($name) = &ada_entity_from_c ($ada_unit, $func);

        # Do not output the pragma for subprograms that are not bound
        if (!defined $obsolescent{$name}
            && defined $binding{$func}
            && $binding{$func} ne "No binding") {
           print "  pragma Obsolescent; --  $name\n";
        }
     }
  }

  ## Output bodies
  foreach $func (sort keys %funcs) {
     if (!defined $binding{$func}) {
        ($args, $returns,$deprecated) =
          ($funcs{$func}->[0], $funcs{$func}->[1], $funcs{$func}->[2]);
        &create_binding ($ada_unit, $func, $args, $returns, 0, $deprecated);
     }
  }
}

## Process the command line
our $c_file;
foreach $c_file (@c_files) {
   &process_c_file ($c_file);
}


