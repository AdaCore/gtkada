#!/usr/bin/env perl
use warnings;
use strict;

our ($ada_dir) = "/home/briot/Ada/GtkAda/src/";
#$c_dir  ="/home/briot/gtk/gtk+-2.9/gtk+-2.9.0/";
our ($c_dir)   = "/home/briot/gtk/gtk+-2.8/gtk+-2.8.17";

## parameters are of the form "gtkbutton", "gtkbutton.h", "/dir/gtkbutton.h"
## If the directory is unspecified, files are looked for in $c_dir.
## To find out the list of files that have no binding:
##     ./binding.pl $c_dir/gtk/*.h
## To find out the missing subprogram bindings for a specific file:
##     ./binding.pl gtkbutton
our (@c_files)=@ARGV;

## Set to 1 to list all the files as they are analyzed. This can be set by
## using -v as the first command line parameter
our ($verbose) = 0;

## The following files do not have an associated binding, and will be ignored
our (%c_files_no_binding) =
  (
   # These are actually bound as part of another package
   "gtkhbox"   => 1,
   "gtkvbox"   => 1,
   "gtkhruler" => 1,
   "gtkvruler" => 1,
   "gtkhscale" => 1,
   "gtkvscale" => 1,
   "gtkhscrollbar" => 1,
   "gtkvscrollbar" => 1,
   "gtkhseparator" => 1,
   "gtkvseparator" => 1,
   "gtkmarshalers" => 1,
   "gtkmarshal" => 1,
   "gtkhpaned" => 1,
   "gtkvpaned" => 1,
   "gtksignal"  => 1,

   # All of these are internal
   "gtkalias"               => 1, # Internal only
   "gtkdebug"               => 1, # Internal only
   "gtkprivate"             => 1, # Internal only
   "gtksocketprivate"       => 1, # Internal only
   "gtkentryprivate"        => 1, # Internal only
   "gtkfilechooserprivate"  => 1, # Internal only
   "gtktoggleactionprivate" => 1, # Internal only
   "gtktreeprivate"         => 1, # Internal only
   "gtktextchildprivate"    => 1, # Internal only
   "gtktextiterprivate"     => 1, # Internal only
   "gtktexttagprivate"      => 1, # Internal only
   "gtktextmarkprivate"     => 1, # Internal only
   "gtktree"        => 1,  # Broken
   "gtktreeitem"    => 1,  # Broken
   "gtkrbtree"      => 1,  # Internal only
   "gtksequence"    => 1,  # Internal only
   "gtkxembed"      => 1,  # Internal only
   "gtkwin32embed"  => 1,  # Internal only
   "gtktextutil"    => 1,  # Internal only
   "gtktexttypes"   => 1,  # Internal only
   "gtktextsegment" => 1,  # Internal only
   "gtktextlayout"  => 1,  # Internal only
   "gtktextdisplay" => 1,  # Internal only
   "gtktextbtree"   => 1,  # Internal only
   "gtktreedatalist" => 1, # Internal only
   "gtkmnemonichash" => 1, # Internal only
   "gtkkeyhash"     => 1,  # Internal only
   "gtkpathbar"     => 1,  # Internal only
   "gtkversion"     => 1,  # Bound in gtk.ads
   "gtktypeutils"   => 1,  # Better to use the functions in glib anyway
   "gtkthemes"      => 1,  # For those that want to implement a theme engine... let's use C
   "gtkmodules"     => 1,  # Internal only
   "gtkintl"        => 1,  # No function to bind, already done in Gtkada.Intl
   "gtkwindow-decorate" => 1,  # Never used anywhere, no ref on google... for gtkwindow.c
   "gtkaccessible"  => 1, # Small interface to ATK, which we do not bind anyway
   "gtkdndcursors"  => 1, # Internal to the DND implementation
   "xembed"         => 1,
   "gtkplugprivate" => 1,

   # We might provide a binding for those one day, but they are very secondary
   "gtkimmodule"    => 1,
   );

## Return the base name (no extension) for a C file

sub basename() {
   my ($file) = shift;
   $file =~ s,.*?/([^/]+?)(\.[ch])?$,$1,;  ## basename, no extension
   return $file;
}

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
  my ($adafile) = &basename ($cfile);
  my (@words);
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
  $adafile =~ s/-([hv]?)bbox/-$1button_box/;
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
  $adafile =~ s/-oldeditable/-old_editable/;
  $adafile =~ s/-range/-grange/;
  $adafile =~ s/-imagemenuitem/-image_menu_item/;
  $adafile =~ s/-gamma/-gamma_curve/;
  $adafile =~ s/-plotcanvas(.+)/-extra-plot_canvas-$1/;
  return $adafile;
}

## Find out all C functions bound by a given Ada unit (.ads and .adb files).
## Returns a hash-table indexed by C functions, containing the name of the Ada
## subprogram
our $import_re      = 'pragma\s+Import\s+\(C\s*,\s*(\w+)\s*,\s*"(\w+)"\s*\)';
our $obsolescent_re = '(pragma\s+Obsolescent).*?(?:-- *(\w+))|(?:pragma Obsolescent(?:\s+\("[^"]+"\))?;\n)';
our $not_bound_re   = '--  No binding: (\w+)';
our $ada_prop_re    = '\b(\w+)_Property\s+:\s+(?:--)?\s*constant\b';
our $ada_signal_re  = '\bSignal_(\w+)\s+:\s+constant String :=\s*"([^"]+)"';
our $external_binding_re = '--  External binding: (\w+)';

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
  } else {
     print "Ada unit doesn't exist yet: $unit.ads\n";
     return (0, %binding, %obsolescent);
  }

  if (-f "$ada_dir/$unit.adb") {
     open (FILE, "$ada_dir/$unit.adb");
     $contents .= join ("", <FILE>);
     close (FILE);
  }

  while ($contents =~ /($import_re)|($obsolescent_re)|($not_bound_re)|($ada_prop_re)|($ada_signal_re)|\n[ \t]*(procedure|function)|($external_binding_re)/iog) {
     if (defined $12) {
        $signals{$12} = $13;  ## Indexed on Ada name, value is C name
     } elsif (defined $16) {
        delete $binding{$16}; ## External binding
        delete $obsolescent{&ada_entity_from_c ($unit, $16)};
     } elsif (defined $14) {
        $subprogram_seen = 1;
     } elsif (defined $10) {
        $properties{$10} ++;
     } elsif (defined $8) {
        $binding{$8} = "No binding";
     } elsif (defined $4) {
        if (!$subprogram_seen) {
           $whole_obsolesent = 1;
        } elsif (defined $6) {
           $obsolescent{$6} = 1;
        }
     } else {
        $binding{$3} = $2;

        ## If the whole package is obsolete, this subprogram in particular is
        ## also obsolete. We do this so that we do not have to repeat the
        ## pragma Obsolescent for every entity.
        if ($3 !~ /^ada_/) {
           $obsolescent{&ada_entity_from_c ($unit, $3)} = 1
             if ($whole_obsolesent);
        }
     }
  }

  return (1, \%binding, \%obsolescent, \%properties, \%signals);
}

#######################
## Return all the properties defined for the current widget, as well as the
## signals
#######################

# $1=type $2=name
our $param_spec_re = '\s*g_param_spec_(\w+)\s*\("([^"]+)"';

# no parenthesis
our $property_re =
  'g_object_class_install_property\s*\(\s*\w+\s*,\s*\w+\s*,';

# no parenthesis
our $style_property_re =
  'gtk_widget_class_install_style_property\s*\(\s*\w+\s*,';

# no parenthesis
our $child_property_re =
  'gtk_container_class_install_child_property\s*\(\s*\w+\s*,\s*\w+\s*,';

# $1=description
our $property_descr_re = '\s*,\s*P_\("[^"]+"\),\s*P_\("([^"]+)"';

# $1=signal name
our $c_signal_re = 'g(?:tk)?_signal_new\s*\("([^"]+)"';

# $1=function name   $2=documentation
our $c_doc_re = '/\*\*\s*\* (\w+):\s*(.*?)\*/';

sub properties_in_c_file() {
   my ($fullname) = shift;
   my (%properties, %child_properties, %style_properties);
   my (%signals, %docs, $cname);

   $fullname =~ s/\.h$/.c/g;

   my ($content) = &get_c_file_content ($fullname);

   while ($content =~
            /(?:($property_re|$style_property_re|$child_property_re)$param_spec_re$property_descr_re)|(?:$c_signal_re)|(?:$c_doc_re)/ogs)
   {
      my ($propcategory, $proptype, $propname, $propdescr,
          $signame,
          $funcname, $funcdoc) = ($1, $2, $3, $4, $5, $6, $7);

      if (defined $funcname) {
         $funcdoc =~ s/^\s*\* */   --  /gm;
         $docs {$funcname} = $funcdoc;

      } elsif (defined $proptype) {
         $cname = $propname;
         $propname =~ s/-/_/g;
         $propname = &capitalize ($propname);

         if ($propcategory =~ /style_property/) {
            $style_properties{$propname} = [$proptype, $propdescr, $cname];
         } elsif ($propcategory =~ /child_property/) {
            $child_properties{$propname} = [$proptype, $propdescr, $cname];
         } else {
            $properties{$propname} = [$proptype, $propdescr, $cname];
         }

      } else {
         $cname = $signame;
         $cname =~ s/-/_/g;
         $cname = &capitalize ($cname);
         $signals {$cname} = $signame;
      }
   }
   return (\%properties, \%signals, \%docs,
           \%child_properties, \%style_properties);
}

## Output the descr
sub output_properties() {
   my ($properties, $child_properties, $style_properties) = @_;

   my ($type, $descr, $cname, $prop, $proptype);

   foreach $proptype (('properties', 'child_properties', 'style_properties')) {
      my (%properties, $title, $comment);

      if ($proptype eq 'properties') {
         %properties = %{$properties};
         $title='Properties';
         $comment = "";
      } elsif ($proptype eq 'child_properties') {
         %properties = %{$child_properties};
         $title='Child Properties';
         $comment = "   --  The following properties can be set on children of this widget. See\n"
            . "   --  in particular Gtk.Containers.Child_Set_Property.\n";
      } elsif ($proptype eq 'style_properties') {
         %properties = %{$style_properties};
         $title='Style Properties';
         $comment = "   --  The following properties can be changed through the gtk theme and\n"
            . "   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property\n";
      }

      my (@list) = sort keys %properties;

      if ($#list >= 0) {
         print "   ", '-' x (length ($title) + 6), "\n";
         print "   -- $title --\n";
         print "   ", '-' x (length ($title) + 6), "\n";
         print $comment;
         print "\n";
         print "   --  <$proptype>\n";

         foreach $prop (@list) {
            ($type, $descr, $cname) = @{$properties{$prop}};
            print "   --  Name:  " . $prop . "_Property\n";
            print "   --  Type:  " . &capitalize ($type) . "\n";
            print "   --  Descr: $descr\n";
            print "   --\n";
         }

         print "\n";
         print "   --  </$proptype>\n";
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
  my ($contents, $tmp);

  # print "--  C file: $fullname\n";
  open (FILE, $fullname) || return "";
  $contents = join ("", <FILE>);
  close (FILE);

  return $contents if ($fullname =~ /gtkbbox.[ch]$/);

  $tmp = $fullname;
  $tmp =~ s,/gtk([^/]+)$,/gtkv$1,;
  if (-f $tmp) {
     # print "--  C file: $fullname\n";
     open (FILE, $tmp);
     $contents .= join ("", <FILE>);
     close (FILE);
  }

  $tmp = $fullname;
  $tmp =~ s,/gtk([^/]+)$,/gtkh$1,;
  if (-f $tmp) {
     # print "--  C file: $fullname\n";
     open (FILE, $tmp);
     $contents .= join ("", <FILE>);
     close (FILE);
  }

  return $contents;
}

## Find out all C functions defined in a C file.
## Return a hash table indexed on the functions
our $c_function_re = '\b(\w+(\s*\*+|\s))\s*(\w+)\s*\(([^)]*\))(\s*G_GNUC_CONST)?;';
our $c_deprecated_re = '(if |ifndef|endif).*GTK_DISABLE_DEPRECATED';
our $c_broken_re     = '\#(ifdef.*GTK_ENABLE_BROKEN|endif)';
sub functions_from_c_file() {
  my ($fullname) = shift;
  my (%funcs, $contents);
  my ($deprecated) = 0;

  $contents = &get_c_file_content ($fullname);

  while ($contents =~ /$c_deprecated_re|$c_broken_re|$c_function_re/g) {
     if (defined $1 && ($1 eq "ifndef" || $1 eq "if ")) {
        $deprecated ++;
     } elsif (defined $1 && $1 eq "endif") {
        $deprecated --;
        $deprecated = 0 if ($deprecated < 0);
     } elsif (defined $2 && $2 =~ /^ifdef/) {
        $deprecated ++;
     } elsif (defined $2 && $2 eq "endif") {
        $deprecated --;
        $deprecated = 0 if ($deprecated < 0);
     } else {
        my ($returns, $args, $name) = ($3, $6, $5);
        ## Ignore internal gtk+ functions:
        if (substr($name,0,1) ne '_') {
           $funcs{$name} = [$args, $returns, ($deprecated > 0 ? 1 : 0)];
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

## Return 1 if $1 is a type derived from GObject (or GObject itself)
sub is_object() {
   my ($c_type) = shift;
   return (($c_type =~ /^Gtk(?:.+)\*/
               && $c_type ne "GtkClipboard*"
               && $c_type ne "GtkTreeSortable*"
               && $c_type ne "GtkCellLayout*")
           || $c_type eq "PangoLayout"
           || $c_type eq "GObject*");
}

## Return the Ada type to use for a given C type
sub c_to_ada() {
   my ($c_type) = shift;
   my ($param_index) = shift;  ## -1 for return type
   my ($is_object) = &is_object ($c_type);
   $c_type =~ s/([^_])([A-Z])/$1_$2/g;  ## Split on upper cases

   return "Boolean"            if ($c_type eq "gboolean");
   return "Gdk_$1"             if ($c_type =~ /Gdk_(.+)\*/);
   return "Gfloat"             if ($c_type eq "gfloat");
   return "String"             if ($c_type =~ /g?char\*/);
   return "Gtk_Tree_Iter"      if ($c_type eq "Gtk_Tree_Iter*");
   return "out Gtk_Sort_Type"  if ($c_type eq "Gtk_Sort_Type*");
   return "Gtk_Text_Iter"      if ($c_type eq "Gtk_Text_Iter*");
   return "Gtk_Tree_Path"      if ($c_type eq "Gtk_Tree_Path*");
   return "out Gfloat"         if ($c_type eq "gfloat*");
   return "System.Address"     if ($c_type eq "gpointer");
   return "GType"              if ($c_type eq "G_Type");
   return "access GObject_Record" if ($param_index >= 0 && $c_type eq "G_Object*");
   return "GObject"            if ($param_index == -1 && $c_type eq "G_Object*");
   return "Gtk_Clipboard"      if ($c_type eq "Gtk_Clipboard*");
   return "Gtk_Cell_Layout"    if ($c_type eq "Gtk_Cell_Layout*");
   return "Gtk_Tree_Sortable"  if ($c_type eq "Gtk_Tree_Sortable*");

   if ($is_object) {
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
   my ($is_object) = &is_object ($c_type);
   return "Gboolean"       if ($c_type eq "gboolean");
   $c_type =~ s/([^_])([A-Z])/$1_$2/g;  ## Split on upper cases
   return "Gtk_Tree_Iter"  if ($c_type eq "Gtk_Tree_Iter*");
   return "Gtk_Text_Iter"  if ($c_type eq "Gtk_Text_Iter*");
   return "Gfloat"         if ($c_type eq "gfloat");
   return "Gtk_Clipboard"  if ($c_type eq "Gtk_Clipboard*");
   return "Gtk_Cell_Layout" if ($c_type eq "Gtk_Cell_Layout*");
   return "Gtk_Tree_Path"   if ($c_type eq "Gtk_Tree_Path*");
   return "Gtk_Tree_Sortable" if ($c_type eq "Gtk_Tree_Sortable*");
   return "Gdk_$1"         if ($c_type =~ /Gdk_(.+)\*/);
   return "System.Address" if ($c_type eq "gpointer");
   return "GType"          if ($c_type eq "G_Type");
   return "out Gfloat"     if ($c_type eq "gfloat*");
   return "out Gtk_Sort_Type"     if ($c_type eq "Gtk_Sort_Type*");
   return "String"         if ($c_type =~ /g?char\*/ && $param_index >= 0);
   return "Interfaces.C.Strings.chars_ptr"
                           if ($c_type =~ /g?char\*/ && $param_index == -1);
   return "System.Address" if ($is_object);
   return &capitalize ($c_type);
}

## Return the code to pass or convert back an Ada type to a
## C type
sub c_to_call_ada() {
   my ($name) = shift;
   my ($c_type) = shift;
   my ($is_object) = &is_object ($c_type);
   return "Boolean'Pos ($name)" if ($c_type eq "gboolean");
   return "$name & ASCII.NUL"   if ($c_type =~ /g?char\*/);
   return "$name"               if ($c_type eq "GtkTreeIter*");
   return "$name"               if ($c_type eq "GtkTextIter*");
   if ($is_object) {
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

   if ($ada_pkg eq "gtk_main" || $ada_pkg eq "gtk_style") {
      $name =~ s/^gtk_//;
   } elsif ($ada_pkg eq "glib_main") {
      $name =~ s/^g_//;
   } elsif ($ada_pkg eq "gtk_gentry") {
      $name =~ s/^gtk_entry_//;
   } elsif ($ada_pkg eq "gtk_color_selection") {
      $name =~ s/^gtk_color_selection_//;
   } elsif ($ada_pkg eq "gtk_dnd") {
      $name =~ s/^gtk_drag_//;
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
      $is_gobject = &is_object ($returns);
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
  my ($binding, $obsolescent, $ada_properties);
  my ($args, $returns, $deprecated, $ada_signals);

  if (-f $c_file) {
    $with_dir = $c_file;
  } elsif (-f "$c_dir/gtk/$c_file") {
    $with_dir = "$c_dir/gtk/$c_file";
  } elsif (-f "$c_dir/gtk/$c_file.h") {
    $with_dir = "$c_dir/gtk/$c_file.h";
  }

  if (!defined $with_dir) {
     print "File $c_file not found\n";
     return;
  }

  %funcs = &functions_from_c_file ($with_dir);
  my ($c_properties, $c_signals, $c_docs, $c_child_properties,
      $c_style_properties) = &properties_in_c_file ($with_dir);
  $ada_unit = &ada_unit_from_c_file ($c_file);
  ($success, $binding, $obsolescent, $ada_properties, $ada_signals) =
      &ada_bindings_in_unit ($ada_unit);
  return if (!$success);

  print "Analyzing $with_dir\n" if ($verbose);

  %binding = %$binding;
  %obsolescent = %$obsolescent;

  ## Ignore properties already defined in Ada
  my ($prop);
  foreach $prop (keys %$ada_properties) {
     delete $c_properties->{$prop};
     delete $c_style_properties->{$prop};
     delete $c_child_properties->{$prop};
  }

  ## Ignore signals already defined in Ada
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

  ## Output list of deprecated subprograms
  foreach $func (sort keys %funcs) {
     ($args, $returns, $deprecated) =
       ($funcs{$func}->[0], $funcs{$func}->[1], $funcs{$func}->[2]);
     if ($deprecated) {
        my ($name) = &ada_entity_from_c ($ada_unit, $func);

        # Do not output the pragma for subprograms that are not bound
        if (defined $binding{$func}
            && $binding{$func} ne "No binding")
        {
          if (!defined $obsolescent{$name}) {
             print "  pragma Obsolescent; --  $name\n";
          } else {
             delete $obsolescent{$name};
          }
        }
     }
  }
  foreach $func (sort keys %obsolescent) {
      print "Remove pragma Obsolescent; --  $func\n";
  }


  ## Output specs
  foreach $func (sort keys %funcs) {
     if (!defined $binding{$func}) {
        ($args, $returns, $deprecated) = ($funcs{$func}->[0], $funcs{$func}->[1], $funcs{$func}->[2]);
        &create_binding ($ada_unit, $func, $args, $returns, 1, $deprecated, $c_docs);
     }
  }

   &output_properties ($c_properties, $c_child_properties, $c_style_properties);
   &output_signals (%$c_signals);

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
our $current_file;

foreach $current_file (sort @c_files) {
   if ($current_file eq "-v") {
      $verbose = 1;
   } elsif (!defined $c_files_no_binding{&basename ($current_file)}) {
     &process_c_file ($current_file);
   }
}

