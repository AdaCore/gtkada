--  Code samples quoted by the "Debugging GtkAda applications" chapter of the
--  User's Guide (docs/gtkada_ug/debugging.rst). The text between the START
--  and END markers is included verbatim in the guide.

with System;
with Glib;        use Glib;
with Gtk.Widget;  use Gtk.Widget;

package UG_Debugging is

   --  START ref_count
   function Ref_Count (Object : System.Address) return Guint
     with Import, Convention => C,
          External_Name => "ada_gtk_debug_get_ref_count";
   --  END ref_count

   function Count_Of (Widget : Gtk_Widget) return Guint;
   --  Return the reference count of Widget, the way the guide shows

end UG_Debugging;
