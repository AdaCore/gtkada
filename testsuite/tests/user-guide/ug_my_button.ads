--  Code samples quoted by the "Object-oriented features" chapter of the
--  User's Guide (docs/gtkada_ug/object_oriented.rst). The text between the
--  START and END markers is included verbatim in the guide.

with Gtk.Button; use Gtk.Button;

package UG_My_Button is

   --  START type
   type My_Button_Record is new Gtk_Button_Record with record
      Count : Natural := 0;
      --  whatever data you want to associate with your button
   end record;
   type My_Button is access all My_Button_Record'Class;
   --  END type

   --  START primitive
   procedure My_Primitive_Func (Myb : access My_Button_Record);
   --  END primitive

   function Create return My_Button;
   --  Create a button the way the guide shows

end UG_My_Button;
