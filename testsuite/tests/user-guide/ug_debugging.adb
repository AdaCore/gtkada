with Glib.Object; use Glib.Object;

package body UG_Debugging is

   --------------
   -- Count_Of --
   --------------

   function Count_Of (Widget : Gtk_Widget) return Guint is
      Count : Guint;
   begin
      --  START ref_count_call
      Count := Ref_Count (Get_Object (Widget));
      --  END ref_count_call
      return Count;
   end Count_Of;

end UG_Debugging;
