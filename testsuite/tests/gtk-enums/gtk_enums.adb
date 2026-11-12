--  Gtk.Enums tests for its string conversion helpers and owned string lists.
--  The list appends and reads exercise both Convert overloads; Free_String_List
--  checks the documented ownership rule for both GLib list representations.

with Ada.Command_Line;

with Glib.Test; use Glib.Test;
with Gtk.Enums;

procedure Gtk_Enums is
   use type Gtk.Enums.String_List.Glist;
   use type Gtk.Enums.String_SList.GSlist;

   --  Free an empty GList safely, then check stored strings and release their
   --  allocated C buffers along with the list cells.
   procedure Test_Glist
   with Convention => C;

   procedure Test_Glist is
      List : Gtk.Enums.String_List.Glist := Gtk.Enums.String_List.Null_List;
   begin
      Gtk.Enums.Free_String_List (List);
      Assert_True (List = Gtk.Enums.String_List.Null_List);

      Gtk.Enums.String_List.Append (List, "first enum name");
      Gtk.Enums.String_List.Append (List, "second enum name");
      Assert_Cmpstr_Eq (Gtk.Enums.String_List.Get_Data (List), "first enum name");
      Assert_Cmpstr_Eq
        (Gtk.Enums.String_List.Get_Data (Gtk.Enums.String_List.Next (List)),
         "second enum name");

      Gtk.Enums.Free_String_List (List);
      Assert_True (List = Gtk.Enums.String_List.Null_List);
   end Test_Glist;

   --  Exercise the single-linked overload with the same ownership checks.
   procedure Test_Gslist
   with Convention => C;

   procedure Test_Gslist is
      List : Gtk.Enums.String_SList.GSlist :=
        Gtk.Enums.String_SList.Null_List;
   begin
      Gtk.Enums.Free_String_List (List);
      Assert_True (List = Gtk.Enums.String_SList.Null_List);

      Gtk.Enums.String_SList.Append (List, "arrow up");
      Gtk.Enums.String_SList.Append (List, "arrow down");
      Assert_Cmpstr_Eq (Gtk.Enums.String_SList.Get_Data (List), "arrow up");
      Assert_Cmpstr_Eq
        (Gtk.Enums.String_SList.Get_Data (Gtk.Enums.String_SList.Next (List)),
         "arrow down");

      Gtk.Enums.Free_String_List (List);
      Assert_True (List = Gtk.Enums.String_SList.Null_List);
   end Test_Gslist;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/gtk-enums/glist", Test_Glist'Unrestricted_Access);
   Glib.Test.Add_Func ("/gtk-enums/gslist", Test_Gslist'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gtk_Enums;
