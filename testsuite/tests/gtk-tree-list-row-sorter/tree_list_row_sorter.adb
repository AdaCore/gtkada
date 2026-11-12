--  Tests the Gtk.Tree_List_Row_Sorter constructors, child sorter accessors,
--  and generated property. GtkAda has no Ada port of the C treesorter cases;
--  this exercises the wrapper API directly and checks the stored GObject.

with Ada.Command_Line;
with System;

with Glib;
with Glib.Object;
with Glib.Properties;
with Glib.Test;
with Gtk.Main;
with Gtk.Sorter;
with Gtk.String_Sorter;
with Gtk.Tree_List_Row_Sorter;

procedure Tree_List_Row_Sorter is
   use Glib.Test;
   use Gtk.Tree_List_Row_Sorter;

   use type Glib.GType;
   use type Glib.Object.GObject;
   use type Gtk.Sorter.Gtk_Sorter;
   use type System.Address;

   procedure Test_Sorter_API with Convention => C;

   ----------------------
   -- Test_Sorter_API --
   ----------------------

   procedure Test_Sorter_API is
      Via_Procedure : Gtk_Tree_List_Row_Sorter;
      Via_Function  : Gtk_Tree_List_Row_Sorter;
      Unused_Child  : Gtk.String_Sorter.Gtk_String_Sorter;
      Assigned      : Gtk.String_Sorter.Gtk_String_Sorter;
      Property_Item : Glib.Object.GObject;
      Original      : System.Address;
   begin
      --  The public type is registered and the procedure constructor accepts
      --  a null child sorter, which leaves Get_Sorter null.
      Assert_True (Get_Type /= Glib.GType_Invalid);
      Gtk_New (Via_Procedure, null);
      Assert_True (Get_Sorter (Via_Procedure) = null);
      Original := Glib.Object.Get_Object (Via_Procedure);

      --  Initialize must preserve an existing GObject and its current child.
      Unused_Child := Gtk.String_Sorter.Gtk_String_Sorter_New (null);
      Initialize (Via_Procedure, Unused_Child);
      Assert_True (Glib.Object.Get_Object (Via_Procedure) = Original);
      Assert_True (Get_Sorter (Via_Procedure) = null);
      Glib.Object.Unref (Glib.Object.GObject (Unused_Child));

      --  Set_Sorter stores a child sorter; the getter and property expose the
      --  same GTK object, so compare its stable C handle.
      Assigned := Gtk.String_Sorter.Gtk_String_Sorter_New (null);
      Set_Sorter (Via_Procedure, Assigned);
      Assert_True
        (Glib.Object.Get_Object (Get_Sorter (Via_Procedure)) =
         Glib.Object.Get_Object (Assigned));
      Property_Item := Glib.Properties.Get_Property
        (Via_Procedure, Sorter_Property);
      Assert_True
        (Glib.Object.Get_Object (Property_Item) =
         Glib.Object.Get_Object (Assigned));

      --  The writable property updates the same child as Set_Sorter, and a
      --  null assignment clears it through both generated access paths.
      Glib.Properties.Set_Property
        (Via_Procedure, Sorter_Property, Assigned);
      Assert_True
        (Glib.Object.Get_Object (Get_Sorter (Via_Procedure)) =
         Glib.Object.Get_Object (Assigned));
      Set_Sorter (Via_Procedure, null);
      Assert_True (Get_Sorter (Via_Procedure) = null);
      Assert_True
        (Glib.Properties.Get_Property (Via_Procedure, Sorter_Property) = null);
      Glib.Object.Unref (Glib.Object.GObject (Assigned));

      --  The function constructor transfers its child to the new sorter and
      --  Get_Sorter returns that same object as a Gtk.Sorter.
      Assigned := Gtk.String_Sorter.Gtk_String_Sorter_New (null);
      Via_Function := Gtk_Tree_List_Row_Sorter_New (Assigned);
      Assert_True
        (Glib.Object.Get_Object (Get_Sorter (Via_Function)) =
         Glib.Object.Get_Object (Assigned));

      --  Release the GTK objects created by this test after their final use.
      Glib.Object.Unref (Glib.Object.GObject (Via_Function));
      Glib.Object.Unref (Glib.Object.GObject (Via_Procedure));
   end Test_Sorter_API;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/tree-list-row-sorter/api", Test_Sorter_API'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Tree_List_Row_Sorter;
