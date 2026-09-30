with Ada.Command_Line;
with Interfaces.C.Strings;
with System;               use type System.Address;
with Glib;                 use Glib;
with Glib.Error;           use Glib.Error;
with Glib.Object;          use Glib.Object;
with Glib.Test;            use Glib.Test;
with Glib.Values;          use Glib.Values;
with Gdk.Content_Provider; use Gdk.Content_Provider;
with Gdk.Drag;             use Gdk.Drag;
with Gtk.Drag_Source;      use Gtk.Drag_Source;
with Gtk.Label;            use Gtk.Label;
with Gtk.Main;

procedure Drag_Source is

   Prepared : Natural := 0;
   --  Incremented by the "prepare" handler

   Last_X, Last_Y : Gdouble := 0.0;

   Provider : Gdk_Content_Provider;
   --  What the "prepare" handler hands out

   procedure Emit_Prepare
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      X, Y     : Gdouble;
      Result   : access System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";
   --  The variadic tail of "prepare" is (double, double, GdkContentProvider **)

   function On_Prepare
     (Self : access Gtk_Drag_Source_Record'Class;
      X, Y : Gdouble) return Gdk_Content_Provider;
   procedure Test_Properties with Convention => C;
   procedure Test_Content with Convention => C;
   procedure Test_Prepare with Convention => C;

   ----------------
   -- On_Prepare --
   ----------------

   function On_Prepare
     (Self : access Gtk_Drag_Source_Record'Class;
      X, Y : Gdouble) return Gdk_Content_Provider
   is
      pragma Unreferenced (Self);
   begin
      Prepared := Prepared + 1;
      Last_X   := X;
      Last_Y   := Y;
      return Provider;
   end On_Prepare;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      S : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
   begin
      --  Nothing to drag yet, and no drag in progress.
      Assert_True (S.Get_Content = null);
      Assert_True (S.Get_Drag = null);

      S.Set_Actions (Gdk_Action_Copy or Gdk_Action_Move);
      Assert_True (S.Get_Actions = (Gdk_Action_Copy or Gdk_Action_Move));

      --  Cancelling with no drag in progress is harmless.
      S.Drag_Cancel;

      Unref (S);
   end Test_Properties;

   ------------------
   -- Test_Content --
   ------------------

   procedure Test_Content is
      S     : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Value : GValue;
      P     : Gdk_Content_Provider;
      Error : GError;
   begin
      Init_Set_String (Value, "hello");
      P := Gdk_Content_Provider_New_For_Value (Value);
      Unset (Value);

      S.Set_Content (P);
      Assert_True (S.Get_Content = P);

      --  The source keeps its own reference to the content.
      Unref (P);
      Assert_True (S.Get_Content /= null);

      --  And it still yields the string it was made from.
      Init (Value, GType_String);
      Assert_True (S.Get_Content.Get_Value (Value, Error));
      Assert_True (Get_String (Value) = "hello");
      Unset (Value);

      Unref (S);
   end Test_Content;

   ------------------
   -- Test_Prepare --
   ------------------

   procedure Test_Prepare is
      S      : constant Gtk_Drag_Source := Gtk_Drag_Source_New;
      Value  : GValue;
      Result : aliased System.Address := System.Null_Address;
      Name   : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("prepare");
   begin
      Init_Set_String (Value, "dragged");
      Provider := Gdk_Content_Provider_New_For_Value (Value);
      Unset (Value);

      S.On_Prepare (On_Prepare'Unrestricted_Access);
      Emit_Prepare (Get_Object (S), Name, 3.5, 7.25, Result'Access);
      Interfaces.C.Strings.Free (Name);

      Assert_Cmpint_Eq (Gint (Prepared), 1);
      Assert_True (Last_X = 3.5);
      Assert_True (Last_Y = 7.25);
      --  The provider the handler returned is what the emission yields.
      Assert_True (Result = Get_Object (Provider));

      Unref (S);
      Unref (Provider);
   end Test_Prepare;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/dragsource/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/content", Test_Content'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/dragsource/prepare", Test_Prepare'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drag_Source;

--  GTK has no C test for GtkDragSource, so these cases are ours: they cover
--  the surface the Gtk.Drag_Source binding adds, chiefly the return value of
--  the "prepare" signal, a Gdk_Content_Provider handed back to GTK.
