--  Gtk.Style_Context coverage.

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Interfaces.C;

with Gdk;               use Gdk;
with Gdk.RGBA;          use Gdk.RGBA;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Test;         use Glib.Test;
with Gtk.Border;
with Gtk.Button;        use Gtk.Button;
with Gtk.Css_Provider;  use Gtk.Css_Provider;
with Gtk.Enums;
with Gtk.Main;
with Gtk.Snapshot;      use Gtk.Snapshot;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Style_Provider;

procedure Style_Context is

   use type Interfaces.C.C_float;
   use type Gdk.Gdk_Display;
   use type Gtk.Enums.Gtk_State_Flags;
   use type Gtk.Style_Provider.Gtk_Style_Provider;

   function Contains (Text, Pattern : String) return Boolean
   is (Ada.Strings.Fixed.Index (Text, Pattern) /= 0);

   procedure Test_Widget_Context
   with Convention => C;

   -------------------------
   -- Test_Widget_Context --
   -------------------------

   procedure Test_Widget_Context is
      Button   : constant Gtk_Button := Gtk_Button_New_With_Label ("Test");
      Context  : constant Gtk_Style_Context := Get_Style_Context (Button);
      Provider : constant Gtk_Css_Provider := Gtk_Css_Provider_New;
      Provider_Interface : constant Gtk.Style_Provider.Gtk_Style_Provider :=
        Gtk.Css_Provider."+" (Provider);
      Same_Interface : constant Gtk.Style_Provider.Gtk_Style_Provider :=
        Gtk.Style_Provider."+" (Provider_Interface);
      Snapshot : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color    : Gdk_RGBA;
      Found    : Boolean;
      Border   : Gtk.Border.Gtk_Border;
      Margin   : Gtk.Border.Gtk_Border;
      Padding  : Gtk.Border.Gtk_Border;
      Display  : constant Gdk_Display := Context.Get_Display;
      Scale    : constant Gint := Context.Get_Scale;
      State    : constant Gtk.Enums.Gtk_State_Flags := Context.Get_State;
   begin
      Ref_Sink (Button);
      --  Gtk.Style_Provider."+" is the identity conversion for an interface
      --  value; check it explicitly in addition to the class-to-interface
      --  conversion used below when adding the provider.
      Assert_True (Same_Interface = Provider_Interface);
      Assert_True (Context /= null);
      Assert_True (Display /= null);

      Context.Add_Class ("style-context-test");
      Assert_True (Context.Has_Class ("style-context-test"));
      Assert_True
        (Contains
           (Context.To_String (Style_Context_Print_None),
            "style-context-test"));

      Context.Save;
      Context.Add_Class ("temporary-style-class");
      Assert_True (Context.Has_Class ("temporary-style-class"));
      Context.Restore;
      Assert_False (Context.Has_Class ("temporary-style-class"));

      Provider.Load_From_String
        ("@define-color gtkada_style_context_test rgb(255, 0, 0);"
         & " button.style-context-test {"
         & " border: 3px solid; margin: 4px; padding: 5px;"
         & " color: rgb(255, 0, 0); }");
      Context.Add_Provider
        (+Provider, Gtk.Style_Provider.Priority_Application);

      --  The legacy getters expose the CSS box and foreground color. The
      --  provider above gives each box edge a deterministic value to check.
      Context.Get_Border (Border);
      Assert_Cmpint_Eq (Gint (Border.Left), 3);
      Assert_Cmpint_Eq (Gint (Border.Right), 3);
      Assert_Cmpint_Eq (Gint (Border.Top), 3);
      Assert_Cmpint_Eq (Gint (Border.Bottom), 3);
      Context.Get_Margin (Margin);
      Assert_Cmpint_Eq (Gint (Margin.Left), 4);
      Assert_Cmpint_Eq (Gint (Margin.Right), 4);
      Assert_Cmpint_Eq (Gint (Margin.Top), 4);
      Assert_Cmpint_Eq (Gint (Margin.Bottom), 4);
      Context.Get_Padding (Padding);
      Assert_Cmpint_Eq (Gint (Padding.Left), 5);
      Assert_Cmpint_Eq (Gint (Padding.Right), 5);
      Assert_Cmpint_Eq (Gint (Padding.Top), 5);
      Assert_Cmpint_Eq (Gint (Padding.Bottom), 5);

      --  Get_Color returns the computed foreground color for this node.
      Context.Get_Color (Color);
      Assert_True (Color.Red = 1.0);
      Assert_True (Color.Green = 0.0);
      Assert_True (Color.Blue = 0.0);
      Assert_True (Color.Alpha = 1.0);

      --  The context from the button already uses this display. Passing it
      --  back exercises the legacy setter without migrating a live widget.
      Context.Set_Display (Display);
      Assert_True (Context.Get_Display = Display);

      --  Scale and state are mutable on this context and are read back using
      --  their corresponding getters. Restore the original values afterwards.
      Context.Set_Scale (2);
      Assert_Cmpint_Eq (Context.Get_Scale, 2);
      Context.Set_Scale (Scale);
      Assert_Cmpint_Eq (Context.Get_Scale, Scale);
      Context.Set_State (Gtk.Enums.Gtk_State_Flag_Selected);
      Assert_True
        (Context.Get_State = Gtk.Enums.Gtk_State_Flag_Selected);
      Context.Set_State (State);
      Assert_True (Context.Get_State = State);

      Context.Lookup_Color ("gtkada_style_context_test", Color, Found);
      Assert_True (Found);
      Assert_True (Color.Red = 1.0);
      Assert_True (Color.Green = 0.0);
      Assert_True (Color.Blue = 0.0);
      Assert_True (Color.Alpha = 1.0);
      Context.Remove_Provider (+Provider);

      --  Removing a class updates the context's class set.
      Context.Remove_Class ("style-context-test");
      Assert_False (Context.Has_Class ("style-context-test"));

      Add_Provider_For_Display
        (Context.Get_Display,
         +Provider,
         Gtk.Style_Provider.Priority_Application);
      Remove_Provider_For_Display (Context.Get_Display, +Provider);

      Snapshot.Render_Background (Context, 0.0, 0.0, 10.0, 10.0);
      Snapshot.Render_Frame (Context, 0.0, 0.0, 10.0, 10.0);
      Snapshot.Render_Focus (Context, 0.0, 0.0, 10.0, 10.0);

      Unref (Snapshot);
      Unref (Provider);
      Unref (Button);
   end Test_Widget_Context;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/style-context/widget-context",
      Test_Widget_Context'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Style_Context;
