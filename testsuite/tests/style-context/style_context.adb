--  Gtk.Style_Context coverage.

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Interfaces.C;

with Gdk.RGBA;          use Gdk.RGBA;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Test;         use Glib.Test;
with Gtk.Button;        use Gtk.Button;
with Gtk.Css_Provider;  use Gtk.Css_Provider;
with Gtk.Main;
with Gtk.Snapshot;      use Gtk.Snapshot;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Style_Provider;

procedure Style_Context is

   use type Interfaces.C.C_float;

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
      Snapshot : constant Gtk_Snapshot := Gtk_Snapshot_New;
      Color    : Gdk_RGBA;
      Found    : Boolean;
   begin
      Ref_Sink (Button);
      Assert_True (Context /= null);

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
        ("@define-color gtkada_style_context_test rgb(255, 0, 0);");
      Context.Add_Provider
        (+Provider, Gtk.Style_Provider.Priority_Application);
      Context.Lookup_Color ("gtkada_style_context_test", Color, Found);
      Assert_True (Found);
      Assert_True (Color.Red = 1.0);
      Assert_True (Color.Green = 0.0);
      Assert_True (Color.Blue = 0.0);
      Assert_True (Color.Alpha = 1.0);
      Context.Remove_Provider (+Provider);

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
