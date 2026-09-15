--  Exercises the Pango.Font accessors bound by this branch, and in
--  particular the nullable instance parameter of pango_font_get_font_map:
--  the C function is documented to return NULL when handed a NULL font, so
--  the binding must accept a null access value rather than reject it with
--  "not null access".

with Ada.Command_Line;

with Glib.Test;  use Glib.Test;
with Gtk.Label;  use Gtk.Label;
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Pango.Context;
with Pango.Font; use Pango.Font;
with Pango.Font_Map;

procedure Main is

   use type Pango.Font_Map.Pango_Font_Map;

   procedure Test_Null_Font
   with Convention => C;

   procedure Test_Font_Map
   with Convention => C;

   --------------------
   -- Test_Null_Font --
   --------------------

   --  pango_font_get_font_map (NULL) returns NULL.

   procedure Test_Null_Font is
   begin
      Assert_True (Pango.Font.Get_Font_Map (null) = null);
   end Test_Null_Font;

   -------------------
   -- Test_Font_Map --
   -------------------

   --  A font loaded from a context reports back the context's font map.

   procedure Test_Font_Map is
      Window  : Gtk_Window;
      Label   : Gtk_Label;
      Context : Pango.Context.Pango_Context;
      Desc    : Pango_Font_Description;
      Font    : Pango_Font;
   begin
      Window := Gtk_Window_New;
      Gtk_New (Label, "hello");
      Window.Set_Child (Label);

      Context := Label.Get_Pango_Context;

      Desc := From_String ("Sans 12");
      Font := Context.Load_Font (Desc);
      Free (Desc);

      Assert_True (Font /= null);
      Assert_True (Pango.Font.Get_Font_Map (Font) = Context.Get_Font_Map);

      Window.Destroy;
   end Test_Font_Map;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/pango-font/null-font", Test_Null_Font'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-font/font-map", Test_Font_Map'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
