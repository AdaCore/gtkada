--  Exercises the Gtk.Drawing_Area binding.
--
--  /drawing-area/content-size checks that the content width and height
--  round-trip and that they are what the widget asks for as its natural size.
--
--  /drawing-area/draw-func is the interesting one: it puts a drawing area in
--  a window, presents it, and pumps the main loop until the draw function has
--  actually run. That proves the whole path works -- the callback is
--  marshalled, the cairo_t reaches Ada as a usable Cairo_Context, and the
--  width and height handed over are the size the widget was allocated.

with Ada.Command_Line;
with Glib;             use Glib;
with Glib.Main;        use Glib.Main;
with Glib.Test;        use Glib.Test;
with Cairo;            use Cairo;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Main;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Window;       use Gtk.Window;

procedure Drawing_Area is

   Drawn : Boolean := False;
   pragma Volatile (Drawn);
   --  Set from the draw function, which is dispatched by the main loop.

   Drawn_Width, Drawn_Height : Gint := -1;
   --  The size the draw function was told about.

   Done : Boolean := False;
   pragma Volatile (Done);

   function Stop return Boolean;

   procedure Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint);

   procedure Test_Content_Size
   with Convention => C;


   procedure Test_Draw_Func
   with Convention => C;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Area   : not null access Gtk_Drawing_Area_Record'Class;
      Cr     : Cairo_Context;
      Width  : Gint;
      Height : Gint)
   is
      pragma Unreferenced (Area);
   begin
      --  Draw something, so that the context is not merely received but used.
      Set_Source_Rgb (Cr, 0.0, 0.0, 0.0);
      Rectangle (Cr, 0.0, 0.0, Gdouble (Width), Gdouble (Height));
      Fill (Cr);

      Drawn_Width := Width;
      Drawn_Height := Height;
      Drawn := True;
   end Draw;

   -----------------------
   -- Test_Content_Size --
   -----------------------

   procedure Test_Content_Size is
      Area : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
   begin
      Area.Set_Content_Width (123);
      Area.Set_Content_Height (45);

      Assert_Cmpint_Eq (Area.Get_Content_Width, 123);
      Assert_Cmpint_Eq (Area.Get_Content_Height, 45);
   end Test_Content_Size;

   --------------------
   -- Test_Draw_Func --
   --------------------

   procedure Test_Draw_Func is
      Window : constant Gtk_Window := Gtk_Window_New;
      Area   : constant Gtk_Drawing_Area := Gtk_Drawing_Area_New;
      Id     : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Area.Set_Content_Width (80);
      Area.Set_Content_Height (60);
      Area.Set_Draw_Func (Draw'Unrestricted_Access);

      Window.Set_Child (Area);
      Window.Present;

      Done := False;
      Id := Timeout_Add (3000, Stop'Unrestricted_Access);

      --  Stop as soon as the draw function has run; the timeout is only
      --  there so the test fails instead of hanging if it never does.
      while not Drawn and not Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_True (Drawn);

      --  The draw function is handed the size the widget was actually
      --  allocated, not the content size: the area is the window's only
      --  child, so it is stretched to fill it. The content size acts as a
      --  floor on that allocation.
      Assert_Cmpint_Eq (Drawn_Width, Area.Get_Width);
      Assert_Cmpint_Eq (Drawn_Height, Area.Get_Height);
      Assert_Cmpint_Ge (Drawn_Width, 80);
      Assert_Cmpint_Ge (Drawn_Height, 60);
   end Test_Draw_Func;

begin
   Glib.Test.Init;

   --  Widgets cannot be created before GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/drawing-area/content-size", Test_Content_Size'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/drawing-area/draw-func", Test_Draw_Func'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drawing_Area;
