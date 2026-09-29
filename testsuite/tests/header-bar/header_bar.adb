--  GTK has no testsuite/gtk/headerbar.c to port, so this exercises
--  Gtk.Header_Bar's own surface: its four properties, the packing calls,
--  and the bar in the role it exists for, a Gtk_Window's titlebar.

with Ada.Command_Line;
with Glib.Main;      use Glib.Main;
with Glib.Object;    use Glib.Object;
with Glib.Test;      use Glib.Test;
with Gtk.Button;     use Gtk.Button;
with Gtk.Header_Bar; use Gtk.Header_Bar;
with Gtk.Label;      use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;     use Gtk.Widget;
with Gtk.Window;     use Gtk.Window;

procedure Header_Bar is

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration

   function Stop return Boolean;

   procedure Test_Properties with Convention => C;
   procedure Test_Title_Widget with Convention => C;
   procedure Test_Packing with Convention => C;
   procedure Test_Titlebar with Convention => C;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Bar : constant Gtk_Header_Bar := Gtk_Header_Bar_New;
   begin
      Ref_Sink (Bar);

      --  Unlike most boolean properties in this area, this one starts true.
      Assert_True (Bar.Get_Show_Title_Buttons);
      Bar.Set_Show_Title_Buttons (False);
      Assert_False (Bar.Get_Show_Title_Buttons);
      Bar.Set_Show_Title_Buttons (True);
      Assert_True (Bar.Get_Show_Title_Buttons);

      --  decoration-layout is nullable in C; unset must reach Ada as "",
      --  not as a dereferenced null.
      Assert_Cmpstr_Eq (Bar.Get_Decoration_Layout, "");
      Bar.Set_Decoration_Layout ("icon:minimize,maximize,close");
      Assert_Cmpstr_Eq
        (Bar.Get_Decoration_Layout, "icon:minimize,maximize,close");
      Bar.Set_Decoration_Layout (":close");
      Assert_Cmpstr_Eq (Bar.Get_Decoration_Layout, ":close");

      Assert_False (Bar.Get_Use_Native_Controls);
      Bar.Set_Use_Native_Controls (True);
      Assert_True (Bar.Get_Use_Native_Controls);
      Bar.Set_Use_Native_Controls (False);
      Assert_False (Bar.Get_Use_Native_Controls);

      Unref (Bar);
   end Test_Properties;

   -----------------------
   -- Test_Title_Widget --
   -----------------------

   procedure Test_Title_Widget is
      Bar   : constant Gtk_Header_Bar := Gtk_Header_Bar_New;
      Label : constant Gtk_Label := Gtk_Label_New ("Title");
   begin
      Ref_Sink (Bar);

      --  A fresh bar has no title widget of its own: it falls back to the
      --  title of the window it ends up in.
      Assert_True (Bar.Get_Title_Widget = null);

      Bar.Set_Title_Widget (Label);
      Assert_True (Bar.Get_Title_Widget = Gtk_Widget (Label));

      Bar.Set_Title_Widget (null);
      Assert_True (Bar.Get_Title_Widget = null);

      Unref (Bar);
   end Test_Title_Widget;

   ------------------
   -- Test_Packing --
   ------------------

   procedure Test_Packing is
      Bar   : constant Gtk_Header_Bar := Gtk_Header_Bar_New;
      Start : constant Gtk_Button := Gtk_Button_New_With_Label ("Start");
      Fin   : constant Gtk_Button := Gtk_Button_New_With_Label ("End");
   begin
      Ref_Sink (Bar);

      --  Pack_Start sinks the button's floating reference, so Remove would
      --  otherwise drop the last one and free the button before it can be
      --  looked at. Hold a reference of our own across the whole test.
      Ref_Sink (Start);
      Ref_Sink (Fin);

      --  The packed children land under the bar's internal windowhandle/box
      --  nodes rather than directly beneath it, so walking Get_First_Child
      --  would assert on GTK's private structure. Ancestry is the part of
      --  the contract that is actually promised.
      Assert_True (Start.Get_Parent = null);
      Assert_True (Fin.Get_Parent = null);

      Bar.Pack_Start (Start);
      Bar.Pack_End (Fin);

      Assert_True (Start.Get_Ancestor (Gtk.Header_Bar.Get_Type) = Gtk_Widget (Bar));
      Assert_True (Fin.Get_Ancestor (Gtk.Header_Bar.Get_Type) = Gtk_Widget (Bar));

      Bar.Remove (Start);
      Assert_True (Start.Get_Parent = null);
      Assert_True (Fin.Get_Ancestor (Gtk.Header_Bar.Get_Type) = Gtk_Widget (Bar));

      Bar.Remove (Fin);
      Assert_True (Fin.Get_Parent = null);

      Unref (Start);
      Unref (Fin);
      Unref (Bar);
   end Test_Packing;

   -------------------
   -- Test_Titlebar --
   -------------------

   procedure Test_Titlebar is
      Window : constant Gtk_Window := Gtk_Window_New;
      Bar    : constant Gtk_Header_Bar := Gtk_Header_Bar_New;
      Label  : constant Gtk_Label := Gtk_Label_New ("Titled");
      Id     : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      Bar.Set_Title_Widget (Label);
      Bar.Pack_Start (Gtk_Button_New_With_Label ("Back"));

      Window.Set_Title ("Header Bar");
      Window.Set_Titlebar (Bar);
      Assert_True (Window.Get_Titlebar = Gtk_Widget (Bar));

      --  Realizing the window is what makes the bar build its window
      --  controls, which is the one thing an unparented bar cannot do.
      Window.Present;

      Done := False;
      Id := Timeout_Add (500, Stop'Unrestricted_Access);

      while not Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_True (Window.Get_Titlebar = Gtk_Widget (Bar));
      Assert_True (Bar.Get_Title_Widget = Gtk_Widget (Label));

      Window.Destroy;
   end Test_Titlebar;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/headerbar/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/headerbar/title-widget", Test_Title_Widget'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/headerbar/packing", Test_Packing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/headerbar/titlebar", Test_Titlebar'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Header_Bar;
