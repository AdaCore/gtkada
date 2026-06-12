--  Ada port of GTK's testsuite/gtk/popover.c (GTK 4.22.2).
--
--  Creates a window holding a Gtk_Menu_Button whose popover contains a
--  label, pops the popover up from a timeout, then tweaks the label's
--  alignment while the popover is shown.

with Ada.Command_Line;
with Glib;            use Glib;
with Glib.Main;       use Glib.Main;
with Glib.Test;       use Glib.Test;
with Gtk.Label;       use Gtk.Label;
with Gtk.Main;
with Gtk.Menu_Button; use Gtk.Menu_Button;
with Gtk.Popover;     use Gtk.Popover;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;

procedure Popover is

   The_Popover : Gtk_Popover;

   Done : Boolean := False;
   pragma Volatile (Done);
   --  Set from a timeout callback dispatched by Main_Context_Iteration

   function Pop_Up return Boolean;
   function Tickle return Boolean;
   function Stop return Boolean;

   procedure Test_Show_Popover
   with Convention => C;

   ------------
   -- Pop_Up --
   ------------

   function Pop_Up return Boolean is
   begin
      The_Popover.Popup;
      return False;
   end Pop_Up;

   ------------
   -- Tickle --
   ------------

   function Tickle return Boolean is
      Label : constant Gtk_Widget := The_Popover.Get_First_Child;
   begin
      Label.Set_Valign (Align_Start);
      return False;
   end Tickle;

   ----------
   -- Stop --
   ----------

   function Stop return Boolean is
   begin
      Done := True;
      Wakeup (null);
      return False;
   end Stop;

   -----------------------
   -- Test_Show_Popover --
   -----------------------

   procedure Test_Show_Popover is
      Window : constant Gtk_Window := Gtk_Window_New;
      Button : constant Gtk_Menu_Button := Gtk_Menu_Button_New;
      Id     : G_Source_Id;
      pragma Unreferenced (Id);
   begin
      The_Popover := Gtk_Popover_New;
      The_Popover.Set_Child (Gtk_Label_New ("Nu?"));
      Button.Set_Popover (The_Popover);
      Window.Set_Child (Button);

      Window.Present;

      Id := Timeout_Add (1000, Pop_Up'Unrestricted_Access);
      Id := Timeout_Add (2000, Tickle'Unrestricted_Access);
      Done := False;
      Id := Timeout_Add (3000, Stop'Unrestricted_Access);

      while not Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
   end Test_Show_Popover;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func ("/popover/show", Test_Show_Popover'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Popover;
