with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Box;
with Gtk.Button;
with Gtk.Container;
with Gtk.Enums;
with Gtk.HSeparator;
with Gtk.Label;
with Gtk.Main;
with Gtk.Rc;
with Gtk.Scrolled_Window;
with Gtk.Signal;
with Gtk.Vbox;
with Gtk.Widget;
with Gtk.Window;

with Create_Buttons;
with Create_Button_Box;
with Create_Check_Buttons;
with Create_Color_Selection;
with Create_Dialog;
with Create_Entry;
with Create_File_Selection;
with Create_Gamma_Curve;
with Create_Handle_Box;
with Create_Menu;
with Create_Toolbar;

--  with Ada.Text_IO; use Ada.Text_IO;

package body Test is

   package ASU renames Ada.Strings.Unbounded;

   function US (Source : String) return ASU.Unbounded_String
     renames ASU.To_Unbounded_String;



   ----------------------
   --  Local services  --
   ----------------------

   procedure Create_Main_Window;
   procedure Do_Exit (Widget : in out Gtk.Widget.Gtk_Widget'Class;
                      Data : in out Window.Gtk_Window'Class);
   procedure Exit_Main (Object : in out Window.Gtk_Window'Class);
   function Gtk_Version_Number return String;


   -------------------------------
   --  Callback instantiations  --
   -------------------------------

   package Button_Callback is new Signal.Void_Callback
     (Widget_Type => Button.Gtk_Button);
   use type Button_Callback.Callback;

   package Do_Exit_Callback is new Signal.Callback
     (Data_Type => Window.Gtk_Window'Class, Widget_Type => Widget.Gtk_Widget);

   package Window_Callback is new Signal.Void_Callback
     (Widget_Type => Window.Gtk_Window);

   ----------------------
   --  The buttons...  --
   ----------------------

   type Button_Information is
      record
         Label : ASU.Unbounded_String;
         Cb : Button_Callback.Callback;
      end record;

   type Buttons_Array is array (Positive range <>) of Button_Information;


   Buttons : constant Buttons_Array :=
     ((US ("button box"), Create_Button_Box.Run'Access),
      (US ("buttons"), Create_Buttons.Run'Access),
      (US ("check buttons"), Create_Check_Buttons.Run'Access),
      (US ("clist"), null),
      (US ("color selection"), Create_Color_Selection.Run'Access),
      (US ("cursors"), null),
      (US ("dialog"), Create_Dialog.Run'Access),
      (US ("dnd"), null),
      (US ("entry"), Create_Entry.Run'Access),
      (US ("file selection"), Create_File_Selection.Run'Access),
      (US ("gamma curve"), Create_Gamma_Curve.Run'Access),
      (US ("handle box"), Create_Handle_Box.Run'Access),
      (US ("list"), null),
      (US ("menus"), Create_Menu.Run'Access),
      (US ("miscellaneous"), null),
      (US ("notebook"), null),
      (US ("panes"), null),
      (US ("pixmap"), null),
      (US ("preview color"), null),
      (US ("preview gray"), null),
      (US ("progress bar"), null),
      (US ("radio buttons"), null),
      (US ("range controls"), null),
      (US ("reparent"), null),
      (US ("rulers"), null),
      (US ("scrolled windows"), null),
      (US ("shapes"), null),
      (US ("spinbutton"), null),
      (US ("statusbar"), null),
      (US ("test idle"), null),
      (US ("test mainloop"), null),
      (US ("test scrolling"), null),
      (US ("test selection"), null),
      (US ("test timeout"), null),
      (US ("text"), null),
      (US ("toggle buttons"), null),
      (US ("toolbar"), Create_Toolbar.Run'Access),
      (US ("tooltips"), null),
      (US ("tree"), null),
      (US ("WM hints"), null)
      );

   --------------------------
   --  Create_Main_Window  --
   --------------------------

   procedure Create_Main_Window is
      Main_Window : Window.Gtk_Window;
      Cb_Id : Guint;
      Box1, Box2 : Vbox.Gtk_Vbox;
      A_Label : Label.Gtk_Label;
      A_Scrolled_Window : Scrolled_Window.Gtk_Scrolled_Window;
      Temp : Adjustment.Gtk_Adjustment;
      A_Button : Button.Gtk_Button;
      Separator : HSeparator.Gtk_HSeparator;
   begin
      Window.Gtk_New (Window => Main_Window,
                      The_Type => Enums.Window_Toplevel);
      Widget.Set_Name (Widget => Main_Window, Name => "main window");
      Widget.Set_USize (Widget => Main_Window, Width => 200, Height => 400);
      Widget.Set_UPosition (Widget => Main_Window, X => 20, Y => 20);

      Cb_Id := Window_Callback.Connect (Obj => Main_Window, Name => "destroy",
                                        Func => Exit_Main'Access);
      Cb_Id := Window_Callback.Connect (Obj => Main_Window,
                                        Name => "delete_event",
                                        Func => Exit_Main'Access);

      Vbox.Gtk_New (Widget => Box1, Homogeneous => False, Spacing => 0);
      Container.Add (Container => Main_Window, Widget => Box1);
      Widget.Show (Box1);

      Label.Gtk_New (Label => A_Label,
                     Str => Gtk_Version_Number);
      Widget.Show (A_Label);
      Box.Pack_Start (In_Box => Box1, Child => A_Label,
                      Expand => False, Fill => False);

      Scrolled_Window.Gtk_New (Scrolled_Window => A_Scrolled_Window);
      Container.Border_Width (Container => A_Scrolled_Window,
                              Border_Width => 10);
      Scrolled_Window.Set_Policy
        (Scrolled_Window => A_Scrolled_Window,
         H_Scrollbar_Policy => Enums.Policy_Automatic,
         V_Scrollbar_Policy => Enums.Policy_Automatic);
      Object.Unset_Flags (Object => A_Scrolled_Window,
                          Flags => Widget.Can_Focus);
      Box.Pack_Start (In_Box => Box1, Child => A_Scrolled_Window,
                      Expand => True, Fill => True);
      Widget.Show (A_Scrolled_Window);

      Vbox.Gtk_New (Widget => Box2, Homogeneous => False, Spacing => 0);
      Container.Border_Width (Container => Box2, Border_Width => 10);
      Container.Add (Container => A_Scrolled_Window, Widget => Box2);
      Scrolled_Window.Get_Vadjustement (Scrolled_Window => A_Scrolled_Window,
                                        Vadjustment => Temp);
      Container.Set_Focus_Vadjustment (Container => Box2, Adjustment => Temp);
      Widget.Show (Box2);

      for Index in Buttons'Range loop

         Button.Gtk_New (Widget => A_Button,
                         Label => ASU.To_String (Buttons (Index).Label));
         if Buttons (Index).Cb /= null then
            Cb_Id := Button_Callback.Connect (Obj => A_Button,
                                              Name => "clicked",
                                              Func => Buttons (Index).Cb);
         else
            Widget.Set_Sensitive (Widget => A_Button, Sensitive => False);
         end if;
         Box.Pack_Start (In_Box => Box2, Child => A_Button);
         Widget.Show (A_Button);

      end loop;

      HSeparator.Gtk_New (Separator);
      Box.Pack_Start (In_Box => Box1, Child => Separator, Expand => False);
      Widget.Show (Separator);

      Vbox.Gtk_New (Widget => Box2, Homogeneous => False, Spacing => 10);
      Container.Border_Width (Container => Box2, Border_Width => 10);
      Box.Pack_Start (In_Box => Box1, Child => Box2,
                       Expand => False, Fill => True);
      Widget.Show (Box2);

      Button.Gtk_New (Widget => A_Button, Label => "close");
      Cb_Id := Do_Exit_Callback.Connect (Obj => A_Button, Name => "clicked",
                                        Func => Do_Exit'Access,
                                        Func_Data => Main_Window);
      Box.Pack_Start (In_Box => Box2, Child => A_Button,
                      Expand => True, Fill => True);
      Object.Set_Flags (Object => A_Button, Flags => Widget.Can_Default);
      Widget.Grab_Default (A_Button);
      Widget.Show (A_Button);

      Widget.Show (Main_Window);

   end Create_Main_Window;


   ---------------
   --  Do_Exit  --
   ---------------

   procedure Do_Exit (Widget : in out Gtk.Widget.Gtk_Widget'Class;
                      Data : in out Window.Gtk_Window'Class) is
   begin
      Gtk.Widget.Destroy (Data);
   end Do_Exit;

   -----------------
   --  Exit_Main  --
   -----------------

   procedure Exit_Main (Object : in out Window.Gtk_Window'Class) is
   begin
      Gtk.Main.Main_Quit;
   end Exit_Main;


   --------------------------
   --  Gtk_Version_Number  --
   --------------------------

   function Gtk_Version_Number return String is
      function Image_Of (I : in Guint) return String;
      function Image_Of (I : in Guint) return String is
      begin
         return Ada.Strings.Fixed.Trim (Guint'Image (I), Ada.Strings.Left);
      end Image_Of;
      Temp : constant String := "Gtk+ " & Image_Of (Gtk.Major_Version) & "." &
        Image_Of (Gtk.Minor_Version);
   begin
      if Gtk.Micro_Version > 0 then
         return Temp & "." & Image_Of (Gtk.Micro_Version);
      else
         return Temp;
      end if;
   end Gtk_Version_Number;


   -------------
   --  Start  --
   -------------

   procedure Start is
   begin


      Gtk.Main.Set_Locale;
      Gtk.Main.Init;
      Gtk.Rc.Parse ("testgtkrc");
      Create_Main_Window;
      Gtk.Main.Main;
   end Start;

end Test;

