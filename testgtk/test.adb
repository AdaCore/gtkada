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
with Create_Color_Selection;
with Create_File_Selection;

package body Test is

   package ASU renames Ada.Strings.Unbounded;


   type Gtk_Window_Access is access all Window.Gtk_Window'Class;

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

   -------------------------------
   --  Gtk+ Example procedures  --
   -------------------------------

   Main_Window : aliased Window.Gtk_Window;
   Check_Buttons_Window : aliased Window.Gtk_Window;

   procedure Create_Check_Buttons (Widget : in out Button.Gtk_Button'Class);
   procedure Create_Check_Buttons (Widget : in out Button.Gtk_Button'Class)
   is separate;

   ---------------------------
   --  The list of buttons  --
   ---------------------------

   Buttons : constant Buttons_Array :=
     ((ASU.To_Unbounded_String ("button box"), null),
      (ASU.To_Unbounded_String ("buttons"), Create_Buttons.Run'Access),
      (ASU.To_Unbounded_String ("check buttons"), Create_Check_Buttons'Access),
      (ASU.To_Unbounded_String ("clist"), null),
      (ASU.To_Unbounded_String ("color selection"),
       Create_Color_Selection.Run'Access),
      (ASU.To_Unbounded_String ("cursors"), null),
      (ASU.To_Unbounded_String ("dialog"), null),
      (ASU.To_Unbounded_String ("dnd"), null),
      (ASU.To_Unbounded_String ("entry"), null),
      (ASU.To_Unbounded_String ("file selection"),
       Create_File_Selection.Run'Access),
      (ASU.To_Unbounded_String ("gamma curve"), null),
      (ASU.To_Unbounded_String ("handle box"), null),
      (ASU.To_Unbounded_String ("list"), null),
      (ASU.To_Unbounded_String ("menus"), null),
      (ASU.To_Unbounded_String ("miscellaneous"), null),
      (ASU.To_Unbounded_String ("notebook"), null),
      (ASU.To_Unbounded_String ("panes"), null),
      (ASU.To_Unbounded_String ("pixmap"), null),
      (ASU.To_Unbounded_String ("preview color"), null),
      (ASU.To_Unbounded_String ("preview gray"), null),
      (ASU.To_Unbounded_String ("progress bar"), null),
      (ASU.To_Unbounded_String ("radio buttons"), null),
      (ASU.To_Unbounded_String ("range controls"), null),
      (ASU.To_Unbounded_String ("reparent"), null),
      (ASU.To_Unbounded_String ("rulers"), null),
      (ASU.To_Unbounded_String ("scrolled windows"), null),
      (ASU.To_Unbounded_String ("shapes"), null),
      (ASU.To_Unbounded_String ("spinbutton"), null),
      (ASU.To_Unbounded_String ("statusbar"), null),
      (ASU.To_Unbounded_String ("test idle"), null),
      (ASU.To_Unbounded_String ("test mainloop"), null),
      (ASU.To_Unbounded_String ("test scrolling"), null),
      (ASU.To_Unbounded_String ("test selection"), null),
      (ASU.To_Unbounded_String ("test timeout"), null),
      (ASU.To_Unbounded_String ("text"), null),
      (ASU.To_Unbounded_String ("toggle buttons"), null),
      (ASU.To_Unbounded_String ("toolbar"), null),
      (ASU.To_Unbounded_String ("tooltips"), null),
      (ASU.To_Unbounded_String ("tree"), null),
      (ASU.To_Unbounded_String ("WM hints"), null)
      );

   --------------------------
   --  Create_Main_Window  --
   --------------------------

   procedure Create_Main_Window is
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
      Cb_Id := Window_Callback.Connect
        (Obj => Main_Window,
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

