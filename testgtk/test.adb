with Ada.Strings.Fixed;
with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Box;
with Gtk.Button;
with Gtk.Container;
with Gtk.Label;
with Gtk.Main;
with Gtk.Rc;
with Gtk.Scrolled_Window;
with Gtk.Signal;
with Gtk.Vbox;
with Gtk.Widget;
with Gtk.Window;


with Ada.Text_IO; use Ada.Text_IO;

package body Test is

   procedure Create_Main_Window;
   procedure Exit_Main (Object : in out Window.Gtk_Window'Class);
   function Gtk_Version_Number return String;

   -------------------------------
   --  Callback instantiations  --
   -------------------------------

   package Window_Callback is new Signal.Void_Callback
     (Widget_Type => Window.Gtk_Window);


   --------------------------
   --  Create_Main_Window  --
   --------------------------

   procedure Create_Main_Window is
      Main_Window : Window.Gtk_Window;
      Cb_Id : Guint;
      Box1, Box2 : Vbox.Gtk_VBox;
      A_Label : Label.Gtk_Label;
      A_Scrolled_Window : Scrolled_Window.Gtk_Scrolled_Window;
      Temp : Adjustment.Gtk_Adjustment;
      A_Button : Button.Gtk_Button;
   begin
      Window.Gtk_New (Window => Main_Window,
                      The_Type => Window.Window_Toplevel);
      Widget.Set_Name (Widget => Main_Window, Name => "main window");
      Widget.Set_USize (Widget => Main_Window, Width => 200, Height => 400);
      Widget.Set_UPosition (Widget => Main_Window, X => 20, Y => 20);

      Cb_Id := Window_Callback.Connect (Obj => Main_Window, Name => "destroy",
                                        Func => Exit_Main'Access);
      Cb_Id := Window_Callback.Connect
        (Obj => Main_Window,
         Name => "delete_event",
         Func => Window_Callback.Void_Callback_Procedure'Access);

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
      --  GTK_WIDGET_UNSET_FLAGS (GTK_SCROLLED_WINDOW
      --  (scrolled_window)->vscrollbar, GTK_CAN_FOCUS);
      --  FIXME : Not binded. What is this for???
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

      Vbox.Gtk_New (Widget => Box2, Homogeneous => False, Spacing => 10);
      Container.Border_Width (Container => Box2, Border_Width => 10);
      Box.Pack_Start (In_Box => Box1, Child => Box2,
                       Expand => False, Fill => True);
      Widget.Show (Box2);

      Button.Gtk_New (Widget => A_Button, Label => "close");
      --  Signal_Connect ....
      Box.Pack_Start (In_Box => Box2, Child => A_Button,
                      Expand => True, Fill => True);
      --  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
      Widget.Grab_Default (A_Button);
      Widget.Show (A_Button);

      Widget.Show (Main_Window);

   end Create_Main_Window;


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

