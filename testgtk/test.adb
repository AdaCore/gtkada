-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Glib; use Glib;
with Gtk; use Gtk;
with Gtk.Adjustment;
with Gtk.Box;
with Gtk.Button;
with Gtk.Container;
with Gtk.Enums;
with Gtk.Separator;
with Gtk.Label;
with Gtk.Main;
with Gtk.Object;
with Gtk.Rc;
with Gtk.Scrolled_Window;
with Gtk.Signal;
with Gtk.Widget;
with Gtk.Window;

with Create_Button_Box;
with Create_Buttons;
with Create_Check_Buttons;
with Create_Clist;
with Create_Color_Selection;
with Create_Cursors;
with Create_Dialog;
with Create_Entry;
with Create_File_Selection;
with Create_Gamma_Curve;
with Create_Handle_Box;
with Create_List;
with Create_Main_Loop;
with Create_Menu;
with Create_Notebook;
with Create_Paned;
with Create_Pixmap;
with Create_Preview_Color;
with Create_Preview_Gray;
with Create_Progress;
with Create_Radio_Button;
with Create_Range;
with Create_Reparent;
with Create_Rulers;
with Create_Scroll_Test;
with Create_Scrolled;
with Create_Spin;
with Create_Status;
with Create_Test_Idle;
with Create_Test_Timeout;
with Create_Text;
with Create_Toggle_Buttons;
with Create_Toolbar;
with Create_Tooltips;
with Create_Tree;

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
      (US ("clist"), Create_Clist.Run'Access),
      (US ("color selection"), Create_Color_Selection.Run'Access),
      (US ("cursors"), Create_Cursors.Run'Access),
      (US ("dialog"), Create_Dialog.Run'Access),
      (US ("dnd"), null),
      (US ("entry"), Create_Entry.Run'Access),
      (US ("file selection"), Create_File_Selection.Run'Access),
      (US ("gamma curve"), Create_Gamma_Curve.Run'Access),
      (US ("handle box"), Create_Handle_Box.Run'Access),
      (US ("list"), Create_List.Run'Access),
      (US ("menus"), Create_Menu.Run'Access),
      (US ("notebook"), Create_Notebook.Run'Access),
      (US ("panes"), Create_Paned.Run'Access),
      (US ("pixmap"), Create_Pixmap.Run'Access),
      (US ("preview color"), Create_Preview_Color.Run'Access),
      (US ("preview gray"), Create_Preview_Gray.Run'Access),
      (US ("progress bar"), Create_Progress.Run'Access),
      (US ("radio buttons"), Create_Radio_Button.Run'Access),
      (US ("range controls"), Create_Range.Run'Access),
      (US ("reparent"), Create_Reparent.Run'Access),
      (US ("rulers"), Create_Rulers.Run'Access),
      (US ("scrolled windows"), Create_Scrolled.Run'Access),
      (US ("shapes"), null),
      (US ("spinbutton"), Create_Spin.Run'Access),
      (US ("statusbar"), Create_Status.Run'Access),
      (US ("test idle"), Create_Test_Idle.Run'Access),
      (US ("test mainloop"), Create_Main_Loop.Run'Access),
      (US ("test scrolling"), Create_Scroll_Test.Run'Access),
      (US ("test selection"), null),
      (US ("test timeout"), Create_Test_Timeout.Run'Access),
      (US ("text"), Create_Text.Run'Access),
      (US ("toggle buttons"), Create_Toggle_Buttons.Run'Access),
      (US ("toolbar"), Create_Toolbar.Run'Access),
      (US ("tooltips"), Create_Tooltips.Run'Access),
      (US ("tree"), Create_Tree.Run'Access),
      (US ("WM hints"), null)
      );

   --------------------------
   --  Create_Main_Window  --
   --------------------------

   procedure Create_Main_Window is
      Main_Window : Window.Gtk_Window;
      Cb_Id : Guint;
      Box1, Box2 : Box.Gtk_Box;
      A_Label : Label.Gtk_Label;
      A_Scrolled_Window : Scrolled_Window.Gtk_Scrolled_Window;
      Temp : Adjustment.Gtk_Adjustment;
      A_Button : Button.Gtk_Button;
      Separator : Gtk.Separator.Gtk_Separator;
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

      Box.Gtk_New_Vbox (Widget => Box1, Homogeneous => False, Spacing => 0);
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

      Box.Gtk_New_Vbox (Widget => Box2, Homogeneous => False, Spacing => 0);
      Container.Border_Width (Container => Box2, Border_Width => 10);
      Container.Add (Container => A_Scrolled_Window, Widget => Box2);
      Temp := Scrolled_Window.Get_Vadjustment (Scrolled_Window => A_Scrolled_Window);
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

      Gtk.Separator.Gtk_New_Hseparator (Separator);
      Box.Pack_Start (In_Box => Box1, Child => Separator, Expand => False);
      Widget.Show (Separator);

      Box.Gtk_New_Vbox (Widget => Box2, Homogeneous => False, Spacing => 10);
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
      Temp : constant String := "(Ada95)Gtk " & Image_Of (Gtk.Major_Version) & "." &
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

