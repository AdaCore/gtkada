------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  with System;               use System;
--  with System.Address_Image;
with Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;

with Glib;                 use Glib;
with Glib.Error;           use Glib.Error;
with Glib.Object;          use Glib.Object;

with Gtk.Box;              use Gtk.Box;
with Gtk.Button;           use Gtk.Button;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Frame;            use Gtk.Frame;

with Gtk.Text_Buffer;      use Gtk.Text_Buffer;
with Gtk.Text_View;        use Gtk.Text_View;
with Gtk.Widget;

with Common;               use Common;

with Gtkada.Builder;       use Gtkada.Builder;

package body Create_Gtkada_Builder is

   Default_Filename : constant String := "gtkbuilder_example.xml";
   --  This is the file from which we'll read our UI description.

   procedure On_Button_Clicked
      (Button : access Gtk_Button_Record'Class);
   --  Callback for a button click

   procedure On_Btn_Concatenate_Clicked
     (Builder : access Gtkada_Builder_Record'Class);
   procedure On_Btn_Console_Greeting_Clicked
     (Builder : access Gtkada_Builder_Record'Class);
   function On_Window1_Delete_Event
     (Builder : access Gtkada_Builder_Record'Class) return Boolean;
   procedure On_Window1_Destroy
     (Builder : access Gtkada_Builder_Record'Class);

   procedure On_Print_To_Console
      (Object : access GObject_Record'Class);
   --  Callbacks referenced by our XML UI definition.  These match the
   --  items in the Callback_Function_Name enumeration.

   procedure Add_Custom_Widget (Box : Gtk_Hbox);
   --  Add custom widgets from the file "gtkbuilder_custom_widget.xml"
   --  to the given hbox

   -----------------------
   -- Add_Custom_Widget --
   -----------------------

   procedure Add_Custom_Widget (Box : Gtk_Hbox) is
      Builder : Gtkada_Builder;
      Error   : aliased GError;
   begin
      --  Create a builder
      Gtk_New (Builder);

      --  Load the custom widget from the XML description
      if Builder.Add_From_File
         ("gtkbuilder_custom_widget.xml", Error'Access) = 0
      then
         Put_Line ("Error [Create_Builder.Add_Custom_Widget]: "
                   & Get_Message (Error));
         Error_Free (Error);
         return;
      end if;

      --  Now get the widget...
      declare
         Custom_Widget : constant Gtk.Widget.Gtk_Widget :=
           Gtk.Widget.Gtk_Widget (Get_Object (Builder, "custom"));
      begin
         --  ... And add it to our Box
         Pack_Start (Box, Custom_Widget);
         Gtk.Widget.Show_All (Custom_Widget);
      end;
   end Add_Custom_Widget;

   -------------------------
   -- On_Print_To_Console --
   -------------------------

   procedure On_Print_To_Console
      (Object : access GObject_Record'Class)
   is
      Term1 : constant Gtk_Entry := Gtk_Entry (Object);
   begin
      Put_Line ("String 1 is: " & Get_Text (Term1));
   end On_Print_To_Console;

   -----------------------
   -- On_Button_Clicked --
   -----------------------

   procedure On_Button_Clicked
      (Button : access Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);

      Builder : Gtkada_Builder;
      Error   : aliased GError;
   begin
      --  Create a new Gtkada_Builder object
      Gtk_New (Builder);

      --  Read in our XML file
      if Builder.Add_From_File (Default_Filename, Error'Access) = 0 then
         Put_Line ("Error [Create_Builder.On_Button_Clicked]: "
                   & Get_Message (Error));
         Error_Free (Error);
      end if;

      --  Do the necessary connections

      Register_Handler
        (Builder      => Builder,
         Handler_Name => "on_btn_concatenate_clicked",
         Handler      => On_Btn_Concatenate_Clicked'Access);

      Register_Handler
        (Builder      => Builder,
         Handler_Name => "on_btn_console_greeting_clicked",
         Handler      => On_Btn_Console_Greeting_Clicked'Access);

      Register_Handler
        (Builder      => Builder,
         Handler_Name => "on_window1_delete_event",
         Handler      => On_Window1_Delete_Event'Access);

      Register_Handler
        (Builder      => Builder,
         Handler_Name => "on_window1_destroy",
         Handler      => On_Window1_Destroy'Access);

      Register_Handler
        (Builder      => Builder,
         Handler_Name => "on_print_to_console",
         Handler      => On_Print_To_Console'Access);

      Do_Connect (Builder);

      --  Add a custom widget 3 times to the placeholder hbox
      Add_Custom_Widget (Gtk_Hbox (Builder.Get_Object ("placeholder_hbox")));
      Add_Custom_Widget (Gtk_Hbox (Builder.Get_Object ("placeholder_hbox")));
      Add_Custom_Widget (Gtk_Hbox (Builder.Get_Object ("placeholder_hbox")));

      --  Find our main window, then display it and all of its children.
      Gtk.Widget.Gtk_Widget (Builder.Get_Object ("window1")).Show_All;
   end On_Button_Clicked;

   --------------------------------
   -- On_Btn_Concatenate_Clicked --
   --------------------------------

   procedure On_Btn_Concatenate_Clicked
     (Builder : access Gtkada_Builder_Record'Class)
   is
      Buffer  : constant Gtk_Text_Buffer := Get_Buffer
        (Gtk.Text_View.Gtk_Text_View (Builder.Get_Object ("textField")));
   begin
      Put_Line ("On_Btn_Concatenate_Clicked");

      Insert_At_Cursor
        (Buffer,
         "Concatenated: " &
         Get_Text (Gtk.GEntry.Gtk_Entry (Builder.Get_Object ("term1"))) &
         Get_Text (Gtk.GEntry.Gtk_Entry (Builder.Get_Object ("term2"))) &
         ASCII.LF);
   exception
      when Event : others =>
         Put_Line ("Error: " & Ada.Exceptions.Exception_Information (Event));
   end On_Btn_Concatenate_Clicked;

   -------------------------------------
   -- On_Btn_Console_Greeting_Clicked --
   -------------------------------------

   procedure On_Btn_Console_Greeting_Clicked
     (Builder : access Gtkada_Builder_Record'Class)
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("On_Btn_Console_Greeting_Clicked says: HELLO!!!");
   end On_Btn_Console_Greeting_Clicked;

   -----------------------------
   -- On_Window1_Delete_Event --
   -----------------------------

   function On_Window1_Delete_Event
     (Builder : access Gtkada_Builder_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Builder);
   begin
      Put_Line ("On_Window1_Delete_Event");

      --  Stop unconditionally.
      return False;
   end On_Window1_Delete_Event;

   ------------------------
   -- On_Window1_Destroy --
   ------------------------

   procedure On_Window1_Destroy
     (Builder : access Gtkada_Builder_Record'Class)
   is
   begin
      --  We actually don't do much here, since within testgtk, we're not
      --  the main window.
      Put_Line ("On_Window1_Destroy");

      --  Free memory associated to the builder
      Unref (Builder);
   end On_Window1_Destroy;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1    : Gtk_Box;
      Button1 : Gtk_Button;
   begin
      Set_Label (Frame, "Builder");

      Gtk_New_Vbox (Box1, False, 0);
      Add (Frame, Box1);

      Gtk_New (Button1, "Invoke Builder with file " & Default_Filename);
      Button_Handler.Connect (Button1, "clicked", On_Button_Clicked'Access);
      Pack_Start
        (Box1, Button1, Expand => False, Fill => False, Padding => 10);

      Show_All (Frame);
   end Run;

end Create_Gtkada_Builder;
