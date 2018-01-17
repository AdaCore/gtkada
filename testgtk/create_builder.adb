------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with System;               use System;
with System.Address_Image;
with Ada.Exceptions;
with Ada.Text_IO;          use Ada.Text_IO;

with Glib;                 use Glib;
with Glib.Error;           use Glib.Error;
with Glib.Object;          use Glib.Object;

with Gtk.Box;              use Gtk.Box;
with Gtk.Builder;          use Gtk.Builder;
with Gtk.Button;           use Gtk.Button;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Handlers;
with Gtk.Text_Buffer;      use Gtk.Text_Buffer;
with Gtk.Text_Iter;        use Gtk.Text_Iter;
with Gtk.Text_View;        use Gtk.Text_View;
with Gtk.Widget;

with Common;               use Common;

package body Create_Builder is

   Default_Filename : constant String := "gtkbuilder_example.xml";
   --  This is the file from which we'll read our UI description.

   procedure On_Button_Clicked
      (Button : access Gtk_Button_Record'Class);
   --  Callback for a button click

   ------------------------------
   -- XML UI Callback Handling --
   ------------------------------

   type Widget_Collection_Record is new Glib.Object.GObject_Record with record
      Term1      : Gtk.GEntry.Gtk_Entry;
      Term2      : Gtk.GEntry.Gtk_Entry;
      Text_Field : Gtk.Text_View.Gtk_Text_View;
   end record;
   type Widget_Collection is access all Widget_Collection_Record'Class;
   --  Type to use as User_Data when connecting signals.

   package Widget_Collection_Cb is new Gtk.Handlers.Callback
     (Widget_Collection_Record);
   package Widget_Collection_Return_Cb is new Gtk.Handlers.Return_Callback
     (Widget_Collection_Record, Boolean);

   type Callback_Function_Name is
     (On_Window1_Delete_Event,
      On_Btn_Concatenate_Clicked,
      On_Window1_Destroy,
      On_Btn_Console_Greeting_Clicked,
      On_Print_To_Console);
   --  An easy way to make sure that all callback functions are referenced
   --  from within the Connect_Signals procedure and that no one has been
   --  overlooked. The names of the callback functions must match the
   --  "handler"s defined on the controls in the UI XML file.

   procedure On_Btn_Concatenate_Clicked
     (Object : access Widget_Collection_Record'Class);
   procedure On_Btn_Console_Greeting_Clicked
     (Object : access Widget_Collection_Record'Class);
   procedure On_Print_To_Console_Clicked
     (Object : access Widget_Collection_Record'Class);
   function On_Window1_Delete_Event
     (Object : access Widget_Collection_Record'Class) return Boolean;
   procedure On_Window1_Destroy
     (Object : access Widget_Collection_Record'Class);
   --  Callbacks referenced by our XML UI definition.  These match the
   --  items in the Callback_Function_Name enumeration.

   type Test_Builder_Record is new Gtk_Builder_Record with record
      Widgets : Widget_Collection;
   end record;
   type Test_Builder is access all Test_Builder_Record'Class;

   procedure Connect_Signals
      (Builder        : not null access Gtk_Builder_Record'Class;
       Object         : not null access Glib.Object.GObject_Record'Class;
       Signal_Name    : Glib.Signal_Name;
       Handler_Name   : UTF8_String;
       Connect_Object : access Glib.Object.GObject_Record'Class;
       Flags          : Glib.G_Connect_Flags);
   --  Subprogram to perform signal connections.

   ---------------------
   -- Connect_Signals --
   ---------------------

   procedure Connect_Signals
      (Builder        : not null access Gtk_Builder_Record'Class;
       Object         : not null access Glib.Object.GObject_Record'Class;
       Signal_Name    : Glib.Signal_Name;
       Handler_Name   : UTF8_String;
       Connect_Object : access Glib.Object.GObject_Record'Class;
       Flags          : Glib.G_Connect_Flags)
   is
      B : constant Test_Builder := Test_Builder (Builder);
      Widget        : constant GObject := GObject (Object);
      After         : constant Boolean := (Flags and G_Connect_After) /= 0;
      Function_Name : constant Callback_Function_Name :=
                        Callback_Function_Name'Value (Handler_Name);
      --  Local translations from our low-level arguments
   begin
      --  Tell the console what we are up to.
      Put_Line ("Connect_Signals callback invoked: ");
      Put_Line ("   object " & System.Address_Image (Object.Get_Object)
                & " emitting " & String (Signal_Name));
      Put_Line ("   to " & Handler_Name & ", with flags:"
                & Glib.G_Connect_Flags'Image (Flags));

      if Connect_Object /= null then
         Put_Line ("   Warning: Connect_Object parameter will be ignored"
                   & "(replaced by Widget_Collection)");
      end if;

      --  Normalize the name of the handler to our Callback_Function_Name
      --  enumeration.
      case Function_Name is
         when On_Btn_Concatenate_Clicked =>
            Widget_Collection_Cb.Object_Connect
              (Widget      => Widget,
               Name        => "clicked",
               Marsh       => Widget_Collection_Cb.To_Marshaller
                                (On_Btn_Concatenate_Clicked'Access),
               Slot_Object => B.Widgets,
               After       => After);

         when On_Window1_Delete_Event =>
            Widget_Collection_Return_Cb.Object_Connect
              (Widget      => Widget,
               Name        => "delete_event",
               Marsh       => Widget_Collection_Return_Cb.To_Marshaller
                                (On_Window1_Delete_Event'Access),
               Slot_Object => B.Widgets,
               After       => After);

         when On_Window1_Destroy =>
            Widget_Collection_Cb.Object_Connect
              (Widget      => Widget,
               Name        => "destroy",
               Marsh       => Widget_Collection_Cb.To_Marshaller
                                (On_Window1_Destroy'Access),
               Slot_Object => B.Widgets,
               After       => After);

         when On_Btn_Console_Greeting_Clicked =>
            Widget_Collection_Cb.Object_Connect
              (Widget      => Widget,
               Name        => "clicked",
               Marsh       => Widget_Collection_Cb.To_Marshaller
                                (On_Btn_Console_Greeting_Clicked'Access),
               Slot_Object => B.Widgets,
               After       => After);

         when On_Print_To_Console =>
            Widget_Collection_Cb.Object_Connect
              (Widget      => Widget,
               Name        => "clicked",
               Marsh       => Widget_Collection_Cb.To_Marshaller
                                (On_Print_To_Console_Clicked'Access),
               Slot_Object => B.Widgets,
               After       => After);

      end case;
   end Connect_Signals;

   -----------------------
   -- On_Button_Clicked --
   -----------------------

   procedure On_Button_Clicked
      (Button : access Gtk_Button_Record'Class)
   is
      pragma Unreferenced (Button);

      Builder1 : Test_Builder;
      Error    : aliased GError;
   begin
      --  Create a new Gtk_Builder object
      Builder1 := new Test_Builder_Record;
      Gtk.Builder.Initialize (Builder1);

      --  Allocate memory for our Widgets_Collection
      Builder1.Widgets := new Widget_Collection_Record;
      Glib.Object.Initialize (Builder1.Widgets);

      --  Read in our XML file
      if Builder1.Add_From_File (Default_Filename, Error'Access) = 0 then
         Put_Line ("Error [Create_Builder.On_Button_Clicked]: "
                   & Get_Message (Error));
         Error_Free (Error);
      end if;

      --  Look up widgets for which we have callbacks, and store the
      --  information in Widget_Collection_Record structure.
      Builder1.Widgets.Term1 :=
         Gtk.GEntry.Gtk_Entry (Builder1.Get_Object ("term1"));
      Builder1.Widgets.Term2 :=
         Gtk.GEntry.Gtk_Entry (Builder1.Get_Object ("term2"));
      Builder1.Widgets.Text_Field := Gtk.Text_View.Gtk_Text_View
        (Builder1.Get_Object ("textField"));

      --  Connect signal handlers
      Builder1.Connect_Signals_Full (Connect_Signals'Access);

      --  Find our main window, then display it and all of its children.
      Gtk.Widget.Gtk_Widget (Builder1.Get_Object ("window1")).Show_All;
   end On_Button_Clicked;

   --------------------------------
   -- On_Btn_Concatenate_Clicked --
   --------------------------------

   procedure On_Btn_Concatenate_Clicked
     (Object : access Widget_Collection_Record'Class)
   is
      Buffer : Gtk_Text_Buffer;
   begin
      Put_Line ("On_Btn_Concatenate_Clicked");

      Buffer := Get_Buffer (Object.Text_Field);
      Insert_At_Cursor
        (Buffer,
         "Concatenated: " &
         Get_Text (Object.Term1) &
         Get_Text (Object.Term2) &
         ASCII.LF);
   exception
      when Event : others =>
         Put_Line ("Error: " & Ada.Exceptions.Exception_Information (Event));
   end On_Btn_Concatenate_Clicked;

   -------------------------------------
   -- On_Btn_Console_Greeting_Clicked --
   -------------------------------------

   procedure On_Btn_Console_Greeting_Clicked
     (Object : access Widget_Collection_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      Put_Line ("On_Btn_Console_Greeting_Clicked says: HELLO!!!");
   end On_Btn_Console_Greeting_Clicked;

   ---------------------------------
   -- On_Print_To_Console_Clicked --
   ---------------------------------

   procedure On_Print_To_Console_Clicked
     (Object : access Widget_Collection_Record'Class)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Object.Text_Field);
      Iter_Start, Iter_End : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Get_Bounds (Buffer, Iter_Start, Iter_End);
      Put_Line ("---[ Buffer Contents ]---");
      Put_Line (Get_Text (Buffer, Iter_Start, Iter_End));
      Put_Line ("---[ End Buffer ]---");
   end On_Print_To_Console_Clicked;

   -----------------------------
   -- On_Window1_Delete_Event --
   -----------------------------

   function On_Window1_Delete_Event
     (Object : access Widget_Collection_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Object);
   begin
      Put_Line ("On_Window1_Delete_Event");

      --  Stop unconditionally.
      return False;
   end On_Window1_Delete_Event;

   ------------------------
   -- On_Window1_Destroy --
   ------------------------

   procedure On_Window1_Destroy
     (Object : access Widget_Collection_Record'Class)
   is
      pragma Unreferenced (Object);
   begin
      --  We actually don't do much here, since within testgtk, we're not
      --  the main window.
      Put_Line ("On_Window1_Destroy");
   end On_Window1_Destroy;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Builder@B is an auxiliary object that reads textual"
        & " descriptions of a user interface and instantiates the described"
        & " objects. To pass a description to a @bGtk_Builder@B, call"
        & " @bAdd_From_File@B or @bAdd_From_String@B. These functions can"
        & " be called multiple times; the builder merges the content of all"
        & " descriptions.";
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

end Create_Builder;
