------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Gtk.Window;
with Gtk.Editable;
with Gdk.Types;
with Glib; use Glib;
with Gtkada.Types; use Gtkada.Types;

package Gnome.Dialog is

   type Gnome_Dialog_Record is new Gtk.Window.Gtk_Window_Record
     with private;
   type Gnome_Dialog is access all Gnome_Dialog_Record'Class;

   procedure Gnome_New
     (Dialog  : out Gnome_Dialog;
      Title   : String;
      Buttons : Chars_Ptr_Array);
   --  Create a new Gnome_Dialog

   procedure Initialize
     (Dialog  : access Gnome_Dialog_Record'Class;
      Title   : String;
      Buttons : Chars_Ptr_Array);
   --  Internal initialization function

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gnome_Dialog

   procedure Append_Button
     (Dialog : access Gnome_Dialog_Record;
      Name   : String);
   --  Append button to dialog (meant for subclasses)

   procedure Append_Button
     (Dialog : access Gnome_Dialog_Record;
      Name   : String;
      Pixmap : String);
   --  Append button with pixmap

   procedure Append_Buttons
     (Dialog  : access Gnome_Dialog_Record;
      Buttons : Chars_Ptr_Array);

   procedure Append_Buttons
     (Dialog  : access Gnome_Dialog_Record;
      Names   : Chars_Ptr_Array;
      Pixmaps : Chars_Ptr_Array);

   --  procedure Button_Connect
   --    (Dialog   : access Gnome_Dialog_Record;
   --     Button   : Gint;
   --     Callback : Gtk_Signal_Func;
   --     Data     : gpointer);

   --  procedure Button_Connect_Object
   --    (Dialog   : access Gnome_Dialog_Record;
   --     Button   : Gint;
   --     Callback : Gtk_Signal_Func;
   --     Obj      : access Object_Record'Class);

   procedure Close (Dialog : access Gnome_Dialog_Record);
   --  Close this dialog

   procedure Close_Hides
     (Dialog    : access Gnome_Dialog_Record;
      Just_Hide : Boolean);
   --  Set whether close hides or destroys

   procedure Editable_Enters
     (Dialog   : access Gnome_Dialog_Record;
      Editable : Gtk.Editable.Gtk_Editable);
   --  Connect activate signal from editable to return

   procedure Grab_Focus
     (Dialog : access Gnome_Dialog_Record;
      Button : Gint);

   function Run (Dialog : access Gnome_Dialog_Record) return Gint;

   function Run_And_Close (Dialog : access Gnome_Dialog_Record) return Gint;
   --  Runs this dialog box, and return button # pressed

   procedure Set_Accelerator
     (Dialog     : access Gnome_Dialog_Record;
      Button     : Gint;
      Accel_Key  : Gdk.Types.Gdk_Key_Type;
      Accel_Mods : Gdk.Types.Gdk_Modifier_Type);
   --  Set accelerators for a button

   procedure Set_Close
     (Dialog       : access Gnome_Dialog_Record;
      Click_Closes : Boolean);
   --  Whether to close after emitting clicked signal

   procedure Set_Default
     (Dialog : access Gnome_Dialog_Record;
      Button :        Gint);
   --  Set the default dialog button

   procedure Set_Parent
     (Dialog : access Gnome_Dialog_Record;
      Parent : access Gtk.Window.Gtk_Window_Record'Class);
   --  Set the parent over which this dialog can be centered.

   procedure Set_Sensitive
     (Dialog  : access Gnome_Dialog_Record;
      Button  : Gint;
      Setting : Boolean);
   --  Set button sensitivity

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler
   --      (Widget : access Gnome_Dialog_Record'Class;
   --       Button_Number : Gint)
   --
   --  - "close"
   --    function Handler
   --      (Widget : access Gnome_Dialog_Record'Class) return Boolean
   --
   --  </signals>

private
   type Gnome_Dialog_Record is new Gtk.Window.Gtk_Window_Record
     with null record;
   pragma Import (C, Get_Type, "gnome_dialog_get_type");
end Gnome.Dialog;
