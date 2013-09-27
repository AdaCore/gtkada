------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

pragma Ada_2005;


pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;       use GNAT.Strings;
with Gdk.Display;        use Gdk.Display;
with Gdk.Event;          use Gdk.Event;
with Gdk.Pixbuf;         use Gdk.Pixbuf;
with Gdk.Types;          use Gdk.Types;
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Gtk.Selection_Data; use Gtk.Selection_Data;
with Gtk.Target_List;    use Gtk.Target_List;

package Gtk.Clipboard is

   type Gtk_Clipboard_Record is new GObject_Record with null record;
   type Gtk_Clipboard is access all Gtk_Clipboard_Record'Class;

   ---------------
   -- Callbacks --
   ---------------

   type Gtk_Clipboard_Received_Func is access procedure
     (Clipboard      : not null access Gtk_Clipboard_Record'Class;
      Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data);
   --  A function to be called when the results of
   --  Gtk.Clipboard.Request_Contents are received, or when the request fails.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data containing
   --  the data was received. If retrieving the data failed, then then length
   --  field of Selection_Data will be negative.

   type Gtk_Clipboard_Image_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  A function to be called when the results of Gtk.Clipboard.Request_Image
   --  are received, or when the request fails.
   --  Since: gtk+ 2.6
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "pixbuf": the received image

   type Gtk_Clipboard_Rich_Text_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Format    : Gdk.Types.Gdk_Atom;
      Text      : in out Guint8;
      Length    : Gsize);

   type Gtk_Clipboard_Targets_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Atoms     : in out Gdk.Types.Gdk_Atom;
      N_Atoms   : Gint);
   --  A function to be called when the results of
   --  Gtk.Clipboard.Request_Targets are received, or when the request fails.
   --  Since: gtk+ 2.4
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "atoms": the supported targets, as array of Gdk.Types.Gdk_Atom, or null
   --  if retrieving the data failed.
   --  "n_atoms": the length of the Atoms array.

   type Gtk_Clipboard_Text_Received_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Text      : UTF8_String);
   --  A function to be called when the results of Gtk.Clipboard.Request_Text
   --  are received, or when the request fails.
   --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
   --  "text": the text received, as a UTF-8 encoded string, or null if
   --  retrieving the data failed.

   type Gtk_Clipboard_Urireceived_Func is access procedure
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Uris      : GNAT.Strings.String_List);

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_clipboard_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear (Clipboard : not null access Gtk_Clipboard_Record);

   function Get_Display
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Display.Gdk_Display;

   function Get_Owner
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Glib.Object.GObject;

   procedure Request_Contents
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom;
       Callback  : Gtk_Clipboard_Received_Func);

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Request_Contents_User_Data is

      type Gtk_Clipboard_Received_Func is access procedure
        (Clipboard      : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
         Selection_Data : Gtk.Selection_Data.Gtk_Selection_Data;
         Data           : User_Data_Type);
      --  A function to be called when the results of
      --  Gtk.Clipboard.Request_Contents are received, or when the request fails.
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "selection_data": a Gtk.Selection_Data.Gtk_Selection_Data containing
      --  the data was received. If retrieving the data failed, then then length
      --  field of Selection_Data will be negative.
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Contents.

      procedure Request_Contents
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Target    : Gdk.Types.Gdk_Atom;
          Callback  : Gtk_Clipboard_Received_Func;
          User_Data : User_Data_Type);

   end Request_Contents_User_Data;

   procedure Request_Image
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Image_Received_Func);

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Request_Image_User_Data is

      type Gtk_Clipboard_Image_Received_Func is access procedure
        (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
         Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
         Data      : User_Data_Type);
      --  A function to be called when the results of Gtk.Clipboard.Request_Image
      --  are received, or when the request fails.
      --  Since: gtk+ 2.6
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "pixbuf": the received image
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Image.

      procedure Request_Image
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Image_Received_Func;
          User_Data : User_Data_Type);

   end Request_Image_User_Data;

   procedure Request_Rich_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Buffer    : not null access Glib.Object.GObject_Record'Class;
       Callback  : Gtk_Clipboard_Rich_Text_Received_Func);

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Request_Rich_Text_User_Data is

      type Gtk_Clipboard_Rich_Text_Received_Func is access procedure
        (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
         Format    : Gdk.Types.Gdk_Atom;
         Text      : in out Guint8;
         Length    : Gsize;
         Data      : User_Data_Type);

      procedure Request_Rich_Text
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Buffer    : not null access Glib.Object.GObject_Record'Class;
          Callback  : Gtk_Clipboard_Rich_Text_Received_Func;
          User_Data : User_Data_Type);

   end Request_Rich_Text_User_Data;

   procedure Request_Targets
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Targets_Received_Func);

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Request_Targets_User_Data is

      type Gtk_Clipboard_Targets_Received_Func is access procedure
        (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
         Atoms     : in out Gdk.Types.Gdk_Atom;
         N_Atoms   : Gint;
         Data      : User_Data_Type);
      --  A function to be called when the results of
      --  Gtk.Clipboard.Request_Targets are received, or when the request fails.
      --  Since: gtk+ 2.4
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "atoms": the supported targets, as array of Gdk.Types.Gdk_Atom, or null
      --  if retrieving the data failed.
      --  "n_atoms": the length of the Atoms array.
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Targets.

      procedure Request_Targets
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Targets_Received_Func;
          User_Data : User_Data_Type);

   end Request_Targets_User_Data;

   procedure Request_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Text_Received_Func);

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Request_Text_User_Data is

      type Gtk_Clipboard_Text_Received_Func is access procedure
        (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
         Text      : UTF8_String;
         Data      : User_Data_Type);
      --  A function to be called when the results of Gtk.Clipboard.Request_Text
      --  are received, or when the request fails.
      --  "clipboard": the Gtk.Clipboard.Gtk_Clipboard
      --  "text": the text received, as a UTF-8 encoded string, or null if
      --  retrieving the data failed.
      --  "data": the User_Data supplied to Gtk.Clipboard.Request_Text.

      procedure Request_Text
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Text_Received_Func;
          User_Data : User_Data_Type);

   end Request_Text_User_Data;

   procedure Request_Uris
      (Clipboard : not null access Gtk_Clipboard_Record;
       Callback  : Gtk_Clipboard_Urireceived_Func);

   generic
      type User_Data_Type (<>) is private;
      with procedure Destroy (Data : in out User_Data_Type) is null;
   package Request_Uris_User_Data is

      type Gtk_Clipboard_Urireceived_Func is access procedure
        (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
         Uris      : GNAT.Strings.String_List;
         Data      : User_Data_Type);

      procedure Request_Uris
         (Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
          Callback  : Gtk_Clipboard_Urireceived_Func;
          User_Data : User_Data_Type);

   end Request_Uris_User_Data;

   procedure Set_Can_Store
      (Clipboard : not null access Gtk_Clipboard_Record;
       Targets   : Gtk.Target_List.Target_Entry_Array;
       N_Targets : Gint);

   procedure Set_Image
      (Clipboard : not null access Gtk_Clipboard_Record;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);

   procedure Set_Text
      (Clipboard : not null access Gtk_Clipboard_Record;
       Text      : UTF8_String);

   procedure Store (Clipboard : not null access Gtk_Clipboard_Record);

   function Wait_For_Contents
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom)
       return Gtk.Selection_Data.Gtk_Selection_Data;

   function Wait_For_Image
      (Clipboard : not null access Gtk_Clipboard_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;

   function Wait_For_Text
      (Clipboard : not null access Gtk_Clipboard_Record) return UTF8_String;

   function Wait_For_Uris
      (Clipboard : not null access Gtk_Clipboard_Record)
       return GNAT.Strings.String_List;

   function Wait_Is_Image_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean;

   function Wait_Is_Rich_Text_Available
      (Clipboard : not null access Gtk_Clipboard_Record;
       Buffer    : not null access Glib.Object.GObject_Record'Class)
       return Boolean;

   function Wait_Is_Target_Available
      (Clipboard : not null access Gtk_Clipboard_Record;
       Target    : Gdk.Types.Gdk_Atom) return Boolean;

   function Wait_Is_Text_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean;

   function Wait_Is_Uris_Available
      (Clipboard : not null access Gtk_Clipboard_Record) return Boolean;

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Wait_For_Targets
     (Clipboard : not null access Gtk_Clipboard_Record)
   return Gdk.Types.Gdk_Atom_Array;
   --  Returns a list of targets that are present on the clipboard, or an empty
   --  array if there aren't any targets available.
   --  This function waits for the data to be received using the main
   --  loop, so events, timeouts, etc, may be dispatched during the wait.

   ---------------
   -- Functions --
   ---------------

   function Get
      (Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
       return Gtk_Clipboard;

   function Get_For_Display
      (Display   : not null access Gdk.Display.Gdk_Display_Record'Class;
       Selection : Gdk.Types.Gdk_Atom := Gdk.Types.Gdk_None)
       return Gtk_Clipboard;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Clipboard_Gdk_Event_Void is not null access procedure
     (Self   : access Gtk_Clipboard_Record'Class;
      Object : Gdk.Event.Gdk_Event);

   type Cb_GObject_Gdk_Event_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Gdk.Event.Gdk_Event);

   Signal_Owner_Change : constant Glib.Signal_Name := "owner-change";
   procedure On_Owner_Change
      (Self  : not null access Gtk_Clipboard_Record;
       Call  : Cb_Gtk_Clipboard_Gdk_Event_Void;
       After : Boolean := False);
   procedure On_Owner_Change
      (Self  : not null access Gtk_Clipboard_Record;
       Call  : Cb_GObject_Gdk_Event_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

end Gtk.Clipboard;
