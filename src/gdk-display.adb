------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Gdk.Event;            use Gdk.Event;
with Gdk.Types;            use Gdk.Types;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

package body Gdk.Display is

   ----------
   -- Beep --
   ----------

   procedure Beep (Display : access Gdk_Display_Record) is
      procedure Internal (Display : System.Address);
      pragma Import (C, Internal, "gdk_display_beep");
   begin
      Internal (Get_Object (Display));
   end Beep;

   -----------
   -- Close --
   -----------

   procedure Close (Display : access Gdk_Display_Record) is
      procedure Internal (Display : System.Address);
      pragma Import (C, Internal, "gdk_display_close");
   begin
      Internal (Get_Object (Display));
   end Close;

   -----------
   -- Flush --
   -----------

   procedure Flush (Display : access Gdk_Display_Record) is
      procedure Internal (Display : System.Address);
      pragma Import (C, Internal, "gdk_display_flush");
   begin
      Internal (Get_Object (Display));
   end Flush;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gdk_Display is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_display_get_default");
      Stub : Gdk_Display_Record;
   begin
      return Gdk_Display (Get_User_Data (Internal, Stub));
   end Get_Default;

   -----------------------------
   -- Get_Default_Cursor_Size --
   -----------------------------

   function Get_Default_Cursor_Size
     (Display : access Gdk_Display_Record)
      return Guint
   is
      function Internal (Display : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_display_get_default_cursor_size");
   begin
      return Internal (Get_Object (Display));
   end Get_Default_Cursor_Size;

   -----------------------
   -- Get_Default_Group --
   -----------------------

   function Get_Default_Group
     (Display : access Gdk_Display_Record) return Gdk_Window
   is
      function Internal (Display : System.Address) return Gdk_Window;
      pragma Import (C, Internal, "gdk_display_get_default_group");
   begin
      return Internal (Get_Object (Display));
   end Get_Default_Group;

   ---------------
   -- Get_Event --
   ---------------

   function Get_Event (Display : access Gdk_Display_Record) return Gdk_Event is
      function Internal (Display : System.Address) return Gdk_Event;
      pragma Import (C, Internal, "gdk_display_get_event");
   begin
      return Internal (Get_Object (Display));
   end Get_Event;

   -----------------------------
   -- Get_Maximal_Cursor_Size --
   -----------------------------

   procedure Get_Maximal_Cursor_Size
     (Display : access Gdk_Display_Record;
      Width   : out Guint;
      Height  : out Guint)
   is
      procedure Internal
        (Display : System.Address;
         Width   : out Guint;
         Height  : out Guint);
      pragma Import (C, Internal, "gdk_display_get_maximal_cursor_size");
   begin
      Internal (Get_Object (Display), Width, Height);
   end Get_Maximal_Cursor_Size;

   -------------------
   -- Get_N_Screens --
   -------------------

   function Get_N_Screens (Display : access Gdk_Display_Record) return Gint is
      function Internal (Display : System.Address) return Gint;
      pragma Import (C, Internal, "gdk_display_get_n_screens");
   begin
      return Internal (Get_Object (Display));
   end Get_N_Screens;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Display : access Gdk_Display_Record) return String
   is
      function Internal (Display : System.Address) return chars_ptr;
      pragma Import (C, Internal, "gdk_display_get_name");

      S : constant chars_ptr := Internal (Get_Object (Display));
   begin
      if S = Null_Ptr then
         return "";
      else
         --  S is owned by gtk+, do not free
         return Value (S);
      end if;
   end Get_Name;

   ---------------------------
   -- Get_Window_At_Pointer --
   ---------------------------

   procedure Get_Window_At_Pointer
     (Display : access Gdk_Display_Record;
      Win_X   : out Gint;
      Win_Y   : out Gint;
      Win     : out Gdk_Window)
   is
      function Internal
        (Display : System.Address;
         Win_X   : access Gint;
         Win_Y   : access Gint) return Gdk_Window;
      pragma Import (C, Internal, "gdk_display_get_window_at_pointer");
      X, Y : aliased Gint;
   begin
      Win   := Internal (Get_Object (Display), X'Access, Y'Access);
      Win_X := X;
      Win_Y := Y;
   end Get_Window_At_Pointer;

   ---------------------
   -- Keyboard_Ungrab --
   ---------------------

   procedure Keyboard_Ungrab
     (Display : access Gdk_Display_Record;
      Time    : Guint32 := Current_Time)
   is
      procedure Internal (Display : System.Address; Time : Guint32);
      pragma Import (C, Internal, "gdk_display_keyboard_ungrab");
   begin
      Internal (Get_Object (Display), Time);
   end Keyboard_Ungrab;

   ----------
   -- Open --
   ----------

   function Open (Display_Name : String) return Gdk_Display
   is
      function Internal (Display_Name : String) return System.Address;
      pragma Import (C, Internal, "gdk_display_open");
      Stub : Gdk_Display_Record;
   begin
      return Gdk_Display
        (Get_User_Data (Internal (Display_Name & ASCII.NUL), Stub));
   end Open;

   ----------------
   -- Peek_Event --
   ----------------

   function Peek_Event
     (Display : access Gdk_Display_Record)
      return Gdk_Event
   is
      function Internal (Display : System.Address) return Gdk_Event;
      pragma Import (C, Internal, "gdk_display_peek_event");
   begin
      return Internal (Get_Object (Display));
   end Peek_Event;

   ------------------------
   -- Pointer_Is_Grabbed --
   ------------------------

   function Pointer_Is_Grabbed
     (Display : access Gdk_Display_Record)
      return Boolean
   is
      function Internal (Display : System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_display_pointer_is_grabbed");
   begin
      return Boolean'Val (Internal (Get_Object (Display)));
   end Pointer_Is_Grabbed;

   --------------------
   -- Pointer_Ungrab --
   --------------------

   procedure Pointer_Ungrab
     (Display : access Gdk_Display_Record;
      Time    : Guint32 := Current_Time)
   is
      procedure Internal (Display : System.Address; Time : Guint32);
      pragma Import (C, Internal, "gdk_display_pointer_ungrab");
   begin
      Internal (Get_Object (Display), Time);
   end Pointer_Ungrab;

   ---------------
   -- Put_Event --
   ---------------

   procedure Put_Event
     (Display : access Gdk_Display_Record;
      Event   : Gdk_Event)
   is
      procedure Internal (Display : System.Address; Event : Gdk_Event);
      pragma Import (C, Internal, "gdk_display_put_event");
   begin
      Internal (Get_Object (Display), Event);
   end Put_Event;

   ------------------------------------
   -- Request_Selection_Notification --
   ------------------------------------

   function Request_Selection_Notification
     (Display   : access Gdk_Display_Record;
      Selection : Gdk_Atom)
      return Boolean
   is
      function Internal
        (Display   : System.Address;
         Selection : Gdk_Atom)
         return Gboolean;
      pragma Import
        (C, Internal, "gdk_display_request_selection_notification");
   begin
      return Boolean'Val (Internal (Get_Object (Display), Selection));
   end Request_Selection_Notification;

   -------------------------------
   -- Set_Double_Click_Distance --
   -------------------------------

   procedure Set_Double_Click_Distance
     (Display  : access Gdk_Display_Record;
      Distance : Guint)
   is
      procedure Internal (Display  : System.Address; Distance : Guint);
      pragma Import (C, Internal, "gdk_display_set_double_click_distance");
   begin
      Internal (Get_Object (Display), Distance);
   end Set_Double_Click_Distance;

   ---------------------------
   -- Set_Double_Click_Time --
   ---------------------------

   procedure Set_Double_Click_Time
     (Display : access Gdk_Display_Record;
      Msec    : Guint)
   is
      procedure Internal (Display : System.Address; Msec : Guint);
      pragma Import (C, Internal, "gdk_display_set_double_click_time");
   begin
      Internal (Get_Object (Display), Msec);
   end Set_Double_Click_Time;

   ---------------------
   -- Store_Clipboard --
   ---------------------

   procedure Store_Clipboard
     (Display          : access Gdk_Display_Record;
      Clipboard_Window : Gdk_Window;
      Time             : Guint32;
      Targets          : Gdk.Types.Gdk_Atom_Array)
   is
      procedure Internal
        (Display          : System.Address;
         Clipboard_Window : Gdk_Window;
         Time             : Guint32;
         Targets          : System.Address;
         N_Targets        : Gint);
      pragma Import (C, Internal, "gdk_display_store_clipboard");
   begin
      Internal (Get_Object (Display), Clipboard_Window, Time,
                Targets (Targets'First)'Address, Targets'Length);
   end Store_Clipboard;

   ------------------------------------
   -- Supports_Clipboard_Persistence --
   ------------------------------------

   function Supports_Clipboard_Persistence
     (Display : access Gdk_Display_Record)
      return Boolean
   is
      function Internal (Display : System.Address) return Gboolean;
      pragma Import
        (C, Internal, "gdk_display_supports_clipboard_persistence");
   begin
      return Boolean'Val (Internal (Get_Object (Display)));
   end Supports_Clipboard_Persistence;

   ---------------------------
   -- Supports_Cursor_Alpha --
   ---------------------------

   function Supports_Cursor_Alpha
     (Display : access Gdk_Display_Record)
      return Boolean
   is
      function Internal (Display : System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_cursor_alpha");
   begin
      return Boolean'Val (Internal (Get_Object (Display)));
   end Supports_Cursor_Alpha;

   ---------------------------
   -- Supports_Cursor_Color --
   ---------------------------

   function Supports_Cursor_Color
     (Display : access Gdk_Display_Record)
      return Boolean
   is
      function Internal (Display : System.Address) return Gboolean;
      pragma Import (C, Internal, "gdk_display_supports_cursor_color");
   begin
      return Boolean'Val (Internal (Get_Object (Display)));
   end Supports_Cursor_Color;

   -------------------------------------
   -- Supports_Selection_Notification --
   -------------------------------------

   function Supports_Selection_Notification
     (Display : access Gdk_Display_Record)
      return Boolean
   is
      function Internal (Display : System.Address) return Gboolean;
      pragma Import
        (C, Internal, "gdk_display_supports_selection_notification");
   begin
      return Boolean'Val (Internal (Get_Object (Display)));
   end Supports_Selection_Notification;

   ----------
   -- Sync --
   ----------

   procedure Sync (Display : access Gdk_Display_Record) is
      procedure Internal (Display : System.Address);
      pragma Import (C, Internal, "gdk_display_sync");
   begin
      Internal (Get_Object (Display));
   end Sync;

end Gdk.Display;
