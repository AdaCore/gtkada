------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Gdk.Types;             use Gdk.Types;
with System;
with Interfaces.C;
with Gtkada.Types;          use Gtkada.Types;

package body Gdk.Main is

   ----------
   -- Init --
   ----------

   procedure Init is
      gnat_argc : Interfaces.C.int;
      pragma Import (C, gnat_argc);

      gnat_argv : System.Address;
      pragma Import (C, gnat_argv);

      procedure Internal (argc : System.Address; argv : System.Address);
      pragma Import (C, Internal, "gdk_init");

   begin
      Internal (gnat_argc'Address, gnat_argv'Address);
   end Init;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display return String is

      function Internal return Chars_Ptr;
      pragma Import (C, Internal, "gdk_get_display");

      Result : constant Chars_Ptr := Internal;

   begin
      if Result = Null_Ptr then
         return "";
      else
         return Value (Internal);
      end if;
   end Get_Display;

   -------------------
   -- Keyboard_Grab --
   -------------------

   function Keyboard_Grab
     (Window       : Gdk.Gdk_Window;
      Owner_Events : Boolean := True;
      Time         : Guint32 := 0) return Gdk_Grab_Status
   is
      function Internal
         (Window       : Gdk_Window;
          Owner_Events : Gint;
          Time         : Guint32) return Gint;
      pragma Import (C, Internal, "gdk_keyboard_grab");

   begin
      return Gdk_Grab_Status'Val
        (Internal (Window, To_Gint (Owner_Events), Time));
   end Keyboard_Grab;

   ---------------------
   -- Keyboard_Ungrab --
   ---------------------

   procedure Keyboard_Ungrab (Time : Guint32 := 0) is
      procedure Internal (Time : Guint32);
      pragma Import (C, Internal, "gdk_keyboard_ungrab");

   begin
      Internal (Time);
   end Keyboard_Ungrab;

   ------------------
   -- Pointer_Grab --
   ------------------

   function Pointer_Grab
     (Window       : Gdk.Gdk_Window;
      Owner_Events : Boolean := True;
      Event_Mask   : Gdk.Event.Gdk_Event_Mask;
      Confine_To   : Gdk.Gdk_Window := null;
      Cursor       : Gdk.Gdk_Cursor := null;
      Time         : Guint32 := 0) return Gdk_Grab_Status
   is
      function Internal
        (Window       : Gdk_Window;
         Owner_Events : Gint;
         Event_Mask   : Gint;
         Confine_To   : Gdk_Window;
         Cursor       : Gdk_Cursor;
         Time         : Guint32) return Gint;
      pragma Import (C, Internal, "gdk_pointer_grab");

   begin
      return Gdk_Grab_Status'Val
        (Internal
          (Window,
           To_Gint (Owner_Events),
           Gint (Event_Mask),
           Confine_To,
           Cursor,
           Time));
   end Pointer_Grab;

   ------------------------
   -- Pointer_Is_Grabbed --
   ------------------------

   function Pointer_Is_Grabbed return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_pointer_is_grabbed");

   begin
      return Internal /= 0;
   end Pointer_Is_Grabbed;

   --------------------
   -- Pointer_Ungrab --
   --------------------

   procedure Pointer_Ungrab (Time : Guint32 := 0) is
      procedure Internal (Time : Guint32);
      pragma Import (C, Internal, "gdk_pointer_ungrab");

   begin
      Internal (Time);
   end Pointer_Ungrab;

end Gdk.Main;
