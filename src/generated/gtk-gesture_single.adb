------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Gesture_Single is

   package Type_Conversion_Gtk_Gesture_Single is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Gesture_Single_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Gesture_Single);

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button
      (Self : not null access Gtk_Gesture_Single_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_gesture_single_get_button");
   begin
      return Internal (Get_Object (Self));
   end Get_Button;

   ------------------------
   -- Get_Current_Button --
   ------------------------

   function Get_Current_Button
      (Self : not null access Gtk_Gesture_Single_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_gesture_single_get_current_button");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Button;

   --------------------------
   -- Get_Current_Sequence --
   --------------------------

   function Get_Current_Sequence
      (Self : not null access Gtk_Gesture_Single_Record)
       return Gdk.Event.Gdk_Event_Sequence
   is
      function Internal
         (Self : System.Address) return access Gdk.Event.Gdk_Event_Sequence;
      pragma Import (C, Internal, "gtk_gesture_single_get_current_sequence");
   begin
      return From_Object_Free (Internal (Get_Object (Self)));
   end Get_Current_Sequence;

   -------------------
   -- Get_Exclusive --
   -------------------

   function Get_Exclusive
      (Self : not null access Gtk_Gesture_Single_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_single_get_exclusive");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Exclusive;

   --------------------
   -- Get_Touch_Only --
   --------------------

   function Get_Touch_Only
      (Self : not null access Gtk_Gesture_Single_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_gesture_single_get_touch_only");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Touch_Only;

   ----------------
   -- Set_Button --
   ----------------

   procedure Set_Button
      (Self   : not null access Gtk_Gesture_Single_Record;
       Button : Guint)
   is
      procedure Internal (Self : System.Address; Button : Guint);
      pragma Import (C, Internal, "gtk_gesture_single_set_button");
   begin
      Internal (Get_Object (Self), Button);
   end Set_Button;

   -------------------
   -- Set_Exclusive --
   -------------------

   procedure Set_Exclusive
      (Self      : not null access Gtk_Gesture_Single_Record;
       Exclusive : Boolean)
   is
      procedure Internal (Self : System.Address; Exclusive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gesture_single_set_exclusive");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Exclusive));
   end Set_Exclusive;

   --------------------
   -- Set_Touch_Only --
   --------------------

   procedure Set_Touch_Only
      (Self       : not null access Gtk_Gesture_Single_Record;
       Touch_Only : Boolean)
   is
      procedure Internal (Self : System.Address; Touch_Only : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_gesture_single_set_touch_only");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Touch_Only));
   end Set_Touch_Only;

end Gtk.Gesture_Single;
