------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

package body Gdk.Event.Pad_Event is

   --------------------
   -- Get_Axis_Value --
   --------------------

   procedure Get_Axis_Value
      (Self  : Gdk.Event.Gdk_Event;
       Index : out Guint;
       Value : out Gdouble)
   is
      procedure Internal
         (Self  : System.Address;
          Index : out Guint;
          Value : out Gdouble);
      pragma Import (C, Internal, "gdk_pad_event_get_axis_value");
   begin
      Internal (Get_Object (Self), Index, Value);
   end Get_Axis_Value;

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button (Self : Gdk.Event.Gdk_Event) return Guint is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_pad_event_get_button");
   begin
      return Internal (Get_Object (Self));
   end Get_Button;

   --------------------
   -- Get_Group_Mode --
   --------------------

   procedure Get_Group_Mode
      (Self  : Gdk.Event.Gdk_Event;
       Group : out Guint;
       Mode  : out Guint)
   is
      procedure Internal
         (Self  : System.Address;
          Group : out Guint;
          Mode  : out Guint);
      pragma Import (C, Internal, "gdk_pad_event_get_group_mode");
   begin
      Internal (Get_Object (Self), Group, Mode);
   end Get_Group_Mode;

end Gdk.Event.Pad_Event;
