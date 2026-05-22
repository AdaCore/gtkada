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
with Gdk.Display;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with System;

package body Gdk.Draw_Context is

   package Type_Conversion_Gdk_Draw_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Draw_Context_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Draw_Context);

   -----------------
   -- Begin_Frame --
   -----------------

   procedure Begin_Frame
      (Self   : not null access Gdk_Draw_Context_Record;
       Region : Cairo.Region.Cairo_Region)
   is
      procedure Internal
         (Self   : System.Address;
          Region : Cairo.Region.Cairo_Region);
      pragma Import (C, Internal, "gdk_draw_context_begin_frame");
   begin
      Internal (Get_Object (Self), Region);
   end Begin_Frame;

   ---------------
   -- End_Frame --
   ---------------

   procedure End_Frame (Self : not null access Gdk_Draw_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_draw_context_end_frame");
   begin
      Internal (Get_Object (Self));
   end End_Frame;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gdk_Draw_Context_Record)
       return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_draw_context_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   ----------------------
   -- Get_Frame_Region --
   ----------------------

   function Get_Frame_Region
      (Self : not null access Gdk_Draw_Context_Record)
       return Cairo.Region.Cairo_Region
   is
      function Internal
         (Self : System.Address) return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gdk_draw_context_get_frame_region");
   begin
      return Internal (Get_Object (Self));
   end Get_Frame_Region;

   -----------------
   -- Is_In_Frame --
   -----------------

   function Is_In_Frame
      (Self : not null access Gdk_Draw_Context_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_draw_context_is_in_frame");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_In_Frame;

end Gdk.Draw_Context;
