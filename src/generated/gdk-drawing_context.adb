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

package body Gdk.Drawing_Context is

   package Type_Conversion_Gdk_Drawing_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Drawing_Context_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Drawing_Context);

   -----------------------
   -- Get_Cairo_Context --
   -----------------------

   function Get_Cairo_Context
      (Self : not null access Gdk_Drawing_Context_Record)
       return Cairo.Cairo_Context
   is
      function Internal (Self : System.Address) return Cairo.Cairo_Context;
      pragma Import (C, Internal, "gdk_drawing_context_get_cairo_context");
   begin
      return Internal (Get_Object (Self));
   end Get_Cairo_Context;

   --------------
   -- Get_Clip --
   --------------

   function Get_Clip
      (Self : not null access Gdk_Drawing_Context_Record)
       return Cairo.Region.Cairo_Region
   is
      function Internal
         (Self : System.Address) return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gdk_drawing_context_get_clip");
   begin
      return Internal (Get_Object (Self));
   end Get_Clip;

   ----------------
   -- Get_Window --
   ----------------

   function Get_Window
      (Self : not null access Gdk_Drawing_Context_Record)
       return Gdk.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Gdk_Window;
      pragma Import (C, Internal, "gdk_drawing_context_get_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Window;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
      (Self : not null access Gdk_Drawing_Context_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_drawing_context_is_valid");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Valid;

end Gdk.Drawing_Context;
