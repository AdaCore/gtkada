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
with Glib.Object; use Glib.Object;

package body Gdk.Color_State is

   function From_Object_Free
     (B : access Gdk_Color_State'Class) return Gdk_Color_State
   is
      Result : constant Gdk_Color_State := Gdk_Color_State (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gdk_Color_State is
      S : Gdk_Color_State;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ------------------------
   -- Create_Cicp_Params --
   ------------------------

   function Create_Cicp_Params
      (Self : Gdk_Color_State) return Gdk.Cicp_Params.Gdk_Cicp_Params
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_color_state_create_cicp_params");
      Stub_Gdk_Cicp_Params : Gdk.Cicp_Params.Gdk_Cicp_Params_Record;
   begin
      return Gdk.Cicp_Params.Gdk_Cicp_Params (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Cicp_Params));
   end Create_Cicp_Params;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : Gdk_Color_State;
       Other : Gdk_Color_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_color_state_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Other)) /= 0;
   end Equal;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent
      (Self  : Gdk_Color_State;
       Other : Gdk_Color_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gdk_color_state_equivalent");
   begin
      return Internal (Get_Object (Self), Get_Object (Other)) /= 0;
   end Equivalent;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gdk_Color_State) return Gdk_Color_State is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_color_state_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gdk_Color_State) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gdk_color_state_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   ---------------
   -- Get_Oklab --
   ---------------

   function Get_Oklab return Gdk_Color_State is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_color_state_get_oklab");
   begin
      return From_Object (Internal);
   end Get_Oklab;

   ---------------
   -- Get_Oklch --
   ---------------

   function Get_Oklch return Gdk_Color_State is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_color_state_get_oklch");
   begin
      return From_Object (Internal);
   end Get_Oklch;

   ------------------------
   -- Get_Rec2100_Linear --
   ------------------------

   function Get_Rec2100_Linear return Gdk_Color_State is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_color_state_get_rec2100_linear");
   begin
      return From_Object (Internal);
   end Get_Rec2100_Linear;

   --------------------
   -- Get_Rec2100_Pq --
   --------------------

   function Get_Rec2100_Pq return Gdk_Color_State is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_color_state_get_rec2100_pq");
   begin
      return From_Object (Internal);
   end Get_Rec2100_Pq;

   --------------
   -- Get_Srgb --
   --------------

   function Get_Srgb return Gdk_Color_State is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_color_state_get_srgb");
   begin
      return From_Object (Internal);
   end Get_Srgb;

   ---------------------
   -- Get_Srgb_Linear --
   ---------------------

   function Get_Srgb_Linear return Gdk_Color_State is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_color_state_get_srgb_linear");
   begin
      return From_Object (Internal);
   end Get_Srgb_Linear;

end Gdk.Color_State;
