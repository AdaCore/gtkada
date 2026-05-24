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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with System;

package body Gdk.Cicp_Params is

   package Type_Conversion_Gdk_Cicp_Params is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Cicp_Params_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Cicp_Params);

   -------------------------
   -- Gdk_Cicp_Params_New --
   -------------------------

   function Gdk_Cicp_Params_New return Gdk_Cicp_Params is
      Self : constant Gdk_Cicp_Params := new Gdk_Cicp_Params_Record;
   begin
      Gdk.Cicp_Params.Initialize (Self);
      return Self;
   end Gdk_Cicp_Params_New;

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Gdk_Cicp_Params) is
   begin
      Self := new Gdk_Cicp_Params_Record;
      Gdk.Cicp_Params.Initialize (Self);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gdk_Cicp_Params_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gdk_cicp_params_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Build_Color_State --
   -----------------------

   function Build_Color_State
      (Self : not null access Gdk_Cicp_Params_Record)
       return Gdk.Color_State.Gdk_Color_State
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_cicp_params_build_color_state");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Build_Color_State;

   -------------------------
   -- Get_Color_Primaries --
   -------------------------

   function Get_Color_Primaries
      (Self : not null access Gdk_Cicp_Params_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_cicp_params_get_color_primaries");
   begin
      return Internal (Get_Object (Self));
   end Get_Color_Primaries;

   -----------------------------
   -- Get_Matrix_Coefficients --
   -----------------------------

   function Get_Matrix_Coefficients
      (Self : not null access Gdk_Cicp_Params_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_cicp_params_get_matrix_coefficients");
   begin
      return Internal (Get_Object (Self));
   end Get_Matrix_Coefficients;

   ---------------
   -- Get_Range --
   ---------------

   function Get_Range
      (Self : not null access Gdk_Cicp_Params_Record) return Gdk_Cicp_Range
   is
      function Internal (Self : System.Address) return Gdk_Cicp_Range;
      pragma Import (C, Internal, "gdk_cicp_params_get_range");
   begin
      return Internal (Get_Object (Self));
   end Get_Range;

   ---------------------------
   -- Get_Transfer_Function --
   ---------------------------

   function Get_Transfer_Function
      (Self : not null access Gdk_Cicp_Params_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gdk_cicp_params_get_transfer_function");
   begin
      return Internal (Get_Object (Self));
   end Get_Transfer_Function;

   -------------------------
   -- Set_Color_Primaries --
   -------------------------

   procedure Set_Color_Primaries
      (Self            : not null access Gdk_Cicp_Params_Record;
       Color_Primaries : Guint)
   is
      procedure Internal (Self : System.Address; Color_Primaries : Guint);
      pragma Import (C, Internal, "gdk_cicp_params_set_color_primaries");
   begin
      Internal (Get_Object (Self), Color_Primaries);
   end Set_Color_Primaries;

   -----------------------------
   -- Set_Matrix_Coefficients --
   -----------------------------

   procedure Set_Matrix_Coefficients
      (Self                : not null access Gdk_Cicp_Params_Record;
       Matrix_Coefficients : Guint)
   is
      procedure Internal
         (Self                : System.Address;
          Matrix_Coefficients : Guint);
      pragma Import (C, Internal, "gdk_cicp_params_set_matrix_coefficients");
   begin
      Internal (Get_Object (Self), Matrix_Coefficients);
   end Set_Matrix_Coefficients;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
      (Self   : not null access Gdk_Cicp_Params_Record;
       GRange : Gdk_Cicp_Range)
   is
      procedure Internal (Self : System.Address; GRange : Gdk_Cicp_Range);
      pragma Import (C, Internal, "gdk_cicp_params_set_range");
   begin
      Internal (Get_Object (Self), GRange);
   end Set_Range;

   ---------------------------
   -- Set_Transfer_Function --
   ---------------------------

   procedure Set_Transfer_Function
      (Self              : not null access Gdk_Cicp_Params_Record;
       Transfer_Function : Guint)
   is
      procedure Internal (Self : System.Address; Transfer_Function : Guint);
      pragma Import (C, Internal, "gdk_cicp_params_set_transfer_function");
   begin
      Internal (Get_Object (Self), Transfer_Function);
   end Set_Transfer_Function;

end Gdk.Cicp_Params;
