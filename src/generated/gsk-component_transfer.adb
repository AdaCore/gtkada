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

package body Gsk.Component_Transfer is

   function From_Object_Free
     (B : access Gsk_Component_Transfer'Class) return Gsk_Component_Transfer
   is
      Result : constant Gsk_Component_Transfer := Gsk_Component_Transfer (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsk_Component_Transfer is
      S : Gsk_Component_Transfer;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------------------------------------
   -- Gsk_Component_Transfer_New_Discrete --
   -----------------------------------------

   function Gsk_Component_Transfer_New_Discrete
      (N      : Guint;
       Values : Gfloat_Array) return Gsk_Component_Transfer
   is
      function Internal
         (N      : Guint;
          Values : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_discrete");
      Self : Gsk_Component_Transfer;
   begin
      Self.Set_Object (Internal (N, Values'Address));
      return Self;
   end Gsk_Component_Transfer_New_Discrete;

   --------------------------------------
   -- Gsk_Component_Transfer_New_Gamma --
   --------------------------------------

   function Gsk_Component_Transfer_New_Gamma
      (Amp : Interfaces.C.C_float;
       Exp : Interfaces.C.C_float;
       Ofs : Interfaces.C.C_float) return Gsk_Component_Transfer
   is
      function Internal
         (Amp : Interfaces.C.C_float;
          Exp : Interfaces.C.C_float;
          Ofs : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_gamma");
      Self : Gsk_Component_Transfer;
   begin
      Self.Set_Object (Internal (Amp, Exp, Ofs));
      return Self;
   end Gsk_Component_Transfer_New_Gamma;

   -----------------------------------------
   -- Gsk_Component_Transfer_New_Identity --
   -----------------------------------------

   function Gsk_Component_Transfer_New_Identity return Gsk_Component_Transfer is
      function Internal return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_identity");
      Self : Gsk_Component_Transfer;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gsk_Component_Transfer_New_Identity;

   ---------------------------------------
   -- Gsk_Component_Transfer_New_Levels --
   ---------------------------------------

   function Gsk_Component_Transfer_New_Levels
      (N : Interfaces.C.C_float) return Gsk_Component_Transfer
   is
      function Internal (N : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_levels");
      Self : Gsk_Component_Transfer;
   begin
      Self.Set_Object (Internal (N));
      return Self;
   end Gsk_Component_Transfer_New_Levels;

   ---------------------------------------
   -- Gsk_Component_Transfer_New_Linear --
   ---------------------------------------

   function Gsk_Component_Transfer_New_Linear
      (M : Interfaces.C.C_float;
       B : Interfaces.C.C_float) return Gsk_Component_Transfer
   is
      function Internal
         (M : Interfaces.C.C_float;
          B : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_linear");
      Self : Gsk_Component_Transfer;
   begin
      Self.Set_Object (Internal (M, B));
      return Self;
   end Gsk_Component_Transfer_New_Linear;

   --------------------------------------
   -- Gsk_Component_Transfer_New_Table --
   --------------------------------------

   function Gsk_Component_Transfer_New_Table
      (N      : Guint;
       Values : Gfloat_Array) return Gsk_Component_Transfer
   is
      function Internal
         (N      : Guint;
          Values : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_table");
      Self : Gsk_Component_Transfer;
   begin
      Self.Set_Object (Internal (N, Values'Address));
      return Self;
   end Gsk_Component_Transfer_New_Table;

   ----------------------
   -- Gtk_New_Discrete --
   ----------------------

   procedure Gtk_New_Discrete
      (Self   : out Gsk_Component_Transfer;
       N      : Guint;
       Values : Gfloat_Array)
   is
      function Internal
         (N      : Guint;
          Values : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_discrete");
   begin
      Self.Set_Object (Internal (N, Values'Address));
   end Gtk_New_Discrete;

   -------------------
   -- Gtk_New_Gamma --
   -------------------

   procedure Gtk_New_Gamma
      (Self : out Gsk_Component_Transfer;
       Amp  : Interfaces.C.C_float;
       Exp  : Interfaces.C.C_float;
       Ofs  : Interfaces.C.C_float)
   is
      function Internal
         (Amp : Interfaces.C.C_float;
          Exp : Interfaces.C.C_float;
          Ofs : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_gamma");
   begin
      Self.Set_Object (Internal (Amp, Exp, Ofs));
   end Gtk_New_Gamma;

   ----------------------
   -- Gtk_New_Identity --
   ----------------------

   procedure Gtk_New_Identity (Self : out Gsk_Component_Transfer) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_identity");
   begin
      Self.Set_Object (Internal);
   end Gtk_New_Identity;

   --------------------
   -- Gtk_New_Levels --
   --------------------

   procedure Gtk_New_Levels
      (Self : out Gsk_Component_Transfer;
       N    : Interfaces.C.C_float)
   is
      function Internal (N : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_levels");
   begin
      Self.Set_Object (Internal (N));
   end Gtk_New_Levels;

   --------------------
   -- Gtk_New_Linear --
   --------------------

   procedure Gtk_New_Linear
      (Self : out Gsk_Component_Transfer;
       M    : Interfaces.C.C_float;
       B    : Interfaces.C.C_float)
   is
      function Internal
         (M : Interfaces.C.C_float;
          B : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_linear");
   begin
      Self.Set_Object (Internal (M, B));
   end Gtk_New_Linear;

   -------------------
   -- Gtk_New_Table --
   -------------------

   procedure Gtk_New_Table
      (Self   : out Gsk_Component_Transfer;
       N      : Guint;
       Values : Gfloat_Array)
   is
      function Internal
         (N      : Guint;
          Values : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_new_table");
   begin
      Self.Set_Object (Internal (N, Values'Address));
   end Gtk_New_Table;

   ----------
   -- Copy --
   ----------

   function Copy
      (Self : Gsk_Component_Transfer) return Gsk_Component_Transfer
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_component_transfer_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Gsk_Component_Transfer) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_component_transfer_free");
   begin
      Internal (Get_Object (Self));
   end Free;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self  : System.Address;
       Other : System.Address) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Other : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_component_transfer_equal");
   begin
      return Internal (Self, Other) /= 0;
   end Equal;

end Gsk.Component_Transfer;
