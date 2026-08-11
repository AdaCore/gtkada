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

package body Gtk.Constant_Expression is

   -------------------------------------------
   -- Gtk_Constant_Expression_New_For_Value --
   -------------------------------------------

   function Gtk_Constant_Expression_New_For_Value
      (Value : in out Glib.Values.GValue) return Gtk_Constant_Expression
   is
      Self : constant Gtk_Constant_Expression := new Gtk_Constant_Expression_Record;
   begin
      Gtk.Constant_Expression.Initialize_For_Value (Self, Value);
      return Self;
   end Gtk_Constant_Expression_New_For_Value;

   -----------------------
   -- Gtk_New_For_Value --
   -----------------------

   procedure Gtk_New_For_Value
      (Self  : out Gtk_Constant_Expression;
       Value : in out Glib.Values.GValue)
   is
   begin
      Self := new Gtk_Constant_Expression_Record;
      Gtk.Constant_Expression.Initialize_For_Value (Self, Value);
   end Gtk_New_For_Value;

   --------------------------
   -- Initialize_For_Value --
   --------------------------

   procedure Initialize_For_Value
      (Self  : not null access Gtk_Constant_Expression_Record'Class;
       Value : in out Glib.Values.GValue)
   is
      function Internal
         (Acc_Value : access Glib.Values.GValue) return System.Address;
      pragma Import (C, Internal, "gtk_constant_expression_new_for_value");
      Acc_Value : aliased Glib.Values.GValue := Value;
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Acc_Value'Access));
      end if;
   end Initialize_For_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
      (Self : Gtk_Constant_Expression) return access constant GValue
   is
      function Internal
         (Self : System.Address) return access constant GValue;
      pragma Import (C, Internal, "gtk_constant_expression_get_value");
   begin
      return Internal (Get_Object (Self));
   end Get_Value;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Constant_Expression_Record
   is
      pragma Unreferenced (Object);
      Result : Gtk_Constant_Expression_Record;
   begin
      return Result;
   end Create;

end Gtk.Constant_Expression;
