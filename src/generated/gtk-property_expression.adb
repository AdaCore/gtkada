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
pragma Warnings(Off);  --  might be unused
with Gtkada.Types; use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Property_Expression is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self          : out Gtk_Property_Expression;
       This_Type     : GType;
       Expression    : Gtk.Expression.Gtk_Expression;
       Property_Name : UTF8_String)
   is
   begin
      Self := new Gtk_Property_Expression_Record;
      Gtk.Property_Expression.Initialize (Self, This_Type, Expression, Property_Name);
   end Gtk_New;

   -----------------------
   -- Gtk_New_For_Pspec --
   -----------------------

   procedure Gtk_New_For_Pspec
      (Self       : out Gtk_Property_Expression;
       Expression : Gtk.Expression.Gtk_Expression;
       Pspec      : Glib.Param_Spec)
   is
   begin
      Self := new Gtk_Property_Expression_Record;
      Gtk.Property_Expression.Initialize_For_Pspec (Self, Expression, Pspec);
   end Gtk_New_For_Pspec;

   ---------------------------------
   -- Gtk_Property_Expression_New --
   ---------------------------------

   function Gtk_Property_Expression_New
      (This_Type     : GType;
       Expression    : Gtk.Expression.Gtk_Expression;
       Property_Name : UTF8_String) return Gtk_Property_Expression
   is
      Self : constant Gtk_Property_Expression := new Gtk_Property_Expression_Record;
   begin
      Gtk.Property_Expression.Initialize (Self, This_Type, Expression, Property_Name);
      return Self;
   end Gtk_Property_Expression_New;

   -------------------------------------------
   -- Gtk_Property_Expression_New_For_Pspec --
   -------------------------------------------

   function Gtk_Property_Expression_New_For_Pspec
      (Expression : Gtk.Expression.Gtk_Expression;
       Pspec      : Glib.Param_Spec) return Gtk_Property_Expression
   is
      Self : constant Gtk_Property_Expression := new Gtk_Property_Expression_Record;
   begin
      Gtk.Property_Expression.Initialize_For_Pspec (Self, Expression, Pspec);
      return Self;
   end Gtk_Property_Expression_New_For_Pspec;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self          : not null access Gtk_Property_Expression_Record'Class;
       This_Type     : GType;
       Expression    : Gtk.Expression.Gtk_Expression;
       Property_Name : UTF8_String)
   is
      function Internal
         (This_Type     : GType;
          Expression    : System.Address;
          Property_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_property_expression_new");
      Tmp_Property_Name : Gtkada.Types.Chars_Ptr := New_String (Property_Name);
      Tmp_Return        : System.Address;
   begin
      if not Self.Is_Created then
         if Expression /= null then
            --  transfer-ownership='full'
            Adjust (Expression.all);
         end if;
         Tmp_Return := Internal (This_Type, Get_Object (Expression), Tmp_Property_Name);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Property_Name);
   end Initialize;

   --------------------------
   -- Initialize_For_Pspec --
   --------------------------

   procedure Initialize_For_Pspec
      (Self       : not null access Gtk_Property_Expression_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression;
       Pspec      : Glib.Param_Spec)
   is
      function Internal
         (Expression : System.Address;
          Pspec      : Glib.Param_Spec) return System.Address;
      pragma Import (C, Internal, "gtk_property_expression_new_for_pspec");
   begin
      if not Self.Is_Created then
         if Expression /= null then
            --  transfer-ownership='full'
            Adjust (Expression.all);
         end if;
         Set_Object (Self, Internal (Get_Object (Expression), Pspec));
      end if;
   end Initialize_For_Pspec;

   --------------------
   -- Get_Expression --
   --------------------

   function Get_Expression
      (Self : Gtk_Property_Expression) return Gtk.Expression.Gtk_Expression
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_property_expression_get_expression");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Expression;

   ---------------
   -- Get_Pspec --
   ---------------

   function Get_Pspec
      (Self : Gtk_Property_Expression) return Glib.Param_Spec
   is
      function Internal (Self : System.Address) return Glib.Param_Spec;
      pragma Import (C, Internal, "gtk_property_expression_get_pspec");
   begin
      return Internal (Get_Object (Self));
   end Get_Pspec;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Property_Expression_Record
   is
      pragma Unreferenced (Object);
      Result : Gtk_Property_Expression_Record;
   begin
      return Result;
   end Create;

end Gtk.Property_Expression;
