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

package body Gtk.Bool_Filter is

   package Type_Conversion_Gtk_Bool_Filter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Bool_Filter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Bool_Filter);

   -------------------------
   -- Gtk_Bool_Filter_New --
   -------------------------

   function Gtk_Bool_Filter_New
      (Expression : Gtk.Expression.Gtk_Expression) return Gtk_Bool_Filter
   is
      Self : constant Gtk_Bool_Filter := new Gtk_Bool_Filter_Record;
   begin
      Gtk.Bool_Filter.Initialize (Self, Expression);
      return Self;
   end Gtk_Bool_Filter_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gtk_Bool_Filter;
       Expression : Gtk.Expression.Gtk_Expression)
   is
   begin
      Self := new Gtk_Bool_Filter_Record;
      Gtk.Bool_Filter.Initialize (Self, Expression);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gtk_Bool_Filter_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression)
   is
      function Internal (Expression : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_bool_filter_new");
   begin
      if not Self.Is_Created then
         if Expression /= null then
            Adjust (Expression.all);
         end if;
         Set_Object (Self, Internal (Get_Object (Expression)));
      end if;
   end Initialize;

   --------------------
   -- Get_Expression --
   --------------------

   function Get_Expression
      (Self : not null access Gtk_Bool_Filter_Record)
       return Gtk.Expression.Gtk_Expression
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_bool_filter_get_expression");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Expression;

   ----------------
   -- Get_Invert --
   ----------------

   function Get_Invert
      (Self : not null access Gtk_Bool_Filter_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bool_filter_get_invert");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Invert;

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
      (Self       : not null access Gtk_Bool_Filter_Record;
       Expression : Gtk.Expression.Gtk_Expression)
   is
      procedure Internal
         (Self       : System.Address;
          Expression : System.Address);
      pragma Import (C, Internal, "gtk_bool_filter_set_expression");
   begin
      Internal (Get_Object (Self), Get_Object (Expression));
   end Set_Expression;

   ----------------
   -- Set_Invert --
   ----------------

   procedure Set_Invert
      (Self   : not null access Gtk_Bool_Filter_Record;
       Invert : Boolean)
   is
      procedure Internal (Self : System.Address; Invert : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_bool_filter_set_invert");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Invert));
   end Set_Invert;

end Gtk.Bool_Filter;
