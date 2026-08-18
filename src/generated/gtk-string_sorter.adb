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

package body Gtk.String_Sorter is

   package Type_Conversion_Gtk_String_Sorter is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_String_Sorter_Record);
   pragma Unreferenced (Type_Conversion_Gtk_String_Sorter);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gtk_String_Sorter;
       Expression : Gtk.Expression.Gtk_Expression)
   is
   begin
      Self := new Gtk_String_Sorter_Record;
      Gtk.String_Sorter.Initialize (Self, Expression);
   end Gtk_New;

   ---------------------------
   -- Gtk_String_Sorter_New --
   ---------------------------

   function Gtk_String_Sorter_New
      (Expression : Gtk.Expression.Gtk_Expression) return Gtk_String_Sorter
   is
      Self : constant Gtk_String_Sorter := new Gtk_String_Sorter_Record;
   begin
      Gtk.String_Sorter.Initialize (Self, Expression);
      return Self;
   end Gtk_String_Sorter_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gtk_String_Sorter_Record'Class;
       Expression : Gtk.Expression.Gtk_Expression)
   is
      function Internal (Expression : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_string_sorter_new");
   begin
      if not Self.Is_Created then
         if Expression /= null then
            --  transfer-ownership='full'
            Adjust (Expression.all);
         end if;
         Set_Object (Self, Internal (Get_Object (Expression)));
      end if;
   end Initialize;

   -------------------
   -- Get_Collation --
   -------------------

   function Get_Collation
      (Self : not null access Gtk_String_Sorter_Record) return Gtk_Collation
   is
      function Internal (Self : System.Address) return Gtk_Collation;
      pragma Import (C, Internal, "gtk_string_sorter_get_collation");
   begin
      return Internal (Get_Object (Self));
   end Get_Collation;

   --------------------
   -- Get_Expression --
   --------------------

   function Get_Expression
      (Self : not null access Gtk_String_Sorter_Record)
       return Gtk.Expression.Gtk_Expression
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_string_sorter_get_expression");
   begin
      return From_Object_None_Ownership (Internal (Get_Object (Self)));
   end Get_Expression;

   ---------------------
   -- Get_Ignore_Case --
   ---------------------

   function Get_Ignore_Case
      (Self : not null access Gtk_String_Sorter_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_string_sorter_get_ignore_case");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Ignore_Case;

   -------------------
   -- Set_Collation --
   -------------------

   procedure Set_Collation
      (Self      : not null access Gtk_String_Sorter_Record;
       Collation : Gtk_Collation)
   is
      procedure Internal (Self : System.Address; Collation : Gtk_Collation);
      pragma Import (C, Internal, "gtk_string_sorter_set_collation");
   begin
      Internal (Get_Object (Self), Collation);
   end Set_Collation;

   --------------------
   -- Set_Expression --
   --------------------

   procedure Set_Expression
      (Self       : not null access Gtk_String_Sorter_Record;
       Expression : Gtk.Expression.Gtk_Expression)
   is
      procedure Internal
         (Self       : System.Address;
          Expression : System.Address);
      pragma Import (C, Internal, "gtk_string_sorter_set_expression");
   begin
      Internal (Get_Object (Self), Get_Object (Expression));
   end Set_Expression;

   ---------------------
   -- Set_Ignore_Case --
   ---------------------

   procedure Set_Ignore_Case
      (Self        : not null access Gtk_String_Sorter_Record;
       Ignore_Case : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Ignore_Case : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_string_sorter_set_ignore_case");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Ignore_Case));
   end Set_Ignore_Case;

end Gtk.String_Sorter;
