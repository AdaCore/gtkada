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

package body Gtk.Object_Expression is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self   : out Gtk_Object_Expression;
       Object : not null access Glib.Object.GObject_Record'Class)
   is
   begin
      Self := new Gtk_Object_Expression_Record;
      Gtk.Object_Expression.Initialize (Self, Object);
   end Gtk_New;

   -------------------------------
   -- Gtk_Object_Expression_New --
   -------------------------------

   function Gtk_Object_Expression_New
      (Object : not null access Glib.Object.GObject_Record'Class)
       return Gtk_Object_Expression
   is
      Self : constant Gtk_Object_Expression := new Gtk_Object_Expression_Record;
   begin
      Gtk.Object_Expression.Initialize (Self, Object);
      return Self;
   end Gtk_Object_Expression_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Object_Expression_Record'Class;
       Object : not null access Glib.Object.GObject_Record'Class)
   is
      function Internal (Object : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_object_expression_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Object)));
      end if;
   end Initialize;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Self : Gtk_Object_Expression) return Glib.Object.GObject
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_object_expression_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self)), Stub_GObject);
   end Get_Object;

   ------------
   -- Create --
   ------------

   overriding function Create
      (Object : not null access System.Address)
       return Gtk_Object_Expression_Record
   is
      pragma Unreferenced (Object);
      Result : Gtk_Object_Expression_Record;
   begin
      return Result;
   end Create;

end Gtk.Object_Expression;
