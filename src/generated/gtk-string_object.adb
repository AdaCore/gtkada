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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.String_Object is

   package Type_Conversion_Gtk_String_Object is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_String_Object_Record);
   pragma Unreferenced (Type_Conversion_Gtk_String_Object);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_String_Object; String : UTF8_String) is
   begin
      Self := new Gtk_String_Object_Record;
      Gtk.String_Object.Initialize (Self, String);
   end Gtk_New;

   ---------------------------
   -- Gtk_String_Object_New --
   ---------------------------

   function Gtk_String_Object_New
      (String : UTF8_String) return Gtk_String_Object
   is
      Self : constant Gtk_String_Object := new Gtk_String_Object_Record;
   begin
      Gtk.String_Object.Initialize (Self, String);
      return Self;
   end Gtk_String_Object_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_String_Object_Record'Class;
       String : UTF8_String)
   is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_string_object_new");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_String);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_String);
   end Initialize;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
      (Self : not null access Gtk_String_Object_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_string_object_get_string");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_String;

end Gtk.String_Object;
