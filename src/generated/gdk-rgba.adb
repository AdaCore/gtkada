------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Gtkada.Bindings;         use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Interfaces.C.Strings;    use Interfaces.C.Strings;
pragma Warnings(On);

package body Gdk.RGBA is

   function From_Object_Free (B : access Gdk_RGBA) return Gdk_RGBA is
      Result : constant Gdk_RGBA := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function To_Address
     (Val : Gdk_RGBA; Addr : System.Address) return System.Address;
   package RGBA_Properties is new Generic_Internal_Boxed_Property
     (Gdk_RGBA, Gdk.RGBA.Get_Type, To_Address);

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_RGBA;
      Value  : Gdk_RGBA) is
   begin
      RGBA_Properties.Set_Property
        (Object, RGBA_Properties.Property (Name), Value);
   end Set_Property;

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name   : Property_RGBA) return Gdk_RGBA is
   begin
      return RGBA_Properties.Get_Property
        (Object, RGBA_Properties.Property (Name));
   end Get_Property;

   function Gdk_RGBA_Or_Null (Val : System.Address) return System.Address is
      function Internal is new Gtkada.Bindings.Generic_To_Address_Or_Null
        (Gdk_RGBA, Null_RGBA);
   begin
      return Internal (Val);
   end Gdk_RGBA_Or_Null;

   function Get_Value (Value : Glib.Values.GValue) return Gdk_RGBA
   renames RGBA_Properties.Get_Value;
   procedure Set_Value (Value : out Glib.Values.GValue; Val : Gdk_RGBA)
   renames RGBA_Properties.Set_Value;

   function To_Address
     (Val : Gdk_RGBA; Addr : System.Address) return System.Address is
   begin
      if Val = Null_RGBA then
         return System.Null_Address;
      else
         return Addr;
      end if;
   end To_Address;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gdk_RGBA; P2 : Gdk_RGBA) return Boolean is
      function Internal (Self : Gdk_RGBA; P2 : Gdk_RGBA) return Integer;
      pragma Import (C, Internal, "gdk_rgba_equal");
   begin
      return Internal (Self, P2) /= 0;
   end Equal;

   -----------
   -- Parse --
   -----------

   procedure Parse
      (Self    : out Gdk_RGBA;
       Spec    : UTF8_String;
       Success : out Boolean)
   is
      function Internal
         (Acc_Self : access Gdk_RGBA;
          Spec     : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gdk_rgba_parse");
      Acc_Self     : aliased Gdk_RGBA;
      Tmp_Acc_Self : aliased Gdk_RGBA;
      Tmp_Spec     : Interfaces.C.Strings.chars_ptr := New_String (Spec);
      Tmp_Return   : Integer;
   begin
      Tmp_Return := Internal (Tmp_Acc_Self'Access, Tmp_Spec);
      Free (Tmp_Spec);
      Acc_Self := Tmp_Acc_Self;
      Self := Acc_Self;
      Success := Tmp_Return /= 0;
   end Parse;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gdk_RGBA) return UTF8_String is
      function Internal
         (Self : Gdk_RGBA) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_rgba_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Self));
   end To_String;

end Gdk.RGBA;
