-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Glib.Values is

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Value : GValue) return Boolean is
      function Internal (Value : GValue) return Gboolean;
      pragma Import (C, Internal, "g_value_get_boolean");
   begin
      return Boolean'Val (Internal (Value));
   end Get_Boolean;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Value : GValue) return String is
      function Internal (Value : GValue) return chars_ptr;
      pragma Import (C, Internal, "g_value_get_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Value));
   end Get_String;

   -----------------
   -- Make_Values --
   -----------------

   function Make_Values (Nb : Guint; Val : System.Address) return GValues is
   begin
      return (Nb => Nb, Arr => Val);
   end Make_Values;

   function Nth (Val : GValues; Num : Guint) return GValue is
      function Internal (Val : System.Address; Num : Guint) return GValue;
      pragma Import (C, Internal, "ada_gvalue_nth");

   begin
      --   Should this be greater or greater or equal ???
      if Num > Val.Nb then
         raise Constraint_Error;
      end if;

      return Internal (Val.Arr, Num);
   end Nth;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Value : GValue; V_Boolean : Boolean) is
      procedure Internal (Value : GValue; V_Boolean : Gboolean);
      pragma Import (C, Internal, "g_value_set_boolean");
   begin
      Internal (Value, Boolean'Pos (V_Boolean));
   end Set_Boolean;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Value : GValue; V_String : String) is
      procedure Internal (Value : GValue; V_String : String);
      pragma Import (C, Internal, "g_value_set_string");
   begin
      Internal (Value, V_String & ASCII.NUL);
   end Set_String;

end Glib.Values;
