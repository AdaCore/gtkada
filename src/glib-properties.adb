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

with Glib.GObjects;        use Glib.GObjects;
with Glib.Values;          use Glib.Values;

package body Glib.Properties is

   procedure Get
     (Object : System.Address; Name : Glib.Property; Value : in out GValue);
   pragma Import (C, Get, "g_object_get_property");
   --  Internal function to get the properties

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (Object : access GObject_Record'Class;
      Name   : Property_String;
      Value  : String)
   is
      procedure Internal
        (Object : System.Address;
         Name : Property;
         Value : String;
         Null_Arg : System.Address := System.Null_Address);
      pragma Import (C, Internal, "g_object_set");
   begin
      Internal (Get_Object (Object), Property (Name),  Value & ASCII.Nul);
   end Set_Property;


   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.GObjects.GObject_Record'Class;
      Name   : Property_String_RO) return String
   is
      Value : GValue;
   begin
      Init (Value, GType_String);
      Get (Get_Object (Object), Property (Name), Value);
      declare
         S : constant String := Get_String (Value);
      begin
         Unset (Value);
         return S;
      end;
   end Get_Property;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Object : access Glib.GObjects.GObject_Record'Class;
      Name   : Property_String) return String is
   begin
      return Get_Property (Object, Property_String_RO (Name));
   end Get_Property;

end Glib.Properties;
