-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with Interfaces.C.Strings;
with Unchecked_Conversion;
with Unchecked_Deallocation;
with Gtk.Type_Conversion_Hooks;

package body Gtk is

   procedure Free_User_Data (Data : in System.Address);
   --  Free the user data Data. This function should not be called directly

   --------------------
   -- Free_User_Data --
   --------------------

   procedure Free_User_Data (Data : in System.Address) is
      function Convert is new Unchecked_Conversion
        (System.Address, Root_Type_Access);
      procedure Free is new Unchecked_Deallocation
        (Root_Type'Class, Root_Type_Access);
      Obj : Root_Type_Access := Convert (Data);

   begin
      Free (Obj);
   end Free_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Obj  : in System.Address;
      Stub : in Root_Type'Class) return Root_Type_Access
   is
      function Internal
        (Object : in System.Address;
         Key : in Glib.GQuark) return Root_Type_Access;
      pragma Import (C, Internal, "gtk_object_get_data_by_id");

      use type System.Address;

      R : Root_Type_Access;

   begin
      if Obj = System.Null_Address then
         return null;
      end if;

      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      R := Internal (Obj, GtkAda_String_Quark);

      if R = null then
         R := Conversion_Function (Obj, Stub);
         --  This function will either simply return what we expect (Stub), or
         --  try to create the exact Ada type corresponding to the C type.
         Set_Object (R, Obj);
         Initialize_User_Data (R);
      end if;

      return R;
   end Get_User_Data;

   --------------------------
   -- Initialize_User_Data --
   --------------------------

   procedure Initialize_User_Data (Obj : access Root_Type'Class) is
      function Internal
        (Object : in System.Address;
         Key    : in Glib.GQuark) return Root_Type_Access;
      pragma Import (C, Internal, "gtk_object_get_data_by_id");

      procedure Set_User_Data
        (Obj     : System.Address;
         Name    : Glib.GQuark;
         Data    : System.Address;
         Destroy : System.Address);
      pragma Import (C, Set_User_Data, "gtk_object_set_data_by_id_full");

   begin
      if GtkAda_String_Quark = Glib.Unknown_Quark then
         GtkAda_String_Quark := Glib.Quark_From_String (GtkAda_String);
      end if;

      if Internal (Get_Object (Obj), GtkAda_String_Quark) = null then
         Set_User_Data (Get_Object (Obj), GtkAda_String_Quark,
                        Obj.all'Address, Free_User_Data'Address);
      end if;
   end Initialize_User_Data;

   -------------------------
   -- Conversion_Function --
   -------------------------

   function Conversion_Function
     (Obj  : System.Address; Stub : Root_Type'Class) return Root_Type_Access
   is
      function Get_Type (Obj : System.Address) return Gtk_Type;
      pragma Import (C, Get_Type, "ada_object_get_type");

      Name  : constant String := Type_Name (Get_Type (Obj));
      Hooks : Gtk.Type_Conversion_Hooks.Hook_List_Access;

      use type Gtk.Type_Conversion_Hooks.Hook_List_Access;

   begin
      Hooks := Gtk.Type_Conversion_Hooks.Conversion_Hooks;

      while Hooks /= null loop
         declare
            R : Root_Type_Access := Hooks.Func (Name);
         begin
            if R /= null then
               return R;
            end if;
         end;

         Hooks := Hooks.Next;
      end loop;

      return new Root_Type'Class' (Stub);
   end Conversion_Function;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : in Gtk_Type) return String is
      function Internal
        (Type_Num : in Gtk_Type) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_type_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Type_Num));
   end Type_Name;

   --------------------
   -- Type_From_Name --
   --------------------

   function Type_From_Name (Name : in String) return Gtk_Type is
      function Internal (Name : String) return Gtk_Type;
      pragma Import (C, Internal, "gtk_type_from_name");
   begin
      return Internal (Name & ASCII.Nul);
   end Type_From_Name;

   ------------------
   --  Get_Object  --
   ------------------

   function Get_Object
     (Object : access Root_Type'Class) return System.Address is
   begin
      return Object.Ptr;
   end Get_Object;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created (Object : in Root_Type'Class) return Boolean is
      use type System.Address;
   begin
      return Object.Ptr /= System.Null_Address;
   end Is_Created;

   ------------------
   --  Set_Object  --
   ------------------

   procedure Set_Object
     (Object : access Root_Type'Class;
      Value  : in     System.Address) is
   begin
      Object.Ptr := Value;
   end Set_Object;

end Gtk;
