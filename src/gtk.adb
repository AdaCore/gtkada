-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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
--  with Ada.Text_IO;
--  with Ada.Tags; use Ada.Tags;

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
      function Convert is new Unchecked_Conversion
        (System.Address, Integer);
      Obj : Root_Type_Access := Convert (Data);
   begin
--        Ada.Text_IO.Put_Line
--          ("Free_User_Data " & External_Tag (Obj.all'Tag)
--           & " size=" & Integer'Image (Obj'Size)
--           & " Address=" & Integer'Image (Convert (Obj.all'Address)));
      Free (Obj);
   end Free_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data (Obj  : in System.Address; Stub : in Root_Type'Class)
                           return Root_Type_Access
   is
      function Internal (Object : in System.Address; Key : in String)
                         return Root_Type_Access;
      pragma Import (C, Internal, "gtk_object_get_data");
      use type System.Address;
      R : Root_Type_Access;
   begin
      if Obj = System.Null_Address then
         return null;
      end if;
      R := Internal (Obj, GtkAda_String);
      if R = null then
         R := new Root_Type'Class'(Stub);
         --  R := new Gtk.Object.Gtk_Object_Record;
         --  TBD??? Above, we should create the correct Ada type
         --  TBD??? Or we get CONSTRAINT_ERROR
         --  TBD??? The idea is to find the correct stub in a htable
         Set_Object (R, Obj);
         Initialize_User_Data (R);
      end if;
      return R;
   end Get_User_Data;

   --------------------------
   -- Initialize_User_Data --
   --------------------------

   procedure Initialize_User_Data (Obj : access Root_Type'Class) is
      function Internal (Object : in System.Address;
                         Key    : in String)
                         return Root_Type_Access;
      pragma Import (C, Internal, "gtk_object_get_data");

      procedure Set_User_Data (Obj     : System.Address;
                               Name    : String;
                               Data    : System.Address;
                               Destroy : System.Address);
      pragma Import (C, Set_User_Data, "gtk_object_set_data_full");

--        function Convert is new Unchecked_Conversion (System.Address,
--                                                      Integer);
   begin
      if Internal (Get_Object (Obj), GtkAda_String) = null then
         Set_User_Data (Get_Object (Obj), GtkAda_String,
                        Obj.all'Address, Free_User_Data'Address);
--           Ada.Text_IO.Put_Line
--             ("Initialize_User_Data " & External_Tag (Obj.all'Tag)
--              & " size=" & Integer'Image (Obj'Size)
--              & " Address=" & Integer'Image (Convert (Obj.all'Address)));
      end if;
   end Initialize_User_Data;

   -------------------
   -- Major_Version --
   -------------------

   function Major_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_major_version");
   begin
      return Number;
   end Major_Version;

   -------------------
   -- Micro_Version --
   -------------------

   function Micro_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_micro_version");
   begin
      return Number;
   end Micro_Version;

   -------------------
   -- Minor_Version --
   -------------------

   function Minor_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_minor_version");
   begin
      return Number;
   end Minor_Version;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : in Gint) return String
   is
      function Internal (Type_Num : in Gint)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_type_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Type_Num));
   end Type_Name;

end Gtk;
