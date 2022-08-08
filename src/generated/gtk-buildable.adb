------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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
with Gtkada.Bindings; use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Buildable is

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
      (Self     : Gtk_Buildable;
       Builder  : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Child    : not null access Glib.Object.GObject_Record'Class;
       The_Type : UTF8_String := "";
       Error    : Glib.Error.GError)
   is
      procedure Internal
         (Self     : Gtk_Buildable;
          Builder  : System.Address;
          Child    : System.Address;
          The_Type : Gtkada.Types.Chars_Ptr;
          Error    : Glib.Error.GError);
      pragma Import (C, Internal, "gtk_buildable_add_child");
      Tmp_The_Type : Gtkada.Types.Chars_Ptr;
   begin
      if The_Type = "" then
         Tmp_The_Type := Gtkada.Types.Null_Ptr;
      else
         Tmp_The_Type := New_String (The_Type);
      end if;
      Internal (Self, Get_Object (Builder), Get_Object (Child), Tmp_The_Type, Error);
      Free (Tmp_The_Type);
   end Add_Child;

   ---------------------
   -- Construct_Child --
   ---------------------

   function Construct_Child
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Name    : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_buildable_construct_child");
      Tmp_Name     : Gtkada.Types.Chars_Ptr := New_String (Name);
      Stub_GObject : Glib.Object.GObject_Record;
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Self, Get_Object (Builder), Tmp_Name);
      Free (Tmp_Name);
      return Get_User_Data (Tmp_Return, Stub_GObject);
   end Construct_Child;

   ---------------------
   -- Custom_Finished --
   ---------------------

   procedure Custom_Finished
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : System.Address)
   is
      procedure Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Child   : System.Address;
          Tagname : Gtkada.Types.Chars_Ptr;
          Data    : System.Address);
      pragma Import (C, Internal, "gtk_buildable_custom_finished");
      Tmp_Tagname : Gtkada.Types.Chars_Ptr := New_String (Tagname);
   begin
      Internal (Self, Get_Object (Builder), Get_Object_Or_Null (GObject (Child)), Tmp_Tagname, Data);
      Free (Tmp_Tagname);
   end Custom_Finished;

   --------------------
   -- Custom_Tag_End --
   --------------------

   procedure Custom_Tag_End
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : in out System.Address)
   is
      procedure Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Child   : System.Address;
          Tagname : Gtkada.Types.Chars_Ptr;
          Data    : in out System.Address);
      pragma Import (C, Internal, "gtk_buildable_custom_tag_end");
      Tmp_Tagname : Gtkada.Types.Chars_Ptr := New_String (Tagname);
   begin
      Internal (Self, Get_Object (Builder), Get_Object_Or_Null (GObject (Child)), Tmp_Tagname, Data);
      Free (Tmp_Tagname);
   end Custom_Tag_End;

   ------------------------
   -- Get_Internal_Child --
   ------------------------

   function Get_Internal_Child
      (Self      : Gtk_Buildable;
       Builder   : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Childname : UTF8_String) return Glib.Object.GObject
   is
      function Internal
         (Self      : Gtk_Buildable;
          Builder   : System.Address;
          Childname : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_buildable_get_internal_child");
      Tmp_Childname : Gtkada.Types.Chars_Ptr := New_String (Childname);
      Stub_GObject  : Glib.Object.GObject_Record;
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Self, Get_Object (Builder), Tmp_Childname);
      Free (Tmp_Childname);
      return Get_User_Data (Tmp_Return, Stub_GObject);
   end Get_Internal_Child;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Gtk_Buildable) return UTF8_String is
      function Internal (Self : Gtk_Buildable) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_buildable_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Self));
   end Get_Name;

   ---------------------
   -- Parser_Finished --
   ---------------------

   procedure Parser_Finished
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class)
   is
      procedure Internal (Self : Gtk_Buildable; Builder : System.Address);
      pragma Import (C, Internal, "gtk_buildable_parser_finished");
   begin
      Internal (Self, Get_Object (Builder));
   end Parser_Finished;

   ----------------------------
   -- Set_Buildable_Property --
   ----------------------------

   procedure Set_Buildable_Property
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String;
       Value   : in out Glib.Values.GValue)
   is
      procedure Internal
         (Self    : Gtk_Buildable;
          Builder : System.Address;
          Name    : Gtkada.Types.Chars_Ptr;
          Value   : in out Glib.Values.GValue);
      pragma Import (C, Internal, "gtk_buildable_set_buildable_property");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Self, Get_Object (Builder), Tmp_Name, Value);
      Free (Tmp_Name);
   end Set_Buildable_Property;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (Self : Gtk_Buildable; Name : UTF8_String) is
      procedure Internal
         (Self : Gtk_Buildable;
          Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_buildable_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
   begin
      Internal (Self, Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   function "+" (W : Gtk_Buildable) return Gtk_Buildable is
   begin
      return W;
   end "+";

end Gtk.Buildable;
