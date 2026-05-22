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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
with System;

package body Gdk.Cursor is

   package Type_Conversion_Gdk_Cursor is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gdk_Cursor_Record);
   pragma Unreferenced (Type_Conversion_Gdk_Cursor);

   ------------------------------
   -- Gdk_Cursor_New_From_Name --
   ------------------------------

   function Gdk_Cursor_New_From_Name
      (Name     : UTF8_String;
       Fallback : access Gdk_Cursor_Record'Class) return Gdk_Cursor
   is
      Self : constant Gdk_Cursor := new Gdk_Cursor_Record;
   begin
      Gdk.Cursor.Initialize_From_Name (Self, Name, Fallback);
      return Self;
   end Gdk_Cursor_New_From_Name;

   -----------------------
   -- Gdk_New_From_Name --
   -----------------------

   procedure Gdk_New_From_Name
      (Self     : out Gdk_Cursor;
       Name     : UTF8_String;
       Fallback : access Gdk_Cursor_Record'Class)
   is
   begin
      Self := new Gdk_Cursor_Record;
      Gdk.Cursor.Initialize_From_Name (Self, Name, Fallback);
   end Gdk_New_From_Name;

   --------------------------
   -- Initialize_From_Name --
   --------------------------

   procedure Initialize_From_Name
      (Self     : not null access Gdk_Cursor_Record'Class;
       Name     : UTF8_String;
       Fallback : access Gdk_Cursor_Record'Class)
   is
      function Internal
         (Name     : Gtkada.Types.Chars_Ptr;
          Fallback : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_cursor_new_from_name");
      Tmp_Name   : Gtkada.Types.Chars_Ptr := New_String (Name);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Name, Get_Object_Or_Null (GObject (Fallback)));
         Free (Tmp_Name);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize_From_Name;

   ------------------
   -- Get_Fallback --
   ------------------

   function Get_Fallback
      (Self : not null access Gdk_Cursor_Record) return Gdk_Cursor
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gdk_cursor_get_fallback");
      Stub_Gdk_Cursor : Gdk_Cursor_Record;
   begin
      return Gdk.Cursor.Gdk_Cursor (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Cursor));
   end Get_Fallback;

   -------------------
   -- Get_Hotspot_X --
   -------------------

   function Get_Hotspot_X
      (Self : not null access Gdk_Cursor_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_cursor_get_hotspot_x");
   begin
      return Internal (Get_Object (Self));
   end Get_Hotspot_X;

   -------------------
   -- Get_Hotspot_Y --
   -------------------

   function Get_Hotspot_Y
      (Self : not null access Gdk_Cursor_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gdk_cursor_get_hotspot_y");
   begin
      return Internal (Get_Object (Self));
   end Get_Hotspot_Y;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Self : not null access Gdk_Cursor_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gdk_cursor_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Name;

end Gdk.Cursor;
