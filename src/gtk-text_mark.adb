------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Interfaces.C.Strings;
with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Text_Mark is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Mark_Record);
   pragma Warnings (Off, Type_Conversion);

   package ICS renames Interfaces.C.Strings;

   -----------------
   -- Get_Deleted --
   -----------------

   function Get_Deleted (Mark : access Gtk_Text_Mark_Record) return Boolean is
      function Internal (Mark : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_mark_get_deleted");

   begin
      return Internal (Get_Object (Mark)) /= 0;
   end Get_Deleted;

   ----------------------
   -- Get_Left_Gravity --
   ----------------------

   function Get_Left_Gravity
     (Mark : access Gtk_Text_Mark_Record) return Boolean
   is
      function Internal (Mark : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_mark_get_left_gravity");

   begin
      return Internal (Get_Object (Mark)) /= 0;
   end Get_Left_Gravity;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Mark : access Gtk_Text_Mark_Record) return String is
      function Internal (Mark : System.Address) return ICS.chars_ptr;
      pragma Import (C, Internal, "gtk_text_mark_get_name");
      --  Note: Do not free the chars_ptr returned by this function.

      Str : constant ICS.chars_ptr := Internal (Get_Object (Mark));

      use type ICS.chars_ptr;

   begin
      if Str = ICS.Null_Ptr then
         return "";
      else
         return ICS.Value (Str);
      end if;
   end Get_Name;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible (Mark : access Gtk_Text_Mark_Record) return Boolean is
      function Internal (Mark : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_text_mark_get_visible");

   begin
      return Internal (Get_Object (Mark)) /= 0;
   end Get_Visible;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
     (Mark    : access Gtk_Text_Mark_Record;
      Setting : Boolean := True)
   is
      procedure Internal (Mark : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_text_mark_set_visible");

   begin
      Internal (Get_Object (Mark), Boolean'Pos (Setting));
   end Set_Visible;

   -------------------
   -- Set_Text_Mark --
   -------------------

   procedure Set_Text_Mark
     (Val  : in out Glib.Values.GValue;
      Mark : access Gtk_Text_Mark_Record) is
   begin
      Glib.Values.Set_Address (Val, Get_Object (Mark));
   end Set_Text_Mark;

   -------------------
   -- Get_Text_Mark --
   -------------------

   function Get_Text_Mark (Val  : Glib.Values.GValue) return Gtk_Text_Mark is
      Stub : Gtk_Text_Mark_Record;
   begin
      return Gtk_Text_Mark
        (Get_User_Data_Fast (Glib.Values.Get_Address (Val), Stub));
   end Get_Text_Mark;

end Gtk.Text_Mark;
