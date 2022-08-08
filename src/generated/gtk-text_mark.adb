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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Text_Mark is

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

   package Type_Conversion_Gtk_Text_Mark is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_Mark_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Text_Mark);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Mark         : out Gtk_Text_Mark;
       Name         : UTF8_String := "";
       Left_Gravity : Boolean)
   is
   begin
      Mark := new Gtk_Text_Mark_Record;
      Gtk.Text_Mark.Initialize (Mark, Name, Left_Gravity);
   end Gtk_New;

   -----------------------
   -- Gtk_Text_Mark_New --
   -----------------------

   function Gtk_Text_Mark_New
      (Name         : UTF8_String := "";
       Left_Gravity : Boolean) return Gtk_Text_Mark
   is
      Mark : constant Gtk_Text_Mark := new Gtk_Text_Mark_Record;
   begin
      Gtk.Text_Mark.Initialize (Mark, Name, Left_Gravity);
      return Mark;
   end Gtk_Text_Mark_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Mark         : not null access Gtk_Text_Mark_Record'Class;
       Name         : UTF8_String := "";
       Left_Gravity : Boolean)
   is
      function Internal
         (Name         : Gtkada.Types.Chars_Ptr;
          Left_Gravity : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_text_mark_new");
      Tmp_Name   : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Mark.Is_Created then
         if Name = "" then
            Tmp_Name := Gtkada.Types.Null_Ptr;
         else
            Tmp_Name := New_String (Name);
         end if;
         Tmp_Return := Internal (Tmp_Name, Boolean'Pos (Left_Gravity));
         Free (Tmp_Name);
         Set_Object (Mark, Tmp_Return);
      end if;
   end Initialize;

   -----------------
   -- Get_Deleted --
   -----------------

   function Get_Deleted
      (Mark : not null access Gtk_Text_Mark_Record) return Boolean
   is
      function Internal (Mark : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_mark_get_deleted");
   begin
      return Internal (Get_Object (Mark)) /= 0;
   end Get_Deleted;

   ----------------------
   -- Get_Left_Gravity --
   ----------------------

   function Get_Left_Gravity
      (Mark : not null access Gtk_Text_Mark_Record) return Boolean
   is
      function Internal (Mark : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_mark_get_left_gravity");
   begin
      return Internal (Get_Object (Mark)) /= 0;
   end Get_Left_Gravity;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
      (Mark : not null access Gtk_Text_Mark_Record) return UTF8_String
   is
      function Internal
         (Mark : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_text_mark_get_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Mark)));
   end Get_Name;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Mark : not null access Gtk_Text_Mark_Record) return Boolean
   is
      function Internal (Mark : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_mark_get_visible");
   begin
      return Internal (Get_Object (Mark)) /= 0;
   end Get_Visible;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Mark    : not null access Gtk_Text_Mark_Record;
       Setting : Boolean)
   is
      procedure Internal (Mark : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_mark_set_visible");
   begin
      Internal (Get_Object (Mark), Boolean'Pos (Setting));
   end Set_Visible;

end Gtk.Text_Mark;
