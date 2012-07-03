------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Gtkada.Bindings;      use Gtkada.Bindings;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.Editable is

   procedure Insert_Text
     (Editable : Gtk_Editable;
      New_Text : UTF8_String;
      Position : in out Gint) is
   begin
      Insert_Text
        (Editable, New_Text & ASCII.NUL, New_Text'Length, Position);
   end Insert_Text;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Editable  : Gtk_Editable;
       Start_Pos : Gint;
       End_Pos   : Gint := -1) return UTF8_String
   is
      function Internal
         (Editable  : Gtk_Editable;
          Start_Pos : Gint;
          End_Pos   : Gint) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Editable, Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable (Editable : Gtk_Editable) return Boolean is
      function Internal (Editable : Gtk_Editable) return Integer;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Boolean'Val (Internal (Editable));
   end Get_Editable;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Editable      : Gtk_Editable;
       Start_Pos     : out Gint;
       End_Pos       : out Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Editable      : Gtk_Editable;
          Acc_Start_Pos : access Gint;
          Acc_End_Pos   : access Gint) return Integer;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Gint;
      Acc_End_Pos   : aliased Gint;
      Tmp_Return    : Integer;
   begin
      Tmp_Return := Internal (Editable, Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Boolean'Val (Tmp_Return);
   end Get_Selection_Bounds;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Editable        : Gtk_Editable;
       New_Text        : UTF8_String;
       New_Text_Length : Gint;
       Position        : in out Gint)
   is
      procedure Internal
         (Editable        : Gtk_Editable;
          New_Text        : Interfaces.C.Strings.chars_ptr;
          New_Text_Length : Gint;
          Position        : in out Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_New_Text : Interfaces.C.Strings.chars_ptr := New_String (New_Text);
   begin
      Internal (Editable, Tmp_New_Text, New_Text_Length, Position);
      Free (Tmp_New_Text);
   end Insert_Text;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable (Editable : Gtk_Editable; Is_Editable : Boolean) is
      procedure Internal (Editable : Gtk_Editable; Is_Editable : Integer);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Editable, Boolean'Pos (Is_Editable));
   end Set_Editable;

end Gtk.Editable;
