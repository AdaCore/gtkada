------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

with Gdk.Color;   use Gdk.Color;
with System;
with Gtk.Widget;
with Ada.Unchecked_Conversion;

with Glib.Type_Conversion_Hooks;

package body Gtk.Extra.Color_Combo is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Combo_Record);
   pragma Warnings (Off, Type_Conversion);

   procedure Set_Row
     (Combo : access Gtk_Color_Combo_Record'Class;
      Row   : Gint);
   --  Set the selected row in the widget

   procedure Set_Column
     (Combo  : access Gtk_Color_Combo_Record'Class;
      Column : Gint);
   --  Set the selected column in the widget

   type Color_Access is access Gdk_Color;
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Color_Access);

   ----------------
   -- Find_Color --
   ----------------

   procedure Find_Color (Color_Combo : access Gtk_Color_Combo_Record;
                         Color       : Gdk.Color.Gdk_Color;
                         Row         : out Gint;
                         Col         : out Gint)
   is
      procedure Internal (Color_Combo : System.Address;
                          Color       : System.Address;
                          Row         : out Gint;
                          Col         : out Gint);
      pragma Import (C, Internal, "gtk_color_combo_find_color");

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;
   begin
      if Color = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Color_Combo), Ca, Row, Col);
   end Find_Color;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Color_Combo) is
   begin
      Widget := new Gtk_Color_Combo_Record;
      Gtk.Extra.Color_Combo.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Color_Combo_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_combo_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Color_Combo;
      Nrows  : Gint;
      Ncols  : Gint;
      Values : Gdk.Color.Gdk_Color_Array) is
   begin
      Widget := new Gtk_Color_Combo_Record;
      Initialize (Widget, Nrows, Ncols, Values);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Color_Combo_Record;
      Nrows  : Gint;
      Ncols  : Gint;
      Values : Gdk.Color.Gdk_Color_Array)
   is
      function Internal (Nrows  : Gint;
                         Ncols  : Gint;
                         Values : System.Address)
                        return System.Address;
      pragma Import (C, Internal, "gtk_color_combo_new_with_values");
   begin
      Set_Object
        (Widget, Internal (Nrows, Ncols,  Values (Values'First)'Address));
   end Initialize;

   ------------------
   -- Get_Color_At --
   ------------------

   function Get_Color_At (Widget : access Gtk_Color_Combo_Record;
                          Row    : Gint;
                          Col    : Gint)
                         return Gdk_Color
   is
      function Internal
        (Widget : System.Address;
         Row    : Gint;
         Col    : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_color_combo_get_color_at");

      Color : Color_Access;
   begin
      Color := Convert (Internal (Get_Object (Widget), Row, Col));
      return Color.all;
   end Get_Color_At;

   ---------------
   -- Set_Color --
   ---------------

   function Set_Color
     (Color_Combo : access Gtk_Color_Combo_Record;
      Name        : String)
      return Boolean
   is
      Color : Gdk_Color;
   begin
      Color := Parse (Name);
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      return Set_Color (Color_Combo, Color);
   end Set_Color;

   ---------------
   -- Set_Color --
   ---------------

   function Set_Color
     (Color_Combo : access Gtk_Color_Combo_Record;
      Color       : Gdk.Color.Gdk_Color)
      return Boolean
   is
      Row, Col : Gint;
   begin
      Find_Color (Color_Combo, Color, Row, Col);

      if Row = -1 or else Col = -1 then
         return False;
      end if;

      --  ??? Need to press the button in the popup window

      Set_Row (Color_Combo, Row);
      Set_Column (Color_Combo, Col);
      Changed (Color_Combo, Row, Col);
      return True;
   end Set_Color;

   -------------
   -- Changed --
   -------------

   procedure Changed
     (Color_Combo : access Gtk_Color_Combo_Record;
      Row : Gint;
      Col : Gint)
   is
      procedure Internal
        (Combo     : System.Address;
         Signal    : String;
         Selection : Gint;
         Color     : System.Address);
      pragma Import (C, Internal, "ada_g_signal_emit_by_name_int_ptr");

      C : aliased constant Gdk_Color := Get_Color_At (Color_Combo, Row, Col);
   begin
      Internal (Get_Object (Color_Combo), "changed" & ASCII.NUL,
                Row * Get_Ncols (Color_Combo) + Col, C'Address);
   end Changed;

   ---------------
   -- Get_Ncols --
   ---------------

   function Get_Ncols (Color_Combo : access Gtk_Color_Combo_Record)
      return Gint
   is
      function Internal (Combo : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_extra_color_combo_get_ncols");
   begin
      return Internal (Get_Object (Color_Combo));
   end Get_Ncols;

   ---------------
   -- Get_Nrows --
   ---------------

   function Get_Nrows (Color_Combo : access Gtk_Color_Combo_Record)
      return Gint
   is
      function Internal (Combo : System.Address) return Gint;
      pragma Import (C, Internal, "ada_gtk_extra_color_combo_get_nrows");
   begin
      return Internal (Get_Object (Color_Combo));
   end Get_Nrows;

   -------------
   -- Set_Row --
   -------------

   procedure Set_Row
     (Combo : access Gtk_Color_Combo_Record'Class;
      Row   : Gint)
   is
      procedure Internal (Combo : System.Address; Row : Gint);
      pragma Import (C, Internal, "ada_gtk_extra_color_combo_set_row");
   begin
      Internal (Get_Object (Combo), Row);
   end Set_Row;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column
     (Combo  : access Gtk_Color_Combo_Record'Class;
      Column : Gint)
   is
      procedure Internal (Combo : System.Address; Column : Gint);
      pragma Import (C, Internal, "ada_gtk_extra_color_combo_set_column");
   begin
      Internal (Get_Object (Combo), Column);
   end Set_Column;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Color_Combo : access Gtk_Color_Combo_Record)
      return Gdk.Color.Gdk_Color
   is
      function Internal (Combo : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_color_combo_get_selection");

      C : constant Color_Access :=
        Convert (Internal (Get_Object (Color_Combo)));
   begin
      return C.all;
   end Get_Selection;

end Gtk.Extra.Color_Combo;
