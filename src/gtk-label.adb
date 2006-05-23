-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);
with Pango.Attributes;     use Pango.Attributes;
with Pango.Layout;         use Pango.Layout;
with Gtk.Widget;           use Gtk.Widget;

package body Gtk.Label is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return GObject;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ---------
   -- Get --
   ---------

   function Get_Text (Label : access Gtk_Label_Record) return UTF8_String is
      function Internal (Label : System.Address) return C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_label_get_text");

   begin
      return Value (Internal (Get_Object (Label)));
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "") is
   begin
      Label := new Gtk_Label_Record;
      Initialize (Label, Str);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Label : out Gtk_Label; Str : UTF8_String) is
   begin
      Label := new Gtk_Label_Record;
      Initialize_With_Mnemonic (Label, Str);
   end Gtk_New_With_Mnemonic;

   -------------------
   -- Get_Line_Wrap --
   -------------------

   function Get_Line_Wrap (Label : access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_label_get_line_wrap");

   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Line_Wrap;

   -----------------
   -- Get_Justify --
   -----------------

   function Get_Justify
     (Label : access Gtk_Label_Record) return Enums.Gtk_Justification
   is
      function Internal
        (Label : System.Address) return Enums.Gtk_Justification;
      pragma Import (C, Internal, "gtk_label_get_justify");

   begin
      return Internal (Get_Object (Label));
   end Get_Justify;

   --------------------
   -- Get_Selectable --
   --------------------

   function Get_Selectable (Label : access Gtk_Label_Record) return Boolean is
      function Internal (Label : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_label_get_selectable");

   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Selectable;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup (Label : access Gtk_Label_Record) return Boolean is
      function Internal (Label : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_label_get_use_markup");

   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
     (Label : access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_label_get_use_underline");

   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Use_Underline;

   -------------------------
   -- Get_Mnemonic_Keyval --
   -------------------------

   function Get_Mnemonic_Keyval
     (Label : access Gtk_Label_Record) return Gdk.Types.Gdk_Key_Type
   is
      function Internal (Label : System.Address) return Gdk.Types.Gdk_Key_Type;
      pragma Import (C, Internal, "gtk_label_get_mnemonic_keyval");

   begin
      return Internal (Get_Object (Label));
   end Get_Mnemonic_Keyval;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String)
   is
      function Internal (Str : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");

   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String)
   is
      function Internal (Str : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new_with_mnemonic");

   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
   end Initialize_With_Mnemonic;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Label        : access Gtk_Label_Record;
      Start_Offset : Integer := -1;
      End_Offset   : Integer := -1)
   is
      procedure Internal (Label : System.Address; Start_Offset : Gint;
         End_Offset : Gint);
      pragma Import (C, Internal, "gtk_label_select_region");

   begin
      Internal (Get_Object (Label), Gint (Start_Offset), Gint
         (End_Offset));
   end Select_Region;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify
     (Label : access Gtk_Label_Record;
      Jtype : Enums.Gtk_Justification)
   is
      procedure Internal
        (Label : System.Address; Jtype : Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_label_set_justify");

   begin
      Internal (Get_Object (Label), Jtype);
   end Set_Justify;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Label : access Gtk_Label_Record; Str : UTF8_String) is
      procedure Internal (Label : System.Address; Str : UTF8_String);
      pragma Import (C, Internal, "gtk_label_set_text");

   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Text;

   ----------------------------
   -- Set_Text_With_Mnemonic --
   ----------------------------

   procedure Set_Text_With_Mnemonic
     (Label : access Gtk_Label_Record;
      Str   : UTF8_String)
   is
      procedure Internal (Label : System.Address; Str : UTF8_String);
      pragma Import (C, Internal, "gtk_label_set_text_with_mnemonic");

   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Text_With_Mnemonic;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup (Label : access Gtk_Label_Record; Str : UTF8_String) is
      procedure Internal (Label : System.Address; Str : UTF8_String);
      pragma Import (C, Internal, "gtk_label_set_markup");

   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Markup;

   ------------------------------
   -- Set_Markup_With_Mnemonic --
   ------------------------------

   procedure Set_Markup_With_Mnemonic
     (Label : access Gtk_Label_Record;
      Str   : UTF8_String)
   is
      procedure Internal (Label : System.Address; Str : UTF8_String);
      pragma Import (C, Internal, "gtk_label_set_markup_with_mnemonic");

   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Markup_With_Mnemonic;

   -------------------------
   -- Set_Mnemonic_Widget --
   -------------------------

   procedure Set_Mnemonic_Widget
     (Label  : access Gtk_Label_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Label : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_label_set_mnemonic_widget");

   begin
      Internal (Get_Object (Label), Get_Object (Widget));
   end Set_Mnemonic_Widget;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
     (Label : access Gtk_Label_Record; Pattern : String)
   is
      procedure Internal (Label : System.Address; Pattern : String);
      pragma Import (C, Internal, "gtk_label_set_pattern");

   begin
      Internal (Get_Object (Label), Pattern & ASCII.NUL);
   end Set_Pattern;

   -------------------
   -- Set_Line_Wrap --
   -------------------

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record; Wrap : Boolean) is
      procedure Internal (Label : System.Address; Wrap : Gint);
      pragma Import (C, Internal, "gtk_label_set_line_wrap");

   begin
      Internal (Get_Object (Label), Boolean'Pos (Wrap));
   end Set_Line_Wrap;

   --------------------
   -- Set_Selectable --
   --------------------

   procedure Set_Selectable
     (Label      : access Gtk_Label_Record;
      Selectable : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Gint);
      pragma Import (C, Internal, "gtk_label_set_selectable");

   begin
      Internal (Get_Object (Label), Boolean'Pos (Selectable));
   end Set_Selectable;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
     (Label  : access Gtk_Label_Record;
      Markup : Boolean)
   is
      procedure Internal (Label : System.Address; Markup : Gint);
      pragma Import (C, Internal, "gtk_label_set_use_markup");

   begin
      Internal (Get_Object (Label), Boolean'Pos (Markup));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
     (Label     : access Gtk_Label_Record;
      Underline : Boolean)
   is
      procedure Internal (Label : System.Address; Underline : Gint);
      pragma Import (C, Internal, "gtk_label_set_use_underline");

   begin
      Internal (Get_Object (Label), Boolean'Pos (Underline));
   end Set_Use_Underline;

   ---------------
   -- Get_Angle --
   ---------------

   function Get_Angle
     (Label : access Gtk_Label_Record) return Gdouble
   is
      function Internal (Label : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_label_get_angle");
   begin
      return Internal (Get_Object (Label));
   end Get_Angle;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
     (Label : access Gtk_Label_Record) return Pango_Ellipsize_Mode
   is
      function Internal (Label : System.Address) return Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_label_get_ellipsize");
   begin
      return Internal (Get_Object (Label));
   end Get_Ellipsize;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Label : access Gtk_Label_Record) return String is
      function Internal
        (Label : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_label_get_label");
   begin
      return Value (Internal (Get_Object (Label)));
   end Get_Label;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (Label : access Gtk_Label_Record) return Pango_Layout is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_layout");
      Stub : Pango_Layout_Record;
   begin
      return Pango_Layout
        (Get_User_Data (Internal (Get_Object (Label)), Stub));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
     (Label : access Gtk_Label_Record;
      X, Y  : out Gint)
   is
      procedure Internal (Label : System.Address; X, Y  : out Gint);
      pragma Import (C, Internal, "gtk_label_get_layout_offsets");
   begin
      Internal (Get_Object (Label), X, Y);
   end Get_Layout_Offsets;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
     (Label : access Gtk_Label_Record) return Gint
   is
      function Internal (Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_label_get_max_width_chars");
   begin
      return Internal (Get_Object (Label));
   end Get_Max_Width_Chars;

   -------------------------
   -- Get_Mnemonic_Widget --
   -------------------------

   function Get_Mnemonic_Widget
     (Label : access Gtk_Label_Record) return Gtk_Widget
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_mnemonic_widget");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Label)), Stub));
   end Get_Mnemonic_Widget;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Label         : access Gtk_Label_Record;
      First, Last   : out Gint;
      Has_Selection : out Boolean)
   is
      function Internal
        (Label : System.Address;
         First : access Gint;
         Last  : access Gint)
         return Gboolean;
      pragma Import (C, Internal, "gtk_label_get_selection_bounds");
      F, L : aliased Gint;
   begin
      Has_Selection := Boolean'Val
        (Internal (Get_Object (Label), F'Access, L'Access));
      First := F;
      Last  := L;
   end Get_Selection_Bounds;

   --------------------------
   -- Get_Single_Line_Mode --
   --------------------------

   function Get_Single_Line_Mode
     (Label : access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_label_get_single_line_mode");
   begin
      return Boolean'Val (Internal (Get_Object (Label)));
   end Get_Single_Line_Mode;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
     (Label : access Gtk_Label_Record) return Gint
   is
      function Internal (Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_label_get_width_chars");
   begin
      return Internal (Get_Object (Label));
   end Get_Width_Chars;

   ---------------
   -- Set_Angle --
   ---------------

   procedure Set_Angle
     (Label : access Gtk_Label_Record; Angle : Gdouble)
   is
      procedure Internal (Label : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_label_set_angle");
   begin
      Internal (Get_Object (Label), Angle);
   end Set_Angle;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
     (Label : access Gtk_Label_Record;
      Mode  : Pango_Ellipsize_Mode)
   is
      procedure Internal (Label : System.Address; Mode : Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "gtk_label_set_ellipsize");
   begin
      Internal (Get_Object (Label), Mode);
   end Set_Ellipsize;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (Label : access Gtk_Label_Record; Str : String) is
      procedure Internal (Label : System.Address; Str : String);
      pragma Import (C, Internal, "gtk_label_set_label");
   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Label;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
     (Label   : access Gtk_Label_Record;
      N_Chars : Gint)
   is
      procedure Internal (Label : System.Address; N_Chars : Gint);
      pragma Import (C, Internal, "gtk_label_set_max_width_chars");
   begin
      Internal (Get_Object (Label), N_Chars);
   end Set_Max_Width_Chars;

   --------------------------
   -- Set_Single_Line_Mode --
   --------------------------

   procedure Set_Single_Line_Mode
     (Label            : access Gtk_Label_Record;
      Single_Line_Mode : Boolean)
   is
      procedure Internal
        (Label            : System.Address;
         Single_Line_Mode : Gboolean);
      pragma Import (C, Internal, "gtk_label_set_single_line_mode");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Single_Line_Mode));
   end Set_Single_Line_Mode;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
     (Label   : access Gtk_Label_Record;
      N_Chars : Gint)
   is
      procedure Internal (Label : System.Address; N_Chars : Gint);
      pragma Import (C, Internal, "gtk_label_set_width_chars");
   begin
      Internal (Get_Object (Label), N_Chars);
   end Set_Width_Chars;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
     (Label : access Gtk_Label_Record)
      return Pango_Attr_List
   is
      function Internal (Label : System.Address) return Pango_Attr_List;
      pragma Import (C, Internal, "gtk_label_get_attributes");
   begin
      return Internal (Get_Object (Label));
   end Get_Attributes;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
     (Label : access Gtk_Label_Record;
      Attrs : Pango_Attr_List)
   is
      procedure Internal (Label : System.Address; Attrs : Pango_Attr_List);
      pragma Import (C, Internal, "gtk_label_set_attributes");
   begin
      Internal (Get_Object (Label), Attrs);
   end Set_Attributes;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return GObject is
   begin
      if Type_Name = "GtkLabel" then
         return new Gtk_Label_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Glib.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.Label;
