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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Label is

   package Type_Conversion_Gtk_Label is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Label_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Label);

   -------------------
   -- Gtk_Label_New --
   -------------------

   function Gtk_Label_New (Str : UTF8_String := "") return Gtk_Label is
      Label : constant Gtk_Label := new Gtk_Label_Record;
   begin
      Gtk.Label.Initialize (Label, Str);
      return Label;
   end Gtk_Label_New;

   ---------------------------------
   -- Gtk_Label_New_With_Mnemonic --
   ---------------------------------

   function Gtk_Label_New_With_Mnemonic
      (Str : UTF8_String := "") return Gtk_Label
   is
      Label : constant Gtk_Label := new Gtk_Label_Record;
   begin
      Gtk.Label.Initialize_With_Mnemonic (Label, Str);
      return Label;
   end Gtk_Label_New_With_Mnemonic;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "") is
   begin
      Label := new Gtk_Label_Record;
      Gtk.Label.Initialize (Label, Str);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
      (Label : out Gtk_Label;
       Str   : UTF8_String := "")
   is
   begin
      Label := new Gtk_Label_Record;
      Gtk.Label.Initialize_With_Mnemonic (Label, Str);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Label : not null access Gtk_Label_Record'Class;
       Str   : UTF8_String := "")
   is
      function Internal (Str : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");
      Tmp_Str    : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Label.Is_Created then
         if Str = "" then
            Tmp_Str := Gtkada.Types.Null_Ptr;
         else
            Tmp_Str := New_String (Str);
         end if;
         Tmp_Return := Internal (Tmp_Str);
         Free (Tmp_Str);
         Set_Object (Label, Tmp_Return);
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
      (Label : not null access Gtk_Label_Record'Class;
       Str   : UTF8_String := "")
   is
      function Internal (Str : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_label_new_with_mnemonic");
      Tmp_Str    : Gtkada.Types.Chars_Ptr;
      Tmp_Return : System.Address;
   begin
      if not Label.Is_Created then
         if Str = "" then
            Tmp_Str := Gtkada.Types.Null_Ptr;
         else
            Tmp_Str := New_String (Str);
         end if;
         Tmp_Return := Internal (Tmp_Str);
         Free (Tmp_Str);
         Set_Object (Label, Tmp_Return);
      end if;
   end Initialize_With_Mnemonic;

   ---------------
   -- Get_Angle --
   ---------------

   function Get_Angle
      (Label : not null access Gtk_Label_Record) return Gdouble
   is
      function Internal (Label : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_label_get_angle");
   begin
      return Internal (Get_Object (Label));
   end Get_Angle;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Label : not null access Gtk_Label_Record)
       return Pango.Attributes.Pango_Attr_List
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_attributes");
   begin
      return From_Object (Internal (Get_Object (Label)));
   end Get_Attributes;

   ---------------------
   -- Get_Current_Uri --
   ---------------------

   function Get_Current_Uri
      (Label : not null access Gtk_Label_Record) return UTF8_String
   is
      function Internal
         (Label : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_label_get_current_uri");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Label)));
   end Get_Current_Uri;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
      (Label : not null access Gtk_Label_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Label : System.Address) return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_label_get_ellipsize");
   begin
      return Internal (Get_Object (Label));
   end Get_Ellipsize;

   -----------------
   -- Get_Justify --
   -----------------

   function Get_Justify
      (Label : not null access Gtk_Label_Record)
       return Gtk.Enums.Gtk_Justification
   is
      function Internal
         (Label : System.Address) return Gtk.Enums.Gtk_Justification;
      pragma Import (C, Internal, "gtk_label_get_justify");
   begin
      return Internal (Get_Object (Label));
   end Get_Justify;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Label : not null access Gtk_Label_Record) return UTF8_String
   is
      function Internal
         (Label : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_label_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Label)));
   end Get_Label;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (Label : not null access Gtk_Label_Record)
       return Pango.Layout.Pango_Layout
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_layout");
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Label)), Stub_Pango_Layout));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
      (Label : not null access Gtk_Label_Record;
       X     : out Glib.Gint;
       Y     : out Glib.Gint)
   is
      procedure Internal
         (Label : System.Address;
          X     : out Glib.Gint;
          Y     : out Glib.Gint);
      pragma Import (C, Internal, "gtk_label_get_layout_offsets");
   begin
      Internal (Get_Object (Label), X, Y);
   end Get_Layout_Offsets;

   -------------------
   -- Get_Line_Wrap --
   -------------------

   function Get_Line_Wrap
      (Label : not null access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_line_wrap");
   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Line_Wrap;

   ------------------------
   -- Get_Line_Wrap_Mode --
   ------------------------

   function Get_Line_Wrap_Mode
      (Label : not null access Gtk_Label_Record)
       return Pango.Enums.Wrap_Mode
   is
      function Internal
         (Label : System.Address) return Pango.Enums.Wrap_Mode;
      pragma Import (C, Internal, "gtk_label_get_line_wrap_mode");
   begin
      return Internal (Get_Object (Label));
   end Get_Line_Wrap_Mode;

   ---------------
   -- Get_Lines --
   ---------------

   function Get_Lines
      (Label : not null access Gtk_Label_Record) return Glib.Gint
   is
      function Internal (Label : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_label_get_lines");
   begin
      return Internal (Get_Object (Label));
   end Get_Lines;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
      (Label : not null access Gtk_Label_Record) return Glib.Gint
   is
      function Internal (Label : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_label_get_max_width_chars");
   begin
      return Internal (Get_Object (Label));
   end Get_Max_Width_Chars;

   -------------------------
   -- Get_Mnemonic_Keyval --
   -------------------------

   function Get_Mnemonic_Keyval
      (Label : not null access Gtk_Label_Record) return Guint
   is
      function Internal (Label : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_label_get_mnemonic_keyval");
   begin
      return Internal (Get_Object (Label));
   end Get_Mnemonic_Keyval;

   -------------------------
   -- Get_Mnemonic_Widget --
   -------------------------

   function Get_Mnemonic_Widget
      (Label : not null access Gtk_Label_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Label : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_label_get_mnemonic_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Label)), Stub_Gtk_Widget));
   end Get_Mnemonic_Widget;

   --------------------
   -- Get_Selectable --
   --------------------

   function Get_Selectable
      (Label : not null access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_selectable");
   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Selectable;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Label         : not null access Gtk_Label_Record;
       Start         : out Glib.Gint;
       The_End       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Label       : System.Address;
          Acc_Start   : access Glib.Gint;
          Acc_The_End : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_selection_bounds");
      Acc_Start   : aliased Glib.Gint;
      Acc_The_End : aliased Glib.Gint;
      Tmp_Return  : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Label), Acc_Start'Access, Acc_The_End'Access);
      Start := Acc_Start;
      The_End := Acc_The_End;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   --------------------------
   -- Get_Single_Line_Mode --
   --------------------------

   function Get_Single_Line_Mode
      (Label : not null access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_single_line_mode");
   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Single_Line_Mode;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Label : not null access Gtk_Label_Record) return UTF8_String
   is
      function Internal
         (Label : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_label_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Label)));
   end Get_Text;

   -----------------------------
   -- Get_Track_Visited_Links --
   -----------------------------

   function Get_Track_Visited_Links
      (Label : not null access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_track_visited_links");
   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Track_Visited_Links;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup
      (Label : not null access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_use_markup");
   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
      (Label : not null access Gtk_Label_Record) return Boolean
   is
      function Internal (Label : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_label_get_use_underline");
   begin
      return Internal (Get_Object (Label)) /= 0;
   end Get_Use_Underline;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
      (Label : not null access Gtk_Label_Record) return Glib.Gint
   is
      function Internal (Label : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_label_get_width_chars");
   begin
      return Internal (Get_Object (Label));
   end Get_Width_Chars;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign
      (Label : not null access Gtk_Label_Record) return Gfloat
   is
      function Internal (Label : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_label_get_xalign");
   begin
      return Internal (Get_Object (Label));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign
      (Label : not null access Gtk_Label_Record) return Gfloat
   is
      function Internal (Label : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_label_get_yalign");
   begin
      return Internal (Get_Object (Label));
   end Get_Yalign;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Label        : not null access Gtk_Label_Record;
       Start_Offset : Glib.Gint := -1;
       End_Offset   : Glib.Gint := -1)
   is
      procedure Internal
         (Label        : System.Address;
          Start_Offset : Glib.Gint;
          End_Offset   : Glib.Gint);
      pragma Import (C, Internal, "gtk_label_select_region");
   begin
      Internal (Get_Object (Label), Start_Offset, End_Offset);
   end Select_Region;

   ---------------
   -- Set_Angle --
   ---------------

   procedure Set_Angle
      (Label : not null access Gtk_Label_Record;
       Angle : Gdouble)
   is
      procedure Internal (Label : System.Address; Angle : Gdouble);
      pragma Import (C, Internal, "gtk_label_set_angle");
   begin
      Internal (Get_Object (Label), Angle);
   end Set_Angle;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Label : not null access Gtk_Label_Record;
       Attrs : Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal (Label : System.Address; Attrs : System.Address);
      pragma Import (C, Internal, "gtk_label_set_attributes");
   begin
      Internal (Get_Object (Label), Get_Object (Attrs));
   end Set_Attributes;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
      (Label : not null access Gtk_Label_Record;
       Mode  : Pango.Layout.Pango_Ellipsize_Mode)
   is
      procedure Internal
         (Label : System.Address;
          Mode  : Pango.Layout.Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "gtk_label_set_ellipsize");
   begin
      Internal (Get_Object (Label), Mode);
   end Set_Ellipsize;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify
      (Label : not null access Gtk_Label_Record;
       Jtype : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
         (Label : System.Address;
          Jtype : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_label_set_justify");
   begin
      Internal (Get_Object (Label), Jtype);
   end Set_Justify;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_label_set_label");
      Tmp_Str : Gtkada.Types.Chars_Ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Label;

   -------------------
   -- Set_Line_Wrap --
   -------------------

   procedure Set_Line_Wrap
      (Label : not null access Gtk_Label_Record;
       Wrap  : Boolean)
   is
      procedure Internal (Label : System.Address; Wrap : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_label_set_line_wrap");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Wrap));
   end Set_Line_Wrap;

   ------------------------
   -- Set_Line_Wrap_Mode --
   ------------------------

   procedure Set_Line_Wrap_Mode
      (Label     : not null access Gtk_Label_Record;
       Wrap_Mode : Pango.Enums.Wrap_Mode)
   is
      procedure Internal
         (Label     : System.Address;
          Wrap_Mode : Pango.Enums.Wrap_Mode);
      pragma Import (C, Internal, "gtk_label_set_line_wrap_mode");
   begin
      Internal (Get_Object (Label), Wrap_Mode);
   end Set_Line_Wrap_Mode;

   ---------------
   -- Set_Lines --
   ---------------

   procedure Set_Lines
      (Label : not null access Gtk_Label_Record;
       Lines : Glib.Gint)
   is
      procedure Internal (Label : System.Address; Lines : Glib.Gint);
      pragma Import (C, Internal, "gtk_label_set_lines");
   begin
      Internal (Get_Object (Label), Lines);
   end Set_Lines;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_label_set_markup");
      Tmp_Str : Gtkada.Types.Chars_Ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Markup;

   ------------------------------
   -- Set_Markup_With_Mnemonic --
   ------------------------------

   procedure Set_Markup_With_Mnemonic
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_label_set_markup_with_mnemonic");
      Tmp_Str : Gtkada.Types.Chars_Ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Markup_With_Mnemonic;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
      (Label   : not null access Gtk_Label_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Label : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_label_set_max_width_chars");
   begin
      Internal (Get_Object (Label), N_Chars);
   end Set_Max_Width_Chars;

   -------------------------
   -- Set_Mnemonic_Widget --
   -------------------------

   procedure Set_Mnemonic_Widget
      (Label  : not null access Gtk_Label_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Label : System.Address; Widget : System.Address);
      pragma Import (C, Internal, "gtk_label_set_mnemonic_widget");
   begin
      Internal (Get_Object (Label), Get_Object_Or_Null (GObject (Widget)));
   end Set_Mnemonic_Widget;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
      (Label   : not null access Gtk_Label_Record;
       Pattern : UTF8_String)
   is
      procedure Internal
         (Label   : System.Address;
          Pattern : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_label_set_pattern");
      Tmp_Pattern : Gtkada.Types.Chars_Ptr := New_String (Pattern);
   begin
      Internal (Get_Object (Label), Tmp_Pattern);
      Free (Tmp_Pattern);
   end Set_Pattern;

   --------------------
   -- Set_Selectable --
   --------------------

   procedure Set_Selectable
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_label_set_selectable");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Setting));
   end Set_Selectable;

   --------------------------
   -- Set_Single_Line_Mode --
   --------------------------

   procedure Set_Single_Line_Mode
      (Label            : not null access Gtk_Label_Record;
       Single_Line_Mode : Boolean)
   is
      procedure Internal
         (Label            : System.Address;
          Single_Line_Mode : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_label_set_single_line_mode");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Single_Line_Mode));
   end Set_Single_Line_Mode;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_label_set_text");
      Tmp_Str : Gtkada.Types.Chars_Ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Text;

   ----------------------------
   -- Set_Text_With_Mnemonic --
   ----------------------------

   procedure Set_Text_With_Mnemonic
      (Label : not null access Gtk_Label_Record;
       Str   : UTF8_String)
   is
      procedure Internal
         (Label : System.Address;
          Str   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_label_set_text_with_mnemonic");
      Tmp_Str : Gtkada.Types.Chars_Ptr := New_String (Str);
   begin
      Internal (Get_Object (Label), Tmp_Str);
      Free (Tmp_Str);
   end Set_Text_With_Mnemonic;

   -----------------------------
   -- Set_Track_Visited_Links --
   -----------------------------

   procedure Set_Track_Visited_Links
      (Label       : not null access Gtk_Label_Record;
       Track_Links : Boolean)
   is
      procedure Internal
         (Label       : System.Address;
          Track_Links : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_label_set_track_visited_links");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Track_Links));
   end Set_Track_Visited_Links;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_label_set_use_markup");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Setting));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
      (Label   : not null access Gtk_Label_Record;
       Setting : Boolean)
   is
      procedure Internal (Label : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_label_set_use_underline");
   begin
      Internal (Get_Object (Label), Boolean'Pos (Setting));
   end Set_Use_Underline;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (Label   : not null access Gtk_Label_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Label : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_label_set_width_chars");
   begin
      Internal (Get_Object (Label), N_Chars);
   end Set_Width_Chars;

   ----------------
   -- Set_Xalign --
   ----------------

   procedure Set_Xalign
      (Label  : not null access Gtk_Label_Record;
       Xalign : Gfloat)
   is
      procedure Internal (Label : System.Address; Xalign : Gfloat);
      pragma Import (C, Internal, "gtk_label_set_xalign");
   begin
      Internal (Get_Object (Label), Xalign);
   end Set_Xalign;

   ----------------
   -- Set_Yalign --
   ----------------

   procedure Set_Yalign
      (Label  : not null access Gtk_Label_Record;
       Yalign : Gfloat)
   is
      procedure Internal (Label : System.Address; Yalign : Gfloat);
      pragma Import (C, Internal, "gtk_label_set_yalign");
   begin
      Internal (Get_Object (Label), Yalign);
   end Set_Yalign;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Label_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Label_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Label_UTF8_String_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Label_UTF8_String_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Label_Gtk_Menu_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Label_Gtk_Menu_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Menu_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Menu_Void);

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_UTF8_String_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_Gtk_Menu_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Menu_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Menu_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Menu_Void);

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void);

   procedure Marsh_GObject_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Boolean);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Label_Gtk_Menu_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Label_Gtk_Menu_Void);

   procedure Marsh_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void);

   procedure Marsh_Gtk_Label_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Label_UTF8_String_Boolean);

   procedure Marsh_Gtk_Label_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Label_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Label_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_UTF8_String_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Label_UTF8_String_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Label_Gtk_Menu_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Label_Gtk_Menu_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Label_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Menu_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Menu_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------
   -- Marsh_GObject_Gtk_Menu_Void --
   ---------------------------------

   procedure Marsh_GObject_Gtk_Menu_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Menu_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Menu.Gtk_Menu (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Menu_Void;

   -------------------------------------------------------
   -- Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void --
   -------------------------------------------------------

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void;

   ---------------------------------------
   -- Marsh_GObject_UTF8_String_Boolean --
   ---------------------------------------

   procedure Marsh_GObject_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Boolean;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -----------------------------------
   -- Marsh_Gtk_Label_Gtk_Menu_Void --
   -----------------------------------

   procedure Marsh_Gtk_Label_Gtk_Menu_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Label_Gtk_Menu_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Label := Gtk_Label (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Menu.Gtk_Menu (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Label_Gtk_Menu_Void;

   ---------------------------------------------------------
   -- Marsh_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void --
   ---------------------------------------------------------

   procedure Marsh_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Label := Gtk_Label (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void;

   -----------------------------------------
   -- Marsh_Gtk_Label_UTF8_String_Boolean --
   -----------------------------------------

   procedure Marsh_Gtk_Label_UTF8_String_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Label_UTF8_String_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Label := Gtk_Label (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_UTF8_String (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Label_UTF8_String_Boolean;

   --------------------------
   -- Marsh_Gtk_Label_Void --
   --------------------------

   procedure Marsh_Gtk_Label_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Label_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Label := Gtk_Label (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Label_Void;

   ------------------------------
   -- On_Activate_Current_Link --
   ------------------------------

   procedure On_Activate_Current_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-current-link" & ASCII.NUL, Call, After);
   end On_Activate_Current_Link;

   ------------------------------
   -- On_Activate_Current_Link --
   ------------------------------

   procedure On_Activate_Current_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-current-link" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Current_Link;

   ----------------------
   -- On_Activate_Link --
   ----------------------

   procedure On_Activate_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_UTF8_String_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate-link" & ASCII.NUL, Call, After);
   end On_Activate_Link;

   ----------------------
   -- On_Activate_Link --
   ----------------------

   procedure On_Activate_Link
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate-link" & ASCII.NUL, Call, After, Slot);
   end On_Activate_Link;

   -----------------------
   -- On_Copy_Clipboard --
   -----------------------

   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "copy-clipboard" & ASCII.NUL, Call, After);
   end On_Copy_Clipboard;

   -----------------------
   -- On_Copy_Clipboard --
   -----------------------

   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "copy-clipboard" & ASCII.NUL, Call, After, Slot);
   end On_Copy_Clipboard;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Gtk_Movement_Step_Gint_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   -----------------------
   -- On_Populate_Popup --
   -----------------------

   procedure On_Populate_Popup
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_Gtk_Label_Gtk_Menu_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "populate-popup" & ASCII.NUL, Call, After);
   end On_Populate_Popup;

   -----------------------
   -- On_Populate_Popup --
   -----------------------

   procedure On_Populate_Popup
      (Self  : not null access Gtk_Label_Record;
       Call  : Cb_GObject_Gtk_Menu_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "populate-popup" & ASCII.NUL, Call, After, Slot);
   end On_Populate_Popup;

end Gtk.Label;
