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

package body Gtk.GEntry is

   procedure Insert_Text
     (Editable : access Gtk_Entry_Record;
      New_Text : UTF8_String;
      Position : in out Gint) is
   begin
      Insert_Text
        (Editable, New_Text & ASCII.NUL, New_Text'Length, Position);
   end Insert_Text;

   package Type_Conversion_Gtk_Entry is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Entry);

   -------------------
   -- Gtk_Entry_New --
   -------------------

   function Gtk_Entry_New return Gtk_Entry is
      The_Entry : constant Gtk_Entry := new Gtk_Entry_Record;
   begin
      Gtk.GEntry.Initialize (The_Entry);
      return The_Entry;
   end Gtk_Entry_New;

   -------------------------------
   -- Gtk_Entry_New_With_Buffer --
   -------------------------------

   function Gtk_Entry_New_With_Buffer
      (Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
       return Gtk_Entry
   is
      The_Entry : constant Gtk_Entry := new Gtk_Entry_Record;
   begin
      Gtk.GEntry.Initialize_With_Buffer (The_Entry, Buffer);
      return The_Entry;
   end Gtk_Entry_New_With_Buffer;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (The_Entry : out Gtk_Entry) is
   begin
      The_Entry := new Gtk_Entry_Record;
      Gtk.GEntry.Initialize (The_Entry);
   end Gtk_New;

   -------------------------
   -- Gtk_New_With_Buffer --
   -------------------------

   procedure Gtk_New_With_Buffer
      (The_Entry : out Gtk_Entry;
       Buffer    : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
   is
   begin
      The_Entry := new Gtk_Entry_Record;
      Gtk.GEntry.Initialize_With_Buffer (The_Entry, Buffer);
   end Gtk_New_With_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (The_Entry : not null access Gtk_Entry_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_new");
   begin
      if not The_Entry.Is_Created then
         Set_Object (The_Entry, Internal);
      end if;
   end Initialize;

   ----------------------------
   -- Initialize_With_Buffer --
   ----------------------------

   procedure Initialize_With_Buffer
      (The_Entry : not null access Gtk_Entry_Record'Class;
       Buffer    : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_new_with_buffer");
   begin
      if not The_Entry.Is_Created then
         Set_Object (The_Entry, Internal (Get_Object (Buffer)));
      end if;
   end Initialize_With_Buffer;

   ---------------------------
   -- Get_Activates_Default --
   ---------------------------

   function Get_Activates_Default
      (The_Entry : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_activates_default");
   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Activates_Default;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
      (The_Entry : not null access Gtk_Entry_Record) return Gfloat
   is
      function Internal (The_Entry : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_entry_get_alignment");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Alignment;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (The_Entry : not null access Gtk_Entry_Record)
       return Pango.Attributes.Pango_Attr_List
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_attributes");
   begin
      return From_Object (Internal (Get_Object (The_Entry)));
   end Get_Attributes;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Entry_Buffer.Gtk_Entry_Buffer
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_buffer");
      Stub_Gtk_Entry_Buffer : Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record;
   begin
      return Gtk.Entry_Buffer.Gtk_Entry_Buffer (Get_User_Data (Internal (Get_Object (The_Entry)), Stub_Gtk_Entry_Buffer));
   end Get_Buffer;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Entry_Completion.Gtk_Entry_Completion
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_completion");
      Stub_Gtk_Entry_Completion : Gtk.Entry_Completion.Gtk_Entry_Completion_Record;
   begin
      return Gtk.Entry_Completion.Gtk_Entry_Completion (Get_User_Data (Internal (Get_Object (The_Entry)), Stub_Gtk_Entry_Completion));
   end Get_Completion;

   ----------------------------------
   -- Get_Current_Icon_Drag_Source --
   ----------------------------------

   function Get_Current_Icon_Drag_Source
      (The_Entry : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (The_Entry : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_current_icon_drag_source");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Current_Icon_Drag_Source;

   ----------------------------
   -- Get_Cursor_Hadjustment --
   ----------------------------

   function Get_Cursor_Hadjustment
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_cursor_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (The_Entry)), Stub_Gtk_Adjustment));
   end Get_Cursor_Hadjustment;

   -------------------
   -- Get_Has_Frame --
   -------------------

   function Get_Has_Frame
      (The_Entry : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_has_frame");
   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Has_Frame;

   --------------------------
   -- Get_Icon_Activatable --
   --------------------------

   function Get_Icon_Activatable
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Boolean
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_icon_activatable");
   begin
      return Internal (Get_Object (The_Entry), Icon_Pos) /= 0;
   end Get_Icon_Activatable;

   -------------------
   -- Get_Icon_Area --
   -------------------

   procedure Get_Icon_Area
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Icon_Area : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Icon_Area : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_entry_get_icon_area");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Icon_Area);
   end Get_Icon_Area;

   ---------------------
   -- Get_Icon_At_Pos --
   ---------------------

   function Get_Icon_At_Pos
      (The_Entry : not null access Gtk_Entry_Record;
       X         : Glib.Gint;
       Y         : Glib.Gint) return Glib.Gint
   is
      function Internal
         (The_Entry : System.Address;
          X         : Glib.Gint;
          Y         : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_icon_at_pos");
   begin
      return Internal (Get_Object (The_Entry), X, Y);
   end Get_Icon_At_Pos;

   --------------------
   -- Get_Icon_Gicon --
   --------------------

   function Get_Icon_Gicon
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Glib.G_Icon.G_Icon
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_entry_get_icon_gicon");
   begin
      return Internal (Get_Object (The_Entry), Icon_Pos);
   end Get_Icon_Gicon;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (The_Entry), Icon_Pos));
   end Get_Icon_Name;

   ---------------------
   -- Get_Icon_Pixbuf --
   ---------------------

   function Get_Icon_Pixbuf
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_icon_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (The_Entry), Icon_Pos), Stub_Gdk_Pixbuf));
   end Get_Icon_Pixbuf;

   ------------------------
   -- Get_Icon_Sensitive --
   ------------------------

   function Get_Icon_Sensitive
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Boolean
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_icon_sensitive");
   begin
      return Internal (Get_Object (The_Entry), Icon_Pos) /= 0;
   end Get_Icon_Sensitive;

   --------------------
   -- Get_Icon_Stock --
   --------------------

   function Get_Icon_Stock
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_stock");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (The_Entry), Icon_Pos));
   end Get_Icon_Stock;

   ---------------------------
   -- Get_Icon_Storage_Type --
   ---------------------------

   function Get_Icon_Storage_Type
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return Gtk.Image.Gtk_Image_Type
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position)
          return Gtk.Image.Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_entry_get_icon_storage_type");
   begin
      return Internal (Get_Object (The_Entry), Icon_Pos);
   end Get_Icon_Storage_Type;

   -----------------------------
   -- Get_Icon_Tooltip_Markup --
   -----------------------------

   function Get_Icon_Tooltip_Markup
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_tooltip_markup");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (The_Entry), Icon_Pos));
   end Get_Icon_Tooltip_Markup;

   ---------------------------
   -- Get_Icon_Tooltip_Text --
   ---------------------------

   function Get_Icon_Tooltip_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_tooltip_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (The_Entry), Icon_Pos));
   end Get_Icon_Tooltip_Text;

   ----------------------
   -- Get_Inner_Border --
   ----------------------

   function Get_Inner_Border
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Style.Gtk_Border
   is
      function Internal
         (The_Entry : System.Address) return access Gtk.Style.Gtk_Border;
      pragma Import (C, Internal, "gtk_entry_get_inner_border");
   begin
      return Internal (Get_Object (The_Entry)).all;
   end Get_Inner_Border;

   ---------------------
   -- Get_Input_Hints --
   ---------------------

   function Get_Input_Hints
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Enums.Gtk_Input_Hints
   is
      function Internal
         (The_Entry : System.Address) return Gtk.Enums.Gtk_Input_Hints;
      pragma Import (C, Internal, "gtk_entry_get_input_hints");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Input_Hints;

   -----------------------
   -- Get_Input_Purpose --
   -----------------------

   function Get_Input_Purpose
      (The_Entry : not null access Gtk_Entry_Record)
       return Gtk.Enums.Gtk_Input_Purpose
   is
      function Internal
         (The_Entry : System.Address) return Gtk.Enums.Gtk_Input_Purpose;
      pragma Import (C, Internal, "gtk_entry_get_input_purpose");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Input_Purpose;

   ------------------------
   -- Get_Invisible_Char --
   ------------------------

   function Get_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record) return Gunichar
   is
      function Internal (The_Entry : System.Address) return Gunichar;
      pragma Import (C, Internal, "gtk_entry_get_invisible_char");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Invisible_Char;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout
      (The_Entry : not null access Gtk_Entry_Record)
       return Pango.Layout.Pango_Layout
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_layout");
      Stub_Pango_Layout : Pango.Layout.Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (The_Entry)), Stub_Pango_Layout));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
      (The_Entry : not null access Gtk_Entry_Record;
       X         : out Glib.Gint;
       Y         : out Glib.Gint)
   is
      procedure Internal
         (The_Entry : System.Address;
          X         : out Glib.Gint;
          Y         : out Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_get_layout_offsets");
   begin
      Internal (Get_Object (The_Entry), X, Y);
   end Get_Layout_Offsets;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length
      (The_Entry : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (The_Entry : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_max_length");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Max_Length;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (The_Entry : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_max_width_chars");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Max_Width_Chars;

   ------------------------
   -- Get_Overwrite_Mode --
   ------------------------

   function Get_Overwrite_Mode
      (The_Entry : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_overwrite_mode");
   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Overwrite_Mode;

   --------------------------
   -- Get_Placeholder_Text --
   --------------------------

   function Get_Placeholder_Text
      (The_Entry : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_placeholder_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (The_Entry)));
   end Get_Placeholder_Text;

   ---------------------------
   -- Get_Progress_Fraction --
   ---------------------------

   function Get_Progress_Fraction
      (The_Entry : not null access Gtk_Entry_Record) return Gdouble
   is
      function Internal (The_Entry : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_entry_get_progress_fraction");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Progress_Fraction;

   -----------------------------
   -- Get_Progress_Pulse_Step --
   -----------------------------

   function Get_Progress_Pulse_Step
      (The_Entry : not null access Gtk_Entry_Record) return Gdouble
   is
      function Internal (The_Entry : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_entry_get_progress_pulse_step");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Progress_Pulse_Step;

   --------------
   -- Get_Tabs --
   --------------

   function Get_Tabs
      (The_Entry : not null access Gtk_Entry_Record)
       return Pango.Tabs.Pango_Tab_Array
   is
      function Internal (The_Entry : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_tabs");
   begin
      return From_Object (Internal (Get_Object (The_Entry)));
   end Get_Tabs;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (The_Entry : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (The_Entry)));
   end Get_Text;

   -------------------
   -- Get_Text_Area --
   -------------------

   procedure Get_Text_Area
      (The_Entry : not null access Gtk_Entry_Record;
       Text_Area : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (The_Entry : System.Address;
          Text_Area : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_entry_get_text_area");
   begin
      Internal (Get_Object (The_Entry), Text_Area);
   end Get_Text_Area;

   ---------------------
   -- Get_Text_Length --
   ---------------------

   function Get_Text_Length
      (The_Entry : not null access Gtk_Entry_Record) return Guint16
   is
      function Internal (The_Entry : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_entry_get_text_length");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Text_Length;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
      (The_Entry : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_visibility");
   begin
      return Internal (Get_Object (The_Entry)) /= 0;
   end Get_Visibility;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (The_Entry : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_width_chars");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Width_Chars;

   ----------------------------------
   -- Grab_Focus_Without_Selecting --
   ----------------------------------

   procedure Grab_Focus_Without_Selecting
      (The_Entry : not null access Gtk_Entry_Record)
   is
      procedure Internal (The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_entry_grab_focus_without_selecting");
   begin
      Internal (Get_Object (The_Entry));
   end Grab_Focus_Without_Selecting;

   --------------------------------
   -- Im_Context_Filter_Keypress --
   --------------------------------

   function Im_Context_Filter_Keypress
      (The_Entry : not null access Gtk_Entry_Record;
       Event     : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      function Internal
         (The_Entry : System.Address;
          Event     : Gdk.Event.Gdk_Event_Key) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_im_context_filter_keypress");
   begin
      return Internal (Get_Object (The_Entry), Event) /= 0;
   end Im_Context_Filter_Keypress;

   --------------------------------
   -- Layout_Index_To_Text_Index --
   --------------------------------

   function Layout_Index_To_Text_Index
      (The_Entry    : not null access Gtk_Entry_Record;
       Layout_Index : Glib.Gint) return Glib.Gint
   is
      function Internal
         (The_Entry    : System.Address;
          Layout_Index : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_layout_index_to_text_index");
   begin
      return Internal (Get_Object (The_Entry), Layout_Index);
   end Layout_Index_To_Text_Index;

   --------------------
   -- Progress_Pulse --
   --------------------

   procedure Progress_Pulse (The_Entry : not null access Gtk_Entry_Record) is
      procedure Internal (The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_entry_progress_pulse");
   begin
      Internal (Get_Object (The_Entry));
   end Progress_Pulse;

   ----------------------
   -- Reset_Im_Context --
   ----------------------

   procedure Reset_Im_Context (The_Entry : not null access Gtk_Entry_Record) is
      procedure Internal (The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_entry_reset_im_context");
   begin
      Internal (Get_Object (The_Entry));
   end Reset_Im_Context;

   ---------------------------
   -- Set_Activates_Default --
   ---------------------------

   procedure Set_Activates_Default
      (The_Entry : not null access Gtk_Entry_Record;
       Setting   : Boolean)
   is
      procedure Internal
         (The_Entry : System.Address;
          Setting   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_activates_default");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Setting));
   end Set_Activates_Default;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (The_Entry : not null access Gtk_Entry_Record;
       Xalign    : Gfloat)
   is
      procedure Internal (The_Entry : System.Address; Xalign : Gfloat);
      pragma Import (C, Internal, "gtk_entry_set_alignment");
   begin
      Internal (Get_Object (The_Entry), Xalign);
   end Set_Alignment;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (The_Entry : not null access Gtk_Entry_Record;
       Attrs     : Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal
         (The_Entry : System.Address;
          Attrs     : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_attributes");
   begin
      Internal (Get_Object (The_Entry), Get_Object (Attrs));
   end Set_Attributes;

   ----------------
   -- Set_Buffer --
   ----------------

   procedure Set_Buffer
      (The_Entry : not null access Gtk_Entry_Record;
       Buffer    : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
   is
      procedure Internal
         (The_Entry : System.Address;
          Buffer    : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_buffer");
   begin
      Internal (Get_Object (The_Entry), Get_Object (Buffer));
   end Set_Buffer;

   --------------------
   -- Set_Completion --
   --------------------

   procedure Set_Completion
      (The_Entry  : not null access Gtk_Entry_Record;
       Completion : access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class)
   is
      procedure Internal
         (The_Entry  : System.Address;
          Completion : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_completion");
   begin
      Internal (Get_Object (The_Entry), Get_Object_Or_Null (GObject (Completion)));
   end Set_Completion;

   ----------------------------
   -- Set_Cursor_Hadjustment --
   ----------------------------

   procedure Set_Cursor_Hadjustment
      (The_Entry  : not null access Gtk_Entry_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (The_Entry  : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_cursor_hadjustment");
   begin
      Internal (Get_Object (The_Entry), Get_Object_Or_Null (GObject (Adjustment)));
   end Set_Cursor_Hadjustment;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame
      (The_Entry : not null access Gtk_Entry_Record;
       Setting   : Boolean := True)
   is
      procedure Internal
         (The_Entry : System.Address;
          Setting   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_has_frame");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Setting));
   end Set_Has_Frame;

   --------------------------
   -- Set_Icon_Activatable --
   --------------------------

   procedure Set_Icon_Activatable
      (The_Entry   : not null access Gtk_Entry_Record;
       Icon_Pos    : Gtk_Entry_Icon_Position;
       Activatable : Boolean)
   is
      procedure Internal
         (The_Entry   : System.Address;
          Icon_Pos    : Gtk_Entry_Icon_Position;
          Activatable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_icon_activatable");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Boolean'Pos (Activatable));
   end Set_Icon_Activatable;

   --------------------------
   -- Set_Icon_Drag_Source --
   --------------------------

   procedure Set_Icon_Drag_Source
      (The_Entry   : not null access Gtk_Entry_Record;
       Icon_Pos    : Gtk_Entry_Icon_Position;
       Target_List : Gtk.Target_List.Gtk_Target_List;
       Actions     : Gdk.Drag_Contexts.Gdk_Drag_Action)
   is
      procedure Internal
         (The_Entry   : System.Address;
          Icon_Pos    : Gtk_Entry_Icon_Position;
          Target_List : System.Address;
          Actions     : Gdk.Drag_Contexts.Gdk_Drag_Action);
      pragma Import (C, Internal, "gtk_entry_set_icon_drag_source");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Get_Object (Target_List), Actions);
   end Set_Icon_Drag_Source;

   -------------------------
   -- Set_Icon_From_Gicon --
   -------------------------

   procedure Set_Icon_From_Gicon
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Icon      : Glib.G_Icon.G_Icon)
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Icon      : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_gicon");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Icon);
   end Set_Icon_From_Gicon;

   -----------------------------
   -- Set_Icon_From_Icon_Name --
   -----------------------------

   procedure Set_Icon_From_Icon_Name
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (The_Entry), Icon_Pos, Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_From_Icon_Name;

   --------------------------
   -- Set_Icon_From_Pixbuf --
   --------------------------

   procedure Set_Icon_From_Pixbuf
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Pixbuf    : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_pixbuf");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Get_Object_Or_Null (GObject (Pixbuf)));
   end Set_Icon_From_Pixbuf;

   -------------------------
   -- Set_Icon_From_Stock --
   -------------------------

   procedure Set_Icon_From_Stock
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Stock_Id  : UTF8_String := "")
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Stock_Id  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr;
   begin
      if Stock_Id = "" then
         Tmp_Stock_Id := Gtkada.Types.Null_Ptr;
      else
         Tmp_Stock_Id := New_String (Stock_Id);
      end if;
      Internal (Get_Object (The_Entry), Icon_Pos, Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
   end Set_Icon_From_Stock;

   ------------------------
   -- Set_Icon_Sensitive --
   ------------------------

   procedure Set_Icon_Sensitive
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Sensitive : Boolean)
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_icon_sensitive");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Boolean'Pos (Sensitive));
   end Set_Icon_Sensitive;

   -----------------------------
   -- Set_Icon_Tooltip_Markup --
   -----------------------------

   procedure Set_Icon_Tooltip_Markup
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Tooltip   : UTF8_String := "")
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Tooltip   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_markup");
      Tmp_Tooltip : Gtkada.Types.Chars_Ptr;
   begin
      if Tooltip = "" then
         Tmp_Tooltip := Gtkada.Types.Null_Ptr;
      else
         Tmp_Tooltip := New_String (Tooltip);
      end if;
      Internal (Get_Object (The_Entry), Icon_Pos, Tmp_Tooltip);
      Free (Tmp_Tooltip);
   end Set_Icon_Tooltip_Markup;

   ---------------------------
   -- Set_Icon_Tooltip_Text --
   ---------------------------

   procedure Set_Icon_Tooltip_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk_Entry_Icon_Position;
       Tooltip   : UTF8_String := "")
   is
      procedure Internal
         (The_Entry : System.Address;
          Icon_Pos  : Gtk_Entry_Icon_Position;
          Tooltip   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_text");
      Tmp_Tooltip : Gtkada.Types.Chars_Ptr;
   begin
      if Tooltip = "" then
         Tmp_Tooltip := Gtkada.Types.Null_Ptr;
      else
         Tmp_Tooltip := New_String (Tooltip);
      end if;
      Internal (Get_Object (The_Entry), Icon_Pos, Tmp_Tooltip);
      Free (Tmp_Tooltip);
   end Set_Icon_Tooltip_Text;

   ----------------------
   -- Set_Inner_Border --
   ----------------------

   procedure Set_Inner_Border
      (The_Entry : not null access Gtk_Entry_Record;
       Border    : Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (The_Entry : System.Address;
          Border    : Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_entry_set_inner_border");
   begin
      Internal (Get_Object (The_Entry), Border);
   end Set_Inner_Border;

   ---------------------
   -- Set_Input_Hints --
   ---------------------

   procedure Set_Input_Hints
      (The_Entry : not null access Gtk_Entry_Record;
       Hints     : Gtk.Enums.Gtk_Input_Hints)
   is
      procedure Internal
         (The_Entry : System.Address;
          Hints     : Gtk.Enums.Gtk_Input_Hints);
      pragma Import (C, Internal, "gtk_entry_set_input_hints");
   begin
      Internal (Get_Object (The_Entry), Hints);
   end Set_Input_Hints;

   -----------------------
   -- Set_Input_Purpose --
   -----------------------

   procedure Set_Input_Purpose
      (The_Entry : not null access Gtk_Entry_Record;
       Purpose   : Gtk.Enums.Gtk_Input_Purpose)
   is
      procedure Internal
         (The_Entry : System.Address;
          Purpose   : Gtk.Enums.Gtk_Input_Purpose);
      pragma Import (C, Internal, "gtk_entry_set_input_purpose");
   begin
      Internal (Get_Object (The_Entry), Purpose);
   end Set_Input_Purpose;

   ------------------------
   -- Set_Invisible_Char --
   ------------------------

   procedure Set_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record;
       Char      : Gunichar)
   is
      procedure Internal (The_Entry : System.Address; Char : Gunichar);
      pragma Import (C, Internal, "gtk_entry_set_invisible_char");
   begin
      Internal (Get_Object (The_Entry), Char);
   end Set_Invisible_Char;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
      (The_Entry : not null access Gtk_Entry_Record;
       Max       : Glib.Gint)
   is
      procedure Internal (The_Entry : System.Address; Max : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_set_max_length");
   begin
      Internal (Get_Object (The_Entry), Max);
   end Set_Max_Length;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record;
       N_Chars   : Glib.Gint)
   is
      procedure Internal (The_Entry : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_set_max_width_chars");
   begin
      Internal (Get_Object (The_Entry), N_Chars);
   end Set_Max_Width_Chars;

   ------------------------
   -- Set_Overwrite_Mode --
   ------------------------

   procedure Set_Overwrite_Mode
      (The_Entry : not null access Gtk_Entry_Record;
       Overwrite : Boolean)
   is
      procedure Internal
         (The_Entry : System.Address;
          Overwrite : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_overwrite_mode");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Overwrite));
   end Set_Overwrite_Mode;

   --------------------------
   -- Set_Placeholder_Text --
   --------------------------

   procedure Set_Placeholder_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Text      : UTF8_String := "")
   is
      procedure Internal
         (The_Entry : System.Address;
          Text      : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_placeholder_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      if Text = "" then
         Tmp_Text := Gtkada.Types.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
      Internal (Get_Object (The_Entry), Tmp_Text);
      Free (Tmp_Text);
   end Set_Placeholder_Text;

   ---------------------------
   -- Set_Progress_Fraction --
   ---------------------------

   procedure Set_Progress_Fraction
      (The_Entry : not null access Gtk_Entry_Record;
       Fraction  : Gdouble)
   is
      procedure Internal (The_Entry : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_entry_set_progress_fraction");
   begin
      Internal (Get_Object (The_Entry), Fraction);
   end Set_Progress_Fraction;

   -----------------------------
   -- Set_Progress_Pulse_Step --
   -----------------------------

   procedure Set_Progress_Pulse_Step
      (The_Entry : not null access Gtk_Entry_Record;
       Fraction  : Gdouble)
   is
      procedure Internal (The_Entry : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_entry_set_progress_pulse_step");
   begin
      Internal (Get_Object (The_Entry), Fraction);
   end Set_Progress_Pulse_Step;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
      (The_Entry : not null access Gtk_Entry_Record;
       Tabs      : Pango.Tabs.Pango_Tab_Array)
   is
      procedure Internal (The_Entry : System.Address; Tabs : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_tabs");
   begin
      Internal (Get_Object (The_Entry), Get_Object (Tabs));
   end Set_Tabs;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Text      : UTF8_String)
   is
      procedure Internal
         (The_Entry : System.Address;
          Text      : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (The_Entry), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
      (The_Entry : not null access Gtk_Entry_Record;
       Visible   : Boolean)
   is
      procedure Internal
         (The_Entry : System.Address;
          Visible   : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_visibility");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Visible));
   end Set_Visibility;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record;
       Width     : Glib.Gint)
   is
      procedure Internal (The_Entry : System.Address; Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_set_width_chars");
   begin
      Internal (Get_Object (The_Entry), Width);
   end Set_Width_Chars;

   --------------------------------
   -- Text_Index_To_Layout_Index --
   --------------------------------

   function Text_Index_To_Layout_Index
      (The_Entry  : not null access Gtk_Entry_Record;
       Text_Index : Glib.Gint) return Glib.Gint
   is
      function Internal
         (The_Entry  : System.Address;
          Text_Index : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_text_index_to_layout_index");
   begin
      return Internal (Get_Object (The_Entry), Text_Index);
   end Text_Index_To_Layout_Index;

   --------------------------
   -- Unset_Invisible_Char --
   --------------------------

   procedure Unset_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record)
   is
      procedure Internal (The_Entry : System.Address);
      pragma Import (C, Internal, "gtk_entry_unset_invisible_char");
   begin
      Internal (Get_Object (The_Entry));
   end Unset_Invisible_Char;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard (Editable : not null access Gtk_Entry_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_copy_clipboard");
   begin
      Internal (Get_Object (Editable));
   end Copy_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard (Editable : not null access Gtk_Entry_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_cut_clipboard");
   begin
      Internal (Get_Object (Editable));
   end Cut_Clipboard;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection (Editable : not null access Gtk_Entry_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");
   begin
      Internal (Get_Object (Editable));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
      (Editable  : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");
   begin
      Internal (Get_Object (Editable), Start_Pos, End_Pos);
   end Delete_Text;

   ------------------
   -- Editing_Done --
   ------------------

   procedure Editing_Done (Cell_Editable : not null access Gtk_Entry_Record) is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_editing_done");
   begin
      Internal (Get_Object (Cell_Editable));
   end Editing_Done;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Editable  : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String
   is
      function Internal
         (Editable  : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Editable), Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
      (Editable : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Editable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Get_Object (Editable)) /= 0;
   end Get_Editable;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Editable : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (Editable : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Editable));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Editable      : System.Address;
          Acc_Start_Pos : access Glib.Gint;
          Acc_End_Pos   : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Glib.Gint;
      Acc_End_Pos   : aliased Glib.Gint;
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Editable), Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Editable        : not null access Gtk_Entry_Record;
       New_Text        : UTF8_String;
       New_Text_Length : Glib.Gint;
       Position        : in out Glib.Gint)
   is
      procedure Internal
         (Editable        : System.Address;
          New_Text        : Gtkada.Types.Chars_Ptr;
          New_Text_Length : Glib.Gint;
          Position        : in out Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_New_Text : Gtkada.Types.Chars_Ptr := New_String (New_Text);
   begin
      Internal (Get_Object (Editable), Tmp_New_Text, New_Text_Length, Position);
      Free (Tmp_New_Text);
   end Insert_Text;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard (Editable : not null access Gtk_Entry_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_paste_clipboard");
   begin
      Internal (Get_Object (Editable));
   end Paste_Clipboard;

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Entry_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_remove_widget");
   begin
      Internal (Get_Object (Cell_Editable));
   end Remove_Widget;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Editable  : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");
   begin
      Internal (Get_Object (Editable), Start_Pos, End_Pos);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (Editable    : not null access Gtk_Entry_Record;
       Is_Editable : Boolean)
   is
      procedure Internal
         (Editable    : System.Address;
          Is_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Editable), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Editable : not null access Gtk_Entry_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Editable : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");
   begin
      Internal (Get_Object (Editable), Position);
   end Set_Position;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Entry_Record;
       Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Cell_Editable : System.Address;
          Event         : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Event);
   end Start_Editing;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Gtk_Delete_Type_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Gtk_Delete_Type_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Delete_Type_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Delete_Type_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Entry_Icon_Position_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Entry_Icon_Position_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Gtk_Widget_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Void);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Delete_Type_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Widget_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Delete_Type_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Delete_Type_Gint_Void);

   procedure Marsh_GObject_Gtk_Entry_Icon_Position_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Entry_Icon_Position_Void);

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void);

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Void);

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_UTF8_String_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Entry_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Gtk_Delete_Type_Gint_Void);

   procedure Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void);

   procedure Marsh_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void);

   procedure Marsh_Gtk_Entry_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Gtk_Widget_Void);

   procedure Marsh_Gtk_Entry_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_UTF8_String_Void);

   procedure Marsh_Gtk_Entry_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Delete_Type_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Gtk_Delete_Type_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Widget_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Entry_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
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
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Delete_Type_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Delete_Type_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Entry_Icon_Position_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Entry_Record'Class;
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
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ---------------------------------------------
   -- Marsh_GObject_Gtk_Delete_Type_Gint_Void --
   ---------------------------------------------

   procedure Marsh_GObject_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Delete_Type_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Delete_Type (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Delete_Type_Gint_Void;

   ------------------------------------------------
   -- Marsh_GObject_Gtk_Entry_Icon_Position_Void --
   ------------------------------------------------

   procedure Marsh_GObject_Gtk_Entry_Icon_Position_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Entry_Icon_Position_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Entry_Icon_Position (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Entry_Icon_Position_Void;

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

   -----------------------------------
   -- Marsh_GObject_Gtk_Widget_Void --
   -----------------------------------

   procedure Marsh_GObject_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Void;

   ------------------------------------
   -- Marsh_GObject_UTF8_String_Void --
   ------------------------------------

   procedure Marsh_GObject_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_UTF8_String_Void;

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

   -----------------------------------------------
   -- Marsh_Gtk_Entry_Gtk_Delete_Type_Gint_Void --
   -----------------------------------------------

   procedure Marsh_Gtk_Entry_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Gtk_Delete_Type_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry := Gtk_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Delete_Type (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Gtk_Delete_Type_Gint_Void;

   --------------------------------------------------
   -- Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void --
   --------------------------------------------------

   procedure Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry := Gtk_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Entry_Icon_Position (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void;

   ---------------------------------------------------------
   -- Marsh_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void --
   ---------------------------------------------------------

   procedure Marsh_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry := Gtk_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void;

   -------------------------------------
   -- Marsh_Gtk_Entry_Gtk_Widget_Void --
   -------------------------------------

   procedure Marsh_Gtk_Entry_Gtk_Widget_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Gtk_Widget_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry := Gtk_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Gtk_Widget_Void;

   --------------------------------------
   -- Marsh_Gtk_Entry_UTF8_String_Void --
   --------------------------------------

   procedure Marsh_Gtk_Entry_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry := Gtk_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_UTF8_String_Void;

   --------------------------
   -- Marsh_Gtk_Entry_Void --
   --------------------------

   procedure Marsh_Gtk_Entry_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Entry_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Entry := Gtk_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Entry_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   ------------------
   -- On_Backspace --
   ------------------

   procedure On_Backspace
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "backspace" & ASCII.NUL, Call, After);
   end On_Backspace;

   ------------------
   -- On_Backspace --
   ------------------

   procedure On_Backspace
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "backspace" & ASCII.NUL, Call, After, Slot);
   end On_Backspace;

   -----------------------
   -- On_Copy_Clipboard --
   -----------------------

   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "copy-clipboard" & ASCII.NUL, Call, After);
   end On_Copy_Clipboard;

   -----------------------
   -- On_Copy_Clipboard --
   -----------------------

   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "copy-clipboard" & ASCII.NUL, Call, After, Slot);
   end On_Copy_Clipboard;

   ----------------------
   -- On_Cut_Clipboard --
   ----------------------

   procedure On_Cut_Clipboard
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cut-clipboard" & ASCII.NUL, Call, After);
   end On_Cut_Clipboard;

   ----------------------
   -- On_Cut_Clipboard --
   ----------------------

   procedure On_Cut_Clipboard
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "cut-clipboard" & ASCII.NUL, Call, After, Slot);
   end On_Cut_Clipboard;

   ---------------------------
   -- On_Delete_From_Cursor --
   ---------------------------

   procedure On_Delete_From_Cursor
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Delete_Type_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "delete-from-cursor" & ASCII.NUL, Call, After);
   end On_Delete_From_Cursor;

   ---------------------------
   -- On_Delete_From_Cursor --
   ---------------------------

   procedure On_Delete_From_Cursor
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Delete_Type_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "delete-from-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Delete_From_Cursor;

   -------------------
   -- On_Icon_Press --
   -------------------

   procedure On_Icon_Press
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "icon-press" & ASCII.NUL, Call, After);
   end On_Icon_Press;

   -------------------
   -- On_Icon_Press --
   -------------------

   procedure On_Icon_Press
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "icon-press" & ASCII.NUL, Call, After, Slot);
   end On_Icon_Press;

   ---------------------
   -- On_Icon_Release --
   ---------------------

   procedure On_Icon_Release
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "icon-release" & ASCII.NUL, Call, After);
   end On_Icon_Release;

   ---------------------
   -- On_Icon_Release --
   ---------------------

   procedure On_Icon_Release
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "icon-release" & ASCII.NUL, Call, After, Slot);
   end On_Icon_Release;

   -------------------------
   -- On_Insert_At_Cursor --
   -------------------------

   procedure On_Insert_At_Cursor
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-at-cursor" & ASCII.NUL, Call, After);
   end On_Insert_At_Cursor;

   -------------------------
   -- On_Insert_At_Cursor --
   -------------------------

   procedure On_Insert_At_Cursor
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-at-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Insert_At_Cursor;

   ---------------------
   -- On_Insert_Emoji --
   ---------------------

   procedure On_Insert_Emoji
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-emoji" & ASCII.NUL, Call, After);
   end On_Insert_Emoji;

   ---------------------
   -- On_Insert_Emoji --
   ---------------------

   procedure On_Insert_Emoji
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "insert-emoji" & ASCII.NUL, Call, After, Slot);
   end On_Insert_Emoji;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Movement_Step_Gint_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   ------------------------
   -- On_Paste_Clipboard --
   ------------------------

   procedure On_Paste_Clipboard
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "paste-clipboard" & ASCII.NUL, Call, After);
   end On_Paste_Clipboard;

   ------------------------
   -- On_Paste_Clipboard --
   ------------------------

   procedure On_Paste_Clipboard
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "paste-clipboard" & ASCII.NUL, Call, After, Slot);
   end On_Paste_Clipboard;

   -----------------------
   -- On_Populate_Popup --
   -----------------------

   procedure On_Populate_Popup
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Gtk_Widget_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "populate-popup" & ASCII.NUL, Call, After);
   end On_Populate_Popup;

   -----------------------
   -- On_Populate_Popup --
   -----------------------

   procedure On_Populate_Popup
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Gtk_Widget_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "populate-popup" & ASCII.NUL, Call, After, Slot);
   end On_Populate_Popup;

   ------------------------
   -- On_Preedit_Changed --
   ------------------------

   procedure On_Preedit_Changed
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "preedit-changed" & ASCII.NUL, Call, After);
   end On_Preedit_Changed;

   ------------------------
   -- On_Preedit_Changed --
   ------------------------

   procedure On_Preedit_Changed
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "preedit-changed" & ASCII.NUL, Call, After, Slot);
   end On_Preedit_Changed;

   -------------------------
   -- On_Toggle_Overwrite --
   -------------------------

   procedure On_Toggle_Overwrite
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_Gtk_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-overwrite" & ASCII.NUL, Call, After);
   end On_Toggle_Overwrite;

   -------------------------
   -- On_Toggle_Overwrite --
   -------------------------

   procedure On_Toggle_Overwrite
      (Self  : not null access Gtk_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-overwrite" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Overwrite;

end Gtk.GEntry;
