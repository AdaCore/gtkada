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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

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
      Set_Object (The_Entry, Internal);
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
      Set_Object (The_Entry, Internal (Get_Object (Buffer)));
   end Initialize_With_Buffer;

   ---------------------------
   -- Get_Activates_Default --
   ---------------------------

   function Get_Activates_Default
      (The_Entry : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_get_activates_default");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry)));
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
      (The_Entry : not null access Gtk_Entry_Record) return Gint
   is
      function Internal (The_Entry : System.Address) return Gint;
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
      function Internal (The_Entry : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_get_has_frame");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry)));
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
          Icon_Pos  : Gtk_Entry_Icon_Position) return Integer;
      pragma Import (C, Internal, "gtk_entry_get_icon_activatable");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry), Icon_Pos));
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
       X         : Gint;
       Y         : Gint) return Gint
   is
      function Internal
         (The_Entry : System.Address;
          X         : Gint;
          Y         : Gint) return Gint;
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
          Icon_Pos  : Gtk_Entry_Icon_Position)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry), Icon_Pos));
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
          Icon_Pos  : Gtk_Entry_Icon_Position) return Integer;
      pragma Import (C, Internal, "gtk_entry_get_icon_sensitive");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry), Icon_Pos));
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
          Icon_Pos  : Gtk_Entry_Icon_Position)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_stock");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry), Icon_Pos));
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
          Icon_Pos  : Gtk_Entry_Icon_Position)
          return Interfaces.C.Strings.chars_ptr;
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
          Icon_Pos  : Gtk_Entry_Icon_Position)
          return Interfaces.C.Strings.chars_ptr;
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
         (The_Entry : System.Address) return Gtk.Style.Gtk_Border;
      pragma Import (C, Internal, "gtk_entry_get_inner_border");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Inner_Border;

   ------------------------
   -- Get_Invisible_Char --
   ------------------------

   function Get_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record) return gunichar
   is
      function Internal (The_Entry : System.Address) return gunichar;
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
       X         : out Gint;
       Y         : out Gint)
   is
      procedure Internal
         (The_Entry : System.Address;
          X         : out Gint;
          Y         : out Gint);
      pragma Import (C, Internal, "gtk_entry_get_layout_offsets");
   begin
      Internal (Get_Object (The_Entry), X, Y);
   end Get_Layout_Offsets;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length
      (The_Entry : not null access Gtk_Entry_Record) return Gint
   is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_max_length");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Max_Length;

   ------------------------
   -- Get_Overwrite_Mode --
   ------------------------

   function Get_Overwrite_Mode
      (The_Entry : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_get_overwrite_mode");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry)));
   end Get_Overwrite_Mode;

   --------------------------
   -- Get_Placeholder_Text --
   --------------------------

   function Get_Placeholder_Text
      (The_Entry : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_placeholder_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry)));
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
   -- Get_Text --
   --------------

   function Get_Text
      (The_Entry : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (The_Entry : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry)));
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
      function Internal (The_Entry : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_entry_get_visibility");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry)));
   end Get_Visibility;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record) return Gint
   is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_width_chars");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Width_Chars;

   --------------------------------
   -- Im_Context_Filter_Keypress --
   --------------------------------

   function Im_Context_Filter_Keypress
      (The_Entry : not null access Gtk_Entry_Record;
       Event     : Gdk.Event.Gdk_Event_Key) return Boolean
   is
      function Internal
         (The_Entry : System.Address;
          Event     : Gdk.Event.Gdk_Event_Key) return Integer;
      pragma Import (C, Internal, "gtk_entry_im_context_filter_keypress");
   begin
      return Boolean'Val (Internal (Get_Object (The_Entry), Event));
   end Im_Context_Filter_Keypress;

   --------------------------------
   -- Layout_Index_To_Text_Index --
   --------------------------------

   function Layout_Index_To_Text_Index
      (The_Entry    : not null access Gtk_Entry_Record;
       Layout_Index : Gint) return Gint
   is
      function Internal
         (The_Entry    : System.Address;
          Layout_Index : Gint) return Gint;
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
      procedure Internal (The_Entry : System.Address; Setting : Integer);
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
       Adjustment : not null access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
      
   is
      procedure Internal
         (The_Entry  : System.Address;
          Adjustment : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_cursor_hadjustment");
   begin
      Internal (Get_Object (The_Entry), Get_Object (Adjustment));
   end Set_Cursor_Hadjustment;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame
      (The_Entry : not null access Gtk_Entry_Record;
       Setting   : Boolean := True)
   is
      procedure Internal (The_Entry : System.Address; Setting : Integer);
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
          Activatable : Integer);
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
          Target_List : Gtk.Target_List.Gtk_Target_List;
          Actions     : Gdk.Drag_Contexts.Gdk_Drag_Action);
      pragma Import (C, Internal, "gtk_entry_set_icon_drag_source");
   begin
      Internal (Get_Object (The_Entry), Icon_Pos, Target_List, Actions);
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
          Icon_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Interfaces.C.Strings.Null_Ptr;
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
          Stock_Id  : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr;
   begin
      if Stock_Id = "" then
         Tmp_Stock_Id := Interfaces.C.Strings.Null_Ptr;
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
          Sensitive : Integer);
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
          Tooltip   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_markup");
      Tmp_Tooltip : Interfaces.C.Strings.chars_ptr;
   begin
      if Tooltip = "" then
         Tmp_Tooltip := Interfaces.C.Strings.Null_Ptr;
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
          Tooltip   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_text");
      Tmp_Tooltip : Interfaces.C.Strings.chars_ptr;
   begin
      if Tooltip = "" then
         Tmp_Tooltip := Interfaces.C.Strings.Null_Ptr;
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
       Border    : in out Gtk.Style.Gtk_Border)
   is
      procedure Internal
         (The_Entry : System.Address;
          Border    : in out Gtk.Style.Gtk_Border);
      pragma Import (C, Internal, "gtk_entry_set_inner_border");
   begin
      Internal (Get_Object (The_Entry), Border);
   end Set_Inner_Border;

   ------------------------
   -- Set_Invisible_Char --
   ------------------------

   procedure Set_Invisible_Char
      (The_Entry : not null access Gtk_Entry_Record;
       Char      : gunichar)
   is
      procedure Internal (The_Entry : System.Address; Char : gunichar);
      pragma Import (C, Internal, "gtk_entry_set_invisible_char");
   begin
      Internal (Get_Object (The_Entry), Char);
   end Set_Invisible_Char;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
      (The_Entry : not null access Gtk_Entry_Record;
       Max       : Gint)
   is
      procedure Internal (The_Entry : System.Address; Max : Gint);
      pragma Import (C, Internal, "gtk_entry_set_max_length");
   begin
      Internal (Get_Object (The_Entry), Max);
   end Set_Max_Length;

   ------------------------
   -- Set_Overwrite_Mode --
   ------------------------

   procedure Set_Overwrite_Mode
      (The_Entry : not null access Gtk_Entry_Record;
       Overwrite : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Overwrite : Integer);
      pragma Import (C, Internal, "gtk_entry_set_overwrite_mode");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Overwrite));
   end Set_Overwrite_Mode;

   --------------------------
   -- Set_Placeholder_Text --
   --------------------------

   procedure Set_Placeholder_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Text      : UTF8_String)
   is
      procedure Internal
         (The_Entry : System.Address;
          Text      : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_set_placeholder_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
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
   -- Set_Text --
   --------------

   procedure Set_Text
      (The_Entry : not null access Gtk_Entry_Record;
       Text      : UTF8_String)
   is
      procedure Internal
         (The_Entry : System.Address;
          Text      : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_entry_set_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
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
      procedure Internal (The_Entry : System.Address; Visible : Integer);
      pragma Import (C, Internal, "gtk_entry_set_visibility");
   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Visible));
   end Set_Visibility;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (The_Entry : not null access Gtk_Entry_Record;
       Width     : Gint)
   is
      procedure Internal (The_Entry : System.Address; Width : Gint);
      pragma Import (C, Internal, "gtk_entry_set_width_chars");
   begin
      Internal (Get_Object (The_Entry), Width);
   end Set_Width_Chars;

   --------------------------------
   -- Text_Index_To_Layout_Index --
   --------------------------------

   function Text_Index_To_Layout_Index
      (The_Entry  : not null access Gtk_Entry_Record;
       Text_Index : Gint) return Gint
   is
      function Internal
         (The_Entry  : System.Address;
          Text_Index : Gint) return Gint;
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
       Start_Pos : Gint;
       End_Pos   : Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Gint;
          End_Pos   : Gint);
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
       Start_Pos : Gint;
       End_Pos   : Gint := -1) return UTF8_String
   is
      function Internal
         (Editable  : System.Address;
          Start_Pos : Gint;
          End_Pos   : Gint) return Interfaces.C.Strings.chars_ptr;
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
      function Internal (Editable : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Boolean'Val (Internal (Get_Object (Editable)));
   end Get_Editable;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Editable : not null access Gtk_Entry_Record) return Gint
   is
      function Internal (Editable : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Editable));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Entry_Record;
       Start_Pos     : out Gint;
       End_Pos       : out Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Editable      : System.Address;
          Acc_Start_Pos : access Gint;
          Acc_End_Pos   : access Gint) return Integer;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Gint;
      Acc_End_Pos   : aliased Gint;
      Tmp_Return    : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Editable), Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Boolean'Val (Tmp_Return);
   end Get_Selection_Bounds;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Editable        : not null access Gtk_Entry_Record;
       New_Text        : UTF8_String;
       New_Text_Length : Gint;
       Position        : in out Gint)
   is
      procedure Internal
         (Editable        : System.Address;
          New_Text        : Interfaces.C.Strings.chars_ptr;
          New_Text_Length : Gint;
          Position        : in out Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_New_Text : Interfaces.C.Strings.chars_ptr := New_String (New_Text);
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
       Start_Pos : Gint;
       End_Pos   : Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Gint;
          End_Pos   : Gint);
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
      procedure Internal (Editable : System.Address; Is_Editable : Integer);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Editable), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Editable : not null access Gtk_Entry_Record;
       Position : Gint)
   is
      procedure Internal (Editable : System.Address; Position : Gint);
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

end Gtk.GEntry;
