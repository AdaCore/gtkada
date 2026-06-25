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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.GEntry is

   package Type_Conversion_Gtk_Entry is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Entry);

   -------------------
   -- Gtk_Entry_New --
   -------------------

   function Gtk_Entry_New return Gtk_Entry is
      Self : constant Gtk_Entry := new Gtk_Entry_Record;
   begin
      Gtk.GEntry.Initialize (Self);
      return Self;
   end Gtk_Entry_New;

   -------------------------------
   -- Gtk_Entry_New_With_Buffer --
   -------------------------------

   function Gtk_Entry_New_With_Buffer
      (Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
       return Gtk_Entry
   is
      Self : constant Gtk_Entry := new Gtk_Entry_Record;
   begin
      Gtk.GEntry.Initialize_With_Buffer (Self, Buffer);
      return Self;
   end Gtk_Entry_New_With_Buffer;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Entry) is
   begin
      Self := new Gtk_Entry_Record;
      Gtk.GEntry.Initialize (Self);
   end Gtk_New;

   -------------------------
   -- Gtk_New_With_Buffer --
   -------------------------

   procedure Gtk_New_With_Buffer
      (Self   : out Gtk_Entry;
       Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
   is
   begin
      Self := new Gtk_Entry_Record;
      Gtk.GEntry.Initialize_With_Buffer (Self, Buffer);
   end Gtk_New_With_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Entry_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------------------
   -- Initialize_With_Buffer --
   ----------------------------

   procedure Initialize_With_Buffer
      (Self   : not null access Gtk_Entry_Record'Class;
       Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_new_with_buffer");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Buffer)));
      end if;
   end Initialize_With_Buffer;

   ---------------------------
   -- Get_Activates_Default --
   ---------------------------

   function Get_Activates_Default
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_activates_default");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activates_Default;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
      (Self : not null access Gtk_Entry_Record) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gtk_entry_get_alignment");
   begin
      return Internal (Get_Object (Self));
   end Get_Alignment;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Self : not null access Gtk_Entry_Record)
       return Pango.Attributes.Pango_Attr_List
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_attributes");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Attributes;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Entry_Buffer.Gtk_Entry_Buffer
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_buffer");
      Stub_Gtk_Entry_Buffer : Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record;
   begin
      return Gtk.Entry_Buffer.Gtk_Entry_Buffer (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Entry_Buffer));
   end Get_Buffer;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Entry_Completion.Gtk_Entry_Completion
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_completion");
      Stub_Gtk_Entry_Completion : Gtk.Entry_Completion.Gtk_Entry_Completion_Record;
   begin
      return Gtk.Entry_Completion.Gtk_Entry_Completion (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Entry_Completion));
   end Get_Completion;

   ----------------------------------
   -- Get_Current_Icon_Drag_Source --
   ----------------------------------

   function Get_Current_Icon_Drag_Source
      (Self : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_current_icon_drag_source");
   begin
      return Internal (Get_Object (Self));
   end Get_Current_Icon_Drag_Source;

   --------------------
   -- Get_Extra_Menu --
   --------------------

   function Get_Extra_Menu
      (Self : not null access Gtk_Entry_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_extra_menu");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (Self)), Stub_Gmenu_Model));
   end Get_Extra_Menu;

   -------------------
   -- Get_Has_Frame --
   -------------------

   function Get_Has_Frame
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_has_frame");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Has_Frame;

   --------------------------
   -- Get_Icon_Activatable --
   --------------------------

   function Get_Icon_Activatable
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_icon_activatable");
   begin
      return Internal (Get_Object (Self), Icon_Pos) /= 0;
   end Get_Icon_Activatable;

   -------------------
   -- Get_Icon_Area --
   -------------------

   procedure Get_Icon_Area
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Icon_Area : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
          Icon_Area : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_entry_get_icon_area");
   begin
      Internal (Get_Object (Self), Icon_Pos, Icon_Area);
   end Get_Icon_Area;

   ---------------------
   -- Get_Icon_At_Pos --
   ---------------------

   function Get_Icon_At_Pos
      (Self : not null access Gtk_Entry_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          X    : Glib.Gint;
          Y    : Glib.Gint) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_icon_at_pos");
   begin
      return Internal (Get_Object (Self), X, Y);
   end Get_Icon_At_Pos;

   --------------------
   -- Get_Icon_Gicon --
   --------------------

   function Get_Icon_Gicon
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
       return Glib.G_Icon.G_Icon
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_entry_get_icon_gicon");
   begin
      return Internal (Get_Object (Self), Icon_Pos);
   end Get_Icon_Gicon;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Icon_Pos));
   end Get_Icon_Name;

   ------------------------
   -- Get_Icon_Paintable --
   ------------------------

   function Get_Icon_Paintable
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
       return Gdk.Paintable.Gdk_Paintable
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Gdk.Paintable.Gdk_Paintable;
      pragma Import (C, Internal, "gtk_entry_get_icon_paintable");
   begin
      return Internal (Get_Object (Self), Icon_Pos);
   end Get_Icon_Paintable;

   ------------------------
   -- Get_Icon_Sensitive --
   ------------------------

   function Get_Icon_Sensitive
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return Boolean
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_icon_sensitive");
   begin
      return Internal (Get_Object (Self), Icon_Pos) /= 0;
   end Get_Icon_Sensitive;

   ---------------------------
   -- Get_Icon_Storage_Type --
   ---------------------------

   function Get_Icon_Storage_Type
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
       return Gtk.Enums.Gtk_Image_Type
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Gtk.Enums.Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_entry_get_icon_storage_type");
   begin
      return Internal (Get_Object (Self), Icon_Pos);
   end Get_Icon_Storage_Type;

   -----------------------------
   -- Get_Icon_Tooltip_Markup --
   -----------------------------

   function Get_Icon_Tooltip_Markup
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_tooltip_markup");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Icon_Pos));
   end Get_Icon_Tooltip_Markup;

   ---------------------------
   -- Get_Icon_Tooltip_Text --
   ---------------------------

   function Get_Icon_Tooltip_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_icon_tooltip_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Icon_Pos));
   end Get_Icon_Tooltip_Text;

   ---------------------
   -- Get_Input_Hints --
   ---------------------

   function Get_Input_Hints
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Enums.Gtk_Input_Hints
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Input_Hints;
      pragma Import (C, Internal, "gtk_entry_get_input_hints");
   begin
      return Internal (Get_Object (Self));
   end Get_Input_Hints;

   -----------------------
   -- Get_Input_Purpose --
   -----------------------

   function Get_Input_Purpose
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Enums.Gtk_Input_Purpose
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Input_Purpose;
      pragma Import (C, Internal, "gtk_entry_get_input_purpose");
   begin
      return Internal (Get_Object (Self));
   end Get_Input_Purpose;

   ------------------------
   -- Get_Invisible_Char --
   ------------------------

   function Get_Invisible_Char
      (Self : not null access Gtk_Entry_Record) return Gunichar
   is
      function Internal (Self : System.Address) return Gunichar;
      pragma Import (C, Internal, "gtk_entry_get_invisible_char");
   begin
      return Internal (Get_Object (Self));
   end Get_Invisible_Char;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length
      (Self : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_entry_get_max_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Length;

   ------------------------------
   -- Get_Menu_Entry_Icon_Text --
   ------------------------------

   function Get_Menu_Entry_Icon_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position) return UTF8_String
   is
      function Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_menu_entry_icon_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Icon_Pos));
   end Get_Menu_Entry_Icon_Text;

   ------------------------
   -- Get_Overwrite_Mode --
   ------------------------

   function Get_Overwrite_Mode
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_overwrite_mode");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Overwrite_Mode;

   --------------------------
   -- Get_Placeholder_Text --
   --------------------------

   function Get_Placeholder_Text
      (Self : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_entry_get_placeholder_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Placeholder_Text;

   ---------------------------
   -- Get_Progress_Fraction --
   ---------------------------

   function Get_Progress_Fraction
      (Self : not null access Gtk_Entry_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_entry_get_progress_fraction");
   begin
      return Internal (Get_Object (Self));
   end Get_Progress_Fraction;

   -----------------------------
   -- Get_Progress_Pulse_Step --
   -----------------------------

   function Get_Progress_Pulse_Step
      (Self : not null access Gtk_Entry_Record) return Gdouble
   is
      function Internal (Self : System.Address) return Gdouble;
      pragma Import (C, Internal, "gtk_entry_get_progress_pulse_step");
   begin
      return Internal (Get_Object (Self));
   end Get_Progress_Pulse_Step;

   --------------
   -- Get_Tabs --
   --------------

   function Get_Tabs
      (Self : not null access Gtk_Entry_Record)
       return Pango.Tabs.Pango_Tab_Array
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_entry_get_tabs");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Tabs;

   ---------------------
   -- Get_Text_Length --
   ---------------------

   function Get_Text_Length
      (Self : not null access Gtk_Entry_Record) return Guint16
   is
      function Internal (Self : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_entry_get_text_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Length;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_visibility");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Visibility;

   ----------------------------------
   -- Grab_Focus_Without_Selecting --
   ----------------------------------

   function Grab_Focus_Without_Selecting
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_entry_grab_focus_without_selecting");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Grab_Focus_Without_Selecting;

   --------------------
   -- Progress_Pulse --
   --------------------

   procedure Progress_Pulse (Self : not null access Gtk_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_entry_progress_pulse");
   begin
      Internal (Get_Object (Self));
   end Progress_Pulse;

   ----------------------
   -- Reset_Im_Context --
   ----------------------

   procedure Reset_Im_Context (Self : not null access Gtk_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_entry_reset_im_context");
   begin
      Internal (Get_Object (Self));
   end Reset_Im_Context;

   ---------------------------
   -- Set_Activates_Default --
   ---------------------------

   procedure Set_Activates_Default
      (Self    : not null access Gtk_Entry_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_activates_default");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Activates_Default;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Self   : not null access Gtk_Entry_Record;
       Xalign : Interfaces.C.C_float)
   is
      procedure Internal
         (Self   : System.Address;
          Xalign : Interfaces.C.C_float);
      pragma Import (C, Internal, "gtk_entry_set_alignment");
   begin
      Internal (Get_Object (Self), Xalign);
   end Set_Alignment;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Self  : not null access Gtk_Entry_Record;
       Attrs : Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal (Self : System.Address; Attrs : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_attributes");
   begin
      Internal (Get_Object (Self), Get_Object (Attrs));
   end Set_Attributes;

   ----------------
   -- Set_Buffer --
   ----------------

   procedure Set_Buffer
      (Self   : not null access Gtk_Entry_Record;
       Buffer : not null access Gtk.Entry_Buffer.Gtk_Entry_Buffer_Record'Class)
   is
      procedure Internal (Self : System.Address; Buffer : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_buffer");
   begin
      Internal (Get_Object (Self), Get_Object (Buffer));
   end Set_Buffer;

   --------------------
   -- Set_Completion --
   --------------------

   procedure Set_Completion
      (Self       : not null access Gtk_Entry_Record;
       Completion : access Gtk.Entry_Completion.Gtk_Entry_Completion_Record'Class)
   is
      procedure Internal
         (Self       : System.Address;
          Completion : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_completion");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Completion)));
   end Set_Completion;

   --------------------
   -- Set_Extra_Menu --
   --------------------

   procedure Set_Extra_Menu
      (Self  : not null access Gtk_Entry_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (Self : System.Address; Model : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_extra_menu");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Model)));
   end Set_Extra_Menu;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame
      (Self    : not null access Gtk_Entry_Record;
       Setting : Boolean)
   is
      procedure Internal (Self : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_has_frame");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Setting));
   end Set_Has_Frame;

   --------------------------
   -- Set_Icon_Activatable --
   --------------------------

   procedure Set_Icon_Activatable
      (Self        : not null access Gtk_Entry_Record;
       Icon_Pos    : Gtk.Enums.Gtk_Entry_Icon_Position;
       Activatable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Icon_Pos    : Gtk.Enums.Gtk_Entry_Icon_Position;
          Activatable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_icon_activatable");
   begin
      Internal (Get_Object (Self), Icon_Pos, Boolean'Pos (Activatable));
   end Set_Icon_Activatable;

   --------------------------
   -- Set_Icon_Drag_Source --
   --------------------------

   procedure Set_Icon_Drag_Source
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Provider : not null access Gdk.Content_Provider.Gdk_Content_Provider_Record'Class;
       Actions  : Gdk.Drag.Drag_Action)
   is
      procedure Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
          Provider : System.Address;
          Actions  : Gdk.Drag.Drag_Action);
      pragma Import (C, Internal, "gtk_entry_set_icon_drag_source");
   begin
      Internal (Get_Object (Self), Icon_Pos, Get_Object (Provider), Actions);
   end Set_Icon_Drag_Source;

   -------------------------
   -- Set_Icon_From_Gicon --
   -------------------------

   procedure Set_Icon_From_Gicon
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Icon     : Glib.G_Icon.G_Icon)
   is
      procedure Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
          Icon     : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_gicon");
   begin
      Internal (Get_Object (Self), Icon_Pos, Icon);
   end Set_Icon_From_Gicon;

   -----------------------------
   -- Set_Icon_From_Icon_Name --
   -----------------------------

   procedure Set_Icon_From_Icon_Name
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Icon_Name : UTF8_String := "")
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
          Icon_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Icon_Name = "" then
         Tmp_Icon_Name := Gtkada.Types.Null_Ptr;
      else
         Tmp_Icon_Name := New_String (Icon_Name);
      end if;
      Internal (Get_Object (Self), Icon_Pos, Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_Icon_From_Icon_Name;

   -----------------------------
   -- Set_Icon_From_Paintable --
   -----------------------------

   procedure Set_Icon_From_Paintable
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Paintable : Gdk.Paintable.Gdk_Paintable)
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
          Paintable : Gdk.Paintable.Gdk_Paintable);
      pragma Import (C, Internal, "gtk_entry_set_icon_from_paintable");
   begin
      Internal (Get_Object (Self), Icon_Pos, Paintable);
   end Set_Icon_From_Paintable;

   ------------------------
   -- Set_Icon_Sensitive --
   ------------------------

   procedure Set_Icon_Sensitive
      (Self      : not null access Gtk_Entry_Record;
       Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
       Sensitive : Boolean)
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Pos  : Gtk.Enums.Gtk_Entry_Icon_Position;
          Sensitive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_icon_sensitive");
   begin
      Internal (Get_Object (Self), Icon_Pos, Boolean'Pos (Sensitive));
   end Set_Icon_Sensitive;

   -----------------------------
   -- Set_Icon_Tooltip_Markup --
   -----------------------------

   procedure Set_Icon_Tooltip_Markup
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Tooltip  : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
          Tooltip  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_markup");
      Tmp_Tooltip : Gtkada.Types.Chars_Ptr;
   begin
      if Tooltip = "" then
         Tmp_Tooltip := Gtkada.Types.Null_Ptr;
      else
         Tmp_Tooltip := New_String (Tooltip);
      end if;
      Internal (Get_Object (Self), Icon_Pos, Tmp_Tooltip);
      Free (Tmp_Tooltip);
   end Set_Icon_Tooltip_Markup;

   ---------------------------
   -- Set_Icon_Tooltip_Text --
   ---------------------------

   procedure Set_Icon_Tooltip_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Tooltip  : UTF8_String := "")
   is
      procedure Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
          Tooltip  : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_icon_tooltip_text");
      Tmp_Tooltip : Gtkada.Types.Chars_Ptr;
   begin
      if Tooltip = "" then
         Tmp_Tooltip := Gtkada.Types.Null_Ptr;
      else
         Tmp_Tooltip := New_String (Tooltip);
      end if;
      Internal (Get_Object (Self), Icon_Pos, Tmp_Tooltip);
      Free (Tmp_Tooltip);
   end Set_Icon_Tooltip_Text;

   ---------------------
   -- Set_Input_Hints --
   ---------------------

   procedure Set_Input_Hints
      (Self  : not null access Gtk_Entry_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints)
   is
      procedure Internal
         (Self  : System.Address;
          Hints : Gtk.Enums.Gtk_Input_Hints);
      pragma Import (C, Internal, "gtk_entry_set_input_hints");
   begin
      Internal (Get_Object (Self), Hints);
   end Set_Input_Hints;

   -----------------------
   -- Set_Input_Purpose --
   -----------------------

   procedure Set_Input_Purpose
      (Self    : not null access Gtk_Entry_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose)
   is
      procedure Internal
         (Self    : System.Address;
          Purpose : Gtk.Enums.Gtk_Input_Purpose);
      pragma Import (C, Internal, "gtk_entry_set_input_purpose");
   begin
      Internal (Get_Object (Self), Purpose);
   end Set_Input_Purpose;

   ------------------------
   -- Set_Invisible_Char --
   ------------------------

   procedure Set_Invisible_Char
      (Self : not null access Gtk_Entry_Record;
       Ch   : Gunichar)
   is
      procedure Internal (Self : System.Address; Ch : Gunichar);
      pragma Import (C, Internal, "gtk_entry_set_invisible_char");
   begin
      Internal (Get_Object (Self), Ch);
   end Set_Invisible_Char;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
      (Self : not null access Gtk_Entry_Record;
       Max  : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Max : Glib.Gint);
      pragma Import (C, Internal, "gtk_entry_set_max_length");
   begin
      Internal (Get_Object (Self), Max);
   end Set_Max_Length;

   ------------------------------
   -- Set_Menu_Entry_Icon_Text --
   ------------------------------

   procedure Set_Menu_Entry_Icon_Text
      (Self     : not null access Gtk_Entry_Record;
       Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
       Text     : UTF8_String)
   is
      procedure Internal
         (Self     : System.Address;
          Icon_Pos : Gtk.Enums.Gtk_Entry_Icon_Position;
          Text     : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_menu_entry_icon_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Icon_Pos, Tmp_Text);
      Free (Tmp_Text);
   end Set_Menu_Entry_Icon_Text;

   ------------------------
   -- Set_Overwrite_Mode --
   ------------------------

   procedure Set_Overwrite_Mode
      (Self      : not null access Gtk_Entry_Record;
       Overwrite : Boolean)
   is
      procedure Internal (Self : System.Address; Overwrite : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_overwrite_mode");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Overwrite));
   end Set_Overwrite_Mode;

   --------------------------
   -- Set_Placeholder_Text --
   --------------------------

   procedure Set_Placeholder_Text
      (Self : not null access Gtk_Entry_Record;
       Text : UTF8_String := "")
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_entry_set_placeholder_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr;
   begin
      if Text = "" then
         Tmp_Text := Gtkada.Types.Null_Ptr;
      else
         Tmp_Text := New_String (Text);
      end if;
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Placeholder_Text;

   ---------------------------
   -- Set_Progress_Fraction --
   ---------------------------

   procedure Set_Progress_Fraction
      (Self     : not null access Gtk_Entry_Record;
       Fraction : Gdouble)
   is
      procedure Internal (Self : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_entry_set_progress_fraction");
   begin
      Internal (Get_Object (Self), Fraction);
   end Set_Progress_Fraction;

   -----------------------------
   -- Set_Progress_Pulse_Step --
   -----------------------------

   procedure Set_Progress_Pulse_Step
      (Self     : not null access Gtk_Entry_Record;
       Fraction : Gdouble)
   is
      procedure Internal (Self : System.Address; Fraction : Gdouble);
      pragma Import (C, Internal, "gtk_entry_set_progress_pulse_step");
   begin
      Internal (Get_Object (Self), Fraction);
   end Set_Progress_Pulse_Step;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
      (Self : not null access Gtk_Entry_Record;
       Tabs : Pango.Tabs.Pango_Tab_Array)
   is
      procedure Internal (Self : System.Address; Tabs : System.Address);
      pragma Import (C, Internal, "gtk_entry_set_tabs");
   begin
      Internal (Get_Object (Self), Get_Object (Tabs));
   end Set_Tabs;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
      (Self    : not null access Gtk_Entry_Record;
       Visible : Boolean)
   is
      procedure Internal (Self : System.Address; Visible : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_visibility");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Visible));
   end Set_Visibility;

   --------------------------
   -- Unset_Invisible_Char --
   --------------------------

   procedure Unset_Invisible_Char (Self : not null access Gtk_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_entry_unset_invisible_char");
   begin
      Internal (Get_Object (Self));
   end Unset_Invisible_Char;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Entry_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority)
   is
      procedure Internal
         (Self     : System.Address;
          Message  : Gtkada.Types.Chars_Ptr;
          Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);
      pragma Import (C, Internal, "gtk_accessible_announce");
      Tmp_Message : Gtkada.Types.Chars_Ptr := New_String (Message);
   begin
      Internal (Get_Object (Self), Tmp_Message, Priority);
      Free (Tmp_Message);
   end Announce;

   --------------------------------------------
   -- Delegate_Get_Accessible_Platform_State --
   --------------------------------------------

   function Delegate_Get_Accessible_Platform_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_delegate_get_accessible_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Delegate_Get_Accessible_Platform_State;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection (Self : not null access Gtk_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");
   begin
      Internal (Get_Object (Self));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
      (Self      : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");
   begin
      Internal (Get_Object (Self), Start_Pos, End_Pos);
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

   ---------------------
   -- Finish_Delegate --
   ---------------------

   procedure Finish_Delegate (Self : not null access Gtk_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_finish_delegate");
   begin
      Internal (Get_Object (Self));
   end Finish_Delegate;

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_id");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Accessible_Id;

   ---------------------------
   -- Get_Accessible_Parent --
   ---------------------------

   function Get_Accessible_Parent
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_parent");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Parent;

   -------------------------
   -- Get_Accessible_Role --
   -------------------------

   function Get_Accessible_Role
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible_Role
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible_Role;
      pragma Import (C, Internal, "gtk_accessible_get_accessible_role");
   begin
      return Internal (Get_Object (Self));
   end Get_Accessible_Role;

   --------------------
   -- Get_At_Context --
   --------------------

   function Get_At_Context
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Atcontext.Gtk_Atcontext
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_accessible_get_at_context");
      Stub_Gtk_Atcontext : Gtk.Atcontext.Gtk_Atcontext_Record;
   begin
      return Gtk.Atcontext.Gtk_Atcontext (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Atcontext));
   end Get_At_Context;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : not null access Gtk_Entry_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_X      : access Glib.Gint;
          Acc_Y      : access Glib.Gint;
          Acc_Width  : access Glib.Gint;
          Acc_Height : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_bounds");
      Acc_X      : aliased Glib.Gint;
      Acc_Y      : aliased Glib.Gint;
      Acc_Width  : aliased Glib.Gint;
      Acc_Height : aliased Glib.Gint;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_X'Access, Acc_Y'Access, Acc_Width'Access, Acc_Height'Access);
      X.all := Acc_X;
      Y.all := Acc_Y;
      Width.all := Acc_Width;
      Height.all := Acc_Height;
      return Tmp_Return /= 0;
   end Get_Bounds;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Self      : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String
   is
      function Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Delegate --
   ------------------

   function Get_Delegate
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Editable.Gtk_Editable
   is
      function Internal
         (Self : System.Address) return Gtk.Editable.Gtk_Editable;
      pragma Import (C, Internal, "gtk_editable_get_delegate");
   begin
      return Internal (Get_Object (Self));
   end Get_Delegate;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Editable;

   ---------------------
   -- Get_Enable_Undo --
   ---------------------

   function Get_Enable_Undo
      (Self : not null access Gtk_Entry_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_enable_undo");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Enable_Undo;

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   -------------------------
   -- Get_Max_Width_Chars --
   -------------------------

   function Get_Max_Width_Chars
      (Self : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_max_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Width_Chars;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Entry_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_next_accessible_sibling");
   begin
      return Internal (Get_Object (Self));
   end Get_Next_Accessible_Sibling;

   ------------------------
   -- Get_Platform_State --
   ------------------------

   function Get_Platform_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean
   is
      function Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accessible_get_platform_state");
   begin
      return Internal (Get_Object (Self), State) /= 0;
   end Get_Platform_State;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Self          : not null access Gtk_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Self          : System.Address;
          Acc_Start_Pos : access Glib.Gint;
          Acc_End_Pos   : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Glib.Gint;
      Acc_End_Pos   : aliased Glib.Gint;
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtk_Entry_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_editable_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Text;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
      (Self : not null access Gtk_Entry_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_width_chars");
   begin
      return Internal (Get_Object (Self));
   end Get_Width_Chars;

   -------------------
   -- Init_Delegate --
   -------------------

   procedure Init_Delegate (Self : not null access Gtk_Entry_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_editable_init_delegate");
   begin
      Internal (Get_Object (Self));
   end Init_Delegate;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Self     : not null access Gtk_Entry_Record;
       Text     : UTF8_String;
       Length   : Glib.Gint;
       Position : in out Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Text     : Gtkada.Types.Chars_Ptr;
          Length   : Glib.Gint;
          Position : in out Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text, Length, Position);
      Free (Tmp_Text);
   end Insert_Text;

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

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Entry_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property)
   is
      procedure Internal
         (Self     : System.Address;
          Property : Gtk.Accessible.Gtk_Accessible_Property);
      pragma Import (C, Internal, "gtk_accessible_reset_property");
   begin
      Internal (Get_Object (Self), Property);
   end Reset_Property;

   --------------------
   -- Reset_Relation --
   --------------------

   procedure Reset_Relation
      (Self     : not null access Gtk_Entry_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation)
   is
      procedure Internal
         (Self     : System.Address;
          Relation : Gtk.Accessible.Gtk_Accessible_Relation);
      pragma Import (C, Internal, "gtk_accessible_reset_relation");
   begin
      Internal (Get_Object (Self), Relation);
   end Reset_Relation;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Self      : not null access Gtk_Entry_Record;
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Self      : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");
   begin
      Internal (Get_Object (Self), Start_Pos, End_Pos);
   end Select_Region;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Entry_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self         : System.Address;
          Parent       : Gtk.Accessible.Gtk_Accessible;
          Next_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_set_accessible_parent");
   begin
      Internal (Get_Object (Self), Parent, Next_Sibling);
   end Set_Accessible_Parent;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (Self        : not null access Gtk_Entry_Record;
       Is_Editable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Is_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ---------------------
   -- Set_Enable_Undo --
   ---------------------

   procedure Set_Enable_Undo
      (Self        : not null access Gtk_Entry_Record;
       Enable_Undo : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Enable_Undo : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_enable_undo");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Enable_Undo));
   end Set_Enable_Undo;

   -------------------------
   -- Set_Max_Width_Chars --
   -------------------------

   procedure Set_Max_Width_Chars
      (Self    : not null access Gtk_Entry_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_max_width_chars");
   begin
      Internal (Get_Object (Self), N_Chars);
   end Set_Max_Width_Chars;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Self     : not null access Gtk_Entry_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");
   begin
      Internal (Get_Object (Self), Position);
   end Set_Position;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self : not null access Gtk_Entry_Record;
       Text : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_editable_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Self), Tmp_Text);
      Free (Tmp_Text);
   end Set_Text;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
      (Self    : not null access Gtk_Entry_Record;
       N_Chars : Glib.Gint)
   is
      procedure Internal (Self : System.Address; N_Chars : Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_set_width_chars");
   begin
      Internal (Get_Object (Self), N_Chars);
   end Set_Width_Chars;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Entry_Record;
       Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Cell_Editable : System.Address;
          Event         : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Get_Object (Event));
   end Start_Editing;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Entry_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible)
   is
      procedure Internal
         (Self        : System.Address;
          New_Sibling : Gtk.Accessible.Gtk_Accessible);
      pragma Import (C, Internal, "gtk_accessible_update_next_accessible_sibling");
   begin
      Internal (Get_Object (Self), New_Sibling);
   end Update_Next_Accessible_Sibling;

   ---------------------------
   -- Update_Platform_State --
   ---------------------------

   procedure Update_Platform_State
      (Self  : not null access Gtk_Entry_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Entry_Icon_Position_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Entry_Icon_Position_Void);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Entry_Gtk_Entry_Icon_Position_Void;
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
       Handler : Cb_GObject_Gtk_Entry_Icon_Position_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gtk_Entry_Icon_Position_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Entry_Icon_Position_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Entry_Gtk_Entry_Icon_Position_Void);

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

end Gtk.GEntry;
