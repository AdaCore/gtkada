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

package body Gtk.Text_View is

   package Type_Conversion_Gtk_Text_View is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Text_View_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Text_View);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (View : out Gtk_Text_View) is
   begin
      View := new Gtk_Text_View_Record;
      Gtk.Text_View.Initialize (View);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (View   : out Gtk_Text_View;
       Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
   is
   begin
      View := new Gtk_Text_View_Record;
      Gtk.Text_View.Initialize (View, Buffer);
   end Gtk_New;

   -----------------------
   -- Gtk_Text_View_New --
   -----------------------

   function Gtk_Text_View_New return Gtk_Text_View is
      View : constant Gtk_Text_View := new Gtk_Text_View_Record;
   begin
      Gtk.Text_View.Initialize (View);
      return View;
   end Gtk_Text_View_New;

   -----------------------------------
   -- Gtk_Text_View_New_With_Buffer --
   -----------------------------------

   function Gtk_Text_View_New_With_Buffer
      (Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
       return Gtk_Text_View
   is
      View : constant Gtk_Text_View := new Gtk_Text_View_Record;
   begin
      Gtk.Text_View.Initialize (View, Buffer);
      return View;
   end Gtk_Text_View_New_With_Buffer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (View : not null access Gtk_Text_View_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_text_view_new");
   begin
      if not View.Is_Created then
         Set_Object (View, Internal);
      end if;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (View   : not null access Gtk_Text_View_Record'Class;
       Buffer : not null access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
   is
      function Internal (Buffer : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_new_with_buffer");
   begin
      if not View.Is_Created then
         Set_Object (View, Internal (Get_Object (Buffer)));
      end if;
   end Initialize;

   -------------------------
   -- Add_Child_At_Anchor --
   -------------------------

   procedure Add_Child_At_Anchor
      (View   : not null access Gtk_Text_View_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Anchor : not null access Gtk.Text_Child_Anchor.Gtk_Text_Child_Anchor_Record'Class)
   is
      procedure Internal
         (View   : System.Address;
          Child  : System.Address;
          Anchor : System.Address);
      pragma Import (C, Internal, "gtk_text_view_add_child_at_anchor");
   begin
      Internal (Get_Object (View), Get_Object (Child), Get_Object (Anchor));
   end Add_Child_At_Anchor;

   -----------------
   -- Add_Overlay --
   -----------------

   procedure Add_Overlay
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Xpos  : Glib.Gint;
       Ypos  : Glib.Gint)
   is
      procedure Internal
         (View  : System.Address;
          Child : System.Address;
          Xpos  : Glib.Gint;
          Ypos  : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_add_overlay");
   begin
      Internal (Get_Object (View), Get_Object (Child), Xpos, Ypos);
   end Add_Overlay;

   ---------------------------
   -- Backward_Display_Line --
   ---------------------------

   function Backward_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   is
      function Internal
         (View : System.Address;
          Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_backward_display_line");
   begin
      return Internal (Get_Object (View), Iter) /= 0;
   end Backward_Display_Line;

   ---------------------------------
   -- Backward_Display_Line_Start --
   ---------------------------------

   function Backward_Display_Line_Start
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   is
      function Internal
         (View : System.Address;
          Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_backward_display_line_start");
   begin
      return Internal (Get_Object (View), Iter) /= 0;
   end Backward_Display_Line_Start;

   -----------------------------
   -- Buffer_To_Window_Coords --
   -----------------------------

   procedure Buffer_To_Window_Coords
      (View     : not null access Gtk_Text_View_Record;
       Win      : Gtk.Enums.Gtk_Text_Window_Type;
       Buffer_X : Glib.Gint;
       Buffer_Y : Glib.Gint;
       Window_X : out Glib.Gint;
       Window_Y : out Glib.Gint)
   is
      procedure Internal
         (View     : System.Address;
          Win      : Gtk.Enums.Gtk_Text_Window_Type;
          Buffer_X : Glib.Gint;
          Buffer_Y : Glib.Gint;
          Window_X : out Glib.Gint;
          Window_Y : out Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_buffer_to_window_coords");
   begin
      Internal (Get_Object (View), Win, Buffer_X, Buffer_Y, Window_X, Window_Y);
   end Buffer_To_Window_Coords;

   --------------------------
   -- Forward_Display_Line --
   --------------------------

   function Forward_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   is
      function Internal
         (View : System.Address;
          Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_forward_display_line");
   begin
      return Internal (Get_Object (View), Iter) /= 0;
   end Forward_Display_Line;

   ------------------------------
   -- Forward_Display_Line_End --
   ------------------------------

   function Forward_Display_Line_End
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   is
      function Internal
         (View : System.Address;
          Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_forward_display_line_end");
   begin
      return Internal (Get_Object (View), Iter) /= 0;
   end Forward_Display_Line_End;

   ---------------------
   -- Get_Accepts_Tab --
   ---------------------

   function Get_Accepts_Tab
      (View : not null access Gtk_Text_View_Record) return Boolean
   is
      function Internal (View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_accepts_tab");
   begin
      return Internal (Get_Object (View)) /= 0;
   end Get_Accepts_Tab;

   -----------------------
   -- Get_Bottom_Margin --
   -----------------------

   function Get_Bottom_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_bottom_margin");
   begin
      return Internal (Get_Object (View));
   end Get_Bottom_Margin;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Text_Buffer.Gtk_Text_Buffer
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_buffer");
      Stub_Gtk_Text_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer_Record;
   begin
      return Gtk.Text_Buffer.Gtk_Text_Buffer (Get_User_Data (Internal (Get_Object (View)), Stub_Gtk_Text_Buffer));
   end Get_Buffer;

   --------------------------
   -- Get_Cursor_Locations --
   --------------------------

   procedure Get_Cursor_Locations
      (View   : not null access Gtk_Text_View_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Strong : out Gdk.Rectangle.Gdk_Rectangle;
       Weak   : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (View   : System.Address;
          Iter   : System.Address;
          Strong : out Gdk.Rectangle.Gdk_Rectangle;
          Weak   : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_text_view_get_cursor_locations");
   begin
      Internal (Get_Object (View), Iter_Or_Null (Iter'Address), Strong, Weak);
   end Get_Cursor_Locations;

   ------------------------
   -- Get_Cursor_Visible --
   ------------------------

   function Get_Cursor_Visible
      (View : not null access Gtk_Text_View_Record) return Boolean
   is
      function Internal (View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_cursor_visible");
   begin
      return Internal (Get_Object (View)) /= 0;
   end Get_Cursor_Visible;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
      (View : not null access Gtk_Text_View_Record) return Boolean
   is
      function Internal (View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_editable");
   begin
      return Internal (Get_Object (View)) /= 0;
   end Get_Editable;

   --------------------
   -- Get_Extra_Menu --
   --------------------

   function Get_Extra_Menu
      (View : not null access Gtk_Text_View_Record)
       return Glib.Menu_Model.Gmenu_Model
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_extra_menu");
      Stub_Gmenu_Model : Glib.Menu_Model.Gmenu_Model_Record;
   begin
      return Glib.Menu_Model.Gmenu_Model (Get_User_Data (Internal (Get_Object (View)), Stub_Gmenu_Model));
   end Get_Extra_Menu;

   ----------------
   -- Get_Gutter --
   ----------------

   function Get_Gutter
      (View : not null access Gtk_Text_View_Record;
       Win  : Gtk.Enums.Gtk_Text_Window_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (View : System.Address;
          Win  : Gtk.Enums.Gtk_Text_Window_Type) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_gutter");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (View), Win), Stub_Gtk_Widget));
   end Get_Gutter;

   ----------------
   -- Get_Indent --
   ----------------

   function Get_Indent
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_indent");
   begin
      return Internal (Get_Object (View));
   end Get_Indent;

   ---------------------
   -- Get_Input_Hints --
   ---------------------

   function Get_Input_Hints
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Input_Hints
   is
      function Internal
         (View : System.Address) return Gtk.Enums.Gtk_Input_Hints;
      pragma Import (C, Internal, "gtk_text_view_get_input_hints");
   begin
      return Internal (Get_Object (View));
   end Get_Input_Hints;

   -----------------------
   -- Get_Input_Purpose --
   -----------------------

   function Get_Input_Purpose
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Input_Purpose
   is
      function Internal
         (View : System.Address) return Gtk.Enums.Gtk_Input_Purpose;
      pragma Import (C, Internal, "gtk_text_view_get_input_purpose");
   begin
      return Internal (Get_Object (View));
   end Get_Input_Purpose;

   --------------------------
   -- Get_Iter_At_Location --
   --------------------------

   function Get_Iter_At_Location
      (View : not null access Gtk_Text_View_Record;
       Iter : access Gtk.Text_Iter.Gtk_Text_Iter;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Boolean
   is
      function Internal
         (View     : System.Address;
          Acc_Iter : access Gtk.Text_Iter.Gtk_Text_Iter;
          X        : Glib.Gint;
          Y        : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_iter_at_location");
      Acc_Iter     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Acc_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (View), Tmp_Acc_Iter'Access, X, Y);
      Acc_Iter := Tmp_Acc_Iter;
      Iter.all := Acc_Iter;
      return Tmp_Return /= 0;
   end Get_Iter_At_Location;

   --------------------------
   -- Get_Iter_At_Position --
   --------------------------

   function Get_Iter_At_Position
      (View     : not null access Gtk_Text_View_Record;
       Iter     : access Gtk.Text_Iter.Gtk_Text_Iter;
       Trailing : access Glib.Gint;
       X        : Glib.Gint;
       Y        : Glib.Gint) return Boolean
   is
      function Internal
         (View         : System.Address;
          Acc_Iter     : access Gtk.Text_Iter.Gtk_Text_Iter;
          Acc_Trailing : access Glib.Gint;
          X            : Glib.Gint;
          Y            : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_iter_at_position");
      Acc_Iter     : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Acc_Trailing : aliased Glib.Gint;
      Tmp_Acc_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (View), Tmp_Acc_Iter'Access, Acc_Trailing'Access, X, Y);
      Acc_Iter := Tmp_Acc_Iter;
      Iter.all := Acc_Iter;
      if Trailing /= null then
         Trailing.all := Acc_Trailing;
      end if;
      return Tmp_Return /= 0;
   end Get_Iter_At_Position;

   -----------------------
   -- Get_Iter_Location --
   -----------------------

   procedure Get_Iter_Location
      (View     : not null access Gtk_Text_View_Record;
       Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
       Location : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (View     : System.Address;
          Iter     : Gtk.Text_Iter.Gtk_Text_Iter;
          Location : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_text_view_get_iter_location");
   begin
      Internal (Get_Object (View), Iter, Location);
   end Get_Iter_Location;

   -----------------------
   -- Get_Justification --
   -----------------------

   function Get_Justification
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Justification
   is
      function Internal
         (View : System.Address) return Gtk.Enums.Gtk_Justification;
      pragma Import (C, Internal, "gtk_text_view_get_justification");
   begin
      return Internal (Get_Object (View));
   end Get_Justification;

   ---------------------
   -- Get_Left_Margin --
   ---------------------

   function Get_Left_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_left_margin");
   begin
      return Internal (Get_Object (View));
   end Get_Left_Margin;

   -------------------
   -- Get_Line_At_Y --
   -------------------

   procedure Get_Line_At_Y
      (View        : not null access Gtk_Text_View_Record;
       Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
       Y           : Glib.Gint;
       Line_Top    : out Glib.Gint)
   is
      procedure Internal
         (View        : System.Address;
          Target_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
          Y           : Glib.Gint;
          Line_Top    : out Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_get_line_at_y");
      Tmp_Target_Iter : aliased Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      Internal (Get_Object (View), Tmp_Target_Iter, Y, Line_Top);
      Target_Iter := Tmp_Target_Iter;
   end Get_Line_At_Y;

   ---------------------
   -- Get_Line_Yrange --
   ---------------------

   procedure Get_Line_Yrange
      (View   : not null access Gtk_Text_View_Record;
       Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
       Y      : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (View   : System.Address;
          Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
          Y      : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_get_line_yrange");
   begin
      Internal (Get_Object (View), Iter, Y, Height);
   end Get_Line_Yrange;

   ---------------------
   -- Get_Ltr_Context --
   ---------------------

   function Get_Ltr_Context
      (View : not null access Gtk_Text_View_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_ltr_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (View)), Stub_Pango_Context));
   end Get_Ltr_Context;

   -------------------
   -- Get_Monospace --
   -------------------

   function Get_Monospace
      (View : not null access Gtk_Text_View_Record) return Boolean
   is
      function Internal (View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_monospace");
   begin
      return Internal (Get_Object (View)) /= 0;
   end Get_Monospace;

   -------------------
   -- Get_Overwrite --
   -------------------

   function Get_Overwrite
      (View : not null access Gtk_Text_View_Record) return Boolean
   is
      function Internal (View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_get_overwrite");
   begin
      return Internal (Get_Object (View)) /= 0;
   end Get_Overwrite;

   ----------------------------
   -- Get_Pixels_Above_Lines --
   ----------------------------

   function Get_Pixels_Above_Lines
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_pixels_above_lines");
   begin
      return Internal (Get_Object (View));
   end Get_Pixels_Above_Lines;

   ----------------------------
   -- Get_Pixels_Below_Lines --
   ----------------------------

   function Get_Pixels_Below_Lines
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_pixels_below_lines");
   begin
      return Internal (Get_Object (View));
   end Get_Pixels_Below_Lines;

   ----------------------------
   -- Get_Pixels_Inside_Wrap --
   ----------------------------

   function Get_Pixels_Inside_Wrap
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_pixels_inside_wrap");
   begin
      return Internal (Get_Object (View));
   end Get_Pixels_Inside_Wrap;

   ----------------------
   -- Get_Right_Margin --
   ----------------------

   function Get_Right_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_right_margin");
   begin
      return Internal (Get_Object (View));
   end Get_Right_Margin;

   ---------------------
   -- Get_Rtl_Context --
   ---------------------

   function Get_Rtl_Context
      (View : not null access Gtk_Text_View_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_rtl_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (View)), Stub_Pango_Context));
   end Get_Rtl_Context;

   --------------
   -- Get_Tabs --
   --------------

   function Get_Tabs
      (View : not null access Gtk_Text_View_Record)
       return Pango.Tabs.Pango_Tab_Array
   is
      function Internal (View : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_text_view_get_tabs");
   begin
      return From_Object (Internal (Get_Object (View)));
   end Get_Tabs;

   --------------------
   -- Get_Top_Margin --
   --------------------

   function Get_Top_Margin
      (View : not null access Gtk_Text_View_Record) return Glib.Gint
   is
      function Internal (View : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_text_view_get_top_margin");
   begin
      return Internal (Get_Object (View));
   end Get_Top_Margin;

   ------------------------
   -- Get_Visible_Offset --
   ------------------------

   procedure Get_Visible_Offset
      (View     : not null access Gtk_Text_View_Record;
       X_Offset : out Gdouble;
       Y_Offset : out Gdouble)
   is
      procedure Internal
         (View     : System.Address;
          X_Offset : out Gdouble;
          Y_Offset : out Gdouble);
      pragma Import (C, Internal, "gtk_text_view_get_visible_offset");
   begin
      Internal (Get_Object (View), X_Offset, Y_Offset);
   end Get_Visible_Offset;

   ----------------------
   -- Get_Visible_Rect --
   ----------------------

   procedure Get_Visible_Rect
      (View         : not null access Gtk_Text_View_Record;
       Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle)
   is
      procedure Internal
         (View         : System.Address;
          Visible_Rect : out Gdk.Rectangle.Gdk_Rectangle);
      pragma Import (C, Internal, "gtk_text_view_get_visible_rect");
   begin
      Internal (Get_Object (View), Visible_Rect);
   end Get_Visible_Rect;

   -------------------
   -- Get_Wrap_Mode --
   -------------------

   function Get_Wrap_Mode
      (View : not null access Gtk_Text_View_Record)
       return Gtk.Enums.Gtk_Wrap_Mode
   is
      function Internal
         (View : System.Address) return Gtk.Enums.Gtk_Wrap_Mode;
      pragma Import (C, Internal, "gtk_text_view_get_wrap_mode");
   begin
      return Internal (Get_Object (View));
   end Get_Wrap_Mode;

   ------------------------
   -- Move_Mark_Onscreen --
   ------------------------

   function Move_Mark_Onscreen
      (View : not null access Gtk_Text_View_Record;
       Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
       return Boolean
   is
      function Internal
         (View : System.Address;
          Mark : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_move_mark_onscreen");
   begin
      return Internal (Get_Object (View), Get_Object (Mark)) /= 0;
   end Move_Mark_Onscreen;

   ------------------
   -- Move_Overlay --
   ------------------

   procedure Move_Overlay
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Xpos  : Glib.Gint;
       Ypos  : Glib.Gint)
   is
      procedure Internal
         (View  : System.Address;
          Child : System.Address;
          Xpos  : Glib.Gint;
          Ypos  : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_move_overlay");
   begin
      Internal (Get_Object (View), Get_Object (Child), Xpos, Ypos);
   end Move_Overlay;

   -------------------
   -- Move_Visually --
   -------------------

   function Move_Visually
      (View  : not null access Gtk_Text_View_Record;
       Iter  : Gtk.Text_Iter.Gtk_Text_Iter;
       Count : Glib.Gint) return Boolean
   is
      function Internal
         (View  : System.Address;
          Iter  : Gtk.Text_Iter.Gtk_Text_Iter;
          Count : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_move_visually");
   begin
      return Internal (Get_Object (View), Iter, Count) /= 0;
   end Move_Visually;

   ---------------------------
   -- Place_Cursor_Onscreen --
   ---------------------------

   function Place_Cursor_Onscreen
      (View : not null access Gtk_Text_View_Record) return Boolean
   is
      function Internal (View : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_place_cursor_onscreen");
   begin
      return Internal (Get_Object (View)) /= 0;
   end Place_Cursor_Onscreen;

   ------------
   -- Remove --
   ------------

   procedure Remove
      (View  : not null access Gtk_Text_View_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (View : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_text_view_remove");
   begin
      Internal (Get_Object (View), Get_Object (Child));
   end Remove;

   ------------------------
   -- Reset_Cursor_Blink --
   ------------------------

   procedure Reset_Cursor_Blink
      (View : not null access Gtk_Text_View_Record)
   is
      procedure Internal (View : System.Address);
      pragma Import (C, Internal, "gtk_text_view_reset_cursor_blink");
   begin
      Internal (Get_Object (View));
   end Reset_Cursor_Blink;

   ----------------------
   -- Reset_Im_Context --
   ----------------------

   procedure Reset_Im_Context (View : not null access Gtk_Text_View_Record) is
      procedure Internal (View : System.Address);
      pragma Import (C, Internal, "gtk_text_view_reset_im_context");
   begin
      Internal (Get_Object (View));
   end Reset_Im_Context;

   --------------------------
   -- Scroll_Mark_Onscreen --
   --------------------------

   procedure Scroll_Mark_Onscreen
      (View : not null access Gtk_Text_View_Record;
       Mark : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class)
   is
      procedure Internal (View : System.Address; Mark : System.Address);
      pragma Import (C, Internal, "gtk_text_view_scroll_mark_onscreen");
   begin
      Internal (Get_Object (View), Get_Object (Mark));
   end Scroll_Mark_Onscreen;

   --------------------
   -- Scroll_To_Iter --
   --------------------

   function Scroll_To_Iter
      (View          : not null access Gtk_Text_View_Record;
       Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
       Within_Margin : Gdouble;
       Use_Align     : Boolean;
       Xalign        : Gdouble;
       Yalign        : Gdouble) return Boolean
   is
      function Internal
         (View          : System.Address;
          Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
          Within_Margin : Gdouble;
          Use_Align     : Glib.Gboolean;
          Xalign        : Gdouble;
          Yalign        : Gdouble) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_scroll_to_iter");
   begin
      return Internal (Get_Object (View), Iter, Within_Margin, Boolean'Pos (Use_Align), Xalign, Yalign) /= 0;
   end Scroll_To_Iter;

   --------------------
   -- Scroll_To_Mark --
   --------------------

   procedure Scroll_To_Mark
      (View          : not null access Gtk_Text_View_Record;
       Mark          : not null access Gtk.Text_Mark.Gtk_Text_Mark_Record'Class;
       Within_Margin : Gdouble;
       Use_Align     : Boolean;
       Xalign        : Gdouble;
       Yalign        : Gdouble)
   is
      procedure Internal
         (View          : System.Address;
          Mark          : System.Address;
          Within_Margin : Gdouble;
          Use_Align     : Glib.Gboolean;
          Xalign        : Gdouble;
          Yalign        : Gdouble);
      pragma Import (C, Internal, "gtk_text_view_scroll_to_mark");
   begin
      Internal (Get_Object (View), Get_Object (Mark), Within_Margin, Boolean'Pos (Use_Align), Xalign, Yalign);
   end Scroll_To_Mark;

   ---------------------
   -- Set_Accepts_Tab --
   ---------------------

   procedure Set_Accepts_Tab
      (View        : not null access Gtk_Text_View_Record;
       Accepts_Tab : Boolean)
   is
      procedure Internal
         (View        : System.Address;
          Accepts_Tab : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_accepts_tab");
   begin
      Internal (Get_Object (View), Boolean'Pos (Accepts_Tab));
   end Set_Accepts_Tab;

   -----------------------
   -- Set_Bottom_Margin --
   -----------------------

   procedure Set_Bottom_Margin
      (View          : not null access Gtk_Text_View_Record;
       Bottom_Margin : Glib.Gint)
   is
      procedure Internal (View : System.Address; Bottom_Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_bottom_margin");
   begin
      Internal (Get_Object (View), Bottom_Margin);
   end Set_Bottom_Margin;

   ----------------
   -- Set_Buffer --
   ----------------

   procedure Set_Buffer
      (View   : not null access Gtk_Text_View_Record;
       Buffer : access Gtk.Text_Buffer.Gtk_Text_Buffer_Record'Class)
   is
      procedure Internal (View : System.Address; Buffer : System.Address);
      pragma Import (C, Internal, "gtk_text_view_set_buffer");
   begin
      Internal (Get_Object (View), Get_Object_Or_Null (GObject (Buffer)));
   end Set_Buffer;

   ------------------------
   -- Set_Cursor_Visible --
   ------------------------

   procedure Set_Cursor_Visible
      (View    : not null access Gtk_Text_View_Record;
       Setting : Boolean)
   is
      procedure Internal (View : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_cursor_visible");
   begin
      Internal (Get_Object (View), Boolean'Pos (Setting));
   end Set_Cursor_Visible;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (View    : not null access Gtk_Text_View_Record;
       Setting : Boolean)
   is
      procedure Internal (View : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_editable");
   begin
      Internal (Get_Object (View), Boolean'Pos (Setting));
   end Set_Editable;

   --------------------
   -- Set_Extra_Menu --
   --------------------

   procedure Set_Extra_Menu
      (View  : not null access Gtk_Text_View_Record;
       Model : access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      procedure Internal (View : System.Address; Model : System.Address);
      pragma Import (C, Internal, "gtk_text_view_set_extra_menu");
   begin
      Internal (Get_Object (View), Get_Object_Or_Null (GObject (Model)));
   end Set_Extra_Menu;

   ----------------
   -- Set_Gutter --
   ----------------

   procedure Set_Gutter
      (View   : not null access Gtk_Text_View_Record;
       Win    : Gtk.Enums.Gtk_Text_Window_Type;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (View   : System.Address;
          Win    : Gtk.Enums.Gtk_Text_Window_Type;
          Widget : System.Address);
      pragma Import (C, Internal, "gtk_text_view_set_gutter");
   begin
      Internal (Get_Object (View), Win, Get_Object_Or_Null (GObject (Widget)));
   end Set_Gutter;

   ----------------
   -- Set_Indent --
   ----------------

   procedure Set_Indent
      (View   : not null access Gtk_Text_View_Record;
       Indent : Glib.Gint)
   is
      procedure Internal (View : System.Address; Indent : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_indent");
   begin
      Internal (Get_Object (View), Indent);
   end Set_Indent;

   ---------------------
   -- Set_Input_Hints --
   ---------------------

   procedure Set_Input_Hints
      (View  : not null access Gtk_Text_View_Record;
       Hints : Gtk.Enums.Gtk_Input_Hints)
   is
      procedure Internal
         (View  : System.Address;
          Hints : Gtk.Enums.Gtk_Input_Hints);
      pragma Import (C, Internal, "gtk_text_view_set_input_hints");
   begin
      Internal (Get_Object (View), Hints);
   end Set_Input_Hints;

   -----------------------
   -- Set_Input_Purpose --
   -----------------------

   procedure Set_Input_Purpose
      (View    : not null access Gtk_Text_View_Record;
       Purpose : Gtk.Enums.Gtk_Input_Purpose)
   is
      procedure Internal
         (View    : System.Address;
          Purpose : Gtk.Enums.Gtk_Input_Purpose);
      pragma Import (C, Internal, "gtk_text_view_set_input_purpose");
   begin
      Internal (Get_Object (View), Purpose);
   end Set_Input_Purpose;

   -----------------------
   -- Set_Justification --
   -----------------------

   procedure Set_Justification
      (View          : not null access Gtk_Text_View_Record;
       Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
         (View          : System.Address;
          Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_text_view_set_justification");
   begin
      Internal (Get_Object (View), Justification);
   end Set_Justification;

   ---------------------
   -- Set_Left_Margin --
   ---------------------

   procedure Set_Left_Margin
      (View        : not null access Gtk_Text_View_Record;
       Left_Margin : Glib.Gint)
   is
      procedure Internal (View : System.Address; Left_Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_left_margin");
   begin
      Internal (Get_Object (View), Left_Margin);
   end Set_Left_Margin;

   -------------------
   -- Set_Monospace --
   -------------------

   procedure Set_Monospace
      (View      : not null access Gtk_Text_View_Record;
       Monospace : Boolean)
   is
      procedure Internal (View : System.Address; Monospace : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_monospace");
   begin
      Internal (Get_Object (View), Boolean'Pos (Monospace));
   end Set_Monospace;

   -------------------
   -- Set_Overwrite --
   -------------------

   procedure Set_Overwrite
      (View      : not null access Gtk_Text_View_Record;
       Overwrite : Boolean)
   is
      procedure Internal (View : System.Address; Overwrite : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_text_view_set_overwrite");
   begin
      Internal (Get_Object (View), Boolean'Pos (Overwrite));
   end Set_Overwrite;

   ----------------------------
   -- Set_Pixels_Above_Lines --
   ----------------------------

   procedure Set_Pixels_Above_Lines
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Above_Lines : Glib.Gint)
   is
      procedure Internal
         (View               : System.Address;
          Pixels_Above_Lines : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_pixels_above_lines");
   begin
      Internal (Get_Object (View), Pixels_Above_Lines);
   end Set_Pixels_Above_Lines;

   ----------------------------
   -- Set_Pixels_Below_Lines --
   ----------------------------

   procedure Set_Pixels_Below_Lines
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Below_Lines : Glib.Gint)
   is
      procedure Internal
         (View               : System.Address;
          Pixels_Below_Lines : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_pixels_below_lines");
   begin
      Internal (Get_Object (View), Pixels_Below_Lines);
   end Set_Pixels_Below_Lines;

   ----------------------------
   -- Set_Pixels_Inside_Wrap --
   ----------------------------

   procedure Set_Pixels_Inside_Wrap
      (View               : not null access Gtk_Text_View_Record;
       Pixels_Inside_Wrap : Glib.Gint)
   is
      procedure Internal
         (View               : System.Address;
          Pixels_Inside_Wrap : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_pixels_inside_wrap");
   begin
      Internal (Get_Object (View), Pixels_Inside_Wrap);
   end Set_Pixels_Inside_Wrap;

   ----------------------
   -- Set_Right_Margin --
   ----------------------

   procedure Set_Right_Margin
      (View         : not null access Gtk_Text_View_Record;
       Right_Margin : Glib.Gint)
   is
      procedure Internal (View : System.Address; Right_Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_right_margin");
   begin
      Internal (Get_Object (View), Right_Margin);
   end Set_Right_Margin;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
      (View : not null access Gtk_Text_View_Record;
       Tabs : Pango.Tabs.Pango_Tab_Array)
   is
      procedure Internal (View : System.Address; Tabs : System.Address);
      pragma Import (C, Internal, "gtk_text_view_set_tabs");
   begin
      Internal (Get_Object (View), Get_Object (Tabs));
   end Set_Tabs;

   --------------------
   -- Set_Top_Margin --
   --------------------

   procedure Set_Top_Margin
      (View       : not null access Gtk_Text_View_Record;
       Top_Margin : Glib.Gint)
   is
      procedure Internal (View : System.Address; Top_Margin : Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_set_top_margin");
   begin
      Internal (Get_Object (View), Top_Margin);
   end Set_Top_Margin;

   -------------------
   -- Set_Wrap_Mode --
   -------------------

   procedure Set_Wrap_Mode
      (View      : not null access Gtk_Text_View_Record;
       Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode)
   is
      procedure Internal
         (View      : System.Address;
          Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode);
      pragma Import (C, Internal, "gtk_text_view_set_wrap_mode");
   begin
      Internal (Get_Object (View), Wrap_Mode);
   end Set_Wrap_Mode;

   -------------------------
   -- Starts_Display_Line --
   -------------------------

   function Starts_Display_Line
      (View : not null access Gtk_Text_View_Record;
       Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean
   is
      function Internal
         (View : System.Address;
          Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_text_view_starts_display_line");
   begin
      return Internal (Get_Object (View), Iter) /= 0;
   end Starts_Display_Line;

   -----------------------------
   -- Window_To_Buffer_Coords --
   -----------------------------

   procedure Window_To_Buffer_Coords
      (View     : not null access Gtk_Text_View_Record;
       Win      : Gtk.Enums.Gtk_Text_Window_Type;
       Window_X : Glib.Gint;
       Window_Y : Glib.Gint;
       Buffer_X : out Glib.Gint;
       Buffer_Y : out Glib.Gint)
   is
      procedure Internal
         (View     : System.Address;
          Win      : Gtk.Enums.Gtk_Text_Window_Type;
          Window_X : Glib.Gint;
          Window_Y : Glib.Gint;
          Buffer_X : out Glib.Gint;
          Buffer_Y : out Glib.Gint);
      pragma Import (C, Internal, "gtk_text_view_window_to_buffer_coords");
   begin
      Internal (Get_Object (View), Win, Window_X, Window_Y, Buffer_X, Buffer_Y);
   end Window_To_Buffer_Coords;

   --------------
   -- Announce --
   --------------

   procedure Announce
      (Self     : not null access Gtk_Text_View_Record;
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

   -----------------------
   -- Get_Accessible_Id --
   -----------------------

   function Get_Accessible_Id
      (Self : not null access Gtk_Text_View_Record) return UTF8_String
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
      (Self : not null access Gtk_Text_View_Record)
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
      (Self : not null access Gtk_Text_View_Record)
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
      (Self : not null access Gtk_Text_View_Record)
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
      (Self   : not null access Gtk_Text_View_Record;
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

   --------------------------------
   -- Get_First_Accessible_Child --
   --------------------------------

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Text_View_Record)
       return Gtk.Accessible.Gtk_Accessible
   is
      function Internal
         (Self : System.Address) return Gtk.Accessible.Gtk_Accessible;
      pragma Import (C, Internal, "gtk_accessible_get_first_accessible_child");
   begin
      return Internal (Get_Object (Self));
   end Get_First_Accessible_Child;

   ---------------------------------
   -- Get_Next_Accessible_Sibling --
   ---------------------------------

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Text_View_Record)
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
      (Self  : not null access Gtk_Text_View_Record;
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

   --------------------
   -- Reset_Property --
   --------------------

   procedure Reset_Property
      (Self     : not null access Gtk_Text_View_Record;
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
      (Self     : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_State);
      pragma Import (C, Internal, "gtk_accessible_reset_state");
   begin
      Internal (Get_Object (Self), State);
   end Reset_State;

   ---------------------------
   -- Set_Accessible_Parent --
   ---------------------------

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Text_View_Record;
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

   ---------------------------
   -- Update_Caret_Position --
   ---------------------------

   procedure Update_Caret_Position
      (Self : not null access Gtk_Text_View_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_accessible_text_update_caret_position");
   begin
      Internal (Get_Object (Self));
   end Update_Caret_Position;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
      (Self    : not null access Gtk_Text_View_Record;
       Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
       Start   : Guint;
       The_End : Guint)
   is
      procedure Internal
         (Self    : System.Address;
          Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
          Start   : Guint;
          The_End : Guint);
      pragma Import (C, Internal, "gtk_accessible_text_update_contents");
   begin
      Internal (Get_Object (Self), Change, Start, The_End);
   end Update_Contents;

   ------------------------------------
   -- Update_Next_Accessible_Sibling --
   ------------------------------------

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State)
   is
      procedure Internal
         (Self  : System.Address;
          State : Gtk.Accessible.Gtk_Accessible_Platform_State);
      pragma Import (C, Internal, "gtk_accessible_update_platform_state");
   begin
      Internal (Get_Object (Self), State);
   end Update_Platform_State;

   ----------------------------
   -- Update_Selection_Bound --
   ----------------------------

   procedure Update_Selection_Bound
      (Self : not null access Gtk_Text_View_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_accessible_text_update_selection_bound");
   begin
      Internal (Get_Object (Self));
   end Update_Selection_Bound;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Delete_Type_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Delete_Type_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_UTF8_String_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_UTF8_String_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Scroll_Step_Gint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Scroll_Step_Gint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Text_View_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Text_View_Boolean_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Void);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_UTF8_String_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Boolean_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Delete_Type_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_UTF8_String_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Step_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Void);

   procedure Marsh_GObject_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Delete_Type_Gint_Void);

   procedure Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Movement_Step_Gint_Boolean_Void);

   procedure Marsh_GObject_Gtk_Scroll_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Scroll_Step_Gint_Void);

   procedure Marsh_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean);

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

   procedure Marsh_Gtk_Text_View_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_Boolean_Void);

   procedure Marsh_Gtk_Text_View_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_Gtk_Delete_Type_Gint_Void);

   procedure Marsh_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void);

   procedure Marsh_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void);

   procedure Marsh_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean);

   procedure Marsh_Gtk_Text_View_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_UTF8_String_Void);

   procedure Marsh_Gtk_Text_View_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Text_View_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_Gtk_Delete_Type_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_UTF8_String_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_UTF8_String_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Text_View_Boolean_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Text_View_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
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
      (Object  : access Gtk_Text_View_Record'Class;
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
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
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
      (Object  : access Gtk_Text_View_Record'Class;
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
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Scroll_Step_Gint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Scroll_Step_Gint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Text_View_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------
   -- Marsh_GObject_Boolean_Void --
   --------------------------------

   procedure Marsh_GObject_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Void;

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

   ---------------------------------------------
   -- Marsh_GObject_Gtk_Scroll_Step_Gint_Void --
   ---------------------------------------------

   procedure Marsh_GObject_Gtk_Scroll_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Scroll_Step_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Step (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Scroll_Step_Gint_Void;

   -----------------------------------------------------------------------------------------------
   -- Marsh_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean --
   -----------------------------------------------------------------------------------------------

   procedure Marsh_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Text_Extend_Selection (Params, 1), Unchecked_To_Gtk_Text_Iter (Params, 2), Unchecked_To_Gtk_Text_Iter (Params, 3), Unchecked_To_Gtk_Text_Iter (Params, 4));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;

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

   --------------------------------------
   -- Marsh_Gtk_Text_View_Boolean_Void --
   --------------------------------------

   procedure Marsh_Gtk_Text_View_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Boolean (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_Boolean_Void;

   ---------------------------------------------------
   -- Marsh_Gtk_Text_View_Gtk_Delete_Type_Gint_Void --
   ---------------------------------------------------

   procedure Marsh_Gtk_Text_View_Gtk_Delete_Type_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Delete_Type (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_Gtk_Delete_Type_Gint_Void;

   -------------------------------------------------------------
   -- Marsh_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void --
   -------------------------------------------------------------

   procedure Marsh_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Movement_Step (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void;

   ---------------------------------------------------
   -- Marsh_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void --
   ---------------------------------------------------

   procedure Marsh_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Scroll_Step (Params, 1), Unchecked_To_Gint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void;

   -----------------------------------------------------------------------------------------------------
   -- Marsh_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean --
   -----------------------------------------------------------------------------------------------------

   procedure Marsh_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Text_Extend_Selection (Params, 1), Unchecked_To_Gtk_Text_Iter (Params, 2), Unchecked_To_Gtk_Text_Iter (Params, 3), Unchecked_To_Gtk_Text_Iter (Params, 4));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;

   ------------------------------------------
   -- Marsh_Gtk_Text_View_UTF8_String_Void --
   ------------------------------------------

   procedure Marsh_Gtk_Text_View_UTF8_String_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_UTF8_String_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_UTF8_String (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_UTF8_String_Void;

   ------------------------------
   -- Marsh_Gtk_Text_View_Void --
   ------------------------------

   procedure Marsh_Gtk_Text_View_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Text_View_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Text_View := Gtk_Text_View (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Text_View_Void;

   ------------------
   -- On_Backspace --
   ------------------

   procedure On_Backspace
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "backspace" & ASCII.NUL, Call, After);
   end On_Backspace;

   ------------------
   -- On_Backspace --
   ------------------

   procedure On_Backspace
      (Self  : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "copy-clipboard" & ASCII.NUL, Call, After);
   end On_Copy_Clipboard;

   -----------------------
   -- On_Copy_Clipboard --
   -----------------------

   procedure On_Copy_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "cut-clipboard" & ASCII.NUL, Call, After);
   end On_Cut_Clipboard;

   ----------------------
   -- On_Cut_Clipboard --
   ----------------------

   procedure On_Cut_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Delete_Type_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "delete-from-cursor" & ASCII.NUL, Call, After);
   end On_Delete_From_Cursor;

   ---------------------------
   -- On_Delete_From_Cursor --
   ---------------------------

   procedure On_Delete_From_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Delete_Type_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "delete-from-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Delete_From_Cursor;

   -------------------------
   -- On_Extend_Selection --
   -------------------------

   procedure On_Extend_Selection
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "extend-selection" & ASCII.NUL, Call, After);
   end On_Extend_Selection;

   -------------------------
   -- On_Extend_Selection --
   -------------------------

   procedure On_Extend_Selection
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Text_Extend_Selection_Gtk_Text_Iter_Gtk_Text_Iter_Gtk_Text_Iter_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "extend-selection" & ASCII.NUL, Call, After, Slot);
   end On_Extend_Selection;

   -------------------------
   -- On_Insert_At_Cursor --
   -------------------------

   procedure On_Insert_At_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-at-cursor" & ASCII.NUL, Call, After);
   end On_Insert_At_Cursor;

   -------------------------
   -- On_Insert_At_Cursor --
   -------------------------

   procedure On_Insert_At_Cursor
      (Self  : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "insert-emoji" & ASCII.NUL, Call, After);
   end On_Insert_Emoji;

   ---------------------
   -- On_Insert_Emoji --
   ---------------------

   procedure On_Insert_Emoji
      (Self  : not null access Gtk_Text_View_Record;
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
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Movement_Step_Gint_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-cursor" & ASCII.NUL, Call, After);
   end On_Move_Cursor;

   --------------------
   -- On_Move_Cursor --
   --------------------

   procedure On_Move_Cursor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Movement_Step_Gint_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-cursor" & ASCII.NUL, Call, After, Slot);
   end On_Move_Cursor;

   ----------------------
   -- On_Move_Viewport --
   ----------------------

   procedure On_Move_Viewport
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Gtk_Scroll_Step_Gint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-viewport" & ASCII.NUL, Call, After);
   end On_Move_Viewport;

   ----------------------
   -- On_Move_Viewport --
   ----------------------

   procedure On_Move_Viewport
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Gtk_Scroll_Step_Gint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-viewport" & ASCII.NUL, Call, After, Slot);
   end On_Move_Viewport;

   ------------------------
   -- On_Paste_Clipboard --
   ------------------------

   procedure On_Paste_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "paste-clipboard" & ASCII.NUL, Call, After);
   end On_Paste_Clipboard;

   ------------------------
   -- On_Paste_Clipboard --
   ------------------------

   procedure On_Paste_Clipboard
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "paste-clipboard" & ASCII.NUL, Call, After, Slot);
   end On_Paste_Clipboard;

   ------------------------
   -- On_Preedit_Changed --
   ------------------------

   procedure On_Preedit_Changed
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_UTF8_String_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "preedit-changed" & ASCII.NUL, Call, After);
   end On_Preedit_Changed;

   ------------------------
   -- On_Preedit_Changed --
   ------------------------

   procedure On_Preedit_Changed
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "preedit-changed" & ASCII.NUL, Call, After, Slot);
   end On_Preedit_Changed;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Boolean_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-all" & ASCII.NUL, Call, After);
   end On_Select_All;

   -------------------
   -- On_Select_All --
   -------------------

   procedure On_Select_All
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Boolean_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-all" & ASCII.NUL, Call, After, Slot);
   end On_Select_All;

   -------------------
   -- On_Set_Anchor --
   -------------------

   procedure On_Set_Anchor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "set-anchor" & ASCII.NUL, Call, After);
   end On_Set_Anchor;

   -------------------
   -- On_Set_Anchor --
   -------------------

   procedure On_Set_Anchor
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "set-anchor" & ASCII.NUL, Call, After, Slot);
   end On_Set_Anchor;

   ------------------------------
   -- On_Toggle_Cursor_Visible --
   ------------------------------

   procedure On_Toggle_Cursor_Visible
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-cursor-visible" & ASCII.NUL, Call, After);
   end On_Toggle_Cursor_Visible;

   ------------------------------
   -- On_Toggle_Cursor_Visible --
   ------------------------------

   procedure On_Toggle_Cursor_Visible
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-cursor-visible" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Cursor_Visible;

   -------------------------
   -- On_Toggle_Overwrite --
   -------------------------

   procedure On_Toggle_Overwrite
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_Gtk_Text_View_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "toggle-overwrite" & ASCII.NUL, Call, After);
   end On_Toggle_Overwrite;

   -------------------------
   -- On_Toggle_Overwrite --
   -------------------------

   procedure On_Toggle_Overwrite
      (Self  : not null access Gtk_Text_View_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "toggle-overwrite" & ASCII.NUL, Call, After, Slot);
   end On_Toggle_Overwrite;

end Gtk.Text_View;
