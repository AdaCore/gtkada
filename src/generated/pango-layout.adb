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

package body Pango.Layout is

   function Get_Text (Layout : access Pango_Layout_Record)
   return Gtkada.Types.Chars_Ptr
   is
      function Internal (Layout : System.Address)
      return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_layout_get_text");
   begin
      return Internal (Get_Object (Layout));
   end Get_Text;

   function From_Object_Free
     (B : access Pango_Layout_Iter'Class) return Pango_Layout_Iter
   is
      Result : constant Pango_Layout_Iter := Pango_Layout_Iter (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Pango_Layout_Iter is
      S : Pango_Layout_Iter;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   function From_Object_Free (B : access Pango_Layout_Line) return Pango_Layout_Line is
      Result : constant Pango_Layout_Line := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   package Type_Conversion_Pango_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Layout_Record);
   pragma Unreferenced (Type_Conversion_Pango_Layout);

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Layout  : out Pango_Layout;
       Context : not null access Pango.Context.Pango_Context_Record'Class)
   is
   begin
      Layout := new Pango_Layout_Record;
      Pango.Layout.Initialize (Layout, Context);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Layout  : not null access Pango_Layout_Record'Class;
       Context : not null access Pango.Context.Pango_Context_Record'Class)
   is
      function Internal (Context : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_new");
   begin
      if not Layout.Is_Created then
         Set_Object (Layout, Internal (Get_Object (Context)));
      end if;
   end Initialize;

   ----------------------
   -- Pango_Layout_New --
   ----------------------

   function Pango_Layout_New
      (Context : not null access Pango.Context.Pango_Context_Record'Class)
       return Pango_Layout
   is
      Layout : constant Pango_Layout := new Pango_Layout_Record;
   begin
      Pango.Layout.Initialize (Layout, Context);
      return Layout;
   end Pango_Layout_New;

   ------------------
   -- At_Last_Line --
   ------------------

   function At_Last_Line (Self : Pango_Layout_Iter) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_iter_at_last_line");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end At_Last_Line;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed (Layout : not null access Pango_Layout_Record) is
      procedure Internal (Layout : System.Address);
      pragma Import (C, Internal, "pango_layout_context_changed");
   begin
      Internal (Get_Object (Layout));
   end Context_Changed;

   ----------
   -- Copy --
   ----------

   function Copy
      (Layout : not null access Pango_Layout_Record) return Pango_Layout
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_copy");
      Stub_Pango_Layout : Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Layout)), Stub_Pango_Layout));
   end Copy;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Pango_Layout_Iter) return Pango_Layout_Iter is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_iter_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Self : Pango_Layout_Iter) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_layout_iter_free");
   begin
      Internal (Get_Object (Self));
   end Free;

   -------------------
   -- Get_Alignment --
   -------------------

   function Get_Alignment
      (Layout : not null access Pango_Layout_Record)
       return Pango.Enums.Alignment
   is
      function Internal
         (Layout : System.Address) return Pango.Enums.Alignment;
      pragma Import (C, Internal, "pango_layout_get_alignment");
   begin
      return Internal (Get_Object (Layout));
   end Get_Alignment;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Layout : not null access Pango_Layout_Record)
       return Pango.Attributes.Pango_Attr_List
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_get_attributes");
   begin
      return From_Object (Internal (Get_Object (Layout)));
   end Get_Attributes;

   ------------------
   -- Get_Auto_Dir --
   ------------------

   function Get_Auto_Dir
      (Layout : not null access Pango_Layout_Record) return Boolean
   is
      function Internal (Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_get_auto_dir");
   begin
      return Internal (Get_Object (Layout)) /= 0;
   end Get_Auto_Dir;

   ------------------
   -- Get_Baseline --
   ------------------

   function Get_Baseline
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_baseline");
   begin
      return Internal (Get_Object (Layout));
   end Get_Baseline;

   ------------------
   -- Get_Baseline --
   ------------------

   function Get_Baseline (Self : Pango_Layout_Iter) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_iter_get_baseline");
   begin
      return Internal (Get_Object (Self));
   end Get_Baseline;

   ----------------------
   -- Get_Char_Extents --
   ----------------------

   procedure Get_Char_Extents
      (Self         : Pango_Layout_Iter;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : System.Address;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_iter_get_char_extents");
   begin
      Internal (Get_Object (Self), Logical_Rect);
   end Get_Char_Extents;

   -------------------------
   -- Get_Character_Count --
   -------------------------

   function Get_Character_Count
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_character_count");
   begin
      return Internal (Get_Object (Layout));
   end Get_Character_Count;

   -------------------------
   -- Get_Cluster_Extents --
   -------------------------

   procedure Get_Cluster_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_iter_get_cluster_extents");
   begin
      Internal (Get_Object (Self), Ink_Rect, Logical_Rect);
   end Get_Cluster_Extents;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context
      (Layout : not null access Pango_Layout_Record)
       return Pango.Context.Pango_Context
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_get_context");
      Stub_Pango_Context : Pango.Context.Pango_Context_Record;
   begin
      return Pango.Context.Pango_Context (Get_User_Data (Internal (Get_Object (Layout)), Stub_Pango_Context));
   end Get_Context;

   --------------------
   -- Get_Cursor_Pos --
   --------------------

   procedure Get_Cursor_Pos
      (Layout     : not null access Pango_Layout_Record;
       Index      : Glib.Gint;
       Strong_Pos : out Pango_Rectangle;
       Weak_Pos   : out Pango_Rectangle)
   is
      procedure Internal
         (Layout     : System.Address;
          Index      : Glib.Gint;
          Strong_Pos : out Pango_Rectangle;
          Weak_Pos   : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_get_cursor_pos");
   begin
      Internal (Get_Object (Layout), Index, Strong_Pos, Weak_Pos);
   end Get_Cursor_Pos;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
      (Layout : not null access Pango_Layout_Record)
       return Pango_Ellipsize_Mode
   is
      function Internal
         (Layout : System.Address) return Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "pango_layout_get_ellipsize");
   begin
      return Internal (Get_Object (Layout));
   end Get_Ellipsize;

   -----------------
   -- Get_Extents --
   -----------------

   procedure Get_Extents
      (Layout       : not null access Pango_Layout_Record;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Layout       : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_get_extents");
   begin
      Internal (Get_Object (Layout), Ink_Rect, Logical_Rect);
   end Get_Extents;

   --------------------------
   -- Get_Font_Description --
   --------------------------

   function Get_Font_Description
      (Layout : not null access Pango_Layout_Record)
       return Pango.Font.Pango_Font_Description
   is
      function Internal
         (Layout : System.Address) return Pango.Font.Pango_Font_Description;
      pragma Import (C, Internal, "pango_layout_get_font_description");
   begin
      return Internal (Get_Object (Layout));
   end Get_Font_Description;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_height");
   begin
      return Internal (Get_Object (Layout));
   end Get_Height;

   ----------------
   -- Get_Indent --
   ----------------

   function Get_Indent
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_indent");
   begin
      return Internal (Get_Object (Layout));
   end Get_Indent;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Self : Pango_Layout_Iter) return Glib.Gint is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_iter_get_index");
   begin
      return Internal (Get_Object (Self));
   end Get_Index;

   --------------
   -- Get_Iter --
   --------------

   function Get_Iter
      (Layout : not null access Pango_Layout_Record'Class)
       return Pango_Layout_Iter
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_get_iter");
   begin
      return From_Object (Internal (Get_Object (Layout)));
   end Get_Iter;

   -----------------
   -- Get_Justify --
   -----------------

   function Get_Justify
      (Layout : not null access Pango_Layout_Record) return Boolean
   is
      function Internal (Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_get_justify");
   begin
      return Internal (Get_Object (Layout)) /= 0;
   end Get_Justify;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (Self : Pango_Layout_Iter) return Pango_Layout is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_iter_get_layout");
      Stub_Pango_Layout : Pango_Layout_Record;
   begin
      return Pango.Layout.Pango_Layout (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Layout));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Extents --
   ------------------------

   procedure Get_Layout_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_iter_get_layout_extents");
   begin
      Internal (Get_Object (Self), Ink_Rect, Logical_Rect);
   end Get_Layout_Extents;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
      (Layout : not null access Pango_Layout_Record;
       Line   : Glib.Gint) return Pango_Layout_Line
   is
      function Internal
         (Layout : System.Address;
          Line   : Glib.Gint) return access Pango_Layout_Line;
      pragma Import (C, Internal, "pango_layout_get_line");
   begin
      return Internal (Get_Object (Layout), Line).all;
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Self : Pango_Layout_Iter) return Pango_Layout_Line is
      function Internal
         (Self : System.Address) return access Pango_Layout_Line;
      pragma Import (C, Internal, "pango_layout_iter_get_line");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Line;

   --------------------
   -- Get_Line_Count --
   --------------------

   function Get_Line_Count
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_line_count");
   begin
      return Internal (Get_Object (Layout));
   end Get_Line_Count;

   ----------------------
   -- Get_Line_Extents --
   ----------------------

   procedure Get_Line_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_iter_get_line_extents");
   begin
      Internal (Get_Object (Self), Ink_Rect, Logical_Rect);
   end Get_Line_Extents;

   -----------------------
   -- Get_Line_Readonly --
   -----------------------

   function Get_Line_Readonly
      (Layout : not null access Pango_Layout_Record;
       Line   : Glib.Gint) return Pango_Layout_Line
   is
      function Internal
         (Layout : System.Address;
          Line   : Glib.Gint) return access Pango_Layout_Line;
      pragma Import (C, Internal, "pango_layout_get_line_readonly");
   begin
      return Internal (Get_Object (Layout), Line).all;
   end Get_Line_Readonly;

   -----------------------
   -- Get_Line_Readonly --
   -----------------------

   function Get_Line_Readonly
      (Self : Pango_Layout_Iter) return Pango_Layout_Line
   is
      function Internal
         (Self : System.Address) return access Pango_Layout_Line;
      pragma Import (C, Internal, "pango_layout_iter_get_line_readonly");
   begin
      return Internal (Get_Object (Self)).all;
   end Get_Line_Readonly;

   ---------------------
   -- Get_Line_Yrange --
   ---------------------

   procedure Get_Line_Yrange
      (Self : Pango_Layout_Iter;
       Y0   : out Glib.Gint;
       Y1   : out Glib.Gint)
   is
      procedure Internal
         (Self : System.Address;
          Y0   : out Glib.Gint;
          Y1   : out Glib.Gint);
      pragma Import (C, Internal, "pango_layout_iter_get_line_yrange");
   begin
      Internal (Get_Object (Self), Y0, Y1);
   end Get_Line_Yrange;

   -----------------------
   -- Get_Pixel_Extents --
   -----------------------

   procedure Get_Pixel_Extents
      (Layout       : not null access Pango_Layout_Record;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Layout       : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_get_pixel_extents");
   begin
      Internal (Get_Object (Layout), Ink_Rect, Logical_Rect);
   end Get_Pixel_Extents;

   --------------------
   -- Get_Pixel_Size --
   --------------------

   procedure Get_Pixel_Size
      (Layout : not null access Pango_Layout_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Layout : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "pango_layout_get_pixel_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Get_Pixel_Size;

   ---------------------
   -- Get_Run_Extents --
   ---------------------

   procedure Get_Run_Extents
      (Self         : Pango_Layout_Iter;
       Ink_Rect     : out Pango_Rectangle;
       Logical_Rect : out Pango_Rectangle)
   is
      procedure Internal
         (Self         : System.Address;
          Ink_Rect     : out Pango_Rectangle;
          Logical_Rect : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_iter_get_run_extents");
   begin
      Internal (Get_Object (Self), Ink_Rect, Logical_Rect);
   end Get_Run_Extents;

   ----------------
   -- Get_Serial --
   ----------------

   function Get_Serial
      (Layout : not null access Pango_Layout_Record) return Guint
   is
      function Internal (Layout : System.Address) return Guint;
      pragma Import (C, Internal, "pango_layout_get_serial");
   begin
      return Internal (Get_Object (Layout));
   end Get_Serial;

   -------------------------------
   -- Get_Single_Paragraph_Mode --
   -------------------------------

   function Get_Single_Paragraph_Mode
      (Layout : not null access Pango_Layout_Record) return Boolean
   is
      function Internal (Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_get_single_paragraph_mode");
   begin
      return Internal (Get_Object (Layout)) /= 0;
   end Get_Single_Paragraph_Mode;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
      (Layout : not null access Pango_Layout_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Layout : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "pango_layout_get_size");
   begin
      Internal (Get_Object (Layout), Width, Height);
   end Get_Size;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_spacing");
   begin
      return Internal (Get_Object (Layout));
   end Get_Spacing;

   --------------
   -- Get_Tabs --
   --------------

   function Get_Tabs
      (Layout : not null access Pango_Layout_Record)
       return Pango.Tabs.Pango_Tab_Array
   is
      function Internal (Layout : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_layout_get_tabs");
   begin
      return From_Object (Internal (Get_Object (Layout)));
   end Get_Tabs;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Layout : not null access Pango_Layout_Record) return UTF8_String
   is
      function Internal
         (Layout : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "pango_layout_get_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Layout)));
   end Get_Text;

   ------------------------------
   -- Get_Unknown_Glyphs_Count --
   ------------------------------

   function Get_Unknown_Glyphs_Count
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_unknown_glyphs_count");
   begin
      return Internal (Get_Object (Layout));
   end Get_Unknown_Glyphs_Count;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width
      (Layout : not null access Pango_Layout_Record) return Glib.Gint
   is
      function Internal (Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "pango_layout_get_width");
   begin
      return Internal (Get_Object (Layout));
   end Get_Width;

   --------------
   -- Get_Wrap --
   --------------

   function Get_Wrap
      (Layout : not null access Pango_Layout_Record)
       return Pango.Enums.Wrap_Mode
   is
      function Internal
         (Layout : System.Address) return Pango.Enums.Wrap_Mode;
      pragma Import (C, Internal, "pango_layout_get_wrap");
   begin
      return Internal (Get_Object (Layout));
   end Get_Wrap;

   ---------------------
   -- Index_To_Line_X --
   ---------------------

   procedure Index_To_Line_X
      (Layout   : not null access Pango_Layout_Record;
       Index    : Glib.Gint;
       Trailing : Boolean;
       Line     : out Glib.Gint;
       X_Pos    : out Glib.Gint)
   is
      procedure Internal
         (Layout   : System.Address;
          Index    : Glib.Gint;
          Trailing : Glib.Gboolean;
          Line     : out Glib.Gint;
          X_Pos    : out Glib.Gint);
      pragma Import (C, Internal, "pango_layout_index_to_line_x");
   begin
      Internal (Get_Object (Layout), Index, Boolean'Pos (Trailing), Line, X_Pos);
   end Index_To_Line_X;

   ------------------
   -- Index_To_Pos --
   ------------------

   procedure Index_To_Pos
      (Layout : not null access Pango_Layout_Record;
       Index  : Glib.Gint;
       Pos    : out Pango_Rectangle)
   is
      procedure Internal
         (Layout : System.Address;
          Index  : Glib.Gint;
          Pos    : out Pango_Rectangle);
      pragma Import (C, Internal, "pango_layout_index_to_pos");
   begin
      Internal (Get_Object (Layout), Index, Pos);
   end Index_To_Pos;

   -------------------
   -- Is_Ellipsized --
   -------------------

   function Is_Ellipsized
      (Layout : not null access Pango_Layout_Record) return Boolean
   is
      function Internal (Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_is_ellipsized");
   begin
      return Internal (Get_Object (Layout)) /= 0;
   end Is_Ellipsized;

   ----------------
   -- Is_Wrapped --
   ----------------

   function Is_Wrapped
      (Layout : not null access Pango_Layout_Record) return Boolean
   is
      function Internal (Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_is_wrapped");
   begin
      return Internal (Get_Object (Layout)) /= 0;
   end Is_Wrapped;

   --------------------------
   -- Move_Cursor_Visually --
   --------------------------

   procedure Move_Cursor_Visually
      (Layout       : not null access Pango_Layout_Record;
       Strong       : Boolean;
       Old_Index    : Glib.Gint;
       Old_Trailing : Glib.Gint;
       Direction    : Glib.Gint;
       New_Index    : out Glib.Gint;
       New_Trailing : out Glib.Gint)
   is
      procedure Internal
         (Layout       : System.Address;
          Strong       : Glib.Gboolean;
          Old_Index    : Glib.Gint;
          Old_Trailing : Glib.Gint;
          Direction    : Glib.Gint;
          New_Index    : out Glib.Gint;
          New_Trailing : out Glib.Gint);
      pragma Import (C, Internal, "pango_layout_move_cursor_visually");
   begin
      Internal (Get_Object (Layout), Boolean'Pos (Strong), Old_Index, Old_Trailing, Direction, New_Index, New_Trailing);
   end Move_Cursor_Visually;

   ---------------
   -- Next_Char --
   ---------------

   function Next_Char (Self : Pango_Layout_Iter) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_iter_next_char");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Next_Char;

   ------------------
   -- Next_Cluster --
   ------------------

   function Next_Cluster (Self : Pango_Layout_Iter) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_iter_next_cluster");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Next_Cluster;

   ---------------
   -- Next_Line --
   ---------------

   function Next_Line (Self : Pango_Layout_Iter) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_iter_next_line");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Next_Line;

   --------------
   -- Next_Run --
   --------------

   function Next_Run (Self : Pango_Layout_Iter) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_iter_next_run");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Next_Run;

   -------------------
   -- Set_Alignment --
   -------------------

   procedure Set_Alignment
      (Layout    : not null access Pango_Layout_Record;
       Alignment : Pango.Enums.Alignment)
   is
      procedure Internal
         (Layout    : System.Address;
          Alignment : Pango.Enums.Alignment);
      pragma Import (C, Internal, "pango_layout_set_alignment");
   begin
      Internal (Get_Object (Layout), Alignment);
   end Set_Alignment;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Layout : not null access Pango_Layout_Record;
       Attrs  : Pango.Attributes.Pango_Attr_List)
   is
      procedure Internal (Layout : System.Address; Attrs : System.Address);
      pragma Import (C, Internal, "pango_layout_set_attributes");
   begin
      Internal (Get_Object (Layout), Get_Object (Attrs));
   end Set_Attributes;

   ------------------
   -- Set_Auto_Dir --
   ------------------

   procedure Set_Auto_Dir
      (Layout   : not null access Pango_Layout_Record;
       Auto_Dir : Boolean)
   is
      procedure Internal (Layout : System.Address; Auto_Dir : Glib.Gboolean);
      pragma Import (C, Internal, "pango_layout_set_auto_dir");
   begin
      Internal (Get_Object (Layout), Boolean'Pos (Auto_Dir));
   end Set_Auto_Dir;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
      (Layout    : not null access Pango_Layout_Record;
       Ellipsize : Pango_Ellipsize_Mode)
   is
      procedure Internal
         (Layout    : System.Address;
          Ellipsize : Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "pango_layout_set_ellipsize");
   begin
      Internal (Get_Object (Layout), Ellipsize);
   end Set_Ellipsize;

   --------------------------
   -- Set_Font_Description --
   --------------------------

   procedure Set_Font_Description
      (Layout : not null access Pango_Layout_Record;
       Desc   : Pango.Font.Pango_Font_Description)
   is
      procedure Internal
         (Layout : System.Address;
          Desc   : Pango.Font.Pango_Font_Description);
      pragma Import (C, Internal, "pango_layout_set_font_description");
   begin
      Internal (Get_Object (Layout), Desc);
   end Set_Font_Description;

   ----------------
   -- Set_Height --
   ----------------

   procedure Set_Height
      (Layout : not null access Pango_Layout_Record;
       Height : Glib.Gint)
   is
      procedure Internal (Layout : System.Address; Height : Glib.Gint);
      pragma Import (C, Internal, "pango_layout_set_height");
   begin
      Internal (Get_Object (Layout), Height);
   end Set_Height;

   ----------------
   -- Set_Indent --
   ----------------

   procedure Set_Indent
      (Layout : not null access Pango_Layout_Record;
       Indent : Glib.Gint)
   is
      procedure Internal (Layout : System.Address; Indent : Glib.Gint);
      pragma Import (C, Internal, "pango_layout_set_indent");
   begin
      Internal (Get_Object (Layout), Indent);
   end Set_Indent;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify
      (Layout  : not null access Pango_Layout_Record;
       Justify : Boolean)
   is
      procedure Internal (Layout : System.Address; Justify : Glib.Gboolean);
      pragma Import (C, Internal, "pango_layout_set_justify");
   begin
      Internal (Get_Object (Layout), Boolean'Pos (Justify));
   end Set_Justify;

   ----------------
   -- Set_Markup --
   ----------------

   procedure Set_Markup
      (Layout : not null access Pango_Layout_Record;
       Markup : UTF8_String)
   is
      procedure Internal
         (Layout : System.Address;
          Markup : Gtkada.Types.Chars_Ptr;
          Length : Glib.Gint);
      pragma Import (C, Internal, "pango_layout_set_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr := New_String (Markup);
   begin
      Internal (Get_Object (Layout), Tmp_Markup, -1);
      Free (Tmp_Markup);
   end Set_Markup;

   ---------------------------
   -- Set_Markup_With_Accel --
   ---------------------------

   procedure Set_Markup_With_Accel
      (Layout       : not null access Pango_Layout_Record;
       Markup       : UTF8_String;
       Length       : Glib.Gint;
       Accel_Marker : Gunichar;
       Accel_Char   : out Gunichar)
   is
      procedure Internal
         (Layout       : System.Address;
          Markup       : Gtkada.Types.Chars_Ptr;
          Length       : Glib.Gint;
          Accel_Marker : Gunichar;
          Accel_Char   : out Gunichar);
      pragma Import (C, Internal, "pango_layout_set_markup_with_accel");
      Tmp_Markup : Gtkada.Types.Chars_Ptr := New_String (Markup);
   begin
      Internal (Get_Object (Layout), Tmp_Markup, Length, Accel_Marker, Accel_Char);
      Free (Tmp_Markup);
   end Set_Markup_With_Accel;

   -------------------------------
   -- Set_Single_Paragraph_Mode --
   -------------------------------

   procedure Set_Single_Paragraph_Mode
      (Layout  : not null access Pango_Layout_Record;
       Setting : Boolean)
   is
      procedure Internal (Layout : System.Address; Setting : Glib.Gboolean);
      pragma Import (C, Internal, "pango_layout_set_single_paragraph_mode");
   begin
      Internal (Get_Object (Layout), Boolean'Pos (Setting));
   end Set_Single_Paragraph_Mode;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Layout  : not null access Pango_Layout_Record;
       Spacing : Glib.Gint)
   is
      procedure Internal (Layout : System.Address; Spacing : Glib.Gint);
      pragma Import (C, Internal, "pango_layout_set_spacing");
   begin
      Internal (Get_Object (Layout), Spacing);
   end Set_Spacing;

   --------------
   -- Set_Tabs --
   --------------

   procedure Set_Tabs
      (Layout : not null access Pango_Layout_Record;
       Tabs   : Pango.Tabs.Pango_Tab_Array)
   is
      procedure Internal (Layout : System.Address; Tabs : System.Address);
      pragma Import (C, Internal, "pango_layout_set_tabs");
   begin
      Internal (Get_Object (Layout), Get_Object (Tabs));
   end Set_Tabs;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Layout : not null access Pango_Layout_Record;
       Text   : UTF8_String)
   is
      procedure Internal
         (Layout : System.Address;
          Text   : Gtkada.Types.Chars_Ptr;
          Length : Glib.Gint);
      pragma Import (C, Internal, "pango_layout_set_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
   begin
      Internal (Get_Object (Layout), Tmp_Text, -1);
      Free (Tmp_Text);
   end Set_Text;

   ---------------
   -- Set_Width --
   ---------------

   procedure Set_Width
      (Layout : not null access Pango_Layout_Record;
       Width  : Glib.Gint)
   is
      procedure Internal (Layout : System.Address; Width : Glib.Gint);
      pragma Import (C, Internal, "pango_layout_set_width");
   begin
      Internal (Get_Object (Layout), Width);
   end Set_Width;

   --------------
   -- Set_Wrap --
   --------------

   procedure Set_Wrap
      (Layout : not null access Pango_Layout_Record;
       Wrap   : Pango.Enums.Wrap_Mode)
   is
      procedure Internal
         (Layout : System.Address;
          Wrap   : Pango.Enums.Wrap_Mode);
      pragma Import (C, Internal, "pango_layout_set_wrap");
   begin
      Internal (Get_Object (Layout), Wrap);
   end Set_Wrap;

   -----------------
   -- Xy_To_Index --
   -----------------

   procedure Xy_To_Index
      (Layout   : not null access Pango_Layout_Record;
       X        : Glib.Gint;
       Y        : Glib.Gint;
       Index    : out Glib.Gint;
       Trailing : out Glib.Gint;
       Exact    : out Boolean)
   is
      function Internal
         (Layout       : System.Address;
          X            : Glib.Gint;
          Y            : Glib.Gint;
          Acc_Index    : access Glib.Gint;
          Acc_Trailing : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "pango_layout_xy_to_index");
      Acc_Index    : aliased Glib.Gint;
      Acc_Trailing : aliased Glib.Gint;
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Layout), X, Y, Acc_Index'Access, Acc_Trailing'Access);
      Index := Acc_Index;
      Trailing := Acc_Trailing;
      Exact := Tmp_Return /= 0;
   end Xy_To_Index;

end Pango.Layout;
