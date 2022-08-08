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

package body Gtk.Grid is

   package Type_Conversion_Gtk_Grid is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Grid_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Grid);

   ------------------
   -- Gtk_Grid_New --
   ------------------

   function Gtk_Grid_New return Gtk_Grid is
      Self : constant Gtk_Grid := new Gtk_Grid_Record;
   begin
      Gtk.Grid.Initialize (Self);
      return Self;
   end Gtk_Grid_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Grid) is
   begin
      Self := new Gtk_Grid_Record;
      Gtk.Grid.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Grid_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_grid_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------
   -- Attach --
   ------------

   procedure Attach
      (Self   : not null access Gtk_Grid_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Left   : Glib.Gint;
       Top    : Glib.Gint;
       Width  : Glib.Gint := 1;
       Height : Glib.Gint := 1)
   is
      procedure Internal
         (Self   : System.Address;
          Child  : System.Address;
          Left   : Glib.Gint;
          Top    : Glib.Gint;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_attach");
   begin
      Internal (Get_Object (Self), Get_Object (Child), Left, Top, Width, Height);
   end Attach;

   --------------------
   -- Attach_Next_To --
   --------------------

   procedure Attach_Next_To
      (Self    : not null access Gtk_Grid_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Sibling : access Gtk.Widget.Gtk_Widget_Record'Class;
       Side    : Gtk.Enums.Gtk_Position_Type;
       Width   : Glib.Gint := 1;
       Height  : Glib.Gint := 1)
   is
      procedure Internal
         (Self    : System.Address;
          Child   : System.Address;
          Sibling : System.Address;
          Side    : Gtk.Enums.Gtk_Position_Type;
          Width   : Glib.Gint;
          Height  : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_attach_next_to");
   begin
      Internal (Get_Object (Self), Get_Object (Child), Get_Object_Or_Null (GObject (Sibling)), Side, Width, Height);
   end Attach_Next_To;

   ----------------------
   -- Get_Baseline_Row --
   ----------------------

   function Get_Baseline_Row
      (Self : not null access Gtk_Grid_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_get_baseline_row");
   begin
      return Internal (Get_Object (Self));
   end Get_Baseline_Row;

   ------------------
   -- Get_Child_At --
   ------------------

   function Get_Child_At
      (Self : not null access Gtk_Grid_Record;
       Left : Glib.Gint;
       Top  : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self : System.Address;
          Left : Glib.Gint;
          Top  : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_grid_get_child_at");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self), Left, Top), Stub_Gtk_Widget));
   end Get_Child_At;

   ----------------------------
   -- Get_Column_Homogeneous --
   ----------------------------

   function Get_Column_Homogeneous
      (Self : not null access Gtk_Grid_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_grid_get_column_homogeneous");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Column_Homogeneous;

   ------------------------
   -- Get_Column_Spacing --
   ------------------------

   function Get_Column_Spacing
      (Self : not null access Gtk_Grid_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_grid_get_column_spacing");
   begin
      return Internal (Get_Object (Self));
   end Get_Column_Spacing;

   -------------------------------
   -- Get_Row_Baseline_Position --
   -------------------------------

   function Get_Row_Baseline_Position
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position
   is
      function Internal
         (Self : System.Address;
          Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position;
      pragma Import (C, Internal, "gtk_grid_get_row_baseline_position");
   begin
      return Internal (Get_Object (Self), Row);
   end Get_Row_Baseline_Position;

   -------------------------
   -- Get_Row_Homogeneous --
   -------------------------

   function Get_Row_Homogeneous
      (Self : not null access Gtk_Grid_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_grid_get_row_homogeneous");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Row_Homogeneous;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
      (Self : not null access Gtk_Grid_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_grid_get_row_spacing");
   begin
      return Internal (Get_Object (Self));
   end Get_Row_Spacing;

   -------------------
   -- Insert_Column --
   -------------------

   procedure Insert_Column
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_insert_column");
   begin
      Internal (Get_Object (Self), Position);
   end Insert_Column;

   --------------------
   -- Insert_Next_To --
   --------------------

   procedure Insert_Next_To
      (Self    : not null access Gtk_Grid_Record;
       Sibling : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Side    : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Self    : System.Address;
          Sibling : System.Address;
          Side    : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_grid_insert_next_to");
   begin
      Internal (Get_Object (Self), Get_Object (Sibling), Side);
   end Insert_Next_To;

   ----------------
   -- Insert_Row --
   ----------------

   procedure Insert_Row
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_insert_row");
   begin
      Internal (Get_Object (Self), Position);
   end Insert_Row;

   -------------------
   -- Remove_Column --
   -------------------

   procedure Remove_Column
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_remove_column");
   begin
      Internal (Get_Object (Self), Position);
   end Remove_Column;

   ----------------
   -- Remove_Row --
   ----------------

   procedure Remove_Row
      (Self     : not null access Gtk_Grid_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_remove_row");
   begin
      Internal (Get_Object (Self), Position);
   end Remove_Row;

   ----------------------
   -- Set_Baseline_Row --
   ----------------------

   procedure Set_Baseline_Row
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Row : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_set_baseline_row");
   begin
      Internal (Get_Object (Self), Row);
   end Set_Baseline_Row;

   ----------------------------
   -- Set_Column_Homogeneous --
   ----------------------------

   procedure Set_Column_Homogeneous
      (Self        : not null access Gtk_Grid_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_grid_set_column_homogeneous");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Homogeneous));
   end Set_Column_Homogeneous;

   ------------------------
   -- Set_Column_Spacing --
   ------------------------

   procedure Set_Column_Spacing
      (Self    : not null access Gtk_Grid_Record;
       Spacing : Guint)
   is
      procedure Internal (Self : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_grid_set_column_spacing");
   begin
      Internal (Get_Object (Self), Spacing);
   end Set_Column_Spacing;

   -------------------------------
   -- Set_Row_Baseline_Position --
   -------------------------------

   procedure Set_Row_Baseline_Position
      (Self : not null access Gtk_Grid_Record;
       Row  : Glib.Gint;
       Pos  : Gtk.Enums.Gtk_Baseline_Position)
   is
      procedure Internal
         (Self : System.Address;
          Row  : Glib.Gint;
          Pos  : Gtk.Enums.Gtk_Baseline_Position);
      pragma Import (C, Internal, "gtk_grid_set_row_baseline_position");
   begin
      Internal (Get_Object (Self), Row, Pos);
   end Set_Row_Baseline_Position;

   -------------------------
   -- Set_Row_Homogeneous --
   -------------------------

   procedure Set_Row_Homogeneous
      (Self        : not null access Gtk_Grid_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_grid_set_row_homogeneous");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Homogeneous));
   end Set_Row_Homogeneous;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
      (Self    : not null access Gtk_Grid_Record;
       Spacing : Guint)
   is
      procedure Internal (Self : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_grid_set_row_spacing");
   begin
      Internal (Get_Object (Self), Spacing);
   end Set_Row_Spacing;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Grid_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Grid_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Grid;
