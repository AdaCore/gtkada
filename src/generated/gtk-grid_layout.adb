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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Grid_Layout is

   package Type_Conversion_Gtk_Grid_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Grid_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Grid_Layout);

   package Type_Conversion_Gtk_Grid_Layout_Child is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Grid_Layout_Child_Get_Type'Access, Gtk_Grid_Layout_Child_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Grid_Layout_Child);

   -------------------------
   -- Gtk_Grid_Layout_New --
   -------------------------

   function Gtk_Grid_Layout_New return Gtk_Grid_Layout is
      Grid : constant Gtk_Grid_Layout := new Gtk_Grid_Layout_Record;
   begin
      Gtk.Grid_Layout.Initialize (Grid);
      return Grid;
   end Gtk_Grid_Layout_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Grid : out Gtk_Grid_Layout) is
   begin
      Grid := new Gtk_Grid_Layout_Record;
      Gtk.Grid_Layout.Initialize (Grid);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Grid : not null access Gtk_Grid_Layout_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_grid_layout_new");
   begin
      if not Grid.Is_Created then
         Set_Object (Grid, Internal);
      end if;
   end Initialize;

   ----------------------
   -- Get_Baseline_Row --
   ----------------------

   function Get_Baseline_Row
      (Grid : not null access Gtk_Grid_Layout_Record) return Glib.Gint
   is
      function Internal (Grid : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_get_baseline_row");
   begin
      return Internal (Get_Object (Grid));
   end Get_Baseline_Row;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_column");
   begin
      return Internal (Get_Object (Child));
   end Get_Column;

   ----------------------------
   -- Get_Column_Homogeneous --
   ----------------------------

   function Get_Column_Homogeneous
      (Grid : not null access Gtk_Grid_Layout_Record) return Boolean
   is
      function Internal (Grid : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_grid_layout_get_column_homogeneous");
   begin
      return Internal (Get_Object (Grid)) /= 0;
   end Get_Column_Homogeneous;

   ------------------------
   -- Get_Column_Spacing --
   ------------------------

   function Get_Column_Spacing
      (Grid : not null access Gtk_Grid_Layout_Record) return Guint
   is
      function Internal (Grid : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_grid_layout_get_column_spacing");
   begin
      return Internal (Get_Object (Grid));
   end Get_Column_Spacing;

   ---------------------
   -- Get_Column_Span --
   ---------------------

   function Get_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_column_span");
   begin
      return Internal (Get_Object (Child));
   end Get_Column_Span;

   -------------
   -- Get_Row --
   -------------

   function Get_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_row");
   begin
      return Internal (Get_Object (Child));
   end Get_Row;

   -------------------------------
   -- Get_Row_Baseline_Position --
   -------------------------------

   function Get_Row_Baseline_Position
      (Grid : not null access Gtk_Grid_Layout_Record;
       Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position
   is
      function Internal
         (Grid : System.Address;
          Row  : Glib.Gint) return Gtk.Enums.Gtk_Baseline_Position;
      pragma Import (C, Internal, "gtk_grid_layout_get_row_baseline_position");
   begin
      return Internal (Get_Object (Grid), Row);
   end Get_Row_Baseline_Position;

   -------------------------
   -- Get_Row_Homogeneous --
   -------------------------

   function Get_Row_Homogeneous
      (Grid : not null access Gtk_Grid_Layout_Record) return Boolean
   is
      function Internal (Grid : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_grid_layout_get_row_homogeneous");
   begin
      return Internal (Get_Object (Grid)) /= 0;
   end Get_Row_Homogeneous;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
      (Grid : not null access Gtk_Grid_Layout_Record) return Guint
   is
      function Internal (Grid : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_grid_layout_get_row_spacing");
   begin
      return Internal (Get_Object (Grid));
   end Get_Row_Spacing;

   ------------------
   -- Get_Row_Span --
   ------------------

   function Get_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record)
       return Glib.Gint
   is
      function Internal (Child : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_grid_layout_child_get_row_span");
   begin
      return Internal (Get_Object (Child));
   end Get_Row_Span;

   ----------------------
   -- Set_Baseline_Row --
   ----------------------

   procedure Set_Baseline_Row
      (Grid : not null access Gtk_Grid_Layout_Record;
       Row  : Glib.Gint)
   is
      procedure Internal (Grid : System.Address; Row : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_set_baseline_row");
   begin
      Internal (Get_Object (Grid), Row);
   end Set_Baseline_Row;

   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column
      (Child  : not null access Gtk_Grid_Layout_Child_Record;
       Column : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Column : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_column");
   begin
      Internal (Get_Object (Child), Column);
   end Set_Column;

   ----------------------------
   -- Set_Column_Homogeneous --
   ----------------------------

   procedure Set_Column_Homogeneous
      (Grid        : not null access Gtk_Grid_Layout_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Grid        : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_grid_layout_set_column_homogeneous");
   begin
      Internal (Get_Object (Grid), Boolean'Pos (Homogeneous));
   end Set_Column_Homogeneous;

   ------------------------
   -- Set_Column_Spacing --
   ------------------------

   procedure Set_Column_Spacing
      (Grid    : not null access Gtk_Grid_Layout_Record;
       Spacing : Guint)
   is
      procedure Internal (Grid : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_grid_layout_set_column_spacing");
   begin
      Internal (Get_Object (Grid), Spacing);
   end Set_Column_Spacing;

   ---------------------
   -- Set_Column_Span --
   ---------------------

   procedure Set_Column_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Span : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_column_span");
   begin
      Internal (Get_Object (Child), Span);
   end Set_Column_Span;

   -------------
   -- Set_Row --
   -------------

   procedure Set_Row
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Row   : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Row : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_row");
   begin
      Internal (Get_Object (Child), Row);
   end Set_Row;

   -------------------------------
   -- Set_Row_Baseline_Position --
   -------------------------------

   procedure Set_Row_Baseline_Position
      (Grid : not null access Gtk_Grid_Layout_Record;
       Row  : Glib.Gint;
       Pos  : Gtk.Enums.Gtk_Baseline_Position)
   is
      procedure Internal
         (Grid : System.Address;
          Row  : Glib.Gint;
          Pos  : Gtk.Enums.Gtk_Baseline_Position);
      pragma Import (C, Internal, "gtk_grid_layout_set_row_baseline_position");
   begin
      Internal (Get_Object (Grid), Row, Pos);
   end Set_Row_Baseline_Position;

   -------------------------
   -- Set_Row_Homogeneous --
   -------------------------

   procedure Set_Row_Homogeneous
      (Grid        : not null access Gtk_Grid_Layout_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Grid        : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_grid_layout_set_row_homogeneous");
   begin
      Internal (Get_Object (Grid), Boolean'Pos (Homogeneous));
   end Set_Row_Homogeneous;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
      (Grid    : not null access Gtk_Grid_Layout_Record;
       Spacing : Guint)
   is
      procedure Internal (Grid : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_grid_layout_set_row_spacing");
   begin
      Internal (Get_Object (Grid), Spacing);
   end Set_Row_Spacing;

   ------------------
   -- Set_Row_Span --
   ------------------

   procedure Set_Row_Span
      (Child : not null access Gtk_Grid_Layout_Child_Record;
       Span  : Glib.Gint)
   is
      procedure Internal (Child : System.Address; Span : Glib.Gint);
      pragma Import (C, Internal, "gtk_grid_layout_child_set_row_span");
   begin
      Internal (Get_Object (Child), Span);
   end Set_Row_Span;

end Gtk.Grid_Layout;
