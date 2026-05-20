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

package body Gtk.Box_Layout is

   package Type_Conversion_Gtk_Box_Layout is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Box_Layout_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Box_Layout);

   ------------------------
   -- Gtk_Box_Layout_New --
   ------------------------

   function Gtk_Box_Layout_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Box_Layout
   is
      Box_Layout : constant Gtk_Box_Layout := new Gtk_Box_Layout_Record;
   begin
      Gtk.Box_Layout.Initialize (Box_Layout, Orientation);
      return Box_Layout;
   end Gtk_Box_Layout_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Box_Layout  : out Gtk_Box_Layout;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
   begin
      Box_Layout := new Gtk_Box_Layout_Record;
      Gtk.Box_Layout.Initialize (Box_Layout, Orientation);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Box_Layout  : not null access Gtk_Box_Layout_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_box_layout_new");
   begin
      if not Box_Layout.Is_Created then
         Set_Object (Box_Layout, Internal (Orientation));
      end if;
   end Initialize;

   ------------------------
   -- Get_Baseline_Child --
   ------------------------

   function Get_Baseline_Child
      (Box_Layout : not null access Gtk_Box_Layout_Record) return Glib.Gint
   is
      function Internal (Box_Layout : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_box_layout_get_baseline_child");
   begin
      return Internal (Get_Object (Box_Layout));
   end Get_Baseline_Child;

   ---------------------------
   -- Get_Baseline_Position --
   ---------------------------

   function Get_Baseline_Position
      (Box_Layout : not null access Gtk_Box_Layout_Record)
       return Gtk.Enums.Gtk_Baseline_Position
   is
      function Internal
         (Box_Layout : System.Address)
          return Gtk.Enums.Gtk_Baseline_Position;
      pragma Import (C, Internal, "gtk_box_layout_get_baseline_position");
   begin
      return Internal (Get_Object (Box_Layout));
   end Get_Baseline_Position;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous
      (Box_Layout : not null access Gtk_Box_Layout_Record) return Boolean
   is
      function Internal (Box_Layout : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_box_layout_get_homogeneous");
   begin
      return Internal (Get_Object (Box_Layout)) /= 0;
   end Get_Homogeneous;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
      (Box_Layout : not null access Gtk_Box_Layout_Record) return Guint
   is
      function Internal (Box_Layout : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_box_layout_get_spacing");
   begin
      return Internal (Get_Object (Box_Layout));
   end Get_Spacing;

   ------------------------
   -- Set_Baseline_Child --
   ------------------------

   procedure Set_Baseline_Child
      (Box_Layout : not null access Gtk_Box_Layout_Record;
       Child      : Glib.Gint)
   is
      procedure Internal (Box_Layout : System.Address; Child : Glib.Gint);
      pragma Import (C, Internal, "gtk_box_layout_set_baseline_child");
   begin
      Internal (Get_Object (Box_Layout), Child);
   end Set_Baseline_Child;

   ---------------------------
   -- Set_Baseline_Position --
   ---------------------------

   procedure Set_Baseline_Position
      (Box_Layout : not null access Gtk_Box_Layout_Record;
       Position   : Gtk.Enums.Gtk_Baseline_Position)
   is
      procedure Internal
         (Box_Layout : System.Address;
          Position   : Gtk.Enums.Gtk_Baseline_Position);
      pragma Import (C, Internal, "gtk_box_layout_set_baseline_position");
   begin
      Internal (Get_Object (Box_Layout), Position);
   end Set_Baseline_Position;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
      (Box_Layout  : not null access Gtk_Box_Layout_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Box_Layout  : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_box_layout_set_homogeneous");
   begin
      Internal (Get_Object (Box_Layout), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Box_Layout : not null access Gtk_Box_Layout_Record;
       Spacing    : Guint)
   is
      procedure Internal (Box_Layout : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_box_layout_set_spacing");
   begin
      Internal (Get_Object (Box_Layout), Spacing);
   end Set_Spacing;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Box_Layout_Record)
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
      (Self        : not null access Gtk_Box_Layout_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Box_Layout;
