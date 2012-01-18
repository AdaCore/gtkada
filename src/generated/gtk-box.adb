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

package body Gtk.Box is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Box_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Box         : out Gtk_Box;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Gint)
   is
   begin
      Box := new Gtk_Box_Record;
      Gtk.Box.Initialize (Box, Orientation, Spacing);
   end Gtk_New;

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox
      (Box         : out Gtk_Hbox;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
   begin
      Box := new Gtk_Hbox_Record;
      Gtk.Box.Initialize_Hbox (Box, Homogeneous, Spacing);
   end Gtk_New_Hbox;

   ------------------
   -- Gtk_New_Vbox --
   ------------------

   procedure Gtk_New_Vbox
      (Box         : out Gtk_Vbox;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
   begin
      Box := new Gtk_Vbox_Record;
      Gtk.Box.Initialize_Vbox (Box, Homogeneous, Spacing);
   end Gtk_New_Vbox;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Box         : access Gtk_Box_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation;
       Spacing     : Gint)
   is
      function Internal
         (Orientation : Integer;
          Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_box_new");
   begin
      Set_Object (Box, Internal (Gtk.Enums.Gtk_Orientation'Pos (Orientation), Spacing));
   end Initialize;

   ---------------------
   -- Initialize_Hbox --
   ---------------------

   procedure Initialize_Hbox
      (Box         : access Gtk_Hbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
      function Internal
         (Homogeneous : Integer;
          Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_hbox_new");
   begin
      Set_Object (Box, Internal (Boolean'Pos (Homogeneous), Spacing));
   end Initialize_Hbox;

   ---------------------
   -- Initialize_Vbox --
   ---------------------

   procedure Initialize_Vbox
      (Box         : access Gtk_Vbox_Record'Class;
       Homogeneous : Boolean := False;
       Spacing     : Gint := 0)
   is
      function Internal
         (Homogeneous : Integer;
          Spacing     : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_vbox_new");
   begin
      Set_Object (Box, Internal (Boolean'Pos (Homogeneous), Spacing));
   end Initialize_Vbox;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Box : not null access Gtk_Box_Record;
       Num : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Box : System.Address;
          Num : Gint) return System.Address;
      pragma Import (C, Internal, "ada_box_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Box), Num), Stub_Gtk_Widget));
   end Get_Child;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous
      (Box : not null access Gtk_Box_Record) return Boolean
   is
      function Internal (Box : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_box_get_homogeneous");
   begin
      return Boolean'Val (Internal (Get_Object (Box)));
   end Get_Homogeneous;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing (Box : not null access Gtk_Box_Record) return Gint is
      function Internal (Box : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_box_get_spacing");
   begin
      return Internal (Get_Object (Box));
   end Get_Spacing;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (In_Box  : not null access Gtk_Box_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand  : Boolean := True;
       Fill    : Boolean := True;
       Padding : Guint := 0)
   is
      procedure Internal
         (In_Box  : System.Address;
          Child   : System.Address;
          Expand  : Integer;
          Fill    : Integer;
          Padding : Guint);
      pragma Import (C, Internal, "gtk_box_pack_end");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (In_Box  : not null access Gtk_Box_Record;
       Child   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand  : Boolean := True;
       Fill    : Boolean := True;
       Padding : Guint := 0)
   is
      procedure Internal
         (In_Box  : System.Address;
          Child   : System.Address;
          Expand  : Integer;
          Fill    : Integer;
          Padding : Guint);
      pragma Import (C, Internal, "gtk_box_pack_start");
   begin
      Internal (Get_Object (In_Box), Get_Object (Child), Boolean'Pos (Expand), Boolean'Pos (Fill), Padding);
   end Pack_Start;

   -------------------------
   -- Query_Child_Packing --
   -------------------------

   procedure Query_Child_Packing
      (Box       : not null access Gtk_Box_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : out Boolean;
       Fill      : out Boolean;
       Padding   : out Guint;
       Pack_Type : out Gtk.Enums.Gtk_Pack_Type)
   is
      procedure Internal
         (Box       : System.Address;
          Child     : System.Address;
          Expand    : out Integer;
          Fill      : out Integer;
          Padding   : out Guint;
          Pack_Type : out Integer);
      pragma Import (C, Internal, "gtk_box_query_child_packing");
      Tmp_Expand    : Integer;
      Tmp_Fill      : Integer;
      Tmp_Pack_Type : Integer;
   begin
      Internal (Get_Object (Box), Get_Object (Child), Tmp_Expand, Tmp_Fill, Padding, Tmp_Pack_Type);
      Expand := Boolean'Val (Tmp_Expand);
      Fill := Boolean'Val (Tmp_Fill);
      Pack_Type := Gtk.Enums.Gtk_Pack_Type'Val (Tmp_Pack_Type);
   end Query_Child_Packing;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
      (Box      : not null access Gtk_Box_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Gint)
   is
      procedure Internal
         (Box      : System.Address;
          Child    : System.Address;
          Position : Gint);
      pragma Import (C, Internal, "gtk_box_reorder_child");
   begin
      Internal (Get_Object (Box), Get_Object (Child), Position);
   end Reorder_Child;

   -----------------------
   -- Set_Child_Packing --
   -----------------------

   procedure Set_Child_Packing
      (Box       : not null access Gtk_Box_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Expand    : Boolean;
       Fill      : Boolean;
       Padding   : Guint;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type)
   is
      procedure Internal
         (Box       : System.Address;
          Child     : System.Address;
          Expand    : Integer;
          Fill      : Integer;
          Padding   : Guint;
          Pack_Type : Integer);
      pragma Import (C, Internal, "gtk_box_set_child_packing");
   begin
      Internal (Get_Object (Box), Get_Object (Child), Boolean'Pos (Expand), Boolean'Pos (Fill), Padding, Gtk.Enums.Gtk_Pack_Type'Pos (Pack_Type));
   end Set_Child_Packing;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
      (Box         : not null access Gtk_Box_Record;
       Homogeneous : Boolean)
   is
      procedure Internal (Box : System.Address; Homogeneous : Integer);
      pragma Import (C, Internal, "gtk_box_set_homogeneous");
   begin
      Internal (Get_Object (Box), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
      (Box     : not null access Gtk_Box_Record;
       Spacing : Gint)
   is
      procedure Internal (Box : System.Address; Spacing : Gint);
      pragma Import (C, Internal, "gtk_box_set_spacing");
   begin
      Internal (Get_Object (Box), Spacing);
   end Set_Spacing;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Box_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Gtk.Enums.Gtk_Orientation'Val (Internal (Get_Object (Self)));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal (Self : System.Address; Orientation : Integer);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Orientation'Pos (Orientation));
   end Set_Orientation;

end Gtk.Box;
