------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

--  This demo shows how to write a custom widget entirely in Ada by deriving
--  from Gtk_Widget_Record and overriding the GtkWidgetClass virtual methods
--  exposed by Gtk.Widget: measure, size_allocate and snapshot. The "gizmo"
--  below deliberately advertises a minimum size that differs from its natural
--  size, records the size it is allocated, and paints itself a solid colour
--  so the override mechanism is plainly visible.

with Ada.Text_IO;  use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with System;

with Glib;         use Glib;
with Glib.Object;  use Glib.Object;
with Gdk.RGBA;     use Gdk.RGBA;
with Gtk.Box;      use Gtk.Box;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.Label;    use Gtk.Label;
with Gtk.Widget;   use Gtk.Widget;

package body Create_Custom_Widget is

   ------------------------
   -- The gizmo widget   --
   ------------------------

   type Gizmo_Record is new Gtk_Widget_Record with record
      Min_Width, Min_Height : Gint;
      Nat_Width, Nat_Height : Gint;
      Alloc_Width           : Gint := 0;
      Alloc_Height          : Gint := 0;
      Info                  : Gtk_Label;
      --  An external label the gizmo updates to report its last allocation.
   end record;
   type Gizmo is access all Gizmo_Record'Class;

   Gizmo_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   procedure Gtk_New (Self : out Gizmo; Info : Gtk_Label);
   procedure Initialize (Self : not null access Gizmo_Record'Class);
   function Get_Type return Glib.GType;

   --  The class virtual-method handlers. They receive the widget as the raw
   --  C pointer and recover the Ada object through Glib.Object.Get_User_Data.

   procedure Class_Init (Self : GObject_Class);
   pragma Convention (C, Class_Init);

   procedure Gizmo_Measure
     (Widget                             : System.Address;
      Orientation                        : Gtk_Orientation;
      For_Size                           : Gint;
      Min, Nat                           : out Gint;
      Minimum_Baseline, Natural_Baseline : out Gint);
   pragma Convention (C, Gizmo_Measure);

   procedure Gizmo_Size_Allocate
     (Widget : System.Address; Width, Height, Baseline : Gint);
   pragma Convention (C, Gizmo_Size_Allocate);

   procedure Gizmo_Snapshot
     (Widget : System.Address; Snapshot : System.Address);
   pragma Convention (C, Gizmo_Snapshot);

   --  graphene_rect_t and the GtkSnapshot drawing entry point are not part of
   --  the generated binding yet, so we declare just enough here to paint a
   --  solid rectangle.

   type Graphene_Rect is record
      X, Y, Width, Height : Interfaces.C.C_float;
   end record;
   pragma Convention (C, Graphene_Rect);

   procedure Snapshot_Append_Color
     (Snapshot : System.Address;
      Color    : System.Address;
      Bounds   : System.Address);
   pragma Import (C, Snapshot_Append_Color, "gtk_snapshot_append_color");

   -------------------
   -- Gizmo_Measure --
   -------------------

   procedure Gizmo_Measure
     (Widget                             : System.Address;
      Orientation                        : Gtk_Orientation;
      For_Size                           : Gint;
      Min, Nat                           : out Gint;
      Minimum_Baseline, Natural_Baseline : out Gint)
   is
      pragma Unreferenced (For_Size);
      Stub : Gizmo_Record;
      Self : constant Gizmo := Gizmo (Get_User_Data (Widget, Stub));
   begin
      if Orientation = Orientation_Horizontal then
         Min := Self.Min_Width;
         Nat := Self.Nat_Width;
      else
         Min := Self.Min_Height;
         Nat := Self.Nat_Height;
      end if;
      Minimum_Baseline := -1;
      Natural_Baseline := -1;
   end Gizmo_Measure;

   -------------------------
   -- Gizmo_Size_Allocate --
   -------------------------

   procedure Gizmo_Size_Allocate
     (Widget : System.Address; Width, Height, Baseline : Gint)
   is
      Stub : Gizmo_Record;
      Self : constant Gizmo := Gizmo (Get_User_Data (Widget, Stub));
   begin
      Self.Alloc_Width  := Width;
      Self.Alloc_Height := Height;
      Self.Info.Set_Text
        ("The gizmo was allocated" & Width'Image & " x" & Height'Image
         & " pixels.");
      Put_Line
        ("Gizmo size_allocate:" & Width'Image & " x" & Height'Image);

      --  Chain to GtkWidget's own size_allocate so the base bookkeeping is
      --  still performed.
      Inherited_Size_Allocate (Gizmo_Class, Self, Width, Height, Baseline);
   end Gizmo_Size_Allocate;

   --------------------
   -- Gizmo_Snapshot --
   --------------------

   procedure Gizmo_Snapshot
     (Widget : System.Address; Snapshot : System.Address)
   is
      Stub   : Gizmo_Record;
      Self   : constant Gizmo := Gizmo (Get_User_Data (Widget, Stub));
      Color  : aliased Gdk_RGBA :=
        (Red => 0.20, Green => 0.50, Blue => 0.85, Alpha => 1.0);
      Bounds : aliased Graphene_Rect :=
        (X      => 0.0,
         Y      => 0.0,
         Width  => C_float (Self.Alloc_Width),
         Height => C_float (Self.Alloc_Height));
   begin
      Snapshot_Append_Color (Snapshot, Color'Address, Bounds'Address);
   end Gizmo_Snapshot;

   ----------------
   -- Class_Init --
   ----------------

   procedure Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Measure_Handler (Self, Gizmo_Measure'Access);
      Set_Default_Size_Allocate_Handler (Self, Gizmo_Size_Allocate'Access);
      Set_Default_Snapshot_Handler (Self, Gizmo_Snapshot'Access);
   end Class_Init;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Widget.Get_Type,
         Class_Record => Gizmo_Class,
         Type_Name    => "GtkAdaDemoGizmo",
         Class_Init   => Class_Init'Access);
      return Gizmo_Class.The_Type;
   end Get_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gizmo_Record'Class) is
   begin
      G_New (Self, Get_Type);
   end Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gizmo; Info : Gtk_Label) is
   begin
      Self := new Gizmo_Record;
      Self.Min_Width  := 60;
      Self.Min_Height := 40;
      Self.Nat_Width  := 240;
      Self.Nat_Height := 150;
      Self.Info       := Info;
      Create_Custom_Widget.Initialize (Self);
   end Gtk_New;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how to write a @bcustom widget@B in Ada for"
        & " Gtk4." & ASCII.LF
        & "The blue @bgizmo@B is a direct @bGtk_Widget@B subclass that"
        & " overrides three class virtual methods through the new"
        & " @bGtk.Widget@B handlers:" & ASCII.LF
        & " - @bmeasure@B advertises a minimum size (60 x 40) that is"
        & " deliberately smaller than its natural size (240 x 150);"
        & ASCII.LF
        & " - @bsize_allocate@B records the size the gizmo is given and"
        & " chains to the inherited implementation;" & ASCII.LF
        & " - @bsnapshot@B paints the gizmo a solid colour." & ASCII.LF
        & "Resize the window and watch the label below the gizmo report the"
        & " size it is allocated. None of this was previously possible in"
        & " Ada: no stock widget lets its minimum and natural sizes differ"
        & " and be read back.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box  : Gtk_Box;
      G    : Gizmo;
      Info : Gtk_Label;
   begin
      Gtk_New (Box, Orientation_Vertical, 10);
      Box.Set_Margin_Start (10);
      Box.Set_Margin_End (10);
      Box.Set_Margin_Top (10);
      Box.Set_Margin_Bottom (10);

      Gtk_New (Info, "The gizmo has not been allocated yet.");

      Gtk_New (G, Info);
      G.Set_Hexpand (True);
      G.Set_Vexpand (True);

      Box.Append (G);
      Box.Append (Info);

      Frame.Set_Child (Box);
   end Run;

end Create_Custom_Widget;
