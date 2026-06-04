with System;

with Glib.Object; use Glib.Object;
with Gtk.Enums;   use Gtk.Enums;

package body Gizmo is

   Gizmo_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   function Get_Type return Glib.GType;

   procedure Class_Init (Self : GObject_Class);
   pragma Convention (C, Class_Init);

   --  The class virtual-method handlers. They receive the widget as the raw
   --  C pointer and recover the Ada object through Glib.Object.Get_User_Data.

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
      Self : constant Gizmo_Widget := Gizmo_Widget (Get_User_Data (Widget, Stub));
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
      pragma Unreferenced (Baseline);
      Stub : Gizmo_Record;
      Self : constant Gizmo_Widget := Gizmo_Widget (Get_User_Data (Widget, Stub));
   begin
      --  Like the C gizmo, record the allocation and deliberately do *not*
      --  chain to GtkWidget's size_allocate.
      Self.Alloc_Width := Width;
      Self.Alloc_Height := Height;
   end Gizmo_Size_Allocate;

   ----------------
   -- Class_Init --
   ----------------

   procedure Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Measure_Handler (Self, Gizmo_Measure'Access);
      Set_Default_Size_Allocate_Handler (Self, Gizmo_Size_Allocate'Access);
   end Class_Init;

   --------------
   -- Get_Type --
   --------------

   function Get_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Widget.Get_Type,
         Class_Record => Gizmo_Class,
         Type_Name    => "GtkAdaTestGizmo",
         Class_Init   => Class_Init'Access);
      return Gizmo_Class.The_Type;
   end Get_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gizmo_Widget) is
   begin
      Self := new Gizmo_Record;
      G_New (Self, Get_Type);
   end Gtk_New;

   ---------------
   -- Set_Sizes --
   ---------------

   procedure Set_Sizes
     (Self                  : not null access Gizmo_Record'Class;
      Min_Width, Min_Height : Gint;
      Nat_Width, Nat_Height : Gint) is
   begin
      Self.Min_Width := Min_Width;
      Self.Min_Height := Min_Height;
      Self.Nat_Width := Nat_Width;
      Self.Nat_Height := Nat_Height;
   end Set_Sizes;

   ---------------------
   -- Allocated_Width --
   ---------------------

   function Allocated_Width
     (Self : not null access Gizmo_Record'Class) return Gint is
   begin
      return Self.Alloc_Width;
   end Allocated_Width;

   ----------------------
   -- Allocated_Height --
   ----------------------

   function Allocated_Height
     (Self : not null access Gizmo_Record'Class) return Gint is
   begin
      return Self.Alloc_Height;
   end Allocated_Height;

end Gizmo;
