with Ada.Command_Line;
with System;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Glib;                  use Glib;
with Glib.Error;            use Glib.Error;
with Glib.List_Model;       use Glib.List_Model;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Glib.Types;
with GNAT.Strings;
with Gtk.Box;               use Gtk.Box;
with Gtk.Builder;           use Gtk.Builder;
with Gtk.Button;            use Gtk.Button;
with Gtk.Constraint;        use Gtk.Constraint;
with Gtk.Constraint_Guide;  use Gtk.Constraint_Guide;
with Gtk.Constraint_Layout; use Gtk.Constraint_Layout;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Main;
with Gtk.Window;            use Gtk.Window;
with Gtk.Widget;            use Gtk.Widget;

procedure Constraints is
   --  Native allocation is an independent oracle for the solver geometry.
   procedure Get_Allocation
     (Widget : System.Address; Rect : out Gdk_Rectangle);
   pragma Import (C, Get_Allocation, "gtk_widget_get_allocation");
   Required : constant Gint :=
     Gtk_Constraint_Strength'Enum_Rep (Constraint_Strength_Required);
   procedure Release (Model : Glist_Model) is
   begin
      Glib.Types.To_Object (Glib.Types.GType_Interface (Model)).Unref;
   end Release;
   procedure Setup
     (Window : out Gtk_Window; Box : out Gtk_Box;
      Layout : out Gtk_Constraint_Layout; A, B : out Gtk_Button)
   is
   begin
      Gtk_New (Box, Orientation_Horizontal, 0);
      Box.Ref_Sink;
      Gtk_New (Layout);
      Box.Set_Layout_Manager (Layout);
      Gtk_New (A, "A");
      Gtk_New (B, "B");
      Box.Append (A);
      Box.Append (B);
      Gtk_New (Window);
      Window.Set_Child (Box);
   end Setup;

   procedure Allocate (Layout : Gtk_Constraint_Layout; Box : Gtk_Box) is
      Minimum, Natural_Size, Min_Base, Nat_Base : Gint;
   begin
      Layout.Measure
        (Box, Orientation_Horizontal, -1, Minimum, Natural_Size, Min_Base,
         Nat_Base);
      Layout.Measure
        (Box, Orientation_Vertical, 400, Minimum, Natural_Size, Min_Base,
         Nat_Base);
      Layout.Allocate (Box, 400, 200, -1);
   end Allocate;

   procedure Test_Objects with
     Convention => C;
   procedure Test_Objects is
      Window              : Gtk_Window;
      Box                 : Gtk_Box;
      Layout              : Gtk_Constraint_Layout;
      A, B                : Gtk_Button;
      C                   : Gtk_Constraint;
      Guide               : Gtk_Constraint_Guide;
      Constraints, Guides : Glist_Model;
      Item                : GObject;
      Width, Height       : Gint;
   begin
      Setup (Window, Box, Layout, A, B);
      Constraints := Layout.Observe_Constraints;
      Guides      := Layout.Observe_Guides;
      Assert_True (Get_N_Items (Constraints) = 0);
      Assert_True (Get_N_Items (Guides) = 0);
      Gtk_New (Guide);
      Guide.Ref; -- Keep a reference across remove_guide.
      Guide.Set_Name ("spacer");
      Guide.Set_Min_Size (8, 4);
      Guide.Set_Nat_Size (24, 12);
      Guide.Set_Max_Size (80, 40);
      Guide.Set_Strength (Constraint_Strength_Strong);
      Guide.Get_Min_Size (Width, Height);
      Assert_True (Width = 8 and Height = 4);
      Guide.Get_Nat_Size (Width, Height);
      Assert_True (Width = 24 and Height = 12);
      Guide.Get_Max_Size (Width, Height);
      Assert_True (Width = 80 and Height = 40);
      Assert_True (Guide.Get_Name = "spacer");
      Assert_True (Guide.Get_Strength = Constraint_Strength_Strong);
      Layout.Add_Guide (Guide);
      Assert_True (Get_N_Items (Guides) = 1);
      Item := Get_Item (Guides, 0);
      Assert_True (Item = GObject (Guide));
      Item.Unref;
      C :=
        Gtk_Constraint_New
          (+A, Constraint_Attribute_Width, Constraint_Relation_Eq, +Guide,
           Constraint_Attribute_Width, 2.0, 5.5, Required);
      C.Ref;
      Assert_False (C.Is_Attached);
      Assert_False (C.Is_Constant);
      Assert_True (C.Is_Required);
      Assert_True (C.Get_Target = +A and C.Get_Source = +Guide);
      Assert_True (C.Get_Target_Attribute = Constraint_Attribute_Width);
      Assert_True (C.Get_Source_Attribute = Constraint_Attribute_Width);
      Assert_True (C.Get_Relation = Constraint_Relation_Eq);
      Assert_Cmpfloat_Eq (C.Get_Multiplier, 2.0);
      Assert_Cmpfloat_Eq (C.Get_Constant, 5.5);
      Assert_True (C.Get_Strength = Required);
      Layout.Add_Constraint (C);
      Assert_True (C.Is_Attached);
      Assert_True (Get_N_Items (Constraints) = 1);
      Layout.Remove_Constraint (C);
      Assert_False (C.Is_Attached);
      Assert_True (Get_N_Items (Constraints) = 0);
      C.Unref;
      C :=
        Gtk_Constraint_New_Constant
          (+A, Constraint_Attribute_Height, Constraint_Relation_Ge, 45.0,
           Required);
      Assert_True (C.Is_Constant);
      Layout.Add_Constraint (C);
      Layout.Remove_All_Constraints;
      Assert_True (Get_N_Items (Constraints) = 0);
      Layout.Remove_Guide (Guide);
      Assert_True (Get_N_Items (Guides) = 0);
      Guide.Get_Nat_Size (Width, Height);
      Assert_True (Width = 24 and Height = 12);
      Guide.Unref;
      Release (Constraints);
      Release (Guides);
      Window.Destroy;
      Box.Unref;
   end Test_Objects;

   procedure Test_VFL with
     Convention => C;
   procedure Test_VFL is
      Window           : Gtk_Window;
      Box              : Gtk_Box;
      Layout           : Gtk_Constraint_Layout;
      A, B             : Gtk_Button;
      --  Non-default bounds exercise both array marshalling paths.
      L1               : aliased String := "H:|-[a(==b)]-[b]-|";
      L2               : aliased String := "V:|-[a]-|";
      L3               : aliased String := "V:|-[b]-|";
      Lines            : constant GNAT.Strings.String_List (4 .. 6) :=
        (L1'Unchecked_Access, L2'Unchecked_Access, L3'Unchecked_Access);
      Views            : Named_View_Array (7 .. 8);
      Error            : GError;
      List, Cursor     : Object_Simple_List.Glist;
      Model            : Glist_Model;
      Alloc_A, Alloc_B : Gdk_Rectangle;
   begin
      Setup (Window, Box, Layout, A, B);
      Views :=
        ((To_Unbounded_String ("a"), +A), (To_Unbounded_String ("b"), +B));
      List  :=
        Layout.Add_Constraints_From_Description
          (Lines, Views, Error, Hspacing => 12, Vspacing => 8);
      Assert_True (Error = null);
      Assert_True (Object_Simple_List.Length (List) = 8);
      Allocate (Layout, Box);
      Get_Allocation (Get_Object (A), Alloc_A);
      Get_Allocation (Get_Object (B), Alloc_B);
      Assert_True (Alloc_A.X = 12 and Alloc_A.Y = 8);
      Assert_True (Alloc_A.Width = 182 and Alloc_A.Height = 184);
      Assert_True (Alloc_B.X = 206 and Alloc_B.Y = 8);
      Assert_True (Alloc_B.Width = 182 and Alloc_B.Height = 184);
      Model := Layout.Observe_Constraints;
      Assert_True (Get_N_Items (Model) = 8);
      Cursor := List;
      while Object_Simple_List.Length (Cursor) > 0 loop
         Layout.Remove_Constraint
           (Gtk_Constraint (Object_Simple_List.Get_Data (Cursor)));
         Cursor := Object_Simple_List.Next (Cursor);
      end loop;
      Object_Simple_List.Free (List);
      Assert_True (Get_N_Items (Model) = 0);
      declare
         Invalid : aliased String := "H:|-[missing]-|";
      begin
         List :=
           Layout.Add_Constraints_From_Description
             ((1 => Invalid'Unchecked_Access), Views, Error);
         Assert_True (Error /= null);
         Assert_True (Get_Message (Error)'Length > 0);
         Error_Free (Error);
         Object_Simple_List.Free (List);
      end;
      declare
         Fresh       : Gtk_Constraint_Layout;
         Invalid     : aliased String := "H:|-[missing]-|";
         Empty_Lines : GNAT.Strings.String_List (1 .. 0);
         Empty_Views : Named_View_Array (1 .. 0);
      begin
         --  GTK adds valid preceding lines even if a later line is invalid.
         --  A disposable layout makes validation transactional for callers.
         Gtk_New (Fresh);
         List :=
           Fresh.Add_Constraints_From_Description
             ((L1'Unchecked_Access, Invalid'Unchecked_Access), Views, Error);
         Assert_True (Error /= null);
         Assert_True (Object_Simple_List.Length (List) = 4);
         Assert_True (Get_N_Items (Model) = 0);
         Error_Free (Error);
         Object_Simple_List.Free (List);
         Fresh.Unref;
         List :=
           Layout.Add_Constraints_From_Description
             (Empty_Lines, Empty_Views, Error);
         Assert_True (Error = null);
         Assert_True (Object_Simple_List.Length (List) = 0);
         Object_Simple_List.Free (List);
      end;
      Release (Model);
      Window.Destroy;
      Box.Unref;
   end Test_VFL;

   procedure Test_Guide_Layout with
     Convention => C;
   procedure Test_Guide_Layout is
      Window           : Gtk_Window;
      Box              : Gtk_Box;
      Layout           : Gtk_Constraint_Layout;
      A, B             : Gtk_Button;
      Guide            : Gtk_Constraint_Guide;
      Error            : GError;
      List             : Object_Simple_List.Glist;
      Alloc_A, Alloc_B : Gdk_Rectangle;
      H                : aliased String := "H:|[a(==b)][gap][b]|";
      VA               : aliased String := "V:|[a]|";
      VB               : aliased String := "V:|[b]|";
   begin
      Setup (Window, Box, Layout, A, B);
      Gtk_New (Guide);
      Guide.Set_Min_Size (24, 0);
      Guide.Set_Nat_Size (24, 0);
      Guide.Set_Max_Size (24, 0);
      Guide.Set_Strength (Constraint_Strength_Strong);
      Layout.Add_Guide (Guide);
      List :=
        Layout.Add_Constraints_From_Description
          ((H'Unchecked_Access, VA'Unchecked_Access, VB'Unchecked_Access),
           ((To_Unbounded_String ("a"), +A), (To_Unbounded_String ("b"), +B),
            (To_Unbounded_String ("gap"), +Guide)),
           Error);
      Assert_True (Error = null);
      Object_Simple_List.Free (List);
      Allocate (Layout, Box);
      Get_Allocation (Get_Object (A), Alloc_A);
      Get_Allocation (Get_Object (B), Alloc_B);
      Assert_True (Alloc_A.Width = 188 and Alloc_B.X = 212);
      Guide.Set_Max_Size (80, -1);
      Guide.Set_Min_Size (80, -1);
      Guide.Set_Nat_Size (80, -1);
      Allocate (Layout, Box);
      Get_Allocation (Get_Object (A), Alloc_A);
      Get_Allocation (Get_Object (B), Alloc_B);
      Assert_True (Alloc_A.Width = 160 and Alloc_B.X = 240);
      Window.Destroy;
      Box.Unref;
   end Test_Guide_Layout;

   procedure Test_Builder with
     Convention => C;
   procedure Test_Builder is
      Builder             : Gtk_Builder;
      Error               : GError;
      Window              : Gtk_Window;
      Box                 : Gtk_Box;
      Layout              : Gtk_Constraint_Layout;
      Constraints, Guides : Glist_Model;
      A                   : Gtk_Button;
      Alloc               : Gdk_Rectangle;
      XML                 : constant String :=
        "<interface><object class='GtkBox' id='box'>" &
        "<property name='layout-manager'><object class='GtkConstraintLayout'>" &
        "<constraints><guide name='gap' min-width='12' nat-width='12' max-width='12'/>" &
        "<constraint relation='eq' strength='required' target='a' target-attribute='start' source='super' source-attribute='start' constant='16'/>" &
        "<constraint relation='eq' strength='required' target='a' target-attribute='top' source='super' source-attribute='top' constant='8'/>" &
        "<constraint relation='eq' strength='required' target='a' target-attribute='width' constant='100'/>" &
        "<constraint relation='eq' strength='required' target='a' target-attribute='height' constant='60'/>" &
        "</constraints></object></property>" &
        "<child><object class='GtkButton' id='a'><property name='label'>Builder</property></object></child>" &
        "</object></interface>";
   begin
      Gtk_New (Builder);
      Assert_True (Builder.Add_From_String (XML, Error));
      if Error /= null then
         raise Program_Error;
      end if;
      Box := Gtk_Box (Builder.Get_Object ("box"));
      Box.Ref;
      A := Gtk_Button (Builder.Get_Object ("a"));
      Builder.Unref;
      Gtk_New (Window);
      Window.Set_Child (Box);
      Layout      := Gtk_Constraint_Layout (Box.Get_Layout_Manager);
      Constraints := Layout.Observe_Constraints;
      Guides      := Layout.Observe_Guides;
      Assert_True (Get_N_Items (Constraints) = 4);
      Assert_True (Get_N_Items (Guides) = 1);
      Allocate (Layout, Box);
      Get_Allocation (Get_Object (A), Alloc);
      Assert_True (Alloc.X = 16 and Alloc.Y = 8);
      Assert_True (Alloc.Width = 100 and Alloc.Height = 60);
      Release (Constraints);
      Release (Guides);
      Window.Destroy;
      Box.Unref;
   end Test_Builder;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/constraints/objects", Test_Objects'Unrestricted_Access);
   Add_Func ("/constraints/vfl", Test_VFL'Unrestricted_Access);
   Add_Func
     ("/constraints/guide-layout", Test_Guide_Layout'Unrestricted_Access);
   Add_Func ("/constraints/builder", Test_Builder'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Constraints;
