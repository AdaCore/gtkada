--  Exercise popup-layout construction, properties, copying, conversion, and
--  reference management. There is no C popup-layout test to port; these
--  checks use only the boxed layout and need no displayed popup.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Gdk.Enums;
with Gdk.Popup_Layout;
with Gdk.Rectangle;
with Glib;
with Glib.Test;

procedure Main is
   use type Gdk.Enums.Gdk_Anchor_Hints;
   use type Gdk.Enums.Gdk_Gravity;
   use type Glib.Gint;
   use type Glib.GType;

   procedure Test_Layout_Properties with Convention => C;

   --  Both constructor forms create equivalent descriptions, and each
   --  getter reflects its corresponding setter on this in-process value.
   procedure Test_Layout_Properties is
      Initial_Rect : constant Gdk.Rectangle.Gdk_Rectangle :=
        (X => 10, Y => 20, Width => 30, Height => 40);
      Changed_Rect : aliased Gdk.Rectangle.Gdk_Rectangle :=
        (X => 50, Y => 60, Width => 70, Height => 80);
      By_Function : Gdk.Popup_Layout.Gdk_Popup_Layout :=
        Gdk.Popup_Layout.Gdk_Popup_Layout_New
          (Initial_Rect,
           Gdk.Enums.Gdk_Gravity_North_West,
           Gdk.Enums.Gdk_Gravity_South_East);
      By_Procedure : Gdk.Popup_Layout.Gdk_Popup_Layout;
      Copied : Gdk.Popup_Layout.Gdk_Popup_Layout;
      Shared_Ref : Gdk.Popup_Layout.Gdk_Popup_Layout;
      Hints : constant Gdk.Enums.Gdk_Anchor_Hints :=
        Gdk.Enums.Gdk_Anchor_Flip_X or Gdk.Enums.Gdk_Anchor_Resize_Y;
      Dx : Glib.Gint;
      Dy : Glib.Gint;
      Left : Glib.Gint;
      Right : Glib.Gint;
      Top : Glib.Gint;
      Bottom : Glib.Gint;
   begin
      --  The null value is the documented empty wrapper; the boxed GType is
      --  registered independently of opening a display.
      Glib.Test.Assert_True
        (Glib.Is_Null (Gdk.Popup_Layout.Null_Gdk_Popup_Layout));
      Glib.Test.Assert_True
        (Gdk.Popup_Layout.Get_Type /= Glib.GType_Invalid);

      Gdk.Popup_Layout.Gdk_New
        (By_Procedure,
         Initial_Rect,
         Gdk.Enums.Gdk_Gravity_North_West,
         Gdk.Enums.Gdk_Gravity_South_East);
      Glib.Test.Assert_True
        (Gdk.Popup_Layout.Equal (By_Function, By_Procedure));

      --  Set and read every mutable property, including the GTK-specific
      --  rectangle accessors and the four independent shadow margins.
      Gdk.Popup_Layout.Set_Anchor_Rect
        (By_Function, Changed_Rect'Access);
      declare
         Got_Rect : constant access constant Gdk.Rectangle.Gdk_Rectangle :=
           Gdk.Popup_Layout.Get_Anchor_Rect (By_Function);
      begin
         Glib.Test.Assert_Cmpint_Eq (Got_Rect.X, Changed_Rect.X);
         Glib.Test.Assert_Cmpint_Eq (Got_Rect.Y, Changed_Rect.Y);
         Glib.Test.Assert_Cmpint_Eq (Got_Rect.Width, Changed_Rect.Width);
         Glib.Test.Assert_Cmpint_Eq (Got_Rect.Height, Changed_Rect.Height);
      end;

      Gdk.Popup_Layout.Set_Rect_Anchor
        (By_Function, Gdk.Enums.Gdk_Gravity_North);
      Gdk.Popup_Layout.Set_Surface_Anchor
        (By_Function, Gdk.Enums.Gdk_Gravity_South);
      Glib.Test.Assert_True
        (Gdk.Popup_Layout.Get_Rect_Anchor (By_Function) =
           Gdk.Enums.Gdk_Gravity_North);
      Glib.Test.Assert_True
        (Gdk.Popup_Layout.Get_Surface_Anchor (By_Function) =
           Gdk.Enums.Gdk_Gravity_South);

      Gdk.Popup_Layout.Set_Anchor_Hints (By_Function, Hints);
      Glib.Test.Assert_True
        (Gdk.Popup_Layout.Get_Anchor_Hints (By_Function) = Hints);
      Gdk.Popup_Layout.Set_Offset (By_Function, -3, 7);
      Gdk.Popup_Layout.Get_Offset (By_Function, Dx, Dy);
      Glib.Test.Assert_Cmpint_Eq (Dx, -3);
      Glib.Test.Assert_Cmpint_Eq (Dy, 7);
      Gdk.Popup_Layout.Set_Shadow_Width (By_Function, 1, 2, 3, 4);
      Gdk.Popup_Layout.Get_Shadow_Width
        (By_Function, Left, Right, Top, Bottom);
      Glib.Test.Assert_Cmpint_Eq (Left, 1);
      Glib.Test.Assert_Cmpint_Eq (Right, 2);
      Glib.Test.Assert_Cmpint_Eq (Top, 3);
      Glib.Test.Assert_Cmpint_Eq (Bottom, 4);
      Glib.Test.Assert_False
        (Gdk.Popup_Layout.Equal (By_Function, By_Procedure));

      --  Copy preserves the properties, and changing its offset does not
      --  mutate the original boxed layout.
      Copied := Gdk.Popup_Layout.Copy (By_Function);
      Glib.Test.Assert_True (Gdk.Popup_Layout.Equal (By_Function, Copied));
      Gdk.Popup_Layout.Set_Offset (Copied, 8, 9);
      Glib.Test.Assert_False (Gdk.Popup_Layout.Equal (By_Function, Copied));
      Gdk.Popup_Layout.Get_Offset (By_Function, Dx, Dy);
      Glib.Test.Assert_Cmpint_Eq (Dx, -3);
      Glib.Test.Assert_Cmpint_Eq (Dy, 7);

      --  Ref returns a second owned reference to the same logical value.
      Shared_Ref := Gdk.Popup_Layout.Ref (By_Function);
      Glib.Test.Assert_True
        (Gdk.Popup_Layout.Equal (By_Function, Shared_Ref));

      --  From_Object is a borrowed pointer conversion. Verify it observes the
      --  same value; the original reference remains responsible for unref.
      declare
         Borrowed : constant Gdk.Popup_Layout.Gdk_Popup_Layout :=
           Gdk.Popup_Layout.From_Object (Glib.Get_Object (By_Function));
      begin
         Glib.Test.Assert_True
           (Gdk.Popup_Layout.Equal (By_Function, Borrowed));
      end;

      --  From_Object_Free consumes a GLib-allocated Ada wrapper, not the
      --  boxed layout itself, so release the native value exactly once below.
      declare
         type Layout_Access is access all
           Gdk.Popup_Layout.Gdk_Popup_Layout;
         function To_Layout_Access is new Ada.Unchecked_Conversion
           (System.Address, Layout_Access);
         function G_Malloc (Size : Glib.Gsize) return System.Address;
         pragma Import (C, G_Malloc, "g_malloc");
         Wrapper_Bytes : constant Glib.Gsize :=
           Glib.Gsize
             ((Gdk.Popup_Layout.Gdk_Popup_Layout'Size
               + System.Storage_Unit - 1) / System.Storage_Unit);
         Wrapper : constant Layout_Access :=
           To_Layout_Access (G_Malloc (Wrapper_Bytes));
         Converted : Gdk.Popup_Layout.Gdk_Popup_Layout;
      begin
         Wrapper.all := By_Function;
         Converted := Gdk.Popup_Layout.From_Object_Free (Wrapper);
         Glib.Test.Assert_True
           (Gdk.Popup_Layout.Equal (By_Function, Converted));
      end;

      Gdk.Popup_Layout.Unref (Shared_Ref);
      Gdk.Popup_Layout.Unref (Copied);
      Gdk.Popup_Layout.Unref (By_Procedure);
      Gdk.Popup_Layout.Unref (By_Function);
   end Test_Layout_Properties;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gdk-popup-layout/properties",
      Test_Layout_Properties'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
