------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;               use Interfaces.C.Strings;
with System;
with Cairo;                              use Cairo;
with Glib.Properties.Creation;           use Glib.Properties.Creation;
with Glib.Values;                        use Glib.Values;
with Gdk;                                use Gdk;
with Gdk.Event;                          use Gdk.Event;
with Gdk.Window;                         use Gdk.Window;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Drawing_Area;                   use Gtk.Drawing_Area;
with Gtk.Scrollable;                     use Gtk.Scrollable;
with Gtk.Widget;                         use Gtk.Widget;
with Gtkada.Bindings;                    use Gtkada.Bindings;
with Gtkada.Canvas_View.Links;           use Gtkada.Canvas_View.Links;
with Gtkada.Canvas_View.Objects;         use Gtkada.Canvas_View.Objects;
with Gtkada.Handlers;                    use Gtkada.Handlers;
with Gtkada.Style;                       use Gtkada.Style;
with Gtkada.Types;                       use Gtkada.Types;

package body Gtkada.Canvas_View is

   Model_Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String (String (Signal_Item_Contents_Changed)),
      2 => New_String (String (Signal_Layout_Changed)));
   View_Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String (String (Signal_Viewport_Changed)));

   Model_Class_Record : Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;
   View_Class_Record : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   Debug_Show_Bounding_Boxes : constant Boolean := False;
   --  Set to True to visualize the bounding boxes of items

   H_Adj_Property    : constant Property_Id := 1;
   V_Adj_Property    : constant Property_Id := 2;
   H_Scroll_Property : constant Property_Id := 3;
   V_Scroll_Property : constant Property_Id := 4;
   --  The properties for the View

   View_Margin : constant View_Coordinate := 20.0;
   --  The number of pixels on each sides of the view. This avoids having
   --  items displays exactly next to the border of the view.

   function On_View_Draw
     (View : System.Address; Cr : Cairo_Context) return Gboolean;
   pragma Convention (C, On_View_Draw);
   --  default handler for "draw" on views.

   procedure On_Size_Allocate (View : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, On_Size_Allocate);
   --  default handler for "size_allocate" on views.

   function GValue_To_Abstract_Item (Value : GValue) return Abstract_Item;
   function Abstract_Item_To_Address (S : Abstract_Item) return System.Address;
   package Abstract_Item_Marshallers is new Object_Callback.Marshallers
     .Generic_Marshaller (Abstract_Item, GValue_To_Abstract_Item);
   procedure Abstract_Item_Emit
     is new Abstract_Item_Marshallers.Emit_By_Name_Generic
     (Abstract_Item_To_Address);
   --  support for the "item_contents_changed" signal

   procedure View_Class_Init (Self : GObject_Class);
   pragma Convention (C, View_Class_Init);
   --  Initialize the class record, in particular adding interfaces, for
   --  the view class.

   procedure On_View_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   procedure Refresh
     (Self : not null access Canvas_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      Area : Model_Rectangle := No_Rectangle);
   --  Redraw the canvas (clear area, set transformation matrix and call Draw)

   procedure On_Layout_Changed_For_View
     (View : not null access GObject_Record'Class);
   procedure On_Item_Contents_Changed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item);
   --  Handles the model events for the view.

   procedure On_Adj_Value_Changed
     (View : access Glib.Object.GObject_Record'Class);
   --  Called when one of the scrollbars has changed value.

   procedure View_Set_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec);
   procedure View_Get_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec);
   --  Handlers for gtk+ properties

   procedure Set_Adjustment_Values
     (Self : not null access Canvas_View_Record'Class);
   --  Update the values for both adjustments

   procedure Translate_And_Draw_Item
     (Self : not null access Abstract_Item_Record'Class;
      Cr   : Cairo.Cairo_Context);
   --  Translate the transformation matrix and draw the item

   -----------------------------
   -- GValue_To_Abstract_Item --
   -----------------------------

   function GValue_To_Abstract_Item (Value : GValue) return Abstract_Item is
      S : constant System.Address := Get_Address (Value);
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Abstract_Item);
   begin
      return Unchecked_Convert (S);
   end GValue_To_Abstract_Item;

   ------------------------------
   -- Abstract_Item_To_Address --
   ------------------------------

   function Abstract_Item_To_Address
     (S : Abstract_Item) return System.Address
   is
   begin
      if S = null then
         return System.Null_Address;
      else
         return S.all'Address;
      end if;
   end Abstract_Item_To_Address;

   -------------------
   -- Item_To_Model --
   -------------------

   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      Rect   : Item_Rectangle) return Model_Rectangle
   is
      Parent : Abstract_Item := Abstract_Item (Item);
      Pos    : Item_Point;
      Result : Model_Rectangle := (Rect.X, Rect.Y, Rect.Width, Rect.Height);

   begin
      while Parent /= null loop
         --  ??? Should take item rotation into account when we implement it.

         Pos := Parent.Position;
         Result.X := Result.X + Pos.X;
         Result.Y := Result.Y + Pos.Y;

         Parent := Parent.Parent;
      end loop;
      return Result;
   end Item_To_Model;

   -------------------
   -- Item_To_Model --
   -------------------

   function Item_To_Model
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Item_Point) return Model_Point
   is
      R : constant Model_Rectangle :=
        Item.Item_To_Model ((P.X, P.Y, 0.0, 0.0));
   begin
      return (R.X, R.Y);
   end Item_To_Model;

   -------------------
   -- Model_To_Item --
   -------------------

   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Point) return Item_Point
   is
      Parent : Abstract_Item := Abstract_Item (Item);
      Result : Item_Point := (P.X, P.Y);
      Pos    : Item_Point;
   begin
      while Parent /= null loop
         Pos := Parent.Position;
         Result.X := Result.X - Pos.X;
         Result.Y := Result.Y - Pos.Y;
         Parent := Parent.Parent;
      end loop;
      return Result;
   end Model_To_Item;

   ------------------------
   -- Model_Bounding_Box --
   ------------------------

   function Model_Bounding_Box
     (Self     : not null access Abstract_Item_Record'Class)
      return Model_Rectangle
   is
   begin
      return Self.Item_To_Model (Self.Bounding_Box);
   end Model_Bounding_Box;

   --------------------
   -- Model_Get_Type --
   --------------------

   function Model_Get_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => GType_Object,
         Signals      => Model_Signals,
         Class_Record => Model_Class_Record,
         Type_Name    => "GtkadaCanvasModel",
         Parameters   => (1 => (1 => GType_Pointer),
                          2 => (1 => GType_None)));
      return Model_Class_Record.The_Type;
   end Model_Get_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access Canvas_Model_Record'Class) is
   begin
      if not Self.Is_Created then
         G_New (Self, Model_Get_Type);
      end if;
   end Initialize;

   --------------------
   -- Layout_Changed --
   --------------------

   procedure Layout_Changed
     (Self : not null access Canvas_Model_Record'Class) is
   begin
      Object_Callback.Emit_By_Name (Self, Signal_Layout_Changed);
   end Layout_Changed;

   -----------------------
   -- On_Layout_Changed --
   -----------------------

   procedure On_Layout_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null) is
   begin
      if Slot = null then
         Object_Callback.Connect (Self, Signal_Layout_Changed, Call);
      else
         Object_Callback.Object_Connect
           (Self, Signal_Layout_Changed, Call, Slot);
      end if;
   end On_Layout_Changed;

   -----------------------------
   -- Item_Contents_Changed --
   -----------------------------

   procedure Item_Contents_Changed
     (Self   : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class)
   is
   begin
      Abstract_Item_Emit
        (Self, Signal_Item_Contents_Changed & ASCII.NUL,
         Abstract_Item (Item));
   end Item_Contents_Changed;

   --------------------------------
   -- On_Item_Contents_Changed --
   --------------------------------

   procedure On_Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
   is
   begin
      if Slot = null then
         Object_Callback.Connect
           (Self, Signal_Item_Contents_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         Object_Callback.Object_Connect
           (Self, Signal_Item_Contents_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Item_Contents_Changed;

   --------------------------------
   -- On_Layout_Changed_For_View --
   --------------------------------

   procedure On_Layout_Changed_For_View
     (View : not null access GObject_Record'Class)
   is
      Self  : constant Canvas_View := Canvas_View (View);
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);

      --  On_Adjustments_Set will be called anyway when Size_Allocate is called
      --  so no need to call it now if the size is unknown yet.

      if Alloc.Width > 1 then
         Set_Adjustment_Values (Self);

         Gdk.Window.Invalidate_Rect
           (Self.Get_Window,
            Rect                => Alloc,
            Invalidate_Children => True);
      end if;
   end On_Layout_Changed_For_View;

   -----------------------------------------
   -- On_Item_Contents_Changed_For_View --
   -----------------------------------------

   procedure On_Item_Contents_Changed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item)
   is
      Self : constant Canvas_View := Canvas_View (View);
   begin
      Gdk.Window.Invalidate_Rect
        (Self.Get_Window,
         Rect                => Self.Model_To_Window (Item.Model_Bounding_Box),
         Invalidate_Children => True);
   end On_Item_Contents_Changed_For_View;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self  : out Canvas_View;
      Model : not null access Canvas_Model_Record'Class) is
   begin
      Self := new Canvas_View_Record;
      Initialize (Self, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access Canvas_View_Record'Class;
      Model : not null access Canvas_Model_Record'Class)
   is
   begin
      G_New (Self, View_Get_Type);

      Self.Model := Canvas_Model (Model);
      Ref (Model);

      Self.Add_Events (Scroll_Mask or Smooth_Scroll_Mask or Touch_Mask);

      Self.On_Destroy (On_View_Destroy'Access);
      Model.On_Layout_Changed (On_Layout_Changed_For_View'Access, Self);
      Model.On_Item_Contents_Changed
        (On_Item_Contents_Changed_For_View'Access, Self);
   end Initialize;

   ---------------------
   -- View_Class_Init --
   ---------------------

   procedure View_Class_Init (Self : GObject_Class) is
   begin
      Set_Properties_Handlers
        (Self, View_Set_Property'Access, View_Get_Property'Access);

      Override_Property (Self, H_Adj_Property, "hadjustment");
      Override_Property (Self, V_Adj_Property, "vadjustment");
      Override_Property (Self, H_Scroll_Property, "hscroll-policy");
      Override_Property (Self, V_Scroll_Property, "vscroll-policy");

      Set_Default_Draw_Handler (Self, On_View_Draw'Access);
      Set_Default_Size_Allocate_Handler (Self, On_Size_Allocate'Access);
   end View_Class_Init;

   -------------------
   -- View_Get_Type --
   -------------------

   function View_Get_Type return Glib.GType is
      Info : access GInterface_Info;
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Drawing_Area.Get_Type,
         Signals      => View_Signals,
         Class_Record => View_Class_Record'Access,
         Type_Name    => "GtkadaCanvasView",
         Parameters   => (1 => (1 .. 0 => GType_None)),
         Class_Init   => View_Class_Init'Access)
      then
         Info := new GInterface_Info'
           (Interface_Init     => null,
            Interface_Finalize => null,
            Interface_Data     => System.Null_Address);
         Glib.Object.Add_Interface
           (View_Class_Record,
            Iface => Gtk.Scrollable.Get_Type,
            Info  => Info);
      end if;

      return View_Class_Record.The_Type;
   end View_Get_Type;

   --------------------------
   -- On_Adj_Value_Changed --
   --------------------------

   procedure On_Adj_Value_Changed
     (View : access Glib.Object.GObject_Record'Class)
   is
      Self : constant Canvas_View := Canvas_View (View);
      Pos  : constant Model_Point :=
        (X => Self.Hadj.Get_Value,
         Y => Self.Vadj.Get_Value);
   begin
      if Pos /= Self.Topleft then
         Self.Topleft := Pos;
         Self.Viewport_Changed;
         Queue_Draw (Self);
      end if;
   end On_Adj_Value_Changed;

   ---------------------------
   -- Set_Adjustment_Values --
   ---------------------------

   procedure Set_Adjustment_Values
     (Self : not null access Canvas_View_Record'Class)
   is
      Box   : Model_Rectangle;
      Area  : constant Model_Rectangle := Self.Get_Visible_Area;
      M     : Model_Coordinate;
   begin
      if Self.Model = null or else Area.Width <= 1.0 then
         --  Not allocated yet
         return;
      end if;

      Box := Self.Model.Bounding_Box;
      M := View_Margin * Self.Scale;

      if Self.Hadj /= null then
         Self.Hadj.Configure
           (Value          => Area.X,
            Lower          => Box.X - M,
            Upper          => Box.X + Box.Width + M,
            Step_Increment => 5.0,
            Page_Increment => 100.0,
            Page_Size      => Area.Width);
      end if;

      if Self.Vadj /= null then
         Self.Vadj.Configure
           (Value          => Area.Y,
            Lower          => Box.Y - M,
            Upper          => Box.Y + Box.Height + M,
            Step_Increment => 5.0,
            Page_Increment => 100.0,
            Page_Size      => Area.Height);
      end if;
   end Set_Adjustment_Values;

   -----------------------
   -- View_Set_Property --
   -----------------------

   procedure View_Set_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Property_Spec);
      Self : constant Canvas_View := Canvas_View (Object);
   begin
      case Prop_Id is
         when H_Adj_Property =>
            Self.Hadj := Gtk_Adjustment (Get_Object (Value));
            if Self.Hadj /= null then
               Set_Adjustment_Values (Self);
               Self.Hadj.On_Value_Changed (On_Adj_Value_Changed'Access, Self);
               Self.Queue_Draw;
            end if;

         when V_Adj_Property =>
            Self.Vadj := Gtk_Adjustment (Get_Object (Value));
            if Self.Vadj /= null then
               Set_Adjustment_Values (Self);
               Self.Vadj.On_Value_Changed (On_Adj_Value_Changed'Access, Self);
               Self.Queue_Draw;
            end if;

         when H_Scroll_Property =>
            null;

         when V_Scroll_Property =>
            null;

         when others =>
            null;
      end case;
   end View_Set_Property;

   -----------------------
   -- View_Get_Property --
   -----------------------

   procedure View_Get_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Property_Spec);
      Self : constant Canvas_View := Canvas_View (Object);
   begin
      case Prop_Id is
         when H_Adj_Property =>
            Set_Object (Value, Self.Hadj);

         when V_Adj_Property =>
            Set_Object (Value, Self.Vadj);

         when H_Scroll_Property =>
            Set_Enum (Value, Gtk_Policy_Type'Pos (Policy_Automatic));

         when V_Scroll_Property =>
            Set_Enum (Value, Gtk_Policy_Type'Pos (Policy_Automatic));

         when others =>
            null;
      end case;
   end View_Get_Property;

   ---------------------
   -- On_View_Destroy --
   ---------------------

   procedure On_View_Destroy (Self : access Gtk_Widget_Record'Class) is
      S : constant Canvas_View := Canvas_View (Self);
   begin
      if S.Model /= null then
         Unref (S.Model);
         S.Model := null;
      end if;
   end On_View_Destroy;

   ------------------
   -- On_View_Draw --
   ------------------

   function On_View_Draw
     (View : System.Address; Cr : Cairo_Context) return Gboolean
   is
      Self : constant Canvas_View := Canvas_View (Glib.Object.Convert (View));
      X1, Y1, X2, Y2 : Gdouble;
   begin
      Clip_Extents (Cr, X1, Y1, X2, Y2);

      if X2 < X1 or else Y2 < Y1 then
         Refresh (Self, Cr);
      else
         Refresh (Self, Cr, Self.View_To_Model ((X1, Y1, X2 - X1, Y2 - Y1)));
      end if;

      --  There are no children, so no need to chain up
      return 1;

   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end On_View_Draw;

   ----------------------
   -- On_Size_Allocate --
   ----------------------

   procedure On_Size_Allocate
     (View : System.Address; Alloc : Gtk_Allocation)
   is
      Self : constant Canvas_View := Canvas_View (Glib.Object.Convert (View));
      SAlloc : Gtk_Allocation := Alloc;
   begin
      --  For some reason, when we maximize the toplevel window in testgtk, or
      --  at least enlarge it horizontally, we are starting to see an alloc
      --  with X < 0 (likely related to the GtkPaned). The drawing area then
      --  moves the GdkWindow, which would introduce an extra ofset in the
      --  display (and influence the clipping done automatically by gtk+
      --  before it emits "draw"). So we prevent the automatic offseting done
      --  by GtkDrawingArea.

      SAlloc.X := 0;
      SAlloc.Y := 0;
      Inherited_Size_Allocate (View_Class_Record, Self, SAlloc);
      Set_Adjustment_Values (Self);
   end On_Size_Allocate;

   ----------------------
   -- Get_Visible_Area --
   ----------------------

   function Get_Visible_Area
     (Self : not null access Canvas_View_Record)
      return Model_Rectangle
   is
   begin
      return Self.View_To_Model
        ((0.0,
         0.0,
         Gdouble (Self.Get_Allocated_Width),
         Gdouble (Self.Get_Allocated_Height)));
   end Get_Visible_Area;

   -------------------
   -- Set_Transform --
   -------------------

   procedure Set_Transform
     (Self   : not null access Canvas_View_Record;
      Cr     : Cairo.Cairo_Context;
      Item : access Abstract_Item_Record'Class := null)
   is
      M       : aliased Cairo_Matrix;
      Model_P : Model_Point;
      P       : View_Point;
   begin
      M.Xx := Self.Scale;
      M.Xy := 0.0;
      M.Yx := 0.0;
      M.Yy := Self.Scale;

      if Item /= null then
         Model_P := Item.Item_To_Model ((0.0, 0.0));
      else
         Model_P := (0.0, 0.0);
      end if;

      P := Self.Model_To_View (Model_P);
      M.X0 := P.X;
      M.Y0 := P.Y;
      Set_Matrix (Cr, M'Access);
   end Set_Transform;

   -------------------
   -- View_To_Model --
   -------------------

   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : View_Rectangle) return Model_Rectangle
   is
   begin
      return (X      => Rect.X / Self.Scale + Self.Topleft.X,
              Y      => Rect.Y / Self.Scale + Self.Topleft.Y,
              Width  => Rect.Width / Self.Scale,
              Height => Rect.Height / Self.Scale);
   end View_To_Model;

   -------------------
   -- Model_To_View --
   -------------------

   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return View_Rectangle
   is
   begin
      return (X      => (Rect.X - Self.Topleft.X) * Self.Scale,
              Y      => (Rect.Y - Self.Topleft.Y) * Self.Scale,
              Width  => Rect.Width * Self.Scale,
              Height => Rect.Height * Self.Scale);
   end Model_To_View;

   -------------------
   -- Model_To_View --
   -------------------

   function Model_To_View
     (Self   : not null access Canvas_View_Record;
      P      : Model_Point) return View_Point
   is
   begin
      return (X => (P.X - Self.Topleft.X) * Self.Scale,
              Y => (P.Y - Self.Topleft.Y) * Self.Scale);
   end Model_To_View;

   ---------------------
   -- Model_To_Window --
   ---------------------

   function Model_To_Window
     (Self   : not null access Canvas_View_Record;
      Rect   : Model_Rectangle) return Window_Rectangle
   is
      View  : constant View_Rectangle := Self.Model_To_View (Rect);
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      return (X      => Gint (View.X) + Alloc.X,
              Y      => Gint (View.Y) + Alloc.Y,
              Width  => Gint (View.Width),
              Height => Gint (View.Height));
   end Model_To_Window;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Self      : not null access Canvas_View_Record;
      Scale     : Gdouble := 1.0;
      Center_On : Model_Point := No_Point)
   is
      Box : constant Model_Rectangle := Self.Get_Visible_Area;
      Tmp : Gdouble;
   begin
      Self.Scale := Scale;

      if Center_On /= No_Point then
         Self.Topleft := Center_On;
      else
         Tmp := Gdouble (Self.Get_Allocated_Width) / Scale;  --  model width
         Self.Topleft.X := Box.X + (Box.Width - Tmp) / 2.0;

         Tmp := Gdouble (Self.Get_Allocated_Height) / Scale;  --  model height
         Self.Topleft.Y := Box.Y + (Box.Height - Tmp) / 2.0;
      end if;

      Self.Viewport_Changed;
   end Set_Scale;

   ----------------------
   -- Viewport_Changed --
   ----------------------

   procedure Viewport_Changed
     (Self   : not null access Canvas_View_Record'Class)
   is
   begin
      Object_Callback.Emit_By_Name (Self, Signal_Viewport_Changed);
   end Viewport_Changed;

   -------------------------
   -- On_Viewport_Changed --
   -------------------------

   procedure On_Viewport_Changed
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
   is
   begin
      if Slot = null then
         Object_Callback.Connect (Self, Signal_Viewport_Changed, Call);
      else
         Object_Callback.Object_Connect
           (Self, Signal_Viewport_Changed, Call, Slot);
      end if;
   end On_Viewport_Changed;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self : not null access Canvas_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      Area : Model_Rectangle := No_Rectangle)
   is
      A : Model_Rectangle;
   begin
      if Area = No_Rectangle then
         A := Self.Get_Visible_Area;
      else
         A := Area;
      end if;

      --  GDK already clears the exposed area to the background color, so
      --  we do not need to clear ourselves.

      Self.Draw_Internal (Cr, A);
   end Refresh;

   -----------------------------
   -- Translate_And_Draw_Item --
   -----------------------------

   procedure Translate_And_Draw_Item
     (Self : not null access Abstract_Item_Record'Class;
      Cr   : Cairo.Cairo_Context)
   is
      Pos : Gtkada.Style.Point;
   begin
      Save (Cr);
      Pos := Self.Position;
      Translate (Cr, Pos.X, Pos.Y);
      Self.Draw (Cr);

      if Debug_Show_Bounding_Boxes then
         declare
            Box : constant Item_Rectangle := Self.Bounding_Box;
         begin
            Gtk_New (Stroke => (1.0, 0.0, 0.0, 0.8),
                     Dashes => (2.0, 2.0))
              .Draw_Rect (Cr, (Box.X, Box.Y), Box.Width, Box.Height);
         end;
      end if;

      Restore (Cr);
   exception
      when E : others =>
         Restore (Cr);
         Process_Exception (E);
   end Translate_And_Draw_Item;

   -------------------
   -- Draw_Internal --
   -------------------

   procedure Draw_Internal
     (Self : not null access Canvas_View_Record;
      Cr   : Cairo.Cairo_Context;
      Area : Model_Rectangle)
   is
      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class);
      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         Translate_And_Draw_Item (Item, Cr);
      end Draw_Item;
   begin
      Save (Cr);
      Self.Set_Transform (Cr);
      Self.Model.For_Each_Item (Draw_Item'Access, In_Area => Area);
      Restore (Cr);
   end Draw_Internal;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
     (From, To    : not null access Abstract_Item_Record'Class;
      Style       : Gtkada.Style.Drawing_Style;
      Routing     : Route_Style := Straight;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment)
      return Canvas_Link
   is
      L : constant Canvas_Link := new Canvas_Link_Record;
   begin
      Initialize (L, From, To, Style, Routing, Anchor_From, Anchor_To);
      return L;
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Link        : not null access Canvas_Link_Record'Class;
      From, To    : not null access Abstract_Item_Record'Class;
      Style       : Gtkada.Style.Drawing_Style;
      Routing     : Route_Style := Straight;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Anchor_To   : Anchor_Attachment := Middle_Attachment)
   is
   begin
      Link.From        := Abstract_Item (From);
      Link.To          := Abstract_Item (To);
      Link.Style       := Style;
      Link.Routing     := Routing;
      Link.Anchor_From := Anchor_From;
      Link.Anchor_To   := Anchor_To;
   end Initialize;

   --------------
   -- Position --
   --------------

   overriding function Position
     (Self : not null access Canvas_Link_Record)
      return Gtkada.Style.Point
   is
      pragma Unreferenced (Self);
   begin
      --  Since we'll be using model coordinates to draw the link
      return (0.0, 0.0);
   end Position;

   ------------------
   -- Bounding_Box --
   ------------------

   overriding function Bounding_Box
     (Self : not null access Canvas_Link_Record)
      return Item_Rectangle is
   begin
      return Self.Bounding_Box;
   end Bounding_Box;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : not null access Canvas_Link_Record;
      Cr   : Cairo.Cairo_Context)
   is
   begin
      Gtkada.Canvas_View.Links.Draw_Link (Self, Cr);
   end Draw;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self : not null access Canvas_Link_Record;
      P    : Item_Point) return Boolean
   is
      pragma Unreferenced (Self, P);
   begin
      return False;
   end Contains;

   ---------------
   -- Clip_Line --
   ---------------

   overriding function Clip_Line
     (Self   : not null access Canvas_Link_Record;
      P1, P2 : Item_Point) return Item_Point
   is
      pragma Unreferenced (Self, P2);
   begin
      return P1;
   end Clip_Line;

   -----------------------
   -- Link_Anchor_Point --
   -----------------------

   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Link_Record;
      Anchor : Anchor_Attachment)
      return Item_Point
   is
      pragma Unreferenced (Anchor);
      Index : Integer;
   begin
      if Self.Points = null then
         Self.Refresh_Layout;
      end if;

      --  We connect to the middle of the middle segment.
      --  ??? We could instead look for the closest segment or some other
      --  algorithm.

      Index := Self.Points'First + Self.Points'Length / 2 - 1;

      return ((Self.Points (Index).X + Self.Points (Index + 1).X) / 2.0,
              (Self.Points (Index).Y + Self.Points (Index + 1).Y) / 2.0);
   end Link_Anchor_Point;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Canvas_Link_Record)
   is
   begin
      Destroy (Abstract_Item_Record (Self.all)'Access);
      Unchecked_Free (Self.Points);
      Unchecked_Free (Self.Waypoints);
   end Destroy;

   -------------------
   -- Point_In_Rect --
   -------------------

   function Point_In_Rect
     (Rect  : Model_Rectangle;
      P     : Model_Point) return Boolean
   is
      X : constant Model_Coordinate := P.X - Rect.X;
      Y : constant Model_Coordinate := P.Y - Rect.Y;
   begin
      return 0.0 <= X and then X <= Rect.Width
        and then 0.0 <= Y and then Y <= Rect.Height;
   end Point_In_Rect;

   -------------------
   -- Point_In_Rect --
   -------------------

   function Point_In_Rect
     (Rect : Item_Rectangle; P : Item_Point) return Boolean
   is
      X : constant Item_Coordinate := P.X - Rect.X;
      Y : constant Item_Coordinate := P.Y - Rect.Y;
   begin
      return 0.0 <= X and then X <= Rect.Width
        and then 0.0 <= Y and then Y <= Rect.Height;
   end Point_In_Rect;

   --------------
   -- Position --
   --------------

   overriding function Position
     (Self : not null access Canvas_Item_Record) return Gtkada.Style.Point
   is
   begin
      return Self.Position;
   end Position;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self : not null access Canvas_Item_Record;
      P    : Item_Point) return Boolean
   is
      Box : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
   begin
      return Point_In_Rect (Box, P);
   end Contains;

   -----------------------
   -- Link_Anchor_Point --
   -----------------------

   overriding function Link_Anchor_Point
     (Self   : not null access Canvas_Item_Record;
      Anchor : Anchor_Attachment)
      return Item_Point
   is
   begin
      return Gtkada.Canvas_View.Objects.Link_Anchor_Point (Self, Anchor);
   end Link_Anchor_Point;

   ---------------
   -- Clip_Line --
   ---------------

   overriding function Clip_Line
     (Self   : not null access Canvas_Item_Record;
      P1, P2 : Item_Point) return Item_Point
   is
      Box    : constant Item_Rectangle  :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
      Deltax : constant Item_Coordinate := P2.X - P1.X;
      Deltay : constant Item_Coordinate := P2.Y - P1.Y;
      Offset : constant Item_Coordinate := P1.X * P2.Y - P2.X * P1.Y;
      Result : Item_Point;

   begin
      if Deltay /= 0.0 then
         if Deltay < 0.0 then
            Result.Y := Box.Y;
         else
            Result.Y := Box.Y + Box.Height;
         end if;

         Result.X := (Deltax * Result.Y + Offset) / Deltay;
         if Box.X <= Result.X and then Result.X <= Box.X + Box.Width then
            return Result;
         end if;
      end if;

      if Deltax /= 0.0 then
         if Deltax < 0.0 then
            Result.X := Box.X;
         else
            Result.X := Box.X + Box.Width;
         end if;

         Result.Y := (Deltay * Result.X - Offset) / Deltax;

         if Box.Y <= Result.Y and then Result.Y <= Box.Y + Box.Height then
            return Result;
         end if;
      end if;

      return (Box.X, Box.Y);
   end Clip_Line;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Self  : not null access Canvas_Item_Record;
      Pos   : Gtkada.Style.Point) is
   begin
      Self.Position := Pos;
   end Set_Position;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out List_Canvas_Model) is
   begin
      Self := new List_Canvas_Model_Record;
      Gtkada.Canvas_View.Initialize (Self);
   end Gtk_New;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
   begin
      Self.Items.Append (Abstract_Item (Item));
      Self.Layout_Changed;
   end Add;

   ----------------
   -- Intersects --
   ----------------

   function Intersects (Rect1, Rect2 : Model_Rectangle) return Boolean is
   begin
      return not
        (Rect1.X > Rect2.X + Rect2.Width            --  R1 on the right of R2
         or else Rect2.X > Rect1.X + Rect1.Width    --  R2 on the right of R1
         or else Rect1.Y > Rect2.Y + Rect2.Height   --  R1 below R2
         or else Rect2.Y > Rect1.Y + Rect1.Height); --  R1 above R2
   end Intersects;

   -------------------
   -- For_Each_Item --
   -------------------

   overriding procedure For_Each_Item
     (Self     : not null access List_Canvas_Model_Record;
      Callback : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      In_Area  : Model_Rectangle := No_Rectangle)
   is
      use Items_Lists;
      C    : Items_Lists.Cursor := Self.Items.First;
      Item : Abstract_Item;
   begin
      while Has_Element (C) loop
         Item := Element (C);

         if In_Area = No_Rectangle
           or else Intersects (In_Area, Item.Model_Bounding_Box)
         then
            Callback (Item);
         end if;

         Next (C);
      end loop;
   end For_Each_Item;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout (Self : not null access Canvas_Link_Record) is
   begin
      case Self.Routing is
         when Orthogonal =>
            Compute_Layout_For_Orthogonal_Link (Self);
         when Straight =>
            Compute_Layout_For_Straight_Link (Self);
         when Curve =>
            Compute_Layout_For_Arc_Link (Self);
         when Orthocurve =>
            Compute_Layout_For_Orthocurve_Link (Self);
      end case;
   end Refresh_Layout;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout (Self : not null access Canvas_Model_Record) is
      procedure Do_Link_Layout
        (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Link_Layout
        (Item : not null access Abstract_Item_Record'Class)
      is
      begin
         if Item.all in Canvas_Link_Record'Class then
            Canvas_Link_Record'Class (Item.all).Refresh_Layout;
         end if;
      end Do_Link_Layout;

      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class)
      is
      begin
         if Item.all in Container_Item_Record'Class then
            Container_Item_Record'Class (Item.all).Refresh_Layout;
         end if;
      end Do_Container_Layout;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Container_Layout'Access);
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Link_Layout'Access);

      Self.Layout_Changed;
   end Refresh_Layout;

   -----------
   -- Union --
   -----------

   procedure Union
     (Rect1 : in out Model_Rectangle;
      Rect2 : Model_Rectangle)
   is
      Right : constant Model_Coordinate :=
        Model_Coordinate'Max (Rect1.X + Rect1.Width, Rect2.X + Rect2.Width);
      Bottom : constant Model_Coordinate :=
        Model_Coordinate'Max (Rect1.Y + Rect1.Height, Rect2.Y + Rect2.Height);
   begin
      Rect1.X := Model_Coordinate'Min (Rect1.X, Rect2.X);
      Rect1.Width := Right - Rect1.X;

      Rect1.Y := Model_Coordinate'Min (Rect1.Y, Rect2.Y);
      Rect1.Height := Bottom - Rect1.Y;
   end Union;

   ------------------
   -- Bounding_Box --
   ------------------

   function Bounding_Box
     (Self : not null access Canvas_Model_Record)
      return Model_Rectangle
   is
      Result : Model_Rectangle;
      Is_First : Boolean := True;

      procedure Do_Item (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Item (Item : not null access Abstract_Item_Record'Class) is
         Box : constant Model_Rectangle := Item.Model_Bounding_Box;
      begin
         if Is_First then
            Is_First := False;
            Result := Box;
         else
            Union (Result, Box);
         end if;
      end Do_Item;
   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Item'Access);

      if Is_First then
         return No_Rectangle;
      else
         return Result;
      end if;
   end Bounding_Box;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Self     : not null access Container_Item_Record'Class;
      Child    : not null access Container_Item_Record'Class;
      Align    : Alignment_Style := Align_Start;
      Margin   : Margins := No_Margins;
      Float    : Float_Style := Float_None;
      Overflow : Overflow_Style := Overflow_Prevent)
   is
   begin
      Child.Margin   := Margin;
      Child.Parent   := Container_Item (Self);
      Child.Float    := Float;
      Child.Overflow := Overflow;
      Child.Align    := Align;
      Self.Children.Append (Abstract_Item (Child));
   end Add_Child;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : not null access Container_Item_Record)
      return Abstract_Item
   is
   begin
      return Abstract_Item (Self.Parent);
   end Parent;

   ------------------
   -- Bounding_Box --
   ------------------

   function Bounding_Box
     (Self : not null access Container_Item_Record)
      return Item_Rectangle is
   begin
      --  assumes size_request has been called already
      return (0.0,
              0.0,
              Self.Width,
              Self.Height);
   end Bounding_Box;

   --------------------
   -- For_Each_Child --
   --------------------

   procedure For_Each_Child
     (Self     : not null access Container_Item_Record'Class;
      Callback : not null access procedure
        (Child : not null access Container_Item_Record'Class))
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Children.First;
   begin
      while Has_Element (C) loop
         Callback (Container_Item (Element (C)));
         Next (C);
      end loop;
   end For_Each_Child;

   ----------------------
   -- Set_Child_Layout --
   ----------------------

   procedure Set_Child_Layout
     (Self   : not null access Container_Item_Record'Class;
      Layout : Child_Layout_Strategy)
   is
   begin
      Self.Layout := Layout;
   end Set_Child_Layout;

   --------------
   -- Position --
   --------------

   overriding function Position
     (Self : not null access Container_Item_Record) return Gtkada.Style.Point
   is
   begin
      return Self.Computed_Position;
   end Position;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Self  : not null access Container_Item_Record)
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
      Child : Container_Item;
      Tmp, Tmp2 : Model_Coordinate;
   begin
      Self.Width  := 0.0;
      Self.Height := 0.0;

      Tmp := 0.0; --  Current coordinate (X or Y) for the child

      while Has_Element (C) loop
         Child := Container_Item (Element (C));

         Child.Computed_Position := (0.0, 0.0);
         Child.Size_Request;

         case Self.Layout is
            when Vertical_Stack =>
               case Child.Overflow is
                  when Overflow_Prevent =>
                     Self.Width := Model_Coordinate'Max
                       (Child.Width + Child.Margin.Left + Child.Margin.Right,
                        Self.Width);
                  when Overflow_Hide =>
                     null;
               end case;

               if Child.Position.Y /= Gdouble'First then
                  Tmp2 := Child.Position.Y + Child.Height;
               else
                  Tmp2 := Tmp + Child.Height;
               end if;

               Tmp2 := Tmp2 + Child.Computed_Position.Y
                 + Child.Margin.Top + Child.Margin.Bottom;

               Self.Height := Model_Coordinate'Max (Self.Height, Tmp2);

               if Child.Float = Float_None then
                  --  The lowest point so far
                  Tmp := Self.Height;
               end if;

            when Horizontal_Stack =>
               case Child.Overflow is
                  when Overflow_Prevent =>
                     Self.Height := Model_Coordinate'Max
                       (Child.Height + Child.Margin.Top + Child.Margin.Bottom,
                        Self.Height);
                  when Overflow_Hide =>
                     null;
               end case;

               if Child.Position.X /= Gdouble'First then
                  Tmp2 := Child.Position.X + Child.Width;
               else
                  Tmp2 := Tmp + Child.Width;
               end if;

               Tmp2 := Tmp2 + Child.Computed_Position.X
                 + Child.Margin.Left + Child.Margin.Right;

               Self.Width := Model_Coordinate'Max (Self.Width, Tmp2);

               if Child.Float = Float_None then
                  Tmp := Self.Width;
               end if;
         end case;

         Next (C);
      end loop;

      --  Ignore the previous computation when a size is forced. It was
      --  still needed to make sure all children have a size.

      if Self.Forced_Width > 0.0 then
         Self.Width := Self.Forced_Width;
      end if;

      if Self.Forced_Height > 0.0 then
         Self.Height := Self.Forced_Height;
      end if;

      --  Ensure a minimal size so that the object is visible.
      Self.Width := Model_Coordinate'Max (Self.Width, 1.0);
      Self.Height := Model_Coordinate'Max (Self.Height, 1.0);
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Self  : not null access Container_Item_Record)
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
      Child : Container_Item;
      Tmp   : Model_Coordinate := 0.0;
   begin
      while Has_Element (C) loop
         Child := Container_Item (Element (C));

         case Self.Layout is
            when Vertical_Stack =>
               if Child.Position.Y /= Gdouble'First then
                  Child.Computed_Position.Y := Child.Position.Y;
               else
                  Child.Computed_Position.Y := Tmp + Child.Margin.Top;
               end if;

               if Child.Position.X /= Gdouble'First then
                  Child.Computed_Position.X := Child.Position.X;
               else
                  Child.Computed_Position.X := Child.Margin.Left;
               end if;

               case Child.Float is
                  when Float_None =>
                     if Child.Forced_Width <= 0.0 then
                        Child.Width := Self.Width
                          - Child.Margin.Right - Child.Margin.Left;
                     end if;

                     case Child.Align is
                        when Align_Start =>
                           null;

                        when Align_Center =>
                           Child.Computed_Position.X :=
                             Child.Computed_Position.X +
                               (Self.Width - Child.Computed_Position.X
                                - Child.Width - Child.Margin.Right) / 2.0;

                        when Align_End =>
                           Child.Computed_Position.X :=
                             Self.Width - Child.Width - Child.Margin.Right;
                     end case;

                  when Float_Start =>
                     null; --  use the requested size and position

                  when Float_End =>
                     --  We ignore the X position imposed by the user, if any
                     Child.Computed_Position.X :=
                       Self.Width - Child.Width - Child.Margin.Right;
               end case;

               Child.Size_Allocate;

               if Child.Float = Float_None then
                  Tmp := Tmp + Child.Height + Child.Margin.Bottom;
               end if;

            when Horizontal_Stack =>
               if Child.Position.X /= Gdouble'First then
                  Child.Computed_Position.X := Child.Position.X;
               else
                  Child.Computed_Position.X := Tmp + Child.Margin.Left;
               end if;

               if Child.Position.Y /= Gdouble'First then
                  Child.Computed_Position.Y := Child.Position.Y;
               else
                  Child.Computed_Position.Y := Child.Margin.Top;
               end if;

               case Child.Float is
                  when Float_None =>
                     if Child.Forced_Height <= 0.0 then
                        Child.Height := Self.Height
                          - Child.Margin.Top - Child.Margin.Bottom;
                     end if;

                     case Child.Align is
                        when Align_Start =>
                           null;

                        when Align_Center =>
                           Child.Computed_Position.Y :=
                             Child.Computed_Position.Y +
                               (Self.Height - Child.Computed_Position.Y
                                - Child.Height - Child.Margin.Bottom) / 2.0;

                        when Align_End =>
                           Child.Computed_Position.Y :=
                             Self.Height - Child.Height - Child.Margin.Bottom;
                     end case;

                  when Float_Start =>
                     null; --  use the requested size and position

                  when Float_End =>
                     Child.Computed_Position.Y :=
                       Self.Height - Child.Height - Child.Margin.Bottom;
               end case;

               Child.Size_Allocate;

               if Child.Float = Float_None then
                  Tmp := Tmp + Child.Width + Child.Margin.Right;
               end if;
         end case;

         Next (C);
      end loop;
   end Size_Allocate;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout (Self : not null access Container_Item_Record) is
   begin
      Self.Computed_Position := Self.Position;
      Container_Item_Record'Class (Self.all).Size_Request;
      Container_Item_Record'Class (Self.all).Size_Allocate;
   end Refresh_Layout;

   -------------------
   -- Draw_Children --
   -------------------

   procedure Draw_Children
     (Self : not null access Container_Item_Record'Class;
      Cr   : Cairo.Cairo_Context)
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
   begin
      while Has_Element (C) loop
         Translate_And_Draw_Item (Element (C), Cr);
         Next (C);
      end loop;
   end Draw_Children;

   ------------------
   -- Gtk_New_Rect --
   ------------------

   function Gtk_New_Rect
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := -1.0;
      Radius        : Model_Coordinate := 0.0)
      return Rect_Item
   is
      R : constant Rect_Item := new Rect_Item_Record;
   begin
      R.Style         := Style;
      R.Forced_Width  := Width;
      R.Forced_Height := Height;
      R.Radius        := Radius;
      return R;
   end Gtk_New_Rect;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : not null access Rect_Item_Record;
      Cr   : Cairo.Cairo_Context) is
   begin
      Self.Style.Draw_Rect
        (Cr, (0.0, 0.0), Self.Width, Self.Height,
         Radius => Self.Radius);
      Self.Draw_Children (Cr);
   end Draw;

   ----------------------
   -- Gtk_New_Polyline --
   ----------------------

   function Gtk_New_Polyline
     (Style    : Gtkada.Style.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False)
      return Polyline_Item
   is
      R   : constant Polyline_Item := new Polyline_Item_Record;
   begin
      R.Style := Style;
      R.Close  := Close;
      R.Points := new Item_Point_Array'(Points);
      return R;
   end Gtk_New_Polyline;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self  : not null access Polyline_Item_Record)
   is
      B : constant Item_Rectangle := Compute_Bounding_Box (Self.Points.all);
   begin
      Self.Width := B.Width;
      Self.Height := B.Height;
      Self.Computed_Position.X := B.X;
      Self.Computed_Position.Y := B.Y;
   end Size_Request;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self : not null access Polyline_Item_Record;
      Cr   : Cairo.Cairo_Context)
   is
   begin
      Self.Style.Draw_Polyline (Cr, Self.Points.all, Close => Self.Close);
      Draw_Children (Self, Cr);
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Polyline_Item_Record)
   is
   begin
      Unchecked_Free (Self.Points);
      Container_Item_Record (Self.all).Destroy;  --  inherited
   end Destroy;


end Gtkada.Canvas_View;
