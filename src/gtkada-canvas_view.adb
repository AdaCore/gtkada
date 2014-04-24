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
with GNAT.Strings;                       use GNAT.Strings;
with Interfaces.C.Strings;               use Interfaces.C.Strings;
with System;
with Cairo;                              use Cairo;
with Cairo.Matrix;                       use Cairo.Matrix;
with Cairo.Pattern;                      use Cairo.Pattern;
with Glib.Properties.Creation;           use Glib.Properties.Creation;
with Glib.Values;                        use Glib.Values;
with Gdk;                                use Gdk;
with Gdk.Cairo;                          use Gdk.Cairo;
with Gdk.RGBA;                           use Gdk.RGBA;
with Gdk.Window;                         use Gdk.Window;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Drawing_Area;                   use Gtk.Drawing_Area;
with Gtk.Handlers;                       use Gtk.Handlers;
with Gtk.Scrollable;                     use Gtk.Scrollable;
with Gtk.Widget;                         use Gtk.Widget;
with Gtkada.Bindings;                    use Gtkada.Bindings;
with Gtkada.Canvas_View.Links;           use Gtkada.Canvas_View.Links;
with Gtkada.Canvas_View.Objects;         use Gtkada.Canvas_View.Objects;
with Gtkada.Handlers;                    use Gtkada.Handlers;
with Gtkada.Types;                       use Gtkada.Types;

package body Gtkada.Canvas_View is

   Model_Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String (String (Signal_Item_Contents_Changed)),
      2 => New_String (String (Signal_Layout_Changed)));
   View_Signals : constant Gtkada.Types.Chars_Ptr_Array :=
     (1 => New_String (String (Signal_Viewport_Changed)),
      2 => New_String (String (Signal_Item_Event)));

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

   Mouse_Move_Before_Drag : constant Gdouble := 4.0 * 4.0;
   --  Minimal amount the mouse should move before we start dragging (this is
   --  the square).

   function On_View_Draw
     (View : System.Address; Cr : Cairo_Context) return Gboolean;
   pragma Convention (C, On_View_Draw);
   --  default handler for "draw" on views.

   procedure On_Size_Allocate (View : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, On_Size_Allocate);
   --  default handler for "size_allocate" on views.

   function GValue_To_Abstract_Item (Value : GValue) return Abstract_Item;
   function Abstract_Item_To_Address is new Ada.Unchecked_Conversion
     (Abstract_Item, System.Address);
   package Abstract_Item_Marshallers is new Object_Callback.Marshallers
     .Generic_Marshaller (Abstract_Item, GValue_To_Abstract_Item);
   procedure Abstract_Item_Emit
     is new Abstract_Item_Marshallers.Emit_By_Name_Generic
     (Abstract_Item_To_Address);
   --  support for the "item_contents_changed" signal

   function GValue_To_EDA (Value : GValue) return Event_Details_Access;
   function EDA_To_Address is new Ada.Unchecked_Conversion
     (Event_Details_Access, System.Address);
   package EDA_Marshallers is new Object_Return_Callback.Marshallers
     .Generic_Marshaller (Event_Details_Access, GValue_To_EDA);
   function EDA_Emit
     is new EDA_Marshallers.Emit_By_Name_Generic (EDA_To_Address);
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

   function Compute_Text
     (Self : not null access Text_Item_Record'Class)
      return String;
   --  Return the text to display for Self, including the directional arrow

   procedure Align_Text (Self : not null access Text_Item_Record);
   --  Adjust the computed position for the text to match the alignment
   --  requested by the user.

   procedure Resize_Fill_Pattern
     (Self : not null access Container_Item_Record'Class);
   --  Resize the fill pattern so that it extends to the whole item, instead of
   --  just the 0.0 .. 1.0 pattern space.

   procedure Free (Self : in out Abstract_Item);
   --  Free the memory used by Self

   function On_Button_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   function On_Motion_Notify_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;
   function On_Scroll_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Scroll) return Boolean;
   --  Low-level handling of mouse events.

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_And_Position_Lists.List :=
        Item_And_Position_Lists.Empty_List);
   --  Refresh the layout for all links (or only the ones linked to Item, or
   --  indirectly to a link to Item).

   function Intersection
     (P11, P12, P21, P22 : Item_Point) return Item_Point;
   --  Compute the intersection of the two segments (P11,P12) and (P21,P22).
   --  The result has X set to Gdouble'First when no intersection exists

   procedure Compute_Item
     (Self    : not null access Canvas_View_Record'Class;
      Details : in out Canvas_Event_Details);
   --  Compute the item that was clicked on, from the coordinates stored in
   --  Details.

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Abstract_Item) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Abstract_Item_Record'Class, Abstract_Item);
   begin
      if Self /= null then
         Destroy (Self);
         Unchecked_Free (Self);
      end if;
   end Free;

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

   -------------------
   -- GValue_To_EDA --
   -------------------

   function GValue_To_EDA (Value : GValue) return Event_Details_Access is
      S : constant System.Address := Get_Address (Value);
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Event_Details_Access);
   begin
      return Unchecked_Convert (S);
   end GValue_To_EDA;

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

         Pos := Position (Parent);
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
      Rect   : constant Item_Rectangle :=
        Model_To_Item (Item, (P.X, P.Y, 1.0, 1.0));
   begin
      return (Rect.X, Rect.Y);
   end Model_To_Item;

   -------------------
   -- Model_To_Item --
   -------------------

   function Model_To_Item
     (Item   : not null access Abstract_Item_Record'Class;
      P      : Model_Rectangle) return Item_Rectangle
   is
      Parent : Abstract_Item := Abstract_Item (Item);
      Result : Item_Rectangle := (P.X, P.Y, P.Width, P.Height);
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

      --  ??? When destroyed, should unreferenced Self.Layout
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

   function On_Layout_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
      Id : Handler_Id;
   begin
      if Slot = null then
         Id := Object_Callback.Connect
            (Self,
             Signal_Layout_Changed,
             Object_Callback.To_Marshaller (Call));
      else
         Id := Object_Callback.Object_Connect
            (Self,
             Signal_Layout_Changed,
             Object_Callback.To_Marshaller (Call),
             Slot);
      end if;
      return Id;
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

   function On_Item_Contents_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
      Id : Handler_Id;
   begin
      if Slot = null then
         Id := Object_Callback.Connect
           (Self, Signal_Item_Contents_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         Id := Object_Callback.Object_Connect
           (Self, Signal_Item_Contents_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
      return Id;
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
         Self.Queue_Draw;
      end if;
   end On_Layout_Changed_For_View;

   -----------------------------------------
   -- On_Item_Contents_Changed_For_View --
   -----------------------------------------

   procedure On_Item_Contents_Changed_For_View
     (View : access GObject_Record'Class;
      Item : Abstract_Item)
   is
      pragma Unreferenced (Item);
      Self : constant Canvas_View := Canvas_View (View);

      --  ??? Ideally we should only redraw the minimal area
--        Rect : constant View_Rectangle :=
--          Self.Model_To_View (Item.Model_Bounding_Box);
   begin
      Self.Queue_Draw;
--        Self.Queue_Draw_Area
--          (X      => Gint (Rect.X),
--           Y      => Gint (Rect.Y),
--           Width  => Gint (Rect.Width),
--           Height => Gint (Rect.Height));
   end On_Item_Contents_Changed_For_View;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self  : out Canvas_View;
      Model : access Canvas_Model_Record'Class := null) is
   begin
      Self := new Canvas_View_Record;
      Initialize (Self, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access Canvas_View_Record'Class;
      Model : access Canvas_Model_Record'Class := null)
   is
   begin
      G_New (Self, View_Get_Type);

      Self.Layout := Self.Create_Pango_Layout;

      Self.Add_Events
        (Scroll_Mask or Smooth_Scroll_Mask or Touch_Mask
         or Button_Press_Mask or Button_Release_Mask
         or Button1_Motion_Mask
         or Button2_Motion_Mask
         or Button3_Motion_Mask
         --  or Pointer_Motion_Mask or Pointer_Motion_Hint_Mask
        );

      Self.On_Destroy (On_View_Destroy'Access);
      Self.On_Button_Press_Event (On_Button_Event'Access);
      Self.On_Button_Release_Event (On_Button_Event'Access);
      Self.On_Motion_Notify_Event (On_Motion_Notify_Event'Access);
      Self.On_Scroll_Event (On_Scroll_Event'Access);

      Self.Set_Model (Model);
   end Initialize;

   ------------------
   -- Compute_Item --
   ------------------

   procedure Compute_Item
     (Self    : not null access Canvas_View_Record'Class;
      Details : in out Canvas_Event_Details)
   is
      Context : Draw_Context;
   begin
      Context := (Cr     => Gdk.Cairo.Create (Self.Get_Window),
                  Layout => null);

      Details.Toplevel_Item := Self.Model.Toplevel_Item_At
        (Details.M_Point, Context => Context);

      if Details.Toplevel_Item = null then
         Details.Item := null;
      else
         Details.T_Point := Details.Toplevel_Item.Model_To_Item
           (Details.M_Point);
         Details.Item := Details.Toplevel_Item.Inner_Most_Item
           (Details.M_Point, Context);

         if Details.Item /= null then
            Details.I_Point := Details.Item.Model_To_Item (Details.M_Point);
         end if;
      end if;

      Cairo.Destroy (Context.Cr);
   end Compute_Item;

   ---------------------
   -- On_Scroll_Event --
   ---------------------

   function On_Scroll_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Scroll) return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Button  : Guint;
   begin
      if Self.Model /= null then
         case Event.Direction is
            when Scroll_Up | Scroll_Left =>
               Button := 5;
            when Scroll_Down | Scroll_Right =>
               Button := 6;
            when Scroll_Smooth =>
               if Event.Delta_Y > 0.0 then
                  Button := 6;
               else
                  Button := 5;
               end if;
         end case;

         Details :=
           (Event_Type => Scroll,
            Button     => Button,
            State      => Event.State,
            Root_Point => (Event.X_Root, Event.Y_Root),
            M_Point    => Self.Window_To_Model ((X => Event.X, Y => Event.Y)),
            T_Point    => No_Item_Point,
            I_Point    => No_Item_Point,
            Item       => null,
            Toplevel_Item => null,
            Allowed_Drag_Area => No_Drag_Allowed);
         Compute_Item (Self, Details);
         return Self.Item_Event (Details'Unchecked_Access);
      end if;
      return False;
   end On_Scroll_Event;

   -----------------
   -- Set_Details --
   -----------------

   procedure Set_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : in out Canvas_Event_Details;
      Event   : Gdk.Event.Gdk_Event_Button)
   is
   begin
      Details :=
        (Event_Type => Button_Press,
         Button     => Event.Button,
         State      => Event.State,
         Root_Point => (Event.X_Root, Event.Y_Root),
         M_Point    => Self.Window_To_Model ((X => Event.X, Y => Event.Y)),
         T_Point    => No_Item_Point,
         I_Point    => No_Item_Point,
         Item       => null,
         Toplevel_Item => null,
         Allowed_Drag_Area => No_Drag_Allowed);

      case Event.The_Type is
         when Gdk.Event.Button_Press =>
            Compute_Item (Self, Details);

         when Gdk_2button_Press =>
            Details.Event_Type := Double_Click;
            Compute_Item (Self, Details);

         when Gdk.Event.Button_Release =>
            if Self.In_Drag then
               Details.Event_Type := End_Drag;
               Details.Toplevel_Item :=
                 Self.Last_Button_Press.Toplevel_Item;
            else
               Details.Event_Type := Button_Release;

               if Details.M_Point = Self.Last_Button_Press.M_Point then
                  --  Do not spend time recomputing
                  Details.Toplevel_Item :=
                    Self.Last_Button_Press.Toplevel_Item;
               else
                  Compute_Item (Self, Details);
               end if;
            end if;

         when others =>
            --  invalid
            Details.Event_Type := Key_Press;
      end case;
   end Set_Details;

   ---------------------
   -- On_Button_Event --
   ---------------------

   function On_Button_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button)
      return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
   begin
      if Self.Model /= null then
         Self.Set_Details (Details, Event);

         if Details.Event_Type = Key_Press then
            return False;
         end if;

         if Event.The_Type = Gdk.Event.Button_Release
           and then Self.In_Drag
         then
            --  Validate the position of items and recompute all links,
            --  not just the ones that moved.
            Self.Model.Refresh_Layout;
         end if;

         if Self.Item_Event (Details'Unchecked_Access) then
            if Details.Event_Type = Button_Press then
               Self.Last_Button_Press := Details;
            end if;

            Self.In_Drag := False;
            Self.Dragged_Items.Clear;
            return True;
         end if;
      end if;

      Self.In_Drag := False;
      Self.Dragged_Items.Clear;
      return False;
   end On_Button_Event;

   ----------------------------
   -- On_Motion_Notify_Event --
   ----------------------------

   function On_Motion_Notify_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      use Item_And_Position_Lists;
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Dx, Dy  : Gdouble;
      Area, B : Model_Rectangle;
      BB      : Item_Rectangle;
      X, Y    : Model_Coordinate;
      C       : Item_And_Position_Lists.Cursor;
      It      : Item_And_Position;
      Dummy   : Boolean;
   begin
      if Self.Model /= null
        and then Self.Last_Button_Press.Allowed_Drag_Area /= No_Drag_Allowed
      then
         if not Self.In_Drag then
            Dx := Event.X_Root - Self.Last_Button_Press.Root_Point.X;
            Dy := Event.Y_Root - Self.Last_Button_Press.Root_Point.Y;

            if Dx * Dx + Dy * Dy >= Mouse_Move_Before_Drag then
               Self.In_Drag := True;
               Details := Self.Last_Button_Press;
               Details.Event_Type := Start_Drag;
               Dummy := Self.Item_Event (Details'Unchecked_Access);

               Self.Topleft_At_Drag_Start := Self.Topleft;

               --  ??? Should add all selected items
               if Details.Toplevel_Item /= null then
                  Self.Dragged_Items.Append
                    ((Item => Details.Toplevel_Item,
                      Pos  => (X => Details.Toplevel_Item.Position.X,
                               Y => Details.Toplevel_Item.Position.Y)));
               end if;
            end if;
         end if;

         --  Whether we were already in a drag or just started

         if Self.In_Drag then
            Details := Self.Last_Button_Press;
            Details.Event_Type := In_Drag;
            Details.Root_Point := (Event.X_Root, Event.Y_Root);
            Details.M_Point :=
              Self.Window_To_Model ((X => Event.X, Y => Event.Y));
            Dummy := Self.Item_Event (Details'Unchecked_Access);

            if Self.Dragged_Items.Is_Empty then
               --  Compute the area that we are allowed to make visible. This
               --  is the combination of the allowed area and the currently
               --  visible one.

               X := Self.Topleft_At_Drag_Start.X
                 - (Details.Root_Point.X
                    - Self.Last_Button_Press.Root_Point.X) / Self.Scale;

               Y := Self.Topleft_At_Drag_Start.Y
                 - (Details.Root_Point.Y
                    - Self.Last_Button_Press.Root_Point.Y) / Self.Scale;

               if Self.Last_Button_Press.Allowed_Drag_Area /=
                 Drag_Anywhere
               then
                  Area := Self.Get_Visible_Area;
                  B    := Self.Last_Button_Press.Allowed_Drag_Area;
                  Union (B, Area);

                  X := Model_Coordinate'Max (X, B.X);
                  X := Model_Coordinate'Min (X, B.X + B.Width - Area.Width);
                  Y := Model_Coordinate'Max (Y, B.Y);
                  Y := Model_Coordinate'Min (Y, B.Y + B.Height - Area.Height);
               end if;

               Self.Topleft := (X, Y);
               Self.Set_Adjustment_Values;
               Self.Queue_Draw;

            else
               C := Self.Dragged_Items.First;
               while Has_Element (C) loop
                  It := Element (C);

                  X := It.Pos.X
                    + (Details.Root_Point.X
                       - Self.Last_Button_Press.Root_Point.X) / Self.Scale;
                  Y := It.Pos.Y
                    + (Details.Root_Point.Y
                       - Self.Last_Button_Press.Root_Point.Y) / Self.Scale;

                  if Self.Last_Button_Press.Allowed_Drag_Area /=
                    Drag_Anywhere
                  then
                     BB := It.Item.Bounding_Box;
                     B  := Self.Last_Button_Press.Allowed_Drag_Area;

                     X := Model_Coordinate'Max (X, B.X);
                     X := Model_Coordinate'Min (X, B.X + B.Width - BB.Width);
                     Y := Model_Coordinate'Max (Y, B.Y);
                     Y := Model_Coordinate'Min (Y, B.Y + B.Height - BB.Height);
                  end if;

                  It.Item.Set_Position ((X => X, Y => Y));
                  Next (C);
               end loop;

               Refresh_Link_Layout (Self.Model, Self.Dragged_Items);

               --  Should redo the layout for links, but this might be
               --  expensive.
               Self.Queue_Draw;
            end if;
         end if;
      end if;
      return False;
   end On_Motion_Notify_Event;

   ---------------
   -- Set_Model --
   ---------------

   procedure Set_Model
      (Self  : not null access Canvas_View_Record'Class;
       Model : access Canvas_Model_Record'Class)
   is
   begin
      if Self.Model = Canvas_Model (Model) then
         return;
      end if;

      if Self.Model /= null then
         Disconnect (Self.Model, Self.Id_Layout_Changed);
         Disconnect (Self.Model, Self.Id_Item_Contents_Changed);
         Unref (Self.Model);
      end if;

      Self.Model := Canvas_Model (Model);
      Ref (Self.Model);

      if Self.Model /= null then
         Self.Id_Layout_Changed := Model.On_Layout_Changed
            (On_Layout_Changed_For_View'Access, Self);
         Self.Id_Item_Contents_Changed := Model.On_Item_Contents_Changed
            (On_Item_Contents_Changed_For_View'Access, Self);
      end if;

      if Self.Model /= null and then Self.Model.Layout = null then
         Self.Model.Layout := Self.Layout;  --  needed for layout
         Ref (Self.Model.Layout);
         Self.Model.Refresh_Layout;
      else
         Set_Adjustment_Values (Self);
         Self.Queue_Draw;
      end if;
   end Set_Model;

   -----------
   -- Model --
   -----------

   function Model
     (Self  : not null access Canvas_View_Record'Class)
      return Canvas_Model
   is
   begin
      return Self.Model;
   end Model;

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
         Parameters   => (1 => (1 => GType_None),
                          2 => (1 => GType_Pointer)),
         Returns      => (1 => GType_None,
                          2 => GType_Boolean),
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
      Min, Max : Gdouble;
   begin
      if Self.Model = null or else Area.Width <= 1.0 then
         --  Not allocated yet
         return;
      end if;

      --  We want a small margin around the minimal box for the model, since it
      --  looks better.

      Box := Self.Model.Bounding_Box (View_Margin / Self.Scale);

      --  We set the adjustments to include the model area, but also at least
      --  the current visible area (if we don't, then part of the display will
      --  not be properly refreshed).

      if Self.Hadj /= null then
         Min := Gdouble'Min (Area.X, Box.X);
         Max := Gdouble'Max (Area.X + Area.Width, Box.X + Box.Width);
         Self.Hadj.Configure
           (Value          => Area.X,
            Lower          => Min,
            Upper          => Max,
            Step_Increment => 5.0,
            Page_Increment => 100.0,
            Page_Size      => Area.Width);
      end if;

      if Self.Vadj /= null then
         Min := Gdouble'Min (Area.Y, Box.Y);
         Max := Gdouble'Max (Area.Y + Area.Height, Box.Y + Box.Height);
         Self.Vadj.Configure
           (Value          => Area.Y,
            Lower          => Min,
            Upper          => Max,
            Step_Increment => 5.0,
            Page_Increment => 100.0,
            Page_Size      => Area.Height);
      end if;

      --  ??? This is already called when changing the value of the adjustments
      --  but not if we only change the page_size for instance.
--      Self.Viewport_Changed;
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

      if S.Layout /= null then
         Unref (S.Layout);
         S.Layout := null;
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

      if Self.Scale_To_Fit_Requested /= 0.0 then
         Self.Scale_To_Fit (Max_Scale => Self.Scale_To_Fit_Requested);
      end if;
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
   -- View_To_Model --
   -------------------

   function View_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : View_Point) return Model_Point
   is
   begin
      return (X      => P.X / Self.Scale + Self.Topleft.X,
              Y      => P.Y / Self.Scale + Self.Topleft.Y);
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
      return (X    => Window_Coordinate (View.X) + Window_Coordinate (Alloc.X),
              Y    => Window_Coordinate (View.Y) + Window_Coordinate (Alloc.Y),
              Width  => Window_Coordinate (View.Width),
              Height => Window_Coordinate (View.Height));
   end Model_To_Window;

   ---------------------
   -- Window_To_Model --
   ---------------------

   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      P      : Window_Point) return Model_Point
   is
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      return Self.View_To_Model
        ((X      => View_Coordinate (P.X) - View_Coordinate (Alloc.X),
          Y      => View_Coordinate (P.Y) - View_Coordinate (Alloc.Y)));
   end Window_To_Model;

   ---------------------
   -- Window_To_Model --
   ---------------------

   function Window_To_Model
     (Self   : not null access Canvas_View_Record;
      Rect   : Window_Rectangle) return Model_Rectangle
   is
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      return Self.View_To_Model
        ((X      => View_Coordinate (Rect.X) - View_Coordinate (Alloc.X),
          Y      => View_Coordinate (Rect.Y) - View_Coordinate (Alloc.Y),
          Width  => View_Coordinate (Rect.Width),
          Height => View_Coordinate (Rect.Height)));
   end Window_To_Model;

   ---------------
   -- Set_Scale --
   ---------------

   procedure Set_Scale
     (Self      : not null access Canvas_View_Record;
      Scale     : Gdouble := 1.0;
      Center_On : Model_Point := No_Point)
   is
      Box : Model_Rectangle;
      Old_Scale : constant Gdouble := Self.Scale;
      P   : Model_Point;
   begin
      if Center_On /= No_Point then
         P := Center_On;
      else
         Box := Self.Get_Visible_Area;
         P := (Box.X + Box.Width / 2.0, Box.Y + Box.Height / 2.0);
      end if;

      Self.Scale := Scale;
      Self.Topleft :=
        (P.X - (P.X - Self.Topleft.X) * Old_Scale / Scale,
         P.Y - (P.Y - Self.Topleft.Y) * Old_Scale / Scale);

      Self.Scale_To_Fit_Requested := 0.0;
      Self.Set_Adjustment_Values;
      Self.Queue_Draw;
   end Set_Scale;

   ---------------
   -- Get_Scale --
   ---------------

   function Get_Scale
     (Self : not null access Canvas_View_Record) return Gdouble is
   begin
      return Self.Scale;
   end Get_Scale;

   ------------------
   -- Scale_To_Fit --
   ------------------

   procedure Scale_To_Fit
     (Self      : not null access Canvas_View_Record;
      Max_Scale : Gdouble := 4.0)
   is
      Box   : Model_Rectangle;
      W, H  : Gdouble;
      Alloc : Gtk_Allocation;
   begin
      Self.Get_Allocation (Alloc);
      if Alloc.Width <= 1 then
         Self.Scale_To_Fit_Requested := Max_Scale;

      elsif Self.Model /= null then
         Self.Scale_To_Fit_Requested := 0.0;

         Box := Self.Model.Bounding_Box;

         if Box.Width /= 0.0 and then Box.Height /= 0.0 then
            W := Gdouble (Alloc.Width);
            H := Gdouble (Alloc.Height);

            --  The "-1.0" below compensates for rounding errors, since
            --  otherwise we are still seeing the scrollbar along the axis
            --  used to compute the scale.
            Self.Scale := Gdouble'Min
              (Max_Scale,
               Gdouble'Min
                 ((W - 2.0 * View_Margin - 1.0) / Box.Width,
                  (H - 2.0 * View_Margin - 1.0) / Box.Height));
            Self.Topleft :=
              (X => Box.X - (W / Self.Scale - Box.Width) / 2.0,
               Y => Box.Y - (H / Self.Scale - Box.Height) / 2.0);

            Self.Set_Adjustment_Values;
            Self.Queue_Draw;
         end if;
      end if;
   end Scale_To_Fit;

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

   ----------------
   -- Item_Event --
   ----------------

   function Item_Event
     (Self    : not null access Canvas_View_Record'Class;
      Details : Event_Details_Access)
      return Boolean
   is
   begin
      return EDA_Emit
        (Self, Signal_Item_Event & ASCII.NUL, Details);
   end Item_Event;

   -------------------
   -- On_Item_Event --
   -------------------

   procedure On_Item_Event
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access function
        (Self    : not null access GObject_Record'Class;
         Details : Event_Details_Access)
        return Boolean;
      Slot : access GObject_Record'Class := null)
   is
   begin
      if Slot = null then
         Object_Return_Callback.Connect
           (Self, Signal_Item_Event,
            EDA_Marshallers.To_Marshaller (Call));
      else
         Object_Return_Callback.Object_Connect
           (Self, Signal_Item_Event,
            EDA_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Item_Event;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self : not null access Canvas_View_Record'Class;
      Cr   : Cairo.Cairo_Context;
      Area : Model_Rectangle := No_Rectangle)
   is
      A : Model_Rectangle;
      C : Draw_Context;
   begin
      if Area = No_Rectangle then
         A := Self.Get_Visible_Area;
      else
         A := Area;
      end if;

      --  GDK already clears the exposed area to the background color, so
      --  we do not need to clear ourselves.

      C := (Cr => Cr, Layout => Self.Layout);

      Save (Cr);
      Self.Set_Transform (Cr);
      Self.Draw_Internal (C, A);
      Restore (Cr);
   end Refresh;

   -----------------------------
   -- Translate_And_Draw_Item --
   -----------------------------

   procedure Translate_And_Draw_Item
     (Self    : not null access Abstract_Item_Record'Class;
      Context : Draw_Context)
   is
      Pos : Gtkada.Style.Point;
   begin
      Save (Context.Cr);
      Pos := Self.Position;
      Translate (Context.Cr, Pos.X, Pos.Y);
      Self.Draw (Context);

      if Debug_Show_Bounding_Boxes then
         declare
            Box : constant Item_Rectangle := Self.Bounding_Box;
         begin
            Gtk_New (Stroke => (1.0, 0.0, 0.0, 0.8),
                     Dashes => (2.0, 2.0))
              .Draw_Rect (Context.Cr, (Box.X, Box.Y), Box.Width, Box.Height);
         end;
      end if;

      Restore (Context.Cr);
   exception
      when E : others =>
         Restore (Context.Cr);
         Process_Exception (E);
   end Translate_And_Draw_Item;

   -------------------
   -- Draw_Internal --
   -------------------

   procedure Draw_Internal
     (Self    : not null access Canvas_View_Record;
      Context : Draw_Context;
      Area    : Model_Rectangle)
   is
      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class);
      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         Translate_And_Draw_Item (Item, Context);
      end Draw_Item;
   begin
      if Self.Model /= null then
         Self.Model.For_Each_Item (Draw_Item'Access, In_Area => Area);
      end if;
   end Draw_Internal;

   -------------
   -- Gtk_New --
   -------------

   function Gtk_New
     (From, To    : not null access Abstract_Item_Record'Class;
      Style       : Gtkada.Style.Drawing_Style;
      Routing     : Route_Style := Straight;
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null)
      return Canvas_Link
   is
      L : constant Canvas_Link := new Canvas_Link_Record;
      F : Anchor_Attachment := Anchor_From;
      T : Anchor_Attachment := Anchor_To;
   begin
      if From.all in Canvas_Link_Record'Class then
         F.Toplevel_Side := No_Clipping;
      end if;

      if To.all in Canvas_Link_Record'Class then
         T.Toplevel_Side := No_Clipping;
      end if;

      Initialize
        (L, From, To, Style, Routing, Label, F, Label_From, T, Label_To);
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
      Label       : access Container_Item_Record'Class := null;
      Anchor_From : Anchor_Attachment := Middle_Attachment;
      Label_From  : access Container_Item_Record'Class := null;
      Anchor_To   : Anchor_Attachment := Middle_Attachment;
      Label_To    : access Container_Item_Record'Class := null)
   is
   begin
      Link.From        := Abstract_Item (From);
      Link.To          := Abstract_Item (To);
      Link.Style       := Style;
      Link.Routing     := Routing;
      Link.Anchor_From := Anchor_From;
      Link.Anchor_To   := Anchor_To;
      Link.Label       := Container_Item (Label);
      Link.Label_From  := Container_Item (Label_From);
      Link.Label_To    := Container_Item (Label_To);
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
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context) is
   begin
      Gtkada.Canvas_View.Links.Draw_Link (Self, Context);
   end Draw;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Canvas_Link_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      pragma Unreferenced (Self, Point, Context);
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
      Free (Abstract_Item (Self.Label));
      Free (Abstract_Item (Self.Label_From));
      Free (Abstract_Item (Self.Label_To));
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
     (Self    : not null access Canvas_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Box : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
   begin
      --  ??? This does not take into account the line width
      return Point_In_Rect (Box, Point);
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

   ----------------
   -- Intersects --
   ----------------

   function Intersects (Rect1, Rect2 : Item_Rectangle) return Boolean is
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

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Self : not null access Canvas_Link_Record) return Drawing_Style
   is
   begin
      return Self.Style;
   end Get_Style;

   ----------------
   -- Get_Points --
   ----------------

   function Get_Points
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array_Access
   is
   begin
      return Self.Points;
   end Get_Points;

   -------------------
   -- Set_Waypoints --
   -------------------

   procedure Set_Waypoints
     (Self     : not null access Canvas_Link_Record;
      Points   : Item_Point_Array;
      Relative : Boolean := False)
   is
   begin
      Unchecked_Free (Self.Waypoints);
      if Points'Length /= 0 then
         Self.Waypoints := new Item_Point_Array'(Points);
      end if;
      Self.Relative_Waypoints := Relative;
   end Set_Waypoints;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context) is
   begin
      --  Target links must already have their own layout

      if Self.From.all in Canvas_Link_Record'Class
         and then Canvas_Link (Self.From).Points = null
      then
         Canvas_Link (Self.From).Refresh_Layout (Context);
      end if;

      if Self.To.all in Canvas_Link_Record'Class
         and then Canvas_Link (Self.To).Points = null
      then
         Canvas_Link (Self.To).Refresh_Layout (Context);
      end if;

      case Self.Routing is
         when Orthogonal =>
            Compute_Layout_For_Orthogonal_Link (Self, Context);
         when Straight =>
            Compute_Layout_For_Straight_Link (Self, Context);
         when Arc =>
            Compute_Layout_For_Arc_Link (Self, Context, Self.Offset);
         when Curve =>
            Compute_Layout_For_Curve_Link (Self, Context);
      end case;
   end Refresh_Layout;

   -------------------------
   -- Refresh_Link_Layout --
   -------------------------

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_And_Position_Lists.List :=
        Item_And_Position_Lists.Empty_List)
   is
      pragma Unreferenced (Items);
      Context : constant Draw_Context := (Cr => <>, Layout => Model.Layout);

      procedure Reset_Link_Layout
        (It : not null access Abstract_Item_Record'Class);
      --  Invalid the current layout for the link

      procedure Do_Link_Layout
        (It : not null access Abstract_Item_Record'Class);
      --  Recompute the layout for the link (and first to any link it is
      --  linked to).

      procedure Reset_Link_Layout
        (It : not null access Abstract_Item_Record'Class) is
      begin
         --  ??? Should only reset layout for the links to any of items, or
         --  links to those links.
         --  This is inefficient to compute in general, so perhaps we should
         --  instead provide an overridable primitive operation on the model.
         if It.all in Canvas_Link_Record'Class then
            Unchecked_Free (Canvas_Link_Record'Class (It.all).Points);
         end if;
      end Reset_Link_Layout;

      procedure Do_Link_Layout
        (It : not null access Abstract_Item_Record'Class)
      is
         Link : Canvas_Link;
      begin
         if It.all in Canvas_Link_Record'Class then
            Link := Canvas_Link (It);
            if Link.Points = null then
               Link.Refresh_Layout (Context);
            end if;
         end if;
      end Do_Link_Layout;

   begin
      --  To properly do the layout of links, we must first compute the layout
      --  of any item they are linked to, including other links. So we do the
      --  following:
      --     reset all previous layout computation
      --     when we layout a link, we first layout its end

      Model.For_Each_Item (Reset_Link_Layout'Access);
      Model.For_Each_Item (Do_Link_Layout'Access);
   end Refresh_Link_Layout;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout (Self : not null access Canvas_Model_Record) is
      Context : constant Draw_Context := (Cr => <>, Layout => Self.Layout);

      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class)
      is
      begin
         if Item.all in Container_Item_Record'Class then
            Container_Item_Record'Class (Item.all).Refresh_Layout (Context);
         end if;
      end Do_Container_Layout;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Container_Layout'Access);
      Refresh_Link_Layout (Self);

      Self.Layout_Changed;
   end Refresh_Layout;

   -----------------------
   -- Toplevel_Items_At --
   -----------------------

   function Toplevel_Item_At
     (Self    : not null access Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item
   is
      Found : Abstract_Item;

      procedure Check_Item
        (Item : not null access Abstract_Item_Record'Class);
      procedure Check_Item
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         if Found = null
           and then Item.Contains (Model_To_Item (Item, Point), Context)
         then
            Found := Abstract_Item (Item);
         end if;
      end Check_Item;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item (Check_Item'Access);
      return Found;
   end Toplevel_Item_At;

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
     (Self   : not null access Canvas_Model_Record;
      Margin : Model_Coordinate := 0.0)
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
         Result.X := Result.X - Margin;
         Result.Y := Result.Y - Margin;
         Result.Width := Result.Width + 2.0 * Margin;
         Result.Height := Result.Height + 2.0 * Margin;
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
      Float    : Boolean := False;
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

   ---------------------
   -- Inner_Most_Item --
   ---------------------

   overriding function Inner_Most_Item
     (Self     : not null access Container_Item_Record;
      At_Point : Model_Point;
      Context  : Draw_Context) return Abstract_Item
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
      P     : Item_Point;
      Child : Container_Item;
   begin
      while Has_Element (C) loop
         Child := Container_Item (Element (C));
         P := Child.Model_To_Item  (At_Point);

         if Child.Contains (P, Context) then
            return Child.Inner_Most_Item (At_Point, Context);
         end if;

         Next (C);
      end loop;
      return Abstract_Item (Self);
   end Inner_Most_Item;

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
   -- Set_Position --
   ------------------

   overriding procedure Set_Position
     (Self  : not null access Container_Item_Record;
      Pos   : Gtkada.Style.Point)
   is
   begin
      Self.Computed_Position := Pos;
      Canvas_Item_Record (Self.all).Set_Position (Pos);  --  inherited
   end Set_Position;

   ------------------
   -- Set_Min_Size --
   ------------------

   procedure Set_Min_Size
     (Self       : not null access Container_Item_Record;
      Min_Width  : Gdouble := 1.0;
      Min_Height : Gdouble := 1.0)
   is
   begin
      Self.Min_Width := Min_Width;
      Self.Min_Height := Min_Height;
   end Set_Min_Size;

   ------------------
   -- Size_Request --
   ------------------

   procedure Size_Request
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context)
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
         Child.Size_Request (Context);

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

               if not Child.Float then
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

               if not Child.Float then
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
      Self.Width := Model_Coordinate'Max (Self.Width, Self.Min_Width);
      Self.Height := Model_Coordinate'Max (Self.Height, Self.Min_Height);
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

               if not Child.Float and then Child.Forced_Width <= 0.0 then
                  Child.Width := Self.Width
                    - Child.Margin.Right
                    - Child.Computed_Position.X;
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

               Child.Size_Allocate;

               if not Child.Float then
                  Tmp := Child.Computed_Position.Y
                    + Child.Height + Child.Margin.Bottom;
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

               if not Child.Float and then Child.Forced_Height <= 0.0 then
                  Child.Height := Self.Height
                    - Child.Margin.Bottom
                    - Child.Computed_Position.Y;
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

               Child.Size_Allocate;

               if not Child.Float then
                  Tmp := Child.Computed_Position.X
                    + Child.Width + Child.Margin.Right;
               end if;
         end case;

         Next (C);
      end loop;
   end Size_Allocate;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self    : not null access Container_Item_Record;
      Context : Draw_Context) is
   begin
      Self.Computed_Position := Self.Position;
      Container_Item_Record'Class (Self.all).Size_Request (Context);
      Container_Item_Record'Class (Self.all).Size_Allocate;
   end Refresh_Layout;

   -------------------
   -- Draw_Children --
   -------------------

   procedure Draw_Children
     (Self    : not null access Container_Item_Record'Class;
      Context : Draw_Context)
   is
      use Items_Lists;
      C     : Items_Lists.Cursor := Self.Children.First;
   begin
      while Has_Element (C) loop
         Translate_And_Draw_Item (Element (C), Context);
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
      Initialize_Rect (R, Style, Width, Height, Radius);
      return R;
   end Gtk_New_Rect;

   ---------------------
   -- Initialize_Rect --
   ---------------------

   procedure Initialize_Rect
     (Self          : not null access Rect_Item_Record'Class;
      Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := -1.0;
      Radius        : Model_Coordinate := 0.0)
   is
   begin
      Self.Style         := Style;
      Self.Forced_Width  := Width;
      Self.Forced_Height := Height;
      Self.Radius        := Radius;
   end Initialize_Rect;

   -------------------------
   -- Resize_Fill_Pattern --
   -------------------------

   procedure Resize_Fill_Pattern
     (Self : not null access Container_Item_Record'Class)
   is
      Matrix  : aliased Cairo_Matrix;
      Fill    : constant Cairo_Pattern := Self.Style.Get_Fill;
   begin
      if Fill /= Null_Pattern then
         case Get_Type (Fill) is
            when Cairo_Pattern_Type_Linear
               | Cairo_Pattern_Type_Radial =>

               Init_Scale (Matrix'Access, 1.0 / Self.Width, 1.0 / Self.Height);
               Set_Matrix (Fill, Matrix'Access);

            when others =>
               null;
         end case;
      end if;
   end Resize_Fill_Pattern;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Rect_Item_Record;
      Context : Draw_Context)
   is
      Stroke : constant Gdk_RGBA := Self.Style.Get_Stroke;
      Fill   : constant Cairo_Pattern := Self.Style.Get_Fill;
   begin
      Resize_Fill_Pattern (Self);

      --  We need to clip the contents of the rectangle. Also, we
      --  stroke only after drawing the children so that they do
      --  no hide the stroke.

      if Self.Style.Path_Rect
        (Context.Cr, (0.0, 0.0), Self.Width, Self.Height,
         Radius => Self.Radius)
      then
         Clip_Preserve (Context.Cr);

         if not Self.Children.Is_Empty then
            Self.Style.Set_Stroke (Null_RGBA);
         end if;

         Self.Style.Finish_Path (Context.Cr);
         Self.Style.Set_Stroke (Stroke);
      end if;

      Self.Draw_Children (Context);

      if not Self.Children.Is_Empty
        and then Stroke /= Null_RGBA
        and then Self.Style.Path_Rect
        (Context.Cr, (0.0, 0.0), Self.Width, Self.Height,
         Radius => Self.Radius)
      then
         Self.Style.Set_Fill (Null_Pattern);
         Self.Style.Finish_Path (Context.Cr);
         Self.Style.Set_Fill (Fill);
      end if;
   end Draw;

   ----------------------
   -- Gtk_New_Polyline --
   ----------------------

   function Gtk_New_Polyline
     (Style    : Gtkada.Style.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False)
      return Polyline_Item
   is
      R   : constant Polyline_Item := new Polyline_Item_Record;
   begin
      Initialize_Polyline (R, Style, Points, Close, Relative);
      return R;
   end Gtk_New_Polyline;

   -------------------------
   -- Initialize_Polyline --
   -------------------------

   procedure Initialize_Polyline
     (Self     : not null access Polyline_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Points   : Item_Point_Array;
      Close    : Boolean := False;
      Relative : Boolean := False) is
   begin
      Self.Style    := Style;
      Self.Close    := Close;
      Self.Relative := Relative;
      Self.Points   := new Item_Point_Array'(Points);
   end Initialize_Polyline;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context)
   is
      B : constant Item_Rectangle := Compute_Bounding_Box
        (Self.Points.all, Relative => Self.Relative);
   begin
      Container_Item_Record (Self.all).Size_Request (Context);  --  inherited
      Self.Width := Model_Coordinate'Max (Self.Width, B.Width + B.X);
      Self.Height := Model_Coordinate'Max (Self.Height, B.Height + B.Y);
   end Size_Request;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Polyline_Item_Record;
      Context : Draw_Context)
   is
   begin
      Resize_Fill_Pattern (Self);
      Self.Style.Draw_Polyline
        (Context.Cr, Self.Points.all,
         Close    => Self.Close,
         Relative => Self.Relative);
      Draw_Children (Self, Context);
   end Draw;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Polyline_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      Box   : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
   begin
      if Point_In_Rect (Box, Point) then
         if Self.Style.Path_Polyline
           (Context.Cr,
            Points   => Self.Points.all,
            Close    => Self.Close,
            Relative => Self.Relative)
         then
            return In_Fill (Context.Cr, Point.X, Point.Y);
         end if;
      end if;
      return False;
   end Contains;

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

   ---------------------
   -- Gtk_New_Ellipse --
   ---------------------

   function Gtk_New_Ellipse
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := -1.0)
      return Ellipse_Item
   is
      R : constant Ellipse_Item := new Ellipse_Item_Record;
   begin
      Initialize_Ellipse (R, Style, Width, Height);
      return R;
   end Gtk_New_Ellipse;

   ------------------------
   -- Initialize_Ellipse --
   ------------------------

   procedure Initialize_Ellipse
     (Self          : not null access Ellipse_Item_Record'Class;
      Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := -1.0)
   is
   begin
      Self.Style := Style;
      Self.Forced_Width := Width;
      Self.Forced_Height := Height;
   end Initialize_Ellipse;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Ellipse_Item_Record;
      Context : Draw_Context)
   is
   begin
      Resize_Fill_Pattern (Self);
      Self.Style.Draw_Ellipse
        (Context.Cr, (0.0, 0.0), Self.Width, Self.Height);
      Self.Draw_Children (Context);
   end Draw;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Ellipse_Item_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      pragma Unreferenced (Context);
      Box   : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
      Center : Item_Point;
      Dx, Dy : Item_Coordinate;
   begin
      if Point_In_Rect (Box, Point) then
         Center := (Box.Width / 2.0, Box.Height / 2.0);

         --  This doesn't test the whole rectangle, only the topleft corner
         Dx := Point.X - Center.X;
         Dy := Point.Y - Center.Y;
         return Dx * Dx / (Center.X * Center.X)
           + Dy * Dy / (Center.Y * Center.Y) <= 1.0;
      end if;
      return False;
   end Contains;

   ------------------
   -- Gtk_New_Text --
   ------------------

   function Gtk_New_Text
     (Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
      return Text_Item
   is
      R : constant Text_Item := new Text_Item_Record;
   begin
      Initialize_Text (R, Style, Text, Directed);
      return R;
   end Gtk_New_Text;

   ---------------------
   -- Initialize_Text --
   ---------------------

   procedure Initialize_Text
     (Self     : not null access Text_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow) is
   begin
      Self.Style := Style;
      Self.Text  := new String'(Text);
      Self.Directed := Directed;
   end Initialize_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Self : not null access Text_Item_Record;
      Text : String)
   is
   begin
      Free (Self.Text);
      Self.Text := new String'(Text);
   end Set_Text;

   ------------------
   -- Compute_Text --
   ------------------

   function Compute_Text
     (Self : not null access Text_Item_Record'Class)
      return String
   is
   begin
      case Self.Directed is
         when No_Text_Arrow =>
            return Self.Text.all;

         when Up_Text_Arrow =>
            return Self.Text.all & " "
              & Character'Val (16#E2#)   --  UTF8 for  \u25b2
              & Character'Val (16#96#)
              & Character'Val (16#B2#);

         when Down_Text_Arrow =>
            return Self.Text.all & " "
              & Character'Val (16#E2#)   --  UTF8 for  \u25bc
              & Character'Val (16#96#)
              & Character'Val (16#BC#);

         when Left_Text_Arrow =>
            return Character'Val (16#E2#)   --  UTF8 for  \u25c0
              & Character'Val (16#97#)
              & Character'Val (16#80#)
              & " " & Self.Text.all;

         when Right_Text_Arrow =>
            return Self.Text.all & " "
              & Character'Val (16#E2#)   --  UTF8 for  \u25b6
              & Character'Val (16#96#)
              & Character'Val (16#B6#);
      end case;
   end Compute_Text;

   ------------------
   -- Set_Directed --
   ------------------

   procedure Set_Directed
     (Self     : not null access Text_Item_Record;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
   is
   begin
      Self.Directed := Directed;
   end Set_Directed;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Text_Item_Record;
      Context : Draw_Context)
   is
      F    : constant Font_Style := Self.Style.Get_Font;
      Text : constant String := Compute_Text (Self);
      Y    : Glib.Gdouble := 0.0;
   begin
      Resize_Fill_Pattern (Self);
      Self.Style.Draw_Rect (Context.Cr, (0.0, 0.0), Self.Width, Self.Height);

      if F.Valign /= 0.0
        and then Self.Height > Self.Requested_Height
      then
         Y := (Self.Height - Self.Requested_Height) * F.Valign;
      end if;

      if Context.Layout /= null then
         Self.Style.Draw_Text
           (Context.Cr, Context.Layout, (0.0, Y), Text,
            Max_Width  => Self.Width,
            Max_Height => Self.Height - Y);
      end if;
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : not null access Text_Item_Record) is
   begin
      Free (Self.Text);
      Container_Item_Record (Self.all).Destroy;  --  inherited
   end Destroy;

   ----------------
   -- Align_Text --
   ----------------

   procedure Align_Text (Self  : not null access Text_Item_Record) is
      F    : constant Font_Style := Self.Style.Get_Font;
   begin
      if F.Valign /= 0.0
        and then Self.Position.Y /= Gdouble'First
      then
         --  We want the Y coordinate to be in a specific position in the
         --  text box

         Self.Computed_Position.Y :=
           Self.Computed_Position.Y
             - Self.Height * F.Valign;
      end if;
   end Align_Text;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self  : not null access Text_Item_Record;
      Context : Draw_Context)
   is
      Text : constant String := Compute_Text (Self);
   begin
      if Context.Layout /= null then
         Self.Style.Measure_Text
           (Context.Layout,
            Text,
            Width => Self.Requested_Width,
            Height => Self.Requested_Height);
         Self.Width  := Self.Requested_Width;
         Self.Height := Self.Requested_Height;

         Align_Text (Self);
      else
         Self.Width := 0.0;
         Self.Height := 0.0;
      end if;
   end Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   overriding procedure Size_Allocate
     (Self : not null access Text_Item_Record)
   is
   begin
      Container_Item_Record (Self.all).Size_Allocate;
      Align_Text (Self);
   end Size_Allocate;

   ----------------
   -- Gtk_New_Hr --
   ----------------

   function Gtk_New_Hr
     (Style   : Gtkada.Style.Drawing_Style;
      Text    : String := "")
     return Hr_Item
   is
      R : constant Hr_Item := new Hr_Item_Record;
   begin
      Initialize_Hr (R, Style, Text);
      return R;
   end Gtk_New_Hr;

   -------------------
   -- Initialize_Hr --
   -------------------

   procedure Initialize_Hr
     (Self    : not null access Hr_Item_Record'Class;
      Style   : Gtkada.Style.Drawing_Style;
      Text    : String := "") is
   begin
      Self.Style := Style;
      Self.Text := new String'(Text);
   end Initialize_Hr;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context)
   is
      H : constant Model_Coordinate := Self.Height / 2.0;
      W : Model_Coordinate;
   begin
      if Self.Text.all = "" then
         Self.Style.Draw_Polyline (Context.Cr, ((0.0, H), (Self.Width, H)));
      else
         W := (Self.Width - Self.Requested_Width) / 2.0;
         Self.Style.Draw_Polyline (Context.Cr, ((0.0, H), (W, H)));

         W := W + Self.Space;
         if Context.Layout /= null then
            Self.Style.Draw_Text
              (Context.Cr, Context.Layout,
               (W, (Self.Height - Self.Requested_Height) / 2.0),
               Self.Text.all,
               Max_Width  => Self.Width,
               Max_Height => Self.Height);
         end if;

         W := W + Self.Requested_Width + Self.Space;
         Self.Style.Draw_Polyline (Context.Cr, ((W, H), (Self.Width, H)));
      end if;
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : not null access Hr_Item_Record) is
   begin
      Free (Self.Text);
      Container_Item_Record (Self.all).Destroy;
   end Destroy;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Hr_Item_Record;
      Context : Draw_Context)
   is
   begin
      if Context.Layout /= null and then Self.Text.all /= "" then
         Self.Style.Measure_Text
           (Context.Layout,
            Self.Text.all,
            Width  => Self.Requested_Width,
            Height => Self.Requested_Height);

         --  Some space to show the lines
         Self.Width  := Self.Requested_Width
             + 2.0 * Self.Space
             + 8.0;   --  lines should be at least 4 pixels each side
         Self.Height := Self.Requested_Height;
      else
         Self.Width  := 8.0;
         Self.Height := 1.0;
      end if;
   end Size_Request;

   ----------------
   -- Set_Offset --
   ----------------

   procedure Set_Offset
     (Self    : not null access Canvas_Link_Record;
      Offset  : Gdouble)
   is
   begin
      Self.Offset := Offset;
   end Set_Offset;

   ------------------
   -- Intersection --
   ------------------
   --  Algorithm adapted from [GGII - xlines.c].
   --  Copied from gnatcoll-geometry.adb

   function Intersection
     (P11, P12, P21, P22 : Item_Point) return Item_Point
   is
      type Line is record
         A, B, C : Item_Coordinate;
      end record;

      function To_Line (P1, P2 : Item_Point) return Line;
      --  Compute the parameters for the line
      --     Ax + By = C

      function To_Line (P1, P2 : Item_Point) return Line is
         A : constant Item_Coordinate := P2.Y - P1.Y;
         B : constant Item_Coordinate := P1.X - P2.X;
      begin
         return (A => A,
                 B => B,
                 C => A * P1.X + B * P1.Y);
      end To_Line;

      L1 : constant Line := To_Line (P11, P12);
      R3 : constant Item_Coordinate := L1.A * P21.X + L1.B * P21.Y - L1.C;
      R4 : constant Item_Coordinate := L1.A * P22.X + L1.B * P22.Y - L1.C;
      L2 : constant Line := To_Line (P21, P22);
      R1 : constant Item_Coordinate := L2.A * P11.X + L2.B * P11.Y - L2.C;
      R2 : constant Item_Coordinate := L2.A * P12.X + L2.B * P12.Y - L2.C;

      Denom : Item_Coordinate;

   begin
      --  Check signs of R3 and R4. If both points 3 and 4 lie on same side
      --  of line 1, the line segments do not intersect

      if (R3 > 0.0 and then R4 > 0.0)
        or else (R3 < 0.0 and then R4 < 0.0)
      then
         return (Gdouble'First, Gdouble'First);
      end if;

      --  Check signs of r1 and r2. If both points lie on same side of
      --  second line segment, the line segments do not intersect

      if (R1 > 0.0 and then R2 > 0.0)
        or else (R1 < 0.0 and then R2 < 0.0)
      then
         return (Gdouble'First, Gdouble'First);
      end if;

      --  Line segments intersect, compute intersection point
      Denom := L1.A * L2.B - L2.A * L1.B;
      if Denom = 0.0 then
         if abs (L1.A * P21.X + L1.B * P21.Y - L1.C) < 0.00001 then
            --  colinears
            return P11;
         else
            return (Gdouble'First, Gdouble'First);
         end if;
      end if;

      return
        (X => (L2.B * L1.C - L1.B * L2.C) / Denom,
         Y => (L1.A * L2.C - L2.A * L1.C) / Denom);
   end Intersection;

   ---------------
   -- Clip_Line --
   ---------------

   overriding function Clip_Line
     (Self   : not null access Polyline_Item_Record;
      P1, P2 : Item_Point) return Item_Point
   is
      Inter : Item_Point;
      Current, Next : Item_Point;
   begin
      if Self.Points /= null then
         Current := Self.Points (Self.Points'First);

         for P in Self.Points'First + 1 .. Self.Points'Last loop
            if Self.Relative then
               Next := (Current.X + Self.Points (P).X,
                        Current.Y + Self.Points (P).Y);
            else
               Next := Self.Points (P);
            end if;

            Inter := Intersection (P1, P2, Current, Next);
            if Inter.X /= Gdouble'First then
               return Inter;
            end if;

            Current := Next;
         end loop;

         if Self.Close then
            Inter := Intersection
              (P1, P2, Current, Self.Points (Self.Points'First));
            if Inter.X /= Gdouble'First then
               return Inter;
            end if;
         end if;
      end if;

      return P1;
   end Clip_Line;

end Gtkada.Canvas_View;
