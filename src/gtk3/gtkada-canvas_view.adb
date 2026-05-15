------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2021, AdaCore                     --
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
with GNAT.Strings;                       use GNAT.Strings;
with System;
with Cairo;                              use Cairo;
with Cairo.Matrix;                       use Cairo.Matrix;
with Cairo.Pattern;                      use Cairo.Pattern;
with Cairo.Png;
with Cairo.PDF;                          use Cairo.PDF;
with Cairo.Surface;
with Cairo.SVG;
with Glib.Main;                          use Glib.Main;
with Glib.Properties.Creation;           use Glib.Properties.Creation;
with Glib.Values;                        use Glib.Values;
with Gdk;                                use Gdk;
with Gdk.Cairo;                          use Gdk.Cairo;
with Gdk.RGBA;                           use Gdk.RGBA;
with Gdk.Types.Keysyms;                  use Gdk.Types.Keysyms;
with Gdk.Window_Attr;                    use Gdk.Window_Attr;
with Gdk.Window;                         use Gdk.Window;
with Gtk.Accel_Group;                    use Gtk.Accel_Group;
with Gtk.Enums;                          use Gtk.Enums;
with Gtk.Handlers;                       use Gtk.Handlers;
with Gtk.Scrollable;                     use Gtk.Scrollable;
with Gtk.Style_Context;                  use Gtk.Style_Context;
with Gtk.Text_Buffer;                    use Gtk.Text_Buffer;
with Gtk.Text_Iter;                      use Gtk.Text_Iter;
with Gtk.Text_View;                      use Gtk.Text_View;
with Gtk.Widget;                         use Gtk.Widget;
with Gtkada.Bindings;                    use Gtkada.Bindings;
with Gtkada.Canvas_View.Links;           use Gtkada.Canvas_View.Links;
with Gtkada.Canvas_View.Objects;         use Gtkada.Canvas_View.Objects;
with Gtkada.Canvas_View.Views;           use Gtkada.Canvas_View.Views;
with Gtkada.Handlers;                    use Gtkada.Handlers;
with Pango.Font;                         use Pango.Font;
with System.Storage_Elements;            use System.Storage_Elements;

pragma Warnings (Off, "call to obsolescent procedure ""Set_Background""");
--  Deprecated in Gtk+ 3.24

package body Gtkada.Canvas_View is

   Model_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String (String (Signal_Item_Contents_Changed)),
      2 => New_String (String (Signal_Layout_Changed)),
      3 => New_String (String (Signal_Selection_Changed)),
      4 => New_String (String (Signal_Item_Destroyed)));
   View_Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String (String (Signal_Viewport_Changed)),
      2 => New_String (String (Signal_Item_Event)),
      3 => New_String (String (Signal_Inline_Editing_Started)),
      4 => New_String (String (Signal_Inline_Editing_Finished)));

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

   Fixed_Size : constant Size := (Unit_Pixels, Gdouble'First);
   --  When this is set for the max size of an item, it indicates that the min
   --  size is in fact a hard-coded size that the widget must respect.

   function On_View_Draw
     (View : System.Address; Cr : Cairo_Context) return Gboolean;
   pragma Convention (C, On_View_Draw);
   --  default handler for "draw" on views.

   procedure On_Size_Allocate (View : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, On_Size_Allocate);
   --  default handler for "size_allocate" on views.

   procedure Move_Inline_Edit_Widget
     (Self : not null access Canvas_View_Record'Class);
   --  Move the inline editing widget, if one exists

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
   procedure On_Item_Destroyed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item);
   procedure On_Selection_Changed_For_View
     (View : not null access GObject_Record'Class;
      Item : Abstract_Item);
   --  Handles the model events for the view.

   procedure Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class);
   --  Emits the "item_destroyed" signal

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

   function Compute_Text
     (Self : not null access Text_Item_Record'Class)
      return String;
   --  Return the text to display for Self, including the directional arrow

   function Size_Above_Threshold
     (Self : not null access Abstract_Item_Record'Class;
      View : access Canvas_View_Record'Class) return Boolean;
   --  Whether the item's size is above the visibility threshold, i.e. whether
   --  the item is visible.

   procedure Resize_Fill_Pattern
     (Self : not null access Container_Item_Record'Class);
   --  Resize the fill pattern so that it extends to the whole item, instead of
   --  just the 0.0 .. 1.0 pattern space.

   procedure Destroy_And_Free
     (Self     : in out Abstract_Item;
      In_Model : not null access Canvas_Model_Record'Class);
   --  Free the memory used by Self

   function On_Button_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   function On_Key_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Key) return Boolean;
   function On_Motion_Notify_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean;
   function On_Scroll_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Scroll) return Boolean;
   --  Low-level handling of mouse events.

   function Intersection
     (P11, P12, P21, P22 : Item_Point) return Item_Point;
   --  Compute the intersection of the two segments (P11,P12) and (P21,P22).
   --  The result has X set to Gdouble'First when no intersection exists

   procedure Compute_Item
     (Self    : not null access Canvas_View_Record'Class;
      Details : in out Canvas_Event_Details);
   --  Compute the item that was clicked on, from the coordinates stored in
   --  Details.

   procedure On_View_Realize (Widget : System.Address);
   pragma Convention (C, On_View_Realize);
   --  Called when the view is realized

   function On_Text_Edit_Key_Press
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Key)
      return Boolean;
   --  Called when the user is inline-editing a text widget, to properly close
   --  the editor.

   procedure Cancel_Drag (Self : not null access Canvas_View_Record'Class);
   --  Cancel any drag currently in place.

   function Size_From_Value (Value : Model_Coordinate) return Size;
   pragma Inline (Size_From_Value);
   --  Return a size suitable for internal use, given a size (in pixels)
   --  given by the user. When the user provides a negative size, it is
   --  meant to indicate an automatic sizing (backward compatibility)

   ---------------------
   -- Size_From_Value --
   ---------------------

   function Size_From_Value (Value : Model_Coordinate) return Size is
   begin
      if Value = Fit_Size_As_Double then
         return Fit_Size;
      elsif Value = Auto_Size_As_Double then
         return Auto_Size;
      else
         return (Unit_Pixels, Value);
      end if;
   end Size_From_Value;

   ----------------------
   -- Destroy_And_Free --
   ----------------------

   procedure Destroy_And_Free
     (Self     : in out Abstract_Item;
      In_Model : not null access Canvas_Model_Record'Class)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Abstract_Item_Record'Class, Abstract_Item);
   begin
      if Self /= null then
         Item_Destroyed (In_Model, Self);
         Destroy (Self, In_Model);
         Unchecked_Free (Self);
      end if;
   end Destroy_And_Free;

   -----------------------------
   -- GValue_To_Abstract_Item --
   -----------------------------

   function GValue_To_Abstract_Item (Value : GValue) return Abstract_Item is
      S : constant System.Address := Get_Address (Value);
      pragma Warnings (Off, "possible aliasing problem*");
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Abstract_Item);
      pragma Warnings (On, "possible aliasing problem*");
   begin
      return Unchecked_Convert (S);
   end GValue_To_Abstract_Item;

   -------------------
   -- GValue_To_EDA --
   -------------------

   function GValue_To_EDA (Value : GValue) return Event_Details_Access is
      S : constant System.Address := Get_Address (Value);
      pragma Warnings (Off, "possible aliasing problem*");
      function Unchecked_Convert is new Ada.Unchecked_Conversion
        (System.Address, Event_Details_Access);
      pragma Warnings (On, "possible aliasing problem*");
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
         Parameters   => (1 => (1 => GType_Pointer), --  item_content_changed
                          2 => (1 => GType_None),  --  layout_changed
                          3 => (1 => GType_Pointer),
                          4 => (1 => GType_Pointer)));  --  item_destroyed
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
   begin
      if Slot = null then
         return Object_Callback.Connect
            (Self,
             Signal_Layout_Changed,
             Object_Callback.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
            (Self,
             Signal_Layout_Changed,
             Object_Callback.To_Marshaller (Call),
             Slot);
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

      Move_Inline_Edit_Widget (Self);
   end On_Layout_Changed_For_View;

   -----------------------------------
   -- On_Selection_Changed_For_View --
   -----------------------------------

   procedure On_Selection_Changed_For_View
     (View : not null access GObject_Record'Class;
      Item : Abstract_Item)
   is
      pragma Unreferenced (Item);
      Self  : constant Canvas_View := Canvas_View (View);
   begin
      Self.Queue_Draw;
   end On_Selection_Changed_For_View;

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

   --------------------------------
   -- On_Item_Destroyed_For_View --
   --------------------------------

   procedure On_Item_Destroyed_For_View
     (View   : access GObject_Record'Class;
      Item : Abstract_Item)
   is
      Self : constant Canvas_View := Canvas_View (View);
   begin
      if Self.Last_Button_Press.Item = Item then
         Self.Last_Button_Press.Item := null;
      end if;

      if Self.Last_Button_Press.Toplevel_Item = Item then
         Self.Last_Button_Press.Toplevel_Item := null;
      end if;

      if Self.Dragged_Items.Contains (Item) then
         Self.Dragged_Items.Delete (Item);
      end if;

      if Self.Inline_Edit.Item = Item then
         Cancel_Inline_Editing (Self);
      end if;

      Terminate_Animation_For_Item (Self, Item);
   end On_Item_Destroyed_For_View;

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
      Self.Set_Has_Window (True);

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
      Self.On_Key_Press_Event (On_Key_Event'Access);
      Self.On_Scroll_Event (On_Scroll_Event'Access);

      Self.Set_Can_Focus (True);

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
                  View   => Canvas_View (Self),
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
            Key        => 0,
            State      => Event.State and Get_Default_Mod_Mask,
            Root_Point => (Event.X_Root, Event.Y_Root),
            M_Point    => Self.Window_To_Model ((X => Event.X, Y => Event.Y)),
            T_Point    => No_Item_Point,
            I_Point    => No_Item_Point,
            Item       => null,
            Toplevel_Item => null,
            Allow_Snapping    => True,
            Allowed_Drag_Area => No_Drag_Allowed);
         Compute_Item (Self, Details);
         return Self.Item_Event (Details'Unchecked_Access);
      end if;
      return False;
   end On_Scroll_Event;

   ------------------------
   -- Initialize_Details --
   ------------------------

   procedure Initialize_Details
      (Self    : not null access Canvas_View_Record'Class;
       Details : out Canvas_Event_Details) is
      pragma Unreferenced (Self);
   begin
      Details :=
        (Event_Type => Custom,
         Button     => 1,
         Key        => 0,
         State      => 0,
         Root_Point => (0.0, 0.0),
         M_Point    => (0.0, 0.0),
         T_Point    => No_Item_Point,
         I_Point    => No_Item_Point,
         Item       => null,
         Toplevel_Item     => null,
         Allow_Snapping    => True,
         Allowed_Drag_Area => No_Drag_Allowed);
   end Initialize_Details;

   -----------------
   -- Set_Details --
   -----------------

   procedure Set_Details
     (Self    : not null access Canvas_View_Record'Class;
      Details : out Canvas_Event_Details;
      Event   : Gdk.Event.Gdk_Event_Button)
   is
   begin
      Details :=
        (Event_Type => Button_Press,
         Button     => Event.Button,
         Key        => 0,
         State      => Event.State and Get_Default_Mod_Mask,
         Root_Point => (Event.X_Root, Event.Y_Root),
         M_Point    => Self.Window_To_Model ((X => Event.X, Y => Event.Y)),
         T_Point    => No_Item_Point,
         I_Point    => No_Item_Point,
         Item       => null,
         Toplevel_Item     => null,
         Allow_Snapping    => True,
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

               --  The previous button press even might have deleted the item,
               --  in which case On_Item_Destroyed_For_View has reset the
               --  Last_Buttton_Press field
               if Details.M_Point = Self.Last_Button_Press.M_Point
                 and then Details.Toplevel_Item /= null
               then
                  --  Do not spend time recomputing
                  Details.Toplevel_Item :=
                    Self.Last_Button_Press.Toplevel_Item;
                  Details.Item := Self.Last_Button_Press.Item;
               else
                  Compute_Item (Self, Details);
               end if;
            end if;

         when others =>
            --  invalid
            Details.Event_Type := Key_Press;
      end case;
   end Set_Details;

   ------------------
   -- On_Key_Event --
   ------------------

   function On_Key_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Key) return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Box     : Model_Rectangle;
      IBox    : Item_Rectangle;
   begin
      --  Do not cancel drag, since pressing shift is used to disable snapping
      --  in this context.

      if Self.Model /= null then
         Details.Event_Type        := Key_Press;
         Details.State             := Event.State and Get_Default_Mod_Mask;
         Details.Allow_Snapping    := True;
         Details.Allowed_Drag_Area := No_Drag_Allowed;
         Details.Key               := Event.Keyval;

         if not Self.Model.Selection.Is_Empty then
            Details.Item := Item_Sets.Element (Self.Model.Selection.First);

            if Details.Item /= null then
               IBox := Details.Item.Bounding_Box;
               Details.I_Point := (IBox.X + IBox.Width / 2.0,
                                   IBox.Y + IBox.Height / 2.0);

               Box := Details.Item.Model_Bounding_Box;
               Details.M_Point := (Box.X + Box.Width / 2.0,
                                   Box.Y + Box.Height / 2.0);

               Details.Toplevel_Item := Details.Item.Get_Toplevel_Item;

               IBox := Details.Toplevel_Item.Bounding_Box;
               Details.T_Point := (IBox.X + IBox.Width / 2.0,
                                   IBox.Y + IBox.Height / 2.0);
            end if;
         end if;

         return Self.Item_Event (Details'Unchecked_Access);
      end if;

      return False;
   end On_Key_Event;

   -----------------
   -- Cancel_Drag --
   -----------------

   procedure Cancel_Drag (Self : not null access Canvas_View_Record'Class) is
   begin
      Self.In_Drag := False;
      Self.Dragged_Items.Clear;
      Self.Last_Button_Press.Allowed_Drag_Area := No_Drag_Allowed;
      Self.Grab_Remove;
   end Cancel_Drag;

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
      Cancel_Continuous_Scrolling (Self);
      Free_Smart_Guides (Self);

      if Self.Model /= null then
         Self.Set_Details (Details, Event);

         if Details.Event_Type = Key_Press then
            return False;
         elsif Details.Event_Type = Button_Press then
            Cancel_Inline_Editing (Self);
            Self.Grab_Focus;
         end if;

         if Event.The_Type = Gdk.Event.Button_Release
           and then Self.In_Drag
         then
            --  Validate the position of items and recompute all links,
            --  not just the ones that moved.
            Self.Model.Refresh_Layout;
         end if;

         if Self.Item_Event (Details'Unchecked_Access) then
            Cancel_Drag (Self);

            if Details.Event_Type = Button_Press then
               Self.Last_Button_Press := Details;
            end if;

            return True;
         end if;
      end if;

      Cancel_Drag (Self);
      return False;
   end On_Button_Event;

   -----------------------
   -- Get_Toplevel_Item --
   -----------------------

   function Get_Toplevel_Item
     (Self : not null access Abstract_Item_Record'Class)
      return Abstract_Item
   is
      Result : Abstract_Item := Abstract_Item (Self);
      P      : Abstract_Item;
   begin
      loop
         P := Parent (Result);
         exit when P = null;
         Result := P;
      end loop;
      return Result;
   end Get_Toplevel_Item;

   ------------------------------------
   -- Copy_Selected_To_Dragged_Items --
   ------------------------------------

   procedure Copy_Selected_To_Dragged_Items
     (Self  : not null access Canvas_View_Record'Class;
      Force : access Abstract_Item_Record'Class)
   is
      use Item_Sets;
      C    : Item_Sets.Cursor := Self.Model.Selection.First;
      Item : Abstract_Item;
      P    : Gtkada.Style.Point;
   begin
      Self.Dragged_Items.Clear;

      while Has_Element (C) loop
         Item := Element (C).Get_Toplevel_Item;
         if not Item.Is_Link then
            P := Item.Position;
            Self.Dragged_Items.Include
              (Item, (Item => Item, Pos  => (X => P.X, Y => P.Y)));
         end if;

         Next (C);
      end loop;

      if Force /= null and then not Force.Is_Link then
         Item := Force.Get_Toplevel_Item;
         P := Item.Position;
         Self.Dragged_Items.Include
           (Item, (Item => Item, Pos  => (X => P.X, Y => P.Y)));
      end if;
   end Copy_Selected_To_Dragged_Items;

   ----------------------------
   -- On_Motion_Notify_Event --
   ----------------------------

   function On_Motion_Notify_Event
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event_Motion) return Boolean
   is
      Self    : constant Canvas_View := Canvas_View (View);
      Details : aliased Canvas_Event_Details;
      Dx, Dy  : Gdouble;
      Dummy   : Boolean;
   begin
      if Self.Model /= null
        and then Self.Last_Button_Press.Allowed_Drag_Area /= No_Drag_Allowed
        and then Self.Get_Child = null   --  no inline editing
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
               Self.Grab_Add;

               --  ??? Should add all selected items
               if Details.Toplevel_Item /= null
                 and then not Details.Toplevel_Item.Is_Link
               then
                  Copy_Selected_To_Dragged_Items
                    (Self, Force => Details.Toplevel_Item);
                  Prepare_Smart_Guides (Self);
               end if;
            end if;
         end if;

         --  Whether we were already in a drag or just started

         if Self.In_Drag then
            Details            := Self.Last_Button_Press;
            Details.Event_Type := In_Drag;
            Details.State      := Event.State and Get_Default_Mod_Mask;
            Details.Root_Point := (Event.X_Root, Event.Y_Root);
            Details.M_Point    :=
              Self.Window_To_Model ((X => Event.X, Y => Event.Y));
            Dummy := Self.Item_Event (Details'Unchecked_Access);
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
         Disconnect (Self.Model, Self.Id_Selection_Changed);
         Disconnect (Self.Model, Self.Id_Item_Destroyed);
         Unref (Self.Model);
      end if;

      Self.Model := Canvas_Model (Model);

      if Self.Model /= null then
         Ref (Self.Model);
         Self.Id_Layout_Changed := Model.On_Layout_Changed
            (On_Layout_Changed_For_View'Access, Self);
         Self.Id_Selection_Changed := Model.On_Selection_Changed
           (On_Selection_Changed_For_View'Access, Self);
         Self.Id_Item_Contents_Changed := Model.On_Item_Contents_Changed
            (On_Item_Contents_Changed_For_View'Access, Self);
         Self.Id_Item_Destroyed :=
           Model.On_Item_Destroyed (On_Item_Destroyed_For_View'Access, Self);
      end if;

      if Self.Model /= null and then Self.Model.Layout = null then
         Self.Model.Layout := Self.Layout;  --  needed for layout
         Ref (Self.Model.Layout);
         Self.Model.Refresh_Layout;
      else
         Set_Adjustment_Values (Self);
         Self.Queue_Draw;
      end if;

      Self.Viewport_Changed;
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
   -- On_View_Realize --
   ---------------------

   procedure On_View_Realize (Widget : System.Address) is
      W          : constant Gtk_Widget :=
        Gtk_Widget (Get_User_Data_Or_Null (Widget));
      Allocation : Gtk_Allocation;
      Window     : Gdk_Window;
      Attr       : Gdk.Window_Attr.Gdk_Window_Attr;
      Mask       : Gdk_Window_Attributes_Type;

   begin
      if not W.Get_Has_Window then
         Inherited_Realize (View_Class_Record, W);
      else
         W.Set_Realized (True);
         W.Get_Allocation (Allocation);

         Gdk_New
           (Attr,
            Window_Type => Gdk.Window.Window_Child,
            X           => Allocation.X,
            Y           => Allocation.Y,
            Width       => Allocation.Width,
            Height      => Allocation.Height,
            Wclass      => Gdk.Window.Input_Output,
            Visual      => W.Get_Visual,
            Event_Mask  => W.Get_Events or Exposure_Mask);
         Mask := Wa_X or Wa_Y or Wa_Visual;

         Gdk_New (Window, W.Get_Parent_Window, Attr, Mask);
         Register_Window (W, Window);
         W.Set_Window (Window);
         Get_Style_Context (W).Set_Background (Window);

         --  See also handler for size_allocate, which moves the window to its
         --  proper location.
      end if;
   end On_View_Realize;

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
      Set_Default_Realize_Handler (Self, On_View_Realize'Access);
   end View_Class_Init;

   -------------------
   -- View_Get_Type --
   -------------------

   function View_Get_Type return Glib.GType is
      Info : GInterface_Info_Access;
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Bin.Get_Type,
         Signals      => View_Signals,
         Class_Record => View_Class_Record'Access,
         Type_Name    => "GtkadaCanvasView",
         Parameters   => (1 => (1 => GType_None),
                          2 => (1 => GType_Pointer),
                          3 => (1 => GType_Pointer),
                          4 => (1 => GType_Pointer)),
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

      Self.Viewport_Changed;
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
      Cancel_Continuous_Scrolling (S);
      Terminate_Animation (S);

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

      --  We might have an inline widget, which we need to draw.
      if Self.Inline_Edit.Item /= null then
         if Inherited_Draw
           (View_Class_Record,
            Widget => Self,
            Cr     => Cr)
         then
            return 1;
         else
            return 0;
         end if;
      else
         return 1;
      end if;

   exception
      when E : others =>
         Process_Exception (E);
         return 0;
   end On_View_Draw;

   -----------------------------
   -- Move_Inline_Edit_Widget --
   -----------------------------

   procedure Move_Inline_Edit_Widget
      (Self : not null access Canvas_View_Record'Class)
   is
      Edit   : Gtk_Widget;
      Box    : View_Rectangle;
      SAlloc : Gtk_Allocation;
      WMin, WNat, HMin, HNat : Gint;
   begin
      if Self.Inline_Edit.Item /= null then
         Edit := Self.Get_Child;
         Box := Self.Model_To_View (Self.Inline_Edit.Item.Model_Bounding_Box);

         --  SAlloc is relative to the view, so we should not add Alloc.{X,Y}
         SAlloc.X := Gint (Box.X);
         SAlloc.Y := Gint (Box.Y);

         Edit.Get_Preferred_Height (HMin, HNat);
         SAlloc.Height := Gint'Max (HMin, Gint (Box.Height));

         Edit.Get_Preferred_Width_For_Height (SAlloc.Height, WMin, WNat);
         SAlloc.Width := Gint'Max (WMin, Gint (Box.Width));

         Edit.Size_Allocate (SAlloc);
      end if;
   end Move_Inline_Edit_Widget;

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
      Self.Set_Allocation (SAlloc);
      Set_Adjustment_Values (Self);

      if Self.Get_Realized then
         if Self.Get_Has_Window then
            Move_Resize
              (Self.Get_Window, Alloc.X, Alloc.Y, Alloc.Width, Alloc.Height);
         end if;

         --  send_configure event ?
      end if;

      --  Are we in the middle of inline-editing ?
      Move_Inline_Edit_Widget (Self);

      if Self.Scale_To_Fit_Requested /= 0.0 then
         Self.Scale_To_Fit
           (Rect      => Self.Scale_To_Fit_Area,
            Max_Scale => Self.Scale_To_Fit_Requested);
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
      Model_P : Model_Point;
      P       : View_Point;
   begin
      if Item /= null then
         Model_P := Item.Item_To_Model ((0.0, 0.0));
      else
         Model_P := (0.0, 0.0);
      end if;

      P := Self.Model_To_View (Model_P);
      Translate (Cr, P.X, P.Y);
      Scale (Cr, Self.Scale, Self.Scale);
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
     (Self     : not null access Canvas_View_Record;
      Scale    : Gdouble := 1.0;
      Preserve : Model_Point := No_Point)
   is
      Box : Model_Rectangle;
      Old_Scale : constant Gdouble := Self.Scale;
      P   : Model_Point;
   begin
      if Preserve /= No_Point then
         P := Preserve;
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

   -----------------
   -- Set_Topleft --
   -----------------

   procedure Set_Topleft
     (Self         : not null access Canvas_View_Record;
      Topleft      : Model_Point) is
   begin
      Self.Topleft := Topleft;
      Self.Queue_Draw;
   end Set_Topleft;

   ---------------
   -- Center_On --
   ---------------

   procedure Center_On
     (Self         : not null access Canvas_View_Record;
      Center_On    : Model_Point;
      X_Pos, Y_Pos : Gdouble := 0.5;
      Duration     : Standard.Duration := 0.0)
   is
      Area : constant Model_Rectangle := Self.Get_Visible_Area;
      Pos  : constant Model_Point :=
        (Center_On.X - Area.Width * X_Pos,
         Center_On.Y - Area.Height * Y_Pos);
   begin
      Self.Scale_To_Fit_Requested := 0.0;

      if Duration = 0.0 then
         Self.Topleft := Pos;
         Self.Set_Adjustment_Values;
         Self.Queue_Draw;
      else
         Animate_Scroll (Self, Pos, Duration => Duration).Start (Self);
      end if;
   end Center_On;

   ----------------------
   -- Scroll_Into_View --
   ----------------------

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Item     : not null access Abstract_Item_Record'Class;
      Duration : Standard.Duration := 0.0)
   is
   begin
      Scroll_Into_View (Self, Item.Model_Bounding_Box, Duration);
   end Scroll_Into_View;

   ----------------------
   -- Scroll_Into_View --
   ----------------------

   procedure Scroll_Into_View
     (Self     : not null access Canvas_View_Record;
      Rect     : Model_Rectangle;
      Duration : Standard.Duration := 0.0)
   is
      Box  : Model_Rectangle := Rect;
      Area : Model_Rectangle := Self.Get_Visible_Area;
      Modified : Boolean := False;
      Margin   : constant Model_Coordinate := View_Margin * Self.Scale;
   begin
      Box.X := Box.X - Margin;
      Box.Y := Box.Y - Margin;
      Box.Width := Box.Width + Margin * 2.0;
      Box.Height := Box.Height + Margin * 2.0;

      if Box.X < Area.X then
         Area.X := Box.X;
         Modified := True;
      elsif Box.X + Box.Width > Area.X + Area.Width then
         Area.X := Gdouble'Min (Box.X, Box.X + Box.Width - Area.Width);
         Modified := True;
      end if;

      if Box.Y < Area.Y then
         Area.Y := Box.Y;
         Modified := True;
      elsif Box.Y + Box.Height > Area.Y + Area.Height then
         Area.Y := Gdouble'Min (Box.Y, Box.Y + Box.Height - Area.Height);
         Modified := True;
      end if;

      if Modified then
         Self.Center_On
           ((Area.X, Area.Y), X_Pos => 0.0, Y_Pos => 0.0,
            Duration => Duration);
      end if;
   end Scroll_Into_View;

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
      Rect      : Model_Rectangle := No_Rectangle;
      Min_Scale : Gdouble := 1.0 / 4.0;
      Max_Scale : Gdouble := 4.0;
      Duration  : Standard.Duration := 0.0)
   is
      Box     : Model_Rectangle;
      W, H, S : Gdouble;
      Alloc   : Gtk_Allocation;
      TL      : Model_Point;
      Wmin, Hmin : Gdouble;
   begin
      Self.Get_Allocation (Alloc);
      if Alloc.Width <= 1 then
         Self.Scale_To_Fit_Requested := Max_Scale;
         Self.Scale_To_Fit_Area := Rect;

      elsif Self.Model /= null then
         Self.Scale_To_Fit_Requested := 0.0;

         if Rect = No_Rectangle then
            Box := Self.Model.Bounding_Box;
         else
            Box := Rect;
         end if;

         if Box.Width /= 0.0 and then Box.Height /= 0.0 then
            W := Gdouble (Alloc.Width);
            H := Gdouble (Alloc.Height);

            --  The "-1.0" below compensates for rounding errors, since
            --  otherwise we are still seeing the scrollbar along the axis
            --  used to compute the scale.
            Wmin := (W - 2.0 * View_Margin - 1.0) / Box.Width;
            Hmin := (H - 2.0 * View_Margin - 1.0) / Box.Height;
            Wmin := Gdouble'Min (Wmin, Hmin);
            S := Gdouble'Min (Max_Scale, Wmin);
            S := Gdouble'Max (Min_Scale, S);
            TL :=
              (X => Box.X - (W / S - Box.Width) / 2.0,
               Y => Box.Y - (H / S - Box.Height) / 2.0);

            if Duration = 0.0 then
               Self.Scale := S;
               Self.Topleft := TL;
               Self.Set_Adjustment_Values;
               Self.Queue_Draw;

            else
               Animate_Scale (Self, S, Duration => Duration).Start (Self);
               Animate_Scroll (Self, TL, Duration).Start (Self);
            end if;
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

   function On_Viewport_Changed
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self, Signal_Viewport_Changed,
            Object_Callback.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self, Signal_Viewport_Changed,
            Object_Callback.To_Marshaller (Call), Slot);
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

   ----------------------------
   -- Inline_Editing_Started --
   ----------------------------

   procedure Inline_Editing_Started
     (Self   : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      Abstract_Item_Emit
        (Self, Signal_Inline_Editing_Started & ASCII.NUL,
         Abstract_Item (Item));
   end Inline_Editing_Started;

   -------------------------------
   -- On_Inline_Editing_Started --
   -------------------------------

   function On_Inline_Editing_Started
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self, Signal_Inline_Editing_Started,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self, Signal_Inline_Editing_Started,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Inline_Editing_Started;

   -----------------------------
   -- Inline_Editing_Finished --
   -----------------------------

   procedure Inline_Editing_Finished
     (Self  : not null access Canvas_View_Record'Class;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      Abstract_Item_Emit
        (Self, Signal_Inline_Editing_Finished & ASCII.NUL,
         Abstract_Item (Item));
   end Inline_Editing_Finished;

   --------------------------------
   -- On_Inline_Editing_Finished --
   --------------------------------

   function On_Inline_Editing_Finished
     (Self : not null access Canvas_View_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class; Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self, Signal_Inline_Editing_Finished,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self, Signal_Inline_Editing_Finished,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
   end On_Inline_Editing_Finished;

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

      C := (Cr => Cr, Layout => Self.Layout, View => Canvas_View (Self));

      Save (Cr);
      Self.Set_Transform (Cr);
      Self.Draw_Internal (C, A);
      Restore (Cr);
   end Refresh;

   ----------------------
   -- Draw_As_Selected --
   ----------------------

   overriding procedure Draw_As_Selected
     (Self            : not null access Canvas_Item_Record;
      Context         : Draw_Context) is
   begin
      if Context.View /= null then
         Canvas_Item (Self).Draw_Outline
           (Style   => Context.View.Selection_Style,
            Context => Context);
      end if;
      Canvas_Item_Record'Class (Self.all).Draw (Context);
   end Draw_As_Selected;

   ------------------
   -- Draw_Outline --
   ------------------

   procedure Draw_Outline
     (Self    : not null access Canvas_Item_Record;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context)
   is
      Box : constant Item_Rectangle :=
        Canvas_Item_Record'Class (Self.all).Bounding_Box;
      Margin_Pixels : constant View_Coordinate := 2.0;
      Margin        : Model_Coordinate;
   begin
      if Context.View /= null then
         Margin := Margin_Pixels / Context.View.Scale;
         Style.Draw_Rect
           (Context.Cr,
            (Box.X - Margin, Box.Y - Margin),
            Box.Width + 2.0 * Margin,
            Box.Height + 2.0 * Margin);
      end if;
   end Draw_Outline;

   --------------------------
   -- Size_Above_Threshold --
   --------------------------

   function Size_Above_Threshold
     (Self : not null access Abstract_Item_Record'Class;
      View : access Canvas_View_Record'Class) return Boolean
   is
      R   : View_Rectangle;
      Threshold : constant Gdouble := Self.Get_Visibility_Threshold;
   begin
      if Threshold = Gdouble'Last then
         --  Always hidden
         return False;
      elsif Threshold > 0.0 and then View /= null then
         R := View.Model_To_View (Self.Model_Bounding_Box);
         if R.Width < Threshold or else R.Height < Threshold then
            return False;
         end if;
      end if;
      return True;
   end Size_Above_Threshold;

   -----------------------------
   -- Translate_And_Draw_Item --
   -----------------------------

   procedure Translate_And_Draw_Item
     (Self          : not null access Abstract_Item_Record'Class;
      Context       : Draw_Context;
      As_Outline    : Boolean := False;
      Outline_Style : Drawing_Style := No_Drawing_Style)
   is
      Pos : Gtkada.Style.Point;
   begin
      if not Size_Above_Threshold (Self, Context.View) then
         return;
      end if;

      Save (Context.Cr);
      Pos := Self.Position;
      Translate (Context.Cr, Pos.X, Pos.Y);

      if As_Outline then
         Self.Draw_Outline (Outline_Style, Context);
      elsif Context.View /= null
        and then Context.View.Model /= null
        and then Context.View.Model.Is_Selected (Self)
      then
         Self.Draw_As_Selected (Context);
      else
         Self.Draw (Context);
      end if;

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
      S  : Item_Sets.Set;

      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class);
      procedure Draw_Item
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         --  If the item is not displayed explicitly afterwards.
         if not Self.In_Drag
           or else not S.Contains (Abstract_Item (Item))
         then
            Translate_And_Draw_Item (Item, Context);
         end if;
      end Draw_Item;

      procedure Add_To_Set (Item : not null access Abstract_Item_Record'Class);
      procedure Add_To_Set
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         S.Include (Abstract_Item (Item));
      end Add_To_Set;

      use Item_Drag_Infos, Item_Sets;
      C  : Item_Drag_Infos.Cursor;
      C2 : Item_Sets.Cursor;
   begin
      if Self.Model /= null then
         --  We must always draw the selected items and their links explicitly
         --  (since the model might not have been updated yet if we are during
         --  an automatic scrolling for instance, using a RTree).

         if Self.In_Drag then
            C := Self.Dragged_Items.First;
            while Has_Element (C) loop
               S.Include (Element (C).Item);  --  toplevel items
               Next (C);
            end loop;
            Self.Model.For_Each_Link (Add_To_Set'Access, From_Or_To => S);
         end if;

         --  Draw the active smart guides if needed
         if Self.In_Drag
           and then Self.Last_Button_Press.Allow_Snapping
           and then Self.Snap.Smart_Guides
           and then not Self.Dragged_Items.Is_Empty
         then
            Draw_Visible_Smart_Guides
              (Self, Context, Element (Self.Dragged_Items.First).Item);
         end if;

         Self.Model.For_Each_Item
           (Draw_Item'Access, In_Area => Area, Filter => Kind_Link);
         Self.Model.For_Each_Item
           (Draw_Item'Access, In_Area => Area, Filter => Kind_Item);

         if Self.In_Drag then
            C2 := S.First;
            while Has_Element (C2) loop
               Translate_And_Draw_Item (Element (C2), Context);
               Next (C2);
            end loop;
         end if;
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
      if From.Is_Link then
         F.Toplevel_Side := No_Clipping;
      end if;

      if To.Is_Link then
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
      if Size_Above_Threshold (Self, Context.View) then
         Gtkada.Canvas_View.Links.Draw_Link (Self, Context, Selected => False);
      end if;
   end Draw;

   ----------------------
   -- Draw_As_Selected --
   ----------------------

   procedure Draw_As_Selected
     (Self    : not null access Canvas_Link_Record;
      Context : Draw_Context) is
   begin
      if Size_Above_Threshold (Self, Context.View) then
         Gtkada.Canvas_View.Links.Draw_Link (Self, Context, Selected => True);
      end if;
   end Draw_As_Selected;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (Self    : not null access Canvas_Link_Record;
      Point   : Item_Point;
      Context : Draw_Context) return Boolean
   is
      Tolerance : constant Gdouble := 10.0;
   begin
      if Self.Points = null then
         return False;
      end if;

      Save (Context.Cr);
      Set_Line_Width (Context.Cr, Tolerance);

      return Prepare_Path (Self, Context)
        and then In_Stroke (Context.Cr, Point.X, Point.Y);
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
     (Self : not null access Canvas_Link_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
   begin
      Unchecked_Free (Self.Points);
      Unchecked_Free (Self.Waypoints);
      Destroy_And_Free (Abstract_Item (Self.Label), In_Model);
      Destroy_And_Free (Abstract_Item (Self.Label_From), In_Model);
      Destroy_And_Free (Abstract_Item (Self.Label_To), In_Model);

      --  Can't call destroy from the interface, this results in infinite loop
      --  Abstract_Item_Record (Self.all).Destroy (In_Model); --  inherited
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
   end Add;

   -----------------------
   -- On_Item_Destroyed --
   -----------------------

   function On_Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
      Id : Handler_Id;
   begin
      if Slot = null then
         Id := Object_Callback.Connect
           (Self, Signal_Item_Destroyed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         Id := Object_Callback.Object_Connect
           (Self, Signal_Item_Destroyed,
            Abstract_Item_Marshallers.To_Marshaller (Call), Slot);
      end if;
      return Id;
   end On_Item_Destroyed;

   --------------------
   -- Item_Destroyed --
   --------------------

   procedure Item_Destroyed
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class)
   is
   begin
      Abstract_Item_Emit
        (Self, Signal_Item_Destroyed & ASCII.NUL, Abstract_Item (Item));
   end Item_Destroyed;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : not null access Canvas_Model_Record;
      Set  : Item_Sets.Set)
   is
      use Item_Sets;
      C  : Item_Sets.Cursor := Set.First;
   begin
      while Has_Element (C) loop
         Canvas_Model_Record'Class (Self.all).Remove (Element (C));
         Next (C);
      end loop;
   end Remove;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
      To_Remove : Item_Sets.Set;
   begin
      Self.Include_Related_Items (Item, To_Remove);
      Remove (Self, To_Remove);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : not null access List_Canvas_Model_Record;
      Set  : Item_Sets.Set)
   is
      use Item_Sets, Items_Lists;
      C2   : Item_Sets.Cursor := Set.First;
      It   : Abstract_Item;
      C    : Items_Lists.Cursor;
   begin
      --  First pass: remove the items from the list of items. This means
      --  that when we later destroy the items (and thus in the case of
      --  container_item we remove the link to their children), we will not
      --  see these items again.

      while Has_Element (C2) loop
         It := Element (C2);
         Next (C2);
         Self.Remove_From_Selection (It);

         C := Self.Items.Find (It);
         if Has_Element (C) then
            Self.Items.Delete (C);
         end if;
      end loop;

      --  Now destroy the items

      C2 := Set.First;
      while Has_Element (C2) loop
         It := Element (C2);
         Next (C2);
         Destroy_And_Free (It, In_Model => Self);
      end loop;
   end Remove;

   ---------------------------
   -- Include_Related_Items --
   ---------------------------

   procedure Include_Related_Items
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set)
   is
      procedure Internal (C : not null access Abstract_Item_Record'Class);
      procedure Internal (C : not null access Abstract_Item_Record'Class) is
      begin
         if C.all in Canvas_Link_Record'Class
           and then (Canvas_Link (C).From = Abstract_Item (Item)
                     or else Canvas_Link (C).To = Abstract_Item (Item))
         then
            Include_Related_Items (Self, C, Set);
         end if;
      end Internal;

   begin
      Self.For_Each_Item (Internal'Access, Filter => Kind_Link);

      --  Removing the container items will call their destroy, and therefore
      --  remove all links to their children.

      Set.Include (Abstract_Item (Item));
   end Include_Related_Items;

   ----------
   -- From --
   ----------

   procedure From
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set)
   is
      procedure Internal (C : not null access Abstract_Item_Record'Class);
      procedure Internal (C : not null access Abstract_Item_Record'Class) is
      begin
         if C.all in Canvas_Link_Record'Class
           and then Canvas_Link (C).From = Abstract_Item (Item)
         then
            declare
               Cur : constant Abstract_Item := Canvas_Link (C).Get_To;
            begin
               if not Set.Contains (Abstract_Item (C)) then
                  Set.Include (Abstract_Item (C));
                  if not Set.Contains (Cur) then
                     Set.Include (Cur);
                     To (Self, Cur, Set);
                     From (Self, Cur, Set);
                  end if;
               end if;
            end;
         end if;
      end Internal;
   begin
      Self.For_Each_Item (Internal'Access, Filter => Kind_Link);
   end From;

   --------
   -- To --
   --------

   procedure To
     (Self : not null access Canvas_Model_Record'Class;
      Item : not null access Abstract_Item_Record'Class;
      Set  : in out Item_Sets.Set)
   is
      procedure Internal (C : not null access Abstract_Item_Record'Class);
      procedure Internal (C : not null access Abstract_Item_Record'Class) is
      begin
         if C.all in Canvas_Link_Record'Class
           and then Canvas_Link (C).To = Abstract_Item (Item)
         then
            declare
               Cur : constant Abstract_Item := Canvas_Link (C).Get_From;
            begin
               if not Set.Contains (Abstract_Item (C)) then
                  Set.Include (Abstract_Item (C));
                  if not Set.Contains (Cur) then
                     Set.Include (Cur);
                     To (Self, Cur, Set);
                     From (Self, Cur, Set);
                  end if;
               end if;
            end;
         end if;
      end Internal;
   begin
      Self.For_Each_Item (Internal'Access, Filter => Kind_Link);
   end To;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self : not null access List_Canvas_Model_Record)
   is
      use Items_Lists;
      Items : constant Items_Lists.List := Self.Items;  --  a copy of the list
      C     : Items_Lists.Cursor := Items.First;
   begin
      --  More efficient to clear the list first, so that 'Remove' finds no
      --  related link (since we are going to free them anyway).
      Self.Items.Clear;

      while Has_Element (C) loop
         Canvas_Model_Record'Class (Self.all).Remove (Element (C));
         Next (C);
      end loop;
      Canvas_Model_Record'Class (Self.all).Refresh_Layout;
   end Clear;

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
      Selected_Only : Boolean := False;
      Filter        : Item_Kind_Filter := Kind_Any;
      In_Area       : Model_Rectangle := No_Rectangle)
   is
      use Items_Lists;
      C    : Items_Lists.Cursor := Self.Items.First;
      Item : Abstract_Item;
   begin
      while Has_Element (C) loop
         Item := Element (C);

         --  ??? Might not work when the Callback removes the item, which in
         --  turn removes a link which might happen to be the next element
         --  we were pointing to.
         Next (C);

         if (Filter = Kind_Any
             or else (Filter = Kind_Item and then not Item.Is_Link)
             or else (Filter = Kind_Link and then Item.Is_Link))
           and then
             (not Selected_Only
              or else List_Canvas_Model (Self).Is_Selected (Item))
           and then
             (In_Area = No_Rectangle
              or else Intersects (In_Area, Item.Model_Bounding_Box))
         then
            Callback (Item);
         end if;
      end loop;
   end For_Each_Item;

   ----------------
   -- Raise_Item --
   ----------------

   overriding procedure Raise_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Items.First;
   begin
      while Has_Element (C) loop
         if Element (C) = Abstract_Item (Item) then
            if C /= Self.Items.Last then
               Self.Items.Delete (C);
               Self.Items.Append (Abstract_Item (Item));
               List_Canvas_Model_Record'Class (Self.all).Layout_Changed;
            end if;

            return;
         end if;
         Next (C);
      end loop;
   end Raise_Item;

   ----------------
   -- Lower_Item --
   ----------------

   procedure Lower_Item
     (Self : not null access List_Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Items.First;
   begin
      if not Has_Element (C)
        or else Element (C) = Abstract_Item (Item)
      then
         return;   --  nothing to do
      end if;

      Next (C);

      while Has_Element (C) loop
         if Element (C) = Abstract_Item (Item) then
            Self.Items.Delete (C);
            Self.Items.Prepend (Abstract_Item (Item));
            List_Canvas_Model_Record'Class (Self.all).Layout_Changed;
            return;
         end if;
         Next (C);
      end loop;
   end Lower_Item;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Self  : not null access Container_Item_Record;
      Style : Drawing_Style) is
   begin
      Self.Style := Style;
   end Set_Style;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Self : not null access Container_Item_Record) return Drawing_Style is
   begin
      return Self.Style;
   end Get_Style;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Self : not null access Canvas_Link_Record) return Drawing_Style is
   begin
      return Self.Style;
   end Get_Style;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
     (Self  : not null access Canvas_Link_Record;
      Style : Drawing_Style) is
   begin
      Self.Style := Style;
   end Set_Style;

   ----------------
   -- Get_Points --
   ----------------

   function Get_Points
     (Self : not null access Canvas_Link_Record)
      return Item_Point_Array_Access is
   begin
      return Self.Points;
   end Get_Points;

   -------------------
   -- Set_Waypoints --
   -------------------

   procedure Set_Waypoints
     (Self     : not null access Canvas_Link_Record;
      Points   : Item_Point_Array;
      Relative : Boolean := False) is
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

      if Self.From.Is_Link
         and then Canvas_Link (Self.From).Points = null
      then
         Canvas_Link (Self.From).Refresh_Layout (Context);
      end if;

      if Self.To.Is_Link
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

   ------------------------
   -- Get_Selected_Items --
   ------------------------

   function Get_Selected_Items
     (Self : not null access Canvas_Model_Record) return Item_Sets.Set is
   begin
      return Self.Selection;
   end Get_Selected_Items;

   -------------------
   -- For_Each_Link --
   -------------------

   procedure For_Each_Link
     (Self       : not null access Canvas_Model_Record;
      Callback   : not null access procedure
        (Item : not null access Abstract_Item_Record'Class);
      From_Or_To : Item_Sets.Set)
   is
      function Matches
        (It : not null access Abstract_Item_Record'Class) return Boolean;
      pragma Inline (Matches);
      --  Whether the corresponding link should be returned

      function Matches
        (It : not null access Abstract_Item_Record'Class) return Boolean
      is
         L : Canvas_Link;
      begin
         --  A custom link ? We don't know how to check its ends, so always
         --  return it.
         if It.all not in Canvas_Link_Record'Class then
            return True;
         end if;

         L := Canvas_Link (It);

         return From_Or_To.Contains (L.From.Get_Toplevel_Item)
           or else From_Or_To.Contains (L.To.Get_Toplevel_Item)
           or else (L.From.Is_Link and then Matches (L.From))
           or else (L.To.Is_Link and then Matches (L.To));
      end Matches;

      procedure Local (It : not null access Abstract_Item_Record'Class);
      procedure Local (It : not null access Abstract_Item_Record'Class) is
      begin
         if Matches (It) then
            Callback (It);
         end if;
      end Local;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Local'Access, Filter => Kind_Link);
   end For_Each_Link;

   -------------------------
   -- Refresh_Link_Layout --
   -------------------------

   procedure Refresh_Link_Layout
     (Model : not null access Canvas_Model_Record'Class;
      Items : Item_Drag_Infos.Map := Item_Drag_Infos.Empty_Map)
   is
      S : Item_Sets.Set;
      Context : constant Draw_Context :=
        (Cr => <>, Layout => Model.Layout, View => null);

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
         Unchecked_Free (Canvas_Link_Record'Class (It.all).Points);
      end Reset_Link_Layout;

      procedure Do_Link_Layout
        (It : not null access Abstract_Item_Record'Class)
      is
         Link : constant Canvas_Link := Canvas_Link (It);
      begin
         if Link.Points = null then
            Link.Refresh_Layout (Context);
         end if;
      end Do_Link_Layout;

      use Item_Drag_Infos;
      C : Item_Drag_Infos.Cursor;

   begin
      --  To properly do the layout of links, we must first compute the
      --  layout of any item they are linked to, including other links.
      --  So we do the following:
      --     reset all previous layout computation
      --     when we layout a link, we first layout its end

      if Items.Is_Empty then
         Model.For_Each_Item
           (Reset_Link_Layout'Access, Filter => Kind_Link);
         Model.For_Each_Item (Do_Link_Layout'Access, Filter => Kind_Link);

      else
         C := Items.First;
         while Has_Element (C) loop
            S.Include (Element (C).Item);  --  toplevel items
            Next (C);
         end loop;

         Model.For_Each_Link (Reset_Link_Layout'Access, From_Or_To => S);
         Model.For_Each_Link (Do_Link_Layout'Access, From_Or_To => S);
      end if;
   end Refresh_Link_Layout;

   -------------------
   -- Build_Context --
   -------------------

   function Build_Context
     (Self : not null access Canvas_View_Record'Class)
      return Draw_Context
   is
   begin
      return (Cr => <>, Layout => Self.Layout, View => Canvas_View (Self));
   end Build_Context;

   --------------------
   -- Refresh_Layout --
   --------------------

   procedure Refresh_Layout
     (Self        : not null access Canvas_Model_Record;
      Send_Signal : Boolean := True)
   is
      Context : constant Draw_Context :=
        (Cr => <>, Layout => Self.Layout, View => null);

      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class);
      procedure Do_Container_Layout
        (Item : not null access Abstract_Item_Record'Class) is
      begin
         Item.Refresh_Layout (Context);
      end Do_Container_Layout;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item
        (Do_Container_Layout'Access, Filter => Kind_Item);
      Refresh_Link_Layout (Self);

      if Send_Signal then
         Canvas_Model_Record'Class (Self.all).Layout_Changed;
      end if;
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
         --  topmost items always occur later in the list.
         if Item.Contains (Model_To_Item (Item, Point), Context) then
            Found := Abstract_Item (Item);
         end if;
      end Check_Item;

   begin
      Canvas_Model_Record'Class (Self.all).For_Each_Item (Check_Item'Access);
      return Found;
   end Toplevel_Item_At;

   ----------------------
   -- Toplevel_Item_At --
   ----------------------

   overriding function Toplevel_Item_At
     (Self    : not null access List_Canvas_Model_Record;
      Point   : Model_Point;
      Context : Draw_Context) return Abstract_Item
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Items.Last;
      Item : Abstract_Item;
   begin
      while Has_Element (C) loop
         Item := Element (C);
         if Item.Contains (Model_To_Item (Item, Point), Context) then
            return Item;
         end if;
         Previous (C);
      end loop;
      return null;
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

   ------------------
   -- Is_Invisible --
   ------------------

   overriding function Is_Invisible
     (Self : not null access Container_Item_Record)
      return Boolean
   is
   begin
      return Self.Style.Get_Stroke = Null_RGBA
        and then Self.Style.Get_Fill = Null_Pattern;
   end Is_Invisible;

   ------------------------------
   -- Set_Visibility_Threshold --
   ------------------------------

   overriding procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Link_Record;
      Threshold : Gdouble)
   is
   begin
      Self.Visibility_Threshold := Threshold;
   end Set_Visibility_Threshold;

   ------------------------------
   -- Get_Visibility_Threshold --
   ------------------------------

   overriding function Get_Visibility_Threshold
     (Self : not null access Canvas_Link_Record) return Gdouble is
   begin
      return Self.Visibility_Threshold;
   end Get_Visibility_Threshold;

   ------------------------------
   -- Set_Visibility_Threshold --
   ------------------------------

   procedure Set_Visibility_Threshold
     (Self      : not null access Canvas_Item_Record;
      Threshold : Gdouble)
   is
   begin
      Self.Visibility_Threshold := Threshold;
   end Set_Visibility_Threshold;

   ------------------------------
   -- Get_Visibility_Threshold --
   ------------------------------

   function Get_Visibility_Threshold
     (Self : not null access Canvas_Item_Record) return Gdouble is
   begin
      return Self.Visibility_Threshold;
   end Get_Visibility_Threshold;

   ----------
   -- Show --
   ----------

   procedure Show (Self : not null access Abstract_Item_Record'Class) is
   begin
      Self.Set_Visibility_Threshold (Threshold => 0.0);
   end Show;

   ----------
   -- Hide --
   ----------

   procedure Hide (Self : not null access Abstract_Item_Record'Class) is
   begin
      Self.Set_Visibility_Threshold (Threshold => Gdouble'Last);
   end Hide;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self     : not null access Container_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
      To_Remove : Item_Sets.Set;

      procedure Do_Child (C : not null access Container_Item_Record'Class);
      procedure Do_Child (C : not null access Container_Item_Record'Class) is
      begin
         --  Remove the children and their links
         In_Model.Include_Related_Items (C, To_Remove);
      end Do_Child;

   begin
      if not Self.Children.Is_Empty then
         Container_Item_Record'Class (Self.all).For_Each_Child
            (Do_Child'Access, Recursive => False);
         Self.Children.Clear;
         Remove (In_Model, To_Remove);
         In_Model.Refresh_Layout;
      end if;
   end Clear;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Container_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Self.Clear (In_Model);
      Canvas_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Self     : not null access Container_Item_Record'Class;
      Child    : not null access Container_Item_Record'Class;
      Align    : Alignment_Style := Align_Start;
      Pack_End : Boolean := False;
      Margin   : Margins := No_Margins;
      Float    : Boolean := False;
      Overflow : Overflow_Style := Overflow_Prevent) is
   begin
      Child.Margin   := Margin;
      Child.Parent   := Container_Item (Self);
      Child.Float    := Float;
      Child.Overflow := Overflow;
      Child.Align    := Align;
      Child.Pack_End := Pack_End;
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
        (Child : not null access Container_Item_Record'Class);
      Recursive : Boolean := False)
   is
      use Items_Lists;
      C : Items_Lists.Cursor := Self.Children.First;
   begin
      while Has_Element (C) loop
         if Recursive then
            Container_Item (Element (C)).For_Each_Child (Callback, Recursive);
         end if;
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
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style.Point)
   is
   begin
      Container_Item_Record'Class (Self.all).Set_Position (Pos, 0.0, 0.0);
   end Set_Position;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Self     : not null access Container_Item_Record;
      Pos      : Gtkada.Style.Point := (Gdouble'First, Gdouble'First);
      Anchor_X : Percent;
      Anchor_Y : Percent)
   is
   begin
      Self.Computed_Position := Pos;
      Self.Anchor_X := Anchor_X;
      Self.Anchor_Y := Anchor_Y;
      Canvas_Item_Record (Self.all).Set_Position (Pos);  --  inherited
   end Set_Position;

   ---------------------
   -- Set_Width_Range --
   ---------------------

   procedure Set_Width_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size) is
   begin
      Self.Min_Width := Min;
      Self.Max_Width := Max;
   end Set_Width_Range;

   ----------------------
   -- Set_Height_Range --
   ----------------------

   procedure Set_Height_Range
     (Self     : not null access Container_Item_Record;
      Min, Max : Size := Auto_Size) is
   begin
      Self.Min_Height := Min;
      Self.Max_Height := Max;
   end Set_Height_Range;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
      (Self : not null access Container_Item_Record;
       Width, Height : Size := Auto_Size) is
   begin
      if Width = Auto_Size or else Width = Fit_Size then
         Self.Min_Width := (Unit_Pixels, 1.0);
         Self.Max_Width := Width;
      else
         Self.Min_Width := Width;
         Self.Max_Width := Fixed_Size;
      end if;

      if Height = Auto_Size or else Height = Fit_Size then
         Self.Min_Height := (Unit_Pixels, 1.0);
         Self.Max_Height := Height;
      else
         Self.Min_Height := Height;
         Self.Max_Height := Fixed_Size;
      end if;
   end Set_Size;

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

               Tmp2 := Tmp2 + Child.Margin.Top + Child.Margin.Bottom;
               Self.Height := Model_Coordinate'Max (Self.Height, Tmp2);

               if not Child.Float then
                  --  The lowest point so far
                  Tmp := Self.Height;
               end if;

            when Horizontal_Stack =>
               case Child.Overflow is
                  when Overflow_Prevent =>
                     Self.Height := Model_Coordinate'Max
                        (Child.Height + Child.Margin.Top
                         + Child.Margin.Bottom,
                         Self.Height);
                  when Overflow_Hide =>
                     null;
               end case;

               if Child.Position.X /= Gdouble'First then
                  Tmp2 := Child.Position.X + Child.Width;
               else
                  Tmp2 := Tmp + Child.Width;
               end if;

               Tmp2 := Tmp2 + Child.Margin.Left + Child.Margin.Right;
               Self.Width := Model_Coordinate'Max (Self.Width, Tmp2);

               if not Child.Float then
                  Tmp := Self.Width;
               end if;
         end case;

         Next (C);
      end loop;

      --  The previous computation was for the standalone / ideal size for
      --  the children and Self.
      --  But we now need to take the size constraints into account. When
      --  they are given in pixels, we apply them immediately. When they are
      --  given in units relative to the parent's size, we can only apply
      --  them in Size_Allocate.

      if Self.Min_Width.Unit = Unit_Pixels then
         if Self.Max_Width = Fixed_Size then
            Self.Width := Self.Min_Width.Length;
         else
            Self.Width := Model_Coordinate'Max
               (Self.Width, Self.Min_Width.Length);
         end if;
      end if;

      if Self.Max_Width.Unit = Unit_Pixels
         and then Self.Max_Width /= Fixed_Size
      then
         Self.Width := Model_Coordinate'Min
            (Self.Width, Self.Max_Width.Length);
      end if;

      if Self.Min_Height.Unit = Unit_Pixels then
         if Self.Max_Height = Fixed_Size then
            Self.Height := Self.Min_Height.Length;
         else
            Self.Height := Model_Coordinate'Max
               (Self.Height, Self.Min_Height.Length);
         end if;
      end if;

      if Self.Max_Height.Unit = Unit_Pixels
         and then Self.Max_Height /= Fixed_Size
      then
         Self.Height := Model_Coordinate'Min
            (Self.Height, Self.Max_Height.Length);
      end if;
   end Size_Request;

   ----------------------
   -- Set_Size_Request --
   ----------------------

   procedure Set_Size_Request
     (Self    : not null access Container_Item_Record;
      Width, Height : Gdouble := -1.0)
   is
   begin
      if Width >= 0.0 then
         Self.Width := Width;
      end if;

      if Height >= 0.0 then
         Self.Height := Height;
      end if;
   end Set_Size_Request;

   -------------------
   -- Size_Allocate --
   -------------------

   procedure Size_Allocate
     (Self  : not null access Container_Item_Record)
   is
      use Items_Lists;
      C       : Items_Lists.Cursor := Self.Children.First;
      Child   : Container_Item;
      Tmp     : Model_Coordinate := 0.0;
      Tmp_End : Model_Coordinate;
   begin
      case Self.Layout is
         when Vertical_Stack   => Tmp_End := Self.Height;
         when Horizontal_Stack => Tmp_End := Self.Width;
      end case;

      while Has_Element (C) loop
         Child := Container_Item (Element (C));

         --  Apply size constraints when they are proportional to the
         --  parent's size.

         case Child.Min_Width.Unit is
            when Unit_Auto | Unit_Fit => null; --  Only relevant for Max_Width
            when Unit_Pixels => null; --  Taken care of in Size_Request
            when Unit_Percent =>
               if Child.Max_Width = Fixed_Size then
                  Child.Width := Child.Min_Width.Value * Self.Width;
               else
                  Child.Width := Model_Coordinate'Max
                     (Child.Width, Child.Min_Width.Value * Self.Width);
               end if;
         end case;

         case Child.Max_Width.Unit is
            when Unit_Pixels  => null;  --  Taken care of in Size_Request
            when Unit_Percent =>        --  Not Fixed_Size
               Child.Width := Model_Coordinate'Min
                  (Child.Width, Child.Max_Width.Value * Self.Width);
            when Unit_Fit     =>        --  Use full parent size
               if Self.Layout = Vertical_Stack then
                  Child.Width :=
                     Self.Width - Child.Margin.Left - Child.Margin.Right;
               end if;
            when Unit_Auto    =>  --  Use size computed in Size_Request
               null;
         end case;

         case Child.Min_Height.Unit is
            when Unit_Auto | Unit_Fit => null; --  Only relevant for Max_Width
            when Unit_Pixels => null; --  Taken care of in Size_Request
            when Unit_Percent =>
               if Child.Max_Height = Fixed_Size then
                  Child.Height := Child.Min_Height.Value * Self.Height;
               else
                  Child.Height := Model_Coordinate'Max
                     (Child.Height, Child.Min_Height.Value * Self.Height);
               end if;
         end case;

         case Child.Max_Height.Unit is
            when Unit_Pixels  => null;  --  taken care of in Size_Request
            when Unit_Percent =>        --  not Fixed_Size
               Child.Height := Model_Coordinate'Min
                  (Child.Height, Child.Max_Height.Value * Self.Height);
            when Unit_Fit    => --  Use full parent size
               if Self.Layout = Horizontal_Stack then
                  Child.Height :=
                     Self.Height - Child.Margin.Top - Child.Margin.Bottom;
               end if;
            when Unit_Auto    =>  --  Use size computed in Size_Request
               null;
         end case;

         --  Now compute the position for the child

         case Self.Layout is
            when Vertical_Stack =>
               if Child.Position.Y /= Gdouble'First then
                  Child.Computed_Position.Y :=
                    Child.Position.Y + Child.Margin.Top
                    - Child.Height * Child.Anchor_Y;
               else
                  if Child.Pack_End then
                     Child.Computed_Position.Y :=
                        Tmp_End - Child.Height - Child.Margin.Bottom;
                  else
                     Child.Computed_Position.Y := Tmp + Child.Margin.Top;
                  end if;
               end if;

               if Child.Position.X /= Gdouble'First then
                  Child.Computed_Position.X :=
                    Child.Position.X + Child.Margin.Left
                    - Child.Width * Child.Anchor_X;
               else
                  case Child.Align is
                     when Align_Start =>
                        Child.Computed_Position.X := Child.Margin.Left;

                     when Align_Center =>
                        Child.Computed_Position.X :=
                           (Self.Width - Child.Width) / 2.0;

                     when Align_End =>
                        Child.Computed_Position.X :=
                          Self.Width - Child.Width - Child.Margin.Right;
                  end case;
               end if;

               Child.Size_Allocate;

               if not Child.Float then
                  if Child.Pack_End then
                     Tmp_End := Child.Computed_Position.Y - Child.Margin.Top;
                  else
                     Tmp := Child.Computed_Position.Y
                       + Child.Height + Child.Margin.Bottom;
                  end if;
               end if;

            when Horizontal_Stack =>
               if Child.Position.X /= Gdouble'First then
                  Child.Computed_Position.X :=
                    Child.Position.X + Child.Margin.Left
                    - Child.Width * Child.Anchor_X;
               else
                  if Child.Pack_End then
                     Child.Computed_Position.X :=
                        Tmp_End - Child.Width - Child.Margin.Right;
                  else
                     Child.Computed_Position.X := Tmp + Child.Margin.Left;
                  end if;
               end if;

               if Child.Position.Y /= Gdouble'First then
                  Child.Computed_Position.Y :=
                    Child.Position.Y + Child.Margin.Top
                    - Child.Height * Child.Anchor_Y;
               else
                  case Child.Align is
                     when Align_Start =>
                        Child.Computed_Position.Y := Child.Margin.Top;

                     when Align_Center =>
                        Child.Computed_Position.Y :=
                            (Self.Height - Child.Height) / 2.0;

                     when Align_End =>
                        Child.Computed_Position.Y :=
                          Self.Height - Child.Height - Child.Margin.Bottom;
                  end case;
               end if;

               Child.Size_Allocate;

               if not Child.Float then
                  if Child.Pack_End then
                     Tmp_End := Child.Computed_Position.X - Child.Margin.Left;
                  else
                     Tmp := Child.Computed_Position.X
                       + Child.Width + Child.Margin.Right;
                  end if;
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

      Self.Computed_Position.X :=
        Self.Computed_Position.X - (Self.Width * Self.Anchor_X);
      Self.Computed_Position.Y :=
        Self.Computed_Position.Y - (Self.Height * Self.Anchor_Y);
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

   -------------------
   -- Gtk_New_Image --
   -------------------

   function Gtk_New_Image
     (Style  : Gtkada.Style.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item
   is
      R : constant Image_Item := new Image_Item_Record;
   begin
      Initialize_Image (R, Style, Image, Allow_Rescale, Width, Height);
      return R;
   end Gtk_New_Image;

   -------------------
   -- Gtk_New_Image --
   -------------------

   function Gtk_New_Image
     (Style  : Gtkada.Style.Drawing_Style;
      Icon_Name : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Image_Item
   is
      R : constant Image_Item := new Image_Item_Record;
   begin
      Initialize_Image (R, Style, Icon_Name, Allow_Rescale, Width, Height);
      return R;
   end Gtk_New_Image;

   ----------------------
   -- Initialize_Image --
   ----------------------

   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style.Drawing_Style;
      Image  : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double) is
   begin
      Self.Style := Style;
      Self.Image := Gdk_Pixbuf (Image);
      Self.Allow_Rescale := Allow_Rescale;
      Ref (Self.Image);
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Image;

   ----------------------
   -- Initialize_Image --
   ----------------------

   procedure Initialize_Image
     (Self   : not null access Image_Item_Record'Class;
      Style  : Gtkada.Style.Drawing_Style;
      Icon_Name  : String;
      Allow_Rescale : Boolean := True;
      Width, Height : Model_Coordinate := Fit_Size_As_Double) is
   begin
      Self.Style := Style;
      Self.Icon_Name := new String'(Icon_Name);
      Self.Allow_Rescale := Allow_Rescale;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Image;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self     : not null access Image_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      if Self.Image /= null then
         Unref (Self.Image);
         Self.Image := null;
      end if;
      Free (Self.Icon_Name);
      Container_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context)
   is
      P : Gdk_Pixbuf;
      W, H  : Gdouble;
   begin
      Self.Style.Draw_Rect (Context.Cr, (0.0, 0.0), Self.Width, Self.Height);

      if Self.Icon_Name /= null then
         W := Gdouble'Min (Self.Width, Self.Height);
         Draw_Pixbuf_With_Scale
            (Context.Cr,
             Self.Icon_Name.all,
             Size => Gint (W),
             X => (Self.Width - W) / 2.0,
             Y => (Self.Height - W) / 2.0,
             Widget => Context.View);

      else
         W := Gdouble (Get_Width (Self.Image)) - Self.Width;
         H := Gdouble (Get_Height (Self.Image)) - Self.Height;

         if Self.Allow_Rescale and then
            (abs (W) >= 2.0 or else abs (H) >= 2.0)
         then
            P := Scale_Simple
              (Self.Image,
               Dest_Width  => Gint (Self.Width),
               Dest_Height => Gint (Self.Height));
            Draw_Pixbuf_With_Scale
              (Context.Cr, P, 0.0, 0.0, Context.View);
            Unref (P);
         else
            Draw_Pixbuf_With_Scale
              (Context.Cr, Self.Image, W / 2.0, H / 2.0, Context.View);
         end if;
      end if;
   end Draw;

   ------------------
   -- Size_Request --
   ------------------

   overriding procedure Size_Request
     (Self    : not null access Image_Item_Record;
      Context : Draw_Context)
   is
   begin
      Container_Item_Record (Self.all).Size_Request (Context);  --  inherited
      if Self.Image /= null then
         Self.Width := Model_Coordinate'Max
           (Self.Width, Gdouble (Get_Width (Self.Image)));
         Self.Height := Model_Coordinate'Max
           (Self.Height, Gdouble (Get_Height (Self.Image)));
      end if;
   end Size_Request;

   ------------------
   -- Gtk_New_Rect --
   ------------------

   function Gtk_New_Rect
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
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
      Width, Height : Model_Coordinate := Fit_Size_As_Double;
      Radius        : Model_Coordinate := 0.0)
   is
   begin
      Self.Style         := Style;
      Self.Radius        := Radius;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
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

   ------------------
   -- Draw_Outline --
   ------------------

   overriding procedure Draw_Outline
     (Self    : not null access Rect_Item_Record;
      Style   : Gtkada.Style.Drawing_Style;
      Context : Draw_Context)
   is
      Margin_Pixels : constant View_Coordinate := 2.0;
      Margin        : Model_Coordinate;
   begin
      if Context.View /= null then
         Margin := Margin_Pixels / Context.View.Scale;
         Style.Draw_Rect
           (Context.Cr, (-Margin, -Margin),
            Self.Width + 2.0 * Margin, Self.Height + 2.0 * Margin,
            Radius => Self.Radius);
      end if;
   end Draw_Outline;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Rect_Item_Record;
      Context : Draw_Context)
   is
      Stroke : constant Gdk_RGBA := Self.Style.Get_Stroke;
      Fill   : constant Cairo_Pattern := Self.Style.Get_Fill;
      Shadow : constant Shadow_Style := Self.Style.Get_Shadow;
   begin
      if Shadow /= No_Shadow then
         Save (Context.Cr);
         Translate (Context.Cr, Shadow.X_Offset, Shadow.Y_Offset);

         if Self.Style.Path_Rect
           (Context.Cr, (0.0, 0.0),
            Self.Width, Self.Height, Radius => Self.Radius)
         then
            Set_Source_RGBA (Context.Cr, Shadow.Color);
            Cairo.Fill (Context.Cr);
         end if;

         Restore (Context.Cr);
      end if;

      Resize_Fill_Pattern (Self);

      --  We need to clip the contents of the rectangle. Also, we
      --  stroke only after drawing the children so that they do
      --  no hide the stroke.

      if Self.Style.Path_Rect
        (Context.Cr, (0.0, 0.0), Self.Width, Self.Height,
         Radius => Self.Radius)
      then
         --  Only clip if the item is visible, otherwise let nested items do
         --  their own clipping. This ensures that nested items with shadows
         --  are properly rendered

         if not Self.Is_Invisible then
            Clip_Preserve (Context.Cr);
         end if;

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
      Shadow : constant Shadow_Style := Self.Style.Get_Shadow;
   begin
      if Shadow /= No_Shadow then
         Save (Context.Cr);
         Translate (Context.Cr, Shadow.X_Offset, Shadow.Y_Offset);

         if Self.Style.Path_Polyline
           (Context.Cr, Self.Points.all,
            Close    => Self.Close,
            Relative => Self.Relative)
         then
            Set_Source_RGBA (Context.Cr, Shadow.Color);
            Cairo.Fill (Context.Cr);
         end if;

         Restore (Context.Cr);
      end if;

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
     (Self : not null access Polyline_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class)
   is
   begin
      Unchecked_Free (Self.Points);
      Container_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

   ---------------------
   -- Gtk_New_Ellipse --
   ---------------------

   function Gtk_New_Ellipse
     (Style         : Gtkada.Style.Drawing_Style;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
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
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
   is
   begin
      Self.Style := Style;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
   end Initialize_Ellipse;

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Self    : not null access Ellipse_Item_Record;
      Context : Draw_Context)
   is
      Shadow : constant Shadow_Style := Self.Style.Get_Shadow;
   begin
      if Shadow /= No_Shadow then
         Save (Context.Cr);
         Translate (Context.Cr, Shadow.X_Offset, Shadow.Y_Offset);

         if Self.Style.Path_Ellipse
           (Context.Cr, (0.0, 0.0), Self.Width, Self.Height)
         then
            Set_Source_RGBA (Context.Cr, Shadow.Color);
            Cairo.Fill (Context.Cr);
         end if;

         Restore (Context.Cr);
      end if;

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
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double)
      return Text_Item
   is
      R : constant Text_Item := new Text_Item_Record;
   begin
      Initialize_Text (R, Style, Text, Directed, Width, Height);
      return R;
   end Gtk_New_Text;

   ---------------------
   -- Initialize_Text --
   ---------------------

   procedure Initialize_Text
     (Self     : not null access Text_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow;
      Width, Height : Model_Coordinate := Fit_Size_As_Double) is
   begin
      Self.Style := Style;
      Self.Text  := new String'(Text);
      Self.Directed := Directed;
      Self.Set_Size (Size_From_Value (Width), Size_From_Value (Height));
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

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Self : not null access Text_Item_Record) return String is
   begin
      return Self.Text.all;
   end Get_Text;

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
      Text : constant String := Compute_Text (Self);
   begin
      Resize_Fill_Pattern (Self);
      Self.Style.Draw_Rect (Context.Cr, (0.0, 0.0), Self.Width, Self.Height);

      if Context.Layout /= null then
         Self.Style.Draw_Text
           (Context.Cr, Context.Layout, (0.0, 0.0), Text,
            Max_Width  => Self.Width,
            Max_Height => Self.Height);
      end if;
   end Draw;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Self : not null access Text_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Free (Self.Text);
      Container_Item_Record (Self.all).Destroy (In_Model);  --  inherited
   end Destroy;

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
           (Context.Layout, Text, Width => Self.Width, Height => Self.Height);
      else
         Self.Width := 0.0;
         Self.Height := 0.0;
      end if;
   end Size_Request;

   ---------------------------
   -- Gtk_New_Editable_Text --
   ---------------------------

   function Gtk_New_Editable_Text
     (Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
      return Editable_Text_Item
   is
      Result : constant Editable_Text_Item := new Editable_Text_Item_Record;
   begin
      Initialize_Editable_Text (Result, Style, Text, Directed);
      return Result;
   end Gtk_New_Editable_Text;

   ------------------------------
   -- Initialize_Editable_Text --
   ------------------------------

   procedure Initialize_Editable_Text
     (Self     : not null access Editable_Text_Item_Record'Class;
      Style    : Gtkada.Style.Drawing_Style;
      Text     : Glib.UTF8_String;
      Directed : Text_Arrow_Direction := No_Text_Arrow)
   is
   begin
      Initialize_Text (Self, Style, Text, Directed);
   end Initialize_Editable_Text;

   ----------------------------
   -- On_Text_Edit_Key_Press --
   ----------------------------

   function On_Text_Edit_Key_Press
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Key)
     return Boolean
   is
      Self : constant Canvas_View := Canvas_View (View);
      Text : Gtk_Text_View;
      From, To : Gtk_Text_Iter;
   begin
      --  <return> closes the editing, but <shift-return> just
      --  inserts a newline
      if Event.Keyval = GDK_Return
         and then (Event.State and Get_Default_Mod_Mask) = 0
      then
         declare
            Old : constant String := Editable_Text_Item
              (Self.Inline_Edit.Item).Text.all;
         begin
            Text := Gtk_Text_View (Self.Get_Child);
            Text.Get_Buffer.Get_Start_Iter (From);
            Text.Get_Buffer.Get_End_Iter (To);

            Editable_Text_Item (Self.Inline_Edit.Item).Set_Text
              (Text.Get_Buffer.Get_Text
                 (Start   => From,
                  The_End => To));

            Editable_Text_Item (Self.Inline_Edit.Item).On_Edited (Old);

            Cancel_Inline_Editing (Self);
            Self.Model.Refresh_Layout;
         end;

         return True;

      elsif Event.Keyval = GDK_Escape then
         Cancel_Inline_Editing (Self);
         return True;
      end if;

      return False;
   end On_Text_Edit_Key_Press;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Self   : not null access Editable_Text_Item_Record'Class;
      Editable : Boolean) is
   begin
      Self.Editable := Editable;
   end Set_Editable;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable
     (Self   : not null access Editable_Text_Item_Record'Class)
     return Boolean is
   begin
      return Self.Editable;
   end Is_Editable;

   -----------------
   -- Edit_Widget --
   -----------------

   overriding function Edit_Widget
     (Self  : not null access Editable_Text_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Text   : Gtk_Text_View;
      Font   : Pango_Font_Description;
      Start, Finish : Gtk_Text_Iter;
      Buffer : Gtk_Text_Buffer;
   begin
      if Self.Editable then
         Gtk_New (Buffer);
         Gtk_New (Text, Buffer);

         Font := Copy (Self.Get_Style.Get_Font.Name);
         Set_Size (Font, Gint (Gdouble (Get_Size (Font)) * View.Get_Scale));
         Text.Override_Font (Font);
         Free (Font);

         Text.Override_Color
            (Gtk_State_Flag_Normal or Gtk_State_Flag_Active,
             Self.Get_Style.Get_Font.Color);

         Buffer.Set_Text (Self.Text.all);   --  not compute_text
         Text.On_Key_Press_Event (On_Text_Edit_Key_Press'Access, View);

         --  Select the whole text so that users can more easily replace
         --  it
         Buffer.Get_Start_Iter (Start);
         Buffer.Get_End_Iter (Finish);
         Buffer.Select_Range (Start, Finish);

         return Gtk_Widget (Text);
      else
         return null;
      end if;
   end Edit_Widget;

   -----------------
   -- Edit_Widget --
   -----------------

   function Edit_Widget
     (Self  : not null access Canvas_Item_Record;
      View  : not null access Canvas_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Self, View);
   begin
      --  for some reason, we need to define this function even though the
      --  one defined for the interface is already defined as "null".
      --  Otherwise, dispatching from Start_Inline_Editing does not work...
      return null;
   end Edit_Widget;

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

   overriding procedure Destroy
     (Self : not null access Hr_Item_Record;
      In_Model : not null access Canvas_Model_Record'Class) is
   begin
      Free (Self.Text);
      Container_Item_Record (Self.all).Destroy (In_Model);
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

   -------------------
   -- Set_Grid_Size --
   -------------------

   procedure Set_Grid_Size
     (Self : not null access Canvas_View_Record'Class;
      Size : Model_Coordinate := 30.0)
   is
   begin
      Self.Grid_Size := Size;
   end Set_Grid_Size;

   --------------
   -- Set_Snap --
   --------------

   procedure Set_Snap
     (Self           : not null access Canvas_View_Record'Class;
      Snap_To_Grid   : Boolean := True;
      Snap_To_Guides : Boolean := False;
      Snap_Margin    : Model_Coordinate := 5.0;
      Guides_Style   : Gtkada.Style.Drawing_Style := Default_Guide_Style)
   is
   begin
      Self.Snap.Grid         := Snap_To_Grid;
      Self.Snap.Smart_Guides := Snap_To_Guides;
      Self.Snap.Margin       := Snap_Margin;
      Self.Snap.Style        := Guides_Style;
   end Set_Snap;

   ----------
   -- Hash --
   ----------

   function Hash (Key : Abstract_Item) return Ada.Containers.Hash_Type is
   begin
      if Key = null then
         return 0;
      else
         return Ada.Containers.Hash_Type
           (To_Integer (Key.all'Address)
            mod Integer_Address (Ada.Containers.Hash_Type'Last));
      end if;
   end Hash;

   ------------
   -- Export --
   ------------

   function Export
     (Self              : not null access Canvas_View_Record;
      Filename          : String;
      Page              : Page_Format;
      Format            : Export_Format := Export_PDF;
      Visible_Area_Only : Boolean := True)
      return Boolean
   is
      W : constant Gdouble := Page.Width_In_Inches * 72.0;  --  in points
      H : constant Gdouble := Page.Height_In_Inches * 72.0;
      Surf : Cairo_Surface;
      Context : Draw_Context;
      Old_Scale : constant Gdouble := Self.Scale;
      Topleft   : constant Model_Point := Self.Topleft;
      Box       : Model_Rectangle;
      Status    : Cairo_Status;
   begin
      case Format is
         when Export_PDF =>
            Surf := Cairo.PDF.Create
              (Filename         => Filename,
               Width_In_Points  => W,
               Height_In_Points => H);
         when Export_SVG =>
            Surf := Cairo.SVG.Create (Filename, W, H);
         when Export_PNG =>
            Surf := Create_Similar_Surface
              (Get_Window (Self),
               Cairo.Cairo_Content_Color_Alpha, Gint (W), Gint (H));
      end case;

      Context := (Cr     => Create (Surf),
                  Layout => Self.Layout,
                  View   => Canvas_View (Self));

      if Visible_Area_Only then
         Box := Self.Get_Visible_Area;

         Self.Scale := Self.Scale *
           Gdouble'Min
             (Gdouble'Min
                (W / Gdouble (Self.Get_Allocated_Width),
                 H / Gdouble (Self.Get_Allocated_Height)),
              1.0);
         Canvas_View_Record'Class (Self.all).Set_Transform (Context.Cr);

         --  Need to clip, otherwise the items partially visible on the screen
         --  will be fully visible in the print

         Rectangle (Context.Cr, Box.X, Box.Y, Box.Width, Box.Height);
         Clip (Context.Cr);

         Canvas_View_Record'Class (Self.all).Draw_Internal (Context, Box);

      else
         Box := Self.Model.Bounding_Box;
         Self.Scale := Gdouble'Min
           (1.0,
            Gdouble'Min (W / (Box.Width + 20.0), H / (Box.Height + 20.0)));
         Self.Topleft := (Box.X - 10.0 * Self.Scale,
                          Box.Y - 10.0 * Self.Scale);
         Canvas_View_Record'Class (Self.all).Set_Transform (Context.Cr);
         Canvas_View_Record'Class (Self.all).Draw_Internal (Context, Box);
      end if;

      Destroy (Context.Cr);

      if Format = Export_PNG then
         Status := Cairo.Png.Write_To_Png (Surf, Filename);
      else
         Status := Cairo.Surface.Status (Surf);
      end if;

      Surface_Destroy (Surf);
      Self.Scale := Old_Scale;
      Self.Topleft := Topleft;

      return Status = Cairo_Status_Success;
   end Export;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
     (Self : not null access Canvas_Model_Record;
      Mode : Selection_Mode)
   is
   begin
      if Mode /= Self.Mode then
         Self.Mode := Mode;
         Canvas_Model_Record'Class (Self.all).Clear_Selection;
      end if;
   end Set_Selection_Mode;

   ---------------------
   -- Clear_Selection --
   ---------------------

   procedure Clear_Selection (Self : not null access Canvas_Model_Record) is
   begin
      if not Self.Selection.Is_Empty then
         Self.Selection.Clear;
         Self.Selection_Changed;
      end if;
   end Clear_Selection;

   ----------------------
   -- Add_To_Selection --
   ----------------------

   procedure Add_To_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      case Self.Mode is
         when Selection_None =>
            null;

         when Selection_Single =>
            if Self.Is_Selectable (Item) then
               Canvas_Model_Record'Class (Self.all).Clear_Selection;
               Self.Selection.Include (Abstract_Item (Item));
               Self.Selection_Changed (Item);
            end if;

         when Selection_Multiple =>
            if Self.Is_Selectable (Item) then
               Self.Selection.Include (Abstract_Item (Item));
               Self.Selection_Changed (Item);
            end if;
      end case;
   end Add_To_Selection;

   ---------------------------
   -- Remove_From_Selection --
   ---------------------------

   procedure Remove_From_Selection
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class) is
   begin
      if Canvas_Model_Record'Class (Self.all).Is_Selected (Item) then
         Self.Selection.Delete (Abstract_Item (Item));
         Self.Selection_Changed (Item);
      end if;
   end Remove_From_Selection;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Self : not null access Canvas_Model_Record;
      Item : not null access Abstract_Item_Record'Class)
      return Boolean
   is
   begin
      return Self.Selection.Contains (Abstract_Item (Item));
   end Is_Selected;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Item : access Abstract_Item_Record'Class := null) is
   begin
      Abstract_Item_Emit
        (Self, Signal_Selection_Changed & ASCII.NUL,
         Abstract_Item (Item));
   end Selection_Changed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   function On_Selection_Changed
     (Self : not null access Canvas_Model_Record'Class;
      Call : not null access procedure
        (Self : not null access GObject_Record'Class;
         Item : Abstract_Item);
      Slot : access GObject_Record'Class := null)
      return Gtk.Handlers.Handler_Id
   is
   begin
      if Slot = null then
         return Object_Callback.Connect
           (Self,
            Signal_Selection_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call));
      else
         return Object_Callback.Object_Connect
           (Self,
            Signal_Selection_Changed,
            Abstract_Item_Marshallers.To_Marshaller (Call),
            Slot);
      end if;
   end On_Selection_Changed;

   -------------------------
   -- Set_Selection_Style --
   -------------------------

   procedure Set_Selection_Style
     (Self  : not null access Canvas_View_Record;
      Style : Gtkada.Style.Drawing_Style) is
   begin
      Self.Selection_Style := Style;
   end Set_Selection_Style;

   -------------------------
   -- Get_Selection_Style --
   -------------------------

   function Get_Selection_Style
     (Self  : not null access Canvas_View_Record)
      return Gtkada.Style.Drawing_Style is
   begin
      return Self.Selection_Style;
   end Get_Selection_Style;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Self : not null access Canvas_Link_Record) return Container_Item is
   begin
      return Self.Label;
   end Get_Label;

   --------------------
   -- Get_Label_From --
   --------------------

   function Get_Label_From
     (Self : not null access Canvas_Link_Record) return Container_Item is
   begin
      return Self.Label_From;
   end Get_Label_From;

   ------------------
   -- Get_Label_To --
   ------------------

   function Get_Label_To
     (Self : not null access Canvas_Link_Record) return Container_Item is
   begin
      return Self.Label_To;
   end Get_Label_To;

   --------------
   -- Get_From --
   --------------

   function Get_From
     (Self : not null access Canvas_Link_Record) return Abstract_Item is
   begin
      return Self.From;
   end Get_From;

   ------------
   -- Get_To --
   ------------

   function Get_To
     (Self : not null access Canvas_Link_Record) return Abstract_Item is
   begin
      return Self.To;
   end Get_To;

   -------------------
   -- Avoid_Overlap --
   -------------------

   procedure Avoid_Overlap
     (Self     : not null access Canvas_View_Record'Class;
      Avoid    : Boolean;
      Duration : Standard.Duration := 0.2)
   is
   begin
      Self.Avoid_Overlap := Avoid;
      Self.Avoid_Overlap_Duration := Duration;
   end Avoid_Overlap;

end Gtkada.Canvas_View;
