with Ada.Unchecked_Deallocation;
with Gdk.Cursor;           use Gdk, Gdk.Cursor;
with Gdk.Drawable;         use Gdk.Drawable;
with Gdk.Event;            use Gdk.Event;
with Gdk.GC;               use Gdk.GC;
with Gdk.Main;             use Gdk.Main;
with Gdk.Rectangle;        use Gdk.Rectangle;
with Gdk.Window;           use Gdk.Window;
with Gdk.Window_Attr;      use Gdk.Window_Attr;
with Glib.Object;          use Glib.Object;
with Glib;                 use Glib;
with Gtk.Arguments;        use Gtk.Arguments;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Fixed;            use Gtk.Fixed;
with Gtk.Object;           use Gtk.Object;
with Gtk.Style;            use Gtk.Style;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;               use System;

package body Gtkada.Multi_Paned is

   Handle_Half_Width : constant := 3;
   --  Half the width, in pixels, of the resizing handles.
   --  ??? Should be read from theme with
   --     gtk_widget_style_get (gtk_paned, "handle_size", &handle_size, NULL)

   Minimum_Child_Width : constant := 10 + 2 * Handle_Half_Width;
   --  Minimum width or height for the children of the window

   Paned_Class_Record : Gtk.Object.GObject_Class :=
     Gtk.Object.Uninitialized_Class;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Handles_Array, Handles_Array_Access);

   procedure Free (Child : in out Child_Description_Access);
   --  Free Child, but not its Next or parent children

   procedure Size_Allocate_Paned
     (Paned : System.Address; Alloc : Gtk_Allocation);
   pragma Convention (C, Size_Allocate_Paned);
   --  Window was resized, need to resize and reposition the children.

   procedure Realize_Paned (Paned : access Gtk_Widget_Record'Class);
   --  Called when the window was realized.

   procedure Create_Handle
     (Split  : access Gtkada_Multi_Paned_Record'Class;
      Handle : out Resize_Handle;
      Orientation : Gtk_Orientation);
   --  Create a new handle

   function Expose_Paned
     (Paned : access Gtk_Widget_Record'Class;
      Event      : Gdk_Event) return Boolean;
   --  Redraw all the handles

   procedure Move_Handles
     (Split : access Gtkada_Multi_Paned_Record'Class;
      Parent : Child_Description_Access;
      Ref   : Positive := 1);
   --  Recompute the position of all the handles.
   --  If Ref is other than 1, than the Ref-th handle will not be
   --  moved. Instead, handles on both side will be moved appropriately so that
   --  their relative position on screen is the same as in Split.Handles.

   function Button_Pressed
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Released
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   function Button_Motion
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;

   procedure Remove_Child
     (Paned : access Gtk_Widget_Record'Class;
      Args       : Gtk_Args);
   --  A child was removed from Spittable

   procedure Draw_Resize_Line (Split : access Gtkada_Multi_Paned_Record'Class);
   --  Draw, in xor mode, the resizing line

   procedure Destroy_Paned
     (Paned : access Gtk_Widget_Record'Class);
   --  The Paned window is being destroyed.

   procedure Compute_Handle_Position
     (Split        : access Gtkada_Multi_Paned_Record'Class;
      Parent       : Child_Description_Access;
      Handle_Index : Natural;
      Position     : in out Gtk_Allocation);
   --  Compute the position of a specific handle

   procedure Compute_Child_Position
     (Split    : access Gtkada_Multi_Paned_Record'Class;
      Child    : Child_Description_Access;
      Position : in out Gtk_Allocation);
   --  Compute the position of a specific child


   type Child_Iterator is record
      Current : Child_Description_Access;
   end record;

   function Start
     (Win : access Gtkada_Multi_Paned_Record'Class) return Child_Iterator;
   --  Start iterating over all the children of Win. This is a depth-first
   --  iterator, so that children can be removed when the iterator is
   --  traversed

   procedure Next (Iter : in out Child_Iterator);
   --  Move to the next child

   function Get (Iter : Child_Iterator) return Child_Description_Access;
   --  Return the current child. You must move to Next before destroying
   --  the returned value, if you need to.
   --  Null is returned when there are no more children.

   procedure Split_Internal
     (Win         : access Gtkada_Multi_Paned_Record'Class;
      Ref_Widget  : Gtk_Widget;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk_Orientation;
      Width, Height : Glib.Gint := 0;
      After       : Boolean := True);
   --  Internal version of Split_Horizontal and Split_Vertical

   procedure Add_Handle
     (Split : access Gtkada_Multi_Paned_Record'Class;
      Parent : Child_Description_Access;
      Child : Child_Description_Access;
      Old_Handles : Handles_Array_Access := null);
   --  Add a new handle to Parent.
   --  The handle corresponding to Child is modified.
   --  Old_Handles is the old percentage values used to split Child.
   --  Child_Percent indicates what proportion of the area the new handle
   --  occupies. It is ignored if Old_Handles /= null or there are more than
   --  one child.

   function Is_Visible (Child : Child_Description_Access) return Boolean;
   --  Return True if Child is visible (or if any of its children is visible).

   ----------
   -- Free --
   ----------

   procedure Free (Child : in out Child_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Child_Description, Child_Description_Access);
   begin
      if not Child.Is_Widget then
         for H in Child.Handles'Range loop
            Destroy (Child.Handles (H).Win);
         end loop;

         Unchecked_Free (Child.Handles);
      end if;

      Unchecked_Free (Child);
   end Free;

   -----------
   -- Start --
   -----------

   function Start
     (Win : access Gtkada_Multi_Paned_Record'Class) return Child_Iterator
   is
      C : Child_Description_Access := Win.Children;
   begin
      while C /= null
        and then not C.Is_Widget
        and then C.First_Child /= null
      loop
         C := C.First_Child;
      end loop;

      return (Current => C);
   end Start;

   ---------
   -- Get --
   ---------

   function Get (Iter : Child_Iterator) return Child_Description_Access is
   begin
      return Iter.Current;
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Child_Iterator) is
   begin
      if Iter.Current = null then
         null;
      elsif Iter.Current.Next = null then
         Iter.Current := Iter.Current.Parent;
      else
         Iter.Current := Iter.Current.Next;
         while not Iter.Current.Is_Widget
           and then Iter.Current.First_Child /= null
         loop
            Iter.Current := Iter.Current.First_Child;
         end loop;
      end if;
   end Next;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Win : out Gtkada_Multi_Paned) is
   begin
      Win := new Gtkada_Multi_Paned_Record;
      Gtkada.Multi_Paned.Initialize (Win);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Win : access Gtkada_Multi_Paned_Record'Class) is
   begin
      Gtk.Fixed.Initialize (Win);
      Gtk.Object.Initialize_Class_Record
        (Win,
         Signals      => (1 .. 0 => Null_Ptr),
         Class_Record => Paned_Class_Record,
         Type_Name    => "GtkAdaMultiPaned",
         Parameters   => (1 .. 0 => (1 => GType_None)));

      Set_Default_Size_Allocate_Handler
        (Paned_Class_Record, Size_Allocate_Paned'Access);

      Widget_Callback.Connect
        (Win, "realize",
         Widget_Callback.To_Marshaller (Realize_Paned'Access));
      Return_Callback.Connect
        (Win, "expose_event",
         Return_Callback.To_Marshaller (Expose_Paned'Access));
      Return_Callback.Connect
        (Win, "button_press_event",
         Return_Callback.To_Marshaller (Button_Pressed'Access));
      Return_Callback.Connect
        (Win, "button_release_event",
         Return_Callback.To_Marshaller (Button_Released'Access));
      Return_Callback.Connect
        (Win, "motion_notify_event",
         Return_Callback.To_Marshaller (Button_Motion'Access));
      Widget_Callback.Connect (Win, "remove", Remove_Child'Access);
      Widget_Callback.Connect
        (Win, "destroy",
         Widget_Callback.To_Marshaller (Destroy_Paned'Access));
   end Initialize;

   ------------------------
   -- Destroy_Paned --
   ------------------------

   procedure Destroy_Paned
     (Paned : access Gtk_Widget_Record'Class)
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Iter : Child_Iterator := Start (Split);
      Current : Child_Description_Access;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null;

         Next (Iter);
         Free (Current);
      end loop;

      if Split.GC /= null then
         Unref (Split.GC);
      end if;
   end Destroy_Paned;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
     (Paned : access Gtk_Widget_Record'Class;
      Args  : Gtk_Args)
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);

      procedure Merge_With_Parent_If_Single_Child
        (Child : in out Child_Description_Access);
      --  If Child has a single child itself, merge it with its parent, and
      --  free Child. Child is set to its first child if they are the same.

      procedure Merge_With_Parent_If_Same
        (Child : in out Child_Description_Access);
      --  Merge Child with its parent if they have the same orientation

      ---------------------------------------
      -- Merge_With_Parent_If_Single_Child --
      ---------------------------------------

      procedure Merge_With_Parent_If_Single_Child
        (Child : in out Child_Description_Access)
      is
         Tmp : Child_Description_Access;
      begin
         if Child.First_Child /= null
           and then Child.First_Child.Next = null
         then
            if Child.Parent /= null then
               Child.First_Child.Parent := Child.Parent;

               if Child.Parent.First_Child = Child then
                  Child.Parent.First_Child := Child.First_Child;
               else
                  Tmp := Child.Parent.First_Child;
                  while Tmp.Next /= Child loop
                     Tmp := Tmp.Next;
                  end loop;

                  Child.First_Child.Next := Child.Next;
                  Tmp.Next := Child.First_Child;
               end if;

               Tmp := Child;
               Child := Child.First_Child;
               Free (Tmp);
            elsif not Child.First_Child.Is_Widget then
               Split.Children := Child.First_Child;
               Split.Children.Parent := null;
               Free (Child);
               Child := Split.Children;
            end if;
         end if;
      end Merge_With_Parent_If_Single_Child;

      -------------------------------
      -- Merge_With_Parent_If_Same --
      -------------------------------

      procedure Merge_With_Parent_If_Same
        (Child : in out Child_Description_Access)
      is
         Tmp, Previous : Child_Description_Access;
      begin
         if not Child.Is_Widget
           and then Child.Parent /= null
           and then Child.Orientation = Child.Parent.Orientation
         then
            Tmp := Child.First_Child;
            while Tmp /= null loop
               Tmp.Parent := Child.Parent;
               Previous := Tmp;
               Tmp := Tmp.Next;
            end loop;

            Add_Handle
              (Gtkada_Multi_Paned (Paned), Child.Parent, Child, Child.Handles);

            if Child.Parent.First_Child = Child then
               Previous.Next := Child.Next;
               Child.Parent.First_Child := Child.First_Child;
            else
               Tmp := Child.Parent.First_Child;
               while Tmp.Next /= Child loop
                  Tmp := Tmp.Next;
               end loop;

               Previous.Next := Tmp.Next.Next;
               Tmp.Next := Child.First_Child;
            end if;

            Free (Child);
         end if;
      end Merge_With_Parent_If_Same;


      Child : constant Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
      Old_Handles  : Handles_Array_Access;
      C, D : Natural;
      Iter : Child_Iterator;
      Current, Tmp, Parent : Child_Description_Access;

   begin
      if In_Destruction_Is_Set (Split) then
         return;
      end if;

      Iter := Start (Split);

      loop
         Current := Get (Iter);
         exit when Current = null
           or else (Current.Is_Widget and then Current.Widget = Child);
         Next (Iter);
      end loop;

      if Current /= null then
         Parent := Current.Parent;
         C := Parent.Handles'First;

         if Parent.First_Child = Current then
            Parent.First_Child := Current.Next;
         else
            Tmp := Parent.First_Child;
            while Tmp.Next /= Current loop
               C := C + 1;
               Tmp := Tmp.Next;
            end loop;
            Tmp.Next := Current.Next;
         end if;

         Free (Current);

         if Parent.Handles'Length /= 0 then
            D := Natural'Min (C, Parent.Handles'Last);

            if Parent.Handles (D).Win /= null then
               Destroy (Parent.Handles (D).Win);
            end if;

            Old_Handles := Parent.Handles;
            Parent.Handles := new Handles_Array
              (1 .. Old_Handles'Length - 1);
            Parent.Handles (1 .. D - 1) := Old_Handles (1 .. D - 1);
            Parent.Handles (D .. Parent.Handles'Last) :=
              Old_Handles (D + 1 .. Old_Handles'Last);
            Unchecked_Free (Old_Handles);
         end if;

         Merge_With_Parent_If_Single_Child (Parent);
         Merge_With_Parent_If_Same (Parent);
         Queue_Resize (Split);
      end if;
   end Remove_Child;

   -----------------------------
   -- Compute_Handle_Position --
   -----------------------------

   procedure Compute_Handle_Position
     (Split        : access Gtkada_Multi_Paned_Record'Class;
      Parent       : Child_Description_Access;
      Handle_Index : Natural;
      Position     : in out Gtk_Allocation)
   is
      Parent_Position      : Gtk_Allocation;
      Percent : constant Float := Parent.Handles (Handle_Index).Percent;
   begin
      Compute_Child_Position (Split, Parent, Parent_Position);

      if Parent.Orientation = Orientation_Horizontal then
         Position :=
           (X           => Parent_Position.X + Gint
              (Percent * Float (Parent_Position.Width)) - Handle_Half_Width,
            Y           => Parent_Position.Y,
            Width       => Handle_Half_Width * 2,
            Height      => Parent_Position.Height);
      else
         Position :=
           (X           => Parent_Position.X,
            Y           => Parent_Position.Y + Gint
              (Percent * Float (Parent_Position.Height)) - Handle_Half_Width,
            Width       => Parent_Position.Width,
            Height      => Handle_Half_Width * 2);
      end if;
   end Compute_Handle_Position;

   ----------------------------
   -- Compute_Child_Position --
   ----------------------------

   procedure Compute_Child_Position
     (Split    : access Gtkada_Multi_Paned_Record'Class;
      Child    : Child_Description_Access;
      Position : in out Gtk_Allocation)
   is
      Parent_Pos : Gtk_Allocation;
      Percent, Percent_End : Float := 0.0;
      Handle_Index : Natural := 1;
      Tmp : Child_Description_Access;
      X_Offset, W_Offset : Allocation_Int;
   begin
      if Child.Parent = null then
         if Get_Has_Window (Split) then
            Position := (X      => 0,
                         Y      => 0,
                         Width  => Get_Allocation_Width (Split),
                         Height => Get_Allocation_Height (Split));
         else
            Position := (X      => Get_Allocation_X (Split),
                         Y      => Get_Allocation_Y (Split),
                         Width  => Get_Allocation_Width (Split),
                         Height => Get_Allocation_Height (Split));
         end if;
      else
         Compute_Child_Position (Split, Child.Parent, Parent_Pos);

         Tmp := Child.Parent.First_Child;
         while Tmp /= Child loop
            Tmp := Tmp.Next;

            if Is_Visible (Child.Parent.Handles (Handle_Index).Win) then
               Percent := Child.Parent.Handles (Handle_Index).Percent;
            end if;

            Handle_Index := Handle_Index + 1;
         end loop;

         if Handle_Index = 1 then
            X_Offset := 0;
         else
            X_Offset := Handle_Half_Width;
         end if;

         while Handle_Index <= Child.Parent.Handles'Last
           and then not Is_Visible (Child.Parent.Handles (Handle_Index).Win)
         loop
            Handle_Index := Handle_Index + 1;
         end loop;

         if Handle_Index > Child.Parent.Handles'Last then
            Percent_End := 1.0;
            W_Offset := X_Offset;
         else
            Percent_End := Child.Parent.Handles (Handle_Index).Percent;
            W_Offset := X_Offset + Handle_Half_Width;
         end if;

         case Child.Parent.Orientation is
            when Orientation_Vertical =>
               Position :=
                 (X      => Parent_Pos.X,
                  Y      => Parent_Pos.Y
                    + Allocation_Int (Percent * Float (Parent_Pos.Height))
                    + X_Offset,
                  Width  => Parent_Pos.Width,
                  Height => Allocation_Int
                    ((Percent_End - Percent) * Float (Parent_Pos.Height))
                    - W_Offset);

            when Orientation_Horizontal =>
               Position :=
                 (X      => Parent_Pos.X
                    + Allocation_Int (Percent * Float (Parent_Pos.Width))
                    + X_Offset,
                  Y      => Parent_Pos.Y,
                  Width  => Allocation_Int
                    ((Percent_End - Percent) * Float (Parent_Pos.Width))
                    - W_Offset,
                  Height => Parent_Pos.Height);
         end case;
      end if;
   end Compute_Child_Position;

   ----------------------
   -- Draw_Resize_Line --
   ----------------------

   procedure Draw_Resize_Line
     (Split : access Gtkada_Multi_Paned_Record'Class) is
   begin
      Draw_Line
        (Get_Window (Split),
         Split.GC, Split.Selected_Handle_Pos.X,
         Split.Selected_Handle_Pos.Y,
         Split.Selected_Handle_Pos.X + Split.Selected_Handle_Pos.Width,
         Split.Selected_Handle_Pos.Y + Split.Selected_Handle_Pos.Height);
   end Draw_Resize_Line;

   --------------------
   -- Button_Pressed --
   --------------------

   function Button_Pressed
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Split   : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Tmp     : Gdk_Grab_Status;
      Cursor  : Gdk_Cursor;
      Iter    : Child_Iterator := Start (Split);
      Current : Child_Description_Access;
      pragma Unreferenced (Tmp);
   begin
      if Get_Button (Event) /= 1 then
         return False;
      end if;

      while Get (Iter) /= null loop
         Current := Get (Iter);
         if not Current.Is_Widget then
            for H in Current.Handles'Range loop
               if Get_Window (Event) = Current.Handles (H).Win then
                  Split.Selected_Handle_Parent := Current;
                  Split.Selected_Handle_Index  := H;
                  exit;
               end if;
            end loop;
         end if;

         Next (Iter);
      end loop;

      if Split.Selected_Handle_Parent = null then
         return False;
      end if;

      case Split.Selected_Handle_Parent.Orientation is
         when Orientation_Vertical =>
            Gdk_New (Cursor, Sb_V_Double_Arrow);
         when Orientation_Horizontal =>
            Gdk_New (Cursor, Sb_H_Double_Arrow);
      end case;

      Tmp := Pointer_Grab
        (Get_Window (Event),
         False,
         Button_Press_Mask or Button_Motion_Mask or Button_Release_Mask,
         Cursor => Cursor,
         Time   => 0);
      Destroy (Cursor);

      Compute_Handle_Position
        (Split,
         Split.Selected_Handle_Parent,
         Split.Selected_Handle_Index,
         Split.Selected_Handle_Pos);

      if Split.Selected_Handle_Parent.Orientation = Orientation_Horizontal then
         Split.Anim_Offset := Gint (Get_X (Event));
         Split.Selected_Handle_Pos.Width := 0;
         Split.Selected_Handle_Pos.X :=
           Split.Selected_Handle_Pos.X + Split.Anim_Offset;
      else
         Split.Anim_Offset := Gint (Get_Y (Event));
         Split.Selected_Handle_Pos.Height := 0;
         Split.Selected_Handle_Pos.Y :=
           Split.Selected_Handle_Pos.Y + Split.Anim_Offset;
      end if;

      Draw_Resize_Line (Split);
      return False;
   end Button_Pressed;

   ---------------------
   -- Button_Released --
   ---------------------

   function Button_Released
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Percent : Float;
      Parent_Pos : Gtk_Allocation;
   begin
      if Split.Selected_Handle_Parent /= null then
         Draw_Resize_Line (Split);

         Compute_Child_Position
           (Split, Split.Selected_Handle_Parent, Parent_Pos);

         case Split.Selected_Handle_Parent.Orientation is
            when Orientation_Horizontal =>
               Percent :=
                 Float (Split.Selected_Handle_Pos.X - Split.Anim_Offset
                          + Handle_Half_Width - Parent_Pos.X) /
                 Float (Parent_Pos.Width);
            when Orientation_Vertical =>
               Percent :=
                 Float (Split.Selected_Handle_Pos.Y - Split.Anim_Offset
                          + Handle_Half_Width - Parent_Pos.Y) /
                 Float (Parent_Pos.Height);
         end case;

         Split.Selected_Handle_Parent.Handles
           (Split.Selected_Handle_Index).Percent := Percent;

         Pointer_Ungrab (Time => 0);

         --  Move the handles, giving the one we just moved priority, and move
         --  the adjacent handles accordingly..
         Move_Handles
           (Split, Split.Selected_Handle_Parent, Split.Selected_Handle_Index);
         Queue_Resize (Split);

         Split.Selected_Handle_Parent := null;
      end if;
      return False;
   end Button_Released;

   -------------------
   -- Button_Motion --
   -------------------

   function Button_Motion
     (Paned : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
   begin
      if Split.Selected_Handle_Parent /= null then
         Draw_Resize_Line (Split);

         case Split.Selected_Handle_Parent.Orientation is
            when Orientation_Horizontal =>
               Split.Selected_Handle_Pos.X := Gint (Get_X (Event))
                 + Split.Selected_Handle_Parent.Handles
                   (Split.Selected_Handle_Index).Position.X;
            when Orientation_Vertical =>
               Split.Selected_Handle_Pos.Y := Gint (Get_Y (Event))
                 + Split.Selected_Handle_Parent.Handles
                   (Split.Selected_Handle_Index).Position.Y;
         end case;

         Draw_Resize_Line (Split);
      end if;
      return False;
   end Button_Motion;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible (Child : Child_Description_Access) return Boolean is
      Tmp     : Child_Description_Access;
   begin
      if Child.Is_Widget then
         return Get_Child_Visible (Child.Widget)
           and then Visible_Is_Set (Child.Widget);
      else
         Tmp := Child.First_Child;
         while Tmp /= null loop
            if Is_Visible (Tmp) then
               return True;
            end if;
            Tmp := Tmp.Next;
         end loop;
         return False;
      end if;
   end Is_Visible;

   -----------------------
   -- Expose_Paned --
   -----------------------

   function Expose_Paned
     (Paned : access Gtk_Widget_Record'Class;
      Event      : Gdk_Event) return Boolean
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Area  : constant Gdk_Rectangle := Get_Area (Event);
      Iter        : Child_Iterator := Start (Split);
      Current     : Child_Description_Access;
      Orientation : Gtk_Orientation;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null;

         if not Current.Is_Widget then
            case Current.Orientation is
               when Orientation_Vertical =>
                  Orientation := Orientation_Horizontal;
               when Orientation_Horizontal =>
                  Orientation := Orientation_Vertical;
            end case;

            for H in Current.Handles'Range loop
               if Is_Visible (Current.Handles (H).Win) then
                  Paint_Handle
                    (Get_Style (Split),
                     Get_Window (Split),
                     State_Normal,
                     Shadow_None,
                     Area,
                     Split,
                     X           => Current.Handles (H).Position.X,
                     Y           => Current.Handles (H).Position.Y,
                     Width       => Current.Handles (H).Position.Width,
                     Height      => Current.Handles (H).Position.Height,
                     Orientation => Orientation);
               end if;
            end loop;
         end if;

         Next (Iter);
      end loop;

      return Default_Expose_Event_Handler
        (Class_From_Type (Parent (Get_Type (Split))))
          (Get_Object (Split), Event);
   end Expose_Paned;

   -------------------
   -- Create_Handle --
   -------------------

   procedure Create_Handle
     (Split  : access Gtkada_Multi_Paned_Record'Class;
      Handle : out Resize_Handle;
      Orientation : Gtk_Orientation)
   is
      Cursor      : Gdk_Cursor;
      Window_Attr : Gdk.Window_Attr.Gdk_Window_Attr;
   begin
      case Orientation is
         when Orientation_Vertical =>
            Gdk_New (Cursor, Sb_V_Double_Arrow);
         when Orientation_Horizontal =>
            Gdk_New (Cursor, Sb_H_Double_Arrow);
      end case;

      Gdk_New (Window_Attr,
               Window_Type => Window_Child,
               Wclass      => Input_Only,   --  Let it be transparent
               Cursor      => Cursor,
               Event_Mask  => Get_Events (Split)
               or Button_Press_Mask
               or Button_Release_Mask
               or Button_Motion_Mask);
      Gdk_New (Handle.Win,
               Parent          => Get_Window (Split),
               Attributes      => Window_Attr,
               Attributes_Mask => Wa_Cursor);

      Set_User_Data (Handle.Win, Split);
      Gdk.Window.Show (Handle.Win);

      Destroy (Cursor);
      Destroy (Window_Attr);
   end Create_Handle;

   ------------------------
   -- Realize_Paned --
   ------------------------

   procedure Realize_Paned
     (Paned : access Gtk_Widget_Record'Class)
   is
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Iter  : Child_Iterator := Start (Split);
      Current : Child_Description_Access;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null;

         if not Current.Is_Widget then
            for H in Current.Handles'Range loop
               if Current.Handles (H).Win = null then
                  Create_Handle
                    (Split, Current.Handles (H), Current.Orientation);
               end if;
            end loop;
         end if;

         Next (Iter);
      end loop;

      Gdk_New       (Split.GC, Get_Window (Split));
      Set_Function  (Split.GC, Invert);
      Set_Exposures (Split.GC, False);
      Set_Subwindow (Split.GC, Include_Inferiors);
      Queue_Resize (Paned);
   end Realize_Paned;

   ------------------
   -- Move_Handles --
   ------------------

   procedure Move_Handles
     (Split  : access Gtkada_Multi_Paned_Record'Class;
      Parent : Child_Description_Access;
      Ref    : Positive := 1)
   is
      Parent_Pos : Gtk_Allocation;

      procedure Resize_Handle (C : Natural);
      --  resize the C-th handle

      -------------------
      -- Resize_Handle --
      -------------------

      procedure Resize_Handle (C : Natural) is
         Handle_Pos : Gtk_Allocation;
      begin
         Compute_Handle_Position (Split, Parent, C, Handle_Pos);
         Parent.Handles (C).Position := Handle_Pos;

         if Is_Visible (Parent.Handles (C).Win) then
            Gdk.Window.Move_Resize
              (Parent.Handles (C).Win,
               X      => Handle_Pos.X,
               Y      => Handle_Pos.Y,
               Width  => Handle_Pos.Width,
               Height => Handle_Pos.Height);

            --  For some reason, some parts of the handles are
            --  sometimes incorrectly exposed when a child is removed. So we
            --  just force an update

            Invalidate_Rect
              (Get_Window (Split),
               (X           => Handle_Pos.X,
                Y           => Handle_Pos.Y,
                Width       => Handle_Pos.Width,
                Height      => Handle_Pos.Height),
               True);
         end if;
      end Resize_Handle;

      Max    : Float;
      Last : Gint;
      Previous : Gint := 0;
   begin
      Compute_Child_Position (Split, Parent, Parent_Pos);

      if Parent.Orientation = Orientation_Vertical then
         Max := Float (Parent_Pos.Width);
      else
         Max := Float (Parent_Pos.Height);
      end if;

      for C in Ref .. Parent.Handles'Last loop
         Last :=  Gint (Parent.Handles (C).Percent * Max);

         if Last <= Previous + Minimum_Child_Width then
            Last := Previous + Minimum_Child_Width;
            Parent.Handles (C).Percent := Float (Last) / Max;

            if Parent.Handles (C).Percent >= 1.0 then
               Parent.Handles (C).Percent := 0.99;
            end if;
         end if;

         Resize_Handle (C);
         Previous := Last;
      end loop;

      if Ref <= Parent.Handles'Last then
         Previous := Gint (Parent.Handles (Ref).Percent * Max);
      else
         Previous := Gint (Max);
      end if;

      for C in reverse Parent.Handles'First .. Ref - 1 loop
         Last :=  Gint (Parent.Handles (C).Percent * Max);
         if Last >= Previous - Minimum_Child_Width then
            Last := Previous - Minimum_Child_Width;
            Parent.Handles (C).Percent := Float (Last) / Max;

            if Parent.Handles (C).Percent <= 0.0 then
               Parent.Handles (C).Percent := 0.01;
            end if;
         end if;

         Resize_Handle (C);
         Previous := Last;
      end loop;
   end Move_Handles;

   ------------------------------
   -- Size_Allocate_Paned --
   ------------------------------

   procedure Size_Allocate_Paned
     (Paned : System.Address; Alloc : Gtk_Allocation)
   is
      Split        : constant Gtkada_Multi_Paned :=
        Gtkada_Multi_Paned (Gtk.Widget.Convert (Paned));
      Iter         : Child_Iterator := Start (Split);
      Current, Tmp : Child_Description_Access;
      Position     : Gtk_Allocation;
      Last_Visible : Integer;
      Requisition  : Gtk_Requisition;
      Parent_Pos   : Gtk_Allocation;

   begin
      --  Register the new size for the window itself
      Set_Allocation (Split, Alloc);

      if not Realized_Is_Set (Split) then
         return;
      end if;

      --  Hide the handles that shouldn't be visible
      loop
         Current := Get (Iter);
         exit when Current = null;

         if not Current.Is_Widget then
            Last_Visible := Current.Handles'Last;
            Tmp := Current.First_Child;
            for H in Current.Handles'Range loop
               if Is_Visible (Tmp) then
                  Show (Current.Handles (H).Win);
                  Last_Visible := H;
               else
                  Hide (Current.Handles (H).Win);
               end if;

               Tmp := Tmp.Next;
            end loop;

            if Current.Handles'Last /= 0 and then not Is_Visible (Tmp) then
               Hide (Current.Handles (Last_Visible).Win);
            end if;
         end if;

         Next (Iter);
      end loop;

      --  Compute the widget size requisition where needed
      Iter := Start (Split);
      loop
         Current := Get (Iter);
         exit when Current = null;

         if Current.Is_Widget then
            if Current.Width = -1 or else Current.Height = -1 then
               Size_Request (Current.Widget, Requisition);
               if Current.Width = -1 then
                  Current.Width := Requisition.Width;
               end if;

               if Current.Height = -1 then
                  Current.Height := Requisition.Height;
               end if;
            end if;

            if (Current.Parent.Orientation = Orientation_Horizontal
                and then Current.Width /= 0)
              or else (Current.Parent.Orientation = Orientation_Vertical
                       and then Current.Height /= 0)
            then
               Tmp := Current.Parent.First_Child;
               for H in Current.Parent.Handles'Range loop
                  if Tmp = Current then
                     Compute_Child_Position
                       (Split, Current.Parent, Parent_Pos);
                     case Current.Parent.Orientation is
                        when Orientation_Horizontal =>
                           Current.Parent.Handles (H).Percent :=
                             Float (Current.Width) / Float (Parent_Pos.Width);
                        when Orientation_Vertical =>
                           Current.Parent.Handles (H).Percent :=
                             Float (Current.Height) /
                               Float (Parent_Pos.Height);
                     end case;

                     if H > Current.Parent.Handles'First then
                        Current.Parent.Handles (H).Percent :=
                          Current.Parent.Handles (H).Percent
                          + Current.Parent.Handles (H - 1).Percent;
                     end if;

                     if Current.Parent.Handles (H).Percent > 1.0 then
                        Current.Parent.Handles (H).Percent := 0.99;
                     end if;

                     Tmp := null;
                     exit;
                  end if;

                  Tmp := Tmp.Next;
               end loop;

               if Tmp = Current then
                  Compute_Child_Position (Split, Current.Parent, Parent_Pos);
                  case Current.Parent.Orientation is
                     when Orientation_Horizontal =>
                        Current.Parent.Handles
                          (Current.Parent.Handles'Last).Percent := 1.0 -
                          Float (Current.Width) / Float (Parent_Pos.Width);
                     when Orientation_Vertical =>
                        Current.Parent.Handles
                          (Current.Parent.Handles'Last).Percent := 1.0 -
                          Float (Current.Height) / Float (Parent_Pos.Height);
                  end case;

                  if Current.Parent.Handles
                    (Current.Parent.Handles'Last).Percent < 0.0
                  then
                     Current.Parent.Handles
                       (Current.Parent.Handles'Last).Percent := 0.1;
                  end if;
               end if;

               Current.Width := 0;
               Current.Height := 0;
            end if;
         end if;

         Next (Iter);
      end loop;

      --  Move the handles first, in case some need to be moved to make enough
      --  space for the children. This must be a separate loop from above,
      --  since the sizes are impacted by what is visible.
      Iter := Start (Split);
      loop
         Current := Get (Iter);
         exit when Current = null;

         if not Current.Is_Widget then
            Move_Handles (Split, Current);
         end if;

         Next (Iter);
      end loop;

      --  Then reposition the children
      --  Since we have a no_window widget, we need to add the coordinates for
      --  the ones of all children.

      Iter := Start (Split);
      loop
         Current := Get (Iter);
         exit when Current = null;

         if Current.Is_Widget then
            Compute_Child_Position (Split, Current, Position);
            if Position.Width < 0 then
               Position.Width := 0;
            end if;

            if Position.Height < 0 then
               Position.Height := 0;
            end if;

            Size_Allocate (Current.Widget, Position);
         end if;

         Next (Iter);
      end loop;
   end Size_Allocate_Paned;

   ----------------
   -- Add_Handle --
   ----------------

   procedure Add_Handle
     (Split : access Gtkada_Multi_Paned_Record'Class;
      Parent : Child_Description_Access;
      Child : Child_Description_Access;
      Old_Handles : Handles_Array_Access := null)
   is
      Old        : Handles_Array_Access := Parent.Handles;
      Count      : Natural := 1;
      Index      : Natural := 1;
      Tmp        : Child_Description_Access;
      Width, Start : Float;
   begin
      if Old_Handles /= null then
         Count := Old_Handles'Length;
      end if;

      Tmp := Parent.First_Child;
      while Tmp /= Child loop
         Index := Index + 1;
         Tmp := Tmp.Next;
      end loop;

      Parent.Handles := new Handles_Array (Old'First .. Old'Last + Count);
      Parent.Handles (Parent.Handles'First .. Index - 1) :=
        Old (Old'First .. Index - 1);
      Parent.Handles (Index + Count .. Parent.Handles'Last) :=
        Old (Index .. Old'Last);

      if Old'Last = 0 then
         Width := 1.0;
         Start := 0.0;
      elsif Index in Old'First + 1 .. Old'Last then
         Width := Old (Index).Percent - Old (Index - 1).Percent;
         Start := Old (Index - 1).Percent;
      elsif Index = 1 then
         Width := Old (Index).Percent;
         Start := 0.0;
      else
         Width := 1.0 - Old (Old'Last).Percent;
         Start := Old (Old'Last).Percent;
      end if;

      if Old_Handles = null then
         if Count = 1 then
            Width := Width * 0.5;
         else
            Width := Width / Float (Count + 1);
         end if;
      end if;

      for H in Index .. Index + Count - 1 loop
         if Old_Handles /= null then
            Parent.Handles (H).Percent :=
              Width * Old_Handles (Old_Handles'First + H - Index).Percent
              + Start;
         else
            Parent.Handles (H).Percent  := Width + Start;
         end if;

         if Realized_Is_Set (Split) then
            Create_Handle (Split, Parent.Handles (H), Parent.Orientation);
         end if;
      end loop;

      Unchecked_Free (Old);
   end Add_Handle;

   --------------------
   -- Split_Internal --
   --------------------

   procedure Split_Internal
     (Win           : access Gtkada_Multi_Paned_Record'Class;
      Ref_Widget    : Gtk_Widget;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk_Orientation;
      Width, Height : Glib.Gint := 0;
      After         : Boolean := True)
   is
      Iter : Child_Iterator := Start (Win);
      Current, Tmp2, Pane : Child_Description_Access;
   begin
      if Ref_Widget /= null then
         loop
            Current := Get (Iter);
            exit when Current = null
              or else (Current.Is_Widget and then Current.Widget = Ref_Widget);
            Next (Iter);
         end loop;
      end if;

      if Current /= null then
         if Current.Parent.Orientation = Orientation then
            Tmp2 := new Child_Description'
              (Parent      => Current.Parent,
               Next        => Current.Next,
               Is_Widget   => True,
               Width       => Width,
               Height      => Height,
               Widget      => Gtk_Widget (New_Child));
            if not After then
               Tmp2.Widget    := Current.Widget;
               Current.Widget := Gtk_Widget (New_Child);
               Tmp2.Width     := Current.Width;
               Tmp2.Height    := Current.Height;
               Current.Width  := Width;
               Current.Height := Height;
            end if;

            Add_Handle (Win, Current.Parent, Current);
            Current.Next := Tmp2;
         else
            Pane := new Child_Description'
              (Parent      => Current.Parent,
               Next        => Current.Next,
               Is_Widget   => False,
               Orientation => Orientation,
               First_Child => Current,
               Handles     => new Handles_Array (1 .. 1));
            Pane.Handles (1) :=
              ((0, 0, 0, 0), Win => null, Percent => 0.5);
            if Realized_Is_Set (Win) then
               Create_Handle (Win, Pane.Handles (1), Pane.Orientation);
            end if;

            Current.Parent := Pane;

            if Pane.Parent.First_Child = Current then
               Pane.Parent.First_Child := Pane;
            else
               Tmp2 := Pane.Parent.First_Child;
               while Tmp2.Next /= Current loop
                  Tmp2 := Tmp2.Next;
               end loop;
               Tmp2.Next := Pane;
            end if;

            Current.Next := new Child_Description'
              (Parent      => Pane,
               Next        => null,
               Is_Widget   => True,
               Width       => Width,
               Height      => Height,
               Widget      => Gtk_Widget (New_Child));
            if not After then
               Current.Next.Widget := Current.Widget;
               Current.Widget      := Gtk_Widget (New_Child);
               Current.Next.Width  := Current.Width;
               Current.Next.Height := Current.Height;
               Current.Width  := Width;
               Current.Height := Height;
            end if;
         end if;

      --   Ref_Widget not found => Do nothing unless there is no child yet
      elsif Win.Children = null then
         Tmp2 := new Child_Description'
           (Parent      => null,
            Next        => null,
            Is_Widget   => True,
            Width       => Width,
            Height      => Height,
            Widget      => Gtk_Widget (New_Child));
         Win.Children := new Child_Description'
           (Parent      => null,
            Next        => null,
            Is_Widget   => False,
            Orientation => Orientation,
            First_Child => Tmp2,
            Handles     => new Handles_Array (1 .. 0));
         Tmp2.Parent := Win.Children;

      elsif Win.Children /= null then
         Tmp2 := new Child_Description'
           (Parent      => Win.Children,
            Next        => null,
            Is_Widget   => True,
            Width       => Width,
            Height      => Height,
            Widget      => Gtk_Widget (New_Child));

         if Win.Children.First_Child = null then
            Win.Children.First_Child := Tmp2;
         else
            Current := Win.Children.First_Child;
            while Current.Next /= null loop
               Current := Current.Next;
            end loop;

            Current.Next := Tmp2;
            Add_Handle (Win, Win.Children, Current);
         end if;
      end if;

      Put (Win, New_Child, 0, 0);
   end Split_Internal;

   -----------
   -- Split --
   -----------

   procedure Split
     (Win         : access Gtkada_Multi_Paned_Record;
      Ref_Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk_Orientation;
      Width, Height : Glib.Gint := 0;
      After       : Boolean := True) is
   begin
      Split_Internal
        (Win, Gtk_Widget (Ref_Widget), New_Child, Orientation,
         Width, Height, After);
   end Split;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Win        : access Gtkada_Multi_Paned_Record;
      New_Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation :=
        Gtk.Enums.Orientation_Horizontal;
      Width, Height : Glib.Gint := 0) is
   begin
      Split_Internal (Win, null, New_Child, Orientation, Width, Height);
   end Add_Child;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Win           : access Gtkada_Multi_Paned_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Width, Height : Glib.Gint := 0)
   is
      Iter    : Child_Iterator := Start (Win);
      Current : Child_Description_Access;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null;

         if Current.Is_Widget
           and then Current.Widget = Gtk_Widget (Widget)
         then
            Current.Width := Width;
            Current.Height := Height;
            exit;
         end if;

         Next (Iter);
      end loop;

      Queue_Resize (Win);
   end Set_Size;

end Gtkada.Multi_Paned;
