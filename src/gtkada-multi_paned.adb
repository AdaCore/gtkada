-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                 Copyright (C) 2003 ACT-Europe                     --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

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
with GNAT.IO;              use GNAT.IO;
with Ada.Exceptions;       use Ada.Exceptions;
with System.Address_Image;

package body Gtkada.Multi_Paned is

   Traces : constant Boolean := False;
   --  Whether debug traces should be displayed on stdout

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

   procedure Remove_Child
      (Split : access Gtkada_Multi_Paned_Record'Class;
       Pane : Child_Description_Access);
   --  Remove a specific pane

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


   function Get (Iter : Child_Iterator) return Child_Description_Access;
   --  Return the current child. You must move to Next before destroying
   --  the returned value, if you need to.
   --  Null is returned when there are no more children.

   procedure Split_Internal
     (Win         : access Gtkada_Multi_Paned_Record'Class;
      Ref_Widget  : Gtk_Widget;
      Ref_Pane    : Pane;
      Use_Ref_Pane  : Boolean;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk_Orientation;
      Fixed_Size  : Boolean := False;
      Width, Height : Glib.Gint := 0;
      After       : Boolean := True;
      Include_Siblings : Boolean := False);
   --  Internal version of Split_Horizontal and Split_Vertical.
   --  If Use_Ref_Pane is true, then all split are done with regards to
   --  Ref_Pane, otherwise they are done relative to Ref_Widget.
   --  See the doc for Split_Group for more info on Include_Siblings

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

   procedure Compute_Resize_Handle_Percent
     (Split : access Gtkada_Multi_Paned_Record'Class);
   --  Compute the new percent value for the handle being dragged

   procedure Dump
     (Split : access Gtkada_Multi_Paned_Record'Class;
      Child : Child_Description_Access;
      Prefix : String := "");
   --  Dump to stdout the status of the multipaned

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Split : access Gtkada_Multi_Paned_Record'Class;
      Child : Child_Description_Access;
      Prefix : String := "")
   is
      Tmp : Child_Description_Access;
      H   : Natural;
      Alloc : Gtk_Allocation;
   begin
      if Child = null then
         Put_Line ("<null>");

      elsif Child.Is_Widget then
         Compute_Child_Position (Split, Child, Alloc);
         Put_Line (Prefix & "<widget req_width=""" & Child.Width'Img
                   & """ req_height="""   & Child.Height'Img
                   & """ width=""" & Alloc.Width'Img
                   & """ height=""" & Alloc.Height'Img
                   & """ visible=""" & Is_Visible (Child)'Img
                   & """ fixed_size=""" & Child.Fixed_Size'Img
                   & """ widget="""
                   & System.Address_Image (Child.Widget.all'Address)
                   & """>");
      else
         Compute_Child_Position (Split, Child, Alloc);
         Put_Line (Prefix & "<pane orientation="
                   & Child.Orientation'Img
                   & """ handles=""" & Child.Handles'Length'Img
                   & """ req_width=""" & Child.Width'Img
                   & """ req_height=""" & Child.Height'Img
                   & """ width=""" & Alloc.Width'Img
                   & """ height=""" & Alloc.Height'Img
                   & """>");
         Tmp := Child.First_Child;
         H := Child.Handles'First;
         while Tmp /= null loop
            Dump (Split, Tmp, Prefix & "  ");
            if H <= Child.Handles'Last then
               Put_Line (Prefix & "  <handle percent="""
                         & Child.Handles (H).Percent'Img & """>");
               H := H + 1;
            end if;
            Tmp := Tmp.Next;
         end loop;

         if H <= Child.Handles'Last then
            Put_Line ("ERROR: Handle not followed by child");
         end if;
      end if;
   end Dump;

   -------------------------
   -- Set_Opaque_Resizing --
   -------------------------

   procedure Set_Opaque_Resizing
     (Win : access Gtkada_Multi_Paned_Record; Opaque : Boolean)
   is
   begin
      Win.Opaque_Resizing := Opaque;
   end Set_Opaque_Resizing;

   ----------
   -- Free --
   ----------

   procedure Free (Child : in out Child_Description_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Child_Description, Child_Description_Access);
   begin
      if not Child.Is_Widget then
         for H in Child.Handles'Range loop
            if Child.Handles (H).Win /= null then
               Destroy (Child.Handles (H).Win);
            end if;
         end loop;

         Unchecked_Free (Child.Handles);
      end if;

      Unchecked_Free (Child);
   end Free;

   -----------
   -- Start --
   -----------

   function Start
     (Win : access Gtkada_Multi_Paned_Record) return Child_Iterator is
   begin
      return (Current => Win.Children, Depth => 0);
   end Start;

   ---------
   -- Get --
   ---------

   function Get (Iter : Child_Iterator) return Child_Description_Access is
   begin
      return Iter.Current;
   end Get;

   ---------------
   -- Get_Depth --
   ---------------

   function Get_Depth (Iter : Child_Iterator) return Natural is
   begin
      return Iter.Depth;
   end Get_Depth;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Child_Iterator) return Boolean is
   begin
      return Iter.Current = null;
   end At_End;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (Iter : Child_Iterator) return Gtk.Widget.Gtk_Widget is
   begin
      if Iter.Current /= null and then Iter.Current.Is_Widget then
         return Iter.Current.Widget;
      else
         return null;
      end if;
   end Get_Widget;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
     (Iter : Child_Iterator) return Gtk.Enums.Gtk_Orientation is
   begin
      if Iter.Current /= null and then not Iter.Current.Is_Widget then
         return Iter.Current.Orientation;
      else
         return Orientation_Horizontal;
      end if;
   end Get_Orientation;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Child_Iterator) is
   begin
      if Iter.Current = null then
         null;
      elsif not Iter.Current.Is_Widget
        and then Iter.Current.First_Child /= null
      then
         Iter.Current := Iter.Current.First_Child;
         Iter.Depth   := Iter.Depth + 1;
      else
         while Iter.Current /= null
           and then Iter.Current.Next = null
         loop
            Iter.Current := Iter.Current.Parent;

            if Iter.Current /= null then
               Iter.Depth   := Iter.Depth - 1;
            end if;
         end loop;

         if Iter.Current /= null then
            Iter.Current := Iter.Current.Next;
         end if;
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
      use type Widget_List.Glist;
      Split : constant Gtkada_Multi_Paned := Gtkada_Multi_Paned (Paned);
      Items, Tmp : Widget_List.Glist := Get_Children (Split);
   begin
      while Tmp /= Widget_List.Null_List loop
         Remove (Split, Widget_List.Get_Data (Tmp));
         Tmp := Widget_List.Next (Tmp);
      end loop;

      Widget_List.Free (Items);

      Free (Split.Children);

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
      Child : constant Gtk_Widget := Gtk_Widget (To_Object (Args, 1));
      Iter : Child_Iterator;
      Current : Child_Description_Access;
   begin
      Iter := Start (Split);

      loop
         Current := Get (Iter);
         exit when Current = null
           or else (Current.Is_Widget and then Current.Widget = Child);
         Next (Iter);
      end loop;
      Remove_Child (Split, Current);
   end Remove_Child;

   ------------------
   -- Remove_Child --
   ------------------

   procedure Remove_Child
      (Split : access Gtkada_Multi_Paned_Record'Class;
       Pane : Child_Description_Access)
   is
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
                  Child.First_Child.Next := Child.Next;
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

            Add_Handle (Split, Child.Parent, Child, Child.Handles);

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

      Current : Child_Description_Access := Pane;
      Old_Handles  : Handles_Array_Access;
      C, D : Natural;
      Tmp, Parent : Child_Description_Access;
   begin
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

         if Parent /= null
            and then not Parent.Is_Widget
            and then Parent.Parent /= null
            and then Parent.First_Child = null
         then
            Remove_Child (Split, Parent);
         end if;

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

            if Child.Parent.Handles (Handle_Index).Win /= null
              and then Is_Visible (Child.Parent.Handles (Handle_Index).Win)
            then
               Percent := Child.Parent.Handles (Handle_Index).Percent;
            end if;

            Handle_Index := Handle_Index + 1;
         end loop;

         X_Offset := 0;
         for H in 1 .. Handle_Index - 1 loop
            if Child.Parent.Handles (H).Win /= null
              and then Is_Visible (Child.Parent.Handles (H).Win)
            then
               X_Offset := Handle_Half_Width;
               exit;
            end if;
         end loop;

         while Handle_Index <= Child.Parent.Handles'Last
           and then
             (Child.Parent.Handles (Handle_Index).Win = null
              or else not Is_Visible (Child.Parent.Handles (Handle_Index).Win))
         loop
            Handle_Index := Handle_Index + 1;
         end loop;

         if Handle_Index > Child.Parent.Handles'Last then
            Percent_End := 1.0;
            W_Offset := X_Offset;
         else
            Percent_End := Child.Parent.Handles (Handle_Index).Percent;

            --  If this is the first visible child, do not add the handle
            --  width
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

   -----------------------------------
   -- Compute_Resize_Handle_Percent --
   -----------------------------------

   procedure Compute_Resize_Handle_Percent
     (Split : access Gtkada_Multi_Paned_Record'Class)
   is
      Parent_Pos : Gtk_Allocation;
      Percent    : Float;
   begin
      Compute_Child_Position
        (Split, Split.Selected_Handle_Parent, Parent_Pos);

      case Split.Selected_Handle_Parent.Orientation is
         when Orientation_Vertical =>
            Percent := Float
              (Split.Selected_Handle_Pos.Y - Split.Anim_Offset
                 + Handle_Half_Width - Parent_Pos.Y) /
                Float (Parent_Pos.Height);
         when Orientation_Horizontal =>
            Percent := Float
              (Split.Selected_Handle_Pos.X - Split.Anim_Offset
                 + Handle_Half_Width - Parent_Pos.X) /
                Float (Parent_Pos.Width);
      end case;

      if Percent < 0.0 then
         Percent := 0.0;
      elsif Percent > 1.0 then
         Percent := 1.0;
      end if;

      Split.Selected_Handle_Parent.Handles
        (Split.Selected_Handle_Index).Percent := Percent;
   end Compute_Resize_Handle_Percent;

   ----------------------
   -- Draw_Resize_Line --
   ----------------------

   procedure Draw_Resize_Line
     (Split : access Gtkada_Multi_Paned_Record'Class) is
   begin
      if not Split.Opaque_Resizing then
         Draw_Line
           (Get_Window (Split),
            Split.GC, Split.Selected_Handle_Pos.X,
            Split.Selected_Handle_Pos.Y,
            Split.Selected_Handle_Pos.X + Split.Selected_Handle_Pos.Width,
            Split.Selected_Handle_Pos.Y + Split.Selected_Handle_Pos.Height);
      else
         Compute_Resize_Handle_Percent (Split);
         Queue_Resize (Split);
      end if;
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
   begin
      if Split.Selected_Handle_Parent /= null then
         Draw_Resize_Line (Split);

         if not Split.Opaque_Resizing then
            Compute_Resize_Handle_Percent (Split);
         end if;

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
      C : Child_Description_Access;
   begin
      if Split.Selected_Handle_Parent /= null then

         --  Make sure none of the widgets has a fixed size, or
         --  the resizing won't take place.

         C := Split.Selected_Handle_Parent.First_Child;
         for H in 1 .. Split.Selected_Handle_Index - 1 loop
            C := C.Next;
         end loop;

         if C.Is_Widget then
            C.Fixed_Size := False;
            C.Width := 0;
            C.Height := 0;
         end if;

         if C.Next /= null and then C.Next.Is_Widget then
            C.Next.Fixed_Size := False;
            C.Next.Width := 0;
            C.Next.Height := 0;
         end if;

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
         Max := Float (Parent_Pos.Height);
      else
         Max := Float (Parent_Pos.Width);
      end if;

      for C in Ref .. Parent.Handles'Last loop
         if Is_Visible (Parent.Handles (C).Win) then
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
         end if;
      end loop;

      if Ref <= Parent.Handles'Last then
         Previous := Gint (Parent.Handles (Ref).Percent * Max);
      else
         Previous := Gint (Max);
      end if;

      for C in reverse Parent.Handles'First .. Ref - 1 loop
         if Is_Visible (Parent.Handles (C).Win) then
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
         end if;
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

      procedure Compute_Requisition (Current : Child_Description_Access);
      --  Propagate the children size requisition to the parent, so that we can
      --  later compute the ratio that each children should occupy.

      procedure Propagate_Sizes
        (Current : Child_Description_Access; Width, Height : Allocation_Int);
      --  Compute the size that each children of Current should really
      --  occupy on the screen. This depends on the size requisition for each
      --  of the children, and the total size allocates to Current (given by
      --  Alloc)

      -------------------------
      -- Compute_Requisition --
      -------------------------

      procedure Compute_Requisition (Current : Child_Description_Access) is
         Requisition : Gtk_Requisition;
         Tmp         : Child_Description_Access;
      begin
         if Current /= null then
            if Current.Is_Widget and then Is_Visible (Current) then
               if Current.Width = -1 or else Current.Height = -1 then
                  Size_Request (Current.Widget, Requisition);
                  if Current.Width = -1 then
                     Current.Width := Requisition.Width;
                  end if;

                  if Current.Height = -1 then
                     Current.Height := Requisition.Height;
                  end if;
               end if;

            elsif not Current.Is_Widget then
               Current.Width  := 0;
               Current.Height := 0;

               Tmp := Current.First_Child;
               while Tmp /= null loop
                  Compute_Requisition (Tmp);

                  case Current.Orientation is
                     when Orientation_Horizontal =>
                        Current.Width := Current.Width + Tmp.Width;
                        Current.Height := Gint'Max
                          (Current.Height, Tmp.Height);
                     when Orientation_Vertical =>
                        Current.Width := Gint'Max
                          (Current.Width, Tmp.Width);
                        Current.Height := Current.Height + Tmp.Height;
                  end case;

                  Tmp := Tmp.Next;
               end loop;
            end if;
         end if;
         if Traces then
            Put_Line ("Compute_Requisition, at end");
            Dump (Split, Split.Children);
         end if;
      end Compute_Requisition;

      ---------------------
      -- Propagate_Sizes --
      ---------------------

      procedure Propagate_Sizes
        (Current : Child_Description_Access; Width, Height : Allocation_Int)
      is
         Tmp    : Child_Description_Access;
         Changed : Boolean;
      begin
         if Current /= null
           and then not Current.Is_Widget
         then
            Tmp := Current.First_Child;
            for Handle in Current.Handles'Range loop
               Changed := False;

               case Current.Orientation is
                  when Orientation_Horizontal =>
                     if Tmp.Width /= 0 then
                        Current.Handles (Handle).Percent :=
                          Float (Tmp.Width) / Float (Current.Width);
                        Changed := True;
                     end if;
                     Propagate_Sizes
                       (Tmp,
                        Allocation_Int
                          (Float (Width) * Current.Handles (Handle).Percent),
                        Height);

                  when Orientation_Vertical =>
                     if Tmp.Height /= 0 then
                        Current.Handles (Handle).Percent :=
                          Float (Tmp.Height) / Float (Current.Height);
                        Changed := True;
                     end if;
                     Propagate_Sizes
                       (Tmp,
                        Width,
                        Allocation_Int
                          (Float (Height) * Current.Handles (Handle).Percent));
               end case;

               if Changed then
                  if Handle > Current.Handles'First then
                     Current.Handles (Handle).Percent :=
                       Current.Handles (Handle).Percent
                       + Current.Handles (Handle - 1).Percent;
                  end if;
               end if;

               if Current.Handles (Handle).Percent > 1.0 then
                  Current.Handles (Handle).Percent := 0.99;
               end if;

               Tmp := Tmp.Next;
            end loop;

            case Current.Orientation is
               when Orientation_Horizontal =>
                  Propagate_Sizes
                    (Tmp,
                     Allocation_Int
                       (Float (Width)
                        * (1.0
                           - Current.Handles (Current.Handles'Last).Percent)),
                     Height);
               when Orientation_Vertical =>
                  Propagate_Sizes
                    (Tmp,
                     Width,
                     Allocation_Int
                       (Float (Height)
                        * (1.0
                           - Current.Handles (Current.Handles'Last).Percent)));
            end case;

         elsif Current /= null then
            if not Current.Fixed_Size then
               Current.Width := 0;
               Current.Height := 0;
            end if;
         end if;
      end Propagate_Sizes;

   begin
      Set_Allocation (Split, Alloc);

      if not Realized_Is_Set (Split) then
         return;
      end if;

      if Get_Has_Window (Split) then
         Gdk.Window.Move_Resize
           (Get_Window (Split),
            X      => Alloc.X,
            Y      => Alloc.Y,
            Width  => Alloc.Width,
            Height => Alloc.Height);
      end if;

      --  Hide the handles that shouldn't be visible
      loop
         Current := Get (Iter);
         exit when Current = null;

         if not Current.Is_Widget then
            Tmp := Current.First_Child;

            --  For GPS's sake, we handle the following configuration
            --      child visible / child invisible / child visible
            --  by allocating the maximum space for the first child, and less
            --  for the last child (think of the docks in GPS)

            if Tmp /= null then
               Tmp := Tmp.Next;
            end if;

            for H in Current.Handles'Range loop
               if Is_Visible (Tmp) then
                  Show (Current.Handles (H).Win);
               else
                  Hide (Current.Handles (H).Win);
               end if;

               Tmp := Tmp.Next;
            end loop;

            if Current.Handles'Last /= 0
              and then Current.First_Child /= null
              and then not Is_Visible (Current.First_Child)
            then
               Hide (Current.Handles (Current.Handles'First).Win);
            end if;
         end if;

         Next (Iter);
      end loop;

      if Traces then
         Put_Line ("Size_Allocate_Multi_Paned: Recomputed children sizes");
      end if;
      Compute_Requisition (Split.Children);
      Propagate_Sizes (Split.Children, Alloc.Width, Alloc.Height);

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

   exception
      when E : others =>
         pragma Debug
           (Put_Line ("Unexpected exception " & Exception_Information (E)));
         null;
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

   -------------------
   -- Splitted_Area --
   -------------------

   function Splitted_Area
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      After         : Boolean := True) return Gtk.Widget.Gtk_Widget
   is
      Current, Tmp : Child_Description_Access;
      Iter    : Child_Iterator := Start (Win);
   begin
      loop
         Current := Get (Iter);
         exit when Current = null
           or else (Current.Is_Widget
                    and then Current.Widget = Gtk_Widget (Ref_Widget));
         Next (Iter);
      end loop;

      if Current /= null
        and then Current.Parent.Orientation = Orientation
      then
         if After then
            Current := Current.Next;
         else
            Tmp := Current.Parent.First_Child;
            while Tmp /= null
              and then Tmp.Next /= Current
            loop
               Tmp := Tmp.Next;
            end loop;

            Current := Tmp;
         end if;

         while Current /= null and then not Current.Is_Widget loop
            Current := Current.First_Child;
         end loop;

         if Current /= null then
            return Current.Widget;
         end if;
      end if;

      return null;
   end Splitted_Area;

   --------------
   -- Get_Pane --
   --------------

   function Get_Pane
     (Win    : access Gtkada_Multi_Paned_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Pane
   is
      Iter    : Child_Iterator := Start (Win);
      Current : Child_Description_Access;
   begin
      loop
         Current := Get (Iter);
         exit when Current = null
           or else (Current.Is_Widget
                    and then Current.Widget = Gtk_Widget (Widget));
         Next (Iter);
      end loop;

      if Current /= null then
         return Pane (Current.Parent);
      else
         return null;
      end if;
   end Get_Pane;

   --------------------
   -- Split_Internal --
   --------------------

   procedure Split_Internal
     (Win           : access Gtkada_Multi_Paned_Record'Class;
      Ref_Widget    : Gtk_Widget;
      Ref_Pane      : Pane;
      Use_Ref_Pane  : Boolean;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := 0;
      After         : Boolean := True;
      Include_Siblings : Boolean := False)
   is
      procedure Add_After_All_Children (Parent : Child_Description_Access);
      --  Add the new child at the end of the child list for Parent

      procedure Add_As_First_Child (Parent : Child_Description_Access);
      --  Add the new child as the first child of parent

      procedure Add_In_List
        (Parent   : Child_Description_Access;
         Ref_Item : Child_Description_Access;
         After    : Boolean);
      --  Add a new child to Parent, before or after Ref_Item

      function Create_Or_Get_Parent
        (Current          : Child_Description_Access;
         Orientation      : Gtk_Orientation;
         Force            : Boolean := False;
         Include_Siblings : Boolean := False) return Child_Description_Access;
      --  Create a new parent for Current with the orientation specified in
      --  parameter to Split_Internal.
      --  If Force is True, then a new parent is created, even if the current
      --  one already has the right orientation.
      --  If Include_Siblings is True, then Current and all its siblings are
      --  moved to the new parent

      procedure Create_Parent_For_Siblings
        (Current : Child_Description_Access);
      --  Transform the following tree:
      --      Horizontal                     Horizontal
      --    /                 into          /
      --    1 -> 2 -> 3                     1 -> Vertical
      --                                          /
      --                                          Horizontal -> 4
      --                                          /
      --                                          2 -> 3
      --  This is used for the implementation of Split_Group

      --------------------------
      -- Create_Or_Get_Parent --
      --------------------------

      function Create_Or_Get_Parent
        (Current          : Child_Description_Access;
         Orientation      : Gtk_Orientation;
         Force            : Boolean := False;
         Include_Siblings : Boolean := False) return Child_Description_Access
      is
         Pane, Tmp2 : Child_Description_Access;
         Count : Integer;
         Old_Handles : Handles_Array_Access;
      begin
         if not Force
            and then Current.Parent /= null
            and then Current.Parent.Orientation = Orientation
         then
            return Current.Parent;
         end if;

         Pane := new Child_Description'
           (Parent      => Current.Parent,
            Next        => Current.Next,
            Is_Widget   => False,
            Orientation => Orientation,
            First_Child => Current,
            Width       => 0,
            Height      => 0,
            Handles     => null);

         Current.Parent := Pane;

         if Pane.Parent = null then
            Win.Children := Pane;
         elsif Pane.Parent.First_Child = Current then
            Pane.Parent.First_Child := Pane;
         else
            Tmp2 := Pane.Parent.First_Child;
            while Tmp2.Next /= Current loop
               Tmp2 := Tmp2.Next;
            end loop;
            Tmp2.Next := Pane;
         end if;

         if Include_Siblings then
            Pane.Next    := null;
            Tmp2 := Current.Next;
            Count := 0;
            while Tmp2 /= null loop
               Tmp2.Parent := Current.Parent;
               Count := Count + 1;
               Tmp2 := Tmp2.Next;
            end loop;
            Pane.Handles := new Handles_Array (1 .. Count);
         else
            Pane.Handles := new Handles_Array (1 .. 0);
            Current.Next := null;
         end if;

         if Pane.Parent /= null then
            Count := 0;
            Tmp2 := Pane.Parent.First_Child.Next;
            while Tmp2 /= null loop
               Count := Count + 1;
               Tmp2 := Tmp2.Next;
            end loop;

            Old_Handles := Pane.Parent.Handles;
            Pane.Parent.Handles := new Handles_Array (1 .. Count);
            Pane.Parent.Handles (1 .. Count) := Old_Handles (1 .. Count);
            Unchecked_Free (Old_Handles);
         end if;

         return Pane;
      end Create_Or_Get_Parent;

      --------------------------------
      -- Create_Parent_For_Siblings --
      --------------------------------

      procedure Create_Parent_For_Siblings
        (Current : Child_Description_Access)
      is
         Pane1, Pane : Child_Description_Access;
         pragma Unreferenced (Pane);
      begin
         Pane1 := Create_Or_Get_Parent
           (Current          => Current,
            Orientation      => Current.Parent.Orientation,
            Force            => True,
            Include_Siblings => True);
         Pane := Create_Or_Get_Parent
           (Current          => Pane1,
            Orientation      => Orientation,
            Force            => True,
            Include_Siblings => False);
      end Create_Parent_For_Siblings;

      -----------------
      -- Add_In_List --
      -----------------

      procedure Add_In_List
        (Parent   : Child_Description_Access;
         Ref_Item : Child_Description_Access;
         After    : Boolean)
      is
         Tmp  : Child_Description_Access := Parent.First_Child;
         Tmp2 : Child_Description_Access;
      begin
         Tmp2 := new Child_Description'
           (Parent      => Parent,
            Next        => null,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
            Width       => Width,
            Height      => Height,
            Widget      => Gtk_Widget (New_Child));

         if After then
            while Tmp /= Ref_Item loop
               Tmp := Tmp.Next;
            end loop;
            Tmp2.Next := Tmp.Next;
            Tmp.Next := Tmp2;
            Add_Handle (Win, Parent, Tmp);
         else
            if Parent.First_Child = Ref_Item then
               Tmp2.Next := Parent.First_Child;
               Parent.First_Child := Tmp2;
            else
               while Tmp.Next /= Ref_Item loop
                  Tmp := Tmp.Next;
               end loop;
               Tmp2.Next := Tmp.Next;
               Tmp.Next := Tmp2;
            end if;
            Add_Handle (Win, Parent, Tmp2);
         end if;
      end Add_In_List;

      ------------------------
      -- Add_As_First_Child --
      ------------------------

      procedure Add_As_First_Child (Parent : Child_Description_Access) is
         Tmp : Child_Description_Access;
      begin
         Tmp := new Child_Description'
           (Parent      => Parent,
            Next        => Parent.First_Child,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
            Width       => Width,
            Height      => Height,
            Widget      => Gtk_Widget (New_Child));
         Parent.First_Child := Tmp;
         Add_Handle (Win, Parent, Tmp);
      end Add_As_First_Child;

      ----------------------------
      -- Add_After_All_Children --
      ----------------------------

      procedure Add_After_All_Children (Parent : Child_Description_Access) is
         Tmp, Tmp2 : Child_Description_Access;
      begin
         Tmp := new Child_Description'
           (Parent      => Parent,
            Next        => null,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
            Width       => Width,
            Height      => Height,
            Widget      => Gtk_Widget (New_Child));
         if Parent.First_Child = null then
            Parent.First_Child := Tmp;
         else
            Tmp2 := Parent.First_Child;
            while Tmp2.Next /= null loop
               Tmp2 := Tmp2.Next;
            end loop;
            Tmp2.Next := Tmp;
         end if;
         Add_Handle (Win, Parent, Tmp2);
      end Add_After_All_Children;

      Current, Tmp2, Pane : Child_Description_Access;
   begin
      if Ref_Pane /= null then
         --  Split specific pane
         Current := Child_Description_Access (Ref_Pane);
      elsif Ref_Widget = null then
         if Use_Ref_Pane then
            --  Split main window
            Current := Win.Children;
         else
            Current := null;
         end if;
      else
         declare
            Iter    : Child_Iterator := Start (Win);
         begin
            loop
               Current := Get (Iter);
               exit when Current = null
                 or else (Current.Is_Widget
                          and then Current.Widget = Ref_Widget);
               Next (Iter);
            end loop;
         end;
      end if;

      if Current /= null then
         if not Current.Is_Widget then
            if Current.Orientation = Orientation then
               if After then
                  Add_After_All_Children (Current);
               else
                  Add_As_First_Child (Current);
               end if;
            else  --  Current.Orientation /= Orientation
               Pane := Create_Or_Get_Parent (Current, Orientation);
               Add_In_List (Pane, Current, After);
            end if;

         else  --  Current.Is_Widget
            if Include_Siblings then
               Create_Parent_For_Siblings (Current);
               Add_After_All_Children (Current.Parent.Parent);
            else
               Pane := Create_Or_Get_Parent (Current, Orientation);
               Add_In_List (Pane, Current, After);
            end if;
         end if;

      --   Current = null => Do nothing unless there is no child yet
      elsif Win.Children = null then
         Tmp2 := new Child_Description'
           (Parent      => null,
            Next        => null,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
            Width       => Width,
            Height      => Height,
            Widget      => Gtk_Widget (New_Child));
         Win.Children := new Child_Description'
           (Parent      => null,
            Next        => null,
            Is_Widget   => False,
            Orientation => Orientation,
            Width       => 0,
            Height      => 0,
            First_Child => Tmp2,
            Handles     => new Handles_Array (1 .. 0));
         Tmp2.Parent := Win.Children;

      elsif Win.Children /= null then
         Tmp2 := new Child_Description'
           (Parent      => Win.Children,
            Next        => null,
            Is_Widget   => True,
            Fixed_Size  => Fixed_Size,
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

      if Traces then
         Put_Line ("Split_Internal: After inserting "
                   & System.Address_Image (New_Child.all'Address));
         Dump (Win, Win.Children);
      end if;
   end Split_Internal;

   -----------
   -- Split --
   -----------

   procedure Split
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Pane      : Pane;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := 0;
      After         : Boolean := True) is
   begin
      Split_Internal
        (Win, null, Ref_Pane, True, New_Child, Orientation,
         Fixed_Size, Width, Height, After);
   end Split;

   -----------
   -- Split --
   -----------

   procedure Split
     (Win         : access Gtkada_Multi_Paned_Record;
      Ref_Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      New_Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation : Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := 0;
      After       : Boolean := True) is
   begin
      Split_Internal
        (Win, Gtk_Widget (Ref_Widget), null, False, New_Child, Orientation,
         Fixed_Size, Width, Height, After);
   end Split;

   -----------------
   -- Split_Group --
   -----------------

   procedure Split_Group
     (Win           : access Gtkada_Multi_Paned_Record;
      Ref_Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      New_Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := 0;
      After         : Boolean := True)
   is
   begin
      Split_Internal
        (Win, Gtk_Widget (Ref_Widget), null, False, New_Child, Orientation,
         Fixed_Size, Width, Height, After,
         Include_Siblings => True);
   end Split_Group;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (Win        : access Gtkada_Multi_Paned_Record;
      New_Child  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Orientation   : Gtk.Enums.Gtk_Orientation :=
        Gtk.Enums.Orientation_Horizontal;
      Fixed_Size    : Boolean := False;
      Width, Height : Glib.Gint := 0;
      After         : Boolean := True) is
   begin
      Split_Internal
        (Win, null, null, False,
         New_Child, Orientation, Fixed_Size, Width, Height, After);
   end Add_Child;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Win           : access Gtkada_Multi_Paned_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Width, Height : Glib.Gint := 0;
      Fixed_Size    : Boolean := False)
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
            Current.Fixed_Size := Fixed_Size;
            exit;
         end if;

         Next (Iter);
      end loop;

      Queue_Resize (Win);
   end Set_Size;

end Gtkada.Multi_Paned;
