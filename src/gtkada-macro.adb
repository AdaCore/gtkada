-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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

with Glib.Object;     use Glib.Object;

with Gdk.Window;

with Gdk.Event;       use Gdk.Event;
with Gdk.Types;       use Gdk.Types;
with Gdk.Window;

with Gtk.Container;   use Gtk.Container;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Window;      use Gtk.Window;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.Main;        use Gtk.Main;
with Gtk.Menu_Item;   use Gtk.Menu_Item;
with Gtk.Label;       use Gtk.Label;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Vbutton_Box; use Gtk.Vbutton_Box;

with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Ada.Text_IO;     use Ada.Text_IO;
with System;          use System;
with Unchecked_Deallocation;

package body Gtkada.Macro is

   use Gdk;

   --  ??? Known problem: handling of grabs is still incorrect
   --  when replaying macros, if a BUTTON_PRESS event is generated for a
   --  Gtk_Button, then the latter does a grab on the mouse. We have tried to
   --  counter-balance that in Step_Macro_Cb, by forcing a grab on the
   --  macro interface, but if the user presses and releases the mouse twice
   --  anywhere on the interface, then the second release is sent to the user
   --  application as if a BUTTON_RELEASE had been generated.
   --    * It would be nice if the gtk application could be started with
   --      grabs disabled.
   --    * Or we could try to prevent any event directly on the user's
   --      application.

   --  ??? Implement special events that either print a message to the
   --  user or do an automatic screenshot and comparison

   --  ??? Recorder_Record should be the window itself

   --  ??? Should support identifiers (widget name,...) with ASCII.LF in them.

   --  ??? Have an option in gate to force a name for all widgets.

   --  ??? How to test resizing

   --  ??? Do we need a function to wait for a specific event: for instance,
   --  when pressing a button does some background work, and then displays
   --  a dialog to report, it would be nice if we were able to wait for the
   --  dialog (for instance, wait until a specific widget is available)

   ---------------
   -- Constants --
   ---------------

   Macro_Window_Title : constant String := "@@gtkada_macro@@";

   ---------------------------
   -- Types and subprograms --
   ---------------------------

   function Get_Widget_From_Id
     (Id : Identifier; List : Widget_List.Glist) return Gtk_Widget;
   --  Find the widget whose Id is ID in the application.

   function Find_Widget (Item : Macro_Item'Class) return Gtk_Widget;
   --  Find the widget associated with Item.

   function Child_From_Coordinates
     (Widget    : access Gtk_Widget_Record'Class;
      Max_Depth : Natural;
      X, Y      : Gint) return Gtk_Widget;
   --  Returns the child (or grand-* child) which is at the coordinates (X, Y)
   --  in WIDGET, or WIDGET itself if there is none.
   --  The search is no deeper than MAX_DEPTH

   procedure Find_Named_Parent
     (Widget    : access Gtk_Widget_Record'Class;
      Parent    : out Gtk_Widget;
      Parent_Id : out Identifier;
      X, Y      : in out Gint;
      Depth     : out Natural);
   --  Returns the first widget in WIDGET's hierarchy that has a name. It might
   --  be WIDGET itself.
   --  (X, Y) are updated so that they end up being relative to PARENT instead
   --  of WIDGET.
   --  DEPTH is the number of levels we had to go up to find this PARENT.
   --  PARENT is null if there was no named parent, or if Widget is part of
   --  the macro GUI (and not of the application itself)

   function Get_Id (Widget : access Gtk_Widget_Record'Class) return Identifier;
   --  Return an identifier that can be used for the widget.
   --  return value.Id is left to null if no specific identifier could be
   --  found.

   function Get_Real_Name (Widget : System.Address) return System.Address;
   pragma Import (C, Get_Real_Name, "ada_gtk_get_real_name");
   --  Returns the real widget name (as opposed to gtk_widget_get_name,
   --  this one returns NULL instead of the class name if no name was
   --  set.

   function Load_Line
     (File : access File_Buffer;
      Name : String;
      Optional : Boolean := False) return String;
   --  Reads the next line in the file, check that the item name is NAME,
   --  and return the vlaue (i.e after ":=" ). Raises Invalid_Line if
   --  the item is incorrect.
   --  If Optional is True, then "" is returned if the current line doesn't
   --  match Name.

   Invalid_Line : exception;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Macro_Item_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Macro_Item'Class, Macro_Item_Access);
   begin
      Free (Item.Id.Id);
      Internal (Item);
   end Free;

   -----------------
   -- Find_Widget --
   -----------------

   function Find_Widget (Item : Macro_Item'Class) return Gtk_Widget is
      W    : Gtk_Widget;
      List : Widget_List.Glist;
   begin
      List := List_Toplevels;
      W    := Get_Widget_From_Id (Item.Id, List_Toplevels);
      Widget_List.Free (List);

      --  Once we have the top-level widget, we go down as many levels as
      --  required, using the mouse coordinates to find the appropriate
      --  child.

      if Item.Widget_Depth /= 0 and then W /= null then
         W := Child_From_Coordinates (W, Item.Widget_Depth, Item.X, Item.Y);
      end if;

      if W = null or else Get_Window (W) = Gdk.Window.Null_Window then
         return null;
      end if;

      return W;
   end Find_Widget;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item) is
   begin
      Put_Line (File, "Id:=" & Item.Id.Id_Type'Img);
      Put_Line (File, "Name:=" & Item.Id.Id.all);
      Put_Line (File, "Type:="
                & Integer'Image (Gdk_Event_Type'Pos (Item.Event_Type)));
      Put_Line (File, "Depth:=" & Natural'Image (Item.Widget_Depth));
      Put_Line (File, "Time:=" & Guint32'Image (Item.Time));
   end Save_To_Disk;

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Mouse_Press) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "Button:=" & Guint'Image (Item.Button));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
   end Save_To_Disk;

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Crossing) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "Mode:=" & Gdk_Crossing_Mode'Image (Item.Mode));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Detail:=" & Gdk_Notify_Type'Image (Item.Detail));
   end Save_To_Disk;

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Key) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Keyval:=" & Gdk_Key_Type'Image (Item.Keyval));
      Put_Line (File, "Str:=" & Item.Str.all);
      Put_Line (File, "Hardware:=" & Guint16'Image (Item.Hardware_Keycode));
      Put_Line (File, "Group:=" & Guint8'Image (Item.Group));
   end Save_To_Disk;

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Motion) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
   end Save_To_Disk;

   ---------------
   -- Load_Line --
   ---------------

   function Load_Line
     (File : access File_Buffer;
      Name : String;
      Optional : Boolean := False) return String
   is
      Last  : Natural;
      First : Natural;
   begin
      Last := File.Index;
      while Last <= File.Buffer'Last
        and then File.Buffer (Last) /= ASCII.LF
      loop
         Last := Last + 1;
      end loop;

      if File.Buffer (File.Index .. File.Index + Name'Length - 1) /= Name then
         if Optional then
            return "";
         else
            Put_Line ("Invalid line read: expecting (" & Name
                      & ")   found  ("
                      & File.Buffer (File.Index .. Last - 1) & "), "
                      & " at index " & File.Index'Img);
            raise Invalid_Line;
         end if;
      end if;

      First := File.Index + Name'Length + 2;
      while First <= Last and then File.Buffer (First) = ' ' loop
         First := First + 1;
      end loop;

      File.Index := Last + 1;
      return File.Buffer (First .. Last - 1);
   end Load_Line;

   --------------------
   -- Load_From_Disk --
   --------------------

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item)  is
   begin
      Item.Widget_Depth := Natural'Value (Load_Line (File, "Depth"));
      Item.Time := Guint32'Value (Load_Line (File, "Time"));
   end Load_From_Disk;

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item_Mouse_Press) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.Button := Guint'Value (Load_Line (File, "Button"));
      Item.State  := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
   end Load_From_Disk;

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item_Key) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := 0;
      Item.Y := 0;
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Keyval := Gdk_Key_Type'Value (Load_Line (File, "Keyval"));
      Item.Str := new String'(Load_Line (File, "Str"));
      Item.Hardware_Keycode := Guint16'Value (Load_Line (File, "Hardware"));
      Item.Group := Guint8'Value (Load_Line (File, "Group"));
   end Load_From_Disk;

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item_Crossing) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.Mode := Gdk_Crossing_Mode'Value (Load_Line (File, "Mode"));
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Detail := Gdk_Notify_Type'Value (Load_Line (File, "Detail"));
   end Load_From_Disk;

   procedure Load_From_Disk
     (File : access File_Buffer;
      Item : out Macro_Item_Motion) is
   begin
      Load_From_Disk (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
   end Load_From_Disk;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Widget : access Gtk_Widget_Record'Class) return Identifier is
   begin
      if Get_Real_Name (Get_Object (Widget)) /= System.Null_Address then
         return (Name, new String'(Get_Name (Widget)));

      elsif Widget.all in Gtk_Window_Record'Class then
         if Get_Title (Gtk_Window (Widget)) /= "" then
            return (Title, new String'(Get_Title (Gtk_Window (Widget))));

         elsif Get_Transient_For (Gtk_Window (Widget)) /= null then
            declare
               T : constant String :=
                 Get_Title (Get_Transient_For (Gtk_Window (Widget)));
            begin
               if T /= "" then
                  return (Transient, new String'(T));
               end if;
            end;
         end if;

      elsif Widget.all in Gtk_Menu_Item_Record'Class
        or else Widget.all in Gtk_Button_Record'Class
      then
         declare
            C : constant Gtk_Widget := Get_Child (Gtk_Bin (Widget));
         begin
            if C /= null and then C.all in Gtk_Label_Record'Class then
               return (Label, new String'(Get (Gtk_Label (C))));
            end if;
         end;

      end if;

      return (None, null);
   end Get_Id;

   ------------------------
   -- Get_Widget_From_Id --
   ------------------------

   function Get_Widget_From_Id
     (Id : Identifier; List : Widget_List.Glist)
      return Gtk_Widget
   is
      Current : Gtk.Widget.Widget_List.Glist := Widget_List.First (List);
      W       : Gtk_Widget;
      L       : Widget_List.Glist;
      W_Id    : Identifier;
      use type Gtk.Widget.Widget_List.Glist;

   begin
      while Current /= Widget_List.Null_List loop
         W := Widget_List.Get_Data (Current);
         W_Id := Get_Id (W);

         if W_Id.Id_Type = Id.Id_Type
           and then W_Id.Id.all = Id.Id.all
         then
            return W;

         --  Else we examine the children of W (except when we know the title
         --  of a top-level window, in which case there is no need to go down)

         elsif W.all in Gtk_Container_Record'Class
           and then Id.Id_Type /= Title
         then
            L := Children (Gtk_Container (W));

            if L /= Widget_List.Null_List then
               W := Get_Widget_From_Id (Id, L);

               if W /= null then
                  return W;
               end if;
            end if;
         end if;

         Current := Widget_List.Next (Current);
      end loop;

      --  Revert to the focus widget by default
      --  Should this be an option ???

      Current := Widget_List.First (List);

      while Current /= Widget_List.Null_List loop
         W := Widget_List.Get_Data (Current);

         if Has_Focus_Is_Set (W) then
            return Get_Focus (Gtk_Window (W));
         end if;

         Current := Widget_List.Next (Current);
      end loop;

      return null;
   end Get_Widget_From_Id;

   ----------------------------
   -- Child_From_Coordinates --
   ----------------------------

   function Child_From_Coordinates (Widget    : access Gtk_Widget_Record'Class;
                                    Max_Depth : Natural;
                                    X, Y      : Gint)
                                   return Gtk_Widget
   is
      use type Widget_List.Glist;
      Current : Widget_List.Glist := Widget_List.Null_List;
      New_X, New_Y : Gint;
   begin
      if Max_Depth = 0 then
         return Gtk_Widget (Widget);
      end if;

      if Widget.all in Gtk_Container_Record'Class then
         Current := Widget_List.First (Children (Gtk_Container (Widget)));
      end if;

      while Current /= Widget_List.Null_List loop
         declare
            W     : constant Gtk_Widget := Widget_List.Get_Data (Current);
            Rec_X : constant Gint := Get_Allocation_X (W);
            Rec_Y : constant Gint := Get_Allocation_Y (W);
            Rec_W : constant Gint := Gint (Get_Allocation_Width (W));
            Rec_H : constant Gint := Gint (Get_Allocation_Height (W));
         begin
            pragma Debug
              (Put_Line ("  "
                         & (1 .. Max_Depth * 3 => ' ')
                         & "Child_From_Coordinates, Depth=" & Max_Depth'Img
                         & " X=" & X'Img & " Y=" & Y'Img
                         & " Rec_X=" & Rec_X'Img & " Rec_Y=" & Rec_Y'Img
                         & " Rec_W=" & Rec_W'Img & " Rec_H=" & Rec_H'Img));

            if X >= Rec_X
              and then X <= Rec_X + Rec_W
              and then Y >= Rec_Y
              and then Y <= Rec_Y + Rec_H
            then
               --  Note that in Gtk+ the button boxes and boxes set the
               --  coordinates of their children to the parent's, so we need a
               --  special handling here

               if W.all in Gtk_Vbutton_Box_Record'Class
                 or else Get_Type (W) = Get_Vbox_Type
               then
                  New_X := X;
               else
                  New_X := X - Rec_X;
               end if;

               if W.all in Gtk_Hbutton_Box_Record'Class
                 or else Get_Type (W) = Get_Hbox_Type
               then
                  New_Y := Y;
               else
                  New_Y := Y - Rec_Y;
               end if;

               return Child_From_Coordinates (W, Max_Depth - 1, New_X, New_Y);
            end if;
         end;
         Current := Widget_List.Next (Current);
      end loop;
      return Gtk_Widget (Widget);
   end Child_From_Coordinates;

   -----------------------
   -- Find_Named_Parent --
   -----------------------

   procedure Find_Named_Parent
     (Widget    : access Gtk_Widget_Record'Class;
      Parent    : out Gtk_Widget;
      Parent_Id : out Identifier;
      X, Y      : in out Gint;
      Depth     : out Natural)
   is
      Tmp : Gtk_Widget;
   begin

      Parent := Gtk_Widget (Widget);
      Depth  := 0;

      --  Check that the event didn't take place in fact in the macro GUI

      Tmp := Gtk_Widget (Widget);

      while Get_Parent (Tmp) /= null loop
         Tmp := Get_Parent (Tmp);
      end loop;

      if Get_Name (Tmp) = Macro_Window_Title then
         return;
      end if;

      --  Stop either at the top-level widget, or at the first widget that is
      --  associated with an id.

      while Parent /= null loop

         if Get_Window (Parent) /= Gdk.Window.Null_Window then
            Parent_Id := Get_Id (Parent);
            exit when Parent_Id.Id /= null;
         end if;

         --  Change the coordinates of the child to include that of the
         --  parent's, except in the special case of boxes and button boxes
         --  where this is already done by gtk+ itself.

         if not (Parent.all in  Gtk_Vbutton_Box_Record'Class)
           and then Get_Type (Parent) /= Get_Vbox_Type
         then
            X := X + Get_Allocation_X (Parent);
         end if;

         if not (Parent.all in  Gtk_Hbutton_Box_Record'Class)
           and then Get_Type (Parent) /= Get_Hbox_Type
         then
            Y := Y + Get_Allocation_Y (Parent);
         end if;

         Parent := Get_Parent (Parent);
         Depth  := Depth + 1;
      end loop;
   end Find_Named_Parent;

   ------------------
   -- Create_Event --
   ------------------

   function Create_Event
     (Item : Macro_Item_Mouse_Press) return Gdk_Event
   is
      E      : Gdk_Event;
      Widget : Gtk_Widget := Find_Widget (Item);
   begin
      if Widget = null then
         return null;
      end if;

      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
      Set_Button (E, Item.Button);
      Set_State (E, Item.State);
      Set_Time (E, Item.Time);
      return E;
   end Create_Event;

   function Create_Event
     (Item : Macro_Item_Crossing) return Gdk_Event
   is
      E      : Gdk_Event;
      Widget : Gtk_Widget := Find_Widget (Item);
   begin
      if Widget = null then
         return null;
      end if;

      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_Mode (E, Item.Mode);
      Set_Subwindow (E, Get_Window (Widget));
      Gdk.Window.Ref (Get_Window (Widget));
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
      Set_State (E, Item.State);
      Set_Focus (E, Item.Focus);
      Set_Detail (E, Item.Detail);
      Set_Time (E, Item.Time);
      return E;
   end Create_Event;

   function Create_Event
     (Item : Macro_Item_Key) return Gdk_Event
   is
      E      : Gdk_Event;
      Widget : constant Gtk_Widget := Find_Widget (Item);
   begin

      if Widget = null then
         return null;
      end if;

      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_State (E, Item.State);
      Set_Time (E, Item.Time);
      Set_Key_Val (E, Item.Keyval);
      Set_String (E, Item.Str.all);
      Set_Group (E, Item.Group);
      Set_Hardware_Keycode (E, Item.Hardware_Keycode);
      return E;
   end Create_Event;

   function Create_Event
     (Item : Macro_Item_Motion) return Gdk.Event.Gdk_Event
   is
      E      : Gdk_Event;
      Widget : Gtk_Widget := Find_Widget (Item);
   begin
      if Widget = null then
         return null;
      end if;

      Allocate (Event      => E,
                Event_Type => Item.Event_Type,
                Window     => Get_Window (Widget));
      Set_State (E, Item.State);
      Set_X (E, Gdouble (Item.X));
      Set_Y (E, Gdouble (Item.Y));
--        Set_X_Root (E, Gdouble (Item.X));
--        Set_Y_Root (E, Gdouble (Item.Y));
      Set_Is_Hint (E, False);
--        Set_Source (E, Source_Mouse);
--        Set_Device_Id (E, Core_Pointer);
      Set_Time (E, Item.Time);
      return E;
   end Create_Event;

   -----------------
   -- Create_Item --
   -----------------

   function Create_Item
     (Event : Gdk_Event_Button) return Macro_Item_Mouse_Press_Access
   is
      W    : Gtk_Widget;
      X    : Gint := Gint (Get_X (Event));
      Y    : Gint := Gint (Get_Y (Event));
      In_W : Natural;
      Id   : Identifier;
      Item : Macro_Item_Mouse_Press_Access;

   begin
      Find_Named_Parent (Gtk.Main.Get_Event_Widget (Event), W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Item              := new Macro_Item_Mouse_Press;
         Item.Id           := Id;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Widget_Depth := In_W;
         Item.X            := X;
         Item.Y            := Y;
         Item.Button       := Get_Button (Event);
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
      end if;

      return Item;
   end Create_Item;

   function Create_Item
     (Event : Gdk_Event_Key) return Macro_Item_Key_Access
   is
      W    : Gtk_Widget;
      X    : Gint := 0;
      Y    : Gint := 0;
      In_W : Natural;
      Id   : Identifier;
      Item : Macro_Item_Key_Access;

   begin
      Find_Named_Parent (Gtk.Main.Get_Event_Widget (Event), W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Item                  := new Macro_Item_Key;
         Item.Id               := Id;
         Item.Event_Type       := Get_Event_Type (Event);
         Item.Widget_Depth     := In_W;
         Item.State            := Get_State (Event);
         Item.Time             := Get_Time (Event);
         Item.Keyval           := Get_Key_Val (Event);
         Item.Str              := new String'(Get_String (Event));
         Item.Group            := Get_Group (Event);
         Item.Hardware_Keycode := Get_Hardware_Keycode (Event);
      end if;

      return Item;
   end Create_Item;

   function Create_Item
     (Event : Gdk_Event_Motion) return Macro_Item_Motion_Access
   is
      W    : Gtk_Widget;
      X    : Gint := Gint (Get_X (Event));
      Y    : Gint := Gint (Get_Y (Event));
      In_W : Natural;
      Id   : Identifier;
      Item : Macro_Item_Motion_Access;

   begin
      Find_Named_Parent (Gtk.Main.Get_Event_Widget (Event), W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Item              := new Macro_Item_Motion;
         Item.Id           := Id;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Widget_Depth := In_W;
         Item.X            := X;
         Item.Y            := Y;
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
      end if;

      return Item;
   end Create_Item;

   function Create_Item
     (Event : Gdk_Event_Crossing) return Macro_Item_Crossing_Access
   is
      W      : Gtk_Widget;
      X      : Gint := Gint (Get_X (Event));
      Y      : Gint := Gint (Get_Y (Event));
      In_W   : Natural;
      Id     : Identifier;
      Item   : Macro_Item_Crossing_Access;

   begin
      Find_Named_Parent (Gtk.Main.Get_Event_Widget (Event), W, Id, X, Y, In_W);

      if W /= null and then Id.Id /= null then
         Item              := new Macro_Item_Crossing;
         Item.Id           := Id;
         Item.Widget_Depth := In_W;
         Item.Event_Type   := Get_Event_Type (Event);
         Item.Mode         := Get_Mode (Event);
         Item.X            := X;
         Item.Y            := Y;
         Item.Focus        := Get_Focus (Event);
         Item.Detail       := Get_Detail (Event);
         Item.State        := Get_State (Event);
         Item.Time         := Get_Time (Event);
      end if;

      return Item;
   end Create_Item;

end Gtkada.Macro;
