------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
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

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Values;          use Glib.Values;

with Gdk.Content_Provider; use Gdk.Content_Provider;
with Gdk.Drag;             use Gdk.Drag;
with Gtk.Box;              use Gtk.Box;
with Gtk.Drag_Source;      use Gtk.Drag_Source;
with Gtk.Drop_Target;      use Gtk.Drop_Target;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Frame;            use Gtk.Frame;
with Gtk.Label;            use Gtk.Label;
with Gtk.Widget;           use Gtk.Widget;

package body Create_Drag_And_Drop is

   --  The demo is a singleton, like the other ones: the widgets that the
   --  handlers report to live at package level.

   Status : Gtk_Label;
   Copied : Gtk_Label;
   Moved  : Gtk_Label;

   Moving : Gtk_Label;
   --  The source that can be moved out of; its text goes away after a move

   Fruit, Vegetable : Gdk_Content_Provider;
   --  What the two-faced source hands out, depending on where it is grabbed.
   --  Kept for the life of the demo: the "prepare" handler must return a
   --  provider, and GTK takes over one reference to it every time.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "Drag-and-drop is made of two controllers. A @bGtk_Drag_Source@B on"
        & " the widget where the drag starts says what can be dragged: it"
        & " owns a @bGdk_Content_Provider@B, and the @bGdk.Drag.Drag_Action@B"
        & " it allows. A @bGtk_Drop_Target@B on the receiving widget says"
        & " which @bGType@B it accepts, and its @bdrop@B signal delivers the"
        & " data as a @bGValue@B."
        & ASCII.LF
        & "Drag the labels of the first row onto the frames of the second."
        & " ""Copy me"" has fixed content, given to the source with"
        & " @bSet_Content@B. ""Fruit / Vegetable"" builds its content on"
        & " demand in @bprepare@B: it depends on which half of the label"
        & " you grab. ""Move me"" allows only @bGdk_Action_Move@B, and when"
        & " a target accepts the drop, @bdrag-end@B says to delete the data:"
        & " the label empties.";
   end Help;

   function Make_Provider (Text : String) return Gdk_Content_Provider;
   function Prepare
     (Source : access Gtk_Drag_Source_Record'Class;
      X, Y   : Gdouble) return Gdk_Content_Provider;
   procedure Drag_Begin
     (Source : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk_Drag_Record'Class);
   procedure Drag_End
     (Source      : access Gtk_Drag_Source_Record'Class;
      Drag        : not null access Gdk_Drag_Record'Class;
      Delete_Data : Boolean);
   function Drag_Cancel
     (Source : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk_Drag_Record'Class;
      Reason : Drag_Cancel_Reason) return Boolean;
   function Drop_On_Copy
     (Target : access Gtk_Drop_Target_Record'Class;
      Value  : GValue;
      X, Y   : Gdouble) return Boolean;
   function Drop_On_Move
     (Target : access Gtk_Drop_Target_Record'Class;
      Value  : GValue;
      X, Y   : Gdouble) return Boolean;
   function Make_Label (Text : String) return Gtk_Label;
   procedure Make_Target
     (Frame : access Gtk_Frame_Record'Class;
      Label : Gtk_Label;
      Call  : Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean);

   -------------------
   -- Make_Provider --
   -------------------

   function Make_Provider (Text : String) return Gdk_Content_Provider is
      Value : GValue;
   begin
      Init_Set_String (Value, Text);
      return P : constant Gdk_Content_Provider :=
        Gdk_Content_Provider_New_For_Value (Value)
      do
         --  The provider holds its own copy.
         Unset (Value);
      end return;
   end Make_Provider;

   -------------
   -- Prepare --
   -------------

   function Prepare
     (Source : access Gtk_Drag_Source_Record'Class;
      X, Y   : Gdouble) return Gdk_Content_Provider
   is
      pragma Unreferenced (Source, Y);
   begin
      --  X is where in the widget the press was: left half, fruit.
      --  The return value is transfer full, so hand GTK its own reference
      --  and keep the package-level one.
      if X < 60.0 then
         Fruit.Ref;
         return Fruit;
      else
         Vegetable.Ref;
         return Vegetable;
      end if;
   end Prepare;

   ----------------
   -- Drag_Begin --
   ----------------

   procedure Drag_Begin
     (Source : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk_Drag_Record'Class)
   is
      pragma Unreferenced (Source, Drag);
   begin
      Status.Set_Text ("Dragging...");
   end Drag_Begin;

   --------------
   -- Drag_End --
   --------------

   procedure Drag_End
     (Source      : access Gtk_Drag_Source_Record'Class;
      Drag        : not null access Gdk_Drag_Record'Class;
      Delete_Data : Boolean)
   is
      pragma Unreferenced (Drag);
   begin
      --  Delete_Data is set when the target accepted a move: the data now
      --  lives at the destination and the source is to get rid of it.
      if Delete_Data then
         Status.Set_Text ("Drag finished, source data to delete");
         if Source.Get_Actions = Gdk_Action_Move then
            Moving.Set_Text ("(moved away)");
         end if;
      else
         Status.Set_Text ("Drag finished");
      end if;
   end Drag_End;

   -----------------
   -- Drag_Cancel --
   -----------------

   function Drag_Cancel
     (Source : access Gtk_Drag_Source_Record'Class;
      Drag   : not null access Gdk_Drag_Record'Class;
      Reason : Drag_Cancel_Reason) return Boolean
   is
      pragma Unreferenced (Source, Drag);
   begin
      Status.Set_Text ("Drag cancelled: " & Drag_Cancel_Reason'Image (Reason));

      --  False leaves GTK to show its cancel animation.
      return False;
   end Drag_Cancel;

   ------------------
   -- Drop_On_Copy --
   ------------------

   function Drop_On_Copy
     (Target : access Gtk_Drop_Target_Record'Class;
      Value  : GValue;
      X, Y   : Gdouble) return Boolean
   is
      pragma Unreferenced (Target, X, Y);
   begin
      --  The target was made for GType_String, so Value holds a string.
      Copied.Set_Text ("Received: " & Get_String (Value));
      return True;
   end Drop_On_Copy;

   ------------------
   -- Drop_On_Move --
   ------------------

   function Drop_On_Move
     (Target : access Gtk_Drop_Target_Record'Class;
      Value  : GValue;
      X, Y   : Gdouble) return Boolean
   is
      pragma Unreferenced (Target, X, Y);
   begin
      Moved.Set_Text ("Received: " & Get_String (Value));
      return True;
   end Drop_On_Move;

   ----------------
   -- Make_Label --
   ----------------

   function Make_Label (Text : String) return Gtk_Label is
      Label : Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, Text);
      Label.Set_Size_Request (120, 60);
      return Label;
   end Make_Label;

   -----------------
   -- Make_Target --
   -----------------

   procedure Make_Target
     (Frame : access Gtk_Frame_Record'Class;
      Label : Gtk_Label;
      Call  : Cb_Gtk_Drop_Target_GValue_Gdouble_Gdouble_Boolean)
   is
      Target : Gtk_Drop_Target;
   begin
      Label.Set_Size_Request (200, 80);
      Frame.Set_Child (Label);

      Gtk_New (Target, GType_String, Gdk_Action_Copy or Gdk_Action_Move);
      Target.On_Drop (Call);
      Frame.Add_Controller (Target);
   end Make_Target;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk_Frame_Record'Class) is
      VBox    : Gtk_Box;
      Sources : Gtk_Box;
      Targets : Gtk_Box;
      Copy_F  : Gtk_Frame;
      Move_F  : Gtk_Frame;
      Source  : Gtk_Drag_Source;
      Fixed   : Gtk_Label;
      Both    : Gtk_Label;
   begin
      Frame.Set_Label ("Drag and Drop");
      Frame.Set_Label_Align (0.5);

      Fruit     := Make_Provider ("Apple");
      Vegetable := Make_Provider ("Carrot");

      --  Fixed content
      Fixed := Make_Label ("Copy me");
      Gtk_New (Source);
      Source.Set_Actions (Gdk_Action_Copy);
      Source.Set_Content (Make_Provider ("Hello, drop target"));
      Source.On_Drag_Begin (Drag_Begin'Access);
      Source.On_Drag_End (Drag_End'Access);
      Source.On_Drag_Cancel (Drag_Cancel'Access);
      Fixed.Add_Controller (Source);

      --  Content built by "prepare"
      Both := Make_Label ("Fruit / Vegetable");
      Gtk_New (Source);
      Source.Set_Actions (Gdk_Action_Copy);
      Source.On_Prepare (Prepare'Access);
      Source.On_Drag_Begin (Drag_Begin'Access);
      Source.On_Drag_End (Drag_End'Access);
      Source.On_Drag_Cancel (Drag_Cancel'Access);
      Both.Add_Controller (Source);

      --  A source to move out of
      Moving := Make_Label ("Move me");
      Gtk_New (Source);
      Source.Set_Actions (Gdk_Action_Move);
      Source.Set_Content (Make_Provider ("Moved text"));
      Source.On_Drag_Begin (Drag_Begin'Access);
      Source.On_Drag_End (Drag_End'Access);
      Source.On_Drag_Cancel (Drag_Cancel'Access);
      Moving.Add_Controller (Source);

      Gtk_New (Sources, Orientation_Horizontal, 20);
      Sources.Set_Homogeneous (True);
      Sources.Append (Fixed);
      Sources.Append (Both);
      Sources.Append (Moving);

      Gtk.Label.Gtk_New (Copied, "Drop here (copy)");
      Gtk.Label.Gtk_New (Moved, "Drop here (move)");
      Gtk_New (Copy_F, "Copy target");
      Gtk_New (Move_F, "Move target");
      Make_Target (Copy_F, Copied, Drop_On_Copy'Access);
      Make_Target (Move_F, Moved, Drop_On_Move'Access);

      Gtk_New (Targets, Orientation_Horizontal, 20);
      Targets.Set_Homogeneous (True);
      Targets.Append (Copy_F);
      Targets.Append (Move_F);

      Gtk.Label.Gtk_New (Status, "Drag a label onto a frame");

      Gtk_New (VBox, Orientation_Vertical, 20);
      VBox.Set_Margin_Top (10);
      VBox.Set_Margin_Bottom (10);
      VBox.Set_Margin_Start (10);
      VBox.Set_Margin_End (10);
      VBox.Append (Sources);
      VBox.Append (Targets);
      VBox.Append (Status);
      Frame.Set_Child (VBox);
   end Run;

end Create_Drag_And_Drop;
