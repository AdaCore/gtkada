------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
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

with Gdk.Rectangle;    use Gdk.Rectangle;
with Glib;             use Glib;
with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Frame;        use Gtk.Frame;
with Gtk.Label;        use Gtk.Label;
with Gtk.Overlay;      use Gtk.Overlay;
with Gtk.Widget;       use Gtk.Widget;

package body Create_Overlay is

   Placed : Gtk_Button;
   --  The overlay that On_Child_Position places by hand

   Status : Gtk_Label;

   Clicks : Natural := 0;

   function On_Child_Position
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean;
   --  Put Placed at a fixed rectangle. For every other overlay, return
   --  False to let GTK use the child's halign/valign.

   procedure On_Click (Self : access Gtk_Button_Record'Class);

   procedure On_Clip_Toggled (Self : access Gtk_Check_Button_Record'Class);

   -----------------------
   -- On_Child_Position --
   -----------------------

   function On_Child_Position
     (Self       : access Gtk_Overlay_Record'Class;
      Widget     : not null access Gtk_Widget_Record'Class;
      Allocation : out Gdk_Rectangle) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      Allocation := (0, 0, 0, 0);
      if Gtk_Widget (Widget) /= Gtk_Widget (Placed) then
         return False;
      end if;

      Allocation := (X => 40, Y => 60, Width => 160, Height => 36);
      return True;
   end On_Child_Position;

   --------------
   -- On_Click --
   --------------

   procedure On_Click (Self : access Gtk_Button_Record'Class) is
   begin
      Clicks := Clicks + 1;
      Status.Set_Text
        ("Clicked """ & Self.Get_Label & """; total clicks:"
         & Natural'Image (Clicks));
   end On_Click;

   ---------------------
   -- On_Clip_Toggled --
   ---------------------

   procedure On_Clip_Toggled (Self : access Gtk_Check_Button_Record'Class) is
      O : constant Gtk_Overlay := Gtk_Overlay (Placed.Get_Parent);
   begin
      O.Set_Clip_Overlay (Placed, Self.Get_Active);
   end On_Clip_Toggled;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Overlay@B stacks widgets on top of a main child."
        & ASCII.LF
        & "The label is the main child (@bSet_Child@B); the buttons are"
        & " overlays added with @bAdd_Overlay@B. Three of them are placed"
        & " by their @bhalign@B and @bvalign@B, as GTK does by default."
        & ASCII.LF
        & "The fourth button is placed by hand: the overlay's"
        & " @bget-child-position@B signal is handled to return an exact"
        & " rectangle for it, and False for the others."
        & ASCII.LF
        & "The check button switches @bSet_Clip_Overlay@B for the"
        & " hand-placed button, which is then clipped to the overlay's"
        & " bounds.";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box     : constant Gtk_Box := Gtk_Box_New (Orientation_Vertical, 6);
      O       : constant Gtk_Overlay := Gtk_Overlay_New;
      Back    : constant Gtk_Label :=
        Gtk_Label_New ("The main child of the overlay");
      Clip    : constant Gtk_Check_Button :=
        Gtk_Check_Button_New_With_Label ("Clip the hand-placed overlay");

      function Add
        (Text : String; H, V : Gtk_Align) return Gtk_Button;

      function Add
        (Text : String; H, V : Gtk_Align) return Gtk_Button
      is
         B : constant Gtk_Button := Gtk_Button_New_With_Label (Text);
      begin
         B.Set_Halign (H);
         B.Set_Valign (V);
         B.On_Clicked (On_Click'Access);
         O.Add_Overlay (B);
         return B;
      end Add;

      Ignore : Gtk_Button;
   begin
      Frame.Set_Label ("Overlay");
      Frame.Set_Child (Box);

      Box.Set_Margin_Top (6);
      Box.Set_Margin_Bottom (6);
      Box.Set_Margin_Start (6);
      Box.Set_Margin_End (6);

      Back.Set_Size_Request (360, 220);
      O.Set_Child (Back);
      Box.Append (O);

      Ignore := Add ("Top left", Align_Start, Align_Start);
      Ignore := Add ("Center", Align_Center, Align_Center);
      Ignore := Add ("Bottom right", Align_End, Align_End);
      Placed := Add ("Placed by hand", Align_Start, Align_Start);
      O.On_Get_Child_Position (On_Child_Position'Access);

      Clip.On_Toggled (On_Clip_Toggled'Access);
      Box.Append (Clip);

      Status := Gtk_Label_New ("Click an overlay");
      Box.Append (Status);
   end Run;

end Create_Overlay;
