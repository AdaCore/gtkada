------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2026, AdaCore                     --
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

with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Label;  use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;

package body Create_Reparent is

   type My_Button_Record is new Gtk_Button_Record with record
      Label      : Gtk_Label;
      New_Parent : Gtk_Box;
   end record;
   type My_Button is access all My_Button_Record'Class;
   --  A button that remembers the label it reparents and the box into which
   --  the label should be moved when the button is clicked.

   procedure Reparent_Label (Self : access Gtk_Button_Record'Class);
   --  "clicked" handler: move Self.Label out of its current container and into
   --  Self.New_Parent.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return
        "This demo does not demonstrate a widget. Instead, it shows how"
        & " you can dynamically change the parent of a widget. Clicking either"
        & " @bbring the label here@B button moves the label into that button's frame: the"
        & " widget is removed from its old container and appended to the new"
        & " one."
        & ASCII.LF
        & "In Gtk4 the single-call @bReparent@B is gone; the move is expressed"
        & " as @bRemove@B on the old container followed by @bAppend@B on the"
        & " new one."
        & ASCII.LF
        & "This demo also shows how to extend an existing @bGtk_Button@B to"
        & " include specific data to it.";
   end Help;

   --------------------
   -- Reparent_Label --
   --------------------

   procedure Reparent_Label (Self : access Gtk_Button_Record'Class) is
      Button     : constant My_Button := My_Button (Self);
      Old_Parent : constant Gtk_Widget := Button.Label.Get_Parent;
   begin
      --  Ref the label to avoid it being destroyed when removed from its old parent.
      Button.Label.Ref;
      if Old_Parent /= null then
         Gtk_Box (Old_Parent).Remove (Button.Label);
      end if;
      Button.New_Parent.Append (Button.Label);
      --  Unref the label now that it has a new parent.
      Button.Label.Unref;
   end Reparent_Label;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1   : Gtk_Box;
      Box2   : Gtk_Box;
      Box3   : Gtk_Box;
      Label  : Gtk_Label;
      Frame2 : Gtk_Frame;
      Myb    : My_Button;

   begin
      Set_Label (Frame, "Reparent");

      Gtk_New (Box1, Orientation_Vertical, 0);
      Frame.Set_Child (Box1);

      Gtk_New (Box2, Orientation_Horizontal, 5);
      Box2.Set_Margin_Start (10);
      Box2.Set_Margin_End (10);
      Box2.Set_Margin_Top (10);
      Box2.Set_Margin_Bottom (10);
      Box1.Append (Box2);

      Gtk_New (Label, "hello world");

      --  Frame 1: holds a "bring the label here" button and, initially, the label.

      Gtk_New (Frame2, "Frame 1");
      Frame2.Set_Hexpand (True);
      Box2.Append (Frame2);

      Gtk_New (Box3, Orientation_Vertical, 5);
      Box3.Set_Margin_Start (5);
      Box3.Set_Margin_End (5);
      Box3.Set_Margin_Top (5);
      Box3.Set_Margin_Bottom (5);
      Frame2.Set_Child (Box3);

      Myb := new My_Button_Record;
      Initialize (Myb, "bring the label here");
      Myb.Label := Label;
      Myb.New_Parent := Box3;
      Gtk_Button (Myb).On_Clicked (Reparent_Label'Access);
      Box3.Append (Myb);

      Box3.Append (Label);

      --  Frame 2: holds a second "bring the label here" button targeting its own box.

      Gtk_New (Frame2, "Frame 2");
      Frame2.Set_Hexpand (True);
      Box2.Append (Frame2);

      Gtk_New (Box3, Orientation_Vertical, 5);
      Box3.Set_Margin_Start (5);
      Box3.Set_Margin_End (5);
      Box3.Set_Margin_Top (5);
      Box3.Set_Margin_Bottom (5);
      Frame2.Set_Child (Box3);

      Myb := new My_Button_Record;
      Initialize (Myb, "bring the label here");
      Myb.Label := Label;
      Myb.New_Parent := Box3;
      Gtk_Button (Myb).On_Clicked (Reparent_Label'Access);
      Box3.Append (Myb);
   end Run;

end Create_Reparent;
