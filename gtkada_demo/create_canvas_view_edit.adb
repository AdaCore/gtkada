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
------------------------------------------------------------------------------

with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Types;                use Gdk.Types;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.Style;             use Gtkada.Style;
with Pango.Font;               use Pango.Font;

package body Create_Canvas_View_Edit is

   function On_Item_Event_Zoom is new On_Item_Event_Zoom_Generic
      (Modifier => Mod1_Mask);
   function On_Item_Event_Key_Navigate is new
      On_Item_Event_Key_Navigate_Generic (Modifier => 0);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how to implement in-place editing of items"
         & ASCII.LF
         & "Double-clicking on any text will display a popup window with a"
         & " text editor. Pressing ctrl-enter in that editor will change the"
         & " text of the item. Pressing escape (or clicking anywhere) will"
         & " simply close the popup";
   end Help;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Scrolled    : Gtk_Scrolled_Window;
      Canvas      : Canvas_View;
      Model       : List_Canvas_Model;
      Text        : Editable_Text_Item;
      NText       : Text_Item;
      Rect        : Rect_Item;
      White, Font : Drawing_Style;
   begin
      Font := Gtk_New
        (Stroke => Null_RGBA,
         Font   => (Name   => From_String ("sans 10"),
                    Color  => Black_RGBA,
                    others => <>));

      White := Gtk_New
        (Fill  => Create_Rgba_Pattern ((1.0, 1.0, 1.0, 1.0)));

      Gtk_New (Model);

      Rect := Gtk_New_Rect (White);
      Rect.Set_Position ((50.0, 50.0));
      Model.Add (Rect);

      Text := Gtk_New_Editable_Text (Font, "Line 1");
      Rect.Add_Child (Text);

      Text := Gtk_New_Editable_Text (Font, "Line 2");
      Rect.Add_Child (Text);

      Text := Gtk_New_Editable_Text (Font, "Line 3");
      Rect.Add_Child (Text);

      NText := Gtk_New_Text (Font, "Non editable");
      Rect.Add_Child (NText);

      Rect := Gtk_New_Rect (White, 50.0, 50.0);
      Rect.Set_Position ((550.0, 550.0));
      Model.Add (Rect);

      Text := Gtk_New_Editable_Text (Font, "Legend");
      Text.Set_Position ((200.0, 50.0));
      Model.Add (Text);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk_New (Canvas, Model);
      Unref (Model);
      Scrolled.Add (Canvas);

      Canvas.On_Item_Event (On_Item_Event_Select'Access);
      Canvas.On_Item_Event (On_Item_Event_Move_Item'Access);
      Canvas.On_Item_Event (On_Item_Event_Scroll_Background'Access);
      Canvas.On_Item_Event (On_Item_Event_Edit'Access);
      Canvas.On_Item_Event (On_Item_Event_Zoom'Access);
      Canvas.On_Item_Event (On_Item_Event_Key_Navigate'Access);

      Frame.Show_All;
   end Run;

end Create_Canvas_View_Edit;
