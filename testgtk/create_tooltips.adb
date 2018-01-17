------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Gdk.RGBA;          use Gdk.RGBA;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Gtk.Box;           use Gtk.Box;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Stock;         use Gtk.Stock;
with Gtk.Tooltip;       use Gtk.Tooltip;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Gtk;               use Gtk;

package body Create_Tooltips is

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "@bGtk_Tooltips@B allow you to provide short help texts to the"
        & " user. " & ASCII.LF
        & " Through the @bwidget_entered@B and @bwidget_selected@B signals,"
        & " you can decide to display some extensive help.";
   end Help;

   ----------------------
   -- Query_Tooltip_Cb --
   ----------------------

   function Query_Tooltip_Cb
      (Check        : access Gtk_Widget_Record'Class;
       X, Y         : Gint;
       Keyboard_Tip : Boolean;
       Tooltip      : not null access GObject_Record'Class)
      return Boolean
   is
      pragma Unreferenced (X, Y, Keyboard_Tip);
   begin
      Gtk_Tooltip (Tooltip).Set_Markup
         ("The text of the widget is <b>"""
          & Gtk_Check_Button (Check).Get_Label & """</b>");
      Gtk_Tooltip (Tooltip).Set_Icon_From_Stock (Stock_Delete, Icon_Size_Menu);
      return True;
   end Query_Tooltip_Cb;

   -----------------------------
   -- Query_Tooltip_Custom_Cb --
   -----------------------------

   function Query_Tooltip_Custom_Cb
      (Check        : access Gtk_Widget_Record'Class;
       X, Y         : Gint;
       Keyboard_Tip : Boolean;
       Tooltip      : not null access GObject_Record'Class)
      return Boolean
   is
      pragma Unreferenced (X, Y, Keyboard_Tip, Tooltip);
      Window : constant Gtk_Window := Gtk_Window (Check.Get_Tooltip_Window);
      Color  : constant Gdk_RGBA := (0.0, 0.0, 1.0, 0.5);
   begin
      Window.Override_Background_Color (Gtk_State_Flag_Normal, Color);
      return True;
   end Query_Tooltip_Custom_Cb;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1       : Gtk_Box;
      Check      : Gtk_Check_Button;
      Vbox       : Gtk_Box;
      Label      : Gtk_Label;
      Tooltip_Window : Gtk_Window;

   begin
      Set_Label (Frame, "Tooltips");
      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Frame, Vbox);

      Gtk_New_Vbox (Box1, False, 0);
      Pack_Start (Vbox, Box1, Expand => False, Fill => False);

      --  A check button using the tooltip-markup property

      Gtk_New (Check, "Using the tooltip-markup property");
      Box1.Pack_Start (Check, False, False, 0);
      Check.Set_Tooltip_Text ("Hello, I am a static tooltip.");

      --  A check button using the query-tooltip signal

      Gtk_New (Check, "Using the query-tooltip signal");
      Box1.Pack_Start (Check, False, False, 0);
      Check.Set_Has_Tooltip (True);
      Check.On_Query_Tooltip (Query_Tooltip_Cb'Access);

      --  A label

      Gtk_New (Label, "A simple label");
      Box1.Pack_Start (Label, False, False, 0);
      Label.Set_Selectable (False);
      Label.Set_Tooltip_Text ("Label and tooltip");

      --  A selectable label

      Gtk_New (Label, "A selectable label");
      Box1.Pack_Start (Label, False, False, 0);
      Label.Set_Selectable (True);
      Label.Set_Tooltip_Text ("<b>Another</b> Label tooltip");

      --  Another one, with a custom tooltip window

      Gtk_New (Tooltip_Window, Window_Popup);
      Gtk_New (Label, "in custom tooltip window");
      Tooltip_Window.Add (Label);
      Label.Show;

      Gtk_New (Check, "Using a custom tooltip window");
      Box1.Pack_Start (Check, False, False, 0);
      Check.Set_Tooltip_Window (Tooltip_Window);
      Check.Set_Has_Tooltip (True);
      Check.On_Query_Tooltip (Query_Tooltip_Custom_Cb'Access);

      --  An insensitive button

      Gtk_New (Check, "Insensitive button");
      Box1.Pack_Start (Check, False, False, 0);
      Check.Set_Sensitive (False);
      Check.Set_Tooltip_Text ("Insensitive!");

      Show_All (Frame);
   end Run;

end Create_Tooltips;
