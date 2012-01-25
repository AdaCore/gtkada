------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with Glib.Error;   use Glib.Error;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gtk.Enums;    use Gtk.Enums;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Image;    use Gtk.Image;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button;         use Gtk.Tool_Button;
with Gtk.Tool_Item; use Gtk.Tool_Item;
with Gtk.Widget;   use Gtk.Widget;
with Gtk;          use Gtk;

package body Create_Toolbar is

   package Toolbar_Cb is new Handlers.Callback (Gtk_Toolbar_Record);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Toolbar@B is a set of buttons. Each button is can be"
        & " represented both as a text or an icon, or even both. A tooltip"
        & " is automatically created for each button." & ASCII.LF
        & "The following two signals are defined:" & ASCII.LF
        & "   - ""orientation_changed"": the widget was re-oriented."
        & ASCII.LF
        & "   - ""style_changed"": a new style was selected." & ASCII.LF
        & ASCII.LF
        & "It is worth noting that the toolbar can basically contain any type"
        & " of widget, not only buttons. In this demo, we have added a "
        & " @bGtk_Entry@B widget in the middle.";
   end Help;

   ----------------
   -- New_Pixmap --
   ----------------

   function New_Pixmap (Filename   : String) return Gtk_Widget is
      Image     : Gtk_Image;
      Data      : Gdk_Pixbuf;
      Error     : GError;
   begin
      Gdk_New_From_File (Data, Filename, Error => Error);
      Gtk_New (Image, Data);
      return Gtk_Widget (Image);
   end New_Pixmap;

   --------------------
   -- Set_Horizontal --
   --------------------

   procedure Set_Horizontal
      (Toolbar : not null access Gtk_Toolbar_Record'Class) is
   begin
      Set_Orientation (Toolbar, Orientation_Horizontal);
   end Set_Horizontal;

   ------------------
   -- Set_Vertical --
   ------------------

   procedure Set_Vertical
      (Toolbar : not null access Gtk_Toolbar_Record'Class) is
   begin
      Set_Orientation (Toolbar, Orientation_Vertical);
   end Set_Vertical;

   ---------------
   -- Set_Icons --
   ---------------

   procedure Set_Icons (Toolbar : not null access Gtk_Toolbar_Record'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Icons);
   end Set_Icons;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Toolbar : not null access Gtk_Toolbar_Record'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Text);
   end Set_Text;

   --------------
   -- Set_Both --
   --------------

   procedure Set_Both (Toolbar : not null access Gtk_Toolbar_Record'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Both);
   end Set_Both;

   ------------------
   -- Make_Toolbar --
   ------------------

   procedure Make_Toolbar (Toolbar    : out Gtk_Toolbar;
                           With_Entry : Boolean := False)
   is
      The_Entry : Gtk_Entry;
      Button    : Gtk_Tool_Button;
      Separator : Gtk_Separator_Tool_Item;
      Item      : Gtk_Tool_Item;

   begin
      Gtk_New (Toolbar);
      Set_Orientation (Toolbar, Orientation_Horizontal);
      Set_Style (Toolbar, Toolbar_Both);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Horizontal");
      Button.Set_Tooltip_Text ("Horizontal toolbar layout");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked", Set_Horizontal'Access, Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Vertical");
      Button.Set_Tooltip_Text ("Vertical toolbar layout");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked", Set_Vertical'Access, Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Icons");
      Button.Set_Tooltip_Text ("Only show toolbar icons");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked", Set_Icons'Access, Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Text");
      Button.Set_Tooltip_Text ("Only show toolbar text");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked", Set_Text'Access, Slot_Object => Toolbar);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Both");
      Button.Set_Tooltip_Text ("Show toolbar icons and text");
      Insert (Toolbar, Button);
      Toolbar_Cb.Object_Connect
        (Button, "clicked", Set_Both'Access, Slot_Object => Toolbar);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      if With_Entry then
         Gtk_New (Item);
         Insert (Toolbar, Item);

         Gtk_New (The_Entry);
         Show (The_Entry);
         Add (Item, The_Entry);

         Gtk_New (Separator);
         Insert (Toolbar, Separator);
      end if;

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Dummy");
      Insert (Toolbar, Button);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Dummy");
      Insert (Toolbar, Button);

      Gtk_New (Separator);
      Insert (Toolbar, Separator);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Dummy");
      Insert (Toolbar, Button);

      Gtk_New (Button, New_Pixmap ("test.xpm"), "Dummy");
      Insert (Toolbar, Button);

   end Make_Toolbar;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Toolbar : Gtk_Toolbar;
   begin
      Set_Label (Frame, "Toolbar");
      Make_Toolbar (Toolbar, True);
      Set_Orientation (Toolbar, Orientation_Vertical);
      Add (Frame, Toolbar);

      Show_All (Frame);
   end Run;

end Create_Toolbar;

