-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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


with Gdk.Bitmap; use Gdk.Bitmap;
with Gdk.Color; use Gdk.Color;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Window; use Gdk.Window;
with Glib; use Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Gtk; use Gtk;
with Common; use Common;

package body Create_Toolbar is

   package Toolbar_Cb is new Signal.Object_Callback (Gtk_Toolbar_Record);

   function New_Pixmap (Filename   : in String;
                        Window     : in Gdk_Window'Class;
                        Background : in Gdk_Color)
                        return Gtk_Widget
   is
      Pixmap    : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      GtkPixmap : Gtk_Pixmap;
   begin
      Create_From_Xpm (Pixmap, Window, Mask, Background, Filename);
      Gtk_New (GtkPixmap, Pixmap, Mask);
      return Gtk_Widget (GtkPixmap);
   end New_Pixmap;

   procedure Set_Horizontal (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Orientation (Toolbar, Orientation_Horizontal);
   end Set_Horizontal;

   procedure Set_Vertical (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Orientation (Toolbar, Orientation_Vertical);
   end Set_Vertical;

   procedure Set_Icons (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Style (Toolbar, Toolbar_Icons);
   end Set_Icons;

   procedure Set_Text (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Style (Toolbar, Toolbar_Text);
   end Set_Text;

   procedure Set_Both (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Style (Toolbar, Toolbar_Both);
   end Set_Both;

   procedure Set_Small_Space (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Space_Size (Toolbar, 5);
   end Set_Small_Space;

   procedure Set_Big_Space (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Space_Size (Toolbar, 10);
   end Set_Big_Space;

   procedure Set_Enable (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Tooltips (Toolbar, True);
   end Set_Enable;

   procedure Set_Disable (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Tooltips (Toolbar, False);
   end Set_Disable;

   procedure Borders (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Button_Relief (Toolbar, Relief_Normal);
   end Borders;

   procedure Borderless (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Button_Relief (Toolbar, Relief_None);
   end Borderless;

   procedure Space_Style_Empty (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Space_Style (Toolbar, Toolbar_Space_Empty);
   end Space_Style_Empty;

   procedure Space_Style_Line (Toolbar : access Gtk_Toolbar_Record) is
   begin
      Set_Space_Style (Toolbar, Toolbar_Space_Line);
   end Space_Style_Line;

   procedure Make_Toolbar (Toolbar    : out Gtk_Toolbar;
                           Toplevel   : in out Gtk_Window;
                           With_Entry : in Boolean := False)
   is
      Id        : Guint;
      The_Entry : Gtk_Entry;
   begin
      if not Realized_Is_Set (Toplevel) then
         Realize (Toplevel);
      end if;

      Gtk_New (Toolbar, Orientation_Horizontal, Toolbar_Both);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar,
                      Text => "Horizontal",
                      Tooltip_Text => "Horizontal_toolbar_layout",
                      Tooltip_Private_Text => "",
                      Icon => New_Pixmap ("test.xpm",
                                          Get_Window (Toplevel),
                                          Get_Bg (Get_Style (Toplevel),
                                                  State_Normal))),
         "clicked", Set_Horizontal'Access, Toolbar);

      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Vertical", "Vertical_toolbar_layout", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Vertical'Access, Toolbar);
      Append_Space (Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Icons", "Only show toolbar icons", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Icons'Access, Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Text", "Only show toolbar text", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Text'Access, Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Both", "Show toolbar icons and text", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Both'Access, Toolbar);
      Append_Space (Toolbar);

      if With_Entry then
         Gtk_New (The_Entry);
         Show (The_Entry);
         Append_Widget (Toolbar, The_Entry, "This is an unusable Gtk_Entry",
                        "Hey dont't click me!!!");
         Append_Space (Toolbar);
      end if;

      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Small", "Use small spaces", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Small_Space'Access, Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Big", "Use big spaces", "Toolbar/Big",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Big_Space'Access, Toolbar);
      Append_Space (Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Enable", "Enable_Tooltips", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Enable'Access, Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Disable", "Disable_Tooltips", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Set_Disable'Access, Toolbar);

      Append_Space (Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Borders", "Show Borders", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Borders'Access, Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Borderless", "Hide Borders", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Borderless'Access, Toolbar);

      Append_Space (Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Empty", "Empty spaces", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Space_Style_Empty'Access, Toolbar);
      Id := Toolbar_Cb.Connect
        (Append_Item (Toolbar, "Lines", "Lines in spaces", "",
                      New_Pixmap ("test.xpm",
                                  Get_Window (Toplevel),
                                  Get_Bg (Get_Style (Toplevel), State_Normal))),
         "clicked", Space_Style_Line'Access, Toolbar);

   end Make_Toolbar;


   Window : aliased Gtk_Window;

   procedure Run (Widget : access Gtk.Button.Gtk_Button_Record) is
      Id      : Guint;
      Toolbar : Gtk_Toolbar;
   begin
      if Window = null then
         Gtk_New (Window, Window_Toplevel);
         Set_Title (Window, "Toolbar test");
         Set_Policy (Window, False, True, False);
         Id := Destroy_Cb.Connect
           (Window, "destroy", Destroy_Window'Access, Window'Access);
         Set_Border_Width (Window, 0);
         Make_Toolbar (Toolbar, Window, True);
         Add (Window, Toolbar);
         Show (Toolbar);
         Show (Window);
      else
         Destroy (Window);
      end if;
   end Run;

end Create_Toolbar;

