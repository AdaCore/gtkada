-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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
with Gtk.Container; use Gtk.Container;
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

   package Toolbar_Cb is new Signal.Object_Callback (Gtk_Toolbar);

   function New_Pixmap (Filename   : in String;
                        Window     : in Gdk_Window'Class;
                        Background : in Gdk_Color)
                        return Gtk_Pixmap
   is
      Pixmap    : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      GtkPixmap : Gtk_Pixmap;
   begin
      Create_From_Xpm (Pixmap, Window, Mask, Background, Filename);
      Gtk_New (GtkPixmap, Pixmap, Mask);
      return GtkPixmap;
   end New_Pixmap;

   procedure Set_Horizontal (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Orientation (Toolbar, Orientation_Horizontal);
   end Set_Horizontal;

   procedure Set_Vertical (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Orientation (Toolbar, Orientation_Vertical);
   end Set_Vertical;

   procedure Set_Icons (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Icons);
   end Set_Icons;

   procedure Set_Text (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Text);
   end Set_Text;

   procedure Set_Both (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Style (Toolbar, Toolbar_Both);
   end Set_Both;

   procedure Set_Small_Space (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Space_Size (Toolbar, 5);
   end Set_Small_Space;

   procedure Set_Big_Space (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Space_Size (Toolbar, 10);
   end Set_Big_Space;

   procedure Set_Enable (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Tooltips (Toolbar, True);
   end Set_Enable;

   procedure Set_Disable (Toolbar : in out Gtk_Toolbar'Class) is
   begin
      Set_Tooltips (Toolbar, False);
   end Set_Disable;

   procedure Make_Toolbar (Toolbar    : out Gtk_Toolbar;
                           Toplevel   : in out Gtk_Window;
                           With_Entry : in Boolean := False)
   is
      Id        : Guint;
      Pixmap    : Gtk_Pixmap;
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

   end Make_Toolbar;


   Window : aliased Gtk_Window;

   procedure Run (Widget : in out Gtk.Button.Gtk_Button'Class) is
      Id      : Guint;
      Toolbar : Gtk_Toolbar;
   begin
      if not Is_Created (Window) then
         Gtk_New (Window, Window_Toplevel);
         Set_Title (Window, "Toolbar test");
         Set_Policy (Window, False, True, False);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                   Window'Access);
         Border_Width (Window, 0);
      end if;
      if Visible_Is_Set (Window) then
         Gtk.Widget.Destroy (Window);
      else
         Make_Toolbar (Toolbar, Window, True);
         Add (Window, Toolbar);
         Show (Toolbar);
         Show (Window);
      end if;
   end Run;

end Create_Toolbar;

