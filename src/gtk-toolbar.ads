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


with Gtk.Button;
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Toolbar is

   type Gtk_Toolbar is new Gtk.Container.Gtk_Container with private;

   function Append_Element
      (Toolbar              : in Gtk_Toolbar;
       The_Type             : in Gtk_Toolbar_Child_Type;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Widget.Gtk_Widget'Class;
   --  Note : you have to set the "clicked" callback yourself, as opposed to
   --  what is done in C.

   function Append_Item
      (Toolbar              : in Gtk_Toolbar;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Button.Gtk_Button;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   procedure Append_Space (Toolbar : in Gtk_Toolbar);
   procedure Append_Widget
      (Toolbar              : in Gtk_Toolbar;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String);
   procedure Gtk_New
      (Widget      : out Gtk_Toolbar;
       Orientation : in Gtk_Orientation;
       Style       : in Gtk_Toolbar_Style);
   function Insert_Element
      (Toolbar              : in Gtk_Toolbar;
       The_Type             : in Gtk_Toolbar_Child_Type;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class;
       Position             : in Gint)
       return                    Gtk.Widget.Gtk_Widget'Class;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   function Insert_Item
      (Toolbar              : in Gtk_Toolbar;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class;
       Position             : in Gint)
       return                    Gtk.Button.Gtk_Button;
   procedure Insert_Space
      (Toolbar  : in Gtk_Toolbar;
       Position : in Gint);
   procedure Insert_Widget
      (Toolbar              : in Gtk_Toolbar;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Position             : in Gint);
   function Prepend_Element
      (Toolbar              : in Gtk_Toolbar;
       The_Type             : in Gtk_Toolbar_Child_Type;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Widget.Gtk_Widget'Class;
   function Prepend_Item
      (Toolbar              : in Gtk_Toolbar;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Button.Gtk_Button;
   procedure Prepend_Space (Toolbar : in Gtk_Toolbar);
   procedure Prepend_Widget
      (Toolbar              : in Gtk_Toolbar;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String);
   procedure Set_Orientation
      (Toolbar     : in Gtk_Toolbar;
       Orientation : in Gtk_Orientation);
   procedure Set_Space_Size
      (Toolbar    : in Gtk_Toolbar;
       Space_Size : in Gint);
   procedure Set_Style
      (Toolbar : in Gtk_Toolbar;
       Style   : in Gtk_Toolbar_Style);
   procedure Set_Tooltips
      (Toolbar : in Gtk_Toolbar;
       Enable  : in Boolean);

private
   type Gtk_Toolbar is new Gtk.Container.Gtk_Container with null record;

end Gtk.Toolbar;
