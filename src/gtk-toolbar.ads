-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

   type Gtk_Toolbar_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Toolbar is access all Gtk_Toolbar_Record'Class;

   procedure Gtk_New
     (Widget      : out Gtk_Toolbar;
      Orientation : in Gtk_Orientation;
      Style       : in Gtk_Toolbar_Style);

   procedure Initialize
     (Widget      : access Gtk_Toolbar_Record'Class;
      Orientation : in Gtk_Orientation;
      Style       : in Gtk_Toolbar_Style);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Toolbar.

   --  In all the following functions, "Tooltip_Text" can be a empty String.
   --  In that case, no tooltip will be created

   function Append_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : in Gtk_Toolbar_Child_Type;
      Widget               : Gtk.Widget.Gtk_Widget := null;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget;
   --  Note : you have to set the "clicked" callback yourself, as opposed to
   --  what is done in C.

   function Append_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   procedure Append_Space (Toolbar : access Gtk_Toolbar_Record);

   procedure Append_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "");

   function Insert_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : in Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position             : in Gint)
      return Gtk.Widget.Gtk_Widget;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   function Insert_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position             : in Gint)
      return Gtk.Button.Gtk_Button;

   procedure Insert_Space
     (Toolbar  : access Gtk_Toolbar_Record;
      Position : in Gint);

   procedure Insert_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Position             : in Gint);

   function Prepend_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : in Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Widget.Gtk_Widget;

   function Prepend_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : in String := "";
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "";
      Icon                 : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk.Button.Gtk_Button;

   procedure Prepend_Space (Toolbar : access Gtk_Toolbar_Record);

   procedure Prepend_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : in String := "";
      Tooltip_Private_Text : in String := "");

   procedure Set_Orientation
     (Toolbar     : access Gtk_Toolbar_Record;
      Orientation : in Gtk_Orientation);

   procedure Set_Space_Size
     (Toolbar    : access Gtk_Toolbar_Record;
      Space_Size : in Gint);

   procedure Set_Space_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : in Gtk_Toolbar_Space_Style);

   procedure Set_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : in Gtk_Toolbar_Style);

   procedure Set_Tooltips
     (Toolbar : access Gtk_Toolbar_Record;
      Enable  : in Boolean);

   procedure Set_Button_Relief
     (Toolbar : access Gtk_Toolbar_Record; Relief : in Gtk_Relief_Style);

   function Get_Button_Relief
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Relief_Style;

   ----------------------
   -- Support for Gate --
   ----------------------

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

private
   type Gtk_Toolbar_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   pragma Import (C, Get_Type, "gtk_toolbar_get_type");
end Gtk.Toolbar;
