-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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

--  <c_version>1.3.11</c_version>

with Glib;
with Gtk.Button;
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Toolbar is

   type Gtk_Toolbar_Record is new
     Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Toolbar is access all Gtk_Toolbar_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Toolbar);

   procedure Gtk_New
     (Widget      : out Gtk_Toolbar;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style);
   --  This procedure is obsolete, and is provided for upward compatibility
   --  only.

   procedure Initialize (Widget : access Gtk_Toolbar_Record'Class);

   procedure Initialize
     (Widget      : access Gtk_Toolbar_Record'Class;
      Orientation : Gtk_Orientation;
      Style       : Gtk_Toolbar_Style);
   --  Initialization function.
   --  This procedure is obsolete, and is provided for upward compatibility
   --  only.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Toolbar.

   --  In all the following functions, "Tooltip_Text" can be a empty String.
   --  In that case, no tooltip will be created

   -------------------------
   -- Simple button items --
   -------------------------

   function Append_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   function Prepend_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Button.Gtk_Button;

   function Insert_Item
     (Toolbar              : access Gtk_Toolbar_Record;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null;
      Position             : Gint)
      return Gtk.Button.Gtk_Button;

   -----------------
   -- Stock items --
   -----------------

   function Insert_Stock
     (Toolbar              : access Gtk_Toolbar_Record;
      Stock_Id             : UTF8_String;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Position             : Gint := -1) return Gtk.Button.Gtk_Button;

   -----------------
   -- Space items --
   -----------------

   procedure Append_Space (Toolbar : access Gtk_Toolbar_Record);

   procedure Prepend_Space (Toolbar : access Gtk_Toolbar_Record);

   procedure Insert_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint);

   procedure Remove_Space
     (Toolbar : access Gtk_Toolbar_Record; Position : Gint);

   ----------------------
   -- Any element type --
   ----------------------

   function Append_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : Gtk.Widget.Gtk_Widget := null;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget;
   --  Note : you have to set the "clicked" callback yourself, as opposed to
   --  what is done in C.

   function Prepend_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null)
      return Gtk.Widget.Gtk_Widget;

   function Insert_Element
     (Toolbar              : access Gtk_Toolbar_Record;
      The_Type             : Gtk_Toolbar_Child_Type;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text                 : UTF8_String := "";
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Icon                 : Gtk.Widget.Gtk_Widget := null;
      Position             : Gint)
      return Gtk.Widget.Gtk_Widget;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   ---------------------
   -- Generic widgets --
   ---------------------

   procedure Append_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "");

   procedure Prepend_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "");

   procedure Insert_Widget
     (Toolbar              : access Gtk_Toolbar_Record;
      Widget               : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tooltip_Text         : UTF8_String := "";
      Tooltip_Private_Text : UTF8_String := "";
      Position             : Gint);

   ---------------------
   -- Style functions --
   ---------------------

   procedure Set_Orientation
     (Toolbar     : access Gtk_Toolbar_Record;
      Orientation : Gtk_Orientation);

   procedure Set_Style
     (Toolbar : access Gtk_Toolbar_Record;
      Style   : Gtk_Toolbar_Style);

   procedure Set_Icon_Size
     (Toolbar   : access Gtk_Toolbar_Record;
      Icon_Size : Gtk_Icon_Size);

   procedure Set_Tooltips
     (Toolbar : access Gtk_Toolbar_Record;
      Enable  : Boolean);

   procedure Unset_Style (Toolbar : access Gtk_Toolbar_Record);

   procedure Unset_Icon_Size (Toolbar : access Gtk_Toolbar_Record);

   function Get_Orientation
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Orientation;

   function Get_Style
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Toolbar_Style;

   function Get_Icon_Size
     (Toolbar : access Gtk_Toolbar_Record) return Gtk_Icon_Size;

   function Get_Tooltips
     (Toolbar : access Gtk_Toolbar_Record) return Boolean;

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

private
   type Gtk_Toolbar_Record is
     new Gtk.Container.Gtk_Container_Record with null record;

   pragma Import (C, Get_Type, "gtk_toolbar_get_type");
end Gtk.Toolbar;
