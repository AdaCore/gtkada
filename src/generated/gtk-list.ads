-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
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

--  <description>
-- 
-- 
--  </description>
--  <group>Obsolescent widgets</group>
--  <testgtk>create_list.adb</testgtk>

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Gtk.Container; use Gtk.Container;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.List is

   type Gtk_List_Record is new Gtk_Container_Record with null record;
   type Gtk_List is access all Gtk_List_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_List);
   procedure Initialize (Self : access Gtk_List_Record'Class);

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_list_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append_Items
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist);

   function Child_Position
      (Self  : access Gtk_List_Record;
       Child : access Gtk_Widget_Record'Class) return Gint;

   procedure Clear_Items
      (Self    : access Gtk_List_Record;
       Start   : Gint;
       The_End : Gint);
   --  Remove some items from the list. If The_End is negative, it means the
   --  end of the list. The first item in the list has an index of 0

   procedure End_Drag_Selection (Self : access Gtk_List_Record);

   procedure End_Selection (Self : access Gtk_List_Record);

   procedure Extend_Selection
      (Self                 : access Gtk_List_Record;
       Scroll_Type          : Gtk.Enums.Gtk_Scroll_Type;
       Position             : Gfloat;
       Auto_Start_Selection : Boolean);

   procedure Insert_Items
      (Self     : access Gtk_List_Record;
       Items    : Widget_List.Glist;
       Position : Gint);

   procedure Prepend_Items
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist);

   procedure Remove_Items
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist);

   procedure Remove_Items_No_Unref
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist);

   procedure Scroll_Horizontal
      (Self        : access Gtk_List_Record;
       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
       Position    : Gfloat);

   procedure Scroll_Vertical
      (Self        : access Gtk_List_Record;
       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
       Position    : Gfloat);

   procedure Select_All (Self : access Gtk_List_Record);

   procedure Select_Child
      (Self  : access Gtk_List_Record;
       Child : access Gtk_Widget_Record'Class);

   procedure Select_Item (Self : access Gtk_List_Record; Item : Gint);

   procedure Set_Selection_Mode
      (Self : access Gtk_List_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode);

   procedure Start_Selection (Self : access Gtk_List_Record);

   procedure Toggle_Add_Mode (Self : access Gtk_List_Record);

   procedure Toggle_Focus_Row (Self : access Gtk_List_Record);

   procedure Toggle_Row
      (Self : access Gtk_List_Record;
       Item : access Gtk_Widget_Record'Class);

   procedure Undo_Selection (Self : access Gtk_List_Record);

   procedure Unselect_All (Self : access Gtk_List_Record);

   procedure Unselect_Child
      (Self  : access Gtk_List_Record;
       Child : access Gtk_Widget_Record'Class);

   procedure Unselect_Item (Self : access Gtk_List_Record; Item : Gint);

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Selection (Widget : access Gtk.List.Gtk_List_Record)
   return Widget_List.Glist;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   -- 
   --  Name: Selection_Mode_Property
   --  Type: Gtk_Selection_Mode
   --  Flags: read-write

   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   -- 
   --  "select-child"
   --  function Handler (Self : access Gtk_List_Record'Class) return none;
   -- 
   --  "selection-changed"
   --  function Handler (Self : access Gtk_List_Record'Class) return none;
   -- 
   --  "unselect-child"
   --  function Handler (Self : access Gtk_List_Record'Class) return none;

   Signal_Select_Child : constant Glib.Signal_Name := "select-child";
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection-changed";
   Signal_Unselect_Child : constant Glib.Signal_Name := "unselect-child";

private
   Selection_Mode_Property : constant Gtk.Enums.Property_Gtk_Selection_Mode:=
     Gtk.Enums.Build ("Selection_Mode_Property");
end Gtk.List;
