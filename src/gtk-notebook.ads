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

with Gdk; use Gdk;
with Glib.Glist;
with Gtk.Object; use Gtk.Object;
with Gtk.Container;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Notebook is

   type Gtk_Notebook_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Notebook is access all Gtk_Notebook_Record'Class;

   type Gtk_Notebook_Page is new Root_Type with null record;

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Append_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);

   function Get_Child (Page : in Gtk_Notebook_Page) return Gtk_Widget;

   function Get_Current_Page
     (Notebook : access Gtk_Notebook_Record) return Gint;
   function Get_Cur_Page
     (Widget : access Gtk_Notebook_Record'Class) return Gtk_Notebook_Page;
   --  Note: This function returns a record type instead of an access type
   --  because there is no easy way to automatically free the memory for a
   --  Gtk_Notebook_Page

   function Get_Menu_Label
     (Page : in Gtk_Notebook_Page) return Gtk_Widget;
   function Get_Tab_Label
     (Page : in Gtk_Notebook_Page) return Gtk_Widget;
   function Get_Tab_Pos
     (Widget : access Gtk_Notebook_Record) return Gtk_Position_Type;
   procedure Gtk_New (Widget : out Gtk_Notebook);
   procedure Initialize (Widget : access Gtk_Notebook_Record'Class);
   procedure Insert_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position  : in Gint);
   procedure Insert_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position   : in Gint);
   procedure Next_Page (Notebook : access Gtk_Notebook_Record);
   procedure Popup_Disable (Notebook : access Gtk_Notebook_Record);
   procedure Popup_Enable (Notebook : access Gtk_Notebook_Record);
   procedure Prepend_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Prepend_Page_Menu
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Prev_Page (Notebook : access Gtk_Notebook_Record);
   procedure Query_Tab_Label_Packing
     (Notebook   : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand     : out Boolean;
      Fill       : out Boolean;
      Pack_Type  : out Gtk_Pack_Type);
   procedure Remove_Page
     (Notebook : access Gtk_Notebook_Record;
      Page_Num : in Gint);
   procedure Set_Homogeneous_Tabs
     (Notebook    : access Gtk_Notebook_Record;
      Homogeneous : in Boolean);
   procedure Set_Page
     (Notebook : access Gtk_Notebook_Record;
      Page_Num : in Gint);
   procedure Set_Scrollable
     (Notebook   : access Gtk_Notebook_Record;
      Scrollable : in Boolean);
   procedure Set_Show_Border
     (Notebook    : access Gtk_Notebook_Record;
      Show_Border : in Boolean);
   procedure Set_Show_Tabs
     (Notebook  : access Gtk_Notebook_Record;
      Show_Tabs : in Boolean);
   procedure Set_Tab_Border
     (Notebook     : access Gtk_Notebook_Record;
      Border_Width : in Gint);
   procedure Set_Tab_Hborder
     (Notebook     : access Gtk_Notebook_Record;
      Border_Width : in Gint);
   procedure Set_Tab_Vborder
     (Notebook     : access Gtk_Notebook_Record;
      Border_Width : in Gint);
   procedure Set_Tab_Label_Packing
     (Notebook  : access Gtk_Notebook_Record;
      Child      : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand     : in Boolean;
      Fill       : in Boolean;
      Pack_Type  : in Gtk_Pack_Type);
   procedure Set_Tab_Pos
     (Notebook : access Gtk_Notebook_Record;
      Pos      : in Gtk_Position_Type);

   --  List of Pages

   function Convert (W : in Gtk_Notebook_Page) return System.Address;
   function Convert (W : System.Address) return Gtk_Notebook_Page;
   package Page_List is new Glib.Glist.Generic_List
     (Gtk_Notebook_Page);
   function Get_Children
     (Widget : access Gtk_Notebook_Record) return Page_List.Glist;

   --  The two following procedures are used to generate and create widgets
   --  from a Node.

   procedure Set_Tab (Notebook  : access Gtk_Notebook_Record;
                      Page_Num  : in Gint;
                      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set Notebook tab widget

   procedure Generate (N        : in Node_Ptr;
                       File     : in File_Type);

   procedure Generate (Notebook : in out Gtk_Object;
                       N        : in Node_Ptr);

private
   type Gtk_Notebook_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Notebook;
