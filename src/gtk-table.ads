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

with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Table is

   type Gtk_Table_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Table is access all Gtk_Table_Record'Class;

   procedure Attach
      (Table         : access Gtk_Table_Record;
       Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : in Gint;
       Right_Attach  : in Gint;
       Top_Attach    : in Gint;
       Bottom_Attach : in Gint;
       Xoptions      : in Gtk_Attach_Options;
       Yoptions      : in Gtk_Attach_Options;
       Xpadding      : in Gint;
       Ypadding      : in Gint);
   procedure Attach_Defaults
      (Table         : access Gtk_Table_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : in Gint;
       Right_Attach  : in Gint;
       Top_Attach    : in Gint;
       Bottom_Attach : in Gint);
   procedure Gtk_New
      (Widget      : out Gtk_Table;
       Rows        : in Gint;
       Columns     : in Gint;
       Homogeneous : in Boolean);
   procedure Initialize
      (Widget      : access Gtk_Table_Record;
       Rows        : in Gint;
       Columns     : in Gint;
       Homogeneous : in Boolean);
   procedure Set_Col_Spacing
      (Table   : access Gtk_Table_Record;
       Column  : in Gint;
       Spacing : in Gint);
   procedure Set_Col_Spacings
      (Table   : access Gtk_Table_Record;
       Spacing : in Gint);
   procedure Set_Homogeneous
      (Table       : access Gtk_Table_Record;
       Homogeneous : in Boolean);
   procedure Set_Row_Spacing
      (Table   : access Gtk_Table_Record;
       Row     : in Gint;
       Spacing : in Gint);
   procedure Set_Row_Spacings
      (Table   : access Gtk_Table_Record;
       Spacing : in Gint);

private
   type Gtk_Table_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Table;
