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

with Gtk.Box;
with Gtk.Enums;

package Gtk.Button_Box is

   type Gtk_Button_Box_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Button_Box is access all Gtk_Button_Box_Record'Class;

   procedure Get_Child_Size_Default (Min_Width  : out Gint;
                                     Min_Height : out Gint);

   procedure Get_Child_Ipadding_Default (Ipad_X : out Gint;
                                         Ipad_Y : out Gint);

   procedure Set_Child_Size_Default (Min_Width  : in Gint;
                                     Min_Height : in Gint);

   procedure Set_Child_Ipadding_Default (Ipad_X : in Gint;
                                         Ipad_Y : in Gint);

   function Get_Spacing (Widget : access Gtk_Button_Box_Record) return Gint;

   function Get_Layout (Widget : access Gtk_Button_Box_Record)
                        return Enums.Gtk_Button_Box_Style;

   procedure Get_Child_Size (Widget     : access Gtk_Button_Box_Record;
                             Min_Width  :    out Gint;
                             Min_Height :    out Gint);

   procedure Get_Child_Ipadding (Widget : access Gtk_Button_Box_Record;
                                 Ipad_X :    out Gint;
                                 Ipad_Y :    out Gint);

   procedure Set_Spacing (Widget  : access Gtk_Button_Box_Record;
                          Spacing : in Gint);

   procedure Set_Layout (Widget       : access Gtk_Button_Box_Record;
                         Layout_Style : in Enums.Gtk_Button_Box_Style);

   procedure Set_Child_Size (Widget     : access Gtk_Button_Box_Record;
                             Min_Width  : in Gint;
                             Min_Height : in Gint);

   procedure Set_Child_Ipadding (Widget : access Gtk_Button_Box_Record;
                                 Ipad_X : in Gint;
                                 Ipad_Y : in Gint);

private
   type Gtk_Button_Box_Record is new Gtk.Box.Gtk_Box_Record with null record;

end Gtk.Button_Box;
