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

with Gtk.Adjustment;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Object; use Gtk.Object;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Container is

   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Container is access all Gtk_Container_Record'Class;

   procedure Add (Container : access Gtk_Container_Record;
                  Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Border_Width (Container : access Gtk_Container_Record;
                               Border_Width : in Gint);

   procedure Remove (Container : access Gtk_Container_Record;
                     Widget    : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Focus_Hadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Set_Focus_Vadjustment
     (Container  : access Gtk_Container_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Set_Resize_Mode (Container   : access Gtk_Container_Record;
                              Resize_Mode : in Gtk_Resize_Mode);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.
 
   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);
 
   procedure Generate (Container : in out Gtk_Object; N : in Node_Ptr);

private

   type Gtk_Container_Record is new Gtk.Widget.Gtk_Widget_Record
     with null record;

end Gtk.Container;
