-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk;
with Gtk.Adjustment;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Dial is

   type Gtk_Dial_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Gtk_Dial is access all Gtk_Dial_Record'Class;

   procedure Gtk_New
     (Widget     : out Gtk_Dial;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   procedure Initialize
     (Widget     : access Gtk_Dial_Record'Class;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   function Get_Adjustment (Dial   : access Gtk_Dial_Record)
                            return Gtk.Adjustment.Gtk_Adjustment;

   function Get_Percentage (Dial   : access Gtk_Dial_Record)
                            return Gfloat;

   function Get_Value (Dial   : access Gtk_Dial_Record)
                       return Gfloat;

   procedure Set_Adjustment
     (Dial       : access Gtk_Dial_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class);

   function Set_Percentage
     (Dial    : access Gtk_Dial_Record;
      Percent : Gfloat)
      return Gfloat;

   procedure Set_Update_Policy
     (Dial   : access Gtk_Dial_Record;
      Policy : Gtk_Update_Type);

   function Set_Value
     (Dial   : access Gtk_Dial_Record;
      Value  : Gfloat)
      return Gfloat;

   procedure Set_View_Only
     (Dial      : access Gtk_Dial_Record;
      View_Only : Boolean);


private
   type Gtk_Dial_Record is new Gtk.Widget.Gtk_Widget_Record with null record;

   pragma Import (C, Get_Type, "gtk_dial_get_type");
end Gtk.Dial;
