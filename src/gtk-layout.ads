-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
with Gtk.Container;
with Gtk.Widget;
with Gtk.Object;

package Gtk.Layout is

   type Gtk_Layout_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Layout is access all Gtk_Layout_Record'Class;

   procedure Gtk_New
     (Layout      : out Gtk_Layout;
      Hadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);

   procedure Initialize
     (Layout      : access Gtk_Layout_Record'Class;
      Hadjustment : Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : Gtk.Adjustment.Gtk_Adjustment);

   procedure Put (Layout : access Gtk_Layout_Record;
                  Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                  X      : in     Gint;
                  Y      : in     Gint);

   procedure Move (Layout : access Gtk_Layout_Record;
                   Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                   X      : in     Gint;
                   Y      : in     Gint);

   procedure Set_Size (Layout : access Gtk_Layout_Record;
                       Width  : in     Guint;
                       Height : in     Guint);

   function Get_Hadjustment (Layout : access Gtk_Layout_Record)
     return Gtk.Adjustment.Gtk_Adjustment;

   function Get_Vadjustment (Layout : access Gtk_Layout_Record)
     return Gtk.Adjustment.Gtk_Adjustment;

   procedure Set_Hadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);

   procedure Set_Vadjustment
     (Layout     : access Gtk_Layout_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);

   --  procedure Freeze
   --  procedure Thaw
   --  Not bound because does not work in Gtk+ 1.2.0...

   procedure Generate (N : in Node_Ptr; File : in File_Type);
   --  Gate internal function

   procedure Generate (Layout : in out Object.Gtk_Object; N : in Node_Ptr);
   --  Dgate internal function

private

   type Gtk_Layout_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Layout;
