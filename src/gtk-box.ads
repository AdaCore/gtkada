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

with Gtk.Container;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Box is

   type Gtk_Box is new Gtk.Container.Gtk_Container with private;

   function Get_Child (Box : in Gtk_Box; Num : in Gint) return Gtk_Widget;

   procedure Gtk_New_Vbox (Widget      : out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint);

   procedure Gtk_New_Hbox (Widget      : out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint);

   procedure Pack_Start
     (In_Box  : in Gtk_Box;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0);

   procedure Pack_End
     (In_Box  : in Gtk_Box;
      Child   : in Gtk.Widget.Gtk_Widget'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0);

   procedure Set_Homogeneous (In_Box : in Gtk_Box; Homogeneous : in Boolean);

   procedure Set_Spacing (In_Box : in Gtk_Box; Spacing : in Gint);

   procedure Reorder_Child
     (In_Box : in Gtk_Box;
      Child  : in Gtk_Widget'Class;
      Pos    : in Guint);

   procedure Query_Child_Packing
     (In_Box   : in Gtk_Box;
      Child    : in Gtk_Widget'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Gtk_Pack_Type);

   procedure Set_Child_Packing
     (In_Box    : in Gtk_Box;
      Child     : in Gtk.Widget.Gtk_Widget'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in Gint;
      PackType  : in Gtk_Pack_Type);

private

   type Gtk_Box is new Gtk.Container.Gtk_Container with null record;

end Gtk.Box;
