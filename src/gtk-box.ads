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
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Object; use Gtk.Object;
with Gtk.Widget; use Gtk.Widget;

package Gtk.Box is

   type Gtk_Box_Record is new Gtk.Container.Gtk_Container_Record with private;
   type Gtk_Box is access all Gtk_Box_Record'Class;
   subtype Gtk_Vbox is Gtk_Box;
   subtype Gtk_Hbox is Gtk_Box;

   function Get_Child (Box : access Gtk_Box_Record; Num : in Gint)
                       return Gtk_Widget;

   procedure Gtk_New_Vbox (Box         : in out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint);
   procedure Initialize_Vbox (Box         : access Gtk_Box_Record;
                              Homogeneous : in  Boolean;
                              Spacing     : in  Gint);

   procedure Gtk_New_Hbox (Box         : in out Gtk_Box;
                           Homogeneous : in  Boolean;
                           Spacing     : in  Gint);
   procedure Initialize_Hbox (Box         : access Gtk_Box_Record;
                              Homogeneous : in  Boolean;
                              Spacing     : in  Gint);

   procedure Pack_Start
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0);

   procedure Pack_End
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand  : in Boolean := True;
      Fill    : in Boolean := True;
      Padding : in Gint    := 0);

   procedure Pack_Start_Defaults
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Pack_End_Defaults
     (In_Box  : access Gtk_Box_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Set_Homogeneous (In_Box      : access Gtk_Box_Record;
                              Homogeneous : in     Boolean);

   procedure Set_Spacing (In_Box : access Gtk_Box_Record; Spacing : in Gint);

   procedure Reorder_Child
     (In_Box : access Gtk_Box_Record;
      Child  : access Gtk_Widget_Record'Class;
      Pos    : in Guint);

   procedure Query_Child_Packing
     (In_Box   : access Gtk_Box_Record;
      Child    : access Gtk_Widget_Record'Class;
      Expand   : out Boolean;
      Fill     : out Boolean;
      Padding  : out Gint;
      PackType : out Gtk_Pack_Type);

   procedure Set_Child_Packing
     (In_Box    : access Gtk_Box_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Expand    : in Boolean;
      Fill      : in Boolean;
      Padding   : in Gint;
      PackType  : in Gtk_Pack_Type);

   --  The two following procedures are used to generate and create widgets
   --  from a Node.
 
   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Box : in out Gtk_Object; N : in Node_Ptr);

private

   type Gtk_Box_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Box;
