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

package Gtk.Packer is

   type Gtk_Packer_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Packer is access all Gtk_Packer_Record'Class;


   type Gtk_Packer_Options is new Guint;
   Gtk_Pack_Expand : constant Gtk_Packer_Options;
   Gtk_Fill_X      : constant Gtk_Packer_Options;
   Gtk_Fill_Y      : constant Gtk_Packer_Options;

   type Gtk_Side_Type is (Side_Top,
                          Side_Bottom,
                          Side_Left,
                          Side_Right);

   type Gtk_Anchor_Type is (Anchor_Center,
                            Anchor_North,
                            Anchor_North_West,
                            Anchor_North_East,
                            Anchor_South,
                            Anchor_South_East,
                            Anchor_South_West,
                            Anchor_West,
                            Anchor_East);
   Anchor_N  : Gtk_Anchor_Type renames Anchor_North;
   Anchor_NW : Gtk_Anchor_Type renames Anchor_North_West;
   Anchor_NE : Gtk_Anchor_Type renames Anchor_North_East;
   Anchor_S  : Gtk_Anchor_Type renames Anchor_South;
   Anchor_SW : Gtk_Anchor_Type renames Anchor_South_West;
   Anchor_SE : Gtk_Anchor_Type renames Anchor_South_East;
   Anchor_W  : Gtk_Anchor_Type renames Anchor_West;
   Anchor_E  : Gtk_Anchor_Type renames Anchor_East;


   procedure Add (Packer       : access Gtk_Packer_Record;
                  Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
                  Side         : in     Gtk_Side_Type;
                  Anchor       : in     Gtk_Anchor_Type;
                  Options      : in     Gtk_Packer_Options;
                  Border_Width : in     Guint;
                  Pad_X        : in     Guint;
                  Pad_Y        : in     Guint;
                  I_Pad_X      : in     Guint;
                  I_Pad_Y      : in     Guint);

   procedure Add_Defaults
     (Packer  : access Gtk_Packer_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Side    : in     Gtk_Side_Type;
      Anchor  : in     Gtk_Anchor_Type;
      Options : in     Gtk_Packer_Options);

   procedure Gtk_New (Widget : out Gtk_Packer);
   procedure Initialize (Widget : access Gtk_Packer_Record);

   procedure Reorder_Child
     (Packer   : access Gtk_Packer_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : in     Gint);

   procedure Set_Child_Packing
     (Packer       : access Gtk_Packer_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Side         : in     Gtk_Side_Type;
      Anchor       : in     Gtk_Anchor_Type;
      Options      : in     Gtk_Packer_Options;
      Border_Width : in     Guint;
      Pad_X        : in     Guint;
      Pad_Y        : in     Guint;
      I_Pad_X      : in     Guint;
      I_Pad_Y      : in     Guint);

   procedure Set_Default_Border_Width (Packer : access Gtk_Packer_Record;
                                       Border : in     Guint);

   procedure Set_Default_Ipad (Packer  : access Gtk_Packer_Record;
                               I_Pad_X : in     Guint;
                               I_Pad_Y : in     Guint);

   procedure Set_Default_Pad (Packer : access Gtk_Packer_Record;
                              Pad_X  : in     Guint;
                              Pad_Y  : in     Guint);

   procedure Set_Spacing (Packer  : access Gtk_Packer_Record;
                          Spacing : in     Guint);

private

   type Gtk_Packer_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

   Gtk_Pack_Expand : constant Gtk_Packer_Options := 2 ** 0;
   Gtk_Fill_X      : constant Gtk_Packer_Options := 2 ** 1;
   Gtk_Fill_Y      : constant Gtk_Packer_Options := 2 ** 2;

end Gtk.Packer;
