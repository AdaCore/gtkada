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
with Gtk.Widget;
with System;

package body Gtk.Packer is

   ---------
   -- Add --
   ---------

   procedure Add (Packer       : access Gtk_Packer_Record;
                  Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
                  Side         : in     Gtk_Side_Type;
                  Anchor       : in     Gtk_Anchor_Type;
                  Options      : in     Gtk_Packer_Options;
                  Border_Width : in     Guint;
                  Pad_X        : in     Guint;
                  Pad_Y        : in     Guint;
                  I_Pad_X      : in     Guint;
                  I_Pad_Y      : in     Guint) is
      procedure Internal (Packer       : in System.Address;
                          Child        : in System.Address;
                          Side         : in Gint;
                          Anchor       : in Gint;
                          Options      : in Gint;
                          Border_Width : in Guint;
                          Pad_X        : in Guint;
                          Pad_Y        : in Guint;
                          I_Pad_X      : in Guint;
                          I_Pad_Y      : in Guint);
      pragma Import (C, Internal, "gtk_packer_add");
   begin
      Internal (Get_Object (Packer),
                Get_Object (Child),
                Gtk_Side_Type'Pos (Side),
                Gtk_Anchor_Type'Pos (Anchor),
                Gtk_Packer_Options'Pos (Options),
                Border_Width,
                Pad_X,
                Pad_Y,
                I_Pad_X,
                I_Pad_Y);
   end Add;

   ------------------
   -- Add_Defaults --
   ------------------

   procedure Add_Defaults
     (Packer  : access Gtk_Packer_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Side    : in     Gtk_Side_Type;
      Anchor  : in     Gtk_Anchor_Type;
      Options : in     Gtk_Packer_Options)
   is
      procedure Internal (Packer  : in System.Address;
                          Child   : in System.Address;
                          Side    : in Gint;
                          Anchor  : in Gint;
                          Options : in Gint);
      pragma Import (C, Internal, "gtk_packer_add_defaults");
   begin
      Internal (Get_Object (Packer),
                Get_Object (Child),
                Gtk_Side_Type'Pos (Side),
                Gtk_Anchor_Type'Pos (Anchor),
                Gtk_Packer_Options'Pos (Options));
   end Add_Defaults;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Packer) is
   begin
      Widget := new Gtk_Packer_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Packer_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_packer_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
     (Packer   : access Gtk_Packer_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Position : in     Gint) is
      procedure Internal (Packer   : in System.Address;
                          Child    : in System.Address;
                          Position : in Gint);
      pragma Import (C, Internal, "gtk_packer_reorder_child");
   begin
      Internal (Get_Object (Packer),
                Get_Object (Child),
                Position);
   end Reorder_Child;

   -----------------------
   -- Set_Child_Packing --
   -----------------------

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
      I_Pad_Y      : in     Guint) is
      procedure Internal (Packer       : in System.Address;
                          Child        : in System.Address;
                          Side         : in Gint;
                          Anchor       : in Gint;
                          Options      : in Gint;
                          Border_Width : in Guint;
                          Pad_X        : in Guint;
                          Pad_Y        : in Guint;
                          I_Pad_X      : in Guint;
                          I_Pad_Y      : in Guint);
      pragma Import (C, Internal, "gtk_packer_set_child_packing");
   begin
      Internal (Get_Object (Packer),
                Get_Object (Child),
                Gtk_Side_Type'Pos (Side),
                Gtk_Anchor_Type'Pos (Anchor),
                Gtk_Packer_Options'Pos (Options),
                Border_Width,
                Pad_X,
                Pad_Y,
                I_Pad_X,
                I_Pad_Y);
   end Set_Child_Packing;

   ------------------------------
   -- Set_Default_Border_Width --
   ------------------------------

   procedure Set_Default_Border_Width (Packer : access Gtk_Packer_Record;
                                       Border : in     Guint) is
      procedure Internal (Packer : in System.Address;
                          Border : in Guint);
      pragma Import (C, Internal, "gtk_packer_set_default_border_width");
   begin
      Internal (Get_Object (Packer),
                Border);
   end Set_Default_Border_Width;

   ----------------------
   -- Set_Default_Ipad --
   ----------------------

   procedure Set_Default_Ipad (Packer  : access Gtk_Packer_Record;
                               I_Pad_X : in     Guint;
                               I_Pad_Y : in     Guint) is
      procedure Internal (Packer  : in System.Address;
                          I_Pad_X : in Guint;
                          I_Pad_Y : in Guint);
      pragma Import (C, Internal, "gtk_packer_set_default_ipad");
   begin
      Internal (Get_Object (Packer),
                I_Pad_X,
                I_Pad_Y);
   end Set_Default_Ipad;

   ---------------------
   -- Set_Default_Pad --
   ---------------------

   procedure Set_Default_Pad (Packer : access Gtk_Packer_Record;
                              Pad_X  : in     Guint;
                              Pad_Y  : in     Guint) is
      procedure Internal (Packer : in System.Address;
                          Pad_X  : in Guint;
                          Pad_Y  : in Guint);
      pragma Import (C, Internal, "gtk_packer_set_default_pad");
   begin
      Internal (Get_Object (Packer),
                Pad_X,
                Pad_Y);
   end Set_Default_Pad;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing (Packer  : access Gtk_Packer_Record;
                          Spacing : in     Guint) is
      procedure Internal (Packer  : in System.Address;
                          Spacing : in Guint);
      pragma Import (C, Internal, "gtk_packer_set_spacing");
   begin
      Internal (Get_Object (Packer),
                Spacing);
   end Set_Spacing;

end Gtk.Packer;
