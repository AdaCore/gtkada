-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with Gtk.Widget; use Gtk.Widget;
with System;

package body Gtk.Packer is

   ---------
   -- Add --
   ---------

   procedure Add
     (Packer       : access Gtk_Packer_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Side         : Gtk_Side_Type;
      Anchor       : Gtk_Anchor_Type;
      Options      : Gtk_Packer_Options;
      Border_Width : Guint;
      Pad_X        : Guint;
      Pad_Y        : Guint;
      I_Pad_X      : Guint;
      I_Pad_Y      : Guint)
   is
      procedure Internal
        (Packer       : System.Address;
         Child        : System.Address;
         Side         : Gtk_Side_Type;
         Anchor       : Gtk_Anchor_Type;
         Options      : Gtk_Packer_Options;
         Border_Width : Guint;
         Pad_X        : Guint;
         Pad_Y        : Guint;
         I_Pad_X      : Guint;
         I_Pad_Y      : Guint);
      pragma Import (C, Internal, "gtk_packer_add");

   begin
      Internal
        (Get_Object (Packer), Get_Object (Child),
         Side, Anchor, Options, Border_Width,
         Pad_X, Pad_Y, I_Pad_X, I_Pad_Y);
   end Add;

   ------------------
   -- Add_Defaults --
   ------------------

   procedure Add_Defaults
     (Packer  : access Gtk_Packer_Record;
      Child   : access Gtk.Widget.Gtk_Widget_Record'Class;
      Side    : Gtk_Side_Type;
      Anchor  : Gtk_Anchor_Type;
      Options : Gtk_Packer_Options)
   is
      procedure Internal
        (Packer  : System.Address;
         Child   : System.Address;
         Side    : Gtk_Side_Type;
         Anchor  : Gtk_Anchor_Type;
         Options : Gtk_Packer_Options);
      pragma Import (C, Internal, "gtk_packer_add_defaults");

   begin
      Internal
        (Get_Object (Packer), Get_Object (Child), Side, Anchor, Options);
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
      Position : Gint)
   is
      procedure Internal
        (Packer   : System.Address;
         Child    : System.Address;
         Position : Gint);
      pragma Import (C, Internal, "gtk_packer_reorder_child");

   begin
      Internal (Get_Object (Packer), Get_Object (Child), Position);
   end Reorder_Child;

   -----------------------
   -- Set_Child_Packing --
   -----------------------

   procedure Set_Child_Packing
     (Packer       : access Gtk_Packer_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Side         : Gtk_Side_Type;
      Anchor       : Gtk_Anchor_Type;
      Options      : Gtk_Packer_Options;
      Border_Width : Guint;
      Pad_X        : Guint;
      Pad_Y        : Guint;
      I_Pad_X      : Guint;
      I_Pad_Y      : Guint)
   is
      procedure Internal
        (Packer       : System.Address;
         Child        : System.Address;
         Side         : Gtk_Side_Type;
         Anchor       : Gtk_Anchor_Type;
         Options      : Gtk_Packer_Options;
         Border_Width : Guint;
         Pad_X        : Guint;
         Pad_Y        : Guint;
         I_Pad_X      : Guint;
         I_Pad_Y      : Guint);
      pragma Import (C, Internal, "gtk_packer_set_child_packing");

   begin
      Internal
        (Get_Object (Packer), Get_Object (Child),
         Side, Anchor, Options, Border_Width,
         Pad_X, Pad_Y, I_Pad_X, I_Pad_Y);
   end Set_Child_Packing;

   ------------------------------
   -- Set_Default_Border_Width --
   ------------------------------

   procedure Set_Default_Border_Width
     (Packer : access Gtk_Packer_Record; Border : Guint)
   is
      procedure Internal (Packer : System.Address; Border : Guint);
      pragma Import (C, Internal, "gtk_packer_set_default_border_width");

   begin
      Internal (Get_Object (Packer), Border);
   end Set_Default_Border_Width;

   ----------------------
   -- Set_Default_Ipad --
   ----------------------

   procedure Set_Default_Ipad
     (Packer  : access Gtk_Packer_Record;
      I_Pad_X : Guint;
      I_Pad_Y : Guint)
   is
      procedure Internal
        (Packer  : System.Address;
         I_Pad_X : Guint;
         I_Pad_Y : Guint);
      pragma Import (C, Internal, "gtk_packer_set_default_ipad");

   begin
      Internal (Get_Object (Packer), I_Pad_X, I_Pad_Y);
   end Set_Default_Ipad;

   ---------------------
   -- Set_Default_Pad --
   ---------------------

   procedure Set_Default_Pad
     (Packer : access Gtk_Packer_Record;
      Pad_X  : Guint;
      Pad_Y  : Guint)
   is
      procedure Internal
        (Packer : System.Address;
         Pad_X  : Guint;
         Pad_Y  : Guint);
      pragma Import (C, Internal, "gtk_packer_set_default_pad");

   begin
      Internal (Get_Object (Packer), Pad_X, Pad_Y);
   end Set_Default_Pad;

   ----------------
   -- Find_Child --
   ----------------

   function Find_Child
     (Packer : access Gtk_Packer_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Gtk_Packer_Child
   is
      function Internal
        (Packer : System.Address;
         Child  : System.Address) return Gtk_Packer_Child;
      pragma Import (C, Internal, "ada_packer_find_child");

   begin
      return Internal (Get_Object (Packer), Get_Object (Child));
   end Find_Child;

   -------------------
   -- Get_Nth_Child --
   -------------------

   function Get_Nth_Child
     (Packer : access Gtk_Packer_Record;
      N      : Guint) return Gtk_Packer_Child
   is
      function Internal
        (Packer : System.Address; N : Guint) return Gtk_Packer_Child;
      pragma Import (C, Internal, "ada_packer_get_nth_child");

   begin
      return Internal (Get_Object (Packer), N);
   end Get_Nth_Child;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
     (Packer  : access Gtk_Packer_Record;
      Spacing : Guint)
   is
      procedure Internal (Packer : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_packer_set_spacing");

   begin
      Internal (Get_Object (Packer), Spacing);
   end Set_Spacing;

end Gtk.Packer;
