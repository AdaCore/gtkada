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

--  <description>
--
--  A Gtk_Frame is a simple border than can be added to any widget or
--  group of widget to enhance its visual aspect.
--  Optionally, a frame can have a title.
--
--  This is a very convenient widget to visually group related widgets (like
--  groups of buttons for instance), possibly with a title to explain the
--  purpose of this group.
--
--  A Gtk_Frame has only one child, so you have to put a container like for
--  instance a Gtk_Box inside if you want the frame to surround multiple
--  widgets.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Frame is

   type Gtk_Frame_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Frame is access all Gtk_Frame_Record'Class;

   procedure Gtk_New (Frame : out Gtk_Frame;
                      Label : in String := "");
   --  Create a new frame.
   --  If Label is not the empty string, the frame will have a title.

   procedure Initialize (Frame : access Gtk_Frame_Record'Class;
                         Label : in String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Frame.

   procedure Set_Label (Frame : access Gtk_Frame_Record;
                        Label : in String := "");
   --  Change the label of the frame dynamically.
   --  If Label is the empty string, the frame's label is deleted.

   procedure Set_Label_Align
     (Frame  : access Gtk_Frame_Record;
      Xalign : in Gfloat := 0.0;
      Yalign : in Gfloat := 0.0);
   --  Change the alignment of the title in the frame.
   --  Xalign and Yalign are both percents that indicate the exact position
   --  of the label relative to the top-left corner of the frame.
   --  Note that Yalign is currently ignored, and the label can only be
   --  displayed on the top of the frame (0.0 for Xalign means align the label
   --  on the left, 1.0 means align the label on the right).

   procedure Set_Shadow_Type
     (Frame    : access Gtk_Frame_Record;
      The_Type : in Gtk_Shadow_Type);
   --  Change the visual aspect of the frame.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Frame_Record is new Gtk.Bin.Gtk_Bin_Record with null record;
   pragma Import (C, Get_Type, "gtk_frame_get_type");
end Gtk.Frame;
