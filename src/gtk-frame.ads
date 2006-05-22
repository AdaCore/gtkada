-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  <c_version>2.8.17</c_version>

with Glib.Properties;
with Gtk.Bin;
with Gtk.Widget;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Frame is

   type Gtk_Frame_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Frame is access all Gtk_Frame_Record'Class;

   procedure Gtk_New (Frame : out Gtk_Frame; Label : UTF8_String := "");
   --  Create a new frame.
   --  If Label is not the empty string, the frame will have a title.

   procedure Initialize
     (Frame : access Gtk_Frame_Record'Class; Label : UTF8_String := "");
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Frame.

   procedure Set_Label
     (Frame : access Gtk_Frame_Record; Label : UTF8_String := "");
   function Get_Label (Frame : access Gtk_Frame_Record) return UTF8_String;
   --  Change the label of the frame dynamically.
   --  If Label is the empty string, the frame's label is deleted.

   procedure Set_Label_Widget
     (Frame        : access Gtk_Frame_Record;
      Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Label_Widget
     (Frame : access Gtk_Frame_Record) return Gtk.Widget.Gtk_Widget;
   --  Set the label widget for the frame.
   --  This is the widget that will appear embedded in the top edge of the
   --  frame as a title.

   procedure Set_Label_Align
     (Frame  : access Gtk_Frame_Record;
      Xalign : Gfloat := 0.0;
      Yalign : Gfloat := 0.0);
   --  Change the alignment of the title in the frame.
   --  Xalign and Yalign are both percents that indicate the exact position
   --  of the label relative to the top-left corner of the frame.
   --  Note that Yalign is currently ignored, and the label can only be
   --  displayed on the top of the frame (0.0 for Xalign means align the label
   --  on the left, 1.0 means align the label on the right).

   procedure Get_Label_Align
     (Frame  : access Gtk_Frame_Record;
      Xalign : out Gfloat;
      Yalign : out Gfloat);
   --  Return the X and Y alignments of the title in the frame.

   procedure Set_Shadow_Type
     (Frame    : access Gtk_Frame_Record;
      The_Type : Gtk_Shadow_Type);
   --  Change the visual aspect of the frame.

   function Get_Shadow_Type
     (Frame : access Gtk_Frame_Record) return Gtk_Shadow_Type;
   --  Return the visual aspect of the frame.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Label_Property
   --  Type:  UTF8_String
   --  Flags: read-write
   --  Descr: Text of the frame's label.
   --  See also: Set_Label and Get_Label
   --
   --  Name:  Shadow_Property
   --  Type:  Gtk_Shadow_Type
   --  Flags: read-write
   --  Descr: Appearance of the frameborder.
   --  See also: Set_Shadow_Type
   --
   --  Name:  Label_Widget_Property
   --  Type:  Gtk_Widget'Class
   --  Flags: read-write
   --  Descr: A widget to display in place of the usual frame label.
   --  See also: Set_Label_Widget
   --
   --  Name:  Label_Xalign_Property
   --  Type:  Float
   --  Descr: The horizontal alignment of the label
   --
   --  Name:  Label_Yalign_Property
   --  Type:  Float
   --  Descr: The vertical alignment of the label
   --
   --  Name:  Shadow_Type_Property
   --  Type:  Enum
   --  Descr: Appearance of the frame border
   --
   --  </properties>

   Label_Property        : constant Glib.Properties.Property_String;
   Label_Widget_Property : constant Glib.Properties.Property_Object;
   Label_Xalign_Property : constant Glib.Properties.Property_Float;
   Label_Yalign_Property : constant Glib.Properties.Property_Float;
   Shadow_Type_Property  : constant Gtk.Enums.Property_Gtk_Shadow_Type;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Frame_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

   Label_Property        : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Label_Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("label_xalign");
   Label_Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("label_yalign");
   Shadow_Type_Property  : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label_widget");

   pragma Import (C, Get_Type, "gtk_frame_get_type");
end Gtk.Frame;
