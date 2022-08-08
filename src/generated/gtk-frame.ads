------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  The frame widget is a bin that surrounds its child with a decorative frame
--  and an optional label. If present, the label is drawn in a gap in the top
--  side of the frame. The position of the label can be controlled with
--  Gtk.Frame.Set_Label_Align.
--
--  # GtkFrame as GtkBuildable
--
--  The GtkFrame implementation of the GtkBuildable interface supports placing
--  a child in the label position by specifying "label" as the "type" attribute
--  of a <child> element. A normal content child can be specified without
--  specifying a <child> type attribute.
--
--  An example of a UI definition fragment with GtkFrame: |[ <object
--  class="GtkFrame"> <child type="label"> <object class="GtkLabel"
--  id="frame-label"/> </child> <child> <object class="GtkEntry"
--  id="frame-content"/> </child> </object> ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> frame ├── border[.flat] ├── <label widget> ╰──
--  <child> ]|
--
--  GtkFrame has a main CSS node named "frame" and a subnode named "border".
--  The "border" node is used to draw the visible border. You can set the
--  appearance of the border using CSS properties like "border-style" on the
--  "border" node.
--
--  The border node can be given the style class ".flat", which is used by
--  themes to disable drawing of the border. To do this from code, call
--  Gtk.Frame.Set_Shadow_Type with Gtk.Enums.Shadow_None to add the ".flat"
--  class or any other shadow type to remove it.
--
--  </description>
--  <description>
--  This is a very convenient widget to visually group related widgets (like
--  groups of buttons for instance), possibly with a title to explain the
--  purpose of this group.
--
--  A Gtk_Frame has only one child, so you have to put a container like for
--  instance a Gtk_Box inside if you want the frame to surround multiple
--  widgets.
--
--  </description>
--  <screenshot>gtk-frame</screenshot>
--  <group>Ornaments</group>
--  <testgtk>create_frame.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Frame is

   type Gtk_Frame_Record is new Gtk_Bin_Record with null record;
   type Gtk_Frame is access all Gtk_Frame_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Frame : out Gtk_Frame; Label : UTF8_String := "");
   procedure Initialize
      (Frame : not null access Gtk_Frame_Record'Class;
       Label : UTF8_String := "");
   --  Creates a new Gtk.Frame.Gtk_Frame, with optional label Label. If Label
   --  is null, the label is omitted.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": the text to use as the label of the frame

   function Gtk_Frame_New (Label : UTF8_String := "") return Gtk_Frame;
   --  Creates a new Gtk.Frame.Gtk_Frame, with optional label Label. If Label
   --  is null, the label is omitted.
   --  "label": the text to use as the label of the frame

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_frame_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Label
      (Frame : not null access Gtk_Frame_Record) return UTF8_String;
   --  If the frame's label widget is a Gtk.Label.Gtk_Label, returns the text
   --  in the label widget. (The frame will have a Gtk.Label.Gtk_Label for the
   --  label widget if a non-null argument was passed to Gtk.Frame.Gtk_New.)

   procedure Set_Label
      (Frame : not null access Gtk_Frame_Record;
       Label : UTF8_String := "");
   --  Removes the current Gtk.Frame.Gtk_Frame:label-widget. If Label is not
   --  null, creates a new Gtk.Label.Gtk_Label with that text and adds it as
   --  the Gtk.Frame.Gtk_Frame:label-widget.
   --  "label": the text to use as the label of the frame

   procedure Get_Label_Align
      (Frame  : not null access Gtk_Frame_Record;
       Xalign : out Gfloat;
       Yalign : out Gfloat);
   --  Retrieves the X and Y alignment of the frame's label. See
   --  Gtk.Frame.Set_Label_Align.
   --  "xalign": location to store X alignment of frame's label, or null
   --  "yalign": location to store X alignment of frame's label, or null

   procedure Set_Label_Align
      (Frame  : not null access Gtk_Frame_Record;
       Xalign : Gfloat;
       Yalign : Gfloat);
   --  Sets the alignment of the frame widget's label. The default values for
   --  a newly created frame are 0.0 and 0.5.
   --  "xalign": The position of the label along the top edge of the widget. A
   --  value of 0.0 represents left alignment; 1.0 represents right alignment.
   --  "yalign": The y alignment of the label. A value of 0.0 aligns under the
   --  frame; 1.0 aligns above the frame. If the values are exactly 0.0 or 1.0
   --  the gap in the frame won't be painted because the label will be
   --  completely above or below the frame.

   function Get_Label_Widget
      (Frame : not null access Gtk_Frame_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the label widget for the frame. See
   --  Gtk.Frame.Set_Label_Widget.

   procedure Set_Label_Widget
      (Frame        : not null access Gtk_Frame_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the Gtk.Frame.Gtk_Frame:label-widget for the frame. This is the
   --  widget that will appear embedded in the top edge of the frame as a
   --  title.
   --  "label_widget": the new label widget

   function Get_Shadow_Type
      (Frame : not null access Gtk_Frame_Record)
       return Gtk.Enums.Gtk_Shadow_Type;
   --  Retrieves the shadow type of the frame. See Gtk.Frame.Set_Shadow_Type.

   procedure Set_Shadow_Type
      (Frame    : not null access Gtk_Frame_Record;
       The_Type : Gtk.Enums.Gtk_Shadow_Type);
   --  Sets the Gtk.Frame.Gtk_Frame:shadow-type for Frame, i.e. whether it is
   --  drawn without (Gtk.Enums.Shadow_None) or with (other values) a visible
   --  border. Values other than Gtk.Enums.Shadow_None are treated identically
   --  by GtkFrame. The chosen type is applied by removing or adding the .flat
   --  class to the CSS node named border.
   --  "type": the new Gtk.Enums.Gtk_Shadow_Type

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Label_Property : constant Glib.Properties.Property_String;

   Label_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Label_Xalign_Property : constant Glib.Properties.Property_Float;

   Label_Yalign_Property : constant Glib.Properties.Property_Float;

   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Frame_Record, Gtk_Frame);
   function "+"
     (Widget : access Gtk_Frame_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Frame
   renames Implements_Gtk_Buildable.To_Object;

private
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
   Label_Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("label-yalign");
   Label_Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("label-xalign");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
end Gtk.Frame;
