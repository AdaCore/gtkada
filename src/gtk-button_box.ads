-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                 --
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
--  A Gtk_Button_Box is a special type of Gtk_Box specially tailored to contain
--  buttons.
--
--  This is only a base class for Gtk_Hbutton_Box and Gtk_Vbutton_Box which
--  provide a way to arrange their children horizontally (resp. vertically).
--  You can not instantiate a Gtk_Button_Box directly, and have to use one the
--  above two instead.
--
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Layout containers</group>

with Glib.Properties;
with Gtk.Box;
with Gtk.Enums;
with Gtk.Widget;

package Gtk.Button_Box is

   type Gtk_Button_Box_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Button_Box is access all Gtk_Button_Box_Record'Class;

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Button_Box.

   procedure Set_Layout
     (Button_Box   : access Gtk_Button_Box_Record;
      Layout_Style : Enums.Gtk_Button_Box_Style);
   function Get_Layout
     (Button_Box : access Gtk_Button_Box_Record)
      return Enums.Gtk_Button_Box_Style;
   --  Set the layout to use for the box.
   --  There are four such styles:
   --
   --  - Buttonbox_Spread: The children are spread regularly across the box
   --
   --  - Buttonbox_Edge  : Same as Spread, except that the first and last
   --                      children are aligned on the border of the box.
   --
   --  - Buttonbox_Start : The children are put as much to the left (resp. top)
   --                      as possible in the box.
   --
   --  - Buttonbox_End   : The children are put as much to the right
   --                      (resp. bottom) as possible in the box.

   procedure Set_Child_Secondary
     (Button_Box   : access Gtk_Button_Box_Record;
      Child        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Is_Secondary : Boolean);
   function Get_Child_Secondary
     (Widget : access Gtk_Button_Box_Record;
      Child  : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Boolean;
   --  Set whether Child should appear in a secondary group of children.
   --  A typical use of a secondary child is the help button in a dialog.
   --
   --  This group appears after the other children if the style is
   --  Buttonbox_Start, Buttonbox_Spread or Buttonbox_Edge, and before the
   --  other children if the style is Buttonbox_End. For horizontal button
   --  boxes, the definition of before/after depends on direction of the
   --  widget. (See Gtk.Widget.Set_Direction) If the style is Buttonbox_Start,
   --  or Buttonbox_End, then the secondary children are aligned at
   --  the other end of the button box from the main children. For the
   --  other styles, they appear immediately next to the main children.
   --
   --  Is_Secondary: if True, the Child appears in a secondary group of the
   --                button box.

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   procedure Set_Child_Size
     (Button_Box : access Gtk_Button_Box_Record;
      Min_Width  : Gint;
      Min_Height : Gint);
   pragma Obsolescent;  --  Set_Child_Size
   --  Set the size to use for children of this specific box.
   --  You can modify the size for all the boxes at once by using
   --  Set_Child_Size_Default.

   procedure Get_Child_Size
     (Button_Box : access Gtk_Button_Box_Record;
      Min_Width  : out Gint;
      Min_Height : out Gint);
   pragma Obsolescent;  --  Get_Child_Size
   --  Return the size to use for children of this specific box.
   --  Min_Width and Min_Height are set to -1 if this widget uses the default
   --  sizes that are set by Set_Child_Size_Default.

   procedure Set_Child_Ipadding
     (Button_Box : access Gtk_Button_Box_Record;
      Ipad_X     : Gint;
      Ipad_Y     : Gint);
   pragma Obsolescent; --  Set_Child_Ipadding
   --  no-op procedure.

   --  </doc_ignore>

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Layout_Style_Property
   --    Type:  Gtk_Button_Box_Style
   --    Flags: read-write
   --    Descr: How to layout the buttons in the box.
   --    See also: Same as calling Set_Layout.
   --
   --  - Name:  Child_Secondary_Property
   --    Type:  Boolean
   --    Flags: read-write
   --    Descr: If True, the child appears in a secondary group of children,
   --           suitable for, e.g., help buttons.
   --    See also: Same as calling Set_Child_Secondary.
   --
   --  </properties>

   Layout_Style_Property : constant Gtk.Enums.Property_Gtk_Button_Box_Style;
   Child_Secondary_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Button_Box_Record is new Gtk.Box.Gtk_Box_Record with null record;
   pragma Import (C, Get_Type, "gtk_button_box_get_type");

   Layout_Style_Property : constant Gtk.Enums.Property_Gtk_Button_Box_Style :=
      Gtk.Enums.Build ("layout_style");
   Child_Secondary_Property : constant Glib.Properties.Property_Boolean :=
      Glib.Properties.Build ("secondary");

end Gtk.Button_Box;

--  The following functions never had a binding, are now obsolescent
--  No binding: gtk_button_box_get_child_ipadding
