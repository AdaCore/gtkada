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
--  A Gtk_Button_Box is a special type of Gtk_Box specially tailored to
--  contain buttons.
--
--  This is only a base class for Gtk_Hbutton_Box and Gtk_Vbutton_Box which
--  provide a way to arrange their children horizontally (resp. vertically).
--  You can not instantiate a Gtk_Button_Box directly, and have to use one the
--  above two instead.
--
--  </description>
--  <screenshot>gtk-button_box</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_button_box.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Types;     use Glib.Types;
with Gtk.Box;        use Gtk.Box;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Orientable; use Gtk.Orientable;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Button_Box is

   type Gtk_Button_Box_Record is new Gtk_Box_Record with null record;
   type Gtk_Button_Box is access all Gtk_Button_Box_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Widget      : out Gtk_Button_Box;
       Orientation : Gtk.Enums.Gtk_Orientation);
   procedure Initialize
      (Widget      : not null access Gtk_Button_Box_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation);
   --  Creates a new Gtk.Button_Box.Gtk_Button_Box.
   --  Since: gtk+ 3.0
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "orientation": the box's orientation.

   function Gtk_Button_Box_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Button_Box;
   --  Creates a new Gtk.Button_Box.Gtk_Button_Box.
   --  Since: gtk+ 3.0
   --  "orientation": the box's orientation.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_button_box_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child_Non_Homogeneous
      (Widget : not null access Gtk_Button_Box_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Returns whether the child is exempted from homogenous sizing.
   --  Since: gtk+ 3.2
   --  "child": a child of Widget

   procedure Set_Child_Non_Homogeneous
      (Widget          : not null access Gtk_Button_Box_Record;
       Child           : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Non_Homogeneous : Boolean);
   --  Sets whether the child is exempted from homogeous sizing.
   --  Since: gtk+ 3.2
   --  "child": a child of Widget
   --  "non_homogeneous": the new value

   function Get_Child_Secondary
      (Widget : not null access Gtk_Button_Box_Record;
       Child  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Returns whether Child should appear in a secondary group of children.
   --  Since: gtk+ 2.4
   --  "child": a child of Widget

   procedure Set_Child_Secondary
      (Widget       : not null access Gtk_Button_Box_Record;
       Child        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Is_Secondary : Boolean);
   --  Sets whether Child should appear in a secondary group of children. A
   --  typical use of a secondary child is the help button in a dialog.
   --  This group appears after the other children if the style is
   --  Gtk.Enums.Buttonbox_Start, Gtk.Enums.Buttonbox_Spread or
   --  Gtk.Enums.Buttonbox_Edge, and before the other children if the style is
   --  Gtk.Enums.Buttonbox_End. For horizontal button boxes, the definition of
   --  before/after depends on direction of the widget (see
   --  Gtk.Widget.Set_Direction). If the style is Gtk.Enums.Buttonbox_Start or
   --  Gtk.Enums.Buttonbox_End, then the secondary children are aligned at the
   --  other end of the button box from the main children. For the other
   --  styles, they appear immediately next to the main children.
   --  "child": a child of Widget
   --  "is_secondary": if True, the Child appears in a secondary group of the
   --  button box.

   function Get_Layout
      (Widget : not null access Gtk_Button_Box_Record)
       return Gtk.Enums.Gtk_Button_Box_Style;
   --  Retrieves the method being used to arrange the buttons in a button box.

   procedure Set_Layout
      (Widget       : not null access Gtk_Button_Box_Record;
       Layout_Style : Gtk.Enums.Gtk_Button_Box_Style);
   --  Changes the way buttons are arranged in their container.
   --  "layout_style": the new layout style

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Button_Box_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Button_Box_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Layout_Style_Property : constant Gtk.Enums.Property_Gtk_Button_Box_Style;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Button_Box_Record, Gtk_Button_Box);
   function "+"
     (Widget : access Gtk_Button_Box_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Button_Box
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Button_Box_Record, Gtk_Button_Box);
   function "+"
     (Widget : access Gtk_Button_Box_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Button_Box
   renames Implements_Gtk_Orientable.To_Object;

private
   Layout_Style_Property : constant Gtk.Enums.Property_Gtk_Button_Box_Style :=
     Gtk.Enums.Build ("layout-style");
end Gtk.Button_Box;
