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
--  The Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget widget lets the user
--  select a color. By default, the chooser presents a predefined palette of
--  colors, plus a small number of settable custom colors. It is also possible
--  to select a different color with the single-color editor. To enter the
--  single-color editing mode, use the context menu of any color of the
--  palette, or use the '+' button to add a new custom color.
--
--  The chooser automatically remembers the last selection, as well as custom
--  colors.
--
--  To change the initially selected color, use Gtk.Color_Chooser.Set_Rgba. To
--  get the selected color use Gtk.Color_Chooser.Get_Rgba.
--
--  The Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget is used in the
--  Gtk.Color_Chooser_Dialog.Gtk_Color_Chooser_Dialog to provide a dialog for
--  selecting colors.
--
--  # CSS names
--
--  GtkColorChooserWidget has a single CSS node with name colorchooser.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;          use Gdk.RGBA;
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Box;           use Gtk.Box;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Color_Chooser; use Gtk.Color_Chooser;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Orientable;    use Gtk.Orientable;

package Gtk.Color_Chooser_Widget is

   type Gtk_Color_Chooser_Widget_Record is new Gtk_Box_Record with null record;
   type Gtk_Color_Chooser_Widget is access all Gtk_Color_Chooser_Widget_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Color_Chooser_Widget);
   procedure Initialize
      (Self : not null access Gtk_Color_Chooser_Widget_Record'Class);
   --  Creates a new Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget.
   --  Since: gtk+ 3.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Color_Chooser_Widget_New return Gtk_Color_Chooser_Widget;
   --  Creates a new Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget.
   --  Since: gtk+ 3.4

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_chooser_widget_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Add_Palette
      (Self            : not null access Gtk_Color_Chooser_Widget_Record;
       Orientation     : Gtk.Enums.Gtk_Orientation;
       Colors_Per_Line : Glib.Gint;
       N_Colors        : Glib.Gint;
       Colors          : array_of_Gdk_RGBA);

   procedure Get_Rgba
      (Self  : not null access Gtk_Color_Chooser_Widget_Record;
       Color : out Gdk.RGBA.Gdk_RGBA);

   procedure Set_Rgba
      (Self  : not null access Gtk_Color_Chooser_Widget_Record;
       Color : Gdk.RGBA.Gdk_RGBA);

   function Get_Use_Alpha
      (Self : not null access Gtk_Color_Chooser_Widget_Record)
       return Boolean;

   procedure Set_Use_Alpha
      (Self      : not null access Gtk_Color_Chooser_Widget_Record;
       Use_Alpha : Boolean);

   function Get_Orientation
      (Self : not null access Gtk_Color_Chooser_Widget_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Color_Chooser_Widget_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Show_Editor_Property : constant Glib.Properties.Property_Boolean;
   --  The ::show-editor property is True when the color chooser is showing
   --  the single-color editor. It can be set to switch the color chooser into
   --  single-color editing mode.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "ColorChooser"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Chooser_Widget_Record, Gtk_Color_Chooser_Widget);
   function "+"
     (Widget : access Gtk_Color_Chooser_Widget_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Chooser_Widget
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Color_Chooser is new Glib.Types.Implements
     (Gtk.Color_Chooser.Gtk_Color_Chooser, Gtk_Color_Chooser_Widget_Record, Gtk_Color_Chooser_Widget);
   function "+"
     (Widget : access Gtk_Color_Chooser_Widget_Record'Class)
   return Gtk.Color_Chooser.Gtk_Color_Chooser
   renames Implements_Gtk_Color_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.Color_Chooser.Gtk_Color_Chooser)
   return Gtk_Color_Chooser_Widget
   renames Implements_Gtk_Color_Chooser.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Color_Chooser_Widget_Record, Gtk_Color_Chooser_Widget);
   function "+"
     (Widget : access Gtk_Color_Chooser_Widget_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Color_Chooser_Widget
   renames Implements_Gtk_Orientable.To_Object;

private
   Show_Editor_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-editor");
end Gtk.Color_Chooser_Widget;
