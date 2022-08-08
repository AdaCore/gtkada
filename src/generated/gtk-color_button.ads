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
--  The Gtk.Color_Button.Gtk_Color_Button is a button which displays the
--  currently selected color and allows to open a color selection dialog to
--  change the color. It is suitable widget for selecting a color in a
--  preference dialog.
--
--  # CSS nodes
--
--  GtkColorButton has a single CSS node with name button. To differentiate it
--  from a plain Gtk.Button.Gtk_Button, it gets the .color style class.
--
--  </description>
--  <group>Buttons and Toggles</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;         use Gdk.Color;
with Gdk.RGBA;          use Gdk.RGBA;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Glib.Variant;      use Glib.Variant;
with Gtk.Action;        use Gtk.Action;
with Gtk.Actionable;    use Gtk.Actionable;
with Gtk.Activatable;   use Gtk.Activatable;
with Gtk.Buildable;     use Gtk.Buildable;
with Gtk.Button;        use Gtk.Button;
with Gtk.Color_Chooser; use Gtk.Color_Chooser;
with Gtk.Enums;         use Gtk.Enums;

package Gtk.Color_Button is

   type Gtk_Color_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Color_Button is access all Gtk_Color_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Button : out Gtk_Color_Button);
   procedure Initialize
      (Button : not null access Gtk_Color_Button_Record'Class);
   --  Creates a new color button.
   --  This returns a widget in the form of a small button containing a swatch
   --  representing the current selected color. When the button is clicked, a
   --  color-selection dialog will open, allowing the user to select a color.
   --  The swatch will be updated to reflect the new color when the user
   --  finishes.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Color_Button_New return Gtk_Color_Button;
   --  Creates a new color button.
   --  This returns a widget in the form of a small button containing a swatch
   --  representing the current selected color. When the button is clicked, a
   --  color-selection dialog will open, allowing the user to select a color.
   --  The swatch will be updated to reflect the new color when the user
   --  finishes.
   --  Since: gtk+ 2.4

   procedure Gtk_New_With_Color
      (Button : out Gtk_Color_Button;
       Color  : Gdk.Color.Gdk_Color);
   procedure Initialize_With_Color
      (Button : not null access Gtk_Color_Button_Record'Class;
       Color  : Gdk.Color.Gdk_Color);
   --  Creates a new color button.
   --  Since: gtk+ 2.4
   --  Initialize_With_Color does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "color": A Gdk.Color.Gdk_Color to set the current color with

   function Gtk_Color_Button_New_With_Color
      (Color : Gdk.Color.Gdk_Color) return Gtk_Color_Button;
   --  Creates a new color button.
   --  Since: gtk+ 2.4
   --  "color": A Gdk.Color.Gdk_Color to set the current color with

   procedure Gtk_New_With_Rgba
      (Button : out Gtk_Color_Button;
       Rgba   : Gdk.RGBA.Gdk_RGBA);
   procedure Initialize_With_Rgba
      (Button : not null access Gtk_Color_Button_Record'Class;
       Rgba   : Gdk.RGBA.Gdk_RGBA);
   --  Creates a new color button.
   --  Since: gtk+ 3.0
   --  Initialize_With_Rgba does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "rgba": A Gdk.RGBA.Gdk_RGBA to set the current color with

   function Gtk_Color_Button_New_With_Rgba
      (Rgba : Gdk.RGBA.Gdk_RGBA) return Gtk_Color_Button;
   --  Creates a new color button.
   --  Since: gtk+ 3.0
   --  "rgba": A Gdk.RGBA.Gdk_RGBA to set the current color with

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Alpha
      (Button : not null access Gtk_Color_Button_Record) return Guint16;
   pragma Obsolescent (Get_Alpha);
   --  Returns the current alpha value.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.4, 1

   procedure Set_Alpha
      (Button : not null access Gtk_Color_Button_Record;
       Alpha  : Guint16);
   pragma Obsolescent (Set_Alpha);
   --  Sets the current opacity to be Alpha.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.4, 1
   --  "alpha": an integer between 0 and 65535

   procedure Get_Color
      (Button : not null access Gtk_Color_Button_Record;
       Color  : out Gdk.Color.Gdk_Color);
   pragma Obsolescent (Get_Color);
   --  Sets Color to be the current color in the
   --  Gtk.Color_Button.Gtk_Color_Button widget.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.4, 1
   --  "color": a Gdk.Color.Gdk_Color to fill in with the current color

   procedure Set_Color
      (Button : not null access Gtk_Color_Button_Record;
       Color  : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Set_Color);
   --  Sets the current color to be Color.
   --  Since: gtk+ 2.4
   --  Deprecated since None, 1
   --  "color": A Gdk.Color.Gdk_Color to set the current color with

   function Get_Title
      (Button : not null access Gtk_Color_Button_Record) return UTF8_String;
   --  Gets the title of the color selection dialog.
   --  Since: gtk+ 2.4

   procedure Set_Title
      (Button : not null access Gtk_Color_Button_Record;
       Title  : UTF8_String);
   --  Sets the title for the color selection dialog.
   --  Since: gtk+ 2.4
   --  "title": String containing new window title

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Color_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Color_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Color_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Color_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Color_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Color_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Color_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Color_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Color_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Color_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Color_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   procedure Add_Palette
      (Self            : not null access Gtk_Color_Button_Record;
       Orientation     : Gtk.Enums.Gtk_Orientation;
       Colors_Per_Line : Glib.Gint;
       N_Colors        : Glib.Gint;
       Colors          : array_of_Gdk_RGBA);

   procedure Get_Rgba
      (Self  : not null access Gtk_Color_Button_Record;
       Color : out Gdk.RGBA.Gdk_RGBA);

   procedure Set_Rgba
      (Self  : not null access Gtk_Color_Button_Record;
       Color : Gdk.RGBA.Gdk_RGBA);

   function Get_Use_Alpha
      (Self : not null access Gtk_Color_Button_Record) return Boolean;

   procedure Set_Use_Alpha
      (Self      : not null access Gtk_Color_Button_Record;
       Use_Alpha : Boolean);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Alpha_Property : constant Glib.Properties.Property_Uint;
   --  The selected opacity value (0 fully transparent, 65535 fully opaque).

   Color_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  The selected color.

   Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The RGBA color.

   Show_Editor_Property : constant Glib.Properties.Property_Boolean;
   --  Set this property to True to skip the palette in the dialog and go
   --  directly to the color editor.
   --
   --  This property should be used in cases where the palette in the editor
   --  would be redundant, such as when the color button is already part of a
   --  palette.

   Title_Property : constant Glib.Properties.Property_String;
   --  The title of the color selection dialog

   Use_Alpha_Property : constant Glib.Properties.Property_Boolean;
   --  If this property is set to True, the color swatch on the button is
   --  rendered against a checkerboard background to show its opacity and the
   --  opacity slider is displayed in the color selection dialog.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Color_Button_Void is not null access procedure
     (Self : access Gtk_Color_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Color_Set : constant Glib.Signal_Name := "color-set";
   procedure On_Color_Set
      (Self  : not null access Gtk_Color_Button_Record;
       Call  : Cb_Gtk_Color_Button_Void;
       After : Boolean := False);
   procedure On_Color_Set
      (Self  : not null access Gtk_Color_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::color-set signal is emitted when the user selects a color. When
   --  handling this signal, use gtk_color_button_get_rgba to find out which
   --  color was just selected.
   --
   --  Note that this signal is only emitted when the user changes the color.
   --  If you need to react to programmatic color changes as well, use the
   --  notify::color signal.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Activatable"
   --
   --  - "Buildable"
   --
   --  - "ColorChooser"

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_Color_Button_Record, Gtk_Color_Button);
   function "+"
     (Widget : access Gtk_Color_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Color_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Color_Button_Record, Gtk_Color_Button);
   function "+"
     (Widget : access Gtk_Color_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Color_Button
   renames Implements_Gtk_Activatable.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Button_Record, Gtk_Color_Button);
   function "+"
     (Widget : access Gtk_Color_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Color_Chooser is new Glib.Types.Implements
     (Gtk.Color_Chooser.Gtk_Color_Chooser, Gtk_Color_Button_Record, Gtk_Color_Button);
   function "+"
     (Widget : access Gtk_Color_Button_Record'Class)
   return Gtk.Color_Chooser.Gtk_Color_Chooser
   renames Implements_Gtk_Color_Chooser.To_Interface;
   function "-"
     (Interf : Gtk.Color_Chooser.Gtk_Color_Chooser)
   return Gtk_Color_Button
   renames Implements_Gtk_Color_Chooser.To_Object;

private
   Use_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-alpha");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Show_Editor_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-editor");
   Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("rgba");
   Color_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("color");
   Alpha_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("alpha");
end Gtk.Color_Button;
