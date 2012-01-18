------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
--  <description>
--  The Gtk.Color_Button.Gtk_Color_Button is a button which displays the
--  currently selected color an allows to open a color selection dialog to
--  change the color. It is suitable widget for selecting a color in a
--  preference dialog.
--
--  </description>
--  <group>Buttons and Toggles</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;       use Gdk.Color;
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Button;      use Gtk.Button;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Color_Button is

   type Gtk_Color_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Color_Button is access all Gtk_Color_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Button : out Gtk_Color_Button);
   procedure Initialize (Button : access Gtk_Color_Button_Record'Class);
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
      (Button : access Gtk_Color_Button_Record'Class;
       Color  : Gdk.Color.Gdk_Color);
   --  Creates a new color button.
   --  Since: gtk+ 2.4
   --  "color": A Gdk_Color to set the current color with

   procedure Gtk_New_With_Rgba
      (Button : out Gtk_Color_Button;
       Rgba   : Gdk.RGBA.Gdk_RGBA);
   procedure Initialize_With_Rgba
      (Button : access Gtk_Color_Button_Record'Class;
       Rgba   : Gdk.RGBA.Gdk_RGBA);
   --  Creates a new color button.
   --  Since: gtk+ 3.0
   --  "rgba": A Gdk_RGBA to set the current color with

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Alpha
      (Button : not null access Gtk_Color_Button_Record) return guint16;
   procedure Set_Alpha
      (Button : not null access Gtk_Color_Button_Record;
       Alpha  : guint16);
   --  Sets the current opacity to be Alpha.
   --  Since: gtk+ 2.4
   --  "alpha": an integer between 0 and 65535

   procedure Get_Color
      (Button : not null access Gtk_Color_Button_Record;
       Color  : out Gdk.Color.Gdk_Color);
   procedure Set_Color
      (Button : not null access Gtk_Color_Button_Record;
       Color  : Gdk.Color.Gdk_Color);
   --  Sets the current color to be Color.
   --  Since: gtk+ 2.4
   --  "color": A Gdk_Color to set the current color with

   procedure Get_Rgba
      (Button : not null access Gtk_Color_Button_Record;
       Rgba   : out Gdk.RGBA.Gdk_RGBA);
   procedure Set_Rgba
      (Button : not null access Gtk_Color_Button_Record;
       Rgba   : Gdk.RGBA.Gdk_RGBA);
   --  Sets the current color to be Rgba.
   --  Since: gtk+ 3.0
   --  "rgba": a Gdk_RGBA to set the current color with

   function Get_Title
      (Button : not null access Gtk_Color_Button_Record) return UTF8_String;
   procedure Set_Title
      (Button : not null access Gtk_Color_Button_Record;
       Title  : UTF8_String);
   --  Sets the title for the color selection dialog.
   --  Since: gtk+ 2.4
   --  "title": String containing new window title

   function Get_Use_Alpha
      (Button : not null access Gtk_Color_Button_Record) return Boolean;
   procedure Set_Use_Alpha
      (Button    : not null access Gtk_Color_Button_Record;
       Use_Alpha : Boolean);
   --  Sets whether or not the color button should use the alpha channel.
   --  Since: gtk+ 2.4
   --  "use_alpha": True if color button should use alpha channel, False if
   --  not

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Color_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Related_Action
      (Self : not null access Gtk_Color_Button_Record)
       return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : not null access Gtk_Color_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Color_Button_Record) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Color_Button_Record;
       Use_Appearance : Boolean);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Color_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Color_Button_Record, Gtk_Color_Button);
   function "+"
     (Widget : access Gtk_Color_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Color_Button
   renames Implements_Activatable.To_Object;

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Button_Record, Gtk_Color_Button);
   function "+"
     (Widget : access Gtk_Color_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Button
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Alpha_Property
   --  Type: Guint
   --  Flags: read-write
   --  The selected opacity value (0 fully transparent, 65535 fully opaque).
   --
   --  Name: Color_Property
   --  Type: Gdk.Color
   --  Flags: read-write
   --  The selected color.
   --
   --  Name: Rgba_Property
   --  Type: Gdk.RGBA
   --  Flags: read-write
   --  The RGBA color.
   --
   --  Name: Title_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The title of the color selection dialog
   --
   --  Name: Use_Alpha_Property
   --  Type: Boolean
   --  Flags: read-write
   --  If this property is set to True, the color swatch on the button is
   --  rendered against a checkerboard background to show its opacity and the
   --  opacity slider is displayed in the color selection dialog.

   Alpha_Property : constant Glib.Properties.Property_Uint;
   Color_Property : constant Glib.Properties.Property_Boxed;
   Rgba_Property : constant Glib.Properties.Property_Boxed;
   Title_Property : constant Glib.Properties.Property_String;
   Use_Alpha_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "color-set"
   --     procedure Handler (Self : access Gtk_Color_Button_Record'Class);
   --  The ::color-set signal is emitted when the user selects a color. When
   --  handling this signal, use Gtk.Color_Button.Get_Color and
   --  Gtk.Color_Button.Get_Alpha (or Gtk.Color_Button.Get_Rgba) to find out
   --  which color was just selected.
   --  Note that this signal is only emitted when the <emphasis>user</emphasis>
   --  changes the color. If you need to react to programmatic color changes as
   --  well, use the notify::color signal.

   Signal_Color_Set : constant Glib.Signal_Name := "color-set";

private
   Alpha_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("alpha");
   Color_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("color");
   Rgba_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("rgba");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Use_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-alpha");
end Gtk.Color_Button;
