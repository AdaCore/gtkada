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
--  A Gtk_Color_Selection widget is a complex dialog that allows the user to
--  select a color based either on its (Red, Green, Blue) or its (Hue,
--  Saturation, Value). An additional field is provided to select the opacity
--  of the color (this is usually called the alpha channel).
--
--  See Gtk.Color_Selection_Dialog for a version of this widget that comes
--  with its own dialog.
--
--  See Gtk.Extra.Color_Combo for a different way to select colors.
--
--  </description>
--  <screenshot>gtk-colorsel</screenshot>
--  <group>Drawing</group>
--  <testgtk>create_color_selection.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Color;       use Gdk.Color;
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Box;         use Gtk.Box;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;

package Gtk.Color_Selection is

   type Gtk_Color_Selection_Record is new Gtk_Box_Record with null record;
   type Gtk_Color_Selection is access all Gtk_Color_Selection_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Colorsel : out Gtk_Color_Selection);
   procedure Initialize
      (Colorsel : not null access Gtk_Color_Selection_Record'Class);
   --  Creates a new GtkColorSelection.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Color_Selection_New return Gtk_Color_Selection;
   --  Creates a new GtkColorSelection.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_selection_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Current_Alpha
      (Colorsel : not null access Gtk_Color_Selection_Record) return Guint16;
   --  Returns the current alpha value.

   procedure Set_Current_Alpha
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Alpha    : Guint16);
   --  Sets the current opacity to be Alpha.
   --  The first time this is called, it will also set the original opacity to
   --  be Alpha too.
   --  "alpha": an integer between 0 and 65535

   procedure Get_Current_Color
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Color    : out Gdk.Color.Gdk_Color);
   pragma Obsolescent (Get_Current_Color);
   --  Sets Color to be the current color in the GtkColorSelection widget.
   --  Deprecated since 3.4, 1
   --  "color": a Gdk.Color.Gdk_Color to fill in with the current color

   procedure Set_Current_Color
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Color    : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Set_Current_Color);
   --  Sets the current color to be Color.
   --  The first time this is called, it will also set the original color to
   --  be Color too.
   --  Deprecated since 3.4, 1
   --  "color": a Gdk.Color.Gdk_Color to set the current color with

   procedure Get_Current_Rgba
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Rgba     : out Gdk.RGBA.Gdk_RGBA);
   --  Sets Rgba to be the current color in the GtkColorSelection widget.
   --  Since: gtk+ 3.0
   --  "rgba": a Gdk.RGBA.Gdk_RGBA to fill in with the current color

   procedure Set_Current_Rgba
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Rgba     : Gdk.RGBA.Gdk_RGBA);
   --  Sets the current color to be Rgba.
   --  The first time this is called, it will also set the original color to
   --  be Rgba too.
   --  Since: gtk+ 3.0
   --  "rgba": A Gdk.RGBA.Gdk_RGBA to set the current color with

   function Get_Has_Opacity_Control
      (Colorsel : not null access Gtk_Color_Selection_Record) return Boolean;
   --  Determines whether the colorsel has an opacity control.

   procedure Set_Has_Opacity_Control
      (Colorsel    : not null access Gtk_Color_Selection_Record;
       Has_Opacity : Boolean);
   --  Sets the Colorsel to use or not use opacity.
   --  "has_opacity": True if Colorsel can set the opacity, False otherwise

   function Get_Has_Palette
      (Colorsel : not null access Gtk_Color_Selection_Record) return Boolean;
   --  Determines whether the color selector has a color palette.

   procedure Set_Has_Palette
      (Colorsel    : not null access Gtk_Color_Selection_Record;
       Has_Palette : Boolean);
   --  Shows and hides the palette based upon the value of Has_Palette.
   --  "has_palette": True if palette is to be visible, False otherwise

   function Get_Previous_Alpha
      (Colorsel : not null access Gtk_Color_Selection_Record) return Guint16;
   --  Returns the previous alpha value.

   procedure Set_Previous_Alpha
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Alpha    : Guint16);
   --  Sets the "previous" alpha to be Alpha.
   --  This function should be called with some hesitations, as it might seem
   --  confusing to have that alpha change.
   --  "alpha": an integer between 0 and 65535

   procedure Get_Previous_Color
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Color    : out Gdk.Color.Gdk_Color);
   pragma Obsolescent (Get_Previous_Color);
   --  Fills Color in with the original color value.
   --  Deprecated since 3.4, 1
   --  "color": a Gdk.Color.Gdk_Color to fill in with the original color value

   procedure Set_Previous_Color
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Color    : Gdk.Color.Gdk_Color);
   pragma Obsolescent (Set_Previous_Color);
   --  Sets the "previous" color to be Color.
   --  This function should be called with some hesitations, as it might seem
   --  confusing to have that color change. Calling
   --  Gtk.Color_Selection.Set_Current_Color will also set this color the first
   --  time it is called.
   --  Deprecated since 3.4, 1
   --  "color": a Gdk.Color.Gdk_Color to set the previous color with

   procedure Get_Previous_Rgba
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Rgba     : out Gdk.RGBA.Gdk_RGBA);
   --  Fills Rgba in with the original color value.
   --  Since: gtk+ 3.0
   --  "rgba": a Gdk.RGBA.Gdk_RGBA to fill in with the original color value

   procedure Set_Previous_Rgba
      (Colorsel : not null access Gtk_Color_Selection_Record;
       Rgba     : Gdk.RGBA.Gdk_RGBA);
   --  Sets the "previous" color to be Rgba.
   --  This function should be called with some hesitations, as it might seem
   --  confusing to have that color change. Calling
   --  Gtk.Color_Selection.Set_Current_Rgba will also set this color the first
   --  time it is called.
   --  Since: gtk+ 3.0
   --  "rgba": a Gdk.RGBA.Gdk_RGBA to set the previous color with

   function Is_Adjusting
      (Colorsel : not null access Gtk_Color_Selection_Record) return Boolean;
   --  Gets the current state of the Colorsel.

   ----------------------
   -- GtkAda additions --
   ----------------------

   type Gtk_Color_Selection_Change_Palette_With_Screen_Func is access procedure
     (Screen   : System.Address;--  Convert to Gdk_Screen with Get_User_Data
      Colors   : Gdk.Color.Gdk_Color_Unconstrained_Array;
      N_Colors : Gint);
   pragma Convention (C, Gtk_Color_Selection_Change_Palette_With_Screen_Func);
   --  This function should save the new palette contents, and update the
   --  Gtk_Settings property "gtk-color-palette" so all Gtk_Color_Selection
   --  widgets will be modified, including the current one. For instance, you
   --  would do:
   --    Set_String_Property
   --      (Get_Default, Gtk_Color_Palette, Palette_To_String (Colors), "Foo");

   function Palette_From_String
     (Str : String) return Gdk.Color.Gdk_Color_Array;
   --  Parses a color palette string. This string is a colon-separated list of
   --  color names readable by Gdk.Color.Parse.
   --  An empty array is returned if Str couldn't be parsed

   function Palette_To_String
     (Colors   : Gdk.Color.Gdk_Color_Array) return String;
   --  Encodes a palette as a string, useful for persistent storage.

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Color_Selection_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Color_Selection_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ---------------
   -- Functions --
   ---------------

   procedure Set_Change_Palette_With_Screen_Hook
      (Func : Gtk_Color_Selection_Change_Palette_With_Screen_Func);
   --  Installs a global function to be called whenever the user tries to
   --  modify the palette in a color selection.
   --  This function should save the new palette contents, and update the
   --  Gtk.Settings.Gtk_Settings:gtk-color-palette GtkSettings property so all
   --  GtkColorSelection widgets will be modified.
   --  Since: gtk+ 2.2
   --  "func": a function to call when the custom palette needs saving

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Current_Alpha_Property : constant Glib.Properties.Property_Uint;

   Current_Color_Property : constant Gdk.Color.Property_Gdk_Color;
   --  Type: Gdk.Color.Gdk_Color
   --  The current GdkColor color.

   Current_Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The current RGBA color.

   Has_Opacity_Control_Property : constant Glib.Properties.Property_Boolean;

   Has_Palette_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Color_Selection_Void is not null access procedure
     (Self : access Gtk_Color_Selection_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Color_Changed : constant Glib.Signal_Name := "color-changed";
   procedure On_Color_Changed
      (Self  : not null access Gtk_Color_Selection_Record;
       Call  : Cb_Gtk_Color_Selection_Void;
       After : Boolean := False);
   procedure On_Color_Changed
      (Self  : not null access Gtk_Color_Selection_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the color changes in the
   --  Gtk.Color_Selection.Gtk_Color_Selection according to its update policy.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Selection_Record, Gtk_Color_Selection);
   function "+"
     (Widget : access Gtk_Color_Selection_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Selection
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Color_Selection_Record, Gtk_Color_Selection);
   function "+"
     (Widget : access Gtk_Color_Selection_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Color_Selection
   renames Implements_Gtk_Orientable.To_Object;

private
   Has_Palette_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-palette");
   Has_Opacity_Control_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-opacity-control");
   Current_Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("current-rgba");
   Current_Color_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Build ("current-color");
   Current_Alpha_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("current-alpha");
end Gtk.Color_Selection;
