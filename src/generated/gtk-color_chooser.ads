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
--  Gtk.Color_Chooser.Gtk_Color_Chooser is an interface that is implemented by
--  widgets for choosing colors. Depending on the situation, colors may be
--  allowed to have alpha (translucency).
--
--  In GTK+, the main widgets that implement this interface are
--  Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget,
--  Gtk.Color_Chooser_Dialog.Gtk_Color_Chooser_Dialog and
--  Gtk.Color_Button.Gtk_Color_Button.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;        use Gdk.RGBA;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Enums;       use Gtk.Enums;

package Gtk.Color_Chooser is

   type Gtk_Color_Chooser is new Glib.Types.GType_Interface;
   Null_Gtk_Color_Chooser : constant Gtk_Color_Chooser;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_chooser_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Palette
      (Self            : Gtk_Color_Chooser;
       Orientation     : Gtk.Enums.Gtk_Orientation;
       Colors_Per_Line : Glib.Gint;
       N_Colors        : Glib.Gint;
       Colors          : array_of_Gdk_RGBA);
   pragma Import (C, Add_Palette, "gtk_color_chooser_add_palette");
   --  Adds a palette to the color chooser. If Orientation is horizontal, the
   --  colors are grouped in rows, with Colors_Per_Line colors in each row. If
   --  Horizontal is False, the colors are grouped in columns instead.
   --  The default color palette of
   --  Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget has 27 colors,
   --  organized in columns of 3 colors. The default gray palette has 9 grays
   --  in a single row.
   --  The layout of the color chooser widget works best when the palettes
   --  have 9-10 columns.
   --  Calling this function for the first time has the side effect of
   --  removing the default color and gray palettes from the color chooser.
   --  If Colors is null, removes all previously added palettes.
   --  Since: gtk+ 3.4
   --  "orientation": Gtk.Enums.Orientation_Horizontal if the palette should
   --  be displayed in rows, Gtk.Enums.Orientation_Vertical for columns
   --  "colors_per_line": the number of colors to show in each row/column
   --  "n_colors": the total number of elements in Colors
   --  "colors": the colors of the palette, or null

   procedure Get_Rgba
      (Self  : Gtk_Color_Chooser;
       Color : out Gdk.RGBA.Gdk_RGBA);
   pragma Import (C, Get_Rgba, "gtk_color_chooser_get_rgba");
   --  Gets the currently-selected color.
   --  Since: gtk+ 3.4
   --  "color": a Gdk.RGBA.Gdk_RGBA to fill in with the current color

   procedure Set_Rgba (Self : Gtk_Color_Chooser; Color : Gdk.RGBA.Gdk_RGBA);
   pragma Import (C, Set_Rgba, "gtk_color_chooser_set_rgba");
   --  Sets the color.
   --  Since: gtk+ 3.4
   --  "color": the new color

   function Get_Use_Alpha (Self : Gtk_Color_Chooser) return Boolean;
   --  Returns whether the color chooser shows the alpha channel.
   --  Since: gtk+ 3.4

   procedure Set_Use_Alpha (Self : Gtk_Color_Chooser; Use_Alpha : Boolean);
   --  Sets whether or not the color chooser should use the alpha channel.
   --  Since: gtk+ 3.4
   --  "use_alpha": True if color chooser should use alpha channel, False if
   --  not

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The ::rgba property contains the currently selected color, as a
   --  Gdk.RGBA.Gdk_RGBA struct. The property can be set to change the current
   --  selection programmatically.

   Use_Alpha_Property : constant Glib.Properties.Property_Boolean;
   --  When ::use-alpha is True, colors may have alpha (translucency)
   --  information. When it is False, the Gdk.RGBA.Gdk_RGBA struct obtained via
   --  the Gtk.Color_Chooser.Gtk_Color_Chooser:rgba property will be forced to
   --  have alpha == 1.
   --
   --  Implementations are expected to show alpha by rendering the color over
   --  a non-uniform background (like a checkerboard pattern).

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Color_Chooser_Gdk_RGBA_Void is not null access procedure
     (Self  : Gtk_Color_Chooser;
      Color : Gdk.RGBA.Gdk_RGBA);

   type Cb_GObject_Gdk_RGBA_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Color : Gdk.RGBA.Gdk_RGBA);

   Signal_Color_Activated : constant Glib.Signal_Name := "color-activated";
   procedure On_Color_Activated
      (Self  : Gtk_Color_Chooser;
       Call  : Cb_Gtk_Color_Chooser_Gdk_RGBA_Void;
       After : Boolean := False);
   procedure On_Color_Activated
      (Self  : Gtk_Color_Chooser;
       Call  : Cb_GObject_Gdk_RGBA_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a color is activated from the color chooser. This usually
   --  happens when the user clicks a color swatch, or a color is selected and
   --  the user presses one of the keys Space, Shift+Space, Return or Enter.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Color_Chooser"

   function "+" (W : Gtk_Color_Chooser) return Gtk_Color_Chooser;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Add_Palette is access procedure
     (Self            : Gtk_Color_Chooser;
      Orientation     : Gtk.Enums.Gtk_Orientation;
      Colors_Per_Line : Glib.Gint;
      N_Colors        : Glib.Gint;
      Colors          : array_of_Gdk_RGBA);
   pragma Convention (C, Virtual_Add_Palette);
   --  Adds a palette to the color chooser. If Orientation is horizontal, the
   --  colors are grouped in rows, with Colors_Per_Line colors in each row. If
   --  Horizontal is False, the colors are grouped in columns instead.
   --  The default color palette of
   --  Gtk.Color_Chooser_Widget.Gtk_Color_Chooser_Widget has 27 colors,
   --  organized in columns of 3 colors. The default gray palette has 9 grays
   --  in a single row.
   --  The layout of the color chooser widget works best when the palettes
   --  have 9-10 columns.
   --  Calling this function for the first time has the side effect of
   --  removing the default color and gray palettes from the color chooser.
   --  If Colors is null, removes all previously added palettes.
   --  Since: gtk+ 3.4
   --  "orientation": Gtk.Enums.Orientation_Horizontal if the palette should
   --  be displayed in rows, Gtk.Enums.Orientation_Vertical for columns
   --  "colors_per_line": the number of colors to show in each row/column
   --  "n_colors": the total number of elements in Colors
   --  "colors": the colors of the palette, or null

   type Virtual_Color_Activated is access procedure (Self : Gtk_Color_Chooser; Color : Gdk.RGBA.Gdk_RGBA);
   pragma Convention (C, Virtual_Color_Activated);

   type Virtual_Get_Rgba is access procedure (Self : Gtk_Color_Chooser; Color : out Gdk.RGBA.Gdk_RGBA);
   pragma Convention (C, Virtual_Get_Rgba);
   --  Gets the currently-selected color.
   --  Since: gtk+ 3.4
   --  "color": a Gdk.RGBA.Gdk_RGBA to fill in with the current color

   type Virtual_Set_Rgba is access procedure (Self : Gtk_Color_Chooser; Color : Gdk.RGBA.Gdk_RGBA);
   pragma Convention (C, Virtual_Set_Rgba);
   --  Sets the color.
   --  Since: gtk+ 3.4
   --  "color": the new color

   subtype Color_Chooser_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Add_Palette
     (Self    : Color_Chooser_Interface_Descr;
      Handler : Virtual_Add_Palette);
   pragma Import (C, Set_Add_Palette, "gtkada_Color_Chooser_set_add_palette");

   procedure Set_Color_Activated
     (Self    : Color_Chooser_Interface_Descr;
      Handler : Virtual_Color_Activated);
   pragma Import (C, Set_Color_Activated, "gtkada_Color_Chooser_set_color_activated");

   procedure Set_Get_Rgba
     (Self    : Color_Chooser_Interface_Descr;
      Handler : Virtual_Get_Rgba);
   pragma Import (C, Set_Get_Rgba, "gtkada_Color_Chooser_set_get_rgba");

   procedure Set_Set_Rgba
     (Self    : Color_Chooser_Interface_Descr;
      Handler : Virtual_Set_Rgba);
   pragma Import (C, Set_Set_Rgba, "gtkada_Color_Chooser_set_set_rgba");
   --  See Glib.Object.Add_Interface

private
   Use_Alpha_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-alpha");
   Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("rgba");

Null_Gtk_Color_Chooser : constant Gtk_Color_Chooser :=
   Gtk_Color_Chooser (Glib.Types.Null_Interface);
end Gtk.Color_Chooser;
