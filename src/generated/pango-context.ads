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
--  The Pango.Context.Pango_Context structure stores global information used
--  to control the itemization process.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Pango.Enums;        use Pango.Enums;
with Pango.Font;         use Pango.Font;
with Pango.Font_Family;  use Pango.Font_Family;
with Pango.Font_Metrics; use Pango.Font_Metrics;
with Pango.Fontset;      use Pango.Fontset;
with Pango.Language;     use Pango.Language;
with Pango.Matrix;       use Pango.Matrix;

package Pango.Context is

   type Pango_Context_Record is new GObject_Record with null record;
   type Pango_Context is access all Pango_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New (Self : out Pango_Context);
   --  Creates a new Pango.Context.Pango_Context initialized to default
   --  values.
   --  This function is not particularly useful as it should always be
   --  followed by a pango_context_set_font_map call, and the function
   --  Pango.Font_Map.Create_Context does these two steps together and hence
   --  users are recommended to use that.
   --  If you are using Pango as part of a higher-level system, that system
   --  may have it's own way of create a Pango.Context.Pango_Context. For
   --  instance, the GTK+ toolkit has, among others,
   --  gdk_pango_context_get_for_screen, and Gtk.Widget.Get_Pango_Context. Use
   --  those instead.

   procedure Initialize (Self : not null access Pango_Context_Record'Class);
   --  Creates a new Pango.Context.Pango_Context initialized to default
   --  values.
   --  This function is not particularly useful as it should always be
   --  followed by a pango_context_set_font_map call, and the function
   --  Pango.Font_Map.Create_Context does these two steps together and hence
   --  users are recommended to use that.
   --  If you are using Pango as part of a higher-level system, that system
   --  may have it's own way of create a Pango.Context.Pango_Context. For
   --  instance, the GTK+ toolkit has, among others,
   --  gdk_pango_context_get_for_screen, and Gtk.Widget.Get_Pango_Context. Use
   --  those instead.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Pango_Context_New return Pango_Context;
   --  Creates a new Pango.Context.Pango_Context initialized to default
   --  values.
   --  This function is not particularly useful as it should always be
   --  followed by a pango_context_set_font_map call, and the function
   --  Pango.Font_Map.Create_Context does these two steps together and hence
   --  users are recommended to use that.
   --  If you are using Pango as part of a higher-level system, that system
   --  may have it's own way of create a Pango.Context.Pango_Context. For
   --  instance, the GTK+ toolkit has, among others,
   --  gdk_pango_context_get_for_screen, and Gtk.Widget.Get_Pango_Context. Use
   --  those instead.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_context_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Self : not null access Pango_Context_Record);
   --  Forces a change in the context, which will cause any
   --  Pango.Layout.Pango_Layout using this context to re-layout.
   --  This function is only useful when implementing a new backend for Pango,
   --  something applications won't do. Backends should call this function if
   --  they have attached extra data to the context and such data is changed.
   --  Since: gtk+ 1.32.4

   function Get_Base_Dir
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.Direction;
   --  Retrieves the base direction for the context. See
   --  Pango.Context.Set_Base_Dir.

   procedure Set_Base_Dir
      (Self      : not null access Pango_Context_Record;
       Direction : Pango.Enums.Direction);
   --  Sets the base direction for the context.
   --  The base direction is used in applying the Unicode bidirectional
   --  algorithm; if the Direction is Pango.Enums.Pango_Direction_Ltr or
   --  Pango.Enums.Pango_Direction_Rtl, then the value will be used as the
   --  paragraph direction in the Unicode bidirectional algorithm. A value of
   --  Pango.Enums.Pango_Direction_Weak_Ltr or
   --  Pango.Enums.Pango_Direction_Weak_Rtl is used only for paragraphs that do
   --  not contain any strong characters themselves.
   --  "direction": the new base direction

   function Get_Base_Gravity
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.Gravity;
   --  Retrieves the base gravity for the context. See
   --  Pango.Context.Set_Base_Gravity.
   --  Since: gtk+ 1.16

   procedure Set_Base_Gravity
      (Self    : not null access Pango_Context_Record;
       Gravity : Pango.Enums.Gravity);
   --  Sets the base gravity for the context.
   --  The base gravity is used in laying vertical text out.
   --  Since: gtk+ 1.16
   --  "gravity": the new base gravity

   function Get_Font_Description
      (Self : not null access Pango_Context_Record)
       return Pango.Font.Pango_Font_Description;
   --  Retrieve the default font description for the context.

   procedure Set_Font_Description
      (Self : not null access Pango_Context_Record;
       Desc : Pango.Font.Pango_Font_Description);
   --  Set the default font description for the context
   --  "desc": the new pango font description

   function Get_Gravity
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.Gravity;
   --  Retrieves the gravity for the context. This is similar to
   --  Pango.Context.Get_Base_Gravity, except for when the base gravity is
   --  Pango.Enums.Pango_Gravity_Auto for which pango_gravity_get_for_matrix is
   --  used to return the gravity from the current context matrix.
   --  Since: gtk+ 1.16

   function Get_Gravity_Hint
      (Self : not null access Pango_Context_Record)
       return Pango.Enums.GravityHint;
   --  Retrieves the gravity hint for the context. See
   --  Pango.Context.Set_Gravity_Hint for details.
   --  Since: gtk+ 1.16

   procedure Set_Gravity_Hint
      (Self : not null access Pango_Context_Record;
       Hint : Pango.Enums.GravityHint);
   --  Sets the gravity hint for the context.
   --  The gravity hint is used in laying vertical text out, and is only
   --  relevant if gravity of the context as returned by
   --  Pango.Context.Get_Gravity is set Pango.Enums.Pango_Gravity_East or
   --  Pango.Enums.Pango_Gravity_West.
   --  Since: gtk+ 1.16
   --  "hint": the new gravity hint

   function Get_Language
      (Self : not null access Pango_Context_Record)
       return Pango.Language.Pango_Language;
   --  Retrieves the global language tag for the context.

   procedure Set_Language
      (Self     : not null access Pango_Context_Record;
       Language : Pango.Language.Pango_Language);
   --  Sets the global language tag for the context. The default language for
   --  the locale of the running process can be found using
   --  Pango.Language.Get_Default.
   --  "language": the new language tag.

   function Get_Matrix
      (Self : not null access Pango_Context_Record)
       return Pango.Matrix.Pango_Matrix;
   --  Gets the transformation matrix that will be applied when rendering with
   --  this context. See Pango.Context.Set_Matrix.
   --  Since: gtk+ 1.6

   procedure Set_Matrix
      (Self   : not null access Pango_Context_Record;
       Matrix : Pango.Matrix.Pango_Matrix);
   --  Sets the transformation matrix that will be applied when rendering with
   --  this context. Note that reported metrics are in the user space
   --  coordinates before the application of the matrix, not device-space
   --  coordinates after the application of the matrix. So, they don't scale
   --  with the matrix, though they may change slightly for different matrices,
   --  depending on how the text is fit to the pixel grid.
   --  Since: gtk+ 1.6
   --  "matrix": a Pango.Matrix.Pango_Matrix, or null to unset any existing
   --  matrix. (No matrix set is the same as setting the identity matrix.)

   function Get_Metrics
      (Self     : not null access Pango_Context_Record;
       Desc     : Pango.Font.Pango_Font_Description;
       Language : Pango.Language.Pango_Language)
       return Pango.Font_Metrics.Pango_Font_Metrics;
   --  Get overall metric information for a particular font description. Since
   --  the metrics may be substantially different for different scripts, a
   --  language tag can be provided to indicate that the metrics should be
   --  retrieved that correspond to the script(s) used by that language.
   --  The Pango.Font.Pango_Font_Description is interpreted in the same way as
   --  by pango_itemize, and the family name may be a comma separated list of
   --  figures. If characters from multiple of these families would be used to
   --  render the string, then the returned fonts would be a composite of the
   --  metrics for the fonts loaded for the individual families.
   --  "desc": a Pango.Font.Pango_Font_Description structure. null means that
   --  the font description from the context will be used.
   --  "language": language tag used to determine which script to get the
   --  metrics for. null means that the language tag from the context will be
   --  used. If no language tag is set on the context, metrics for the default
   --  language (as determined by Pango.Language.Get_Default) will be returned.

   function Get_Round_Glyph_Positions
      (Self : not null access Pango_Context_Record) return Boolean;
   --  Returns whether font rendering with this context should round glyph
   --  positions and widths.
   --  Since: gtk+ 1.44

   procedure Set_Round_Glyph_Positions
      (Self            : not null access Pango_Context_Record;
       Round_Positions : Boolean);
   --  Sets whether font rendering with this context should round glyph
   --  positions and widths to integral positions, in device units.
   --  This is useful when the renderer can't handle subpixel positioning of
   --  glyphs.
   --  The default value is to round glyph positions, to remain compatible
   --  with previous Pango behavior.
   --  Since: gtk+ 1.44
   --  "round_positions": whether to round glyph positions

   function Get_Serial
      (Self : not null access Pango_Context_Record) return Guint;
   --  Returns the current serial number of Context. The serial number is
   --  initialized to an small number larger than zero when a new context is
   --  created and is increased whenever the context is changed using any of
   --  the setter functions, or the Pango.Font_Map.Pango_Font_Map it uses to
   --  find fonts has changed. The serial may wrap, but will never have the
   --  value 0. Since it can wrap, never compare it with "less than", always
   --  use "not equals".
   --  This can be used to automatically detect changes to a
   --  Pango.Context.Pango_Context, and is only useful when implementing
   --  objects that need update when their Pango.Context.Pango_Context changes,
   --  like Pango.Layout.Pango_Layout.
   --  Since: gtk+ 1.32.4

   function List_Families
      (Self : not null access Pango_Context_Record)
       return Pango_Font_Family_Array;
   --  List all families for a context.

   function Load_Font
      (Self : not null access Pango_Context_Record;
       Desc : Pango.Font.Pango_Font_Description)
       return Pango.Font.Pango_Font;
   --  Loads the font in one of the fontmaps in the context that is the
   --  closest match for Desc.
   --  "desc": a Pango.Font.Pango_Font_Description describing the font to load

   function Load_Fontset
      (Self     : not null access Pango_Context_Record;
       Desc     : Pango.Font.Pango_Font_Description;
       Language : Pango.Language.Pango_Language)
       return Pango.Fontset.Pango_Fontset;
   --  Load a set of fonts in the context that can be used to render a font
   --  matching Desc.
   --  "desc": a Pango.Font.Pango_Font_Description describing the fonts to
   --  load
   --  "language": a Pango.Language.Pango_Language the fonts will be used for

end Pango.Context;
