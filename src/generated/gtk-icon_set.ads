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
--  An icon set represents a single icon in various sizes and widget states.
--  It can provide a Gdk_Pixbuf for a given size and state on request, and
--  automatically caches some of the rendered Gdk_Pixbuf objects.
--
--  Normally you would use Gtk.Widget.Render_Icon instead of using icon sets
--  directly. The one case where you'd use an icon set is to create
--  application-specific icon sets to place in an icon factory.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;             use Cairo;
with Gdk;               use Gdk;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Glib;              use Glib;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Icon_Source;   use Gtk.Icon_Source;
with Gtk.Style;         use Gtk.Style;
with Gtk.Style_Context; use Gtk.Style_Context;
with Gtk.Widget;        use Gtk.Widget;

package Gtk.Icon_Set is

   type Gtk_Icon_Set is new Glib.C_Boxed with null record;
   Null_Gtk_Icon_Set : constant Gtk_Icon_Set;

   function From_Object (Object : System.Address) return Gtk_Icon_Set;
   function From_Object_Free (B : access Gtk_Icon_Set'Class) return Gtk_Icon_Set;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Icon_Set);
   --  Creates a new Gtk.Icon_Set.Gtk_Icon_Set. A Gtk.Icon_Set.Gtk_Icon_Set
   --  represents a single icon in various sizes and widget states. It can
   --  provide a Gdk.Pixbuf.Gdk_Pixbuf for a given size and state on request,
   --  and automatically caches some of the rendered Gdk.Pixbuf.Gdk_Pixbuf
   --  objects.
   --  Normally you would use Gtk.Widget.Render_Icon_Pixbuf instead of using
   --  Gtk.Icon_Set.Gtk_Icon_Set directly. The one case where you'd use
   --  Gtk.Icon_Set.Gtk_Icon_Set is to create application-specific icon sets to
   --  place in a Gtk.Icon_Factory.Gtk_Icon_Factory.

   function Gtk_Icon_Set_New return Gtk_Icon_Set;
   --  Creates a new Gtk.Icon_Set.Gtk_Icon_Set. A Gtk.Icon_Set.Gtk_Icon_Set
   --  represents a single icon in various sizes and widget states. It can
   --  provide a Gdk.Pixbuf.Gdk_Pixbuf for a given size and state on request,
   --  and automatically caches some of the rendered Gdk.Pixbuf.Gdk_Pixbuf
   --  objects.
   --  Normally you would use Gtk.Widget.Render_Icon_Pixbuf instead of using
   --  Gtk.Icon_Set.Gtk_Icon_Set directly. The one case where you'd use
   --  Gtk.Icon_Set.Gtk_Icon_Set is to create application-specific icon sets to
   --  place in a Gtk.Icon_Factory.Gtk_Icon_Factory.

   procedure Gtk_New_From_Pixbuf
      (Self   : out Gtk_Icon_Set;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a new Gtk.Icon_Set.Gtk_Icon_Set with Pixbuf as the
   --  default/fallback source image. If you don't add any additional
   --  Gtk.Icon_Source.Gtk_Icon_Source to the icon set, all variants of the
   --  icon will be created from Pixbuf, using scaling, pixelation, etc. as
   --  required to adjust the icon size or make the icon look
   --  insensitive/prelighted.
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf

   function Gtk_Icon_Set_New_From_Pixbuf
      (Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Icon_Set;
   --  Creates a new Gtk.Icon_Set.Gtk_Icon_Set with Pixbuf as the
   --  default/fallback source image. If you don't add any additional
   --  Gtk.Icon_Source.Gtk_Icon_Source to the icon set, all variants of the
   --  icon will be created from Pixbuf, using scaling, pixelation, etc. as
   --  required to adjust the icon size or make the icon look
   --  insensitive/prelighted.
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_set_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Source
      (Self   : Gtk_Icon_Set;
       Source : Gtk.Icon_Source.Gtk_Icon_Source);
   pragma Obsolescent (Add_Source);
   --  Icon sets have a list of Gtk.Icon_Source.Gtk_Icon_Source, which they
   --  use as base icons for rendering icons in different states and sizes.
   --  Icons are scaled, made to look insensitive, etc. in
   --  Gtk.Icon_Set.Render_Icon, but Gtk.Icon_Set.Gtk_Icon_Set needs base
   --  images to work with. The base images and when to use them are described
   --  by a Gtk.Icon_Source.Gtk_Icon_Source.
   --  This function copies Source, so you can reuse the same source
   --  immediately without affecting the icon set.
   --  An example of when you'd use this function: a web browser's "Back to
   --  Previous Page" icon might point in a different direction in Hebrew and
   --  in English; it might look different when insensitive; and it might
   --  change size depending on toolbar mode (small/large icons). So a single
   --  icon set would contain all those variants of the icon, and you might add
   --  a separate source for each one.
   --  You should nearly always add a "default" icon source with all fields
   --  wildcarded, which will be used as a fallback if no more specific source
   --  matches. Gtk.Icon_Set.Gtk_Icon_Set always prefers more specific icon
   --  sources to more generic icon sources. The order in which you add the
   --  sources to the icon set does not matter.
   --  gtk_icon_set_new_from_pixbuf creates a new icon set with a default icon
   --  source based on the given pixbuf.
   --  Deprecated since 3.10, 1
   --  "source": a Gtk.Icon_Source.Gtk_Icon_Source

   function Copy (Self : Gtk_Icon_Set) return Gtk_Icon_Set;
   pragma Obsolescent (Copy);
   --  Copies Icon_Set by value.
   --  Deprecated since 3.10, 1

   function Get_Sizes (Self : Gtk_Icon_Set) return Gint_Array;
   pragma Obsolescent (Get_Sizes);
   --  Obtains a list of icon sizes this icon set can render. The returned
   --  array must be freed with g_free.
   --  Deprecated since 3.10, 1

   function Ref (Self : Gtk_Icon_Set) return Gtk_Icon_Set;
   pragma Obsolescent (Ref);
   --  Increments the reference count on Icon_Set.
   --  Deprecated since 3.10, 1

   function Render_Icon
      (Self      : Gtk_Icon_Set;
       Style     : access Gtk.Style.Gtk_Style_Record'Class;
       Direction : Gtk.Enums.Gtk_Text_Direction;
       State     : Gtk.Enums.Gtk_State_Type;
       Size      : Gtk.Enums.Gtk_Icon_Size;
       Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
       Detail    : UTF8_String := "") return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Render_Icon);
   --  Renders an icon using gtk_style_render_icon. In most cases,
   --  Gtk.Widget.Render_Icon is better, since it automatically provides most
   --  of the arguments from the current widget settings. This function never
   --  returns null; if the icon can't be rendered (perhaps because an image
   --  file fails to load), a default "missing image" icon will be returned
   --  instead.
   --  Deprecated since 3.0, 1
   --  "style": a Gtk.Style.Gtk_Style associated with Widget, or null
   --  "direction": text direction
   --  "state": widget state
   --  "size": icon size (Gtk.Enums.Gtk_Icon_Size). A size of
   --  `(GtkIconSize)-1` means render at the size of the source and don't
   --  scale.
   --  "widget": widget that will display the icon, or null. The only use that
   --  is typically made of this is to determine the appropriate
   --  Gdk.Screen.Gdk_Screen.
   --  "detail": detail to pass to the theme engine, or null. Note that
   --  passing a detail of anything but null will disable caching.

   function Render_Icon_Pixbuf
      (Self    : Gtk_Icon_Set;
       Context : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Size    : Gtk.Enums.Gtk_Icon_Size) return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Render_Icon_Pixbuf);
   --  Renders an icon using gtk_render_icon_pixbuf. In most cases,
   --  Gtk.Widget.Render_Icon_Pixbuf is better, since it automatically provides
   --  most of the arguments from the current widget settings. This function
   --  never returns null; if the icon can't be rendered (perhaps because an
   --  image file fails to load), a default "missing image" icon will be
   --  returned instead.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.10, 1
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "size": icon size (Gtk.Enums.Gtk_Icon_Size). A size of
   --  `(GtkIconSize)-1` means render at the size of the source and don't
   --  scale.

   function Render_Icon_Surface
      (Self       : Gtk_Icon_Set;
       Context    : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Size       : Gtk.Enums.Gtk_Icon_Size;
       Scale      : Glib.Gint;
       For_Window : Gdk.Gdk_Window) return Cairo.Cairo_Surface;
   pragma Obsolescent (Render_Icon_Surface);
   --  Renders an icon using gtk_render_icon_pixbuf and converts it to a cairo
   --  surface.
   --  This function never returns null; if the icon can't be rendered
   --  (perhaps because an image file fails to load), a default "missing image"
   --  icon will be returned instead.
   --  Since: gtk+ 3.10
   --  Deprecated since 3.10, 1
   --  "context": a Gtk.Style_Context.Gtk_Style_Context
   --  "size": icon size (Gtk.Enums.Gtk_Icon_Size). A size of
   --  `(GtkIconSize)-1` means render at the size of the source and don't
   --  scale.
   --  "scale": the window scale to render for
   --  "for_window": Gdk.Gdk_Window to optimize drawing for, or null

   procedure Unref (Self : Gtk_Icon_Set);
   pragma Obsolescent (Unref);
   --  Decrements the reference count on Icon_Set, and frees memory if the
   --  reference count reaches 0.
   --  Deprecated since 3.10, 1

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Lookup_Icon_Set
     (Style    : access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
      Stock_Id : String)
   return Gtk_Icon_Set;
   --  Retrieve an icon set by its name. The icon might exist in various sizes,
   --  that can be manipulated through the result set

private

   Null_Gtk_Icon_Set : constant Gtk_Icon_Set := (Glib.C_Boxed with null record);

end Gtk.Icon_Set;
