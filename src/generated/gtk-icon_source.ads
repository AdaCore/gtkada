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
--  An icon source contains a Gdk_Pixbuf (or image filename) that serves as
--  the base image for one or more of the icons in an icon set, along with a
--  specification for which icons in the icon set will be based on that pixbuf
--  or image file. An icon set contains a set of icons that represent "the
--  same" logical concept in different states, different global text
--  directions, and different sizes.
--
--  So for example a web browser's "Back to Previous Page" icon might point in
--  a different direction in Hebrew and in English; it might look different
--  when insensitive; and it might change size depending on toolbar mode
--  (small/large icons). So a single icon set would contain all those variants
--  of the icon. An icon set contains a list of icon sources from which it can
--  derive specific icon variants in the set.
--
--  In the simplest case, an icon set contains one source pixbuf from which it
--  derives all variants.
--
--  If you want to use a different base pixbuf for different icon variants,
--  you create multiple icon sources, mark which variants they'll be used to
--  create, and add them to the icon set with Add_Source (see below).
--
--  By default, the icon source has all parameters wildcarded. That is, the
--  icon source will be used as the base icon for any desired text direction,
--  widget state, or icon size.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Glib;       use Glib;
with Gtk.Enums;  use Gtk.Enums;

package Gtk.Icon_Source is

   type Gtk_Icon_Source is new Glib.C_Boxed with null record;
   Null_Gtk_Icon_Source : constant Gtk_Icon_Source;

   function From_Object (Object : System.Address) return Gtk_Icon_Source;
   function From_Object_Free (B : access Gtk_Icon_Source'Class) return Gtk_Icon_Source;
   pragma Inline (From_Object_Free, From_Object);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Icon_Source);
   --  Creates a new Gtk.Icon_Source.Gtk_Icon_Source. A
   --  Gtk.Icon_Source.Gtk_Icon_Source contains a Gdk.Pixbuf.Gdk_Pixbuf (or
   --  image filename) that serves as the base image for one or more of the
   --  icons in a Gtk.Icon_Set.Gtk_Icon_Set, along with a specification for
   --  which icons in the icon set will be based on that pixbuf or image file.
   --  An icon set contains a set of icons that represent "the same" logical
   --  concept in different states, different global text directions, and
   --  different sizes.
   --  So for example a web browser's "Back to Previous Page" icon might point
   --  in a different direction in Hebrew and in English; it might look
   --  different when insensitive; and it might change size depending on
   --  toolbar mode (small/large icons). So a single icon set would contain all
   --  those variants of the icon. Gtk.Icon_Set.Gtk_Icon_Set contains a list of
   --  Gtk.Icon_Source.Gtk_Icon_Source from which it can derive specific icon
   --  variants in the set.
   --  In the simplest case, Gtk.Icon_Set.Gtk_Icon_Set contains one source
   --  pixbuf from which it derives all variants. The convenience function
   --  gtk_icon_set_new_from_pixbuf handles this case; if you only have one
   --  source pixbuf, just use that function.
   --  If you want to use a different base pixbuf for different icon variants,
   --  you create multiple icon sources, mark which variants they'll be used to
   --  create, and add them to the icon set with Gtk.Icon_Set.Add_Source.
   --  By default, the icon source has all parameters wildcarded. That is, the
   --  icon source will be used as the base icon for any desired text
   --  direction, widget state, or icon size.

   function Gtk_Icon_Source_New return Gtk_Icon_Source;
   --  Creates a new Gtk.Icon_Source.Gtk_Icon_Source. A
   --  Gtk.Icon_Source.Gtk_Icon_Source contains a Gdk.Pixbuf.Gdk_Pixbuf (or
   --  image filename) that serves as the base image for one or more of the
   --  icons in a Gtk.Icon_Set.Gtk_Icon_Set, along with a specification for
   --  which icons in the icon set will be based on that pixbuf or image file.
   --  An icon set contains a set of icons that represent "the same" logical
   --  concept in different states, different global text directions, and
   --  different sizes.
   --  So for example a web browser's "Back to Previous Page" icon might point
   --  in a different direction in Hebrew and in English; it might look
   --  different when insensitive; and it might change size depending on
   --  toolbar mode (small/large icons). So a single icon set would contain all
   --  those variants of the icon. Gtk.Icon_Set.Gtk_Icon_Set contains a list of
   --  Gtk.Icon_Source.Gtk_Icon_Source from which it can derive specific icon
   --  variants in the set.
   --  In the simplest case, Gtk.Icon_Set.Gtk_Icon_Set contains one source
   --  pixbuf from which it derives all variants. The convenience function
   --  gtk_icon_set_new_from_pixbuf handles this case; if you only have one
   --  source pixbuf, just use that function.
   --  If you want to use a different base pixbuf for different icon variants,
   --  you create multiple icon sources, mark which variants they'll be used to
   --  create, and add them to the icon set with Gtk.Icon_Set.Add_Source.
   --  By default, the icon source has all parameters wildcarded. That is, the
   --  icon source will be used as the base icon for any desired text
   --  direction, widget state, or icon size.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_source_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Gtk_Icon_Source) return Gtk_Icon_Source;
   pragma Obsolescent (Copy);
   --  Creates a copy of Source; mostly useful for language bindings.
   --  Deprecated since 3.10, 1

   procedure Free (Self : Gtk_Icon_Source);
   pragma Obsolescent (Free);
   --  Frees a dynamically-allocated icon source, along with its filename,
   --  size, and pixbuf fields if those are not null.
   --  Deprecated since 3.10, 1

   function Get_Direction
      (Self : Gtk_Icon_Source) return Gtk.Enums.Gtk_Text_Direction;
   pragma Obsolescent (Get_Direction);
   --  Obtains the text direction this icon source applies to. The return
   --  value is only useful/meaningful if the text direction is not wildcarded.
   --  Deprecated since 3.10, 1

   procedure Set_Direction
      (Self      : Gtk_Icon_Source;
       Direction : Gtk.Enums.Gtk_Text_Direction);
   pragma Obsolescent (Set_Direction);
   --  Sets the text direction this icon source is intended to be used with.
   --  Setting the text direction on an icon source makes no difference if the
   --  text direction is wildcarded. Therefore, you should usually call
   --  Gtk.Icon_Source.Set_Direction_Wildcarded to un-wildcard it in addition
   --  to calling this function.
   --  Deprecated since 3.10, 1
   --  "direction": text direction this source applies to

   function Get_Direction_Wildcarded (Self : Gtk_Icon_Source) return Boolean;
   pragma Obsolescent (Get_Direction_Wildcarded);
   --  Gets the value set by Gtk.Icon_Source.Set_Direction_Wildcarded.
   --  Deprecated since 3.10, 1

   procedure Set_Direction_Wildcarded
      (Self    : Gtk_Icon_Source;
       Setting : Boolean);
   pragma Obsolescent (Set_Direction_Wildcarded);
   --  If the text direction is wildcarded, this source can be used as the
   --  base image for an icon in any Gtk.Enums.Gtk_Text_Direction. If the text
   --  direction is not wildcarded, then the text direction the icon source
   --  applies to should be set with Gtk.Icon_Source.Set_Direction, and the
   --  icon source will only be used with that text direction.
   --  Gtk.Icon_Set.Gtk_Icon_Set prefers non-wildcarded sources (exact
   --  matches) over wildcarded sources, and will use an exact match when
   --  possible.
   --  Deprecated since 3.10, 1
   --  "setting": True to wildcard the text direction

   function Get_Filename (Self : Gtk_Icon_Source) return UTF8_String;
   pragma Obsolescent (Get_Filename);
   --  Retrieves the source filename, or null if none is set. The filename is
   --  not a copy, and should not be modified or expected to persist beyond the
   --  lifetime of the icon source.
   --  Deprecated since 3.10, 1

   procedure Set_Filename (Self : Gtk_Icon_Source; Filename : UTF8_String);
   pragma Obsolescent (Set_Filename);
   --  Sets the name of an image file to use as a base image when creating
   --  icon variants for Gtk.Icon_Set.Gtk_Icon_Set. The filename must be
   --  absolute.
   --  Deprecated since 3.10, 1
   --  "filename": image file to use

   function Get_Icon_Name (Self : Gtk_Icon_Source) return UTF8_String;
   pragma Obsolescent (Get_Icon_Name);
   --  Retrieves the source icon name, or null if none is set. The icon_name
   --  is not a copy, and should not be modified or expected to persist beyond
   --  the lifetime of the icon source.
   --  Deprecated since 3.10, 1

   procedure Set_Icon_Name
      (Self      : Gtk_Icon_Source;
       Icon_Name : UTF8_String := "");
   pragma Obsolescent (Set_Icon_Name);
   --  Sets the name of an icon to look up in the current icon theme to use as
   --  a base image when creating icon variants for Gtk.Icon_Set.Gtk_Icon_Set.
   --  Deprecated since 3.10, 1
   --  "icon_name": name of icon to use

   function Get_Pixbuf (Self : Gtk_Icon_Source) return Gdk.Pixbuf.Gdk_Pixbuf;
   pragma Obsolescent (Get_Pixbuf);
   --  Retrieves the source pixbuf, or null if none is set. In addition, if a
   --  filename source is in use, this function in some cases will return the
   --  pixbuf from loaded from the filename. This is, for example, true for the
   --  GtkIconSource passed to the Gtk.Style.Gtk_Style render_icon virtual
   --  function. The reference count on the pixbuf is not incremented.
   --  Deprecated since 3.10, 1

   procedure Set_Pixbuf
      (Self   : Gtk_Icon_Source;
       Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   pragma Obsolescent (Set_Pixbuf);
   --  Sets a pixbuf to use as a base image when creating icon variants for
   --  Gtk.Icon_Set.Gtk_Icon_Set.
   --  Deprecated since 3.10, 1
   --  "pixbuf": pixbuf to use as a source

   function Get_Size (Self : Gtk_Icon_Source) return Gtk.Enums.Gtk_Icon_Size;
   pragma Obsolescent (Get_Size);
   --  Obtains the icon size this source applies to. The return value is only
   --  useful/meaningful if the icon size is not wildcarded.
   --  Deprecated since 3.10, 1

   procedure Set_Size
      (Self : Gtk_Icon_Source;
       Size : Gtk.Enums.Gtk_Icon_Size);
   pragma Obsolescent (Set_Size);
   --  Sets the icon size this icon source is intended to be used with.
   --  Setting the icon size on an icon source makes no difference if the size
   --  is wildcarded. Therefore, you should usually call
   --  Gtk.Icon_Source.Set_Size_Wildcarded to un-wildcard it in addition to
   --  calling this function.
   --  Deprecated since 3.10, 1
   --  "size": icon size (Gtk.Enums.Gtk_Icon_Size) this source applies to

   function Get_Size_Wildcarded (Self : Gtk_Icon_Source) return Boolean;
   pragma Obsolescent (Get_Size_Wildcarded);
   --  Gets the value set by Gtk.Icon_Source.Set_Size_Wildcarded.
   --  Deprecated since 3.10, 1

   procedure Set_Size_Wildcarded (Self : Gtk_Icon_Source; Setting : Boolean);
   pragma Obsolescent (Set_Size_Wildcarded);
   --  If the icon size is wildcarded, this source can be used as the base
   --  image for an icon of any size. If the size is not wildcarded, then the
   --  size the source applies to should be set with Gtk.Icon_Source.Set_Size
   --  and the icon source will only be used with that specific size.
   --  Gtk.Icon_Set.Gtk_Icon_Set prefers non-wildcarded sources (exact
   --  matches) over wildcarded sources, and will use an exact match when
   --  possible.
   --  Gtk.Icon_Set.Gtk_Icon_Set will normally scale wildcarded source images
   --  to produce an appropriate icon at a given size, but will not change the
   --  size of source images that match exactly.
   --  Deprecated since 3.10, 1
   --  "setting": True to wildcard the widget state

   function Get_State
      (Self : Gtk_Icon_Source) return Gtk.Enums.Gtk_State_Type;
   pragma Obsolescent (Get_State);
   --  Obtains the widget state this icon source applies to. The return value
   --  is only useful/meaningful if the widget state is not wildcarded.
   --  Deprecated since 3.10, 1

   procedure Set_State
      (Self  : Gtk_Icon_Source;
       State : Gtk.Enums.Gtk_State_Type);
   pragma Obsolescent (Set_State);
   --  Sets the widget state this icon source is intended to be used with.
   --  Setting the widget state on an icon source makes no difference if the
   --  state is wildcarded. Therefore, you should usually call
   --  Gtk.Icon_Source.Set_State_Wildcarded to un-wildcard it in addition to
   --  calling this function.
   --  Deprecated since 3.10, 1
   --  "state": widget state this source applies to

   function Get_State_Wildcarded (Self : Gtk_Icon_Source) return Boolean;
   pragma Obsolescent (Get_State_Wildcarded);
   --  Gets the value set by Gtk.Icon_Source.Set_State_Wildcarded.
   --  Deprecated since 3.10, 1

   procedure Set_State_Wildcarded
      (Self    : Gtk_Icon_Source;
       Setting : Boolean);
   pragma Obsolescent (Set_State_Wildcarded);
   --  If the widget state is wildcarded, this source can be used as the base
   --  image for an icon in any Gtk.Enums.Gtk_State_Type. If the widget state
   --  is not wildcarded, then the state the source applies to should be set
   --  with Gtk.Icon_Source.Set_State and the icon source will only be used
   --  with that specific state.
   --  Gtk.Icon_Set.Gtk_Icon_Set prefers non-wildcarded sources (exact
   --  matches) over wildcarded sources, and will use an exact match when
   --  possible.
   --  Gtk.Icon_Set.Gtk_Icon_Set will normally transform wildcarded source
   --  images to produce an appropriate icon for a given state, for example
   --  lightening an image on prelight, but will not modify source images that
   --  match exactly.
   --  Deprecated since 3.10, 1
   --  "setting": True to wildcard the widget state

private

   Null_Gtk_Icon_Source : constant Gtk_Icon_Source := (Glib.C_Boxed with null record);

end Gtk.Icon_Source;
