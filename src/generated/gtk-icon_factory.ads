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
--  An icon factory manages a collection of Gtk.Icon_Set.Gtk_Icon_Set; a
--  Gtk.Icon_Set.Gtk_Icon_Set manages a set of variants of a particular icon
--  (i.e. a Gtk.Icon_Set.Gtk_Icon_Set contains variants for different sizes and
--  widget states). Icons in an icon factory are named by a stock ID, which is
--  a simple string identifying the icon. Each Gtk.Style.Gtk_Style has a list
--  of Gtk.Icon_Factory.Gtk_Icon_Factory derived from the current theme; those
--  icon factories are consulted first when searching for an icon. If the theme
--  doesn't set a particular icon, GTK+ looks for the icon in a list of default
--  icon factories, maintained by Gtk.Icon_Factory.Add_Default and
--  Gtk.Icon_Factory.Remove_Default. Applications with icons should add a
--  default icon factory with their icons, which will allow themes to override
--  the icons for the application.
--
--  To display an icon, always use gtk_style_lookup_icon_set on the widget
--  that will display the icon, or the convenience function
--  Gtk.Widget.Render_Icon. These functions take the theme into account when
--  looking up the icon to use for a given stock ID.
--
--  # GtkIconFactory as GtkBuildable #
--  {Gtk.Icon_Factory.Gtk_Icon_Factory-BUILDER-UI}
--
--  GtkIconFactory supports a custom <sources> element, which can contain
--  multiple <source> elements. The following attributes are allowed:
--
--  - stock-id
--
--  The stock id of the source, a string. This attribute is mandatory
--
--  - filename
--
--  The filename of the source, a string. This attribute is optional
--
--  - icon-name
--
--  The icon name for the source, a string. This attribute is optional.
--
--  - size
--
--  Size of the icon, a Gtk.Enums.Gtk_Icon_Size enum value. This attribute is
--  optional.
--
--  - direction
--
--  Direction of the source, a Gtk.Enums.Gtk_Text_Direction enum value. This
--  attribute is optional.
--
--  - state
--
--  State of the source, a Gtk.Enums.Gtk_State_Type enum value. This attribute
--  is optional.
--
--  ## A Gtk.Icon_Factory.Gtk_Icon_Factory UI definition fragment. ##
--
--  |[ <object class="GtkIconFactory" id="iconfactory1"> <sources> <source
--  stock-id="apple-red" filename="apple-red.png"/> </sources> </object>
--  <object class="GtkWindow" id="window1"> <child> <object class="GtkButton"
--  id="apple_button"> <property name="label">apple-red</property> <property
--  name="use-stock">True</property> </object> </child> </object> ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Types;         use Glib.Types;
with Gtk.Buildable;      use Gtk.Buildable;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Icon_Set;       use Gtk.Icon_Set;
with Gtk.Settings;       use Gtk.Settings;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Icon_Factory is

   type Gtk_Icon_Factory_Record is new GObject_Record with null record;
   type Gtk_Icon_Factory is access all Gtk_Icon_Factory_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Icon_Factory);
   procedure Initialize
      (Self : not null access Gtk_Icon_Factory_Record'Class);
   --  Creates a new Gtk.Icon_Factory.Gtk_Icon_Factory. An icon factory
   --  manages a collection of Gtk_Icon_Sets; a Gtk.Icon_Set.Gtk_Icon_Set
   --  manages a set of variants of a particular icon (i.e. a
   --  Gtk.Icon_Set.Gtk_Icon_Set contains variants for different sizes and
   --  widget states). Icons in an icon factory are named by a stock ID, which
   --  is a simple string identifying the icon. Each Gtk.Style.Gtk_Style has a
   --  list of Gtk_Icon_Factorys derived from the current theme; those icon
   --  factories are consulted first when searching for an icon. If the theme
   --  doesn't set a particular icon, GTK+ looks for the icon in a list of
   --  default icon factories, maintained by Gtk.Icon_Factory.Add_Default and
   --  Gtk.Icon_Factory.Remove_Default. Applications with icons should add a
   --  default icon factory with their icons, which will allow themes to
   --  override the icons for the application.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Icon_Factory_New return Gtk_Icon_Factory;
   --  Creates a new Gtk.Icon_Factory.Gtk_Icon_Factory. An icon factory
   --  manages a collection of Gtk_Icon_Sets; a Gtk.Icon_Set.Gtk_Icon_Set
   --  manages a set of variants of a particular icon (i.e. a
   --  Gtk.Icon_Set.Gtk_Icon_Set contains variants for different sizes and
   --  widget states). Icons in an icon factory are named by a stock ID, which
   --  is a simple string identifying the icon. Each Gtk.Style.Gtk_Style has a
   --  list of Gtk_Icon_Factorys derived from the current theme; those icon
   --  factories are consulted first when searching for an icon. If the theme
   --  doesn't set a particular icon, GTK+ looks for the icon in a list of
   --  default icon factories, maintained by Gtk.Icon_Factory.Add_Default and
   --  Gtk.Icon_Factory.Remove_Default. Applications with icons should add a
   --  default icon factory with their icons, which will allow themes to
   --  override the icons for the application.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_factory_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add
      (Self     : not null access Gtk_Icon_Factory_Record;
       Stock_Id : UTF8_String;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set);
   pragma Obsolescent (Add);
   --  Adds the given Icon_Set to the icon factory, under the name Stock_Id.
   --  Stock_Id should be namespaced for your application, e.g.
   --  "myapp-whatever-icon". Normally applications create a
   --  Gtk.Icon_Factory.Gtk_Icon_Factory, then add it to the list of default
   --  factories with Gtk.Icon_Factory.Add_Default. Then they pass the Stock_Id
   --  to widgets such as Gtk.Image.Gtk_Image to display the icon. Themes can
   --  provide an icon with the same name (such as "myapp-whatever-icon") to
   --  override your application's default icons. If an icon already existed in
   --  Factory for Stock_Id, it is unreferenced and replaced with the new
   --  Icon_Set.
   --  Deprecated since 3.10, 1
   --  "stock_id": icon name
   --  "icon_set": icon set

   procedure Add_Default (Self : not null access Gtk_Icon_Factory_Record);
   pragma Obsolescent (Add_Default);
   --  Adds an icon factory to the list of icon factories searched by
   --  gtk_style_lookup_icon_set. This means that, for example,
   --  Gtk.Image.Gtk_New will be able to find icons in Factory. There will
   --  normally be an icon factory added for each library or application that
   --  comes with icons. The default icon factories can be overridden by
   --  themes.
   --  Deprecated since 3.10, 1

   function Lookup
      (Self     : not null access Gtk_Icon_Factory_Record;
       Stock_Id : UTF8_String) return Gtk.Icon_Set.Gtk_Icon_Set;
   pragma Obsolescent (Lookup);
   --  Looks up Stock_Id in the icon factory, returning an icon set if found,
   --  otherwise null. For display to the user, you should use
   --  gtk_style_lookup_icon_set on the Gtk.Style.Gtk_Style for the widget that
   --  will display the icon, instead of using this function directly, so that
   --  themes are taken into account.
   --  Deprecated since 3.10, 1
   --  "stock_id": an icon name

   procedure Remove_Default (Self : not null access Gtk_Icon_Factory_Record);
   pragma Obsolescent (Remove_Default);
   --  Removes an icon factory from the list of default icon factories. Not
   --  normally used; you might use it for a library that can be unloaded or
   --  shut down.
   --  Deprecated since 3.10, 1

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Icon_Factory
     (Self : Gtk.Style_Provider.Gtk_Style_Provider;
      Path : Gtk.Widget.Gtk_Widget_Path)
   return Gtk_Icon_Factory;
   --  Returns the Gtk.Icon_Factory.Gtk_Icon_Factory defined to be in use for
   --  Path, or null if none is defined.
   --  Since: gtk+ 3.0

   ---------------
   -- Functions --
   ---------------

   function Lookup_Default
      (Stock_Id : UTF8_String) return Gtk.Icon_Set.Gtk_Icon_Set;
   pragma Obsolescent (Lookup_Default);
   --  Looks for an icon in the list of default icon factories. For display to
   --  the user, you should use gtk_style_lookup_icon_set on the
   --  Gtk.Style.Gtk_Style for the widget that will display the icon, instead
   --  of using this function directly, so that themes are taken into account.
   --  Deprecated since 3.10, 1
   --  "stock_id": an icon name

   procedure Icon_Size_Lookup
      (Size   : Gtk.Enums.Gtk_Icon_Size;
       Width  : out Glib.Gint;
       Height : out Glib.Gint;
       Result : out Boolean);
   --  Obtains the pixel size of a semantic icon size Size:
   --  GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_BUTTON, etc. This function isn't
   --  normally needed, Gtk.Icon_Theme.Load_Icon is the usual way to get an
   --  icon for rendering, then just look at the size of the rendered pixbuf.
   --  The rendered pixbuf may not even correspond to the width/height returned
   --  by Gtk.Icon_Factory.Icon_Size_Lookup, because themes are free to render
   --  the pixbuf however they like, including changing the usual size.
   --  "size": an icon size (Gtk.Enums.Gtk_Icon_Size)
   --  "width": location to store icon width
   --  "height": location to store icon height

   procedure Icon_Size_Lookup_For_Settings
      (Settings : not null access Gtk.Settings.Gtk_Settings_Record'Class;
       Size     : Gtk.Enums.Gtk_Icon_Size;
       Width    : out Glib.Gint;
       Height   : out Glib.Gint;
       Result   : out Boolean);
   pragma Obsolescent (Icon_Size_Lookup_For_Settings);
   --  Obtains the pixel size of a semantic icon size, possibly modified by
   --  user preferences for a particular Gtk.Settings.Gtk_Settings. Normally
   --  Size would be GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_BUTTON, etc. This
   --  function isn't normally needed, Gtk.Widget.Render_Icon_Pixbuf is the
   --  usual way to get an icon for rendering, then just look at the size of
   --  the rendered pixbuf. The rendered pixbuf may not even correspond to the
   --  width/height returned by Gtk.Icon_Factory.Icon_Size_Lookup, because
   --  themes are free to render the pixbuf however they like, including
   --  changing the usual size.
   --  Since: gtk+ 2.2
   --  Deprecated since 3.10, 1
   --  "settings": a Gtk.Settings.Gtk_Settings object, used to determine which
   --  set of user preferences to used.
   --  "size": an icon size (Gtk.Enums.Gtk_Icon_Size)
   --  "width": location to store icon width
   --  "height": location to store icon height

   function Icon_Size_Register
      (Name   : UTF8_String;
       Width  : Glib.Gint;
       Height : Glib.Gint) return Gtk.Enums.Gtk_Icon_Size;
   pragma Obsolescent (Icon_Size_Register);
   --  Registers a new icon size, along the same lines as GTK_ICON_SIZE_MENU,
   --  etc. Returns the integer value for the size.
   --  Deprecated since 3.10, 1
   --  "name": name of the icon size
   --  "width": the icon width
   --  "height": the icon height

   procedure Icon_Size_Register_Alias
      (Alias  : UTF8_String;
       Target : Gtk.Enums.Gtk_Icon_Size);
   pragma Obsolescent (Icon_Size_Register_Alias);
   --  Registers Alias as another name for Target. So calling
   --  gtk_icon_size_from_name with Alias as argument will return Target.
   --  Deprecated since 3.10, 1
   --  "alias": an alias for Target
   --  "target": an existing icon size (Gtk.Enums.Gtk_Icon_Size)

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Icon_Factory_Record, Gtk_Icon_Factory);
   function "+"
     (Widget : access Gtk_Icon_Factory_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Icon_Factory
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Icon_Factory;
