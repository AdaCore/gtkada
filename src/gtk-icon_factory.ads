-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2004                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  This package provides facilities for defining application-wide icon
--  repositories, that are compatible with the Gtk theme engines.

with Glib.Object;
with Gdk.Pixbuf;

package Gtk.Icon_Factory is

   --------------------
   -- Icon factories --
   --------------------

   --  An icon factory manages a collection of icon sets; an icon set manages
   --  a set of variants of a particular icon (i.e. an icon set contains
   --  variants for different sizes and widget states).
   --
   --  Icons in an icon factory are named by a stock ID, which is a simple
   --  string identifying the icon. Each Gtk_Style has a list of icon factories
   --  derived from the current theme; those icon factories are consulted first
   --  when searching for an icon. If the theme doesn't set a particular icon,
   --  GtkAda looks for the icon in a list of default icon factories,
   --  maintained by Add_Default and Remove_Default (see below).
   --
   --  Applications with icons should add a default icon factory with their
   --  icons, which will allow themes to override the icons for the
   --  application.

   type Icon_Factory_Record is new Glib.Object.GObject_Record with private;
   type Icon_Factory is access all Icon_Factory_Record'Class;

   procedure Gtk_New (Widget : out Icon_Factory);
   --  Create a new Icon_Factory.

   procedure Initialize (Widget : access Icon_Factory_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Icon_Factory.

   ---------------
   -- Icon sets --
   ---------------

   --  An icon set represents a single icon in various sizes and widget states.
   --  It can provide a Gdk_Pixbuf for a given size and state on request, and
   --  automatically caches some of the rendered Gdk_Pixbuf objects.
   --
   --  Normally you would use Gtk.Widget.Render_Icon instead of using icon sets
   --  directly. The one case where you'd use an icon set is to create
   --  application-specific icon sets to place in an icon factory.

   type Icon_Set is new Glib.C_Proxy;

   function Gtk_New return Icon_Set;
   --  Create an empty Icon_Set.

   function Gtk_New (Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf) return Icon_Set;
   --  Creates a new icon set with Pixbuf as the default/fallback source
   --  image. If you don't add any additional icon sources (see below) to the
   --  icon set, all variants of the icon will be created from Pixbuf,
   --  using scaling, pixelation, etc. as required to adjust the icon size
   --  or make the icon look insensitive/prelighted.


   ------------------
   -- Icon sources --
   ------------------

   --  An icon source contains a Gdk_Pixbuf (or image filename) that serves as
   --  the base image for one or more of the icons in an icon set, along with a
   --  specification for which icons in the icon set will be based on that
   --  pixbuf or image file. An icon set contains a set of icons that represent
   --  "the same" logical concept in different states, different global text
   --  directions, and different sizes.
   --
   --  So for example a web browser's "Back to Previous Page" icon might point
   --  in a different direction in Hebrew and in English; it might look
   --  different when insensitive; and it might change size depending on
   --  toolbar mode (small/large icons). So a single icon set would contain all
   --  those variants of the icon. An icon set contains a list of icon sources
   --  from which it can derive specific icon variants in the set.
   --
   --  In the simplest case, an icon set contains one source pixbuf from which
   --  it derives all variants.
   --
   --  If you want to use a different base pixbuf for different icon variants,
   --  you create multiple icon sources, mark which variants they'll be used to
   --  create, and add them to the icon set with Add_Source (see below).

   --  By default, the icon source has all parameters wildcarded. That is, the
   --  icon source will be used as the base icon for any desired text
   --  direction, widget state, or icon size.

   type Icon_Source is new Glib.C_Proxy;

   function Gtk_New return Icon_Source;
   --  Create a new icon source.

   procedure Add
     (Factory  : access Icon_Factory_Record;
      Stock_Id : String;
      Set      : Icon_Set);
   --  Adds the given icon set to the icon factory, under the name Stock_Id.
   --  Stock_Id should be namespaced for your application, e.g.
   --  "myapp-whatever-icon".  Normally applications create an icon factory,
   --  then add it to the list of default factories with Add_Default. Then they
   --  pass the Stock_Id to widgets such as Gtk_Image to display the icon.
   --  Themes can provide an icon with the same name (such as
   --  "myapp-whatever-icon") to override your application's default icons. If
   --  an icon already existed in Factory for Stock_Id, it is unreferenced and
   --  replaced with the new icon set.

   function Lookup
     (Factory  : access Icon_Factory_Record;
      Stock_Id : String)
      return Icon_Set;
   --  Looks up Stock_Id in the icon factory, returning an icon set if found,
   --  otherwise null. For display to the user, you should use
   --  Gtk.Style.Lookup_Icon_Set on the Gtk_Style for the widget that will
   --  display the icon, instead of using this function directly, so that
   --  themes are taken into account.

   procedure Add_Default (Factory : access Icon_Factory_Record);
   --  Adds an icon factory to the list of icon factories searched by
   --  Gtk.Style.Lookup_Icon_Set. This means that, for example,
   --  Gtk.Image.New_From_Stock will be able to find icons in Factory.
   --  There will normally be an icon factory added for each library or
   --  application that comes with icons. The default icon factories
   --  can be overridden by themes.

   procedure Remove_Default (Factory : access Icon_Factory_Record);
   --  Removes an icon factory from the list of default icon
   --  factories. Not normally used; you might use it for a library that
   --  can be unloaded or shut down.

   function Lookup_Default
     (Stock_Id : String)
      return Icon_Set;
   --  Looks for an icon in the list of default icon factories.  For
   --  display to the user, you should use Gtk.Style.Lookup_Icon_Set on
   --  the Gtk_Style for the widget that will display the icon, instead of
   --  using this function directly, so that themes are taken into
   --  account.

   procedure Add_Source
     (Set    : Icon_Set;
      Source : Icon_Source);
   --  Icon sets have a list of icon sources, which they use as base
   --  icons for rendering icons in different states and sizes. Icons are
   --  scaled, made to look insensitive, etc. in Gtk.Icon.Set_Render_Icon,
   --  but an icon set needs base images to work with. The base images and
   --  when to use them are described by an icon source.
   --
   --  This function copies Source, so you can reuse the same source
   --  immediately without affecting the icon set.
   --
   --  An example of when you'd use this function: a web browser's "Back to
   --  Previous Page" icon might point in a different direction in Hebrew and
   --  in English; it might look different when insensitive; and it might
   --  change size depending on toolbar mode (small/large icons). So a single
   --  icon set would contain all those variants of the icon, and you might
   --  add a separate source for each one.

   procedure Free (Source : Icon_Source);
   --  Free memory allocated to Source.

   procedure Set_Filename
     (Source   : Icon_Source;
      Filename : String);
   --  Sets the name of an image file to use as a base image when creating
   --  icon variants for an icon set. The filename must be absolute.

   procedure Set_Pixbuf
     (Source : Icon_Source;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Sets a pixbuf to use as a base image when creating icon variants
   --  for an icon set. If an icon source has both a filename and a pixbuf
   --  set, the pixbuf will take priority.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Icon_Factory_Record is
     new Glib.Object.GObject_Record with null record;

   pragma Import (C, Get_Type, "gtk_icon_factory_get_type");
end Gtk.Icon_Factory;
