-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

--  <description>
--  The Gtk_Image widget displays a graphical image. The image is typically
--  created using Gdk.Image.Gdk_New.
--
--  The pixels in a Gtk_Image may be manipulated by the application after
--  creation, as Gtk_Image store the pixel data on the client side. If you wish
--  to store the pixel data on the server side (thus not allowing manipulation
--  of the data after creation) you should use Gtk_Pixmap.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Display widgets</group>
--  <screenshot>gtk-image</screenshot>

with Glib.Properties;
with Gdk.Bitmap;
with Gdk.Pixbuf;
with Gdk.Pixmap;
with Gdk.Image;
with Gtk.Enums;
with Gtk.Icon_Factory;
with Gtk.Misc;
with GNAT.Strings;
with Glib.Generic_Properties;

package Gtk.Image is

   type Gtk_Image_Record is new Gtk.Misc.Gtk_Misc_Record with private;
   type Gtk_Image is access all Gtk_Image_Record'Class;

   type Gtk_Image_Type is
     (Image_Empty,
      Image_Pixmap,
      Image_Image,
      Image_Pixbuf,
      Image_Stock,
      Image_Icon_Set,
      Image_Animation);
   pragma Convention (C, Gtk_Image_Type);

   procedure Gtk_New    (Image : out Gtk_Image);
   procedure Initialize (Image : access Gtk_Image_Record'Class);
   --  Creates a new empty image

   procedure Gtk_New
     (Image : out Gtk_Image;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap);
   procedure Initialize
     (Image : access Gtk_Image_Record'Class;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap);
   --  Creates or initializes an image

   procedure Gtk_New
     (Image  : out Gtk_Image;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   procedure Initialize
     (Image  : access Gtk_Image_Record'Class;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Creates or initializes an image from its components

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Filename : String);
   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Filename : String);
   --  Creates or initializes an image from a file

   procedure Gtk_New
     (Image  : out Gtk_Image;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   procedure Initialize
     (Image  : access Gtk_Image_Record'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Creates or initializes an image from an existing pixbuf

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Creates or initializes an image from one of the predefined images of
   --  gtk+ (see gtk-stock.ads). The image will be scaled to the appropriate
   --  format.

   procedure Gtk_New
     (Image    : out Gtk_Image;
      Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize
     (Image    : access Gtk_Image_Record'Class;
      Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Creates or initializes an image from a set of icons

   procedure Gtk_New
     (Image     : out Gtk_Image;
      Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   procedure Initialize
     (Image     : access Gtk_Image_Record'Class;
      Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   --  Create or initializes a GtkImage displaying the given animation.

   procedure Gtk_New_From_Icon_Name
     (Image     : out Gtk_Image;
      Icon_Name : String;
      Size      : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize_From_Icon_Name
     (Image     : access Gtk_Image_Record'Class;
      Icon_Name : String;
      Size      : Gtk.Enums.Gtk_Icon_Size);
   --  Creates or initialized an image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Image.

   procedure Set
     (Image  : access Gtk_Image_Record;
      Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   procedure Get
     (Image  : access Gtk_Image_Record;
      Pixmap : out Gdk.Pixmap.Gdk_Pixmap;
      Mask   : out Gdk.Bitmap.Gdk_Bitmap);
   --  Set or Get the values of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   procedure Set
     (Image : access Gtk_Image_Record;
      Val   : Gdk.Image.Gdk_Image;
      Mask  : Gdk.Bitmap.Gdk_Bitmap);
   procedure Get
     (Image : access Gtk_Image_Record;
      Val   : out Gdk.Image.Gdk_Image;
      Mask  : out Gdk.Bitmap.Gdk_Bitmap);
   --  Set or Get the value of a Gtk_Image.
   --  Mask indicates which parts of the image should be transparent.

   procedure Set (Image : access Gtk_Image_Record; File : String);

   procedure Set
     (Image : access Gtk_Image_Record; Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf);
   function Get (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Set or get the image stored in Image

   procedure Set
     (Image    : access Gtk_Image_Record;
      Stock_Id : String;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String;
   --  Set or get the image stored in Image

   procedure Set
     (Image    : access Gtk_Image_Record;
      Icon_Set : Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Get
     (Image    : access Gtk_Image_Record;
      Icon_Set : out Gtk.Icon_Factory.Gtk_Icon_Set;
      Size     : out Gtk.Enums.Gtk_Icon_Size);
   --  Set or get the image stored in Image

   procedure Set
     (Image     : access Gtk_Image_Record;
      Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   function Get
     (Image : access Gtk_Image_Record) return Gdk.Pixbuf.Gdk_Pixbuf_Animation;
   --  Get the Pixbuf Animation being displayed by the given Image. The
   --  reference counter for the returned animation is not incremented. This
   --  must be done separately if needed.

   function Get_Storage_Type
     (Image : access Gtk_Image_Record) return Gtk_Image_Type;
   --  Indicates how the image was created

   procedure Clear (Image : access Gtk_Image_Record);
   --  Resets the image to be empty.

   procedure Set_From_Icon_Name
     (Image     : access Gtk_Image_Record;
      Icon_Name : String;
      Size      : Gtk.Enums.Gtk_Icon_Size);
   procedure Get_Icon_Name
     (Image : access Gtk_Image_Record;
      Name  : out GNAT.Strings.String_Access;
      Size  : out Gtk.Enums.Gtk_Icon_Size);
   --  Gets the icon name and size being displayed by the image
   --  The storage type of the image must be Image_Empty or Image_Icon_Name.
   --  The returned string must be freed by the caller.

   procedure Set_Pixel_Size
     (Image : access Gtk_Image_Record; Pixel_Size : Gint);
   function Get_Pixel_Size (Image : access Gtk_Image_Record) return Gint;
   --  Sets or Gets the pixel size used for named icons.
   --  If the pixel size is set to a value different from -1, it is used
   --  instead of the icon size set by Set_From_Icon_Name.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  File_Property
   --  Type:  String
   --  Descr: Filename to load and display
   --
   --  Name:  Icon_Name_Property
   --  Type:  String
   --  Descr: The name of the icon from the icon theme
   --
   --  Name:  Icon_Set_Property
   --  Type:  Boxed
   --  Descr: Icon set to display
   --
   --  Name:  Icon_Size_Property
   --  Type:  Int
   --  Descr: Symbolic size to use for stock icon, icon set or named icon
   --
   --  Name:  Image_Property
   --  Type:  Object
   --  Descr: A GdkImage to display
   --
   --  Name:  Mask_Property
   --  Type:  Object
   --  Descr: Mask bitmap to use with GdkImage or GdkPixmap
   --
   --  Name:  Pixbuf_Property
   --  Type:  Object
   --  Descr: A GdkPixbuf to display
   --
   --  Name:  Pixbuf_Animation_Property
   --  Type:  Object
   --  Descr: GdkPixbufAnimation to display
   --
   --  Name:  Pixel_Size_Property
   --  Type:  Int
   --  Descr: Pixel size to use for named icon
   --
   --  Name:  Pixmap_Property
   --  Type:  Object
   --  Descr: A GdkPixmap to display
   --
   --  Name:  Stock_Property
   --  Type:  String
   --  Descr: Stock ID for a stock image to display
   --
   --  Name:  Storage_Type_Property
   --  Type:  Enum
   --  Descr: The representation being used for image data
   --
   --  </properties>

   package Image_Type_Properties is new
     Glib.Generic_Properties.Generic_Internal_Discrete_Property
       (Gtk_Image_Type);
   type Property_Image_Type is new Image_Type_Properties.Property;

   File_Property             : constant Glib.Properties.Property_String;
   Icon_Name_Property        : constant Glib.Properties.Property_String;
   --   Icon_Set_Property         : constant Glib.Properties.Property_Boxed;
   Icon_Size_Property        : constant Glib.Properties.Property_Int;
   Image_Property            : constant Glib.Properties.Property_Object;
   Mask_Property             : constant Glib.Properties.Property_Object;
   Pixbuf_Property           : constant Glib.Properties.Property_Object;
   Pixbuf_Animation_Property : constant Glib.Properties.Property_Object;
   Pixel_Size_Property       : constant Glib.Properties.Property_Int;
   Pixmap_Property           : constant Glib.Properties.Property_Object;
   Stock_Property            : constant Glib.Properties.Property_String;
   Storage_Type_Property     : constant Property_Image_Type;

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Image_Record is new Gtk.Misc.Gtk_Misc_Record with null record;

   File_Property      : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   --  Icon_Set_Property  : constant Glib.Properties.Property_Boxed :=
   --    Glib.Properties.Build ("icon-set");
   Icon_Size_Property  : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("icon-size");
   Image_Property      : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("image");
   Mask_Property       : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("mask");
   Pixbuf_Property     : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Pixbuf_Animation_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf-animation");
   Pixel_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixel-size");
   Pixmap_Property     : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixmap");
   Stock_Property      : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock");
   Storage_Type_Property : constant Property_Image_Type :=
     Build ("storage-type");

   pragma Import (C, Get_Type, "gtk_image_get_type");
end Gtk.Image;

--  These subprograms never had a binding, are now obsolescent anyway
--  No binding: gtk_image_get
--  No binding: gtk_image_set
