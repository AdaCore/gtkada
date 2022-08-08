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
--  The Gtk.Image.Gtk_Image widget displays an image. Various kinds of object
--  can be displayed as an image; most typically, you would load a
--  Gdk.Pixbuf.Gdk_Pixbuf ("pixel buffer") from a file, and then display that.
--  There's a convenience function to do this, Gtk.Image.Gtk_New, used as
--  follows: |[<!-- language="C" --> GtkWidget *image; image =
--  gtk_image_new_from_file ("myfile.png"); ]| If the file isn't loaded
--  successfully, the image will contain a "broken image" icon similar to that
--  used in many web browsers. If you want to handle errors in loading the file
--  yourself, for example by displaying an error message, then load the image
--  with Gdk.Pixbuf.Gdk_New_From_File, then create the Gtk.Image.Gtk_Image with
--  Gtk.Image.Gtk_New.
--
--  The image file may contain an animation, if so the Gtk.Image.Gtk_Image
--  will display an animation (Gdk_Pixbuf_Animation) instead of a static image.
--
--  Gtk.Image.Gtk_Image is a subclass of Gtk.Misc.Gtk_Misc, which implies that
--  you can align it (center, left, right) and add padding to it, using
--  Gtk.Misc.Gtk_Misc methods.
--
--  Gtk.Image.Gtk_Image is a "no window" widget (has no Gdk.Gdk_Window of its
--  own), so by default does not receive events. If you want to receive events
--  on the image, such as button clicks, place the image inside a
--  Gtk.Event_Box.Gtk_Event_Box, then connect to the event signals on the event
--  box.
--
--  ## Handling button press events on a Gtk.Image.Gtk_Image.
--
--  |[<!-- language="C" --> static gboolean button_press_callback (GtkWidget
--  *event_box, GdkEventButton *event, gpointer data) { g_print ("Event box
--  clicked at coordinates %f,%f\n", event->x, event->y);
--
--  // Returning TRUE means we handled the event, so the signal // emission
--  should be stopped (don't call any further callbacks // that may be
--  connected). Return FALSE to continue invoking callbacks. return TRUE; }
--
--  static GtkWidget* create_image (void) { GtkWidget *image; GtkWidget
--  *event_box;
--
--  image = gtk_image_new_from_file ("myfile.png");
--
--  event_box = gtk_event_box_new ();
--
--  gtk_container_add (GTK_CONTAINER (event_box), image);
--
--  g_signal_connect (G_OBJECT (event_box), "button_press_event", G_CALLBACK
--  (button_press_callback), image);
--
--  return image; } ]|
--
--  When handling events on the event box, keep in mind that coordinates in
--  the image may be different from event box coordinates due to the alignment
--  and padding settings on the image (see Gtk.Misc.Gtk_Misc). The simplest way
--  to solve this is to set the alignment to 0.0 (left/top), and set the
--  padding to zero. Then the origin of the image will be the same as the
--  origin of the event box.
--
--  Sometimes an application will want to avoid depending on external data
--  files, such as image files. GTK+ comes with a program to avoid this, called
--  "gdk-pixbuf-csource". This library allows you to convert an image into a C
--  variable declaration, which can then be loaded into a Gdk.Pixbuf.Gdk_Pixbuf
--  using gdk_pixbuf_new_from_inline.
--
--  # CSS nodes
--
--  GtkImage has a single CSS node with the name image. The style classes may
--  appear on image CSS nodes: .icon-dropshadow, .lowres-icon.
--
--  </description>
--  <screenshot>gtk-image</screenshot>
--  <group>Display widgets</group>

pragma Warnings (Off, "*is already use-visible*");
with Cairo;                   use Cairo;
with GNAT.Strings;            use GNAT.Strings;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Glib;                    use Glib;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Icon_Set;            use Gtk.Icon_Set;
with Gtk.Misc;                use Gtk.Misc;

package Gtk.Image is

   type Gtk_Image_Record is new Gtk_Misc_Record with null record;
   type Gtk_Image is access all Gtk_Image_Record'Class;

   type Gtk_Image_Type is (
      Image_Empty,
      Image_Pixbuf,
      Image_Stock,
      Image_Icon_Set,
      Image_Animation,
      Image_Icon_Name,
      Image_Gicon,
      Image_Surface);
   pragma Convention (C, Gtk_Image_Type);
   --  Describes the image data representation used by a Gtk.Image.Gtk_Image.
   --  If you want to get the image from the widget, you can only get the
   --  currently-stored representation. e.g. if the Gtk.Image.Get_Storage_Type
   --  returns GTK_IMAGE_PIXBUF, then you can call Gtk.Image.Get but not Get.
   --  For empty images, you can request any storage type (call any of the
   --  "get" functions), but they will all return null values.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Image_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Image_Type);
   type Property_Gtk_Image_Type is new Gtk_Image_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Image : out Gtk_Image);
   procedure Initialize (Image : not null access Gtk_Image_Record'Class);
   --  Creates a new empty Gtk.Image.Gtk_Image widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Image_New return Gtk_Image;
   --  Creates a new empty Gtk.Image.Gtk_Image widget.

   procedure Gtk_New
      (Image     : out Gtk_Image;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   procedure Initialize
      (Image     : not null access Gtk_Image_Record'Class;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   --  Creates a Gtk.Image.Gtk_Image displaying the given animation. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the animation; you
   --  still need to unref it if you own references. Gtk.Image.Gtk_Image will
   --  add its own reference rather than adopting yours.
   --  Note that the animation frames are shown using a timeout with
   --  G_PRIORITY_DEFAULT. When using animations to indicate busyness, keep in
   --  mind that the animation will only be shown if the main loop is not busy
   --  with something that has a higher priority.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "animation": an animation

   function Gtk_Image_New_From_Animation
      (Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation) return Gtk_Image;
   --  Creates a Gtk.Image.Gtk_Image displaying the given animation. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the animation; you
   --  still need to unref it if you own references. Gtk.Image.Gtk_Image will
   --  add its own reference rather than adopting yours.
   --  Note that the animation frames are shown using a timeout with
   --  G_PRIORITY_DEFAULT. When using animations to indicate busyness, keep in
   --  mind that the animation will only be shown if the main loop is not busy
   --  with something that has a higher priority.
   --  "animation": an animation

   procedure Gtk_New (Image : out Gtk_Image; Filename : UTF8_String);
   procedure Initialize
      (Image    : not null access Gtk_Image_Record'Class;
       Filename : UTF8_String);
   --  Creates a new Gtk.Image.Gtk_Image displaying the file Filename. If the
   --  file isn't found or can't be loaded, the resulting Gtk.Image.Gtk_Image
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid Gtk.Image.Gtk_Image widget.
   --  If the file contains an animation, the image will contain an animation.
   --  If you need to detect failures to load the file, use
   --  Gdk.Pixbuf.Gdk_New_From_File to load the file yourself, then create the
   --  Gtk.Image.Gtk_Image from the pixbuf. (Or for animations, use
   --  Gdk.Pixbuf.Gdk_New_From_File).
   --  The storage type (gtk_image_get_storage_type) of the returned image is
   --  not defined, it will be whatever is appropriate for displaying the file.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "filename": a filename

   function Gtk_Image_New_From_File
      (Filename : UTF8_String) return Gtk_Image;
   --  Creates a new Gtk.Image.Gtk_Image displaying the file Filename. If the
   --  file isn't found or can't be loaded, the resulting Gtk.Image.Gtk_Image
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid Gtk.Image.Gtk_Image widget.
   --  If the file contains an animation, the image will contain an animation.
   --  If you need to detect failures to load the file, use
   --  Gdk.Pixbuf.Gdk_New_From_File to load the file yourself, then create the
   --  Gtk.Image.Gtk_Image from the pixbuf. (Or for animations, use
   --  Gdk.Pixbuf.Gdk_New_From_File).
   --  The storage type (gtk_image_get_storage_type) of the returned image is
   --  not defined, it will be whatever is appropriate for displaying the file.
   --  "filename": a filename

   procedure Gtk_New_From_Gicon
      (Image : out Gtk_Image;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize_From_Gicon
      (Image : not null access Gtk_Image_Record'Class;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.
   --  Since: gtk+ 2.14
   --  Initialize_From_Gicon does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "icon": an icon
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   function Gtk_Image_New_From_Gicon
      (Icon : Glib.G_Icon.G_Icon;
       Size : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image;
   --  Creates a Gtk.Image.Gtk_Image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.
   --  Since: gtk+ 2.14
   --  "icon": an icon
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Gtk_New_From_Icon_Name
      (Image     : out Gtk_Image;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize_From_Icon_Name
      (Image     : not null access Gtk_Image_Record'Class;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.
   --  Since: gtk+ 2.6
   --  Initialize_From_Icon_Name does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  "icon_name": an icon name or null
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   function Gtk_Image_New_From_Icon_Name
      (Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image;
   --  Creates a Gtk.Image.Gtk_Image displaying an icon from the current icon
   --  theme. If the icon name isn't known, a "broken image" icon will be
   --  displayed instead. If the current icon theme is changed, the icon will
   --  be updated appropriately.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name or null
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize
      (Image    : not null access Gtk_Image_Record'Class;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying an icon set. Sample stock
   --  sizes are GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. Instead of
   --  using this function, usually it's better to create a
   --  Gtk.Icon_Factory.Gtk_Icon_Factory, put your icon sets in the icon
   --  factory, add the icon factory to the list of default factories with
   --  Gtk.Icon_Factory.Add_Default, and then use Gtk.Image.Gtk_New. This will
   --  allow themes to override the icon you ship with your application.
   --  The Gtk.Image.Gtk_Image does not assume a reference to the icon set;
   --  you still need to unref it if you own references. Gtk.Image.Gtk_Image
   --  will add its own reference rather than adopting yours.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "icon_set": a Gtk.Icon_Set.Gtk_Icon_Set
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   function Gtk_Image_New_From_Icon_Set
      (Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image;
   --  Creates a Gtk.Image.Gtk_Image displaying an icon set. Sample stock
   --  sizes are GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. Instead of
   --  using this function, usually it's better to create a
   --  Gtk.Icon_Factory.Gtk_Icon_Factory, put your icon sets in the icon
   --  factory, add the icon factory to the list of default factories with
   --  Gtk.Icon_Factory.Add_Default, and then use Gtk.Image.Gtk_New. This will
   --  allow themes to override the icon you ship with your application.
   --  The Gtk.Image.Gtk_Image does not assume a reference to the icon set;
   --  you still need to unref it if you own references. Gtk.Image.Gtk_Image
   --  will add its own reference rather than adopting yours.
   --  "icon_set": a Gtk.Icon_Set.Gtk_Icon_Set
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Gtk_New
      (Image  : out Gtk_Image;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize
      (Image  : not null access Gtk_Image_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a new Gtk.Image.Gtk_Image displaying Pixbuf. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the pixbuf; you still
   --  need to unref it if you own references. Gtk.Image.Gtk_Image will add its
   --  own reference rather than adopting yours.
   --  Note that this function just creates an Gtk.Image.Gtk_Image from the
   --  pixbuf. The Gtk.Image.Gtk_Image created will not react to state changes.
   --  Should you want that, you should use Gtk.Image.Gtk_New_From_Icon_Name.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   function Gtk_Image_New_From_Pixbuf
      (Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class) return Gtk_Image;
   --  Creates a new Gtk.Image.Gtk_Image displaying Pixbuf. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the pixbuf; you still
   --  need to unref it if you own references. Gtk.Image.Gtk_Image will add its
   --  own reference rather than adopting yours.
   --  Note that this function just creates an Gtk.Image.Gtk_Image from the
   --  pixbuf. The Gtk.Image.Gtk_Image created will not react to state changes.
   --  Should you want that, you should use Gtk.Image.Gtk_New_From_Icon_Name.
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   procedure Gtk_New_From_Resource
      (Image         : out Gtk_Image;
       Resource_Path : UTF8_String);
   procedure Initialize_From_Resource
      (Image         : not null access Gtk_Image_Record'Class;
       Resource_Path : UTF8_String);
   --  Creates a new Gtk.Image.Gtk_Image displaying the resource file
   --  Resource_Path. If the file isn't found or can't be loaded, the resulting
   --  Gtk.Image.Gtk_Image will display a "broken image" icon. This function
   --  never returns null, it always returns a valid Gtk.Image.Gtk_Image
   --  widget.
   --  If the file contains an animation, the image will contain an animation.
   --  If you need to detect failures to load the file, use
   --  Gdk.Pixbuf.Gdk_New_From_File to load the file yourself, then create the
   --  Gtk.Image.Gtk_Image from the pixbuf. (Or for animations, use
   --  Gdk.Pixbuf.Gdk_New_From_File).
   --  The storage type (gtk_image_get_storage_type) of the returned image is
   --  not defined, it will be whatever is appropriate for displaying the file.
   --  Since: gtk+ 3.4
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "resource_path": a resource path

   function Gtk_Image_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Image;
   --  Creates a new Gtk.Image.Gtk_Image displaying the resource file
   --  Resource_Path. If the file isn't found or can't be loaded, the resulting
   --  Gtk.Image.Gtk_Image will display a "broken image" icon. This function
   --  never returns null, it always returns a valid Gtk.Image.Gtk_Image
   --  widget.
   --  If the file contains an animation, the image will contain an animation.
   --  If you need to detect failures to load the file, use
   --  Gdk.Pixbuf.Gdk_New_From_File to load the file yourself, then create the
   --  Gtk.Image.Gtk_Image from the pixbuf. (Or for animations, use
   --  Gdk.Pixbuf.Gdk_New_From_File).
   --  The storage type (gtk_image_get_storage_type) of the returned image is
   --  not defined, it will be whatever is appropriate for displaying the file.
   --  Since: gtk+ 3.4
   --  "resource_path": a resource path

   procedure Gtk_New
      (Image    : out Gtk_Image;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   procedure Initialize
      (Image    : not null access Gtk_Image_Record'Class;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   --  Creates a Gtk.Image.Gtk_Image displaying a stock icon. Sample stock
   --  icon names are GTK_STOCK_OPEN, GTK_STOCK_QUIT. Sample stock sizes are
   --  GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. If the stock icon name
   --  isn't known, the image will be empty. You can register your own stock
   --  icon names, see Gtk.Icon_Factory.Add_Default and Gtk.Icon_Factory.Add.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "stock_id": a stock icon name
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   function Gtk_Image_New_From_Stock
      (Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size) return Gtk_Image;
   --  Creates a Gtk.Image.Gtk_Image displaying a stock icon. Sample stock
   --  icon names are GTK_STOCK_OPEN, GTK_STOCK_QUIT. Sample stock sizes are
   --  GTK_ICON_SIZE_MENU, GTK_ICON_SIZE_SMALL_TOOLBAR. If the stock icon name
   --  isn't known, the image will be empty. You can register your own stock
   --  icon names, see Gtk.Icon_Factory.Add_Default and Gtk.Icon_Factory.Add.
   --  "stock_id": a stock icon name
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Gtk_New_From_Surface
      (Image   : out Gtk_Image;
       Surface : Cairo.Cairo_Surface);
   procedure Initialize_From_Surface
      (Image   : not null access Gtk_Image_Record'Class;
       Surface : Cairo.Cairo_Surface);
   --  Creates a new Gtk.Image.Gtk_Image displaying Surface. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the surface; you
   --  still need to unref it if you own references. Gtk.Image.Gtk_Image will
   --  add its own reference rather than adopting yours.
   --  Since: gtk+ 3.10
   --  Initialize_From_Surface does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "surface": a cairo_surface_t, or null

   function Gtk_Image_New_From_Surface
      (Surface : Cairo.Cairo_Surface) return Gtk_Image;
   --  Creates a new Gtk.Image.Gtk_Image displaying Surface. The
   --  Gtk.Image.Gtk_Image does not assume a reference to the surface; you
   --  still need to unref it if you own references. Gtk.Image.Gtk_Image will
   --  add its own reference rather than adopting yours.
   --  Since: gtk+ 3.10
   --  "surface": a cairo_surface_t, or null

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_image_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear (Image : not null access Gtk_Image_Record);
   --  Resets the image to be empty.
   --  Since: gtk+ 2.8

   function Get
      (Image : not null access Gtk_Image_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf_Animation;
   --  Gets the Gdk_Pixbuf_Animation being displayed by the
   --  Gtk.Image.Gtk_Image. The storage type of the image must be
   --  Gtk.Image.Image_Empty or Gtk.Image.Image_Animation (see
   --  Gtk.Image.Get_Storage_Type). The caller of this function does not own a
   --  reference to the returned animation.

   procedure Get
      (Image  : not null access Gtk_Image_Record;
       G_Icon : out Glib.G_Icon.G_Icon;
       Size   : out Gtk.Enums.Gtk_Icon_Size);
   --  Gets the Glib.G_Icon.G_Icon and size being displayed by the
   --  Gtk.Image.Gtk_Image. The storage type of the image must be
   --  Gtk.Image.Image_Empty or Gtk.Image.Image_Gicon (see
   --  Gtk.Image.Get_Storage_Type). The caller of this function does not own a
   --  reference to the returned Glib.G_Icon.G_Icon.
   --  Since: gtk+ 2.14
   --  "gicon": place to store a Glib.G_Icon.G_Icon, or null
   --  "size": place to store an icon size (Gtk.Enums.Gtk_Icon_Size), or null

   procedure Get
      (Image    : not null access Gtk_Image_Record;
       Icon_Set : out Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : out Gtk.Enums.Gtk_Icon_Size);
   pragma Obsolescent (Get);
   --  Gets the icon set and size being displayed by the Gtk.Image.Gtk_Image.
   --  The storage type of the image must be Gtk.Image.Image_Empty or
   --  Gtk.Image.Image_Icon_Set (see Gtk.Image.Get_Storage_Type).
   --  Deprecated since 3.10, 1
   --  "icon_set": location to store a Gtk.Icon_Set.Gtk_Icon_Set, or null
   --  "size": location to store a stock icon size (Gtk.Enums.Gtk_Icon_Size),
   --  or null

   function Get
      (Image : not null access Gtk_Image_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Gets the Gdk.Pixbuf.Gdk_Pixbuf being displayed by the
   --  Gtk.Image.Gtk_Image. The storage type of the image must be
   --  Gtk.Image.Image_Empty or Gtk.Image.Image_Pixbuf (see
   --  Gtk.Image.Get_Storage_Type). The caller of this function does not own a
   --  reference to the returned pixbuf.

   function Get_Pixel_Size
      (Image : not null access Gtk_Image_Record) return Glib.Gint;
   --  Gets the pixel size used for named icons.
   --  Since: gtk+ 2.6

   procedure Set_Pixel_Size
      (Image      : not null access Gtk_Image_Record;
       Pixel_Size : Glib.Gint);
   --  Sets the pixel size to use for named icons. If the pixel size is set to
   --  a value != -1, it is used instead of the icon size set by
   --  Gtk.Image.Set_From_Icon_Name.
   --  Since: gtk+ 2.6
   --  "pixel_size": the new pixel size

   function Get_Storage_Type
      (Image : not null access Gtk_Image_Record) return Gtk_Image_Type;
   --  Gets the type of representation being used by the Gtk.Image.Gtk_Image
   --  to store image data. If the Gtk.Image.Gtk_Image has no image data, the
   --  return value will be Gtk.Image.Image_Empty.

   procedure Set
      (Image     : not null access Gtk_Image_Record;
       Animation : Gdk.Pixbuf.Gdk_Pixbuf_Animation);
   --  Causes the Gtk.Image.Gtk_Image to display the given animation (or
   --  display nothing, if you set the animation to null).
   --  "animation": the Gdk_Pixbuf_Animation

   procedure Set
      (Image    : not null access Gtk_Image_Record;
       Filename : UTF8_String := "");
   --  See Gtk.Image.Gtk_New for details.
   --  "filename": a filename or null

   procedure Set
      (Image : not null access Gtk_Image_Record;
       Icon  : Glib.G_Icon.G_Icon;
       Size  : Gtk.Enums.Gtk_Icon_Size);
   --  See Gtk.Image.Gtk_New_From_Gicon for details.
   --  Since: gtk+ 2.14
   --  "icon": an icon
   --  "size": an icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set
      (Image    : not null access Gtk_Image_Record;
       Icon_Set : Gtk.Icon_Set.Gtk_Icon_Set;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   pragma Obsolescent (Set);
   --  See Gtk.Image.Gtk_New for details.
   --  Deprecated since 3.10, 1
   --  "icon_set": a Gtk.Icon_Set.Gtk_Icon_Set
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set
      (Image  : not null access Gtk_Image_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  See Gtk.Image.Gtk_New for details.
   --  "pixbuf": a Gdk.Pixbuf.Gdk_Pixbuf or null

   procedure Set
      (Image    : not null access Gtk_Image_Record;
       Stock_Id : UTF8_String;
       Size     : Gtk.Enums.Gtk_Icon_Size);
   pragma Obsolescent (Set);
   --  See Gtk.Image.Gtk_New for details.
   --  Deprecated since 3.10, 1
   --  "stock_id": a stock icon name
   --  "size": a stock icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set_From_Icon_Name
      (Image     : not null access Gtk_Image_Record;
       Icon_Name : UTF8_String := "";
       Size      : Gtk.Enums.Gtk_Icon_Size);
   --  See Gtk.Image.Gtk_New_From_Icon_Name for details.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name or null
   --  "size": an icon size (Gtk.Enums.Gtk_Icon_Size)

   procedure Set_From_Resource
      (Image         : not null access Gtk_Image_Record;
       Resource_Path : UTF8_String := "");
   --  See Gtk.Image.Gtk_New_From_Resource for details.
   --  "resource_path": a resource path or null

   procedure Set_From_Surface
      (Image   : not null access Gtk_Image_Record;
       Surface : Cairo.Cairo_Surface);
   --  See Gtk.Image.Gtk_New_From_Surface for details.
   --  Since: gtk+ 3.10
   --  "surface": a cairo_surface_t or null

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get
     (Image : access Gtk_Image_Record;
      Size  : access Gtk.Enums.Gtk_Icon_Size) return String;
   --  Get the stock_id for the image displayed

   procedure Get_Icon_Name
     (Image : access Gtk_Image_Record;
      Name  : out GNAT.Strings.String_Access;
      Size  : out Gtk.Enums.Gtk_Icon_Size);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   File_Property : constant Glib.Properties.Property_String;

   G_Icon_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Glib.G_Icon.G_Icon
   --  The GIcon displayed in the GtkImage. For themed icons, If the icon
   --  theme is changed, the image will be updated automatically.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the icon in the icon theme. If the icon theme is changed,
   --  the image will be updated automatically.

   Icon_Set_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Icon_Set.Gtk_Icon_Set

   Icon_Size_Property : constant Glib.Properties.Property_Int;

   Pixbuf_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf

   Pixbuf_Animation_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf_Animation

   Pixel_Size_Property : constant Glib.Properties.Property_Int;
   --  The "pixel-size" property can be used to specify a fixed size
   --  overriding the Gtk.Image.Gtk_Image:icon-size property for images of type
   --  Gtk.Image.Image_Icon_Name.

   Resource_Property : constant Glib.Properties.Property_String;
   --  A path to a resource file to display.

   Stock_Property : constant Glib.Properties.Property_String;

   Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type;
   --  Type: Gtk_Image_Type

   Surface_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cairo.Cairo_Surface

   Use_Fallback_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the icon displayed in the GtkImage will use standard icon names
   --  fallback. The value of this property is only relevant for images of type
   --  Gtk.Image.Image_Icon_Name and Gtk.Image.Image_Gicon.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Image_Record, Gtk_Image);
   function "+"
     (Widget : access Gtk_Image_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Image
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Fallback_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-fallback");
   Surface_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("surface");
   Storage_Type_Property : constant Gtk.Image.Property_Gtk_Image_Type :=
     Gtk.Image.Build ("storage-type");
   Stock_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock");
   Resource_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("resource");
   Pixel_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixel-size");
   Pixbuf_Animation_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pixbuf-animation");
   Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Icon_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("icon-size");
   Icon_Set_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("icon-set");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   G_Icon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gicon");
   File_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file");
end Gtk.Image;
