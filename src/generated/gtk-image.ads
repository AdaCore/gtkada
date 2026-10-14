------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Displays an image.
--
--  <picture> <source srcset="image-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkImage" src="image.png"> </picture>
--  Various kinds of object can be displayed as an image; most typically, you
--  would load a `GdkTexture` from a file, using the convenience function
--  [ctorGtk.Image.new_from_file], for instance:
--
--  ```c GtkWidget *image = gtk_image_new_from_file ("myfile.png"); ```
--
--  If the file isn't loaded successfully, the image will contain a "broken
--  image" icon similar to that used in many web browsers.
--
--  If you want to handle errors in loading the file yourself, for example by
--  displaying an error message, then load the image with an image loading
--  framework such as libglycin, then create the `GtkImage` with
--  [ctorGtk.Image.new_from_paintable].
--
--  Sometimes an application will want to avoid depending on external data
--  files, such as image files. See the documentation of `GResource` inside
--  GIO, for details. In this case, [propertyGtk.Image:resource],
--  [ctorGtk.Image.new_from_resource], and [methodGtk.Image.set_from_resource]
--  should be used.
--
--  `GtkImage` displays its image as an icon, with a size that is determined
--  by the application. See [classGtk.Picture] if you want to show an image at
--  is actual size.
--
--  ## CSS nodes
--
--  `GtkImage` has a single CSS node with the name `image`. The style classes
--  `.normal-icons` or `.large-icons` may appear, depending on the
--  [propertyGtk.Image:icon-size] property.
--
--  ## Accessibility
--
--  `GtkImage` uses the [enumGtk.AccessibleRole.img] role.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Paintable;         use Gdk.Paintable;
with Gdk.Pixbuf;            use Gdk.Pixbuf;
with Glib;                  use Glib;
with Glib.G_Icon;           use Glib.G_Icon;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Image is

   type Gtk_Image_Record is new Gtk_Widget_Record with null record;
   type Gtk_Image is access all Gtk_Image_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Image);
   procedure Initialize (Self : not null access Gtk_Image_Record'Class);
   --  Creates a new empty `GtkImage` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Image_New return Gtk_Image;
   --  Creates a new empty `GtkImage` widget.

   procedure Gtk_New_From_File
      (Self     : out Gtk_Image;
       Filename : UTF8_String);
   procedure Initialize_From_File
      (Self     : not null access Gtk_Image_Record'Class;
       Filename : UTF8_String);
   --  Creates a new `GtkImage` displaying the file Filename.
   --  If the file isn't found or can't be loaded, the resulting `GtkImage`
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid `GtkImage` widget.
   --  If you need to detect failures to load the file, use an image loading
   --  framework such as libglycin to load the file yourself, then create the
   --  `GtkImage` from the texture.
   --  The storage type (see [methodGtk.Image.get_storage_type]) of the
   --  returned image is not defined, it will be whatever is appropriate for
   --  displaying the file.
   --  Initialize_From_File does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Filename a filename

   function Gtk_Image_New_From_File
      (Filename : UTF8_String) return Gtk_Image;
   --  Creates a new `GtkImage` displaying the file Filename.
   --  If the file isn't found or can't be loaded, the resulting `GtkImage`
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid `GtkImage` widget.
   --  If you need to detect failures to load the file, use an image loading
   --  framework such as libglycin to load the file yourself, then create the
   --  `GtkImage` from the texture.
   --  The storage type (see [methodGtk.Image.get_storage_type]) of the
   --  returned image is not defined, it will be whatever is appropriate for
   --  displaying the file.
   --  @param Filename a filename

   procedure Gtk_New_From_Gicon
      (Self : out Gtk_Image;
       Icon : Glib.G_Icon.G_Icon);
   procedure Initialize_From_Gicon
      (Self : not null access Gtk_Image_Record'Class;
       Icon : Glib.G_Icon.G_Icon);
   --  Creates a `GtkImage` displaying an icon from the current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead. If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  Initialize_From_Gicon does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Icon an icon

   function Gtk_Image_New_From_Gicon
      (Icon : Glib.G_Icon.G_Icon) return Gtk_Image;
   --  Creates a `GtkImage` displaying an icon from the current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead. If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  @param Icon an icon

   procedure Gtk_New_From_Icon_Name
      (Self      : out Gtk_Image;
       Icon_Name : UTF8_String := "");
   procedure Initialize_From_Icon_Name
      (Self      : not null access Gtk_Image_Record'Class;
       Icon_Name : UTF8_String := "");
   --  Creates a `GtkImage` displaying an icon from the current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead. If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  Initialize_From_Icon_Name does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Icon_Name an icon name

   function Gtk_Image_New_From_Icon_Name
      (Icon_Name : UTF8_String := "") return Gtk_Image;
   --  Creates a `GtkImage` displaying an icon from the current icon theme.
   --  If the icon name isn't known, a "broken image" icon will be displayed
   --  instead. If the current icon theme is changed, the icon will be updated
   --  appropriately.
   --  @param Icon_Name an icon name

   procedure Gtk_New_From_Paintable
      (Self      : out Gtk_Image;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   procedure Initialize_From_Paintable
      (Self      : not null access Gtk_Image_Record'Class;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Creates a new `GtkImage` displaying Paintable.
   --  The `GtkImage` does not assume a reference to the paintable; you still
   --  need to unref it if you own references. `GtkImage` will add its own
   --  reference rather than adopting yours.
   --  The `GtkImage` will track changes to the Paintable and update its size
   --  and contents in response to it.
   --  Note that paintables are still subject to the icon size that is set on
   --  the image. If you want to display a paintable at its intrinsic size, use
   --  [classGtk.Picture] instead.
   --  If Paintable is a [ifaceGtk.SymbolicPaintable], then it will be
   --  recolored with the symbolic palette from the theme.
   --  Initialize_From_Paintable does nothing if the object was already
   --  created with another call to Initialize* or G_New.
   --  @param Paintable a `GdkPaintable`

   function Gtk_Image_New_From_Paintable
      (Paintable : Gdk.Paintable.Gdk_Paintable) return Gtk_Image;
   --  Creates a new `GtkImage` displaying Paintable.
   --  The `GtkImage` does not assume a reference to the paintable; you still
   --  need to unref it if you own references. `GtkImage` will add its own
   --  reference rather than adopting yours.
   --  The `GtkImage` will track changes to the Paintable and update its size
   --  and contents in response to it.
   --  Note that paintables are still subject to the icon size that is set on
   --  the image. If you want to display a paintable at its intrinsic size, use
   --  [classGtk.Picture] instead.
   --  If Paintable is a [ifaceGtk.SymbolicPaintable], then it will be
   --  recolored with the symbolic palette from the theme.
   --  @param Paintable a `GdkPaintable`

   procedure Gtk_New_From_Pixbuf
      (Self   : out Gtk_Image;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize_From_Pixbuf
      (Self   : not null access Gtk_Image_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a new `GtkImage` displaying Pixbuf.
   --  The `GtkImage` does not assume a reference to the pixbuf; you still
   --  need to unref it if you own references. `GtkImage` will add its own
   --  reference rather than adopting yours.
   --  This is a helper for [ctorGtk.Image.new_from_paintable], and you can't
   --  get back the exact pixbuf once this is called, only a texture.
   --  Note that this function just creates an `GtkImage` from the pixbuf. The
   --  `GtkImage` created will not react to state changes. Should you want
   --  that, you should use [ctorGtk.Image.new_from_icon_name].
   --  Initialize_From_Pixbuf does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Pixbuf a `GdkPixbuf`

   function Gtk_Image_New_From_Pixbuf
      (Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class) return Gtk_Image;
   --  Creates a new `GtkImage` displaying Pixbuf.
   --  The `GtkImage` does not assume a reference to the pixbuf; you still
   --  need to unref it if you own references. `GtkImage` will add its own
   --  reference rather than adopting yours.
   --  This is a helper for [ctorGtk.Image.new_from_paintable], and you can't
   --  get back the exact pixbuf once this is called, only a texture.
   --  Note that this function just creates an `GtkImage` from the pixbuf. The
   --  `GtkImage` created will not react to state changes. Should you want
   --  that, you should use [ctorGtk.Image.new_from_icon_name].
   --  @param Pixbuf a `GdkPixbuf`

   procedure Gtk_New_From_Resource
      (Self          : out Gtk_Image;
       Resource_Path : UTF8_String);
   procedure Initialize_From_Resource
      (Self          : not null access Gtk_Image_Record'Class;
       Resource_Path : UTF8_String);
   --  Creates a new `GtkImage` displaying the resource file Resource_Path.
   --  If the file isn't found or can't be loaded, the resulting `GtkImage`
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid `GtkImage` widget.
   --  If you need to detect failures to load the file, use an image loading
   --  framework such as libglycin to load the file yourself, then create the
   --  `GtkImage` from the texture.
   --  The storage type (see [methodGtk.Image.get_storage_type]) of the
   --  returned image is not defined, it will be whatever is appropriate for
   --  displaying the file.
   --  Initialize_From_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Resource_Path a resource path

   function Gtk_Image_New_From_Resource
      (Resource_Path : UTF8_String) return Gtk_Image;
   --  Creates a new `GtkImage` displaying the resource file Resource_Path.
   --  If the file isn't found or can't be loaded, the resulting `GtkImage`
   --  will display a "broken image" icon. This function never returns null, it
   --  always returns a valid `GtkImage` widget.
   --  If you need to detect failures to load the file, use an image loading
   --  framework such as libglycin to load the file yourself, then create the
   --  `GtkImage` from the texture.
   --  The storage type (see [methodGtk.Image.get_storage_type]) of the
   --  returned image is not defined, it will be whatever is appropriate for
   --  displaying the file.
   --  @param Resource_Path a resource path

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_image_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear (Self : not null access Gtk_Image_Record);
   --  Resets the image to be empty.

   function Get_Gicon
      (Self : not null access Gtk_Image_Record) return Glib.G_Icon.G_Icon;
   --  Gets the `GIcon` being displayed by the `GtkImage`.
   --  The storage type of the image must be Gtk.Enums.Image_Empty or
   --  Gtk.Enums.Image_Gicon (see [methodGtk.Image.get_storage_type]). The
   --  caller of this function does not own a reference to the returned
   --  `GIcon`.
   --  @return a `GIcon`

   procedure Set_From_Gicon
      (Self : not null access Gtk_Image_Record;
       Icon : Glib.G_Icon.G_Icon);
   --  Sets a `GtkImage` to show a `GIcon`.
   --  See [ctorGtk.Image.new_from_gicon] for details.
   --  @param Icon an icon

   function Get_Icon_Name
      (Self : not null access Gtk_Image_Record) return UTF8_String;
   --  Gets the icon name and size being displayed by the `GtkImage`.
   --  The storage type of the image must be Gtk.Enums.Image_Empty or
   --  Gtk.Enums.Image_Icon_Name (see [methodGtk.Image.get_storage_type]). The
   --  returned string is owned by the `GtkImage` and should not be freed.
   --  @return the icon name

   procedure Set_From_Icon_Name
      (Self      : not null access Gtk_Image_Record;
       Icon_Name : UTF8_String := "");
   --  Sets a `GtkImage` to show a named icon.
   --  See [ctorGtk.Image.new_from_icon_name] for details.
   --  @param Icon_Name an icon name

   function Get_Icon_Size
      (Self : not null access Gtk_Image_Record)
       return Gtk.Enums.Gtk_Icon_Size;
   --  Gets the icon size used by the Image when rendering icons.
   --  @return the image size used by icons

   procedure Set_Icon_Size
      (Self      : not null access Gtk_Image_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size);
   --  Suggests an icon size to the theme for named icons.
   --  @param Icon_Size the new icon size

   function Get_Paintable
      (Self : not null access Gtk_Image_Record)
       return Gdk.Paintable.Gdk_Paintable;
   --  Gets the image `GdkPaintable` being displayed by the `GtkImage`.
   --  The storage type of the image must be Gtk.Enums.Image_Empty or
   --  Gtk.Enums.Image_Paintable (see [methodGtk.Image.get_storage_type]). The
   --  caller of this function does not own a reference to the returned
   --  paintable.
   --  @return the displayed paintable

   procedure Set_From_Paintable
      (Self      : not null access Gtk_Image_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Sets a `GtkImage` to show a `GdkPaintable`.
   --  See [ctorGtk.Image.new_from_paintable] for details.
   --  @param Paintable a `GdkPaintable`

   function Get_Pixel_Size
      (Self : not null access Gtk_Image_Record) return Glib.Gint;
   --  Gets the pixel size used for named icons.
   --  @return the pixel size used for named icons.

   procedure Set_Pixel_Size
      (Self       : not null access Gtk_Image_Record;
       Pixel_Size : Glib.Gint);
   --  Sets the pixel size to use for named icons.
   --  If the pixel size is set to a value != -1, it is used instead of the
   --  icon size set by [methodGtk.Image.set_icon_size].
   --  @param Pixel_Size the new pixel size

   function Get_Storage_Type
      (Self : not null access Gtk_Image_Record)
       return Gtk.Enums.Gtk_Image_Type;
   --  Gets the type of representation being used by the `GtkImage` to store
   --  image data.
   --  If the `GtkImage` has no image data, the return value will be
   --  Gtk.Enums.Image_Empty.
   --  @return image representation being used

   procedure Set_From_File
      (Self     : not null access Gtk_Image_Record;
       Filename : UTF8_String := "");
   --  Sets a `GtkImage` to show a file.
   --  See [ctorGtk.Image.new_from_file] for details.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`, and then use
   --  [methodGtk.Image.set_from_paintable].
   --  @param Filename a filename

   procedure Set_From_Resource
      (Self          : not null access Gtk_Image_Record;
       Resource_Path : UTF8_String := "");
   --  Sets a `GtkImage` to show a resource.
   --  See [ctorGtk.Image.new_from_resource] for details.
   --  @param Resource_Path a resource path

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Image_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Image_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Image_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Image_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Image_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Image_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Image_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Image_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Image_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Image_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Image_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Image_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   File_Property : constant Glib.Properties.Property_String;
   --  A path to the file to display.

   G_Icon_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Glib.G_Icon.G_Icon
   --  The `GIcon` displayed in the GtkImage.
   --
   --  For themed icons, If the icon theme is changed, the image will be
   --  updated automatically.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the icon in the icon theme.
   --
   --  If the icon theme is changed, the image will be updated automatically.

   Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size;
   --  The symbolic size to display icons at.

   Paintable_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gdk.Paintable.Gdk_Paintable
   --  The `GdkPaintable` to display.

   Pixel_Size_Property : constant Glib.Properties.Property_Int;
   --  The size in pixels to display icons at.
   --
   --  If set to a value != -1, this property overrides the
   --  [propertyGtk.Image:icon-size] property for images of type
   --  `GTK_IMAGE_ICON_NAME`.

   Resource_Property : constant Glib.Properties.Property_String;
   --  A path to a resource file to display.

   Storage_Type_Property : constant Gtk.Enums.Property_Gtk_Image_Type;
   --  The representation being used for image data.

   Use_Fallback_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the icon displayed in the `GtkImage` will use standard icon
   --  names fallback.
   --
   --  The value of this property is only relevant for images of type
   --  Gtk.Enums.Image_Icon_Name and Gtk.Enums.Image_Gicon.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Image_Record, Gtk_Image);
   function "+"
     (Widget : access Gtk_Image_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Image
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Image_Record, Gtk_Image);
   function "+"
     (Widget : access Gtk_Image_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Image
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Fallback_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-fallback");
   Storage_Type_Property : constant Gtk.Enums.Property_Gtk_Image_Type :=
     Gtk.Enums.Build ("storage-type");
   Resource_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("resource");
   Pixel_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pixel-size");
   Paintable_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("paintable");
   Icon_Size_Property : constant Gtk.Enums.Property_Gtk_Icon_Size :=
     Gtk.Enums.Build ("icon-size");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   G_Icon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gicon");
   File_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("file");
end Gtk.Image;
