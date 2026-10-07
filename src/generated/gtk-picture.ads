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

--  Displays a `GdkPaintable`.
--
--  <picture> <source srcset="picture-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkPicture" src="picture.png"> </picture>
--  Many convenience functions are provided to make pictures simple to use.
--  For example, if you want to load an image from a file, and then display it,
--  there's a convenience function to do this:
--
--  ```c GtkWidget *widget = gtk_picture_new_for_filename ("myfile.png"); ```
--
--  If the file isn't loaded successfully, the picture will contain a "broken
--  image" icon similar to that used in many web browsers. If you want to
--  handle errors in loading the file yourself, for example by displaying an
--  error message, then load the image with and image loading framework such as
--  libglycin, then create the `GtkPicture` with
--  [ctorGtk.Picture.new_for_paintable].
--
--  Sometimes an application will want to avoid depending on external data
--  files, such as image files. See the documentation of `GResource` for
--  details. In this case, [ctorGtk.Picture.new_for_resource] and
--  [methodGtk.Picture.set_resource] should be used.
--
--  `GtkPicture` displays an image at its natural size. See [classGtk.Image]
--  if you want to display a fixed-size image, such as an icon.
--
--  ## Sizing the paintable
--
--  You can influence how the paintable is displayed inside the `GtkPicture`
--  by changing [propertyGtk.Picture:content-fit]. See [enumGtk.ContentFit] for
--  details. [propertyGtk.Picture:can-shrink] can be unset to make sure that
--  paintables are never made smaller than their ideal size - but be careful if
--  you do not know the size of the paintable in use (like when displaying
--  user-loaded images). This can easily cause the picture to grow larger than
--  the screen. And [propertyGtk.Widget:halign] and [propertyGtk.Widget:valign]
--  can be used to make sure the paintable doesn't fill all available space but
--  is instead displayed at its original size.
--
--  ## CSS nodes
--
--  `GtkPicture` has a single CSS node with the name `picture`.
--
--  ## Accessibility
--
--  `GtkPicture` uses the [enumGtk.AccessibleRole.img] role.
--
--  <group>Display Widgets</group>
--  <gtkada_demo>create_pixbuf.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Paintable;           use Gdk.Paintable;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Glib;                    use Glib;
with Glib.GFile;              use Glib.GFile;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Picture is

   type Gtk_Picture_Record is new Gtk_Widget_Record with null record;
   type Gtk_Picture is access all Gtk_Picture_Record'Class;

   type Gtk_Content_Fit is (
      Content_Fit_Fill,
      Content_Fit_Contain,
      Content_Fit_Cover,
      Content_Fit_Scale_Down);
   pragma Convention (C, Gtk_Content_Fit);
   --  Controls how a content should be made to fit inside an allocation.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Content_Fit_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Content_Fit);
   type Property_Gtk_Content_Fit is new Gtk_Content_Fit_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Picture);
   procedure Initialize (Self : not null access Gtk_Picture_Record'Class);
   --  Creates a new empty `GtkPicture` widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Picture_New return Gtk_Picture;
   --  Creates a new empty `GtkPicture` widget.

   procedure Gtk_New_For_File
      (Self : out Gtk_Picture;
       File : Glib.GFile.Gfile);
   procedure Initialize_For_File
      (Self : not null access Gtk_Picture_Record'Class;
       File : Glib.GFile.Gfile);
   --  Creates a new `GtkPicture` displaying the given File.
   --  If the file isn't found or can't be loaded, the resulting `GtkPicture`
   --  is empty.
   --  If you need to detect failures to load the file, use an image loading
   --  framework such as libglycin to load the file yourself, then create the
   --  `GtkPicture` from the texture.
   --  Initialize_For_File does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param File a `GFile`

   function Gtk_Picture_New_For_File
      (File : Glib.GFile.Gfile) return Gtk_Picture;
   --  Creates a new `GtkPicture` displaying the given File.
   --  If the file isn't found or can't be loaded, the resulting `GtkPicture`
   --  is empty.
   --  If you need to detect failures to load the file, use an image loading
   --  framework such as libglycin to load the file yourself, then create the
   --  `GtkPicture` from the texture.
   --  @param File a `GFile`

   procedure Gtk_New_For_Filename
      (Self     : out Gtk_Picture;
       Filename : UTF8_String := "");
   procedure Initialize_For_Filename
      (Self     : not null access Gtk_Picture_Record'Class;
       Filename : UTF8_String := "");
   --  Creates a new `GtkPicture` displaying the file Filename.
   --  This is a utility function that calls [ctorGtk.Picture.new_for_file].
   --  See that function for details.
   --  Initialize_For_Filename does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Filename a filename

   function Gtk_Picture_New_For_Filename
      (Filename : UTF8_String := "") return Gtk_Picture;
   --  Creates a new `GtkPicture` displaying the file Filename.
   --  This is a utility function that calls [ctorGtk.Picture.new_for_file].
   --  See that function for details.
   --  @param Filename a filename

   procedure Gtk_New_For_Paintable
      (Self      : out Gtk_Picture;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   procedure Initialize_For_Paintable
      (Self      : not null access Gtk_Picture_Record'Class;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Creates a new `GtkPicture` displaying Paintable.
   --  The `GtkPicture` will track changes to the Paintable and update its
   --  size and contents in response to it.
   --  Initialize_For_Paintable does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Paintable a `GdkPaintable`

   function Gtk_Picture_New_For_Paintable
      (Paintable : Gdk.Paintable.Gdk_Paintable) return Gtk_Picture;
   --  Creates a new `GtkPicture` displaying Paintable.
   --  The `GtkPicture` will track changes to the Paintable and update its
   --  size and contents in response to it.
   --  @param Paintable a `GdkPaintable`

   procedure Gtk_New_For_Pixbuf
      (Self   : out Gtk_Picture;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   procedure Initialize_For_Pixbuf
      (Self   : not null access Gtk_Picture_Record'Class;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Creates a new `GtkPicture` displaying Pixbuf.
   --  This is a utility function that calls
   --  [ctorGtk.Picture.new_for_paintable], See that function for details.
   --  The pixbuf must not be modified after passing it to this function.
   --  Initialize_For_Pixbuf does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Pixbuf a `GdkPixbuf`

   function Gtk_Picture_New_For_Pixbuf
      (Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Picture;
   --  Creates a new `GtkPicture` displaying Pixbuf.
   --  This is a utility function that calls
   --  [ctorGtk.Picture.new_for_paintable], See that function for details.
   --  The pixbuf must not be modified after passing it to this function.
   --  @param Pixbuf a `GdkPixbuf`

   procedure Gtk_New_For_Resource
      (Self          : out Gtk_Picture;
       Resource_Path : UTF8_String := "");
   procedure Initialize_For_Resource
      (Self          : not null access Gtk_Picture_Record'Class;
       Resource_Path : UTF8_String := "");
   --  Creates a new `GtkPicture` displaying the resource at Resource_Path.
   --  This is a utility function that calls [ctorGtk.Picture.new_for_file].
   --  See that function for details.
   --  Initialize_For_Resource does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Resource_Path resource path to play back

   function Gtk_Picture_New_For_Resource
      (Resource_Path : UTF8_String := "") return Gtk_Picture;
   --  Creates a new `GtkPicture` displaying the resource at Resource_Path.
   --  This is a utility function that calls [ctorGtk.Picture.new_for_file].
   --  See that function for details.
   --  @param Resource_Path resource path to play back

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_picture_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Alternative_Text
      (Self : not null access Gtk_Picture_Record) return UTF8_String;
   --  Gets the alternative textual description of the picture.
   --  The returned string will be null if the picture cannot be described
   --  textually.
   --  @return the alternative textual description of Self.

   procedure Set_Alternative_Text
      (Self             : not null access Gtk_Picture_Record;
       Alternative_Text : UTF8_String := "");
   --  Sets an alternative textual description for the picture contents.
   --  It is equivalent to the "alt" attribute for images on websites.
   --  This text will be made available to accessibility tools.
   --  If the picture cannot be described textually, set this property to
   --  null.
   --  @param Alternative_Text a textual description of the contents

   function Get_Can_Shrink
      (Self : not null access Gtk_Picture_Record) return Boolean;
   --  Returns whether the `GtkPicture` respects its contents size.
   --  @return True if the picture can be made smaller than its contents

   procedure Set_Can_Shrink
      (Self       : not null access Gtk_Picture_Record;
       Can_Shrink : Boolean);
   --  If set to True, then Self can be made smaller than its contents.
   --  The contents will then be scaled down when rendering.
   --  If you want to still force a minimum size manually, consider using
   --  [methodGtk.Widget.set_size_request].
   --  Also of note is that a similar function for growing does not exist
   --  because the grow behavior can be controlled via
   --  [methodGtk.Widget.set_halign] and [methodGtk.Widget.set_valign].
   --  @param Can_Shrink if Self can be made smaller than its contents

   function Get_Content_Fit
      (Self : not null access Gtk_Picture_Record) return Gtk_Content_Fit;
   --  Returns the fit mode for the content of the `GtkPicture`.
   --  See [enumGtk.ContentFit] for details.
   --  Since: gtk+ 4.8
   --  @return the content fit mode

   procedure Set_Content_Fit
      (Self        : not null access Gtk_Picture_Record;
       Content_Fit : Gtk_Content_Fit);
   --  Sets how the content should be resized to fit the `GtkPicture`.
   --  See [enumGtk.ContentFit] for details.
   --  Since: gtk+ 4.8
   --  @param Content_Fit the content fit mode

   function Get_File
      (Self : not null access Gtk_Picture_Record) return Glib.GFile.Gfile;
   --  Gets the `GFile` currently displayed if Self is displaying a file.
   --  If Self is not displaying a file, for example when
   --  [methodGtk.Picture.set_paintable] was used, then null is returned.
   --  @return The `GFile` displayed by Self.

   procedure Set_File
      (Self : not null access Gtk_Picture_Record;
       File : Glib.GFile.Gfile);
   --  Makes Self load and display File.
   --  See [ctorGtk.Picture.new_for_file] for details.
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`, and then use
   --  [methodGtk.Image.set_from_paintable].
   --  @param File a `GFile`

   function Get_Isolate_Contents
      (Self : not null access Gtk_Picture_Record) return Boolean;
   --  Returns whether the contents are isolated.
   --  Since: gtk+ 4.22
   --  @return True if contents are isolated

   procedure Set_Isolate_Contents
      (Self             : not null access Gtk_Picture_Record;
       Isolate_Contents : Boolean);
   --  If set to true, then the contents will be rendered individually.
   --  If set to false they will be able to erase or otherwise mix with the
   --  background.
   --  GTK supports finer grained isolation, in rare cases where you need
   --  this, you can use [methodGtk.Snapshot.push_isolation] yourself to
   --  achieve this.
   --  By default contents are isolated.
   --  Since: gtk+ 4.22
   --  @param Isolate_Contents if contents are rendered separately

   function Get_Keep_Aspect_Ratio
      (Self : not null access Gtk_Picture_Record) return Boolean;
   pragma Obsolescent (Get_Keep_Aspect_Ratio);
   --  Returns whether the `GtkPicture` preserves its contents aspect ratio.
   --  Deprecated since 4.8, 1
   --  @return True if the self tries to keep the contents' aspect ratio

   procedure Set_Keep_Aspect_Ratio
      (Self              : not null access Gtk_Picture_Record;
       Keep_Aspect_Ratio : Boolean);
   pragma Obsolescent (Set_Keep_Aspect_Ratio);
   --  If set to True, the Self will render its contents according to their
   --  aspect ratio.
   --  That means that empty space may show up at the top/bottom or left/right
   --  of Self.
   --  If set to False or if the contents provide no aspect ratio, the
   --  contents will be stretched over the picture's whole area.
   --  Deprecated since 4.8, 1
   --  @param Keep_Aspect_Ratio whether to keep aspect ratio

   function Get_Paintable
      (Self : not null access Gtk_Picture_Record)
       return Gdk.Paintable.Gdk_Paintable;
   --  Gets the `GdkPaintable` being displayed by the `GtkPicture`.
   --  @return the displayed paintable

   procedure Set_Paintable
      (Self      : not null access Gtk_Picture_Record;
       Paintable : Gdk.Paintable.Gdk_Paintable);
   --  Makes Self display the given Paintable.
   --  If Paintable is `NULL`, nothing will be displayed.
   --  See [ctorGtk.Picture.new_for_paintable] for details.
   --  @param Paintable a `GdkPaintable`

   procedure Set_Filename
      (Self     : not null access Gtk_Picture_Record;
       Filename : UTF8_String := "");
   --  Makes Self load and display the given Filename.
   --  This is a utility function that calls [methodGtk.Picture.set_file].
   --  ::: warning Note that this function should not be used with untrusted
   --  data. Use a proper image loading framework such as libglycin, which can
   --  load many image formats into a `GdkTexture`, and then use
   --  [methodGtk.Image.set_from_paintable].
   --  @param Filename the filename to play

   procedure Set_Pixbuf
      (Self   : not null access Gtk_Picture_Record;
       Pixbuf : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   pragma Obsolescent (Set_Pixbuf);
   --  Sets a `GtkPicture` to show a `GdkPixbuf`.
   --  See [ctorGtk.Picture.new_for_pixbuf] for details.
   --  This is a utility function that calls
   --  [methodGtk.Picture.set_paintable].
   --  Deprecated since 4.12, 1
   --  @param Pixbuf a `GdkPixbuf`

   procedure Set_Resource
      (Self          : not null access Gtk_Picture_Record;
       Resource_Path : UTF8_String := "");
   --  Makes Self load and display the resource at the given Resource_Path.
   --  This is a utility function that calls [methodGtk.Picture.set_file].
   --  @param Resource_Path the resource to set

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Picture_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Picture_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Picture_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Picture_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Picture_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Picture_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Picture_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Picture_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Picture_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Picture_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Picture_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Picture_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Picture_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Picture_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Picture_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Alternative_Text_Property : constant Glib.Properties.Property_String;
   --  The alternative textual description for the picture.

   Can_Shrink_Property : constant Glib.Properties.Property_Boolean;
   --  If the `GtkPicture` can be made smaller than the natural size of its
   --  contents.

   Content_Fit_Property : constant Gtk.Picture.Property_Gtk_Content_Fit;
   --  Type: Gtk_Content_Fit
   --  How the content should be resized to fit inside the `GtkPicture`.

   File_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.GFile.Gfile
   --  The `GFile` that is displayed or null if none.

   Isolate_Contents_Property : constant Glib.Properties.Property_Boolean;
   --  If the rendering of the contents is isolated from the rest of the
   --  widget tree.

   Keep_Aspect_Ratio_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the GtkPicture will render its contents trying to preserve the
   --  aspect ratio.

   Paintable_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gdk.Paintable.Gdk_Paintable
   --  The `GdkPaintable` to be displayed by this `GtkPicture`.

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
     (Gtk.Accessible.Gtk_Accessible, Gtk_Picture_Record, Gtk_Picture);
   function "+"
     (Widget : access Gtk_Picture_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Picture
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Picture_Record, Gtk_Picture);
   function "+"
     (Widget : access Gtk_Picture_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Picture
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Picture_Record, Gtk_Picture);
   function "+"
     (Widget : access Gtk_Picture_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Picture
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Paintable_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("paintable");
   Keep_Aspect_Ratio_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("keep-aspect-ratio");
   Isolate_Contents_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("isolate-contents");
   File_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("file");
   Content_Fit_Property : constant Gtk.Picture.Property_Gtk_Content_Fit :=
     Gtk.Picture.Build ("content-fit");
   Can_Shrink_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("can-shrink");
   Alternative_Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("alternative-text");
end Gtk.Picture;
