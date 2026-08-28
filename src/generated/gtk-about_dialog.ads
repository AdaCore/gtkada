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

--  Displays information about a program.
--
--  The shown information includes the programs' logo, name, copyright,
--  website and license. It is also possible to give credits to the authors,
--  documenters, translators and artists who have worked on the program.
--
--  An about dialog is typically opened when the user selects the `About`
--  option from the `Help` menu. All parts of the dialog are optional.
--
--  <picture> <source srcset="aboutdialot-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkAboutDialog"
--  src="aboutdialog.png"> </picture>
--  About dialogs often contain links and email addresses. `GtkAboutDialog`
--  displays these as clickable links. By default, it calls
--  [methodGtk.FileLauncher.launch] when a user clicks one. The behaviour can
--  be overridden with the [signalGtk.AboutDialog::activate-link] signal.
--
--  To specify a person with an email address, use a string like `Edgar Allan
--  Poe <edgarPoe.com>`. To specify a website with a title, use a string like
--  `GTK team https://www.gtk.org`.
--
--  To make constructing an about dialog as convenient as possible, you can
--  use the function [funcGtk.show_about_dialog] which constructs and shows a
--  dialog and keeps it around so that it can be shown again.
--
--  Note that GTK sets a default title of `_("About %s")` on the dialog window
--  (where `%s` is replaced by the name of the application, but in order to
--  ensure proper translation of the title, applications should set the title
--  property explicitly when constructing an about dialog, as shown in the
--  following example:
--
--  ```c GFile *logo_file = g_file_new_for_path ("./logo.png"); GdkTexture
--  *example_logo = gdk_texture_new_from_file (logo_file, NULL); g_object_unref
--  (logo_file);
--
--  gtk_show_about_dialog (NULL, "program-name", "ExampleCode", "logo",
--  example_logo, "title", _("About ExampleCode"), NULL); ```
--
--  ## Shortcuts and Gestures
--
--  `GtkAboutDialog` supports the following keyboard shortcuts:
--
--  - <kbd>Escape</kbd> closes the window.
--
--  ## CSS nodes
--
--  `GtkAboutDialog` has a single CSS node with the name `window` and style
--  class `.aboutdialog`.

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Gdk;                     use Gdk;
with Gdk.Paintable;           use Gdk.Paintable;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Native;              use Gtk.Native;
with Gtk.Root;                use Gtk.Root;
with Gtk.Shortcut_Manager;    use Gtk.Shortcut_Manager;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

package Gtk.About_Dialog is

   type Gtk_About_Dialog_Record is new Gtk_Window_Record with null record;
   type Gtk_About_Dialog is access all Gtk_About_Dialog_Record'Class;

   type Gtk_License is (
      License_Unknown,
      License_Custom,
      License_Gpl_2_0,
      License_Gpl_3_0,
      License_Lgpl_2_1,
      License_Lgpl_3_0,
      License_Bsd,
      License_Mit_X11,
      License_Artistic,
      License_Gpl_2_0_Only,
      License_Gpl_3_0_Only,
      License_Lgpl_2_1_Only,
      License_Lgpl_3_0_Only,
      License_Agpl_3_0,
      License_Agpl_3_0_Only,
      License_Bsd_3,
      License_Apache_2_0,
      License_Mpl_2_0,
      License_0Bsd);
   pragma Convention (C, Gtk_License);
   --  The type of license for an application.
   --
   --  This enumeration can be expanded at later date.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_License_Properties is
      new Generic_Internal_Discrete_Property (Gtk_License);
   type Property_Gtk_License is new Gtk_License_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_About_Dialog);
   procedure Initialize
      (Self : not null access Gtk_About_Dialog_Record'Class);
   --  Creates a new `GtkAboutDialog`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_About_Dialog_New return Gtk_About_Dialog;
   --  Creates a new `GtkAboutDialog`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_about_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Credit_Section
      (Self         : not null access Gtk_About_Dialog_Record;
       Section_Name : UTF8_String;
       People       : GNAT.Strings.String_List);
   --  Creates a new section in the "Credits" page.
   --  @param Section_Name The name of the section
   --  @param People the people who belong to that section

   function Get_Artists
      (Self : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the names of the artists which are displayed in the credits
   --  page.
   --  @return A `NULL`-terminated string array containing the artists

   procedure Set_Artists
      (Self    : not null access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List);
   --  Sets the names of the artists to be displayed in the "Credits" page.
   --  @param Artists the authors of the artwork of the application

   function Get_Authors
      (Self : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the names of the authors which are displayed in the credits
   --  page.
   --  @return A `NULL`-terminated string array containing the authors

   procedure Set_Authors
      (Self    : not null access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List);
   --  Sets the names of the authors which are displayed in the "Credits" page
   --  of the about dialog.
   --  @param Authors the authors of the application

   function Get_Comments
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the comments string.
   --  @return The comments

   procedure Set_Comments
      (Self     : not null access Gtk_About_Dialog_Record;
       Comments : UTF8_String := "");
   --  Sets the comments string to display in the about dialog.
   --  This should be a short string of one or two lines.
   --  @param Comments a comments string

   function Get_Copyright
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the copyright string.
   --  @return The copyright string

   procedure Set_Copyright
      (Self      : not null access Gtk_About_Dialog_Record;
       Copyright : UTF8_String := "");
   --  Sets the copyright string to display in the about dialog.
   --  This should be a short string of one or two lines.
   --  @param Copyright the copyright string

   function Get_Documenters
      (Self : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the name of the documenters which are displayed in the credits
   --  page.
   --  @return A `NULL`-terminated string array containing the documenters

   procedure Set_Documenters
      (Self        : not null access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List);
   --  Sets the names of the documenters which are displayed in the "Credits"
   --  page.
   --  @param Documenters the authors of the documentation of the application

   function Get_License
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the license information.
   --  @return The license information

   procedure Set_License
      (Self    : not null access Gtk_About_Dialog_Record;
       License : UTF8_String := "");
   --  Sets the license information to be displayed in the about dialog.
   --  If `license` is `NULL`, the license page is hidden.
   --  @param License the license information

   function Get_License_Type
      (Self : not null access Gtk_About_Dialog_Record) return Gtk_License;
   --  Retrieves the license type.
   --  @return a [enumGtk.License] value

   procedure Set_License_Type
      (Self         : not null access Gtk_About_Dialog_Record;
       License_Type : Gtk_License);
   --  Sets the license of the application showing the about dialog from a
   --  list of known licenses.
   --  This function overrides the license set using
   --  [methodGtk.AboutDialog.set_license].
   --  @param License_Type the type of license

   function Get_Logo
      (Self : not null access Gtk_About_Dialog_Record)
       return Gdk.Paintable.Gdk_Paintable;
   --  Returns the paintable displayed as logo in the about dialog.
   --  @return the paintable displayed as logo or `NULL` if the logo is unset
   --  or has been set via [methodGtk.AboutDialog.set_logo_icon_name]

   procedure Set_Logo
      (Self : not null access Gtk_About_Dialog_Record;
       Logo : Gdk.Paintable.Gdk_Paintable);
   --  Sets the logo in the about dialog.
   --  @param Logo a `GdkPaintable`

   function Get_Logo_Icon_Name
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the icon name displayed as logo in the about dialog.
   --  @return the icon name displayed as logo, or `NULL` if the logo has been
   --  set via [methodGtk.AboutDialog.set_logo]

   procedure Set_Logo_Icon_Name
      (Self      : not null access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String := "");
   --  Sets the icon name to be displayed as logo in the about dialog.
   --  @param Icon_Name an icon name

   function Get_Program_Name
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the program name displayed in the about dialog.
   --  @return the program name

   procedure Set_Program_Name
      (Self : not null access Gtk_About_Dialog_Record;
       Name : UTF8_String := "");
   --  Sets the name to display in the about dialog.
   --  If `name` is not set, the string returned by `g_get_application_name`
   --  is used.
   --  @param Name the program name

   function Get_System_Information
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the system information that is shown in the about dialog.
   --  @return the system information

   procedure Set_System_Information
      (Self               : not null access Gtk_About_Dialog_Record;
       System_Information : UTF8_String := "");
   --  Sets the system information to be displayed in the about dialog.
   --  If `system_information` is `NULL`, the system information page is
   --  hidden.
   --  See [propertyGtk.AboutDialog:system-information].
   --  @param System_Information system information

   function Get_Translator_Credits
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the translator credits string which is displayed in the credits
   --  page.
   --  @return The translator credits string

   procedure Set_Translator_Credits
      (Self               : not null access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String := "");
   --  Sets the translator credits string which is displayed in the credits
   --  page.
   --  The intended use for this string is to display the translator of the
   --  language which is currently used in the user interface. Using `gettext`,
   --  a simple way to achieve that is to mark the string for translation:
   --  ```c GtkWidget *about = gtk_about_dialog_new ();
   --  gtk_about_dialog_set_translator_credits (GTK_ABOUT_DIALOG (about),
   --  _("translator-credits")); ```
   --  It is a good idea to use the customary `msgid` "translator-credits" for
   --  this purpose, since translators will already know the purpose of that
   --  `msgid`, and since `GtkAboutDialog` will detect if "translator-credits"
   --  is untranslated and omit translator credits.
   --  @param Translator_Credits the translator credits

   function Get_Version
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the version string.
   --  @return The version string

   procedure Set_Version
      (Self    : not null access Gtk_About_Dialog_Record;
       Version : UTF8_String := "");
   --  Sets the version string to display in the about dialog.
   --  @param Version the version string

   function Get_Website
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the website URL.
   --  @return The website URL

   procedure Set_Website
      (Self    : not null access Gtk_About_Dialog_Record;
       Website : UTF8_String := "");
   --  Sets the URL to use for the website link.
   --  @param Website a URL string starting with `http://`

   function Get_Website_Label
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the label used for the website link.
   --  @return The label used for the website link

   procedure Set_Website_Label
      (Self          : not null access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String);
   --  Sets the label to be used for the website link.
   --  @param Website_Label the label used for the website link

   function Get_Wrap_License
      (Self : not null access Gtk_About_Dialog_Record) return Boolean;
   --  Returns whether the license text in the about dialog is automatically
   --  wrapped.
   --  @return `TRUE` if the license text is wrapped

   procedure Set_Wrap_License
      (Self         : not null access Gtk_About_Dialog_Record;
       Wrap_License : Boolean);
   --  Sets whether the license text in the about dialog should be
   --  automatically wrapped.
   --  @param Wrap_License whether to wrap the license

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_About_Dialog_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_About_Dialog_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_About_Dialog_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_About_Dialog_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_About_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_About_Dialog_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_About_Dialog_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_About_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_About_Dialog_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_About_Dialog_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_About_Dialog_Record)
       return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_About_Dialog_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_About_Dialog_Record);

   procedure Unrealize (Self : not null access Gtk_About_Dialog_Record);

   function Get_Display
      (Self : not null access Gtk_About_Dialog_Record)
       return Gdk.Gdk_Display;

   function Get_Focus
      (Self : not null access Gtk_About_Dialog_Record)
       return Gtk.Widget.Gtk_Widget;

   procedure Set_Focus
      (Self  : not null access Gtk_About_Dialog_Record;
       Focus : access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Artists_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("artists");--  Unknown type: unspecified

   Authors_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("authors");--  Unknown type: unspecified

   Documenters_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("documenters");--  Unknown type: unspecified

   Comments_Property : constant Glib.Properties.Property_String;
   --  Comments about the program.
   --
   --  This string is displayed in a label in the main dialog, thus it should
   --  be a short explanation of the main purpose of the program, not a
   --  detailed list of features.

   Copyright_Property : constant Glib.Properties.Property_String;
   --  Copyright information for the program.

   License_Property : constant Glib.Properties.Property_String;
   --  The license of the program, as free-form text.
   --
   --  This string is displayed in a text view in a secondary dialog,
   --  therefore it is fine to use a long multi-paragraph text. Note that the
   --  text is only wrapped in the text view if the "wrap-license" property is
   --  set to `TRUE`; otherwise the text itself must contain the intended
   --  linebreaks.
   --
   --  When setting this property to a non-`NULL` value, the
   --  [propertyGtk.AboutDialog:license-type] property is set to
   --  [enumGtk.License.custom] as a side effect.
   --
   --  The text may contain links in this format `<http://www.some.place/>`
   --  and email references in the form `<mail-toSome.body>`, and these will be
   --  converted into clickable links.

   License_Type_Property : constant Gtk.About_Dialog.Property_Gtk_License;
   --  Type: Gtk_License
   --  The license of the program.
   --
   --  The `GtkAboutDialog` will automatically fill out a standard disclaimer
   --  and link the user to the appropriate online resource for the license
   --  text.
   --
   --  If [enumGtk.License.unknown] is used, the link used will be the same
   --  specified in the [propertyGtk.AboutDialog:website] property.
   --
   --  If [enumGtk.License.custom] is used, the current contents of the
   --  [propertyGtk.AboutDialog:license] property are used.
   --
   --  For any other [enumGtk.License] value, the contents of the
   --  [propertyGtk.AboutDialog:license] property are also set by this property
   --  as a side effect.

   Logo_Property : constant Glib.Properties.Property_Interface;
   --  Type: Gdk.Paintable.Gdk_Paintable
   --  A logo for the about box.
   --
   --  If it is `NULL`, the default window icon set with
   --  [funcGtk.Window.set_default_icon_name] will be used.

   Logo_Icon_Name_Property : constant Glib.Properties.Property_String;
   --  A named icon to use as the logo for the about box.
   --
   --  This property overrides the [propertyGtk.AboutDialog:logo] property.

   Program_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the program.
   --
   --  If this is not set, it defaults to the value returned by
   --  [funcGlib.get_application_name].

   System_Information_Property : constant Glib.Properties.Property_String;
   --  Information about the system on which the program is running.
   --
   --  This information is displayed in a separate page, therefore it is fine
   --  to use a long multi-paragraph text. Note that the text should contain
   --  the intended linebreaks.
   --
   --  The text may contain links in this format `<http://www.some.place/>`
   --  and email references in the form `<mail-toSome.body>`, and these will be
   --  converted into clickable links.

   Translator_Credits_Property : constant Glib.Properties.Property_String;
   --  Credits to the translators.
   --
   --  This string should be marked as translatable.
   --
   --  The string may contain email addresses and URLs, which will be
   --  displayed as links, see the introduction for more details.

   Version_Property : constant Glib.Properties.Property_String;
   --  The version of the program.

   Website_Property : constant Glib.Properties.Property_String;
   --  The URL for the link to the website of the program.
   --
   --  This should be a string starting with `http://` or `https://`.

   Website_Label_Property : constant Glib.Properties.Property_String;
   --  The label for the link to the website of the program.

   Wrap_License_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to wrap the text in the license dialog.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_About_Dialog_UTF8_String_Boolean is not null access function
     (Self : access Gtk_About_Dialog_Record'Class;
      URI  : UTF8_String) return Boolean;

   type Cb_GObject_UTF8_String_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class;
      URI  : UTF8_String) return Boolean;

   Signal_Activate_Link : constant Glib.Signal_Name := "activate-link";
   procedure On_Activate_Link
      (Self  : not null access Gtk_About_Dialog_Record;
       Call  : Cb_Gtk_About_Dialog_UTF8_String_Boolean;
       After : Boolean := False);
   procedure On_Activate_Link
      (Self  : not null access Gtk_About_Dialog_Record;
       Call  : Cb_GObject_UTF8_String_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted every time a URL is activated.
   --
   --  Applications may connect to it to override the default behaviour, which
   --  is to call [methodGtk.FileLauncher.launch].
   -- 
   --  Callback parameters:
   --    --  @param URI the URI that is activated

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
   --
   --  - "Gtk.Native"
   --
   --  - "Gtk.Root"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_About_Dialog
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_About_Dialog
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_About_Dialog
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_About_Dialog
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Root is new Glib.Types.Implements
     (Gtk.Root.Gtk_Root, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Root.Gtk_Root
   renames Implements_Gtk_Root.To_Interface;
   function "-"
     (Interf : Gtk.Root.Gtk_Root)
   return Gtk_About_Dialog
   renames Implements_Gtk_Root.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_About_Dialog
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Wrap_License_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap-license");
   Website_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("website-label");
   Website_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("website");
   Version_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("version");
   Translator_Credits_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("translator-credits");
   System_Information_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("system-information");
   Program_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("program-name");
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("logo-icon-name");
   Logo_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("logo");
   License_Type_Property : constant Gtk.About_Dialog.Property_Gtk_License :=
     Gtk.About_Dialog.Build ("license-type");
   License_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("license");
   Copyright_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("copyright");
   Comments_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("comments");
end Gtk.About_Dialog;
