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
--  The GtkAboutDialog offers a simple way to display information about a
--  program like its logo, name, copyright, website and license. It is also
--  possible to give credits to the authors, documenters, translators and
--  artists who have worked on the program. An about dialog is typically opened
--  when the user selects the `About` option from the `Help` menu. All parts of
--  the dialog are optional.
--
--  About dialogs often contain links and email addresses. GtkAboutDialog
--  displays these as clickable links. By default, it calls
--  gtk_show_uri_on_window when a user clicks one. The behaviour can be
--  overridden with the Gtk.About_Dialog.Gtk_About_Dialog::activate-link
--  signal.
--
--  To specify a person with an email address, use a string like "Edgar Allan
--  Poe <edgar\Poe.com>". To specify a website with a title, use a string like
--  "GTK+ team http://www.gtk.org".
--
--  To make constructing a GtkAboutDialog as convenient as possible, you can
--  use the function gtk_show_about_dialog which constructs and shows a dialog
--  and keeps it around so that it can be shown again.
--
--  Note that GTK+ sets a default title of `_("About %s")` on the dialog
--  window (where \%s is replaced by the name of the application, but in order
--  to ensure proper translation of the title, applications should set the
--  title property explicitly when constructing a GtkAboutDialog, as shown in
--  the following example: |[<!-- language="C" --> GdkPixbuf *example_logo =
--  gdk_pixbuf_new_from_file ("./logo.png", NULL); gtk_show_about_dialog (NULL,
--  "program-name", "ExampleCode", "logo", example_logo, "title", _("About
--  ExampleCode"), NULL); ]|
--
--  It is also possible to show a Gtk.About_Dialog.Gtk_About_Dialog like any
--  other Gtk.Dialog.Gtk_Dialog, e.g. using Gtk.Dialog.Run. In this case, you
--  might need to know that the "Close" button returns the GTK_RESPONSE_CANCEL
--  response id.
--
--  </description>
--  <group>Windows</group>
--  <testgtk>create_about.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;            use GNAT.Strings;
with Gdk.Pixbuf;              use Gdk.Pixbuf;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Dialog;              use Gtk.Dialog;

package Gtk.About_Dialog is

   type Gtk_About_Dialog_Record is new Gtk_Dialog_Record with null record;
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
      License_Mpl_2_0);
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

   procedure Gtk_New (About : out Gtk_About_Dialog);
   procedure Initialize
      (About : not null access Gtk_About_Dialog_Record'Class);
   --  Creates a new Gtk.About_Dialog.Gtk_About_Dialog.
   --  Since: gtk+ 2.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_About_Dialog_New return Gtk_About_Dialog;
   --  Creates a new Gtk.About_Dialog.Gtk_About_Dialog.
   --  Since: gtk+ 2.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_about_dialog_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Credit_Section
      (About        : not null access Gtk_About_Dialog_Record;
       Section_Name : UTF8_String;
       People       : GNAT.Strings.String_List);
   --  Creates a new section in the Credits page.
   --  Since: gtk+ 3.4
   --  "section_name": The name of the section
   --  "people": The people who belong to that section

   function Get_Artists
      (About : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the string which are displayed in the artists tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6

   procedure Set_Artists
      (About   : not null access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the artists tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "artists": a null-terminated array of strings

   function Get_Authors
      (About : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the string which are displayed in the authors tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6

   procedure Set_Authors
      (About   : not null access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the authors tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "authors": a null-terminated array of strings

   function Get_Comments
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the comments string.
   --  Since: gtk+ 2.6

   procedure Set_Comments
      (About    : not null access Gtk_About_Dialog_Record;
       Comments : UTF8_String := "");
   --  Sets the comments string to display in the about dialog. This should be
   --  a short string of one or two lines.
   --  Since: gtk+ 2.6
   --  "comments": a comments string

   function Get_Copyright
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the copyright string.
   --  Since: gtk+ 2.6

   procedure Set_Copyright
      (About     : not null access Gtk_About_Dialog_Record;
       Copyright : UTF8_String := "");
   --  Sets the copyright string to display in the about dialog. This should
   --  be a short string of one or two lines.
   --  Since: gtk+ 2.6
   --  "copyright": the copyright string

   function Get_Documenters
      (About : not null access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   --  Returns the string which are displayed in the documenters tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6

   procedure Set_Documenters
      (About       : not null access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the documenters tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "documenters": a null-terminated array of strings

   function Get_License
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the license information.
   --  Since: gtk+ 2.6

   procedure Set_License
      (About   : not null access Gtk_About_Dialog_Record;
       License : UTF8_String := "");
   --  Sets the license information to be displayed in the secondary license
   --  dialog. If License is null, the license button is hidden.
   --  Since: gtk+ 2.6
   --  "license": the license information or null

   function Get_License_Type
      (About : not null access Gtk_About_Dialog_Record) return Gtk_License;
   --  Retrieves the license set using Gtk.About_Dialog.Set_License_Type
   --  Since: gtk+ 3.0

   procedure Set_License_Type
      (About        : not null access Gtk_About_Dialog_Record;
       License_Type : Gtk_License);
   --  Sets the license of the application showing the About dialog from a
   --  list of known licenses.
   --  This function overrides the license set using
   --  Gtk.About_Dialog.Set_License.
   --  Since: gtk+ 3.0
   --  "license_type": the type of license

   function Get_Logo
      (About : not null access Gtk_About_Dialog_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Returns the pixbuf displayed as logo in the about dialog.
   --  Since: gtk+ 2.6

   procedure Set_Logo
      (About : not null access Gtk_About_Dialog_Record;
       Logo  : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the pixbuf to be displayed as logo in the about dialog. If it is
   --  null, the default window icon set with Gtk.Window.Set_Default_Icon will
   --  be used.
   --  Since: gtk+ 2.6
   --  "logo": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   function Get_Logo_Icon_Name
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the icon name displayed as logo in the about dialog.
   --  Since: gtk+ 2.6

   procedure Set_Logo_Icon_Name
      (About     : not null access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String := "");
   --  Sets the pixbuf to be displayed as logo in the about dialog. If it is
   --  null, the default window icon set with Gtk.Window.Set_Default_Icon will
   --  be used.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name, or null

   function Get_Program_Name
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the program name displayed in the about dialog.
   --  Since: gtk+ 2.12

   procedure Set_Program_Name
      (About : not null access Gtk_About_Dialog_Record;
       Name  : UTF8_String);
   --  Sets the name to display in the about dialog. If this is not set, it
   --  defaults to g_get_application_name.
   --  Since: gtk+ 2.12
   --  "name": the program name

   function Get_Translator_Credits
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the translator credits string which is displayed in the
   --  translators tab of the secondary credits dialog.
   --  Since: gtk+ 2.6

   procedure Set_Translator_Credits
      (About              : not null access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String := "");
   --  Sets the translator credits string which is displayed in the
   --  translators tab of the secondary credits dialog.
   --  The intended use for this string is to display the translator of the
   --  language which is currently used in the user interface. Using gettext, a
   --  simple way to achieve that is to mark the string for translation: |[<!--
   --  language="C" --> GtkWidget *about = gtk_about_dialog_new ();
   --  gtk_about_dialog_set_translator_credits (GTK_ABOUT_DIALOG (about),
   --  _("translator-credits")); ]| It is a good idea to use the customary
   --  msgid "translator-credits" for this purpose, since translators will
   --  already know the purpose of that msgid, and since
   --  Gtk.About_Dialog.Gtk_About_Dialog will detect if "translator-credits" is
   --  untranslated and hide the tab.
   --  Since: gtk+ 2.6
   --  "translator_credits": the translator credits

   function Get_Version
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the version string.
   --  Since: gtk+ 2.6

   procedure Set_Version
      (About   : not null access Gtk_About_Dialog_Record;
       Version : UTF8_String := "");
   --  Sets the version string to display in the about dialog.
   --  Since: gtk+ 2.6
   --  "version": the version string

   function Get_Website
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the website URL.
   --  Since: gtk+ 2.6

   procedure Set_Website
      (About   : not null access Gtk_About_Dialog_Record;
       Website : UTF8_String := "");
   --  Sets the URL to use for the website link.
   --  Since: gtk+ 2.6
   --  "website": a URL string starting with "http://"

   function Get_Website_Label
      (About : not null access Gtk_About_Dialog_Record) return UTF8_String;
   --  Returns the label used for the website link.
   --  Since: gtk+ 2.6

   procedure Set_Website_Label
      (About         : not null access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String);
   --  Sets the label to be used for the website link.
   --  Since: gtk+ 2.6
   --  "website_label": the label used for the website link

   function Get_Wrap_License
      (About : not null access Gtk_About_Dialog_Record) return Boolean;
   --  Returns whether the license text in About is automatically wrapped.
   --  Since: gtk+ 2.8

   procedure Set_Wrap_License
      (About        : not null access Gtk_About_Dialog_Record;
       Wrap_License : Boolean);
   --  Sets whether the license text in About is automatically wrapped.
   --  Since: gtk+ 2.8
   --  "wrap_license": whether to wrap the license

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
   --  Comments about the program. This string is displayed in a label in the
   --  main dialog, thus it should be a short explanation of the main purpose
   --  of the program, not a detailed list of features.

   Copyright_Property : constant Glib.Properties.Property_String;
   --  Copyright information for the program.

   License_Property : constant Glib.Properties.Property_String;
   --  The license of the program. This string is displayed in a text view in
   --  a secondary dialog, therefore it is fine to use a long multi-paragraph
   --  text. Note that the text is only wrapped in the text view if the
   --  "wrap-license" property is set to True; otherwise the text itself must
   --  contain the intended linebreaks. When setting this property to a
   --  non-null value, the Gtk.About_Dialog.Gtk_About_Dialog:license-type
   --  property is set to Gtk.About_Dialog.License_Custom as a side effect.

   License_Type_Property : constant Gtk.About_Dialog.Property_Gtk_License;
   --  Type: Gtk_License
   --  The license of the program, as a value of the GtkLicense enumeration.
   --
   --  The Gtk.About_Dialog.Gtk_About_Dialog will automatically fill out a
   --  standard disclaimer and link the user to the appropriate online resource
   --  for the license text.
   --
   --  If Gtk.About_Dialog.License_Unknown is used, the link used will be the
   --  same specified in the Gtk.About_Dialog.Gtk_About_Dialog:website
   --  property.
   --
   --  If Gtk.About_Dialog.License_Custom is used, the current contents of the
   --  Gtk.About_Dialog.Gtk_About_Dialog:license property are used.
   --
   --  For any other Gtk.About_Dialog.Gtk_License value, the contents of the
   --  Gtk.About_Dialog.Gtk_About_Dialog:license property are also set by this
   --  property as a side effect.

   Logo_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  A logo for the about box. If it is null, the default window icon set
   --  with Gtk.Window.Set_Default_Icon will be used.

   Logo_Icon_Name_Property : constant Glib.Properties.Property_String;
   --  A named icon to use as the logo for the about box. This property
   --  overrides the Gtk.About_Dialog.Gtk_About_Dialog:logo property.

   Program_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the program. If this is not set, it defaults to
   --  g_get_application_name.

   Translator_Credits_Property : constant Glib.Properties.Property_String;
   --  Credits to the translators. This string should be marked as
   --  translatable. The string may contain email addresses and URLs, which
   --  will be displayed as links, see the introduction for more details.

   Version_Property : constant Glib.Properties.Property_String;
   --  The version of the program.

   Website_Property : constant Glib.Properties.Property_String;
   --  The URL for the link to the website of the program. This should be a
   --  string starting with "http://.

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
   --  The signal which gets emitted to activate a URI. Applications may
   --  connect to it to override the default behaviour, which is to call
   --  gtk_show_uri_on_window.
   -- 
   --  Callback parameters:
   --    --  "uri": the URI that is activated
   --    --  Returns True if the link has been activated

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

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
   Program_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("program-name");
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("logo-icon-name");
   Logo_Property : constant Glib.Properties.Property_Object :=
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
