------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  when the user selects the 'About' option from the 'Help' menu. All parts of
--  the dialog are optional.
--
--  About dialog often contain links and email addresses. GtkAboutDialog
--  displays these as clickable links. By default, it calls gtk_show_uri() when
--  a user clicks one. The behaviour can be overridden with the
--  Gtk.About_Dialog.Gtk_About_Dialog::activate-link signal.
--
--  To make constructing a GtkAboutDialog as convenient as possible, you can
--  use the function gtk_show_about_dialog which constructs and shows a dialog
--  and keeps it around so that it can be shown again.
--
--  Note that GTK+ sets a default title of '_("About %s")' on the dialog
--  window (where %s is replaced by the name of the application, but in order
--  to ensure proper translation of the title, applications should set the
--  title property explicitly when constructing a GtkAboutDialog, as shown in
--  the following example:
--

--     gtk_show_about_dialog (NULL,
--        "program-name", "ExampleCode",
--        "logo", example_logo,
--        "title" _("About ExampleCode"),
--        NULL);
--
--  It is also possible to show a Gtk.About_Dialog.Gtk_About_Dialog like any
--  other Gtk.Dialog.Gtk_Dialog, e.g. using Gtk.Dialog.Run. In this case, you
--  might need to know that the 'Close' button returns the GTK_RESPONSE_CANCEL
--  response id.
--
--  </description>
--  <group>Windows</group>
--  <testgtk>create_about.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with GNAT.Strings;    use GNAT.Strings;
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Dialog;      use Gtk.Dialog;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.About_Dialog is

   type Gtk_About_Dialog_Record is new Gtk_Dialog_Record with null record;
   type Gtk_About_Dialog is access all Gtk_About_Dialog_Record'Class;

   type Gtk_License is (
      License_Unknown,
      License_Custom,
      License_GPL_2_0,
      License_GPL_3_0,
      License_LGPL_2_1,
      License_LGPL_3_0,
      License_BSD,
      License_MIT_X11,
      License_Artistic);

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (About : out Gtk_About_Dialog);
   procedure Initialize (About : access Gtk_About_Dialog_Record'Class);
   --  Creates a new Gtk.About_Dialog.Gtk_About_Dialog.
   --  Since: gtk+ 2.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_about_dialog_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Artists
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   procedure Set_Artists
      (About   : access Gtk_About_Dialog_Record;
       Artists : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the artists tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "artists": a null-terminated array of strings

   function Get_Authors
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   procedure Set_Authors
      (About   : access Gtk_About_Dialog_Record;
       Authors : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the authors tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "authors": a null-terminated array of strings

   function Get_Comments
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Comments
      (About    : access Gtk_About_Dialog_Record;
       Comments : UTF8_String);
   --  Sets the comments string to display in the about dialog. This should be
   --  a short string of one or two lines.
   --  Since: gtk+ 2.6
   --  "comments": a comments string

   function Get_Copyright
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Copyright
      (About     : access Gtk_About_Dialog_Record;
       Copyright : UTF8_String);
   --  Sets the copyright string to display in the about dialog. This should
   --  be a short string of one or two lines.
   --  Since: gtk+ 2.6
   --  "copyright": (allow-none) the copyright string

   function Get_Documenters
      (About : access Gtk_About_Dialog_Record)
       return GNAT.Strings.String_List;
   procedure Set_Documenters
      (About       : access Gtk_About_Dialog_Record;
       Documenters : GNAT.Strings.String_List);
   --  Sets the strings which are displayed in the documenters tab of the
   --  secondary credits dialog.
   --  Since: gtk+ 2.6
   --  "documenters": a null-terminated array of strings

   function Get_License
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_License
      (About   : access Gtk_About_Dialog_Record;
       License : UTF8_String);
   --  Sets the license information to be displayed in the secondary license
   --  dialog. If License is null, the license button is hidden.
   --  Since: gtk+ 2.6
   --  "license": the license information or null

   function Get_License_Type
      (About : access Gtk_About_Dialog_Record) return Gtk_License;
   procedure Set_License_Type
      (About        : access Gtk_About_Dialog_Record;
       License_Type : Gtk_License);
   --  Sets the license of the application showing the About dialog from a
   --  list of known licenses.
   --  This function overrides the license set using
   --  Gtk.About_Dialog.Set_License.
   --  Since: gtk+ 3.0
   --  "license_type": the type of license

   function Get_Logo
      (About : access Gtk_About_Dialog_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   procedure Set_Logo
      (About : access Gtk_About_Dialog_Record;
       Logo  : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Sets the pixbuf to be displayed as logo in the about dialog. If it is
   --  null, the default window icon set with Gtk.Window.Set_Default_Icon will
   --  be used.
   --  Since: gtk+ 2.6
   --  "logo": a Gdk.Pixbuf.Gdk_Pixbuf, or null

   function Get_Logo_Icon_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Logo_Icon_Name
      (About     : access Gtk_About_Dialog_Record;
       Icon_Name : UTF8_String);
   --  Sets the pixbuf to be displayed as logo in the about dialog. If it is
   --  null, the default window icon set with Gtk.Window.Set_Default_Icon will
   --  be used.
   --  Since: gtk+ 2.6
   --  "icon_name": an icon name, or null

   function Get_Program_Name
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Program_Name
      (About : access Gtk_About_Dialog_Record;
       Name  : UTF8_String);
   --  Sets the name to display in the about dialog. If this is not set, it
   --  defaults to g_get_application_name.
   --  Since: gtk+ 2.12
   --  "name": the program name

   function Get_Translator_Credits
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Translator_Credits
      (About              : access Gtk_About_Dialog_Record;
       Translator_Credits : UTF8_String);
   --  Sets the translator credits string which is displayed in the
   --  translators tab of the secondary credits dialog.
   --  The intended use for this string is to display the translator of the
   --  language which is currently used in the user interface. Using gettext, a
   --  simple way to achieve that is to mark the string for translation: |[
   --  gtk_about_dialog_set_translator_credits (about,
   --  _("translator-credits")); ]| It is a good idea to use the customary
   --  msgid "translator-credits" for this purpose, since translators will
   --  already know the purpose of that msgid, and since
   --  Gtk.About_Dialog.Gtk_About_Dialog will detect if "translator-credits" is
   --  untranslated and hide the tab.
   --  Since: gtk+ 2.6
   --  "translator_credits": the translator credits

   function Get_Version
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Version
      (About   : access Gtk_About_Dialog_Record;
       Version : UTF8_String);
   --  Sets the version string to display in the about dialog.
   --  Since: gtk+ 2.6
   --  "version": the version string

   function Get_Website
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Website
      (About   : access Gtk_About_Dialog_Record;
       Website : UTF8_String);
   --  Sets the URL to use for the website link.
   --  Since: gtk+ 2.6
   --  "website": a URL string starting with "http://"

   function Get_Website_Label
      (About : access Gtk_About_Dialog_Record) return UTF8_String;
   procedure Set_Website_Label
      (About         : access Gtk_About_Dialog_Record;
       Website_Label : UTF8_String);
   --  Sets the label to be used for the website link.
   --  Since: gtk+ 2.6
   --  "website_label": the label used for the website link

   function Get_Wrap_License
      (About : access Gtk_About_Dialog_Record) return Boolean;
   procedure Set_Wrap_License
      (About        : access Gtk_About_Dialog_Record;
       Wrap_License : Boolean);
   --  Sets whether the license text in About is automatically wrapped.
   --  Since: gtk+ 2.8
   --  "wrap_license": whether to wrap the license

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_About_Dialog_Record, Gtk_About_Dialog);
   function "+"
     (Widget : access Gtk_About_Dialog_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_About_Dialog
   renames Implements_Buildable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Comments_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Comments about the program. This string is displayed in a label in the
   --  main dialog, thus it should be a short explanation of the main purpose
   --  of the program, not a detailed list of features.
   --
   --  Name: Copyright_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Copyright information for the program.
   --
   --  Name: License_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The license of the program. This string is displayed in a text view in
   --  a secondary dialog, therefore it is fine to use a long multi-paragraph
   --  text. Note that the text is only wrapped in the text view if the
   --  "wrap-license" property is set to True; otherwise the text itself must
   --  contain the intended linebreaks. When setting this property to a
   --  non-null value, the Gtk.About_Dialog.Gtk_About_Dialog:license-type
   --  property is set to %GTK_LICENSE_CUSTOM as a side effect.
   --
   --  Name: License_Type_Property
   --  Type: Gtk_License
   --  Flags: read-write
   --  The license of the program, as a value of the %GtkLicense enumeration.
   --  The Gtk.About_Dialog.Gtk_About_Dialog will automatically fill out a
   --  standard disclaimer and link the user to the appropriate online resource
   --  for the license text.
   --  If %GTK_LICENSE_UNKNOWN is used, the link used will be the same
   --  specified in the Gtk.About_Dialog.Gtk_About_Dialog:website property.
   --  If %GTK_LICENSE_CUSTOM is used, the current contents of the
   --  Gtk.About_Dialog.Gtk_About_Dialog:license property are used.
   --  For any other Gtk_License value, the contents of the
   --  Gtk.About_Dialog.Gtk_About_Dialog:license property are also set by this
   --  property as a side effect.
   --
   --  Name: Logo_Property
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf
   --  Flags: read-write
   --  A logo for the about box. If this is not set, it defaults to
   --  Gtk.Window.Get_Default_Icon_List.
   --
   --  Name: Logo_Icon_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  A named icon to use as the logo for the about box. This property
   --  overrides the Gtk.About_Dialog.Gtk_About_Dialog:logo property.
   --
   --  Name: Program_Name_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The name of the program. If this is not set, it defaults to
   --  g_get_application_name.
   --
   --  Name: Translator_Credits_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  Credits to the translators. This string should be marked as
   --  translatable. The string may contain email addresses and URLs, which
   --  will be displayed as links, see the introduction for more details.
   --
   --  Name: Version_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The version of the program.
   --
   --  Name: Website_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The URL for the link to the website of the program. This should be a
   --  string starting with "http://.
   --
   --  Name: Website_Label_Property
   --  Type: UTF8_String
   --  Flags: read-write
   --  The label for the link to the website of the program.
   --
   --  Name: Wrap_License_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether to wrap the text in the license dialog.

   Comments_Property : constant Glib.Properties.Property_String;
   Copyright_Property : constant Glib.Properties.Property_String;
   License_Property : constant Glib.Properties.Property_String;
   License_Type_Property : constant Glib.Properties.Property_Boxed;
   Logo_Property : constant Glib.Properties.Property_Object;
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String;
   Program_Name_Property : constant Glib.Properties.Property_String;
   Translator_Credits_Property : constant Glib.Properties.Property_String;
   Version_Property : constant Glib.Properties.Property_String;
   Website_Property : constant Glib.Properties.Property_String;
   Website_Label_Property : constant Glib.Properties.Property_String;
   Wrap_License_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "activate-link"
   --     function Handler
   --       (Self : access Gtk_About_Dialog_Record'Class;
   --        Uri  : UTF8_String) return Boolean;
   --    --  "uri": the URI that is activated
   --  The signal which gets emitted to activate a URI. Applications may
   --  connect to it to override the default behaviour, which is to call
   --  gtk_show_uri().
   --  Returns True if the link has been activated

   Signal_Activate_Link : constant Glib.Signal_Name := "activate-link";

private
   Comments_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("comments");
   Copyright_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("copyright");
   License_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("license");
   License_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("license-type");
   Logo_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("logo");
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("logo-icon-name");
   Program_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("program-name");
   Translator_Credits_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("translator-credits");
   Version_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("version");
   Website_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("website");
   Website_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("website-label");
   Wrap_License_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("wrap-license");
end Gtk.About_Dialog;
