-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006-2009, AdaCore                   --
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
--  The Gtk_About_Dialog offers a simple way to display information about a
--  program like its logo, name, copyright, website and license. It is also
--  possible to give credits to the authors, documenters, translators and
--  artists who have worked on the program. An about dialog is typically opened
--  when the user selects the About option from the Help menu. All parts of the
--  dialog are optional.
--
--  About dialog often contain links and email addresses. Gtk_About_Dialog
--  supports this by offering global hooks, which are called when the user
--  clicks on a link or email address, see Set_Email_Hook and Set_Url_Hook.
--  Email addresses in the authors, documenters and artists properties are
--  recognized by looking for <user@host>, URLs are recognized by looking for
--  http://url, with url extending to the next space, tab or line break.
--
--  To make constructing a Gtk_About_Dialog as convenient as possible, you can
--  use the function gtk_show_about_dialog which constructs and shows a dialog
--  and keeps it around so that it can be shown again.
--  </description>
--  <c_version>2.16</c_version>
--  <group>Windows</group>
--  <testgtk>create_about.adb</testgtk>

with Glib.Properties;
with Gdk.Color;
with Gdk.Pixbuf;
with Gtk.Dialog;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

package Gtk.About_Dialog is

   type Gtk_About_Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with
     null record;
   type Gtk_About_Dialog is access all Gtk_About_Dialog_Record'Class;

   procedure Gtk_New (About : out Gtk_About_Dialog);
   procedure Initialize (About : access Gtk_About_Dialog_Record'Class);
   --  Creates a new Gtk_About_Dialog.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Button.

   procedure Set_Artists
     (About   : access Gtk_About_Dialog_Record;
      Artists : GNAT.Strings.String_List);
   function Get_Artists
     (About : access Gtk_About_Dialog_Record) return GNAT.Strings.String_List;
   --  Returns the strings which are displayed in the artists tab
   --  of the secondary credits dialog.
   --  The returned value must be freed by the caller, as well as the Artists
   --  parameter.

   procedure Set_Authors
     (About   : access Gtk_About_Dialog_Record;
      Authors : GNAT.Strings.String_List);
   function Get_Authors
     (About : access Gtk_About_Dialog_Record) return GNAT.Strings.String_List;
   --  Returns the string which are displayed in the authors tab
   --  of the secondary credits dialog.
   --  The returned value must be freed by the caller, as well as the Authors
   --  paramaeter.

   procedure Set_Comments
     (About  : access Gtk_About_Dialog_Record; Comments : String);
   function Get_Comments
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the comments string.

   procedure Set_Copyright
     (About : access Gtk_About_Dialog_Record; Copyright : String);
   function Get_Copyright
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the copyright string.

   procedure Set_Documenters
     (About       : access Gtk_About_Dialog_Record;
      Documenters : GNAT.Strings.String_List);
   function Get_Documenters
     (About : access Gtk_About_Dialog_Record) return GNAT.Strings.String_List;
   --  Returns the string which are displayed in the documenters
   --  tab of the secondary credits dialog.
   --  The returned value must be freed by the caller, as well as the
   --  Documenters parameter.

   procedure Set_License
     (About : access Gtk_About_Dialog_Record; License : String);
   function Get_License
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the license information.

   procedure Set_Logo
     (About : access Gtk_About_Dialog_Record;
      Logo  : Gdk.Pixbuf.Gdk_Pixbuf);
   function Get_Logo
     (About : access Gtk_About_Dialog_Record) return Gdk.Pixbuf.Gdk_Pixbuf;
   --  Returns the pixbuf displayed as logo in the about dialog.
   --  The returned value is owned by the dialog. If you want to keep a
   --  reference to it, you must call Ref on it.
   --  Set_Logo sets the pixbuf to be displayed as logo in the about dialog. If
   --  it is null, the default window icon set with Gtk.Window.Set_Default_Icon
   --  will be used.

   procedure Set_Logo_Icon_Name
     (About     : access Gtk_About_Dialog_Record;
      Icon_Name : String := "");
   function Get_Logo_Icon_Name
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the icon name displayed as logo in the about dialog.
   --  If the Icon_Name is set to the empty string, the default window icon
   --  set with Gtk.Window.Set_Default_Icon will be used.

   function Get_Program_Name (About : access Gtk_About_Dialog_Record)
      return String;
   procedure Set_Program_Name
     (About : access Gtk_About_Dialog_Record;
      Name  : String);
   --  Returns or sets the program name displayed in the about dialog.
   --  (since 2.12)

   procedure Set_Translator_Credits
     (About              : access Gtk_About_Dialog_Record;
      Translator_Credits : String);
   function Get_Translator_Credits
     (About : access Gtk_About_Dialog_Record) return String;
   --  Sets the translator credits string which is displayed in
   --  the translators tab of the secondary credits dialog.
   --
   --  The intended use for this string is to display the translator
   --  of the language which is currently used in the user interface.
   --  Using Gtkada.Intl.Gettext, a simple way to achieve that is to mark the
   --  string for translation:
   --       Set_Translator_Credits (About, -"translator-credits");
   --  It is a good idea to use the customary msgid "translator-credits" for
   --  this purpose, since translators will already know the purpose of that
   --  msgid, and since Gtk_About_Dialog will detect if "translator-credits" is
   --  untranslated and hide the tab.

   procedure Set_Version
     (About   : access Gtk_About_Dialog_Record; Version : String);
   function Get_Version
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the version string.

   procedure Set_Website
     (About   : access Gtk_About_Dialog_Record; Website : String);
   function Get_Website
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the website URL. This URL must start with http:// to be
   --  properly recognized as an hyper link. You must also have called
   --  Set_Url_Hook before calling this function.

   procedure Set_Website_Label
     (About         : access Gtk_About_Dialog_Record;
      Website_Label : String);
   function Get_Website_Label
     (About : access Gtk_About_Dialog_Record) return String;
   --  Returns the label used for the website link. It defaults to the URL.

   procedure Set_Wrap_License
     (About        : access Gtk_About_Dialog_Record;
      Wrap_License : Boolean);
   function Get_Wrap_License
     (About : access Gtk_About_Dialog_Record) return Boolean;
   --  Returns whether the license text in About is
   --  automatically wrapped.

   type Activate_Link_Func is access procedure
     (About : System.Address;
      Link  : Interfaces.C.Strings.chars_ptr;
      Data  : System.Address);
   pragma Convention (C, Activate_Link_Func);
   --  A callback called when the user presses an hyper link in the about
   --  dialog. This is a low-level function, and you'll need to convert the
   --  parameters to more useful types with:
   --     Stub : Gtk_About_Dialog_Record;
   --     A    : constant Gtk_About_Dialog :=
   --       Gtk_About_Dialog (Get_User_Data (About, Stub));
   --     L    : constant String := Interfaces.C.Strings.Value (Link);

   function Set_Email_Hook
     (Func    : Activate_Link_Func;
      Data    : System.Address;
      Destroy : Glib.G_Destroy_Notify_Address)
      return Activate_Link_Func;
   --  Installs a global function to be called whenever the user activates an
   --  email link in an about dialog.
   --  Return value: the previous email hook.

   function Set_Url_Hook
     (Func    : Activate_Link_Func;
      Data    : System.Address;
      Destroy : Glib.G_Destroy_Notify_Address)
      return Activate_Link_Func;
   --  Installs a global function to be called whenever the user activates a
   --  URL link in an about dialog.
   --  Return value: the previous URL hook.

   procedure Set_Name
     (About : access Gtk_About_Dialog_Record; Name  : String)
     renames Set_Program_Name;
   pragma Obsolescent; --  Set_Name
   function Get_Name
     (About : access Gtk_About_Dialog_Record) return String
     renames Get_Program_Name;
   pragma Obsolescent; --  Get_Name
   --  Returns the program name displayed in the about dialog.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.

   --  <properties>
   --  Name:  Artists_Property
   --  Type:  Boxed
   --  Descr: List of people who have contributed artwork to the program
   --
   --  Name:  Authors_Property
   --  Type:  Boxed
   --  Descr: List of authors of the program
   --
   --  Name:  Comments_Property
   --  Type:  String
   --  Descr: Comments about the program
   --
   --  Name:  Copyright_Property
   --  Type:  String
   --  Descr: Copyright information for the program
   --
   --  Name:  Documenters_Property
   --  Type:  Boxed
   --  Descr: List of people documenting the program
   --
   --  Name:  Logo_Property
   --  Type:  Object
   --  Descr: A logo for the about box. If this is not set, it defaults to
   --         Gtk.Window.Get_Default_Icon_List
   --
   --  Name:  Logo_Icon_Name_Property
   --  Type:  String
   --  Descr: A named icon to use as the logo for the about box.
   --
   --  Name:  Name_Property
   --  Type:  String
   --  Obsolescent, see Program_Name_Property
   --  Descr: The name of the program. If this is not set, it defaults to
   --         the application name from the command line
   --
   --  Name:  Program_Name_Property
   --  Type:  String
   --  Descr: The name of the program.
   --
   --  Name:  Translator_Credits_Property
   --  Type:  String
   --  Descr: Credits to the translators. This string should be marked as
   --         translatable
   --
   --  Name:  Version_Property
   --  Type:  String
   --  Descr: The version of the program
   --
   --  Name:  Website_Property
   --  Type:  String
   --  Descr: The URL for the link to the website of the program
   --
   --  Name:  Website_Label_Property
   --  Type:  String
   --  Descr: The label for the link to the website of the program. If this is
   --         not set, it defaults to the URL
   --
   --  Name:  Wrap_License_Property
   --  Type:  Boolean
   --  Descr: Whether to wrap the license text.
   --  </properties>

   --   Artists_Property       : constant Glib.Properties.Property_Boxed;
   --   Authors_Property       : constant Glib.Properties.Property_Boxed;
   Comments_Property           : constant Glib.Properties.Property_String;
   Copyright_Property          : constant Glib.Properties.Property_String;
   --  Documenters_Property    : constant Glib.Properties.Property_Boxed;
   Logo_Property               : constant Glib.Properties.Property_Object;
   Logo_Icon_Name_Property     : constant Glib.Properties.Property_String;
   Name_Property               : constant Glib.Properties.Property_String;
   Program_Name_Property       : constant Glib.Properties.Property_String;
   Translator_Credits_Property : constant Glib.Properties.Property_String;
   Version_Property            : constant Glib.Properties.Property_String;
   Website_Property            : constant Glib.Properties.Property_String;
   Website_Label_Property      : constant Glib.Properties.Property_String;
   Wrap_License_Property       : constant Glib.Properties.Property_Boolean;

   -----------------
   -- Obsolescent --
   -----------------
   --  All subprograms below are now obsolescent in gtk+. They might be removed
   --  from future versions of gtk+ (and therefore GtkAda).
   --  To find out whether your code uses any of these, we recommend compiling
   --  with the -gnatwj switch
   --  <doc_ignore>

   ----------------------
   -- Style Properties --
   ----------------------
   --  The following properties can be changed through the gtk theme and
   --  configuration files, and retrieved through Gtk.Widget.Style_Get_Property

   --  <style_properties>
   --  Name:  Link_Color_Property
   --  Type:  Boxed
   --  Descr: Color of hyperlinks
   --  </style_properties>

   Link_Color_Property : constant Gdk.Color.Property_Gdk_Color;

private
--     Artists_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("artists");
--     Authors_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("authors");
   Comments_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("comments");
   Copyright_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("copyright");
--     Documenters_Property : constant Glib.Properties.Property_Boxed :=
--       Glib.Properties.Build ("documenters");
   Logo_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("logo");
   Logo_Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("logo-icon-name");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
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

   Link_Color_Property : constant Gdk.Color.Property_Gdk_Color :=
     Gdk.Color.Property_Gdk_Color (Glib.Build ("link-color"));

   pragma Import (C, Get_Type,       "gtk_about_dialog_get_type");
   pragma Import (C, Set_Email_Hook, "gtk_about_dialog_set_email_hook");
   pragma Import (C, Set_Url_Hook,   "gtk_about_dialog_set_url_hook");
end Gtk.About_Dialog;

--  No binding: gtk_show_about_dialog
--
--  These are obsolescent, and implemented as renamings
--  No binding: gtk_about_dialog_get_name
--  No binding: gtk_about_dialog_set_name
