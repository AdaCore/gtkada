------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2018, AdaCore                     --
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
--
--  This package provides support for string internationalization using the
--  libintl library.
--
--  Developer setup
--  ===============
--
--  To provide internationalization in your application, you must install a
--  number of files along with your application, and modify your code to
--  highlight the strings to translate. This translation is based on the
--  gettext() library. Reading its documentation is recommended since it
--  explains best practices for handling translations.
--
--  Preparing your code
--  ===================
--
--  Gettext needs to information to locate the translation files: a language,
--  as setup by the user (see User Setup below), and a domain, hard-coded in
--  the application. The domain is the name of your application. Given these
--  two pieces of information, the translation file will be found in:
--     $prefix/<lang>/LC_MESSAGES/<domain>.mo
--
--  Where $prefix is either one of the standard search paths, or specified
--  through a call to Bind_Text_Domain.
--
--  Although the user can simply specify which language to use by setting one
--  environment variable, they are in fact several other setup to be done, so
--  that the C library properly handles date format for instance. This is done
--  through a call to Setlocale.
--
--  An application can be associated with several domains, although it is
--  generally recommended to have one default domain, specify through a call to
--  Text_Domain. Each string can then be translated through a call to Gettext,
--  without specifying the domain every time.
--  A convenient shortcut is provided in the form of the "-" operator.
--
--  As a result, typical code would look like:
--    begin
--       Setlocale;
--       Text_Domain ("application");
--       Bind_Text_Domain ("application", "/usr/local/share/locale");
--       ...
--       Put_Line (-"I18n string");
--    end;
--
--  Preparing and installing the translation files
--  ===============================================
--
--  The Gtkada distribution comes with a convenient script named
--  build_skeleton.pl, which you can run on your application to extract all the
--  strings that should be translated. See the "po/" directory in GtkAda, as
--  well as the Makefile in this directory.
--
--  Running "make refresh" will reparse all the source files in your
--  application, and create (or update if they already exist) one file .po for
--  each language registered in the Makefile.
--
--  You would then translate each of the string indicated my "msgid", by
--  modifying the lines starting with "msgstr".
--
--  Once this is done, running the msgfmt tool through "make install" will
--  generate a <lang>.mo binary file, which should be copied in the directory
--     $prefix/<lang>/LC_MESSAGES/<domain>.mo
--
--  The translation files can also be created fully by hand.
--  Here is a sample translation file that can be used as an input for msgfmt:
--
--  # gtkada-fr.po
--  msgid  "Help"
--  msgstr "Aide"
--
--  msgid  "Yes"
--  msgstr "Oui"
--
--  $ msgfmt gtkada-fr.po -o gtkada-fr.gmo
--  $ cp gtkada-fr.gmo /usr/share/locale/fr/LC_MESSAGES/gtkada.mo
--
--  User setup
--  ==========
--
--  To change the current locale setting, use the environment variables
--  "LANG". For example, to switch to the french locale using
--  bash:
--
--  $ export LANG=fr_FR
--
--  Depending on the specific implementation of gettext, the following
--  environment variables may be set to change the default settings of locale
--  parameters:
--
--    - LANG Specifies locale name.
--
--    - LC_MESSAGES
--          Specifies messaging locale, and if present overrides
--          LANG for messages.
--
--    - TEXTDOMAIN
--          Specifies the text domain name, which is identical to
--          the message object filename without .mo suffix.
--
--    - TEXTDOMAINDIR
--          Specifies the pathname to the message database, and if
--          present replaces the default (e.g /usr/lib/locale on Solaris,
--          /usr/share/locale on Linux).
--
--  See the gettext documentation of your specific OS for more details.
--
--  </description>

with Glib;

package Gtkada.Intl is
   pragma Preelaborate;

   function Gettext (Msg : Glib.UTF8_String) return Glib.UTF8_String;
   --  Look up Msg in the current default message catalog.
   --  Use the current locale as specified by LC_MESSAGES. If not found, return
   --  Msg itself (the default text).

   function Dgettext
     (Domain : String; Msg : Glib.UTF8_String) return Glib.UTF8_String;
   --  Look up Msg in the Domain message catalog for the current locale.

   function "-" (Msg : Glib.UTF8_String) return Glib.UTF8_String;
   --  Shortcut for Dgettext ("GtkAda", Msg)

   function Dcgettext
     (Domain : String; Msg : Glib.UTF8_String; Category : Integer)
      return Glib.UTF8_String;
   --  Look up Msg in the Domain message catalog for the Category locale.

   function Default_Text_Domain return String;
   --  Return the current default message catalog.

   procedure Text_Domain (Domain : String := "");
   --  Set the current default message catalog to Domain.
   --  If Domain is "", reset to the default of "messages".

   procedure Bind_Text_Domain (Domain : String; Dirname : String);
   --  Specify that the Domain message catalog will be found in Dirname.
   --  This overrides the default system locale data base.
   --  Dirname will generally be the installation prefix for your application.

   procedure Bind_Text_Domain_Codeset (Domain : String; Codeset : String);
   --  Specify the character encoding in which the messages from the
   --  DOMAINNAME message catalog will be returned.

   --  Category values
   LC_ALL      : Integer := 0;
   LC_COLLATE  : Integer := 1;
   LC_CTYPE    : Integer := 2;
   LC_MONETARY : Integer := 3;
   LC_NUMERIC  : Integer := 4;
   LC_TIME     : Integer := 5;
   LC_MESSAGES : Integer := 6;
   u_LC_LAST   : Integer := 7;  --  Marks end

   function Getlocale return String;
   --  Return the current locale.

   procedure Setlocale (Category : Integer := LC_ALL; Locale : String := "");
   --  This procedure must be called before any other subprogram in this
   --  package. It will initialize internal variables based on the environment
   --  variables.

end Gtkada.Intl;
