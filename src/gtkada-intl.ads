-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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
--
--  This package provides support for string internationalization using the
--  libintl library.
--
--  To change the current locale setting, use on the environment variables
--  "LANG" or "LC_MESSAGES". For example, to switch to the french locale using
--  bash:
--
--  $ export LANG=fr
--
--  The following environment variables can be set to change the default
--  settings of locale parameters:
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
--          Specifies the pathname to the message database, and  if
--          present replaces the default (e.g /usr/lib/locale on Solaris,
--          /usr/share/locale on Linux).
--
--  The recommended way to use the gettext capability in your application is
--  to use Dgettext with your own domain, and define the following shortcut:
--
--  function "-" (Msg : String) return String;
--  --  Convenient shortcut to the Gettext function.
--
--  function "-" (Msg : String) return String is
--  begin
--     return Dgettext ("my_domain", Msg);
--  end "-";
--
--  Do not forget to call Bind_Text_Domain ("my_domain", "my locale prefix")
--  at the beginning of your program. For example, if the prefix of your
--  application is /usr, the standard location of the locale would
--  be /usr/share/locale, e.g:
--    Bind_Text_Domain ("GtkAda", "/usr/share/locale");
--
--  Under this locale directory, the functions provided by this package
--  will look for the directory $LANG/LC_MESSAGES,
--  /usr/share/locale/fr/LC_MESSAGES in our example; and in this directory,
--  the file <domain>.mo will be used, e.g
--  /usr/share/locale/fr/LC_MESSAGES/GtkAda.mo
--
--  The .mo files can be generated using the GNU tool msgfmt that takes a
--  text file containing for each string the original and the translation.
--  See msgfmt documentation for more details.
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
--  $ cp gtkada-fr.gmo /usr/share/locale/fr/LC_MESSAGES/GtkAda.mo
--
--  Then, to enable the string translation in your application, use the "-"
--  function defined above, e.g:
--
--  Gtk_New (Label, -"Help");
--  Gtk_New (Label, -("Help ?") & ASCII.LF & -("Yes"));
--
--  </description>

package Gtkada.Intl is
   pragma Preelaborate;

   function Gettext (Msg : String) return String;
   --  Look up Msg in the current default message catalog.
   --  Use the current locale as specified by LC_MESSAGES. If not found, return
   --  Msg itself (the default text).

   function Dgettext (Domain : String; Msg : String) return String;
   --  Look up Msg in the Domain message catalog for the current locale.

   function "-" (Msg : String) return String;
   --  Shortcut for Dgettext ("GtkAda", Msg)

   function Dcgettext
     (Domain : String; Msg : String; Category : Integer) return String;
   --  Look up Msg in the Domain message catalog for the Category locale.

   function Default_Text_Domain return String;
   --  Return the current default message catalog.

   procedure Text_Domain (Domain : String := "");
   --  Set the current default message catalog to Domain.
   --  If Domain is "", reset to the default of "messages".

   procedure Bind_Text_Domain (Domain : String; Dirname : String);
   --  Specify that the Domain message catalog will be found in Dirname.
   --  This overrides the default system locale data base.

end Gtkada.Intl;
