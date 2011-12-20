------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
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

with Gdk.Pixbuf;
with Gtk;
with Gnome.Dialog;
with Gtkada.Types; use Gtkada.Types;

package Gnome.About is
   type Gnome_About_Record is new Gnome.Dialog.Gnome_Dialog_Record
     with private;
   type Gnome_About is access all Gnome_About_Record'Class;

   procedure Gnome_New
     (About              : out Gnome_About;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk.Pixbuf.Gdk_Pixbuf);
   --  Create a new about box.
   --  Name is the name of the application.
   --  Version is the version string.
   --  Copyright is the copyright notice (one line.)
   --  Comments provides other comments.
   --  Authors is the list of authors.
   --  Documenters is the list of Documenters
   --  Translator_Credits is the names of Translators
   --  Logo is a logo pixbuf file.
   --
   --  Usage:
   --    Gnome_New (About, -"GnoApp", "1.2b", -"Copyright FSF (c) 2001",
   --      "author1" + "author2",
   --      "Comment line 1" & ASCII.LF & "Comment Line 2",
   --      Logo);
   --    Show (About);

   procedure Initialize
     (About              : access Gnome_About_Record'Class;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk.Pixbuf.Gdk_Pixbuf);

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gnome_About.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  </signals>

private
   type Gnome_About_Record is new Gnome.Dialog.Gnome_Dialog_Record
     with null record;
   pragma Import (C, Get_Type, "gnome_about_get_type");
end Gnome.About;
