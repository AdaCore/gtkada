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

with System;
with Gtk; use Gtk;
with Gdk.Pixbuf; use Gdk.Pixbuf;

package body Gnome.About is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (About              : out Gnome_About;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk_Pixbuf) is
   begin
      About := new Gnome_About_Record;
      Initialize
        (About, Name, Version, Copyright,
         Comments, Authors, Documenters,
         Translator_Credits, Logo);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (About              : access Gnome_About_Record'Class;
      Name               : String;
      Version            : String;
      Copyright          : String;
      Comments           : String;
      Authors            : Chars_Ptr_Array;
      Documenters        : Chars_Ptr_Array;
      Translator_Credits : String;
      Logo               : Gdk_Pixbuf)
   is
      Authors_Padded : constant Chars_Ptr_Array := Authors + Null_Ptr;
      Documenters_Padded : constant Chars_Ptr_Array := Documenters + Null_Ptr;

      function Internal
        (Name               : String;
         Version            : String;
         Copyright          : String;
         Comments           : String;
         Authors            : Chars_Ptr_Array;
         Documenters        : Chars_Ptr_Array;
         Translator_Credits : String;
         Logo               : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_about_new");

   begin
      Set_Object
        (About,
         Internal
         (Name & ASCII.NUL,
          Version & ASCII.NUL,
          Copyright & ASCII.NUL,
          Comments & ASCII.NUL,
          Authors_Padded,
          Documenters_Padded,
          Translator_Credits & ASCII.NUL,
          Get_Object (Logo)));
   end Initialize;

end Gnome.About;
