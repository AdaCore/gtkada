-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                       Copyright (C) 2000                          --
--                            ACT-Europe                             --
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

with System;
with Gtk; use Gtk;

package body Gnome.About is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (About     : out Gnome_About;
      Title     : String;
      Version   : String;
      Copyright : String;
      Authors   : Chars_Ptr_Array;
      Comments  : String;
      Logo      : String) is
   begin
      About := new Gnome_About_Record;
      Initialize (About, Title, Version, Copyright, Authors, Comments, Logo);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (About     : access Gnome_About_Record'Class;
      Title     : String;
      Version   : String;
      Copyright : String;
      Authors   : Chars_Ptr_Array;
      Comments  : String;
      Logo      : String)
   is
      Authors_Padded : constant Chars_Ptr_Array := Authors + Null_Ptr;

      function Internal
        (Title     : String;
         Version   : String;
         Copyright : String;
         Authors   : Chars_Ptr_Array;
         Comments  : String;
         Logo      : String) return System.Address;
      pragma Import (C, Internal, "gnome_about_new");

   begin
      Set_Object
        (About,
         Internal
           (Title & ASCII.NUL,
            Version & ASCII.NUL,
            Copyright & ASCII.NUL,
            Authors_Padded,
            Comments & ASCII.NUL,
            Logo & ASCII.NUL));
      Initialize_User_Data (About);
   end Initialize;

end Gnome.About;
