-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Gtk is

   -------------------
   -- Argument_Type --
   -------------------

   function Argument_Type
     (The_Type : Gtk_Type; Name : in String; Num : in Gint) return Gtk_Type
   is
      function Internal
        (The_Type : Gtk_Type; Name : String; Num  : Gint) return Gtk_Type;
      pragma Import (C, Internal, "ada_signal_argument_type");
   begin
      return Internal (The_Type, Name & ASCII.NUL, Num);
   end Argument_Type;

   ---------------------
   -- Count_Arguments --
   ---------------------

   function Count_Arguments
     (The_Type : Gtk_Type; Name : in String) return Guint
   is
      function Internal (The_Type : Gtk_Type; Name : String) return Guint;
      pragma Import (C, Internal, "ada_signal_count_arguments");

   begin
      return Internal (The_Type, Name & ASCII.NUL);
   end Count_Arguments;

end Gtk;
