-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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

with Interfaces.C.Strings;

package body Gtk is

   -------------------
   -- Major_Version --
   -------------------

   function Major_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_major_version");
   begin
      return Number;
   end Major_Version;

   -------------------
   -- Micro_Version --
   -------------------

   function Micro_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_micro_version");
   begin
      return Number;
   end Micro_Version;

   -------------------
   -- Minor_Version --
   -------------------

   function Minor_Version return Guint is
      Number : Guint;
      pragma Import (C, Number, "gtk_minor_version");
   begin
      return Number;
   end Minor_Version;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (Type_Num : in Gint) return      String
   is
      function Internal (Type_Num : in Gint)
                         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_type_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Type_Num));
   end Type_Name;

end Gtk;
