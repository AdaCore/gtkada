-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

package body Glib is


   ------------------
   --  To_Boolean  --
   ------------------

   function To_Boolean (Value : in Gboolean) return Boolean is
   begin
      return Value /= Gboolean (Gboolean'Val (0));
   end To_Boolean;


   ------------------
   --  To_Boolean  --
   ------------------

   function To_Boolean (Value : in Gint) return Boolean is
   begin
      return Value /= 0;
   end To_Boolean;


   ------------------
   --  To_Boolean  --
   ------------------

   function To_Boolean (Value : in Guint) return Boolean is
   begin
      return Value /= 0;
   end To_Boolean;


   ------------------------
   --  To_Boolean_Array  --
   ------------------------

   function To_Boolean_Array (A : in Gboolean_Array) return Boolean_Array is
      Result : Boolean_Array (A'Range);
   begin
      for Index in A'Range loop
         Result (Index) := To_Boolean (A (Index));
      end loop;
      return Result;
   end To_Boolean_Array;


   -------------------
   --  To_Gboolean  --
   -------------------

   function To_Gboolean (Bool : in Boolean) return Gboolean is
   begin
      if Bool then
         return Gboolean'Val (1);
      else
         return Gboolean'Val (0);
      end if;
   end To_Gboolean;


   ---------------
   --  To_Gint  --
   ---------------

   function To_Gint (Bool : in Boolean) return Gint is
   begin
      if Bool then
         return 1;
      else
         return 0;
      end if;
   end To_Gint;

end Glib;
