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

with Unchecked_Conversion;
with Interfaces.C.Strings;
with System;
with Gdk; use Gdk;

package body Gtk.Enums is

   -------------
   -- Convert --
   -------------

   function Convert (S : String) return System.Address is
      function Internal is new Unchecked_Conversion
        (Interfaces.C.Strings.chars_ptr, System.Address);
   begin
      return Internal (Interfaces.C.Strings.New_String (S));
   end Convert;

   function Convert (S : System.Address) return String is
      function Internal is new Unchecked_Conversion
        (System.Address, Interfaces.C.Strings.chars_ptr);
   begin
      return Interfaces.C.Strings.Value (Internal (S));
   end Convert;

   function Convert (W : Gtk.Widget.Gtk_Widget'Class) return System.Address is
   begin
      return Get_Object (W);
   end Convert;

   function Convert (W : System.Address) return Gtk.Widget.Gtk_Widget'Class is
      Widget : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Widget, W);
      return Widget;
   end Convert;

   function Convert (I : Gint) return System.Address is
      function Internal is new Unchecked_Conversion
        (Gint, System.Address);
   begin
      return Internal (I);
   end Convert;

   function Convert (S : System.Address) return Gint is
      function Internal is new Unchecked_Conversion
        (System.Address, Gint);
   begin
      return Internal (S);
   end Convert;

end Gtk.Enums;
