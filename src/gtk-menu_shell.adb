-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
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
with Gdk; use Gdk;

package body Gtk.Menu_Shell is

   ------------
   -- Append --
   ------------

   procedure Append
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
        (Menu_Shell : System.Address;
         Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_append");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Append;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Menu_Shell : in Gtk_Menu_Shell'Class)
   is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deactivate");
   begin
      Internal (Get_Object (Menu_Shell));
   end Deactivate;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class;
      Position   : in Gint)
   is
      procedure Internal (Menu_Shell : System.Address;
                          Child      : System.Address;
                          Position   : Gint);
      pragma Import (C, Internal, "gtk_menu_shell_insert");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child), Position);
   end Insert;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Menu_Shell : in Gtk_Menu_Shell'Class;
      Child      : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal (Menu_Shell : System.Address;
                          Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_prepend");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Prepend;
end Gtk.Menu_Shell;
