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
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;

package body Gtk.Fixed is

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Widget : in Gtk.Fixed.Gtk_Fixed'Class)
                          return      Enums.Widget_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_fixed_get_children");
      use Enums.Widget_List;
      Children : Glist;
   begin
      Set_Object (Children, Internal (Get_Object (Widget)));
      return Children;
   end Get_Children;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Fixed)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_fixed_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------
   -- Move --
   ----------

   procedure Move
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16)
   is
      procedure Internal
         (Fixed  : in System.Address;
          Widget : in System.Address;
          X      : in Gint16;
          Y      : in Gint16);
      pragma Import (C, Internal, "gtk_fixed_move");
   begin
      Internal (Get_Object (Fixed),
                Get_Object (Widget),
                X,
                Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16)
   is
      procedure Internal
         (Fixed  : in System.Address;
          Widget : in System.Address;
          X      : in Gint16;
          Y      : in Gint16);
      pragma Import (C, Internal, "gtk_fixed_put");
   begin
      Internal (Get_Object (Fixed),
                Get_Object (Widget),
                X,
                Y);
   end Put;

end Gtk.Fixed;
