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

package body Gtk.Paned is

   ----------
   -- Add1 --
   ----------

   procedure Add1
      (Paned : in Gtk_Paned'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Paned : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");
   begin
      Internal (Get_Object (Paned),
                Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
      (Paned : in Gtk_Paned'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (Paned : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");
   begin
      Internal (Get_Object (Paned),
                Get_Object (Child));
   end Add2;

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Widget : out Gtk_Paned) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Widget : out Gtk_Paned) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New_Vpaned;

   -----------------
   -- Gutter_Size --
   -----------------

   procedure Gutter_Size
      (Paned : in Gtk_Paned'Class;
       Size  : in Guint16)
   is
      procedure Internal
         (Paned : in System.Address;
          Size  : in Gint);
      pragma Import (C, Internal, "gtk_paned_gutter_size");
   begin
      Internal (Get_Object (Paned),
                Guint16'Pos (Size));
   end Gutter_Size;

   -----------------
   -- Handle_Size --
   -----------------

   procedure Handle_Size
      (Paned : in Gtk_Paned'Class;
       Size  : in Guint16)
   is
      procedure Internal
         (Paned : in System.Address;
          Size  : in Gint);
      pragma Import (C, Internal, "gtk_paned_handle_size");
   begin
      Internal (Get_Object (Paned),
                Guint16'Pos (Size));
   end Handle_Size;

end Gtk.Paned;
