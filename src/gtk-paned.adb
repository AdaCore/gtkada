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

with System;
with Gdk; use Gdk;

package body Gtk.Paned is

   ----------
   -- Add1 --
   ----------

   procedure Add1
      (Paned : access Gtk_Paned_Record;
       Child : in Gtk.Widget.Gtk_Widget)
   is
      procedure Internal
         (Paned : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_paned_add1");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add1;

   ----------
   -- Add2 --
   ----------

   procedure Add2
      (Paned : access Gtk_Paned_Record;
       Child : in Gtk.Widget.Gtk_Widget)
   is
      procedure Internal
         (Paned : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_paned_add2");
   begin
      Internal (Get_Object (Paned), Get_Object (Child));
   end Add2;

   --------------------
   -- Gtk_New_Hpaned --
   --------------------

   procedure Gtk_New_Hpaned (Widget : out Gtk_Paned) is
   begin
      Widget := new Gtk_Paned_Record;
      Initialize_Hpaned (Widget);
   end Gtk_New_Hpaned;

   --------------------
   -- Gtk_New_Vpaned --
   --------------------

   procedure Gtk_New_Vpaned (Widget : out Gtk_Paned) is
   begin
      Widget := new Gtk_Paned_Record;
      Initialize_Vpaned (Widget);
   end Gtk_New_Vpaned;

   -----------------------
   -- Initialize_Hpaned --
   -----------------------

   procedure Initialize_Hpaned (Widget : access Gtk_Paned_Record) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hpaned_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize_Hpaned;

   -----------------------
   -- Initialize_Vpaned --
   -----------------------

   procedure Initialize_Vpaned (Widget : access Gtk_Paned_Record) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vpaned_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize_Vpaned;

   ---------------------
   -- Set_Gutter_Size --
   ---------------------

   procedure Set_Gutter_Size
      (Paned : access Gtk_Paned_Record;
       Size  : in Guint16)
   is
      procedure Internal
         (Paned : in System.Address;
          Size  : in Gint);
      pragma Import (C, Internal, "gtk_paned_set_gutter_size");
   begin
      Internal (Get_Object (Paned), Guint16'Pos (Size));
   end Set_Gutter_Size;

   ---------------------
   -- Set_Handle_Size --
   ---------------------

   procedure Set_Handle_Size
      (Paned : access Gtk_Paned_Record;
       Size  : in Guint16)
   is
      procedure Internal
         (Paned : in System.Address;
          Size  : in Gint);
      pragma Import (C, Internal, "gtk_paned_set_handle_size");
   begin
      Internal (Get_Object (Paned), Guint16'Pos (Size));
   end Set_Handle_Size;

end Gtk.Paned;
