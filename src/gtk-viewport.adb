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

with System;
with Gdk; use Gdk;

package body Gtk.Viewport is

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment (Viewport : in Gtk_Viewport'Class)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class
   is
      function Internal (Viewport : in System.Address)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_hadjustment");
      Widget : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Widget, Internal (Get_Object (Viewport)));
      return Widget;
   end Get_Hadjustment;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Viewport : in Gtk_Viewport'Class)
                             return        Gtk.Adjustment.Gtk_Adjustment'Class
   is
      function Internal (Viewport : in System.Address)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_viewport_get_vadjustment");
      Widget : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Widget, Internal (Get_Object (Viewport)));
      return Widget;
   end Get_Vadjustment;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Viewport;
       Hadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class;
       Vadjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      function Internal
         (Hadjustment : in System.Address;
          Vadjustment : in System.Address)
          return           System.Address;
      pragma Import (C, Internal, "gtk_viewport_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Hadjustment),
                                    Get_Object (Vadjustment)));
   end Gtk_New;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Viewport   : in Gtk_Viewport'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
         (Viewport   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_hadjustment");
   begin
      Internal (Get_Object (Viewport),
                Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Viewport : in Gtk_Viewport'Class;
       The_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Viewport : in System.Address;
          The_Type : in Gint);
      pragma Import (C, Internal, "gtk_viewport_set_shadow_type");
   begin
      Internal (Get_Object (Viewport),
                Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Viewport   : in Gtk_Viewport'Class;
       Adjustment : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
         (Viewport   : in System.Address;
          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_viewport_set_vadjustment");
   begin
      Internal (Get_Object (Viewport),
                Get_Object (Adjustment));
   end Set_Vadjustment;

end Gtk.Viewport;
