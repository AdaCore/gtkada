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

package body Gtk.Container is

   ---------
   -- Add --
   ---------

   procedure Add (Container : in out Gtk_Container;
                  Widget       : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Container : System.Address;
                          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove (Container : in out Gtk_Container;
                     Widget : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Container : System.Address;
                          Widget : System.Address);
      pragma Import (C, Internal, "gtk_container_remove");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Remove;

   ----------------------
   -- Set_Border_Width --
   ----------------------

   procedure Set_Border_Width (Container : in Gtk_Container;
                           Border_Width : in Gint) is
      procedure Internal (Container  : System.Address;
                          Border_Widget : Gint);
      pragma Import (C, Internal, "gtk_container_set_border_width");
   begin
      Internal (Get_Object (Container), Border_Width);
   end Set_Border_Width;

   ---------------------------
   -- Set_Focus_Hadjustment --
   ---------------------------

   procedure Set_Focus_Hadjustment
     (Container  : in out Gtk_Container;
      Adjustment : in     Gtk.Adjustment.Gtk_Adjustment'Class) is

      procedure Internal (Container : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_hadjustment");

   begin
      Internal (Get_Object (Container), Get_Object (Adjustment));
   end Set_Focus_Hadjustment;

   ---------------------------
   -- Set_Focus_Vadjustment --
   ---------------------------

   procedure Set_Focus_Vadjustment
     (Container  : in out Gtk_Container;
      Adjustment : in     Gtk.Adjustment.Gtk_Adjustment'Class) is

      procedure Internal (Container : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_vadjustment");

   begin
      Internal (Get_Object (Container), Get_Object (Adjustment));
   end Set_Focus_Vadjustment;

   ---------------------
   -- Set_Resize_Mode --
   ---------------------

   procedure Set_Resize_Mode (Container   : in out Gtk_Container;
                              Resize_Mode : in Gtk_Resize_Mode) is
      procedure Internal (Container : in System.Address;
                          Mode      : in Integer);
      pragma Import (C, Internal, "gtk_container_set_resize_mode");
   begin
      Internal (Get_Object (Container), Gtk_Resize_Mode'Pos (Resize_Mode));
   end Set_Resize_Mode;

   --------------
   -- Generate --
   --------------

   procedure Generate (Container : in Gtk_Container;
                       N         : in Node_Ptr;
                       File      : in File_Type) is
   begin
      Generate (Gtk_Widget (Container), N, File);
      Gen_Set (N, "Container", "border_width", File);
      --  ??? Missing Set_Focus_Hadjustment
      --  ??? Missing Set_Focus_Vadjustment
      Gen_Set (N, "Container", "resize_mode", File);
   end Generate;

   procedure Generate (Container : in out Gtk_Container;
                       N         : in Node_Ptr) is
      S : String_Ptr;
   begin
      Generate (Gtk_Widget (Container), N);
      S := Get_Field (N, "border_width");

      if S /= null then
         Set_Border_Width (Container, Gint'Value (S.all));
      end if;

      S := Get_Field (N, "resize_mode");

      if S /= null then
         Set_Resize_Mode (Container, Gtk_Resize_Mode'Value (S.all));
      end if;
   end Generate;

end Gtk.Container;
