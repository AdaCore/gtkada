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
--         General Public License for more details.                  --
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

   -----------
   --  Add  --
   -----------

   procedure Add (Container : in out Gtk_Container'Class;
                  Widget       : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Container : System.Address;
                          Widget    : System.Address);
      pragma Import (C, Internal, "gtk_container_add");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Add;


   --------------------
   --  Block_Resize  --
   --------------------

   procedure Block_Resize (Container : in out Gtk_Container'Class) is
      procedure Internal (Container : in System.Address);
      pragma Import (C, Internal, "gtk_container_block_resize");
   begin
      Internal (Get_Object (Container));
   end Block_Resize;


   --------------------
   --  Border_Width  --
   --------------------

   procedure Border_Width (Container : in Gtk_Container'Class;
                           Border_Width : in Gint) is
      procedure Internal (Container  : System.Address;
                          Border_Widget : Gint);
      pragma Import (C, Internal, "gtk_container_border_width");
   begin
      Internal (Get_Object (Container), Border_Width);
   end Border_Width;


   ----------------------
   --  Disable_Resize  --
   ----------------------

   procedure Disable_Resize (Container : in out Gtk_Container'Class) is
      procedure Internal (Container : in System.Address);
      pragma Import (C, Internal, "gtk_container_disable_resize");
   begin
      Internal (Get_Object (Container));
   end Disable_Resize;


   ---------------------
   --  Enable_Resize  --
   ---------------------

   procedure Enable_Resize (Container : in out Gtk_Container'Class) is
      procedure Internal (Container : in System.Address);
      pragma Import (C, Internal, "gtk_container_enable_resize");
   begin
      Internal (Get_Object (Container));
   end Enable_Resize;


   -------------------
   --  Need_Resize  --
   -------------------

   function Need_Resize (Container : in Gtk_Container'Class)
                         return Boolean is
      function Internal (Container : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_container_need_resize");
   begin
      return To_Boolean (Internal (Get_Object (Container)));
   end Need_Resize;


   --------------
   --  Remove  --
   --------------

   procedure Remove (Container : in out Gtk_Container'Class;
                     Widget : in Gtk.Widget.Gtk_Widget'Class) is
      procedure Internal (Container : System.Address;
                          Widget : System.Address);
      pragma Import (C, Internal, "gtk_container_remove");
   begin
      Internal (Get_Object (Container), Get_Object (Widget));
   end Remove;


   -----------------------------
   --  Set_Focus_Hadjustment  --
   -----------------------------

   procedure Set_Focus_Hadjustment
     (Container  : in out Gtk_Container'Class;
      Adjustment : in     Gtk.Adjustment.Gtk_Adjustment'Class) is

      procedure Internal (Container : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_hadjustment");

   begin
      Internal (Get_Object (Container), Get_Object (Adjustment));
   end Set_Focus_Hadjustment;


   -----------------------------
   --  Set_Focus_Vadjustment  --
   -----------------------------

   procedure Set_Focus_Vadjustment
     (Container  : in out Gtk_Container'Class;
      Adjustment : in     Gtk.Adjustment.Gtk_Adjustment'Class) is

      procedure Internal (Container : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_container_set_focus_vadjustment");

   begin
      Internal (Get_Object (Container), Get_Object (Adjustment));
   end Set_Focus_Vadjustment;


   ----------------------
   --  Unblock_Resize  --
   ----------------------

   procedure Unblock_Resize (Container : in out Gtk_Container'Class) is
      procedure Internal (Container : in System.Address);
      pragma Import (C, Internal, "gtk_container_unblock_resize");
   begin
      Internal (Get_Object (Container));
   end Unblock_Resize;

end Gtk.Container;
