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

package body Gtk.Menu is

   ------------
   -- Append --
   ------------

   procedure Append
     (Menu  : in Gtk_Menu'Class;
      Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal (Menu  : System.Address;
                          Child : System.Address);
      pragma Import (C, Internal, "gtk_menu_append");
   begin
      Internal (Get_Object (Menu), Get_Object (Child));
   end Append;

   ----------------------
   -- Attach_To_Widget --
   ----------------------

   procedure Attach_To_Widget
     (Menu          : in Gtk_Menu'Class;
      Attach_Widget : in Gtk.Widget.Gtk_Widget'Class;
      Detacher      : in Gtk_Menu_Detach_Func)
   is
      procedure Internal (Menu          : System.Address;
                          Attach_Widget : System.Address;
                          Detacher      : System.Address);
      pragma Import (C, Internal, "gtk_menu_attach_to_widget");
   begin
      Internal (Get_Object (Menu), Get_Object (Attach_Widget),
                Detacher'Address);
   end Attach_To_Widget;

   ------------
   -- Detach --
   ------------

   procedure Detach (Menu : in Gtk_Menu'Class)
   is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_detach");
   begin
      Internal (Get_Object (Menu));
   end Detach;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active (Menu : in Gtk_Menu'Class)
                        return Gtk.Menu_Item.Gtk_Menu_Item
   is
      function Internal (Menu : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_active");
      Item : Gtk.Menu_Item.Gtk_Menu_Item;
   begin
      Set_Object (Item, Internal (Get_Object (Menu)));
      return Item;
   end Get_Active;

   -----------------------
   -- Get_Attach_Widget --
   -----------------------

   function Get_Attach_Widget (Menu : in Gtk_Menu'Class)
                               return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_attach_widget");
      Widget : Gtk.Widget.Gtk_Widget;
   begin
      Set_Object (Widget, Internal (Get_Object (Menu)));
      return Widget;
   end Get_Attach_Widget;

   -------------
   -- Get_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Menu)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Menu     : in Gtk_Menu'Class;
      Child    : in Gtk.Widget.Gtk_Widget'Class;
      Position : in Gint)
   is
      procedure Internal (Menu     : System.Address;
                          Child    : System.Address;
                          Position : Gint);
      pragma Import (C, Internal, "gtk_menu_insert");
   begin
      Internal (Get_Object (Menu), Get_Object (Child), Position);
   end Insert;

   -------------
   -- Popdown --
   -------------

   procedure Popdown (Menu : in Gtk_Menu'Class)
   is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_popdown");
   begin
      Internal (Get_Object (Menu));
   end Popdown;

   -------------
   -- Prepend --
   -------------

   procedure Prepend
     (Menu  : in Gtk_Menu'Class;
      Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal (Menu  : System.Address;
                          Child : System.Address);
      pragma Import (C, Internal, "gtk_menu_prepend");
   begin
      Internal (Get_Object (Menu), Get_Object (Child));
   end Prepend;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active
     (Menu  : in Gtk_Menu'Class;
      Index : in Guint)
   is
      procedure Internal (Menu  : System.Address;
                          Index : Guint);
      pragma Import (C, Internal, "gtk_menu_set_active");
   begin
      Internal (Get_Object (Menu), Index);
   end Set_Active;


   package body Menu_Popup is

      -----------
      -- Popup --
      -----------

      procedure Popup
        (Menu              : in Gtk_Menu'Class;
         Parent_Menu_Shell : in Gtk.Menu_Shell.Gtk_Menu_Shell'Class;
         Parent_Menu_Item  : in Gtk.Menu_Item.Gtk_Menu_Item;
         Func              : in Gtk_Menu_Position_Func;
         Data              : access Data_Type;
         Button            : in Guint;
         Activate_Time     : in Guint32)
      is
         procedure Internal (Menu          : System.Address;
                             Parent_M      : System.Address;
                             Parent_I      : System.Address;
                             Func          : System.Address;
                             Data          : System.Address;
                             Button        : Guint;
                             Activate_Time : Guint32);
         pragma Import (C, Internal, "gtk_menu_popup");
      begin
         Internal (Get_Object (Menu),
                   Get_Object (Parent_Menu_Shell),
                   Get_Object (Parent_Menu_Item),
                   Func'Address,
                   Data.all'Address,
                   Button,
                   Activate_Time);
      end Popup;
   end Menu_Popup;


end Gtk.Menu;
