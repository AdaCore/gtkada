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
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Util; use Gtk.Util;

package body Gtk.Menu is

   ------------
   -- Append --
   ------------

   procedure Append
     (Menu  : access Gtk_Menu_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
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
     (Menu          : access Gtk_Menu_Record;
      Attach_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
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

   procedure Detach (Menu : access Gtk_Menu_Record)
   is
      procedure Internal (Menu : System.Address);
      pragma Import (C, Internal, "gtk_menu_detach");
   begin
      Internal (Get_Object (Menu));
   end Detach;

   ----------------
   -- Get_Active --
   ----------------

   function Get_Active (Menu : access Gtk_Menu_Record)
                        return Gtk.Menu_Item.Gtk_Menu_Item
   is
      function Internal (Menu : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_active");
      Stub : Gtk.Menu_Item.Gtk_Menu_Item_Record;
   begin
      return Gtk.Menu_Item.Gtk_Menu_Item
        (Get_User_Data (Internal (Get_Object (Menu)), Stub));
   end Get_Active;

   -----------------------
   -- Get_Attach_Widget --
   -----------------------

   function Get_Attach_Widget (Menu : access Gtk_Menu_Record)
                               return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu : System.Address)
                         return System.Address;
      pragma Import (C, Internal, "gtk_menu_get_attach_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Menu)), Stub));
   end Get_Attach_Widget;

   -------------
   -- Get_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Menu)
   is
   begin
      Widget := new Gtk_Menu_Record;
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Menu_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Menu     : access Gtk_Menu_Record;
      Child    : access Gtk.Widget.Gtk_Widget_Record'Class;
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

   procedure Popdown (Menu : access Gtk_Menu_Record)
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
     (Menu  : access Gtk_Menu_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class)
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
     (Menu  : access Gtk_Menu_Record;
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
        (Menu              : access Gtk_Menu_Record;
         Parent_Menu_Shell : access Gtk.Menu_Shell.Gtk_Menu_Shell_Record'Class;
         Parent_Menu_Item  : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
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

   --------------
   -- Generate --
   --------------

   procedure Generate (N    : in Node_Ptr;
                       File : in File_Type) is
      S : String_Ptr;
   begin
      Gen_New (N, "Menu", File => File);
      Menu_Shell.Generate (N, File);

      S := Get_Field (N.Parent, "class");

      if S /= null and then S.all = "GtkMenuItem" then
         Gen_Call_Child (N, null, "Menu_Item", "Set_Submenu", File => File);
      end if;
   end Generate;

   procedure Generate (Menu : in out Gtk_Object;
                       N    : in Node_Ptr) is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Gtk_Menu (Menu));
         Set_Object (Get_Field (N, "name"), Menu);
         N.Specific_Data.Created := True;
      end if;

      Menu_Shell.Generate (Menu, N);
      S := Get_Field (N.Parent, "class");

      if S /= null and then S.all = "GtkMenuItem" then
         Menu_Item.Set_Submenu
           (Gtk_Menu_Item (Get_Object (Get_Field (N.Parent, "name"))),
            Widget.Gtk_Widget (Menu));
      end if;
   end Generate;

end Gtk.Menu;
