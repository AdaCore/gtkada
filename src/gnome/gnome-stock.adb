-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

with Gdk.Pixmap;
with Gdk; use Gdk;
with Gtk; use Gtk;
with Gtk.Button;
with Gtk.Widget;
with System;

package body Gnome.Stock is

   use Gtk.Button;
   use Gtk.Menu_Item;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Stock; Icon : String) is
   begin
      Widget := new Gnome_Stock_Record;
      Initialize (Widget, Icon);
   end Gnome_New;

   procedure Gnome_New (Widget : out Gnome_Stock) is
   begin
      Widget := new Gnome_Stock_Record;
      Initialize (Widget);
   end Gnome_New;

   procedure Gnome_New
     (Widget : out Gnome_Stock;
      Window : access Gtk.Widget.Gtk_Widget_Record'Class;
      Icon   : String) is
   begin
      Widget := new Gnome_Stock_Record;
      Initialize (Widget, Window, Icon);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_Stock_Record'Class; Icon : String)
   is
      function Internal (Icon : String) return System.Address;
      pragma Import (C, Internal, "gnome_stock_new_with_icon");
   begin
      Set_Object (Widget, Internal (Icon & ASCII.NUL));
      Initialize_User_Data (Widget);
   end Initialize;

   procedure Initialize (Widget : access Gnome_Stock_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_stock_new");
   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   procedure Initialize
     (Widget : access Gnome_Stock_Record'Class;
      Window : access Gtk.Widget.Gtk_Widget_Record'Class;
      Icon   : String)
   is
      function Internal
        (Window : System.Address;
         Icon   : String) return System.Address;
      pragma Import (C, Internal, "gnome_stock_pixmap_widget_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Window), Icon & ASCII.NUL));
      Initialize_User_Data (Widget);
   end Initialize;

   ------------
   -- Button --
   ------------

   function Button
     (Button_Type : String;
      Ordinary    : Boolean := False) return Gtk_Button
   is
      function Internal (Button_Type : String) return System.Address;
      pragma Import (C, Internal, "gnome_stock_button");

      function Internal2 (Button_Type : String) return System.Address;
      pragma Import (C, Internal2, "gnome_stock_or_ordinary_button");

   begin
      if Ordinary then
         return Gtk_Button
           (Widget.Convert (Internal2 (Button_Type & ASCII.NUL)));
      else
         return Gtk_Button
           (Widget.Convert (Internal (Button_Type & ASCII.NUL)));
      end if;
   end Button;

   ------------------------
   -- Button_Can_Default --
   ------------------------

   procedure Button_Can_Default
     (Button      : access Gtk.Button.Gtk_Button_Record'Class;
      Can_Default : Boolean)
   is
      procedure Internal
        (Button      : System.Address;
         Can_Default : Gint);
      pragma Import (C, Internal, "gnome_button_can_default");
   begin
      Internal (Get_Object (Button), Boolean'Pos (Can_Default));
   end Button_Can_Default;

   ----------------
   -- Menu_Accel --
   ----------------

   procedure Menu_Accel
     (The_Type : String;
      Key      : String;
      Modifier : out Gdk.Types.Gdk_Modifier_Type;
      Result   : out Boolean)
   is
      function Internal
        (The_Type : String;
         Key      : String;
         Modifier : access Gint) return Gint;
      pragma Import (C, Internal, "gnome_stock_menu_accel");

      Local_Modifier : aliased Gint;
   begin
      Result := Boolean'Val (Internal
        (The_Type & ASCII.NUL, Key & ASCII.NUL,
         Local_Modifier'Unchecked_Access));
      Modifier := Gdk.Types.Gdk_Modifier_Type'Val (Local_Modifier);
   end Menu_Accel;

   procedure Menu_Accel
     (The_Type : String;
      Key      : String;
      Modifier : out Gdk.Types.Gdk_Modifier_Type)
   is
      Tmp : Boolean;
   begin
      Menu_Accel (The_Type, Key, Modifier, Tmp);
   end Menu_Accel;

   ----------------------
   -- Menu_Accel_Parse --
   ----------------------

   procedure Menu_Accel_Parse (Section : String)
   is
      procedure Internal (Section : String);
      pragma Import (C, Internal, "gnome_stock_menu_accel_parse");
   begin
      Internal (Section & ASCII.NUL);
   end Menu_Accel_Parse;

   ---------------
   -- Menu_Item --
   ---------------

   function Menu_Item
     (Icon : String;
      Text : String) return Gtk_Menu_Item
   is
      function Internal
        (Icon : String;
         Text : String) return System.Address;
      pragma Import (C, Internal, "gnome_stock_menu_item");

   begin
      return Gtk_Menu_Item
        (Widget.Convert (Internal (Icon & ASCII.NUL, Text & ASCII.NUL)));
   end Menu_Item;

   -------------------
   -- Pixmap_Button --
   -------------------

   function Pixmap_Button
     (Pixmap : access Gtk.Widget.Gtk_Widget_Record'Class;
      Text   : String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Pixmap : System.Address;
         Text   : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_pixmap_button");
   begin
      return Widget.Convert (Internal (Get_Object (Pixmap), Text & ASCII.NUL));
   end Pixmap_Button;

   -------------------
   -- Pixmap_Change --
   -------------------

   --  function Pixmap_Change
   --    (Icon     : String;
   --     Sub_Type : String;
   --     GEntry   : access Gnome_Stock_Pixmap_Entry_Record) return Gint
   --  is
   --     function Internal
   --       (Icon     : String;
   --        Sub_Type : String;
   --        GEntry   : System.Address)
   --        return Gint;
   --     pragma Import (C, Internal, "gnome_stock_pixmap_change");
   --  begin
   --     return Internal (Icon & ASCII.NUL,
   --                      Sub_Type & ASCII.NUL,
   --                      Get_Object (GEntry));
   --  end Pixmap_Change;

   ---------------------
   -- Pixmap_Checkfor --
   ---------------------

   --  function Pixmap_Checkfor
   --    (Icon     : String;
   --     Sub_Type : String) return Gnome_Stock_Pixmap_Entry
   --  is
   --     function Internal
   --       (Icon     : String;
   --        Sub_Type : String) return System.Address;
   --     pragma Import (C, Internal, "gnome_stock_pixmap_checkfor");
   --     Tmp : Gnome_Stock_Pixmap_Entry;
   --  begin
   --     return Gnome_Stock_Pixmap_Entry (Convert
   --       (Internal (Icon & ASCII.NUL, Sub_Type & ASCII.NUL)));
   --  end Pixmap_Checkfor;

   ----------------
   -- Pixmap_Gdk --
   ----------------

   procedure Pixmap_Gdk
     (Icon     : String;
      Sub_Type : String;
      Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
      Mask     : Gdk.Pixmap.Gdk_Pixmap)
   is
      procedure Internal
        (Icon     : String;
         Sub_Type : String;
         Pixmap   : Gdk.Pixmap.Gdk_Pixmap;
         Mask     : Gdk.Pixmap.Gdk_Pixmap);
      pragma Import (C, Internal, "gnome_stock_pixmap_gdk");
   begin
      Internal (Icon & ASCII.NUL, Sub_Type & ASCII.NUL, Pixmap, Mask);
   end Pixmap_Gdk;

   ---------------------
   -- Pixmap_Register --
   ---------------------

   --  function Pixmap_Register
   --    (Icon     : String;
   --     Sub_Type : String;
   --     GEntry   : access Gnome_Stock_Pixmap_Entry_Record)
   --     return Gint
   --  is
   --     function Internal
   --       (Icon     : String;
   --        Sub_Type : String;
   --        GEntry   : System.Address)
   --        return Gint;
   --     pragma Import (C, Internal, "gnome_stock_pixmap_register");
   --  begin
   --     return Internal (Icon & ASCII.NUL,
   --                      Sub_Type & ASCII.NUL,
   --                      Get_Object (GEntry));
   --  end Pixmap_Register;

   -------------------
   -- Pixmap_Widget --
   -------------------

   function Pixmap_Widget
     (Window : access Gtk.Widget.Gtk_Widget_Record'Class;
      Icon   : String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Window : System.Address;
         Icon   : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_stock_pixmap_widget");
   begin
      return Widget.Convert (Internal (Get_Object (Window), Icon & ASCII.NUL));
   end Pixmap_Widget;

   ---------------------------
   -- Pixmap_Widget_At_Size --
   ---------------------------

   function Pixmap_Widget_At_Size
     (Window : access Gtk.Widget.Gtk_Widget_Record'Class;
      Icon   : String;
      Width  : Guint;
      Height : Guint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Window : System.Address;
         Icon   : String;
         Width  : Guint;
         Height : Guint)
         return System.Address;
      pragma Import (C, Internal, "gnome_stock_pixmap_widget_at_size");
   begin
      return Widget.Convert
        (Internal (Get_Object (Window), Icon & ASCII.NUL, Width, Height));
   end Pixmap_Widget_At_Size;

   ----------------------------
   -- Pixmap_Widget_Set_Icon --
   ----------------------------

   procedure Pixmap_Widget_Set_Icon
     (Widget : access Gnome_Stock_Record;
      Icon   : String)
   is
      procedure Internal
        (Widget : System.Address;
         Icon   : String);
      pragma Import (C, Internal, "gnome_stock_pixmap_widget_set_icon");
   begin
      Internal (Get_Object (Widget), Icon & ASCII.NUL);
   end Pixmap_Widget_Set_Icon;

   --------------
   -- Set_Icon --
   --------------

   function Set_Icon
     (Stock  : access Gnome_Stock_Record;
      Icon   : String) return Boolean
   is
      function Internal
        (Stock  : System.Address;
         Icon   : String) return Gint;
      pragma Import (C, Internal, "gnome_stock_set_icon");
   begin
      return Boolean'Val (Internal (Get_Object (Stock), Icon & ASCII.NUL));
   end Set_Icon;

   ------------------------
   -- Transparent_Window --
   ------------------------

   function Transparent_Window
     (Icon     : String;
      Sub_Type : String) return Gtk.Widget.Gtk_Widget
   is
      function Internal
        (Icon     : String;
         Sub_Type : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_stock_transparent_window");
   begin
      return
        Widget.Convert (Internal (Icon & ASCII.NUL, Sub_Type & ASCII.NUL));
   end Transparent_Window;

end Gnome.Stock;
