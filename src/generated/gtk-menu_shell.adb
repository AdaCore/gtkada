------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Menu_Shell is

   package Type_Conversion_Gtk_Menu_Shell is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Shell_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Shell);

   -------------------
   -- Activate_Item --
   -------------------

   procedure Activate_Item
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Menu_Item        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Force_Deactivate : Boolean)
   is
      procedure Internal
         (Menu_Shell       : System.Address;
          Menu_Item        : System.Address;
          Force_Deactivate : Integer);
      pragma Import (C, Internal, "gtk_menu_shell_activate_item");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Menu_Item), Boolean'Pos (Force_Deactivate));
   end Activate_Item;

   ------------
   -- Append --
   ------------

   procedure Append
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_append");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Append;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Menu_Shell : not null access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_cancel");
   begin
      Internal (Get_Object (Menu_Shell));
   end Cancel;

   ----------------
   -- Deactivate --
   ----------------

   procedure Deactivate (Menu_Shell : not null access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deactivate");
   begin
      Internal (Get_Object (Menu_Shell));
   end Deactivate;

   --------------
   -- Deselect --
   --------------

   procedure Deselect (Menu_Shell : not null access Gtk_Menu_Shell_Record) is
      procedure Internal (Menu_Shell : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_deselect");
   begin
      Internal (Get_Object (Menu_Shell));
   end Deselect;

   ----------------------
   -- Get_Parent_Shell --
   ----------------------

   function Get_Parent_Shell
      (Menu_Shell : not null access Gtk_Menu_Shell_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Shell : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_shell_get_parent_shell");
      Stub_1836 : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu_Shell)), Stub_1836));
   end Get_Parent_Shell;

   -----------------------
   -- Get_Selected_Item --
   -----------------------

   function Get_Selected_Item
      (Menu_Shell : not null access Gtk_Menu_Shell_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Menu_Shell : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_shell_get_selected_item");
      Stub_1838 : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Menu_Shell)), Stub_1838));
   end Get_Selected_Item;

   --------------------
   -- Get_Take_Focus --
   --------------------

   function Get_Take_Focus
      (Menu_Shell : not null access Gtk_Menu_Shell_Record) return Boolean
   is
      function Internal (Menu_Shell : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_menu_shell_get_take_focus");
   begin
      return Boolean'Val (Internal (Get_Object (Menu_Shell)));
   end Get_Take_Focus;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position   : Gint)
   is
      procedure Internal
         (Menu_Shell : System.Address;
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
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Child      : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_prepend");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Child));
   end Prepend;

   ------------------
   -- Select_First --
   ------------------

   procedure Select_First
      (Menu_Shell       : not null access Gtk_Menu_Shell_Record;
       Search_Sensitive : Boolean)
   is
      procedure Internal
         (Menu_Shell       : System.Address;
          Search_Sensitive : Integer);
      pragma Import (C, Internal, "gtk_menu_shell_select_first");
   begin
      Internal (Get_Object (Menu_Shell), Boolean'Pos (Search_Sensitive));
   end Select_First;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Menu_Item  : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Menu_Shell : System.Address;
          Menu_Item  : System.Address);
      pragma Import (C, Internal, "gtk_menu_shell_select_item");
   begin
      Internal (Get_Object (Menu_Shell), Get_Object (Menu_Item));
   end Select_Item;

   --------------------
   -- Set_Take_Focus --
   --------------------

   procedure Set_Take_Focus
      (Menu_Shell : not null access Gtk_Menu_Shell_Record;
       Take_Focus : Boolean)
   is
      procedure Internal (Menu_Shell : System.Address; Take_Focus : Integer);
      pragma Import (C, Internal, "gtk_menu_shell_set_take_focus");
   begin
      Internal (Get_Object (Menu_Shell), Boolean'Pos (Take_Focus));
   end Set_Take_Focus;

end Gtk.Menu_Shell;
