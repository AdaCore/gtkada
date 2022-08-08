------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

package body Gtk.Menu_Bar is

   package Type_Conversion_Gtk_Menu_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Menu_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Menu_Bar);

   ----------------------
   -- Gtk_Menu_Bar_New --
   ----------------------

   function Gtk_Menu_Bar_New return Gtk_Menu_Bar is
      Menu_Bar : constant Gtk_Menu_Bar := new Gtk_Menu_Bar_Record;
   begin
      Gtk.Menu_Bar.Initialize (Menu_Bar);
      return Menu_Bar;
   end Gtk_Menu_Bar_New;

   ---------------------------------
   -- Gtk_Menu_Bar_New_From_Model --
   ---------------------------------

   function Gtk_Menu_Bar_New_From_Model
      (Model : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Menu_Bar
   is
      Menu_Bar : constant Gtk_Menu_Bar := new Gtk_Menu_Bar_Record;
   begin
      Gtk.Menu_Bar.Initialize_From_Model (Menu_Bar, Model);
      return Menu_Bar;
   end Gtk_Menu_Bar_New_From_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Menu_Bar : out Gtk_Menu_Bar) is
   begin
      Menu_Bar := new Gtk_Menu_Bar_Record;
      Gtk.Menu_Bar.Initialize (Menu_Bar);
   end Gtk_New;

   ------------------------
   -- Gtk_New_From_Model --
   ------------------------

   procedure Gtk_New_From_Model
      (Menu_Bar : out Gtk_Menu_Bar;
       Model    : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
   begin
      Menu_Bar := new Gtk_Menu_Bar_Record;
      Gtk.Menu_Bar.Initialize_From_Model (Menu_Bar, Model);
   end Gtk_New_From_Model;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Menu_Bar : not null access Gtk_Menu_Bar_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_bar_new");
   begin
      if not Menu_Bar.Is_Created then
         Set_Object (Menu_Bar, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_From_Model --
   ---------------------------

   procedure Initialize_From_Model
      (Menu_Bar : not null access Gtk_Menu_Bar_Record'Class;
       Model    : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
   is
      function Internal (Model : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_menu_bar_new_from_model");
   begin
      if not Menu_Bar.Is_Created then
         Set_Object (Menu_Bar, Internal (Get_Object (Model)));
      end if;
   end Initialize_From_Model;

   ------------------------------
   -- Get_Child_Pack_Direction --
   ------------------------------

   function Get_Child_Pack_Direction
      (Menu_Bar : not null access Gtk_Menu_Bar_Record)
       return Gtk.Enums.Gtk_Pack_Direction
   is
      function Internal
         (Menu_Bar : System.Address) return Gtk.Enums.Gtk_Pack_Direction;
      pragma Import (C, Internal, "gtk_menu_bar_get_child_pack_direction");
   begin
      return Internal (Get_Object (Menu_Bar));
   end Get_Child_Pack_Direction;

   ------------------------
   -- Get_Pack_Direction --
   ------------------------

   function Get_Pack_Direction
      (Menu_Bar : not null access Gtk_Menu_Bar_Record)
       return Gtk.Enums.Gtk_Pack_Direction
   is
      function Internal
         (Menu_Bar : System.Address) return Gtk.Enums.Gtk_Pack_Direction;
      pragma Import (C, Internal, "gtk_menu_bar_get_pack_direction");
   begin
      return Internal (Get_Object (Menu_Bar));
   end Get_Pack_Direction;

   ------------------------------
   -- Set_Child_Pack_Direction --
   ------------------------------

   procedure Set_Child_Pack_Direction
      (Menu_Bar       : not null access Gtk_Menu_Bar_Record;
       Child_Pack_Dir : Gtk.Enums.Gtk_Pack_Direction)
   is
      procedure Internal
         (Menu_Bar       : System.Address;
          Child_Pack_Dir : Gtk.Enums.Gtk_Pack_Direction);
      pragma Import (C, Internal, "gtk_menu_bar_set_child_pack_direction");
   begin
      Internal (Get_Object (Menu_Bar), Child_Pack_Dir);
   end Set_Child_Pack_Direction;

   ------------------------
   -- Set_Pack_Direction --
   ------------------------

   procedure Set_Pack_Direction
      (Menu_Bar : not null access Gtk_Menu_Bar_Record;
       Pack_Dir : Gtk.Enums.Gtk_Pack_Direction)
   is
      procedure Internal
         (Menu_Bar : System.Address;
          Pack_Dir : Gtk.Enums.Gtk_Pack_Direction);
      pragma Import (C, Internal, "gtk_menu_bar_set_pack_direction");
   begin
      Internal (Get_Object (Menu_Bar), Pack_Dir);
   end Set_Pack_Direction;

end Gtk.Menu_Bar;
