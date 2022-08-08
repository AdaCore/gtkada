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

package body Gtk.Action_Bar is

   package Type_Conversion_Gtk_Action_Bar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Action_Bar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Action_Bar);

   ------------------------
   -- Gtk_Action_Bar_New --
   ------------------------

   function Gtk_Action_Bar_New return Gtk_Action_Bar is
      Self : constant Gtk_Action_Bar := new Gtk_Action_Bar_Record;
   begin
      Gtk.Action_Bar.Initialize (Self);
      return Self;
   end Gtk_Action_Bar_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Action_Bar) is
   begin
      Self := new Gtk_Action_Bar_Record;
      Gtk.Action_Bar.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Action_Bar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_action_bar_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Get_Center_Widget --
   -----------------------

   function Get_Center_Widget
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_action_bar_get_center_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Center_Widget;

   --------------
   -- Pack_End --
   --------------

   procedure Pack_End
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_action_bar_pack_end");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Pack_End;

   ----------------
   -- Pack_Start --
   ----------------

   procedure Pack_Start
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_action_bar_pack_start");
   begin
      Internal (Get_Object (Self), Get_Object (Child));
   end Pack_Start;

   -----------------------
   -- Set_Center_Widget --
   -----------------------

   procedure Set_Center_Widget
      (Self          : not null access Gtk_Action_Bar_Record;
       Center_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self          : System.Address;
          Center_Widget : System.Address);
      pragma Import (C, Internal, "gtk_action_bar_set_center_widget");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Center_Widget)));
   end Set_Center_Widget;

end Gtk.Action_Bar;
