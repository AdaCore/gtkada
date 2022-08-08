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

package body Gtk.Invisible is

   package Type_Conversion_Gtk_Invisible is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Invisible_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Invisible);

   -----------------------
   -- Gtk_Invisible_New --
   -----------------------

   function Gtk_Invisible_New return Gtk_Invisible is
      Self : constant Gtk_Invisible := new Gtk_Invisible_Record;
   begin
      Gtk.Invisible.Initialize (Self);
      return Self;
   end Gtk_Invisible_New;

   ----------------------------------
   -- Gtk_Invisible_New_For_Screen --
   ----------------------------------

   function Gtk_Invisible_New_For_Screen
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
       return Gtk_Invisible
   is
      Self : constant Gtk_Invisible := new Gtk_Invisible_Record;
   begin
      Gtk.Invisible.Initialize_For_Screen (Self, Screen);
      return Self;
   end Gtk_Invisible_New_For_Screen;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Invisible) is
   begin
      Self := new Gtk_Invisible_Record;
      Gtk.Invisible.Initialize (Self);
   end Gtk_New;

   ------------------------
   -- Gtk_New_For_Screen --
   ------------------------

   procedure Gtk_New_For_Screen
      (Self   : out Gtk_Invisible;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
   begin
      Self := new Gtk_Invisible_Record;
      Gtk.Invisible.Initialize_For_Screen (Self, Screen);
   end Gtk_New_For_Screen;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Invisible_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_invisible_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_For_Screen --
   ---------------------------

   procedure Initialize_For_Screen
      (Self   : not null access Gtk_Invisible_Record'Class;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      function Internal (Screen : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_invisible_new_for_screen");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object (Screen)));
      end if;
   end Initialize_For_Screen;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
      (Self : not null access Gtk_Invisible_Record)
       return Gdk.Screen.Gdk_Screen
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_invisible_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Screen));
   end Get_Screen;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
      (Self   : not null access Gtk_Invisible_Record;
       Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal (Self : System.Address; Screen : System.Address);
      pragma Import (C, Internal, "gtk_invisible_set_screen");
   begin
      Internal (Get_Object (Self), Get_Object (Screen));
   end Set_Screen;

end Gtk.Invisible;
