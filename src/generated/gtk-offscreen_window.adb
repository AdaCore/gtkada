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

package body Gtk.Offscreen_Window is

   package Type_Conversion_Gtk_Offscreen_Window is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Offscreen_Window_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Offscreen_Window);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Offscreen_Window) is
   begin
      Self := new Gtk_Offscreen_Window_Record;
      Gtk.Offscreen_Window.Initialize (Self);
   end Gtk_New;

   ------------------------------
   -- Gtk_Offscreen_Window_New --
   ------------------------------

   function Gtk_Offscreen_Window_New return Gtk_Offscreen_Window is
      Self : constant Gtk_Offscreen_Window := new Gtk_Offscreen_Window_Record;
   begin
      Gtk.Offscreen_Window.Initialize (Self);
      return Self;
   end Gtk_Offscreen_Window_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Offscreen_Window_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_offscreen_window_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
      (Self : not null access Gtk_Offscreen_Window_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_offscreen_window_get_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Pixbuf));
   end Get_Pixbuf;

   -----------------
   -- Get_Surface --
   -----------------

   function Get_Surface
      (Self : not null access Gtk_Offscreen_Window_Record)
       return Cairo.Cairo_Surface
   is
      function Internal (Self : System.Address) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gtk_offscreen_window_get_surface");
   begin
      return Internal (Get_Object (Self));
   end Get_Surface;

end Gtk.Offscreen_Window;
