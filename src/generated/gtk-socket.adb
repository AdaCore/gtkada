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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Socket is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Socket_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Socket) is
   begin
      Self := new Gtk_Socket_Record;
      Gtk.Socket.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_Socket_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_socket_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   ------------
   -- Add_Id --
   ------------

   procedure Add_Id
      (Self   : not null access Gtk_Socket_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class)
   is
      procedure Internal (Self : System.Address; Window : System.Address);
      pragma Import (C, Internal, "gtk_socket_add_id");
   begin
      Internal (Get_Object (Self), Get_Object (Window));
   end Add_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
      (Self : not null access Gtk_Socket_Record)
       return Gtk.Window.Gtk_Window
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_socket_get_id");
      Stub_Gtk_Window : Gtk.Window.Gtk_Window_Record;
   begin
      return Gtk.Window.Gtk_Window (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Window));
   end Get_Id;

   ---------------------
   -- Get_Plug_Window --
   ---------------------

   function Get_Plug_Window
      (Self : not null access Gtk_Socket_Record)
       return Gdk.Window.Gdk_Window
   is
      function Internal (Self : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_socket_get_plug_window");
   begin
      return Internal (Get_Object (Self));
   end Get_Plug_Window;

end Gtk.Socket;
