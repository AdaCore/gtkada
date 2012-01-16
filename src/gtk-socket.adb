------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Socket is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Socket_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Socket) is
   begin
      Widget := new Gtk_Socket_Record;
      Gtk.Socket.Initialize (Widget);
   end Gtk_New;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Widget : access Gtk_Socket_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_socket_new");

   begin
      Set_Object (Widget, Internal);
   end Initialize;

   -----------
   -- Steal --
   -----------

   procedure Steal (Socket : access Gtk_Socket_Record; Wid : Guint32) is
      procedure Internal (Socket : System.Address; Wid : Guint32);
      pragma Import (C, Internal, "gtk_socket_steal");

   begin
      Internal (Get_Object (Socket), Wid);
   end Steal;

   ---------------------
   -- Get_Plug_Window --
   ---------------------

   function Get_Plug_Window
     (Socket : access Gtk_Socket_Record) return Gdk.Window.Gdk_Window
   is
      function Internal (Socket : System.Address) return Gdk.Window.Gdk_Window;
      pragma Import (C, Internal, "gtk_socket_get_plug_window");

   begin
      return Internal (Get_Object (Socket));
   end Get_Plug_Window;

   ------------
   -- Add_Id --
   ------------

   procedure Add_Id (Socket : access Gtk_Socket_Record; Id : Guint32) is
      procedure Internal (Socket : System.Address; Id : Guint32);
      pragma Import (C, Internal, "gtk_socket_add_id");

   begin
      Internal (Get_Object (Socket), Id);
   end Add_Id;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Socket : access Gtk_Socket_Record) return Guint32 is
      function Internal (Socket : System.Address) return Guint32;
      pragma Import (C, Internal, "gtk_socket_get_id");

   begin
      return Internal (Get_Object (Socket));
   end Get_Id;

end Gtk.Socket;
