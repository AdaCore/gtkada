-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gtk.Socket is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Socket) is
   begin
      Widget := new Gtk_Socket_Record;
      Initialize (Widget);
   end Gtk_New;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Widget : access Gtk_Socket_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_socket_new");

   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
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
      pragma Import (C, Internal, "ada_gtk_socket_get_plug_window");

   begin
      return Internal (Get_Object (Socket));
   end Get_Plug_Window;

   -----------------
   -- Get_XWindow --
   -----------------

   function Get_XWindow (Window : Gdk.Window.Gdk_Window) return Guint32 is
      function Internal (Window : Gdk.Window.Gdk_Window) return Guint32;
      pragma Import (C, Internal, "ada_get_xwindow");

   begin
      if Window = null then
         raise Constraint_Error;
      end if;

      return Internal (Window);
   end Get_XWindow;

end Gtk.Socket;
