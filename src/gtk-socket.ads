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

--  <description>
--  Together with Gtk_Plug, Gtk_Socket provides the ability to embed widgets
--  from one process into another process in a fashion that is transparent to
--  the user. One process creates a Gtk_Socket widget and, passes the XID of
--  that widget's window to the other process, which then creates a Gtk_Plug
--  window with that XID.
--  Any widgets contained in the Gtk_Plug then will appear inside the first
--  applications window.
--
--  The XID of the socket's window is obtained by using the
--  GTK_WINDOW_XWINDOW() macro from the header file <gdk/gdkx.h>. Before using
--  this macro, the socket must have been realized, and for hence, have been
--  added to its parent.
--
--  Note that if you pass the XID of the socket to another process that will
--  create a plug in the socket, you must make sure that the socket widget is
--  not destroyed until that plug is created. Violating this rule will cause
--  unpredictable consequences, the most likely consequence being that the plug
--  will appear as a separate toplevel window. You can check if the plug has
--  been created by examining the plug_window field of the Gtk_Socket
--  structure. If this field is non-NULL, then the plug has been succesfully
--  created inside of the socket.
--
--  When GtkAda is notified that the embedded window has been destroyed, then
--  it will destroy the socket as well. You should always, therefore, be
--  prepared for your sockets to be destroyed at any time when the main event
--  loop is running.
--
--  A socket can also be used to swallow arbitrary pre-existing top-level
--  windows using Steal, though the integration when this is done will not be
--  as close as between a Gtk_Plug and a Gtk_Socket.
--  </description>
--  <c_version>1.2.8</c_version>

with Gtk.Container;
with Gdk.Window;

package Gtk.Socket is

   type Gtk_Socket_Record is new Gtk.Container.Gtk_Container_Record
     with private;
   type Gtk_Socket is access all Gtk_Socket_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Socket);
   --  Create a new empty GtkSocket.

   procedure Initialize (Widget : access Gtk_Socket_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Steal
     (Socket : access Gtk_Socket_Record;
      Wid    : in     Guint32);
   --  Reparent a pre-existing toplevel window into a Gtk_Socket.
   --  Wid is the XID of an existing toplevel window.

   function Get_Plug_Window (Socket : access Gtk_Socket_Record)
                            return Gdk.Window.Gdk_Window;
   --  Return the id of the embedded window.

private

   type Gtk_Socket_Record is new Gtk.Container.Gtk_Container_Record
     with null record;

end Gtk.Socket;

--  <example>
--  Obtaining the XID of a socket
--
--  with Gtk.Socket;
--  use Gtk.Socket;
--
--  Socket : Gtk_Socket;
--
--  Gtk_New (Socket);
--  Show (Socket);
--  Add (Parent, Socket);
--
--  --  The following call is only necessary if one of
--  --  the ancestors of the socket is not yet visible.
--
--  Realize (Socket);
--  Put_Line ("The XID of the sockets window is" &
--            Guint32'Image (XWINDOW (Get_Window (Socket))));
--  </example>
