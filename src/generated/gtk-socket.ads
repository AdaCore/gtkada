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
--  <description>
--  Together with Gtk.Plug.Gtk_Plug, Gtk.Socket.Gtk_Socket provides the
--  ability to embed widgets from one process into another process in a fashion
--  that is transparent to the user. One process creates a
--  Gtk.Socket.Gtk_Socket widget and passes that widget's window ID to the
--  other process, which then creates a Gtk.Plug.Gtk_Plug with that window ID.
--  Any widgets contained in the Gtk.Plug.Gtk_Plug then will appear inside the
--  first application's window.
--
--  The socket's window ID is obtained by using Gtk.Socket.Get_Id. Before
--  using this function, the socket must have been realized, and for hence,
--  have been added to its parent.
--
--  == Obtaining the window ID of a socket. ==
--
--    GtkWidget *socket = gtk_socket_new (<!-- -->);
--       gtk_widget_show (socket);
--       gtk_container_add (GTK_CONTAINER (parent), socket);
--       /&ast; The following call is only necessary if one of
--       * the ancestors of the socket is not yet visible.
--       &ast;/
--       gtk_widget_realize (socket);
--          g_print ("The ID of the sockets window is %#x\n",
--          gtk_socket_get_id (socket));
--
--  Note that if you pass the window ID of the socket to another process that
--  will create a plug in the socket, you must make sure that the socket widget
--  is not destroyed until that plug is created. Violating this rule will cause
--  unpredictable consequences, the most likely consequence being that the plug
--  will appear as a separate toplevel window. You can check if the plug has
--  been created by using Gtk.Socket.Get_Plug_Window. If it returns a non-null
--  value, then the plug has been successfully created inside of the socket.
--
--  When GTK+ is notified that the embedded window has been destroyed, then it
--  will destroy the socket as well. You should always, therefore, be prepared
--  for your sockets to be destroyed at any time when the main event loop is
--  running. To prevent this from happening, you can connect to the
--  Gtk.Socket.Gtk_Socket::plug-removed signal.
--
--  The communication between a Gtk.Socket.Gtk_Socket and a Gtk.Plug.Gtk_Plug
--  follows the <ulink
--  url="http://www.freedesktop.org/Standards/xembed-spec">XEmbed</ulink>
--  protocol. This protocol has also been implemented in other toolkits, e.g.
--  <application>Qt</application>, allowing the same level of integration when
--  embedding a <application>Qt</application> widget in GTK or vice versa.
--
--  Note: The Gtk.Plug.Gtk_Plug and Gtk.Socket.Gtk_Socket widgets are only
--  available when GTK+ is compiled for the X11 platform and GDK_WINDOWING_X11
--  is defined. They can only be used on a Gdk_X11_Display. To use
--  Gtk.Plug.Gtk_Plug and Gtk.Socket.Gtk_Socket, you need to include the
--  <filename>gtk/gtkx.h</filename> header.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Window;    use Gdk.Window;
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Window;    use Gtk.Window;

package Gtk.Socket is

   type Gtk_Socket_Record is new Gtk_Container_Record with null record;
   type Gtk_Socket is access all Gtk_Socket_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Socket);
   procedure Initialize (Self : access Gtk_Socket_Record'Class);
   --  Create a new empty Gtk.Socket.Gtk_Socket.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_socket_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Id
      (Self   : not null access Gtk_Socket_Record;
       Window : not null access Gtk.Window.Gtk_Window_Record'Class);
   --  Adds an XEMBED client, such as a Gtk.Plug.Gtk_Plug, to the
   --  Gtk.Socket.Gtk_Socket. The client may be in the same process or in a
   --  different process.
   --  To embed a Gtk.Plug.Gtk_Plug in a Gtk.Socket.Gtk_Socket, you can either
   --  create the Gtk.Plug.Gtk_Plug with <literal>gtk_plug_new (0)</literal>,
   --  call gtk_plug_get_id to get the window ID of the plug, and then pass
   --  that to the Gtk.Socket.Add_Id, or you can call Gtk.Socket.Get_Id to get
   --  the window ID for the socket, and call gtk_plug_new passing in that ID.
   --  The Gtk.Socket.Gtk_Socket must have already be added into a toplevel
   --  window before you can make this call.
   --  "window": the Window of a client participating in the XEMBED protocol.

   function Get_Id
      (Self : not null access Gtk_Socket_Record)
       return Gtk.Window.Gtk_Window;
   --  Gets the window ID of a Gtk.Socket.Gtk_Socket widget, which can then be
   --  used to create a client embedded inside the socket, for instance with
   --  gtk_plug_new.
   --  The Gtk.Socket.Gtk_Socket must have already be added into a toplevel
   --  window before you can make this call.

   function Get_Plug_Window
      (Self : not null access Gtk_Socket_Record)
       return Gdk.Window.Gdk_Window;
   --  Retrieves the window of the plug. Use this to check if the plug has
   --  been created inside of the socket.
   --  Since: gtk+ 2.14

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Socket_Record, Gtk_Socket);
   function "+"
     (Widget : access Gtk_Socket_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Socket
   renames Implements_Buildable.To_Object;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "plug-added"
   --     procedure Handler (Self : access Gtk_Socket_Record'Class);
   --  This signal is emitted when a client is successfully added to the
   --  socket.
   --
   --  "plug-removed"
   --     function Handler
   --       (Self : access Gtk_Socket_Record'Class) return Boolean;
   --  This signal is emitted when a client is removed from the socket. The
   --  default action is to destroy the Gtk.Socket.Gtk_Socket widget, so if you
   --  want to reuse it you must add a signal handler that returns True.
   --  Returns True to stop other handlers from being invoked.

   Signal_Plug_Added : constant Glib.Signal_Name := "plug-added";
   Signal_Plug_Removed : constant Glib.Signal_Name := "plug-removed";

end Gtk.Socket;
