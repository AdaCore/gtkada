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

--  <description>
--  A status bar is a special widget in which you can display messages.
--  This type of widget is generally found at the bottom of application
--  windows, and is used to display help or error messages.
--
--  This widget works as a stack of messages, ie all older messages are
--  kept when a new one is inserted. It is of course possible to remove the
--  most recent message from the stack.
--  This stack behavior is especially useful when messages can be displayed
--  from several places in your application. Thus, each one subprogram that
--  needs to print a message can simply push it on the stack, and does not
--  need to make sure that the user has had enough time to read the previous
--  message (a timeout can be set to automatically remove the message after
--  a specific delay)
--
--  Each message is associated with a specific Context_Id. Each of this context
--  can have a special name, and these context can be used to organize the
--  messages into categories (for instance one for help messages and one for
--  error messages). You can then selectively remove the most recent message
--  of each category.
--  </description>
--  <c_version>1.3.4</c_version>

with Gtk.Box;
with Interfaces.C.Strings;
with Glib.GSlist;
with System;

package Gtk.Status_Bar is

   type Gtk_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtk_Status_Bar is access all Gtk_Status_Bar_Record'Class;

   subtype Gtk_Statusbar is Gtk_Status_Bar;
   --  This is needed by Gate since the C name is GtkStatusbar

   type Context_Id is new Guint;
   type Message_Id is new Guint;

   type Status_Bar_Msg is record
      Text    : Interfaces.C.Strings.chars_ptr;
      Context : Context_Id;
      Message : Message_Id;
   end record;
   --  A message from the queue. Each of this message is associated with a
   --  specific context, and has a specific number.

   --  <no_doc>
   function Convert (Msg : Status_Bar_Msg) return System.Address;
   function Convert (Msg : System.Address) return Status_Bar_Msg;
   package Messages_List is new Glib.GSlist.Generic_SList (Status_Bar_Msg);
   --  </no_doc>

   procedure Gtk_New (Statusbar : out Gtk_Status_Bar);
   --  Create a new status bar, in which messages will be displayed.

   procedure Initialize (Statusbar : access Gtk_Status_Bar_Record'Class);
   --  Internal initialization function.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Gtk_Status_Bar.

   function Get_Context_Id
     (Statusbar           : access Gtk_Status_Bar_Record;
      Context_Description : String) return Context_Id;
   --  Create the context id associated with a special name.
   --  If no context is currently associated with Context_Description, then
   --  a new context is created.

   function Get_Messages
     (Statusbar : access Gtk_Status_Bar_Record) return Messages_List.GSlist;
   --  Return a list of all the messages currently stored in the queue.
   --  The first item in the list is the most recent message.

   function Push
     (Statusbar : access Gtk_Status_Bar_Record;
      Context   : Context_Id;
      Text      : String) return Message_Id;
   --  Push a new message on the queue, associated with a specific context.
   --  This message is directly displayed in the status bar.
   --  A new unique message id is associated with this message.

   procedure Pop
     (Statusbar : access Gtk_Status_Bar_Record;
      Context   : Context_Id);
   --  Remove the most recent message from a specific context. All other
   --  contexts are ignored, and no error is raised if there is no message in
   --  Context.

   procedure Remove
     (Statusbar  : access Gtk_Status_Bar_Record;
      Context    : Context_Id;
      Message    : Message_Id);
   --  Remove a message from the list.
   --  The message is only removed if it is in a specific context.
   --  Nothing happens if no matching message is found.

   procedure Set_Has_Resize_Grip
     (Statusbar  : access Gtk_Status_Bar_Record;
      Setting    : Boolean);
   --  Set the value of the resize_grip attribute for a given status bar.

   function Get_Has_Resize_Grip
     (Statusbar : access Gtk_Status_Bar_Record) return Boolean;
   --  Return the value of the resize_grip attribute for a given status bar.

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  </properties>

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "text_pushed"
   --    procedure Handler (Status_Bar : access Gtk_Status_Bar_Record'Class;
   --                       Context    : Context_Id;
   --                       Text       : Interfaces.C.Strings.chars_ptr);
   --
   --    Emitted when a new message has been in the queue.
   --
   --  - "text_popped"
   --    procedure Handler (Status_Bar : access Gtk_Status_Bar_Record'Class;
   --                       Context    : Context_Id;
   --                       Text       : Interfaces.C.Strings.chars_ptr);
   --
   --    Emitted when a message has been removed from the queue.
   --
   --  </signals>

private
   type Gtk_Status_Bar_Record is new Gtk.Box.Gtk_Box_Record with null record;

   pragma Import (C, Get_Type, "gtk_statusbar_get_type");
end Gtk.Status_Bar;
