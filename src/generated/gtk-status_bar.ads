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

--  <description>
--  A Gtk.Status_Bar.Gtk_Status_Bar is usually placed along the bottom of an
--  application's main Gtk.Window.Gtk_Window. It may provide a regular
--  commentary of the application's status (as is usually the case in a web
--  browser, for example), or may be used to simply output a message when the
--  status changes, (when an upload is complete in an FTP client, for example).
--
--  Status bars in GTK+ maintain a stack of messages. The message at the top
--  of the each bar's stack is the one that will currently be displayed.
--
--  Any messages added to a statusbar's stack must specify a context id that
--  is used to uniquely identify the source of a message. This context id can
--  be generated by Gtk.Status_Bar.Get_Context_Id, given a message and the
--  statusbar that it will be added to. Note that messages are stored in a
--  stack, and when choosing which message to display, the stack structure is
--  adhered to, regardless of the context identifier of a message.
--
--  One could say that a statusbar maintains one stack of messages for display
--  purposes, but allows multiple message producers to maintain sub-stacks of
--  the messages they produced (via context ids).
--
--  Status bars are created using Gtk.Status_Bar.Gtk_New.
--
--  Messages are added to the bar's stack with Gtk.Status_Bar.Push.
--
--  The message at the top of the stack can be removed using
--  Gtk.Status_Bar.Pop. A message can be removed from anywhere in the stack if
--  its message id was recorded at the time it was added. This is done using
--  Gtk.Status_Bar.Remove.
--
--  # CSS node
--
--  GtkStatusbar has a single CSS node with name statusbar.
--
--  </description>
--  <screenshot>gtk-status_bar</screenshot>
--  <group>Display widgets</group>
--  <testgtk>create_status.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;           use Glib;
with Glib.Object;    use Glib.Object;
with Glib.Types;     use Glib.Types;
with Gtk.Box;        use Gtk.Box;
with Gtk.Buildable;  use Gtk.Buildable;
with Gtk.Enums;      use Gtk.Enums;
with Gtk.Orientable; use Gtk.Orientable;
with Gtk.Widget;     use Gtk.Widget;

package Gtk.Status_Bar is

   type Gtk_Status_Bar_Record is new Gtk_Box_Record with null record;
   type Gtk_Status_Bar is access all Gtk_Status_Bar_Record'Class;

   type Context_Id is new Guint;
   type Message_Id is new Guint;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Statusbar : out Gtk_Status_Bar);
   procedure Initialize
      (Statusbar : not null access Gtk_Status_Bar_Record'Class);
   --  Creates a new Gtk.Status_Bar.Gtk_Status_Bar ready for messages.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Status_Bar_New return Gtk_Status_Bar;
   --  Creates a new Gtk.Status_Bar.Gtk_Status_Bar ready for messages.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_statusbar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Context_Id
      (Statusbar           : not null access Gtk_Status_Bar_Record;
       Context_Description : UTF8_String) return Context_Id;
   --  Returns a new context identifier, given a description of the actual
   --  context. Note that the description is not shown in the UI.
   --  "context_description": textual description of what context the new
   --  message is being used in

   function Get_Message_Area
      (Statusbar : not null access Gtk_Status_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the box containing the label widget.
   --  Since: gtk+ 2.20

   procedure Pop
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id);
   --  Removes the first message in the Gtk.Status_Bar.Gtk_Status_Bar's stack
   --  with the given context id.
   --  Note that this may not change the displayed message, if the message at
   --  the top of the stack has a different context id.
   --  "context": a context identifier

   function Push
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Text      : UTF8_String) return Message_Id;
   --  Pushes a new message onto a statusbar's stack.
   --  "context": the message's context id, as returned by
   --  Gtk.Status_Bar.Get_Context_Id
   --  "text": the message to add to the statusbar

   procedure Remove
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id;
       Message   : Message_Id);
   --  Forces the removal of a message from a statusbar's stack. The exact
   --  Context_Id and Message_Id must be specified.
   --  "context": a context identifier
   --  "Message": a message identifier, as returned by Gtk.Status_Bar.Push

   procedure Remove_All
      (Statusbar : not null access Gtk_Status_Bar_Record;
       Context   : Context_Id);
   --  Forces the removal of all messages from a statusbar's stack with the
   --  exact Context_Id.
   --  Since: gtk+ 2.22
   --  "context": a context identifier

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Orientation
      (Self : not null access Gtk_Status_Bar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Status_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void is not null access procedure
     (Self    : access Gtk_Status_Bar_Record'Class;
      Context : Context_Id;
      Text    : UTF8_String);

   type Cb_GObject_Context_Id_UTF8_String_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Context : Context_Id;
      Text    : UTF8_String);

   Signal_Text_Popped : constant Glib.Signal_Name := "text-popped";
   procedure On_Text_Popped
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Text_Popped
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_GObject_Context_Id_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Is emitted whenever a new message is popped off a statusbar's stack.
   -- 
   --  Callback parameters:
   --    --  "context": the context id of the relevant message/statusbar
   --    --  "text": the message that was just popped

   Signal_Text_Pushed : constant Glib.Signal_Name := "text-pushed";
   procedure On_Text_Pushed
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_Gtk_Status_Bar_Context_Id_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Text_Pushed
      (Self  : not null access Gtk_Status_Bar_Record;
       Call  : Cb_GObject_Context_Id_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Is emitted whenever a new message gets pushed onto a statusbar's stack.
   -- 
   --  Callback parameters:
   --    --  "context": the context id of the relevant message/statusbar
   --    --  "text": the message that was pushed

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Status_Bar_Record, Gtk_Status_Bar);
   function "+"
     (Widget : access Gtk_Status_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Status_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Status_Bar_Record, Gtk_Status_Bar);
   function "+"
     (Widget : access Gtk_Status_Bar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Status_Bar
   renames Implements_Gtk_Orientable.To_Object;

end Gtk.Status_Bar;
