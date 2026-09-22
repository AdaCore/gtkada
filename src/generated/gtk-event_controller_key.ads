------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  Provides access to key events.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Enums;            use Gdk.Enums;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Gtk.Event_Controller; use Gtk.Event_Controller;
with Gtk.IM_Context;       use Gtk.IM_Context;
with Gtk.Widget;           use Gtk.Widget;

package Gtk.Event_Controller_Key is

   type Gtk_Event_Controller_Key_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Event_Controller_Key is access all Gtk_Event_Controller_Key_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Event_Controller_Key);
   procedure Initialize
      (Self : not null access Gtk_Event_Controller_Key_Record'Class);
   --  Creates a new event controller that will handle key events.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Event_Controller_Key_New return Gtk_Event_Controller_Key;
   --  Creates a new event controller that will handle key events.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_event_controller_key_get_type");

   -------------
   -- Methods --
   -------------

   function Forward
      (Self   : not null access Gtk_Event_Controller_Key_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean;
   --  Forwards the current event of this Controller to a Widget.
   --  This function can only be used in handlers for the
   --  [signalGtk.EventControllerKey::key-pressed],
   --  [signalGtk.EventControllerKey::key-released] or
   --  [signalGtk.EventControllerKey::modifiers] signals.
   --  @param Widget a `GtkWidget`
   --  @return whether the Widget handled the event

   function Get_Group
      (Self : not null access Gtk_Event_Controller_Key_Record) return Guint;
   --  Gets the key group of the current event of this Controller.
   --  See [methodGdk.KeyEvent.get_layout].
   --  @return the key group

   function Get_Im_Context
      (Self : not null access Gtk_Event_Controller_Key_Record)
       return Gtk.IM_Context.Gtk_IM_Context;
   --  Gets the input method context of the key Controller.
   --  @return the `GtkIMContext`. Has transfer-ownership='none'.

   procedure Set_Im_Context
      (Self       : not null access Gtk_Event_Controller_Key_Record;
       Im_Context : access Gtk.IM_Context.Gtk_IM_Context_Record'Class);
   --  Sets the input method context of the key Controller.
   --  @param Im_Context a `GtkIMContext`

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Event_Controller_Key_Void is not null access procedure
     (Self : access Gtk_Event_Controller_Key_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Im_Update : constant Glib.Signal_Name := "im-update";
   procedure On_Im_Update
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Void;
       After : Boolean := False);
   procedure On_Im_Update
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the input method context filters away a keypress and
   --  prevents the Controller receiving it.
   --
   --  See [methodGtk.EventControllerKey.set_im_context] and
   --  [methodGtk.IMContext.filter_keypress].

   type Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean is not null access function
     (Self    : access Gtk_Event_Controller_Key_Record'Class;
      Keyval  : Guint;
      Keycode : Guint;
      State   : Gdk.Enums.Gdk_Modifier_Type) return Boolean;

   type Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean is not null access function
     (Self    : access Glib.Object.GObject_Record'Class;
      Keyval  : Guint;
      Keycode : Guint;
      State   : Gdk.Enums.Gdk_Modifier_Type) return Boolean;

   Signal_Key_Pressed : constant Glib.Signal_Name := "key-pressed";
   procedure On_Key_Pressed
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Boolean;
       After : Boolean := False);
   procedure On_Key_Pressed
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever a key is pressed.
   -- 
   --  Callback parameters:
   --    --  @param Keyval the pressed key.
   --    --  @param Keycode the raw code of the pressed key.
   --    --  @param State the bitmask, representing the state of modifier keys and
   --    --  pointer buttons.

   type Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void is not null access procedure
     (Self    : access Gtk_Event_Controller_Key_Record'Class;
      Keyval  : Guint;
      Keycode : Guint;
      State   : Gdk.Enums.Gdk_Modifier_Type);

   type Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void is not null access procedure
     (Self    : access Glib.Object.GObject_Record'Class;
      Keyval  : Guint;
      Keycode : Guint;
      State   : Gdk.Enums.Gdk_Modifier_Type);

   Signal_Key_Released : constant Glib.Signal_Name := "key-released";
   procedure On_Key_Released
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Guint_Guint_Gdk_Modifier_Type_Void;
       After : Boolean := False);
   procedure On_Key_Released
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Guint_Guint_Gdk_Modifier_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever a key is released.
   -- 
   --  Callback parameters:
   --    --  @param Keyval the released key.
   --    --  @param Keycode the raw code of the released key.
   --    --  @param State the bitmask, representing the state of modifier keys and
   --    --  pointer buttons.

   type Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean is not null access function
     (Self  : access Gtk_Event_Controller_Key_Record'Class;
      State : Gdk.Enums.Gdk_Modifier_Type) return Boolean;

   type Cb_GObject_Gdk_Modifier_Type_Boolean is not null access function
     (Self  : access Glib.Object.GObject_Record'Class;
      State : Gdk.Enums.Gdk_Modifier_Type) return Boolean;

   Signal_Modifiers : constant Glib.Signal_Name := "modifiers";
   procedure On_Modifiers
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_Gtk_Event_Controller_Key_Gdk_Modifier_Type_Boolean;
       After : Boolean := False);
   procedure On_Modifiers
      (Self  : not null access Gtk_Event_Controller_Key_Record;
       Call  : Cb_GObject_Gdk_Modifier_Type_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whenever the state of modifier keys and pointer buttons change.
   -- 
   --  Callback parameters:
   --    --  @param State the bitmask, representing the new state of modifier keys
   --    --  and pointer buttons.

end Gtk.Event_Controller_Key;
