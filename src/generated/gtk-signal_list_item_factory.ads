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

--  Emits signals to manage listitems.
--
--  Signals are emitted for every listitem in the same order:
--
--  1. [signalGtk.SignalListItemFactory::setup] is emitted to set up permanent
--  things on the listitem. This usually means constructing the widgets used in
--  the row and adding them to the listitem.
--
--  2. [signalGtk.SignalListItemFactory::bind] is emitted to bind the item
--  passed via [propertyGtk.ListItem:item] to the widgets that have been
--  created in step 1 or to add item-specific widgets. Signals are connected to
--  listen to changes - both to changes in the item to update the widgets or to
--  changes in the widgets to update the item. After this signal has been
--  called, the listitem may be shown in a list widget.
--
--  3. [signalGtk.SignalListItemFactory::unbind] is emitted to undo everything
--  done in step 2. Usually this means disconnecting signal handlers. Once this
--  signal has been called, the listitem will no longer be used in a list
--  widget.
--
--  4. [signalGtk.SignalListItemFactory::bind] and
--  [signalGtk.SignalListItemFactory::unbind] may be emitted multiple times
--  again to bind the listitem for use with new items. By reusing listitems,
--  potentially costly setup can be avoided. However, it means code needs to
--  make sure to properly clean up the listitem in step 3 so that no
--  information from the previous use leaks into the next one.
--
--  5. [signalGtk.SignalListItemFactory::teardown] is emitted to allow undoing
--  the effects of [signalGtk.SignalListItemFactory::setup]. After this signal
--  was emitted on a listitem, the listitem will be destroyed and not be used
--  again.
--
--  Note that during the signal emissions, changing properties on the
--  listitems passed will not trigger notify signals as the listitem's
--  notifications are frozen. See [methodGobject.Object.freeze_notify] for
--  details.
--
--  For tracking changes in other properties in the listitem, the ::notify
--  signal is recommended. The signal can be connected in the
--  [signalGtk.SignalListItemFactory::setup] signal and removed again during
--  [signalGtk.SignalListItemFactory::teardown].
--
--  <group>Trees and Lists</group>
--  <gtkada_demo>create_column_view.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Gtk.List_Item;         use Gtk.List_Item;
with Gtk.List_Item_Factory; use Gtk.List_Item_Factory;

package Gtk.Signal_List_Item_Factory is

   type Gtk_Signal_List_Item_Factory_Record is new Gtk_List_Item_Factory_Record with null record;
   type Gtk_Signal_List_Item_Factory is access all Gtk_Signal_List_Item_Factory_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Signal_List_Item_Factory);
   procedure Initialize
      (Self : not null access Gtk_Signal_List_Item_Factory_Record'Class);
   --  Creates a new `GtkSignalListItemFactory`.
   --  You need to connect signal handlers before you use it.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Signal_List_Item_Factory_New return Gtk_Signal_List_Item_Factory;
   --  Creates a new `GtkSignalListItemFactory`.
   --  You need to connect signal handlers before you use it.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_signal_list_item_factory_get_type");

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void is not null access procedure
     (Self   : access Gtk_Signal_List_Item_Factory_Record'Class;
      Object : not null access Gtk.List_Item.Gtk_List_Item_Record'Class);

   type Cb_GObject_Gtk_List_Item_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : not null access Gtk.List_Item.Gtk_List_Item_Record'Class);

   Signal_Bind : constant Glib.Signal_Name := "bind";
   procedure On_Bind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False);
   procedure On_Bind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an object has been bound to an item.
   --
   --  The handler for this signal must set to populate the listitem with
   --  widgets.
   --
   --  After this signal was emitted, the object might be shown in a
   --  [classGtk.ListView] or other widget.
   --
   --  The [signalGtk.SignalListItemFactory::unbind] signal is the opposite of
   --  this signal and can be used to undo everything done in this signal.

   Signal_Setup : constant Glib.Signal_Name := "setup";
   procedure On_Setup
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False);
   procedure On_Setup
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when a newly created listitem needs to be prepared for use.
   --
   --  It is the first signal emitted for every listitem.
   --
   --  The handler for this signal must call [methodGtk.ListItem.set_child] to
   --  populate the listitem with widgets.
   --
   --  The [signalGtk.SignalListItemFactory::teardown] signal is the opposite
   --  of this signal and can be used to undo everything done in this signal.

   Signal_Teardown : constant Glib.Signal_Name := "teardown";
   procedure On_Teardown
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False);
   procedure On_Teardown
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an object is about to be destroyed.
   --
   --  It is the last signal ever emitted for this Object.
   --
   --  This signal is the opposite of the
   --  [signalGtk.SignalListItemFactory::setup] signal and should be used to
   --  undo everything done in that signal.

   Signal_Unbind : constant Glib.Signal_Name := "unbind";
   procedure On_Unbind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_Gtk_Signal_List_Item_Factory_Gtk_List_Item_Void;
       After : Boolean := False);
   procedure On_Unbind
      (Self  : not null access Gtk_Signal_List_Item_Factory_Record;
       Call  : Cb_GObject_Gtk_List_Item_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when an object has been unbound from its item.
   --
   --  This happens for example when a listitem was removed from use in a list
   --  widget and its [propertyGtk.ListItem:item] is about to be unset.
   --
   --  This signal is the opposite of the
   --  [signalGtk.SignalListItemFactory::bind] signal and should be used to
   --  undo everything done in that signal.

end Gtk.Signal_List_Item_Factory;
