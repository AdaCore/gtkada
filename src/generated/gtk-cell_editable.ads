------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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
--  The Gtk.Cell_Editable.Gtk_Cell_Editable interface must be implemented for
--  widgets to be usable when editing the contents of a
--  Gtk.Tree_View.Gtk_Tree_View cell.
--
--  </description>
--  <group>Trees and Lists</group>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Event;       use Gdk.Event;
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;

package Gtk.Cell_Editable is

   type Gtk_Cell_Editable is new Glib.Types.GType_Interface;
   Null_Gtk_Cell_Editable : constant Gtk_Cell_Editable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_editable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Editing_Done (Cell_Editable : Gtk_Cell_Editable);
   pragma Import (C, Editing_Done, "gtk_cell_editable_editing_done");
   --  Emits the Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done signal.

   procedure Remove_Widget (Cell_Editable : Gtk_Cell_Editable);
   pragma Import (C, Remove_Widget, "gtk_cell_editable_remove_widget");
   --  Emits the Gtk.Cell_Editable.Gtk_Cell_Editable::remove-widget signal.

   procedure Start_Editing
      (Cell_Editable : Gtk_Cell_Editable;
       Event         : Gdk.Event.Gdk_Event);
   pragma Import (C, Start_Editing, "gtk_cell_editable_start_editing");
   --  Begins editing on a Cell_Editable. Event is the Gdk.Event.Gdk_Event
   --  that began the editing process. It may be null, in the instance that
   --  editing was initiated through programatic means.
   --  "event": A Gdk.Event.Gdk_Event, or null

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Editing_Canceled_Property : constant Glib.Properties.Property_Boolean;
   --  Indicates whether editing on the cell has been canceled.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Editable_Void is not null access procedure (Self : Gtk_Cell_Editable);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Editing_Done : constant Glib.Signal_Name := "editing-done";
   procedure On_Editing_Done
      (Self  : Gtk_Cell_Editable;
       Call  : Cb_Gtk_Cell_Editable_Void;
       After : Boolean := False);
   procedure On_Editing_Done
      (Self  : Gtk_Cell_Editable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is a sign for the cell renderer to update its value from
   --  the Cell_Editable.
   --
   --  Implementations of Gtk.Cell_Editable.Gtk_Cell_Editable are responsible
   --  for emitting this signal when they are done editing, e.g.
   --  Gtk.GEntry.Gtk_Entry is emitting it when the user presses Enter.
   --
   --  Gtk.Cell_Editable.Editing_Done is a convenience method for emitting
   --  Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done.

   Signal_Remove_Widget : constant Glib.Signal_Name := "remove-widget";
   procedure On_Remove_Widget
      (Self  : Gtk_Cell_Editable;
       Call  : Cb_Gtk_Cell_Editable_Void;
       After : Boolean := False);
   procedure On_Remove_Widget
      (Self  : Gtk_Cell_Editable;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is meant to indicate that the cell is finished editing, and
   --  the widget may now be destroyed.
   --
   --  Implementations of Gtk.Cell_Editable.Gtk_Cell_Editable are responsible
   --  for emitting this signal when they are done editing. It must be emitted
   --  after the Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done signal, to
   --  give the cell renderer a chance to update the cell's value before the
   --  widget is removed.
   --
   --  Gtk.Cell_Editable.Remove_Widget is a convenience method for emitting
   --  Gtk.Cell_Editable.Gtk_Cell_Editable::remove-widget.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Cell_Editable"

   function "+" (W : Gtk_Cell_Editable) return Gtk_Cell_Editable;
   pragma Inline ("+");

private
   Editing_Canceled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editing-canceled");

Null_Gtk_Cell_Editable : constant Gtk_Cell_Editable :=
   Gtk_Cell_Editable (Glib.Types.Null_Interface);
end Gtk.Cell_Editable;
