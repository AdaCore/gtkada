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
--  The Gtk.Cell_Editable.Gtk_Cell_Editable interface must be implemented for
--  widgets to be usable to edit the contents of a Gtk.Tree_View.Gtk_Tree_View
--  cell. It provides a way to specify how temporary widgets should be
--  configured for editing, get the new value, etc.
--
--  </description>
--  <group>Trees and Lists</group>

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
   --  Begins editing on a Cell_Editable.
   --  The Gtk.Cell_Renderer.Gtk_Cell_Renderer for the cell creates and
   --  returns a Gtk.Cell_Editable.Gtk_Cell_Editable from
   --  Gtk.Cell_Renderer.Start_Editing, configured for the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer type.
   --  Gtk.Cell_Editable.Start_Editing can then set up Cell_Editable suitably
   --  for editing a cell, e.g. making the Esc key emit
   --  Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done.
   --  Note that the Cell_Editable is created on-demand for the current edit;
   --  its lifetime is temporary and does not persist across other edits and/or
   --  cells.
   --  "event": The Gdk.Event.Gdk_Event that began the editing process, or
   --  null if editing was initiated programmatically

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
   --  Gtk.GEntry.Gtk_Entry emits this signal when the user presses Enter.
   --  Typical things to do in a handler for ::editing-done are to capture the
   --  edited value, disconnect the Cell_Editable from signals on the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer, etc.
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
   --  the Cell_Editable widget is being removed and may subsequently be
   --  destroyed.
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

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Editing_Done is access procedure (Cell_Editable : Gtk_Cell_Editable);
   pragma Convention (C, Virtual_Editing_Done);
   --  Emits the Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done signal.

   type Virtual_Remove_Widget is access procedure (Cell_Editable : Gtk_Cell_Editable);
   pragma Convention (C, Virtual_Remove_Widget);
   --  Emits the Gtk.Cell_Editable.Gtk_Cell_Editable::remove-widget signal.

   type Virtual_Start_Editing is access procedure
     (Cell_Editable : Gtk_Cell_Editable;
      Event         : Gdk.Event.Gdk_Event);
   pragma Convention (C, Virtual_Start_Editing);
   --  Begins editing on a Cell_Editable.
   --  The Gtk.Cell_Renderer.Gtk_Cell_Renderer for the cell creates and
   --  returns a Gtk.Cell_Editable.Gtk_Cell_Editable from
   --  gtk_cell_renderer_start_editing, configured for the
   --  Gtk.Cell_Renderer.Gtk_Cell_Renderer type.
   --  Gtk.Cell_Editable.Start_Editing can then set up Cell_Editable suitably
   --  for editing a cell, e.g. making the Esc key emit
   --  Gtk.Cell_Editable.Gtk_Cell_Editable::editing-done.
   --  Note that the Cell_Editable is created on-demand for the current edit;
   --  its lifetime is temporary and does not persist across other edits and/or
   --  cells.
   --  "event": The Gdk.Event.Gdk_Event that began the editing process, or
   --  null if editing was initiated programmatically

   subtype Cell_Editable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Editing_Done
     (Self    : Cell_Editable_Interface_Descr;
      Handler : Virtual_Editing_Done);
   pragma Import (C, Set_Editing_Done, "gtkada_Cell_Editable_set_editing_done");

   procedure Set_Remove_Widget
     (Self    : Cell_Editable_Interface_Descr;
      Handler : Virtual_Remove_Widget);
   pragma Import (C, Set_Remove_Widget, "gtkada_Cell_Editable_set_remove_widget");

   procedure Set_Start_Editing
     (Self    : Cell_Editable_Interface_Descr;
      Handler : Virtual_Start_Editing);
   pragma Import (C, Set_Start_Editing, "gtkada_Cell_Editable_set_start_editing");
   --  See Glib.Object.Add_Interface

private
   Editing_Canceled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("editing-canceled");

Null_Gtk_Cell_Editable : constant Gtk_Cell_Editable :=
   Gtk_Cell_Editable (Glib.Types.Null_Interface);
end Gtk.Cell_Editable;
