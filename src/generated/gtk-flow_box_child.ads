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


pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.Types;    use Glib.Types;
with Gtk.Bin;       use Gtk.Bin;
with Gtk.Buildable; use Gtk.Buildable;

package Gtk.Flow_Box_Child is

   type Gtk_Flow_Box_Child_Record is new Gtk_Bin_Record with null record;
   type Gtk_Flow_Box_Child is access all Gtk_Flow_Box_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Flow_Box_Child);
   procedure Initialize
      (Self : not null access Gtk_Flow_Box_Child_Record'Class);
   --  Creates a new Gtk.Flow_Box_Child.Gtk_Flow_Box_Child, to be used as a
   --  child of a Gtk.Flow_Box.Gtk_Flow_Box.
   --  Since: gtk+ 3.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Flow_Box_Child_New return Gtk_Flow_Box_Child;
   --  Creates a new Gtk.Flow_Box_Child.Gtk_Flow_Box_Child, to be used as a
   --  child of a Gtk.Flow_Box.Gtk_Flow_Box.
   --  Since: gtk+ 3.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_flow_box_child_get_type");

   -------------
   -- Methods --
   -------------

   procedure Changed (Self : not null access Gtk_Flow_Box_Child_Record);
   --  Marks Child as changed, causing any state that depends on this to be
   --  updated. This affects sorting and filtering.
   --  Note that calls to this method must be in sync with the data used for
   --  the sorting and filtering functions. For instance, if the list is
   --  mirroring some external data set, and *two* children changed in the
   --  external data set when you call Gtk.Flow_Box_Child.Changed on the first
   --  child, the sort function must only read the new data for the first of
   --  the two changed children, otherwise the resorting of the children will
   --  be wrong.
   --  This generally means that if you don't fully control the data model,
   --  you have to duplicate the data that affects the sorting and filtering
   --  functions into the widgets themselves. Another alternative is to call
   --  Gtk.Flow_Box.Invalidate_Sort on any model change, but that is more
   --  expensive.
   --  Since: gtk+ 3.12

   function Get_Index
      (Self : not null access Gtk_Flow_Box_Child_Record) return Glib.Gint;
   --  Gets the current index of the Child in its Gtk.Flow_Box.Gtk_Flow_Box
   --  container.
   --  Since: gtk+ 3.12

   function Is_Selected
      (Self : not null access Gtk_Flow_Box_Child_Record) return Boolean;
   --  Returns whether the Child is currently selected in its
   --  Gtk.Flow_Box.Gtk_Flow_Box container.
   --  Since: gtk+ 3.12

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Flow_Box_Child_Void is not null access procedure
     (Self : access Gtk_Flow_Box_Child_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       Call  : Cb_Gtk_Flow_Box_Child_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Flow_Box_Child_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::activate signal is emitted when the user activates a child widget
   --  in a Gtk.Flow_Box.Gtk_Flow_Box, either by clicking or double-clicking,
   --  or by using the Space or Enter key.
   --
   --  While this signal is used as a [keybinding signal][GtkBindingSignal],
   --  it can be used by applications for their own purposes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Flow_Box_Child_Record, Gtk_Flow_Box_Child);
   function "+"
     (Widget : access Gtk_Flow_Box_Child_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Flow_Box_Child
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Flow_Box_Child;
