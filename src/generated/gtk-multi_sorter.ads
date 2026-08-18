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

--  Combines multiple sorters by trying them in turn.
--
--  If the first sorter compares two items as equal, the second is tried next,
--  and so on.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.List_Model; use Glib.List_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Sorter;      use Gtk.Sorter;

package Gtk.Multi_Sorter is

   type Gtk_Multi_Sorter_Record is new Gtk_Sorter_Record with null record;
   type Gtk_Multi_Sorter is access all Gtk_Multi_Sorter_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Multi_Sorter);
   procedure Initialize
      (Self : not null access Gtk_Multi_Sorter_Record'Class);
   --  Creates a new multi sorter.
   --  This sorter compares items by trying each of the sorters in turn, until
   --  one returns non-zero. In particular, if no sorter has been added to it,
   --  it will always compare items as equal.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Multi_Sorter_New return Gtk_Multi_Sorter;
   --  Creates a new multi sorter.
   --  This sorter compares items by trying each of the sorters in turn, until
   --  one returns non-zero. In particular, if no sorter has been added to it,
   --  it will always compare items as equal.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_multi_sorter_get_type");

   -------------
   -- Methods --
   -------------

   procedure Append
      (Self   : not null access Gtk_Multi_Sorter_Record;
       Sorter : not null access Gtk.Sorter.Gtk_Sorter_Record'Class);
   --  Add Sorter to Self to use for sorting at the end.
   --  Self will consult all existing sorters before it will sort with the
   --  given Sorter.
   --  Parameter Sorter has transfer-ownership='full'
   --  @param Sorter a sorter to add

   procedure Remove
      (Self     : not null access Gtk_Multi_Sorter_Record;
       Position : Guint);
   --  Removes the sorter at the given Position from the list of sorter used
   --  by Self.
   --  If Position is larger than the number of sorters, nothing happens.
   --  @param Position position of sorter to remove

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Item_Type
      (Self : not null access Gtk_Multi_Sorter_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Multi_Sorter_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Multi_Sorter_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Multi_Sorter_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.Buildable"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Multi_Sorter_Record, Gtk_Multi_Sorter);
   function "+"
     (Widget : access Gtk_Multi_Sorter_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Multi_Sorter
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Multi_Sorter_Record, Gtk_Multi_Sorter);
   function "+"
     (Widget : access Gtk_Multi_Sorter_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Multi_Sorter
   renames Implements_Gtk_Buildable.To_Object;

private
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
end Gtk.Multi_Sorter;
