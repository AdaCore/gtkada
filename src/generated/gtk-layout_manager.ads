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

--  Handles the preferred size and allocation for children of a widget.
--
--  You typically subclass `GtkLayoutManager` if you want to implement a
--  layout policy for the children of a widget, or if you want to determine the
--  size of a widget depending on its contents.
--
--  Each `GtkWidget` can only have a `GtkLayoutManager` instance associated to
--  it at any given time; it is possible, though, to replace the layout manager
--  instance using [methodGtk.Widget.set_layout_manager].
--
--  ## Layout properties
--
--  A layout manager can expose properties for controlling the layout of each
--  child, by creating an object type derived from [classGtk.LayoutChild] and
--  installing the properties on it as normal `GObject` properties.
--
--  Each `GtkLayoutChild` instance storing the layout properties for a
--  specific child is created through the
--  [methodGtk.LayoutManager.get_layout_child] method; a `GtkLayoutManager`
--  controls the creation of its `GtkLayoutChild` instances by overriding the
--  GtkLayoutManagerClass.create_layout_child virtual function. The typical
--  implementation should look like:
--
--  ```c static GtkLayoutChild * create_layout_child (GtkLayoutManager
--  *manager, GtkWidget *container, GtkWidget *child) { return g_object_new
--  (your_layout_child_get_type (), "layout-manager", manager, "child-widget",
--  child, NULL); } ```
--
--  The [propertyGtk.LayoutChild:layout-manager] and
--  [propertyGtk.LayoutChild:child-widget] properties on the newly created
--  `GtkLayoutChild` instance are mandatory. The `GtkLayoutManager` will cache
--  the newly created `GtkLayoutChild` instance until the widget is removed
--  from its parent, or the parent removes the layout manager.
--
--  Each `GtkLayoutManager` instance creating a `GtkLayoutChild` should use
--  [methodGtk.LayoutManager.get_layout_child] every time it needs to query the
--  layout properties; each `GtkLayoutChild` instance should call
--  [methodGtk.LayoutManager.layout_changed] every time a property is updated,
--  in order to queue a new size measuring and allocation.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Gtk.Enums;   use Gtk.Enums;

package Gtk.Layout_Manager is

   pragma Elaborate_Body;

   type Gtk_Layout_Manager_Record is new GObject_Record with null record;
   type Gtk_Layout_Manager is access all Gtk_Layout_Manager_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_layout_manager_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Request_Mode
      (Manager : not null access Gtk_Layout_Manager_Record)
       return Gtk.Enums.Gtk_Size_Request_Mode;
   --  Retrieves the request mode of Manager.
   --  @return a `GtkSizeRequestMode`

   procedure Layout_Changed
      (Manager : not null access Gtk_Layout_Manager_Record);
   --  Queues a resize on the `GtkWidget` using Manager, if any.
   --  This function should be called by subclasses of `GtkLayoutManager` in
   --  response to changes to their layout management policies.

end Gtk.Layout_Manager;
