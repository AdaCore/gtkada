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

--  Manages keyboard shortcuts and their activation.
--
--  Most common shortcuts are using this controller implicitly, e.g. by adding
--  a mnemonic underline to a [classGtk.Label], or by installing a key binding
--  using [methodGtk.WidgetClass.add_binding], or by adding accelerators to
--  global actions using [methodGtk.Application.set_accels_for_action].
--
--  But it is possible to create your own shortcut controller, and add
--  shortcuts to it.
--
--  `GtkShortcutController` implements [ifaceGio.ListModel] for querying the
--  shortcuts that have been added to it.
--
--  # GtkShortcutController as GtkBuildable
--
--  `GtkShortcutController`s can be created in [classGtk.Builder] ui files, to
--  set up shortcuts in the same place as the widgets.
--
--  An example of a UI definition fragment with `GtkShortcutController`:
--  ```xml <object class='GtkButton'> <child> <object
--  class='GtkShortcutController'> <property name='scope'>managed</property>
--  <child> <object class='GtkShortcut'> <property
--  name='trigger'><Control>k</property> <property
--  name='action'>activate</property> </object> </child> </object> </child>
--  </object> ```
--
--  This example creates a [classGtk.ActivateAction] for triggering the
--  `activate` signal of the [classGtk.Button]. See
--  [ctorGtk.ShortcutAction.parse_string] for the syntax for other kinds of
--  [classGtk.ShortcutAction]. See [ctorGtk.ShortcutTrigger.parse_string] to
--  learn more about the syntax for triggers.
--
--  <gtkada_demo>create_shortcuts.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Enums;               use Gdk.Enums;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.List_Model;         use Glib.List_Model;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Event_Controller;    use Gtk.Event_Controller;
with Gtk.Shortcut;            use Gtk.Shortcut;

package Gtk.Shortcut_Controller is

   type Gtk_Shortcut_Controller_Record is new Gtk_Event_Controller_Record with null record;
   type Gtk_Shortcut_Controller is access all Gtk_Shortcut_Controller_Record'Class;

   type Gtk_Shortcut_Scope is (
      Local,
      Managed,
      Global);
   pragma Convention (C, Gtk_Shortcut_Scope);
   --  Describes where [classShortcut]s added to a [classShortcutcontroller]
   --  get handled.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Shortcut_Scope_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Shortcut_Scope);
   type Property_Gtk_Shortcut_Scope is new Gtk_Shortcut_Scope_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Shortcut_Controller);
   procedure Initialize
      (Self : not null access Gtk_Shortcut_Controller_Record'Class);
   --  Creates a new shortcut controller.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Shortcut_Controller_New return Gtk_Shortcut_Controller;
   --  Creates a new shortcut controller.

   procedure Gtk_New_For_Model
      (Self  : out Gtk_Shortcut_Controller;
       Model : Glib.List_Model.Glist_Model);
   procedure Initialize_For_Model
      (Self  : not null access Gtk_Shortcut_Controller_Record'Class;
       Model : Glib.List_Model.Glist_Model);
   --  Creates a new shortcut controller that takes its shortcuts from the
   --  given list model.
   --  A controller created by this function does not let you add or remove
   --  individual shortcuts using the shortcut controller api, but you can
   --  change the contents of the model.
   --  Initialize_For_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Model a `GListModel` containing shortcuts

   function Gtk_Shortcut_Controller_New_For_Model
      (Model : Glib.List_Model.Glist_Model) return Gtk_Shortcut_Controller;
   --  Creates a new shortcut controller that takes its shortcuts from the
   --  given list model.
   --  A controller created by this function does not let you add or remove
   --  individual shortcuts using the shortcut controller api, but you can
   --  change the contents of the model.
   --  @param Model a `GListModel` containing shortcuts

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_shortcut_controller_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Shortcut
      (Self     : not null access Gtk_Shortcut_Controller_Record;
       Shortcut : not null access Gtk.Shortcut.Gtk_Shortcut_Record'Class);
   --  Adds Shortcut to the list of shortcuts handled by Self.
   --  If this controller uses an external shortcut list, this function does
   --  nothing.
   --  @param Shortcut a `GtkShortcut`. Has transfer-ownership='full'.

   function Get_Mnemonics_Modifiers
      (Self : not null access Gtk_Shortcut_Controller_Record)
       return Gdk.Enums.Gdk_Modifier_Type;
   --  Gets the mnemonics modifiers for when this controller activates its
   --  shortcuts.
   --  @return the controller's mnemonics modifiers

   procedure Set_Mnemonics_Modifiers
      (Self      : not null access Gtk_Shortcut_Controller_Record;
       Modifiers : Gdk.Enums.Gdk_Modifier_Type);
   --  Sets the controller to use the given modifier for mnemonics.
   --  The mnemonics modifiers determines which modifiers need to be pressed
   --  to allow activation of shortcuts with mnemonics triggers.
   --  GTK normally uses the Alt modifier for mnemonics, except in
   --  `GtkPopoverMenu`s, where mnemonics can be triggered without any
   --  modifiers. It should be very rarely necessary to change this, and doing
   --  so is likely to interfere with other shortcuts.
   --  This value is only relevant for local shortcut controllers. Global and
   --  managed shortcut controllers will have their shortcuts activated from
   --  other places which have their own modifiers for activating mnemonics.
   --  @param Modifiers the new mnemonics_modifiers to use

   function Get_Scope
      (Self : not null access Gtk_Shortcut_Controller_Record)
       return Gtk_Shortcut_Scope;
   --  Gets the scope for when this controller activates its shortcuts.
   --  See [methodGtk.ShortcutController.set_scope] for details.
   --  @return the controller's scope

   procedure Set_Scope
      (Self  : not null access Gtk_Shortcut_Controller_Record;
       Scope : Gtk_Shortcut_Scope);
   --  Sets the controller to have the given Scope.
   --  The scope allows shortcuts to be activated outside of the normal event
   --  propagation. In particular, it allows installing global keyboard
   --  shortcuts that can be activated even when a widget does not have focus.
   --  With Gtk.Shortcut_Controller.Local, shortcuts will only be activated
   --  when the widget has focus.
   --  @param Scope the new scope to use

   procedure Remove_Shortcut
      (Self     : not null access Gtk_Shortcut_Controller_Record;
       Shortcut : not null access Gtk.Shortcut.Gtk_Shortcut_Record'Class);
   --  Removes Shortcut from the list of shortcuts handled by Self.
   --  If Shortcut had not been added to Controller or this controller uses an
   --  external shortcut list, this function does nothing.
   --  @param Shortcut a `GtkShortcut`

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Item_Type
      (Self : not null access Gtk_Shortcut_Controller_Record) return GType;

   function Get_N_Items
      (Self : not null access Gtk_Shortcut_Controller_Record) return Guint;

   function Get_Item
      (Self     : not null access Gtk_Shortcut_Controller_Record;
       Position : Guint) return Glib.Object.GObject;

   procedure Items_Changed
      (Self     : not null access Gtk_Shortcut_Controller_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Item_Type_Property : constant Glib.Properties.Property_Boxed;
   --  Type: GType
   --  The type of items. See [methodGio.ListModel.get_item_type].

   Mnemonic_Modifiers_Property : constant Gdk.Enums.Property_Gdk_Modifier_Type;
   --  Type: Gdk.Enums.Gdk_Modifier_Type
   --  The modifiers that need to be pressed to allow mnemonics activation.

   Model_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gio.List_Model
   --  Flags: write
   --  A list model to take shortcuts from.

   N_Items_Property : constant Glib.Properties.Property_Uint;
   --  The number of items. See [methodGio.ListModel.get_n_items].

   Scope_Property : constant Gtk.Shortcut_Controller.Property_Gtk_Shortcut_Scope;
   --  Type: Gtk_Shortcut_Scope
   --  What scope the shortcuts will be handled in.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gio.ListModel"
   --
   --  - "Gtk.Buildable"

   package Implements_Glist_Model is new Glib.Types.Implements
     (Glib.List_Model.Glist_Model, Gtk_Shortcut_Controller_Record, Gtk_Shortcut_Controller);
   function "+"
     (Widget : access Gtk_Shortcut_Controller_Record'Class)
   return Glib.List_Model.Glist_Model
   renames Implements_Glist_Model.To_Interface;
   function "-"
     (Interf : Glib.List_Model.Glist_Model)
   return Gtk_Shortcut_Controller
   renames Implements_Glist_Model.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Shortcut_Controller_Record, Gtk_Shortcut_Controller);
   function "+"
     (Widget : access Gtk_Shortcut_Controller_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Shortcut_Controller
   renames Implements_Gtk_Buildable.To_Object;

private
   Scope_Property : constant Gtk.Shortcut_Controller.Property_Gtk_Shortcut_Scope :=
     Gtk.Shortcut_Controller.Build ("scope");
   N_Items_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("n-items");
   Model_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("model");
   Mnemonic_Modifiers_Property : constant Gdk.Enums.Property_Gdk_Modifier_Type :=
     Gdk.Enums.Build ("mnemonic-modifiers");
   Item_Type_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("item-type");
end Gtk.Shortcut_Controller;
