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
--  Activatable widgets can be connected to a Gtk.Action.Gtk_Action and
--  reflects the state of its action. A Gtk.Activatable.Gtk_Activatable can
--  also provide feedback through its action, as they are responsible for
--  activating their related actions.
--
--  # Implementing GtkActivatable
--
--  When extending a class that is already Gtk.Activatable.Gtk_Activatable; it
--  is only necessary to implement the
--  Gtk.Activatable.Gtk_Activatable->sync_action_properties and
--  Gtk.Activatable.Gtk_Activatable->update methods and chain up to the parent
--  implementation, however when introducing a new
--  Gtk.Activatable.Gtk_Activatable class; the
--  Gtk.Activatable.Gtk_Activatable:related-action and
--  Gtk.Activatable.Gtk_Activatable:use-action-appearance properties need to be
--  handled by the implementor. Handling these properties is mostly a matter of
--  installing the action pointer and boolean flag on your instance, and
--  calling Gtk.Activatable.Do_Set_Related_Action and
--  Gtk.Activatable.Sync_Action_Properties at the appropriate times.
--
--  ## A class fragment implementing Gtk.Activatable.Gtk_Activatable
--
--  |[<!-- language="C" -->
--
--  enum { ...
--
--  PROP_ACTIVATABLE_RELATED_ACTION, PROP_ACTIVATABLE_USE_ACTION_APPEARANCE }
--
--  struct _FooBarPrivate {
--
--  ...
--
--  GtkAction *action; gboolean use_action_appearance; };
--
--  ...
--
--  static void foo_bar_activatable_interface_init (GtkActivatableIface
--  *iface); static void foo_bar_activatable_update (GtkActivatable
--  *activatable, GtkAction *action, const gchar *property_name); static void
--  foo_bar_activatable_sync_action_properties (GtkActivatable *activatable,
--  GtkAction *action); ...
--
--  static void foo_bar_class_init (FooBarClass *klass) {
--
--  ...
--
--  g_object_class_override_property (gobject_class,
--  PROP_ACTIVATABLE_RELATED_ACTION, "related-action");
--  g_object_class_override_property (gobject_class,
--  PROP_ACTIVATABLE_USE_ACTION_APPEARANCE, "use-action-appearance");
--
--  ... }
--
--  static void foo_bar_activatable_interface_init (GtkActivatableIface
--  *iface) { iface->update = foo_bar_activatable_update;
--  iface->sync_action_properties = foo_bar_activatable_sync_action_properties;
--  }
--
--  ... Break the reference using Gtk.Activatable.Do_Set_Related_Action...
--
--  static void foo_bar_dispose (GObject *object) { FooBar *bar = FOO_BAR
--  (object); FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
--
--  ...
--
--  if (priv->action) { gtk_activatable_do_set_related_action (GTK_ACTIVATABLE
--  (bar), NULL); priv->action = NULL; } G_OBJECT_CLASS
--  (foo_bar_parent_class)->dispose (object); }
--
--  ... Handle the "related-action" and "use-action-appearance" properties ...
--
--  static void foo_bar_set_property (GObject *object, guint prop_id, const
--  GValue *value, GParamSpec *pspec) { FooBar *bar = FOO_BAR (object);
--  FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
--
--  switch (prop_id) {
--
--  ...
--
--  case PROP_ACTIVATABLE_RELATED_ACTION: foo_bar_set_related_action (bar,
--  g_value_get_object (value)); break; case
--  PROP_ACTIVATABLE_USE_ACTION_APPEARANCE: foo_bar_set_use_action_appearance
--  (bar, g_value_get_boolean (value)); break; default:
--  G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec); break; } }
--
--  static void foo_bar_get_property (GObject *object, guint prop_id, GValue
--  *value, GParamSpec *pspec) { FooBar *bar = FOO_BAR (object); FooBarPrivate
--  *priv = FOO_BAR_GET_PRIVATE (bar);
--
--  switch (prop_id) {
--
--  ...
--
--  case PROP_ACTIVATABLE_RELATED_ACTION: g_value_set_object (value,
--  priv->action); break; case PROP_ACTIVATABLE_USE_ACTION_APPEARANCE:
--  g_value_set_boolean (value, priv->use_action_appearance); break; default:
--  G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec); break; } }
--
--  static void foo_bar_set_use_action_appearance (FooBar *bar, gboolean
--  use_appearance) { FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
--
--  if (priv->use_action_appearance != use_appearance) {
--  priv->use_action_appearance = use_appearance;
--  gtk_activatable_sync_action_properties (GTK_ACTIVATABLE (bar),
--  priv->action); } }
--
--  ... call Gtk.Activatable.Do_Set_Related_Action and then assign the action
--  pointer, no need to reference the action here since
--  Gtk.Activatable.Do_Set_Related_Action already holds a reference here for
--  you... static void foo_bar_set_related_action (FooBar *bar, GtkAction
--  *action) { FooBarPrivate *priv = FOO_BAR_GET_PRIVATE (bar);
--
--  if (priv->action == action) return;
--
--  gtk_activatable_do_set_related_action (GTK_ACTIVATABLE (bar), action);
--
--  priv->action = action; }
--
--  ... Selectively reset and update activatable depending on the
--  use-action-appearance property ... static void
--  gtk_button_activatable_sync_action_properties (GtkActivatable *activatable,
--  GtkAction *action) { GtkButtonPrivate *priv = GTK_BUTTON_GET_PRIVATE
--  (activatable);
--
--  if (!action) return;
--
--  if (gtk_action_is_visible (action)) gtk_widget_show (GTK_WIDGET
--  (activatable)); else gtk_widget_hide (GTK_WIDGET (activatable));
--  gtk_widget_set_sensitive (GTK_WIDGET (activatable), gtk_action_is_sensitive
--  (action));
--
--  ... if (priv->use_action_appearance) { if (gtk_action_get_stock_id
--  (action)) foo_bar_set_stock (button, gtk_action_get_stock_id (action));
--  else if (gtk_action_get_label (action)) foo_bar_set_label (button,
--  gtk_action_get_label (action));
--
--  ...
--
--  } }
--
--  static void foo_bar_activatable_update (GtkActivatable *activatable,
--  GtkAction *action, const gchar *property_name) { FooBarPrivate *priv =
--  FOO_BAR_GET_PRIVATE (activatable);
--
--  if (strcmp (property_name, "visible") == 0) { if (gtk_action_is_visible
--  (action)) gtk_widget_show (GTK_WIDGET (activatable)); else gtk_widget_hide
--  (GTK_WIDGET (activatable)); } else if (strcmp (property_name, "sensitive")
--  == 0) gtk_widget_set_sensitive (GTK_WIDGET (activatable),
--  gtk_action_is_sensitive (action));
--
--  ...
--
--  if (!priv->use_action_appearance) return;
--
--  if (strcmp (property_name, "stock-id") == 0) foo_bar_set_stock (button,
--  gtk_action_get_stock_id (action)); else if (strcmp (property_name, "label")
--  == 0) foo_bar_set_label (button, gtk_action_get_label (action));
--
--  ... } ]|
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;

package Gtk.Activatable is

   type Gtk_Activatable is new Glib.Types.GType_Interface;
   Null_Gtk_Activatable : constant Gtk_Activatable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_activatable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Do_Set_Related_Action
      (Self   : Gtk_Activatable;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);
   --  This is a utility function for Gtk.Activatable.Gtk_Activatable
   --  implementors.
   --  When implementing Gtk.Activatable.Gtk_Activatable you must call this
   --  when handling changes of the
   --  Gtk.Activatable.Gtk_Activatable:related-action, and you must also use
   --  this to break references in Glib.Object.GObject->dispose.
   --  This function adds a reference to the currently set related action for
   --  you, it also makes sure the Gtk.Activatable.Gtk_Activatable->update
   --  method is called when the related Gtk.Action.Gtk_Action properties
   --  change and registers to the action's proxy list.
   --  > Be careful to call this before setting the local > copy of the
   --  Gtk.Action.Gtk_Action property, since this function uses >
   --  Gtk.Activatable.Get_Related_Action to retrieve the > previous action.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "action": the Gtk.Action.Gtk_Action to set

   function Get_Related_Action
      (Self : Gtk_Activatable) return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);
   --  Gets the related Gtk.Action.Gtk_Action for Activatable.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Related_Action
      (Self   : Gtk_Activatable;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);
   --  Sets the related action on the Activatable object.
   --  > Gtk.Activatable.Gtk_Activatable implementors need to handle the
   --  Gtk.Activatable.Gtk_Activatable:related-action > property and call
   --  Gtk.Activatable.Do_Set_Related_Action when it changes.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "action": the Gtk.Action.Gtk_Action to set

   function Get_Use_Action_Appearance
      (Self : Gtk_Activatable) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);
   --  Gets whether this activatable should reset its layout and appearance
   --  when setting the related action or when the action changes appearance.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1

   procedure Set_Use_Action_Appearance
      (Self           : Gtk_Activatable;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);
   --  Sets whether this activatable should reset its layout and appearance
   --  when setting the related action or when the action changes appearance
   --  > Gtk.Activatable.Gtk_Activatable implementors need to handle the >
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance property and call
   --  > Gtk.Activatable.Sync_Action_Properties to update Activatable > if
   --  needed.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "use_appearance": whether to use the actions appearance

   procedure Sync_Action_Properties
      (Self   : Gtk_Activatable;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);
   --  This is called to update the activatable completely, this is called
   --  internally when the Gtk.Activatable.Gtk_Activatable:related-action
   --  property is set or unset and by the implementing class when
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance changes.
   --  Since: gtk+ 2.16
   --  Deprecated since 3.10, 1
   --  "action": the related Gtk.Action.Gtk_Action or null

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Related_Action_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Action.Gtk_Action
   --  The action that this activatable will activate and receive updates from
   --  for various states and possibly appearance.
   --
   --  > Gtk.Activatable.Gtk_Activatable implementors need to handle the this
   --  property and > call Gtk.Activatable.Do_Set_Related_Action when it
   --  changes.

   Use_Action_Appearance_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this activatable should reset its layout and appearance when
   --  setting the related action or when the action changes appearance.
   --
   --  See the Gtk.Action.Gtk_Action documentation directly to find which
   --  properties should be ignored by the Gtk.Activatable.Gtk_Activatable when
   --  this property is False.
   --
   --  > Gtk.Activatable.Gtk_Activatable implementors need to handle this
   --  property > and call Gtk.Activatable.Sync_Action_Properties on the
   --  activatable > widget when it changes.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Activatable"

   function "+" (W : Gtk_Activatable) return Gtk_Activatable;
   pragma Inline ("+");

private
   Use_Action_Appearance_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-action-appearance");
   Related_Action_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("related-action");

Null_Gtk_Activatable : constant Gtk_Activatable :=
   Gtk_Activatable (Glib.Types.Null_Interface);
end Gtk.Activatable;
