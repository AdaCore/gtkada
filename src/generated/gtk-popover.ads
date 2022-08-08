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
--  GtkPopover is a bubble-like context window, primarily meant to provide
--  context-dependent information or options. Popovers are attached to a
--  widget, passed at construction time on Gtk.Popover.Gtk_New, or updated
--  afterwards through Gtk.Popover.Set_Relative_To, by default they will point
--  to the whole widget area, although this behavior can be changed through
--  Gtk.Popover.Set_Pointing_To.
--
--  The position of a popover relative to the widget it is attached to can
--  also be changed through Gtk.Popover.Set_Position.
--
--  By default, Gtk.Popover.Gtk_Popover performs a GTK+ grab, in order to
--  ensure input events get redirected to it while it is shown, and also so the
--  popover is dismissed in the expected situations (clicks outside the
--  popover, or the Esc key being pressed). If no such modal behavior is
--  desired on a popover, Gtk.Popover.Set_Modal may be called on it to tweak
--  its behavior.
--
--  ## GtkPopover as menu replacement
--
--  GtkPopover is often used to replace menus. To facilitate this, it supports
--  being populated from a Glib.Menu_Model.Gmenu_Model, using
--  Gtk.Popover.Gtk_New_From_Model. In addition to all the regular menu model
--  features, this function supports rendering sections in the model in a more
--  compact form, as a row of icon buttons instead of menu items.
--
--  To use this rendering, set the "display-hint" attribute of the section to
--  "horizontal-buttons" and set the icons of your items with the "verb-icon"
--  attribute.
--
--  |[ <section> <attribute name="display-hint">horizontal-buttons</attribute>
--  <item> <attribute name="label">Cut</attribute> <attribute
--  name="action">app.cut</attribute> <attribute
--  name="verb-icon">edit-cut-symbolic</attribute> </item> <item> <attribute
--  name="label">Copy</attribute> <attribute name="action">app.copy</attribute>
--  <attribute name="verb-icon">edit-copy-symbolic</attribute> </item> <item>
--  <attribute name="label">Paste</attribute> <attribute
--  name="action">app.paste</attribute> <attribute
--  name="verb-icon">edit-paste-symbolic</attribute> </item> </section> ]|
--
--  # CSS nodes
--
--  GtkPopover has a single css node called popover. It always gets the
--  .background style class and it gets the .menu style class if it is
--  menu-like (e.g. Gtk.Popover_Menu.Gtk_Popover_Menu or created using
--  Gtk.Popover.Gtk_New_From_Model.
--
--  Particular uses of GtkPopover, such as touch selection popups or
--  magnifiers in Gtk.GEntry.Gtk_Entry or Gtk.Text_View.Gtk_Text_View get style
--  classes like .touch-selection or .magnifier to differentiate from plain
--  popovers.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Rectangle;   use Gdk.Rectangle;
with Glib;            use Glib;
with Glib.Menu_Model; use Glib.Menu_Model;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Popover is

   type Gtk_Popover_Record is new Gtk_Bin_Record with null record;
   type Gtk_Popover is access all Gtk_Popover_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Popover;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize
      (Self        : not null access Gtk_Popover_Record'Class;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Creates a new popover to point to Relative_To
   --  Since: gtk+ 3.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "relative_to": Gtk.Widget.Gtk_Widget the popover is related to

   function Gtk_Popover_New
      (Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Popover;
   --  Creates a new popover to point to Relative_To
   --  Since: gtk+ 3.12
   --  "relative_to": Gtk.Widget.Gtk_Widget the popover is related to

   procedure Gtk_New_From_Model
      (Self        : out Gtk_Popover;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class;
       Model       : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   procedure Initialize_From_Model
      (Self        : not null access Gtk_Popover_Record'Class;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class;
       Model       : not null access Glib.Menu_Model.Gmenu_Model_Record'Class);
   --  Creates a Gtk.Popover.Gtk_Popover and populates it according to Model.
   --  The popover is pointed to the Relative_To widget.
   --  The created buttons are connected to actions found in the
   --  Gtk.Application_Window.Gtk_Application_Window to which the popover
   --  belongs - typically by means of being attached to a widget that is
   --  contained within the Gtk_Application_Windows widget hierarchy.
   --  Actions can also be added using gtk_widget_insert_action_group on the
   --  menus attach widget or on any of its parent widgets.
   --  Since: gtk+ 3.12
   --  Initialize_From_Model does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "relative_to": Gtk.Widget.Gtk_Widget the popover is related to
   --  "model": a Glib.Menu_Model.Gmenu_Model

   function Gtk_Popover_New_From_Model
      (Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class;
       Model       : not null access Glib.Menu_Model.Gmenu_Model_Record'Class)
       return Gtk_Popover;
   --  Creates a Gtk.Popover.Gtk_Popover and populates it according to Model.
   --  The popover is pointed to the Relative_To widget.
   --  The created buttons are connected to actions found in the
   --  Gtk.Application_Window.Gtk_Application_Window to which the popover
   --  belongs - typically by means of being attached to a widget that is
   --  contained within the Gtk_Application_Windows widget hierarchy.
   --  Actions can also be added using gtk_widget_insert_action_group on the
   --  menus attach widget or on any of its parent widgets.
   --  Since: gtk+ 3.12
   --  "relative_to": Gtk.Widget.Gtk_Widget the popover is related to
   --  "model": a Glib.Menu_Model.Gmenu_Model

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_popover_get_type");

   -------------
   -- Methods --
   -------------

   procedure Bind_Model
      (Self             : not null access Gtk_Popover_Record;
       Model            : access Glib.Menu_Model.Gmenu_Model_Record'Class;
       Action_Namespace : UTF8_String := "");
   --  Establishes a binding between a Gtk.Popover.Gtk_Popover and a
   --  Glib.Menu_Model.Gmenu_Model.
   --  The contents of Popover are removed and then refilled with menu items
   --  according to Model. When Model changes, Popover is updated. Calling this
   --  function twice on Popover with different Model will cause the first
   --  binding to be replaced with a binding to the new model. If Model is null
   --  then any previous binding is undone and all children are removed.
   --  If Action_Namespace is non-null then the effect is as if all actions
   --  mentioned in the Model have their names prefixed with the namespace,
   --  plus a dot. For example, if the action "quit" is mentioned and
   --  Action_Namespace is "app" then the effective action name is "app.quit".
   --  This function uses Gtk.Actionable.Gtk_Actionable to define the action
   --  name and target values on the created menu items. If you want to use an
   --  action group other than "app" and "win", or if you want to use a
   --  Gtk.Menu_Shell.Gtk_Menu_Shell outside of a
   --  Gtk.Application_Window.Gtk_Application_Window, then you will need to
   --  attach your own action group to the widget hierarchy using
   --  gtk_widget_insert_action_group. As an example, if you created a group
   --  with a "quit" action and inserted it with the name "mygroup" then you
   --  would use the action name "mygroup.quit" in your
   --  Glib.Menu_Model.Gmenu_Model.
   --  Since: gtk+ 3.12
   --  "model": the Glib.Menu_Model.Gmenu_Model to bind to or null to remove
   --  binding
   --  "action_namespace": the namespace for actions in Model

   function Get_Constrain_To
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Enums.Gtk_Popover_Constraint;
   --  Returns the constraint for placing this popover. See
   --  Gtk.Popover.Set_Constrain_To.
   --  Since: gtk+ 3.20

   procedure Set_Constrain_To
      (Self       : not null access Gtk_Popover_Record;
       Constraint : Gtk.Enums.Gtk_Popover_Constraint);
   --  Sets a constraint for positioning this popover.
   --  Note that not all platforms support placing popovers freely, and may
   --  already impose constraints.
   --  Since: gtk+ 3.20
   --  "constraint": the new constraint

   function Get_Default_Widget
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the widget that should be set as the default while the popover is
   --  shown.
   --  Since: gtk+ 3.18

   procedure Set_Default_Widget
      (Self   : not null access Gtk_Popover_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the widget that should be set as default widget while the popover
   --  is shown (see Gtk.Window.Set_Default). Gtk.Popover.Gtk_Popover remembers
   --  the previous default widget and reestablishes it when the popover is
   --  dismissed.
   --  Since: gtk+ 3.18
   --  "widget": the new default widget, or null

   function Get_Modal
      (Self : not null access Gtk_Popover_Record) return Boolean;
   --  Returns whether the popover is modal, see gtk_popover_set_modal to see
   --  the implications of this.
   --  Since: gtk+ 3.12

   procedure Set_Modal
      (Self  : not null access Gtk_Popover_Record;
       Modal : Boolean);
   --  Sets whether Popover is modal, a modal popover will grab all input
   --  within the toplevel and grab the keyboard focus on it when being
   --  displayed. Clicking outside the popover area or pressing Esc will
   --  dismiss the popover and ungrab input.
   --  Since: gtk+ 3.12
   --  "modal": TRUE to make popover claim all input within the toplevel

   function Get_Pointing_To
      (Self : not null access Gtk_Popover_Record;
       Rect : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  If a rectangle to point to has been set, this function will return True
   --  and fill in Rect with such rectangle, otherwise it will return False and
   --  fill in Rect with the attached widget coordinates.
   --  "rect": location to store the rectangle

   procedure Set_Pointing_To
      (Self : not null access Gtk_Popover_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle);
   --  Sets the rectangle that Popover will point to, in the coordinate space
   --  of the widget Popover is attached to, see Gtk.Popover.Set_Relative_To.
   --  Since: gtk+ 3.12
   --  "rect": rectangle to point to

   function Get_Position
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Enums.Gtk_Position_Type;
   --  Returns the preferred position of Popover.

   procedure Set_Position
      (Self     : not null access Gtk_Popover_Record;
       Position : Gtk.Enums.Gtk_Position_Type);
   --  Sets the preferred position for Popover to appear. If the Popover is
   --  currently visible, it will be immediately updated.
   --  This preference will be respected where possible, although on lack of
   --  space (eg. if close to the window edges), the Gtk.Popover.Gtk_Popover
   --  may choose to appear on the opposite side
   --  Since: gtk+ 3.12
   --  "position": preferred popover position

   function Get_Relative_To
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the widget Popover is currently attached to
   --  Since: gtk+ 3.12

   procedure Set_Relative_To
      (Self        : not null access Gtk_Popover_Record;
       Relative_To : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets a new widget to be attached to Popover. If Popover is visible, the
   --  position will be updated.
   --  Note: the ownership of popovers is always given to their Relative_To
   --  widget, so if Relative_To is set to null on an attached Popover, it will
   --  be detached from its previous widget, and consequently destroyed unless
   --  extra references are kept.
   --  Since: gtk+ 3.12
   --  "relative_to": a Gtk.Widget.Gtk_Widget

   function Get_Transitions_Enabled
      (Self : not null access Gtk_Popover_Record) return Boolean;
   pragma Obsolescent (Get_Transitions_Enabled);
   --  Returns whether show/hide transitions are enabled on this popover.
   --  Since: gtk+ 3.16
   --  Deprecated since 3.22, 1

   procedure Set_Transitions_Enabled
      (Self                : not null access Gtk_Popover_Record;
       Transitions_Enabled : Boolean);
   pragma Obsolescent (Set_Transitions_Enabled);
   --  Sets whether show/hide transitions are enabled on this popover
   --  Since: gtk+ 3.16
   --  Deprecated since 3.22, 1
   --  "transitions_enabled": Whether transitions are enabled

   procedure Popdown (Self : not null access Gtk_Popover_Record);
   --  Pops Popover down.This is different than a Gtk.Widget.Hide call in that
   --  it shows the popover with a transition. If you want to hide the popover
   --  without a transition, use Gtk.Widget.Hide.
   --  Since: gtk+ 3.22

   procedure Popup (Self : not null access Gtk_Popover_Record);
   --  Pops Popover up. This is different than a Gtk.Widget.Show call in that
   --  it shows the popover with a transition. If you want to show the popover
   --  without a transition, use Gtk.Widget.Show.
   --  Since: gtk+ 3.22

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Constrain_To_Property : constant Gtk.Enums.Property_Gtk_Popover_Constraint;
   --  Sets a constraint for the popover position.

   Modal_Property : constant Glib.Properties.Property_Boolean;
   --  Sets whether the popover is modal (so other elements in the window do
   --  not receive input while the popover is visible).

   Pointing_To_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Rectangle
   --  Marks a specific rectangle to be pointed.

   Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   --  Sets the preferred position of the popover.

   Relative_To_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  Sets the attached widget.

   Transitions_Enabled_Property : constant Glib.Properties.Property_Boolean;
   --  Whether show/hide transitions are enabled for this popover.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Popover_Void is not null access procedure (Self : access Gtk_Popover_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Closed : constant Glib.Signal_Name := "closed";
   procedure On_Closed
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_Gtk_Popover_Void;
       After : Boolean := False);
   procedure On_Closed
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted when the popover is dismissed either through API
   --  or user interaction.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Popover_Record, Gtk_Popover);
   function "+"
     (Widget : access Gtk_Popover_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Popover
   renames Implements_Gtk_Buildable.To_Object;

private
   Transitions_Enabled_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("transitions-enabled");
   Relative_To_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("relative-to");
   Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("position");
   Pointing_To_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pointing-to");
   Modal_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("modal");
   Constrain_To_Property : constant Gtk.Enums.Property_Gtk_Popover_Constraint :=
     Gtk.Enums.Build ("constrain-to");
end Gtk.Popover;
