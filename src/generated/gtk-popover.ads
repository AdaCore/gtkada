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

--  Presents a bubble-like popup.
--
--  <picture> <source srcset="popover-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkPopover" src="popover.png"> </picture>
--  It is primarily meant to provide context-dependent information or options.
--  Popovers are attached to a parent widget. The parent widget must support
--  popover children, as [classGtk.MenuButton] and [classGtk.PopoverMenuBar]
--  do. If you want to make a custom widget that has an attached popover, you
--  need to call [methodGtk.Popover.present] in your
--  [vfuncGtk.Widget.size_allocate] vfunc, in order to update the positioning
--  of the popover.
--
--  The position of a popover relative to the widget it is attached to can
--  also be changed with [methodGtk.Popover.set_position]. By default, it
--  points to the whole widget area, but it can be made to point to a specific
--  area using [methodGtk.Popover.set_pointing_to].
--
--  By default, `GtkPopover` performs a grab, in order to ensure input events
--  get redirected to it while it is shown, and also so the popover is
--  dismissed in the expected situations (clicks outside the popover, or the
--  Escape key being pressed). If no such modal behavior is desired on a
--  popover, [methodGtk.Popover.set_autohide] may be called on it to tweak its
--  behavior.
--
--  ## GtkPopover as menu replacement
--
--  `GtkPopover` is often used to replace menus. The best way to do this is to
--  use the [classGtk.PopoverMenu] subclass which supports being populated from
--  a `GMenuModel` with [ctorGtk.PopoverMenu.new_from_model].
--
--  ```xml <section> <attribute
--  name="display-hint">horizontal-buttons</attribute> <item> <attribute
--  name="label">Cut</attribute> <attribute name="action">app.cut</attribute>
--  <attribute name="verb-icon">edit-cut-symbolic</attribute> </item> <item>
--  <attribute name="label">Copy</attribute> <attribute
--  name="action">app.copy</attribute> <attribute
--  name="verb-icon">edit-copy-symbolic</attribute> </item> <item> <attribute
--  name="label">Paste</attribute> <attribute
--  name="action">app.paste</attribute> <attribute
--  name="verb-icon">edit-paste-symbolic</attribute> </item> </section> ```
--
--  # Shortcuts and Gestures
--
--  `GtkPopover` supports the following keyboard shortcuts:
--
--  - <kbd>Escape</kbd> closes the popover. - <kbd>Alt</kbd> makes the
--  mnemonics visible.
--
--  The following signals have default keybindings:
--
--  - [signalGtk.Popover::activate-default]
--
--  # CSS nodes
--
--  ``` popover.background[.menu] ├── arrow ╰── contents ╰── <child> ```
--
--  `GtkPopover` has a main node with name `popover`, an arrow with name
--  `arrow`, and another node for the content named `contents`. The `popover`
--  node always gets the `.background` style class. It also gets the `.menu`
--  style class if the popover is menu-like, e.g. is a [classGtk.PopoverMenu].
--
--  Particular uses of `GtkPopover`, such as touch selection popups or
--  magnifiers in `GtkEntry` or `GtkTextView` get style classes like
--  `.touch-selection` or `.magnifier` to differentiate from plain popovers.
--
--  When styling a popover directly, the `popover` node should usually not
--  have any background. The visible part of the popover can have a shadow. To
--  specify it in CSS, set the box-shadow of the `contents` node.
--
--  Note that, in order to accomplish appropriate arrow visuals, `GtkPopover`
--  uses custom drawing for the `arrow` node. This makes it possible for the
--  arrow to change its shape dynamically, but it also limits the possibilities
--  of styling it using CSS. In particular, the `arrow` gets drawn over the
--  `content` node's border and shadow, so they look like one shape, which
--  means that the border width of the `content` node and the `arrow` node
--  should be the same. The arrow also does not support any border shape other
--  than solid, no border-radius, only one border width (border-bottom-width is
--  used) and no box-shadow.
--
--  <group>Menus and Toolbars</group>
--  <gtkada_demo>create_menu.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                   use Gdk;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Native;            use Gtk.Native;
with Gtk.Shortcut_Manager;  use Gtk.Shortcut_Manager;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Popover is

   type Gtk_Popover_Record is new Gtk_Widget_Record with null record;
   type Gtk_Popover is access all Gtk_Popover_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Popover);
   procedure Initialize (Self : not null access Gtk_Popover_Record'Class);
   --  Creates a new `GtkPopover`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Popover_New return Gtk_Popover;
   --  Creates a new `GtkPopover`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_popover_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Autohide
      (Self : not null access Gtk_Popover_Record) return Boolean;
   --  Returns whether the popover is modal.
   --  See [methodGtk.Popover.set_autohide] for the implications of this.
   --  @return True if Popover is modal

   procedure Set_Autohide
      (Self     : not null access Gtk_Popover_Record;
       Autohide : Boolean);
   --  Sets whether Popover is modal.
   --  A modal popover will grab the keyboard focus on it when being
   --  displayed. Focus will wrap around within the popover. Clicking outside
   --  the popover area or pressing Esc will dismiss the popover.
   --  Called this function on an already showing popup with a new autohide
   --  value different from the current one, will cause the popup to be hidden.
   --  @param Autohide True to dismiss the popover on outside clicks

   function Get_Cascade_Popdown
      (Self : not null access Gtk_Popover_Record) return Boolean;
   --  Returns whether the popover will close after a modal child is closed.
   --  @return True if Popover will close after a modal child.

   procedure Set_Cascade_Popdown
      (Self            : not null access Gtk_Popover_Record;
       Cascade_Popdown : Boolean);
   --  If Cascade_Popdown is True, the popover will be closed when a child
   --  modal popover is closed.
   --  If False, Popover will stay visible.
   --  @param Cascade_Popdown True if the popover should follow a child
   --  closing

   function Get_Child
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Popover.
   --  @return the child widget of Popover

   procedure Set_Child
      (Self  : not null access Gtk_Popover_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Popover.
   --  @param Child the child widget

   function Get_Has_Arrow
      (Self : not null access Gtk_Popover_Record) return Boolean;
   --  Gets whether this popover is showing an arrow pointing at the widget
   --  that it is relative to.
   --  @return whether the popover has an arrow

   procedure Set_Has_Arrow
      (Self      : not null access Gtk_Popover_Record;
       Has_Arrow : Boolean);
   --  Sets whether this popover should draw an arrow pointing at the widget
   --  it is relative to.
   --  @param Has_Arrow True to draw an arrow

   function Get_Mnemonics_Visible
      (Self : not null access Gtk_Popover_Record) return Boolean;
   --  Gets whether mnemonics are visible.
   --  @return True if mnemonics are supposed to be visible in this popover

   procedure Set_Mnemonics_Visible
      (Self              : not null access Gtk_Popover_Record;
       Mnemonics_Visible : Boolean);
   --  Sets whether mnemonics should be visible.
   --  @param Mnemonics_Visible the new value

   procedure Get_Offset
      (Self     : not null access Gtk_Popover_Record;
       X_Offset : out Glib.Gint;
       Y_Offset : out Glib.Gint);
   --  Gets the offset previous set with [methodGtk.Popover.set_offset].
   --  @param X_Offset a location for the x_offset
   --  @param Y_Offset a location for the y_offset

   procedure Set_Offset
      (Self     : not null access Gtk_Popover_Record;
       X_Offset : Glib.Gint;
       Y_Offset : Glib.Gint);
   --  Sets the offset to use when calculating the position of the popover.
   --  These values are used when preparing the [structGdk.PopupLayout] for
   --  positioning the popover.
   --  @param X_Offset the x offset to adjust the position by
   --  @param Y_Offset the y offset to adjust the position by

   function Get_Pointing_To
      (Self : not null access Gtk_Popover_Record;
       Rect : access Gdk.Rectangle.Gdk_Rectangle) return Boolean;
   --  Gets the rectangle that the popover points to.
   --  If a rectangle to point to has been set, this function will return True
   --  and fill in Rect with such rectangle, otherwise it will return False and
   --  fill in Rect with the parent widget coordinates.
   --  @param Rect location to store the rectangle
   --  @return True if a rectangle to point to was set.

   procedure Set_Pointing_To
      (Self : not null access Gtk_Popover_Record;
       Rect : Gdk.Rectangle.Gdk_Rectangle);
   --  Sets the rectangle that Popover points to.
   --  This is in the coordinate space of the Popover parent.
   --  @param Rect rectangle to point to

   function Get_Position
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Enums.Gtk_Position_Type;
   --  Returns the preferred position of Popover.
   --  @return The preferred position.

   procedure Set_Position
      (Self     : not null access Gtk_Popover_Record;
       Position : Gtk.Enums.Gtk_Position_Type);
   --  Sets the preferred position for Popover to appear.
   --  If the Popover is currently visible, it will be immediately updated.
   --  This preference will be respected where possible, although on lack of
   --  space (eg. if close to the window edges), the `GtkPopover` may choose to
   --  appear on the opposite side.
   --  @param Position preferred popover position

   procedure Popdown (Self : not null access Gtk_Popover_Record);
   --  Pops Popover down.
   --  This may have the side-effect of closing a parent popover as well. See
   --  [propertyGtk.Popover:cascade-popdown].

   procedure Popup (Self : not null access Gtk_Popover_Record);
   --  Pops Popover up.

   procedure Present (Self : not null access Gtk_Popover_Record);
   --  Allocate a size for the `GtkPopover`.
   --  This function needs to be called in size-allocate by widgets who have a
   --  `GtkPopover` as child. When using a layout manager, this is happening
   --  automatically.
   --  To make a popover appear on screen, use [methodGtk.Popover.popup].

   procedure Set_Default_Widget
      (Self   : not null access Gtk_Popover_Record;
       Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the default widget of a `GtkPopover`.
   --  The default widget is the widget that's activated when the user presses
   --  Enter in a dialog (for example). This function sets or unsets the
   --  default widget for a `GtkPopover`.
   --  @param Widget a child widget of Popover to set as the default, or null
   --  to unset the default widget for the popover

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Popover_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Popover_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Popover_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Popover_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Popover_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Popover_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Popover_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Popover_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Popover_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Popover_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Popover_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Surface
      (Self : not null access Gtk_Popover_Record) return Gdk.Gdk_Surface;

   procedure Get_Surface_Transform
      (Self : not null access Gtk_Popover_Record;
       X    : out Gdouble;
       Y    : out Gdouble);

   procedure Realize (Self : not null access Gtk_Popover_Record);

   procedure Unrealize (Self : not null access Gtk_Popover_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Autohide_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to dismiss the popover on outside clicks.

   Cascade_Popdown_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the popover pops down after a child popover.
   --
   --  This is used to implement the expected behavior of submenus.

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Default_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The default widget inside the popover.

   Has_Arrow_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to draw an arrow.

   Mnemonics_Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether mnemonics are currently visible in this popover.

   Pointing_To_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Rectangle
   --  Rectangle in the parent widget that the popover points to.

   Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type;
   --  How to place the popover, relative to its parent.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Popover_Void is not null access procedure (Self : access Gtk_Popover_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate_Default : constant Glib.Signal_Name := "activate-default";
   procedure On_Activate_Default
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_Gtk_Popover_Void;
       After : Boolean := False);
   procedure On_Activate_Default
      (Self  : not null access Gtk_Popover_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted whend the user activates the default widget.
   --
   --  This is a [keybinding signal](class.SignalAction.html).
   --
   --  The default binding for this signal is <kbd>Enter</kbd>.

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
   --  Emitted when the popover is closed.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Native"
   --
   --  - "Gtk.ShortcutManager"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Popover_Record, Gtk_Popover);
   function "+"
     (Widget : access Gtk_Popover_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Popover
   renames Implements_Gtk_Accessible.To_Object;

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

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Popover_Record, Gtk_Popover);
   function "+"
     (Widget : access Gtk_Popover_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Popover
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Native is new Glib.Types.Implements
     (Gtk.Native.Gtk_Native, Gtk_Popover_Record, Gtk_Popover);
   function "+"
     (Widget : access Gtk_Popover_Record'Class)
   return Gtk.Native.Gtk_Native
   renames Implements_Gtk_Native.To_Interface;
   function "-"
     (Interf : Gtk.Native.Gtk_Native)
   return Gtk_Popover
   renames Implements_Gtk_Native.To_Object;

   package Implements_Gtk_Shortcut_Manager is new Glib.Types.Implements
     (Gtk.Shortcut_Manager.Gtk_Shortcut_Manager, Gtk_Popover_Record, Gtk_Popover);
   function "+"
     (Widget : access Gtk_Popover_Record'Class)
   return Gtk.Shortcut_Manager.Gtk_Shortcut_Manager
   renames Implements_Gtk_Shortcut_Manager.To_Interface;
   function "-"
     (Interf : Gtk.Shortcut_Manager.Gtk_Shortcut_Manager)
   return Gtk_Popover
   renames Implements_Gtk_Shortcut_Manager.To_Object;

private
   Position_Property : constant Gtk.Enums.Property_Gtk_Position_Type :=
     Gtk.Enums.Build ("position");
   Pointing_To_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("pointing-to");
   Mnemonics_Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("mnemonics-visible");
   Has_Arrow_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("has-arrow");
   Default_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("default-widget");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
   Cascade_Popdown_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("cascade-popdown");
   Autohide_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("autohide");
end Gtk.Popover;
