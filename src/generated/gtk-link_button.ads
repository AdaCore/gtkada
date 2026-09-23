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

--  A button with a hyperlink.
--
--  <picture> <source srcset="link-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkLinkButton"
--  src="link-button.png"> </picture>
--  It is useful to show quick links to resources.
--
--  A link button is created by calling either [ctorGtk.LinkButton.new] or
--  [ctorGtk.LinkButton.new_with_label]. If using the former, the URI you pass
--  to the constructor is used as a label for the widget.
--
--  The URI bound to a `GtkLinkButton` can be set specifically using
--  [methodGtk.LinkButton.set_uri].
--
--  By default, `GtkLinkButton` calls [methodGtk.FileLauncher.launch] when the
--  button is clicked. This behaviour can be overridden by connecting to the
--  [signalGtk.LinkButton::activate-link] signal and returning True from the
--  signal handler.
--
--  # Shortcuts and Gestures
--
--  `GtkLinkButton` supports the following keyboard shortcuts:
--
--  - <kbd>Shift</kbd>+<kbd>F10</kbd> or <kbd>Menu</kbd> opens the context
--  menu.
--
--  # Actions
--
--  `GtkLinkButton` defines a set of built-in actions:
--
--  - `clipboard.copy` copies the url to the clipboard. - `menu.popup` opens
--  the context menu.
--
--  # CSS nodes
--
--  `GtkLinkButton` has a single CSS node with name button. To differentiate
--  it from a plain `GtkButton`, it gets the .link style class.
--
--  # Accessibility
--
--  `GtkLinkButton` uses the [enumGtk.AccessibleRole.link] role.
--
--  <screenshot>gtk-link_button</screenshot>
--  <group>Buttons and Toggles</group>
--  <gtkada_demo>create_link_buttons.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Button;            use Gtk.Button;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;

package Gtk.Link_Button is

   type Gtk_Link_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Link_Button is access all Gtk_Link_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Link_Button; URI : UTF8_String);
   procedure Initialize
      (Self : not null access Gtk_Link_Button_Record'Class;
       URI  : UTF8_String);
   --  Creates a new `GtkLinkButton` with the URI as its text.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param URI a valid URI

   function Gtk_Link_Button_New (URI : UTF8_String) return Gtk_Link_Button;
   --  Creates a new `GtkLinkButton` with the URI as its text.
   --  @param URI a valid URI

   procedure Gtk_New_With_Label
      (Self  : out Gtk_Link_Button;
       URI   : UTF8_String;
       Label : UTF8_String := "");
   procedure Initialize_With_Label
      (Self  : not null access Gtk_Link_Button_Record'Class;
       URI   : UTF8_String;
       Label : UTF8_String := "");
   --  Creates a new `GtkLinkButton` containing a label.
   --  Initialize_With_Label does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param URI a valid URI
   --  @param Label the text of the button

   function Gtk_Link_Button_New_With_Label
      (URI   : UTF8_String;
       Label : UTF8_String := "") return Gtk_Link_Button;
   --  Creates a new `GtkLinkButton` containing a label.
   --  @param URI a valid URI
   --  @param Label the text of the button

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_link_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Uri
      (Self : not null access Gtk_Link_Button_Record) return UTF8_String;
   --  Retrieves the URI of the `GtkLinkButton`.
   --  @return a valid URI. The returned string is owned by the link button
   --  and should not be modified or freed.

   procedure Set_Uri
      (Self : not null access Gtk_Link_Button_Record;
       URI  : UTF8_String);
   --  Sets Uri as the URI where the `GtkLinkButton` points.
   --  As a side-effect this unsets the "visited" state of the button.
   --  @param URI a valid URI

   function Get_Visited
      (Self : not null access Gtk_Link_Button_Record) return Boolean;
   --  Retrieves the "visited" state of the `GtkLinkButton`.
   --  The button becomes visited when it is clicked. If the URI is changed on
   --  the button, the "visited" state is unset again.
   --  The state may also be changed using [methodGtk.LinkButton.set_visited].
   --  @return True if the link has been visited, False otherwise

   procedure Set_Visited
      (Self    : not null access Gtk_Link_Button_Record;
       Visited : Boolean);
   --  Sets the "visited" state of the `GtkLinkButton`.
   --  See [methodGtk.LinkButton.get_visited] for more details.
   --  @param Visited the new "visited" state

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Link_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Link_Button_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Link_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Link_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Link_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Link_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Link_Button_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Link_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Link_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Link_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Link_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Link_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Link_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Link_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Link_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   URI_Property : constant Glib.Properties.Property_String;
   --  The URI bound to this button.

   Visited_Property : constant Glib.Properties.Property_Boolean;
   --  The 'visited' state of this button.
   --
   --  A visited link is drawn in a different color.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Link_Button_Boolean is not null access function
     (Self : access Gtk_Link_Button_Record'Class) return Boolean;

   type Cb_GObject_Boolean is not null access function
     (Self : access Glib.Object.GObject_Record'Class)
   return Boolean;

   Signal_Activate_Link : constant Glib.Signal_Name := "activate-link";
   procedure On_Activate_Link
      (Self  : not null access Gtk_Link_Button_Record;
       Call  : Cb_Gtk_Link_Button_Boolean;
       After : Boolean := False);
   procedure On_Activate_Link
      (Self  : not null access Gtk_Link_Button_Record;
       Call  : Cb_GObject_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted each time the `GtkLinkButton` is clicked.
   --
   --  The default handler will call [methodGtk.FileLauncher.launch] with the
   --  URI stored inside the [propertyGtk.LinkButton:uri] property.
   --
   --  To override the default behavior, you can connect to the
   --  ::activate-link signal and stop the propagation of the signal by
   --  returning True from your handler.
   -- 
   --  Callback parameters:

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

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Link_Button_Record, Gtk_Link_Button);
   function "+"
     (Widget : access Gtk_Link_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Link_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Link_Button_Record, Gtk_Link_Button);
   function "+"
     (Widget : access Gtk_Link_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Link_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Link_Button_Record, Gtk_Link_Button);
   function "+"
     (Widget : access Gtk_Link_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Link_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Visited_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visited");
   URI_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("uri");
end Gtk.Link_Button;
