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

--  Allows the user to reveal or conceal a child widget.
--
--  <picture> <source srcset="expander-dark.png" media="(prefers-color-scheme:
--  dark)"> <img alt="An example GtkExpander" src="expander.png"> </picture>
--  This is similar to the triangles used in a `GtkTreeView`.
--
--  Normally you use an expander as you would use a frame; you create the
--  child widget and use [methodGtk.Expander.set_child] to add it to the
--  expander. When the expander is toggled, it will take care of showing and
--  hiding the child automatically.
--
--  # Special Usage
--
--  There are situations in which you may prefer to show and hide the expanded
--  widget yourself, such as when you want to actually create the widget at
--  expansion time. In this case, create a `GtkExpander` but do not add a child
--  to it. The expander widget has an [propertyGtk.Expander:expanded] property
--  which can be used to monitor its expansion state. You should watch this
--  property with a signal connection as follows:
--
--  ```c static void expander_callback (GObject *object, GParamSpec
--  *param_spec, gpointer user_data) { GtkExpander *expander;
--
--  expander = GTK_EXPANDER (object);
--
--  if (gtk_expander_get_expanded (expander)) { // Show or create widgets }
--  else { // Hide or destroy widgets } }
--
--  static void create_expander (void) { GtkWidget *expander =
--  gtk_expander_new_with_mnemonic ("_More Options"); g_signal_connect
--  (expander, "notify::expanded", G_CALLBACK (expander_callback), NULL);
--
--  // ... } ```
--
--  # GtkExpander as GtkBuildable
--
--  An example of a UI definition fragment with GtkExpander:
--
--  ```xml <object class="GtkExpander"> <property name="label-widget"> <object
--  class="GtkLabel" id="expander-label"/> </property> <property name="child">
--  <object class="GtkEntry" id="expander-content"/> </property> </object> ```
--
--  # CSS nodes
--
--  ``` expander-widget ╰── box ├── title │ ├── expander │ ╰── <label widget>
--  ╰── <child> ```
--
--  `GtkExpander` has a main node `expander-widget`, and subnode `box`
--  containing the title and child widget. The box subnode `title` contains
--  node `expander`, i.e. the expand/collapse arrow; then the label widget if
--  any. The arrow of an expander that is showing its child gets the `:checked`
--  pseudoclass set on it.
--
--  # Accessibility
--
--  `GtkExpander` uses the [enumGtk.AccessibleRole.button] role.
--
--  <screenshot>gtk-expander</screenshot>
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Expander is

   type Gtk_Expander_Record is new Gtk_Widget_Record with null record;
   type Gtk_Expander is access all Gtk_Expander_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Expander; Label : UTF8_String := "");
   procedure Initialize
      (Self  : not null access Gtk_Expander_Record'Class;
       Label : UTF8_String := "");
   --  Creates a new expander using Label as the text of the label.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Label the text of the label

   function Gtk_Expander_New (Label : UTF8_String := "") return Gtk_Expander;
   --  Creates a new expander using Label as the text of the label.
   --  @param Label the text of the label

   procedure Gtk_New_With_Mnemonic
      (Self  : out Gtk_Expander;
       Label : UTF8_String := "");
   procedure Initialize_With_Mnemonic
      (Self  : not null access Gtk_Expander_Record'Class;
       Label : UTF8_String := "");
   --  Creates a new expander using Label as the text of the label.
   --  If characters in Label are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  "__" (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic.
   --  Pressing Alt and that key activates the button.
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  @param Label the text of the label with an underscore in front of the
   --  mnemonic character

   function Gtk_Expander_New_With_Mnemonic
      (Label : UTF8_String := "") return Gtk_Expander;
   --  Creates a new expander using Label as the text of the label.
   --  If characters in Label are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  "__" (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic.
   --  Pressing Alt and that key activates the button.
   --  @param Label the text of the label with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_expander_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Gets the child widget of Expander.
   --  @return the child widget of Expander

   procedure Set_Child
      (Self  : not null access Gtk_Expander_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the child widget of Expander.
   --  @param Child the child widget

   function Get_Expanded
      (Self : not null access Gtk_Expander_Record) return Boolean;
   --  Queries a `GtkExpander` and returns its current state.
   --  Returns True if the child widget is revealed.
   --  @return the current state of the expander

   procedure Set_Expanded
      (Self     : not null access Gtk_Expander_Record;
       Expanded : Boolean);
   --  Sets the state of the expander.
   --  Set to True, if you want the child widget to be revealed, and False if
   --  you want the child widget to be hidden.
   --  @param Expanded whether the child widget is revealed

   function Get_Label
      (Self : not null access Gtk_Expander_Record) return UTF8_String;
   --  Fetches the text from a label widget.
   --  This is including any embedded underlines indicating mnemonics and
   --  Pango markup, as set by [methodGtk.Expander.set_label]. If the label
   --  text has not been set the return value will be null. This will be the
   --  case if you create an empty button with gtk_button_new to use as a
   --  container.
   --  @return The text of the label widget. This string is owned by the
   --  widget and must not be modified or freed.

   procedure Set_Label
      (Self  : not null access Gtk_Expander_Record;
       Label : UTF8_String := "");
   --  Sets the text of the label of the expander to Label.
   --  This will also clear any previously set labels.
   --  @param Label a string

   function Get_Label_Widget
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the label widget for the frame.
   --  @return the label widget

   procedure Set_Label_Widget
      (Self         : not null access Gtk_Expander_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the label widget for the expander.
   --  This is the widget that will appear embedded alongside the expander
   --  arrow.
   --  @param Label_Widget the new label widget

   function Get_Resize_Toplevel
      (Self : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether the expander will resize the toplevel widget containing
   --  the expander upon resizing and collapsing.
   --  @return the "resize toplevel" setting.

   procedure Set_Resize_Toplevel
      (Self            : not null access Gtk_Expander_Record;
       Resize_Toplevel : Boolean);
   --  Sets whether the expander will resize the toplevel widget containing
   --  the expander upon resizing and collapsing.
   --  @param Resize_Toplevel whether to resize the toplevel

   function Get_Use_Markup
      (Self : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether the label's text is interpreted as Pango markup.
   --  @return True if the label's text will be parsed for markup

   procedure Set_Use_Markup
      (Self       : not null access Gtk_Expander_Record;
       Use_Markup : Boolean);
   --  Sets whether the text of the label contains Pango markup.
   --  @param Use_Markup True if the label's text should be parsed for markup

   function Get_Use_Underline
      (Self : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether an underline in the text indicates a mnemonic.
   --  @return True if an embedded underline in the expander label indicates
   --  the mnemonic accelerator keys

   procedure Set_Use_Underline
      (Self          : not null access Gtk_Expander_Record;
       Use_Underline : Boolean);
   --  If true, an underline in the text indicates a mnemonic.
   --  @param Use_Underline True if underlines in the text indicate mnemonics

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Expander_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Expander_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Expander_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Expander_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Expander_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Expander_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Expander_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Expander_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Expander_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Expanded_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the expander has been opened to reveal the child.

   Label_Property : constant Glib.Properties.Property_String;
   --  The text of the expanders label.

   Label_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  A widget to display instead of the usual expander label.

   Resize_Toplevel_Property : constant Glib.Properties.Property_Boolean;
   --  When this property is True, the expander will resize the toplevel
   --  widget containing the expander upon expanding and collapsing.

   Use_Markup_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the text in the label is Pango markup.

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   --  Whether an underline in the text indicates a mnemonic.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Expander_Void is not null access procedure (Self : access Gtk_Expander_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Expander_Record;
       Call  : Cb_Gtk_Expander_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Expander_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Activates the `GtkExpander`.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Accessible"
   --
   --  - "ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Expander_Record, Gtk_Expander);
   function "+"
     (Widget : access Gtk_Expander_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Expander
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Expander_Record, Gtk_Expander);
   function "+"
     (Widget : access Gtk_Expander_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Expander
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Resize_Toplevel_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resize-toplevel");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expanded");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Expander;
