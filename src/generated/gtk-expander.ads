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
--  A Gtk.Expander.Gtk_Expander allows the user to hide or show its child by
--  clicking on an expander triangle similar to the triangles used in a
--  Gtk.Tree_View.Gtk_Tree_View.
--
--  Normally you use an expander as you would use any other descendant of
--  Gtk.Bin.Gtk_Bin; you create the child widget and use Gtk.Container.Add to
--  add it to the expander. When the expander is toggled, it will take care of
--  showing and hiding the child automatically.
--
--  # Special Usage
--
--  There are situations in which you may prefer to show and hide the expanded
--  widget yourself, such as when you want to actually create the widget at
--  expansion time. In this case, create a Gtk.Expander.Gtk_Expander but do not
--  add a child to it. The expander widget has an
--  Gtk.Expander.Gtk_Expander:expanded property which can be used to monitor
--  its expansion state. You should watch this property with a signal
--  connection as follows:
--
--  |[<!-- language="C" --> static void expander_callback (GObject *object,
--  GParamSpec *param_spec, gpointer user_data) { GtkExpander *expander;
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
--  // ... } ]|
--
--  # GtkExpander as GtkBuildable
--
--  The GtkExpander implementation of the GtkBuildable interface supports
--  placing a child in the label position by specifying "label" as the "type"
--  attribute of a <child> element. A normal content child can be specified
--  without specifying a <child> type attribute.
--
--  An example of a UI definition fragment with GtkExpander: |[ <object
--  class="GtkExpander"> <child type="label"> <object class="GtkLabel"
--  id="expander-label"/> </child> <child> <object class="GtkEntry"
--  id="expander-content"/> </child> </object> ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> expander ├── title │ ├── arrow │ ╰── <label
--  widget> ╰── <child> ]|
--
--  GtkExpander has three CSS nodes, the main node with the name expander, a
--  subnode with name title and node below it with name arrow. The arrow of an
--  expander that is showing its child gets the :checked pseudoclass added to
--  it.
--
--  </description>
--  <screenshot>gtk-expanded</screenshot>
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Bin;         use Gtk.Bin;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Expander is

   type Gtk_Expander_Record is new Gtk_Bin_Record with null record;
   type Gtk_Expander is access all Gtk_Expander_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Expander : out Gtk_Expander;
       Label    : UTF8_String := "");
   procedure Initialize
      (Expander : not null access Gtk_Expander_Record'Class;
       Label    : UTF8_String := "");
   --  Creates a new expander using Label as the text of the label.
   --  Since: gtk+ 2.4
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "label": the text of the label

   function Gtk_Expander_New (Label : UTF8_String := "") return Gtk_Expander;
   --  Creates a new expander using Label as the text of the label.
   --  Since: gtk+ 2.4
   --  "label": the text of the label

   procedure Gtk_New_With_Mnemonic
      (Expander : out Gtk_Expander;
       Label    : UTF8_String := "");
   procedure Initialize_With_Mnemonic
      (Expander : not null access Gtk_Expander_Record'Class;
       Label    : UTF8_String := "");
   --  Creates a new expander using Label as the text of the label. If
   --  characters in Label are preceded by an underscore, they are underlined.
   --  If you need a literal underscore character in a label, use "__" (two
   --  underscores). The first underlined character represents a keyboard
   --  accelerator called a mnemonic. Pressing Alt and that key activates the
   --  button.
   --  Since: gtk+ 2.4
   --  Initialize_With_Mnemonic does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "label": the text of the label with an underscore in front of the
   --  mnemonic character

   function Gtk_Expander_New_With_Mnemonic
      (Label : UTF8_String := "") return Gtk_Expander;
   --  Creates a new expander using Label as the text of the label. If
   --  characters in Label are preceded by an underscore, they are underlined.
   --  If you need a literal underscore character in a label, use "__" (two
   --  underscores). The first underlined character represents a keyboard
   --  accelerator called a mnemonic. Pressing Alt and that key activates the
   --  button.
   --  Since: gtk+ 2.4
   --  "label": the text of the label with an underscore in front of the
   --  mnemonic character

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_expander_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Expanded
      (Expander : not null access Gtk_Expander_Record) return Boolean;
   --  Queries a Gtk.Expander.Gtk_Expander and returns its current state.
   --  Returns True if the child widget is revealed.
   --  See Gtk.Expander.Set_Expanded.
   --  Since: gtk+ 2.4

   procedure Set_Expanded
      (Expander : not null access Gtk_Expander_Record;
       Expanded : Boolean);
   --  Sets the state of the expander. Set to True, if you want the child
   --  widget to be revealed, and False if you want the child widget to be
   --  hidden.
   --  Since: gtk+ 2.4
   --  "expanded": whether the child widget is revealed

   function Get_Label
      (Expander : not null access Gtk_Expander_Record) return UTF8_String;
   --  Fetches the text from a label widget including any embedded underlines
   --  indicating mnemonics and Pango markup, as set by Gtk.Expander.Set_Label.
   --  If the label text has not been set the return value will be null. This
   --  will be the case if you create an empty button with gtk_button_new to
   --  use as a container.
   --  Note that this function behaved differently in versions prior to 2.14
   --  and used to return the label text stripped of embedded underlines
   --  indicating mnemonics and Pango markup. This problem can be avoided by
   --  fetching the label text directly from the label widget.
   --  Since: gtk+ 2.4

   procedure Set_Label
      (Expander : not null access Gtk_Expander_Record;
       Label    : UTF8_String := "");
   --  Sets the text of the label of the expander to Label.
   --  This will also clear any previously set labels.
   --  Since: gtk+ 2.4
   --  "label": a string

   function Get_Label_Fill
      (Expander : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether the label widget will fill all available horizontal
   --  space allocated to Expander.
   --  Since: gtk+ 2.22

   procedure Set_Label_Fill
      (Expander   : not null access Gtk_Expander_Record;
       Label_Fill : Boolean);
   --  Sets whether the label widget should fill all available horizontal
   --  space allocated to Expander.
   --  Note that this function has no effect since 3.20.
   --  Since: gtk+ 2.22
   --  "label_fill": True if the label should should fill all available
   --  horizontal space

   function Get_Label_Widget
      (Expander : not null access Gtk_Expander_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the label widget for the frame. See
   --  Gtk.Expander.Set_Label_Widget.
   --  Since: gtk+ 2.4

   procedure Set_Label_Widget
      (Expander     : not null access Gtk_Expander_Record;
       Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Set the label widget for the expander. This is the widget that will
   --  appear embedded alongside the expander arrow.
   --  Since: gtk+ 2.4
   --  "label_widget": the new label widget

   function Get_Resize_Toplevel
      (Expander : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether the expander will resize the toplevel widget containing
   --  the expander upon resizing and collpasing.
   --  Since: gtk+ 3.2

   procedure Set_Resize_Toplevel
      (Expander        : not null access Gtk_Expander_Record;
       Resize_Toplevel : Boolean);
   --  Sets whether the expander will resize the toplevel widget containing
   --  the expander upon resizing and collpasing.
   --  Since: gtk+ 3.2
   --  "resize_toplevel": whether to resize the toplevel

   function Get_Spacing
      (Expander : not null access Gtk_Expander_Record) return Glib.Gint;
   pragma Obsolescent (Get_Spacing);
   --  Gets the value set by Gtk.Expander.Set_Spacing.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.20, 1

   procedure Set_Spacing
      (Expander : not null access Gtk_Expander_Record;
       Spacing  : Glib.Gint);
   pragma Obsolescent (Set_Spacing);
   --  Sets the spacing field of Expander, which is the number of pixels to
   --  place between expander and the child.
   --  Since: gtk+ 2.4
   --  Deprecated since 3.20, 1
   --  "spacing": distance between the expander and child in pixels

   function Get_Use_Markup
      (Expander : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether the label's text is interpreted as marked up with the
   --  [Pango text markup language][PangoMarkupFormat]. See
   --  Gtk.Expander.Set_Use_Markup.
   --  Since: gtk+ 2.4

   procedure Set_Use_Markup
      (Expander   : not null access Gtk_Expander_Record;
       Use_Markup : Boolean);
   --  Sets whether the text of the label contains markup in [Pango's text
   --  markup language][PangoMarkupFormat]. See Gtk.Label.Set_Markup.
   --  Since: gtk+ 2.4
   --  "use_markup": True if the label's text should be parsed for markup

   function Get_Use_Underline
      (Expander : not null access Gtk_Expander_Record) return Boolean;
   --  Returns whether an embedded underline in the expander label indicates a
   --  mnemonic. See Gtk.Expander.Set_Use_Underline.
   --  Since: gtk+ 2.4

   procedure Set_Use_Underline
      (Expander      : not null access Gtk_Expander_Record;
       Use_Underline : Boolean);
   --  If true, an underline in the text of the expander label indicates the
   --  next character should be used for the mnemonic accelerator key.
   --  Since: gtk+ 2.4
   --  "use_underline": True if underlines in the text indicate mnemonics

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Expanded_Property : constant Glib.Properties.Property_Boolean;

   Label_Property : constant Glib.Properties.Property_String;

   Label_Fill_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the label widget should fill all available horizontal space.
   --
   --  Note that this property is ignored since 3.20.

   Label_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   Resize_Toplevel_Property : constant Glib.Properties.Property_Boolean;
   --  When this property is True, the expander will resize the toplevel
   --  widget containing the expander upon expanding and collapsing.

   Spacing_Property : constant Glib.Properties.Property_Int;
   --  Space to put between the label and the child when the expander is
   --  expanded.

   Use_Markup_Property : constant Glib.Properties.Property_Boolean;

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;

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

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Expander_Record, Gtk_Expander);
   function "+"
     (Widget : access Gtk_Expander_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Expander
   renames Implements_Gtk_Buildable.To_Object;

private
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Resize_Toplevel_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("resize-toplevel");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Label_Fill_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("label-fill");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expanded");
end Gtk.Expander;
