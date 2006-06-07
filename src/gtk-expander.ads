-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  A container which can hide its child.
--  </description>
--  <c_version>2.8.17</c_version>
--  <group>Layout Containers</group>
--  <screenshot>gtk-expander</screenshot>

with Glib.Properties;
with Gtk.Bin;
with Gtk.Widget;

package Gtk.Expander is

   type Gtk_Expander_Record is new Gtk.Bin.Gtk_Bin_Record with null record;
   type Gtk_Expander is access all Gtk_Expander_Record'Class;

   procedure Gtk_New (Expander : out Gtk_Expander; Label : String);
   procedure Initialize
     (Expander : access Gtk_Expander_Record'Class; Label  : String);
   --  Creates or initializes a new expander, using Label as the text of the
   --  label.

   procedure Gtk_New_With_Mnemonic
     (Expander : out Gtk_Expander; Label : String);
   procedure Initialize_With_Mnemonic
     (Expander : access Gtk_Expander_Record'Class; Label  : String);
   --  Creates or initializes a new expander, using Label as the text of the
   --  label.
   --  If characters in Label are preceded by an underscore, they are
   --  underlined. If you need a literal underscore character in a label, use
   --  '__' (two underscores). The first underlined character represents a
   --  keyboard accelerator called a mnemonic.
   --  Pressing Alt and that key activates the button.

   function Get_Type return Glib.GType;
   --  Returns the internal value used for an expander

   procedure Set_Expanded
     (Expander : access Gtk_Expander_Record; Expanded : Boolean);
   function Get_Expanded
     (Expander : access Gtk_Expander_Record) return Boolean;
   --  Sets the state of the expander. Set to True, if you want
   --  the child widget to be revealed, and False if you want the
   --  child widget to be hidden.

   procedure Set_Label
     (Expander : access Gtk_Expander_Record; Label : String);
   function Get_Label
     (Expander : access Gtk_Expander_Record) return String;
   --  Sets the text of the label of the expander to Label.

   procedure Set_Label_Widget
     (Expander     : access Gtk_Expander_Record;
      Label_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Label_Widget
     (Expander : access Gtk_Expander_Record) return Gtk.Widget.Gtk_Widget;
   --  Set the label widget for the expander. This is the widget
   --  that will appear embedded alongside the expander arrow.

   procedure Set_Spacing
     (Expander : access Gtk_Expander_Record; Spacing  : Gint);
   function Get_Spacing
     (Expander : access Gtk_Expander_Record) return Gint;
   --  Sets the spacing field of Expander, which is the number of pixels to
   --  place between expander and the child.

   procedure Set_Use_Markup
     (Expander : access Gtk_Expander_Record; Use_Markup : Boolean);
   function Get_Use_Markup
     (Expander : access Gtk_Expander_Record) return Boolean;
   --  Sets whether the text of the label contains markup in Pango's text
   --  markup language. See Gtk.Label.Set_Markup.

   procedure Set_Use_Underline
     (Expander : access Gtk_Expander_Record; Use_Underline : Boolean);
   function Get_Use_Underline
     (Expander : access Gtk_Expander_Record) return Boolean;
   --  If true, an underline in the text of the expander label indicates
   --  the next character should be used for the mnemonic accelerator key.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "activate"
   --    procedure Handler (Expander : access Gtk_Expander_Record'Class);
   --    Send this signal if you want to toggle the state of the expander, as
   --    if the user had clicked on it. This is mostly useful when associated
   --    with a keybinding
   --
   --  </signals>

   Signal_Activate : constant String := "activate";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  Name:  Expanded_Property
   --  Type:  Boolean
   --  Descr: Whether the expander has been opened to reveal the child widget
   --
   --  Name:  Label_Property
   --  Type:  String
   --  Descr: Text of the expander's label
   --
   --  Name:  Label_Widget_Property
   --  Type:  Object
   --  Descr: A widget to display in place of the usual expander label
   --
   --  Name:  Spacing_Property
   --  Type:  Int
   --  Descr: Space to put between the label and the child
   --
   --  Name:  Use_Markup_Property
   --  Type:  Boolean
   --  Descr: The text of the label includes XML markup. See pango_parse_markup
   --
   --  Name:  Use_Underline_Property
   --  Type:  Boolean
   --  Descr: If set, an underline in the text indicates the next character
   --         should be used for the mnemonic accelerator key
   --
   --  </properties>

   Expanded_Property      : constant Glib.Properties.Property_Boolean;
   Label_Property         : constant Glib.Properties.Property_String;
   Label_Widget_Property  : constant Glib.Properties.Property_Object;
   Spacing_Property       : constant Glib.Properties.Property_Int;
   Use_Markup_Property    : constant Glib.Properties.Property_Boolean;
   Use_Underline_Property : constant Glib.Properties.Property_Boolean;

private
   pragma Import (C, Get_Type, "gtk_expander_get_type");

   Expanded_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("expanded");
   Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("label");
   Label_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("label-widget");
   Spacing_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("spacing");
   Use_Markup_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-markup");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");

end Gtk.Expander;
