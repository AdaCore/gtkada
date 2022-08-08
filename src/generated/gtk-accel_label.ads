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
--  The Gtk.Accel_Label.Gtk_Accel_Label widget is a subclass of
--  Gtk.Label.Gtk_Label that also displays an accelerator key on the right of
--  the label text, e.g. "Ctrl+S". It is commonly used in menus to show the
--  keyboard short-cuts for commands.
--
--  The accelerator key to display is typically not set explicitly (although
--  it can be, with Gtk.Accel_Label.Set_Accel). Instead, the
--  Gtk.Accel_Label.Gtk_Accel_Label displays the accelerators which have been
--  added to a particular widget. This widget is set by calling
--  Gtk.Accel_Label.Set_Accel_Widget.
--
--  For example, a Gtk.Menu_Item.Gtk_Menu_Item widget may have an accelerator
--  added to emit the "activate" signal when the "Ctrl+S" key combination is
--  pressed. A Gtk.Accel_Label.Gtk_Accel_Label is created and added to the
--  Gtk.Menu_Item.Gtk_Menu_Item, and Gtk.Accel_Label.Set_Accel_Widget is called
--  with the Gtk.Menu_Item.Gtk_Menu_Item as the second argument. The
--  Gtk.Accel_Label.Gtk_Accel_Label will now display "Ctrl+S" after its label.
--
--  Note that creating a Gtk.Menu_Item.Gtk_Menu_Item with
--  Gtk.Menu_Item.Gtk_New_With_Label (or one of the similar functions for
--  Gtk.Check_Menu_Item.Gtk_Check_Menu_Item and
--  Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item) automatically adds a
--  Gtk.Accel_Label.Gtk_Accel_Label to the Gtk.Menu_Item.Gtk_Menu_Item and
--  calls Gtk.Accel_Label.Set_Accel_Widget to set it up for you.
--
--  A Gtk.Accel_Label.Gtk_Accel_Label will only display accelerators which
--  have Gtk.Target_List.Accel_Visible set (see
--  Gtk.Accel_Group.Gtk_Accel_Flags). A Gtk.Accel_Label.Gtk_Accel_Label can
--  display multiple accelerators and even signal names, though it is almost
--  always used to display just one accelerator key.
--
--  ## Creating a simple menu item with an accelerator key.
--
--  |[<!-- language="C" --> GtkWidget *window = gtk_window_new
--  (GTK_WINDOW_TOPLEVEL); GtkWidget *menu = gtk_menu_new (); GtkWidget
--  *save_item; GtkAccelGroup *accel_group;
--
--  // Create a GtkAccelGroup and add it to the window. accel_group =
--  gtk_accel_group_new (); gtk_window_add_accel_group (GTK_WINDOW (window),
--  accel_group);
--
--  // Create the menu item using the convenience function. save_item =
--  gtk_menu_item_new_with_label ("Save"); gtk_widget_show (save_item);
--  gtk_container_add (GTK_CONTAINER (menu), save_item);
--
--  // Now add the accelerator to the GtkMenuItem. Note that since we //
--  called Gtk.Menu_Item.Gtk_New_With_Label to create the GtkMenuItem // the
--  GtkAccelLabel is automatically set up to display the // GtkMenuItem
--  accelerators. We just need to make sure we use // GTK_ACCEL_VISIBLE here.
--  gtk_widget_add_accelerator (save_item, "activate", accel_group, GDK_KEY_s,
--  GDK_CONTROL_MASK, GTK_ACCEL_VISIBLE); ]|
--
--  # CSS nodes
--
--  |[<!-- language="plain" --> label ╰── accelerator ]|
--
--  Like Gtk.Label.Gtk_Label, GtkAccelLabel has a main CSS node with the name
--  label. It adds a subnode with name accelerator.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;       use Gdk.Types;
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Label;       use Gtk.Label;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Accel_Label is

   type Gtk_Accel_Label_Record is new Gtk_Label_Record with null record;
   type Gtk_Accel_Label is access all Gtk_Accel_Label_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Accel_Label : out Gtk_Accel_Label;
       String      : UTF8_String);
   procedure Initialize
      (Accel_Label : not null access Gtk_Accel_Label_Record'Class;
       String      : UTF8_String);
   --  Creates a new Gtk.Accel_Label.Gtk_Accel_Label.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "string": the label string. Must be non-null.

   function Gtk_Accel_Label_New
      (String : UTF8_String) return Gtk_Accel_Label;
   --  Creates a new Gtk.Accel_Label.Gtk_Accel_Label.
   --  "string": the label string. Must be non-null.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accel_label_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Accel
      (Accel_Label      : not null access Gtk_Accel_Label_Record;
       Accelerator_Key  : out Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : out Gdk.Types.Gdk_Modifier_Type);
   --  Gets the keyval and modifier mask set with Gtk.Accel_Label.Set_Accel.
   --  Since: gtk+ 3.12
   --  "accelerator_key": return location for the keyval
   --  "accelerator_mods": return location for the modifier mask

   procedure Set_Accel
      (Accel_Label      : not null access Gtk_Accel_Label_Record;
       Accelerator_Key  : Gdk.Types.Gdk_Key_Type;
       Accelerator_Mods : Gdk.Types.Gdk_Modifier_Type);
   --  Manually sets a keyval and modifier mask as the accelerator rendered by
   --  Accel_Label.
   --  If a keyval and modifier are explicitly set then these values are used
   --  regardless of any associated accel closure or widget.
   --  Providing an Accelerator_Key of 0 removes the manual setting.
   --  Since: gtk+ 3.6
   --  "accelerator_key": a keyval, or 0
   --  "accelerator_mods": the modifier mask for the accel

   function Get_Accel_Widget
      (Accel_Label : not null access Gtk_Accel_Label_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Fetches the widget monitored by this accelerator label. See
   --  Gtk.Accel_Label.Set_Accel_Widget.

   procedure Set_Accel_Widget
      (Accel_Label  : not null access Gtk_Accel_Label_Record;
       Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the widget to be monitored by this accelerator label. Passing null
   --  for Accel_Widget will dissociate Accel_Label from its current widget, if
   --  any.
   --  "accel_widget": the widget to be monitored, or null

   function Get_Accel_Width
      (Accel_Label : not null access Gtk_Accel_Label_Record) return Guint;
   --  Returns the width needed to display the accelerator key(s). This is
   --  used by menus to align all of the Gtk.Menu_Item.Gtk_Menu_Item widgets,
   --  and shouldn't be needed by applications.

   function Refetch
      (Accel_Label : not null access Gtk_Accel_Label_Record) return Boolean;
   --  Recreates the string representing the accelerator keys. This should not
   --  be needed since the string is automatically updated whenever
   --  accelerators are added or removed from the associated widget.

   procedure Set_Accel_Closure
      (Accel_Label   : not null access Gtk_Accel_Label_Record;
       Accel_Closure : System.Address);
   --  Sets the closure to be monitored by this accelerator label. The closure
   --  must be connected to an accelerator group; see Gtk.Accel_Group.Connect.
   --  Passing null for Accel_Closure will dissociate Accel_Label from its
   --  current closure, if any.
   --  "accel_closure": the closure to monitor for accelerator changes, or
   --  null

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Accel_Closure_Property : constant Glib.Properties.Property_String :=
   Glib.Properties.Build ("accel-closure");--  Unknown type: GObject.Closure

   Accel_Widget_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Accel_Label_Record, Gtk_Accel_Label);
   function "+"
     (Widget : access Gtk_Accel_Label_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Accel_Label
   renames Implements_Gtk_Buildable.To_Object;

private
   Accel_Widget_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("accel-widget");
end Gtk.Accel_Label;
