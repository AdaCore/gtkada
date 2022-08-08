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
--  A GtkLinkButton is a Gtk.Button.Gtk_Button with a hyperlink, similar to
--  the one used by web browsers, which triggers an action when clicked. It is
--  useful to show quick links to resources.
--
--  A link button is created by calling either Gtk.Link_Button.Gtk_New or
--  Gtk.Link_Button.Gtk_New_With_Label. If using the former, the URI you pass
--  to the constructor is used as a label for the widget.
--
--  The URI bound to a GtkLinkButton can be set specifically using
--  Gtk.Link_Button.Set_Uri, and retrieved using Gtk.Link_Button.Get_Uri.
--
--  By default, GtkLinkButton calls gtk_show_uri_on_window when the button is
--  clicked. This behaviour can be overridden by connecting to the
--  Gtk.Link_Button.Gtk_Link_Button::activate-link signal and returning True
--  from the signal handler.
--
--  # CSS nodes
--
--  GtkLinkButton has a single CSS node with name button. To differentiate it
--  from a plain Gtk.Button.Gtk_Button, it gets the .link style class.
--
--  </description>
--  <group>Buttons and Toggles</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;
with Gtk.Action;      use Gtk.Action;
with Gtk.Actionable;  use Gtk.Actionable;
with Gtk.Activatable; use Gtk.Activatable;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Button;      use Gtk.Button;

package Gtk.Link_Button is

   type Gtk_Link_Button_Record is new Gtk_Button_Record with null record;
   type Gtk_Link_Button is access all Gtk_Link_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Widget : out Gtk_Link_Button; URI : UTF8_String);
   procedure Initialize
      (Widget : not null access Gtk_Link_Button_Record'Class;
       URI    : UTF8_String);
   --  Creates a new Gtk.Link_Button.Gtk_Link_Button with the URI as its text.
   --  Since: gtk+ 2.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "uri": a valid URI

   function Gtk_Link_Button_New (URI : UTF8_String) return Gtk_Link_Button;
   --  Creates a new Gtk.Link_Button.Gtk_Link_Button with the URI as its text.
   --  Since: gtk+ 2.10
   --  "uri": a valid URI

   procedure Gtk_New_With_Label
      (Widget : out Gtk_Link_Button;
       URI    : UTF8_String;
       Label  : UTF8_String := "");
   procedure Initialize_With_Label
      (Widget : not null access Gtk_Link_Button_Record'Class;
       URI    : UTF8_String;
       Label  : UTF8_String := "");
   --  Creates a new Gtk.Link_Button.Gtk_Link_Button containing a label.
   --  Since: gtk+ 2.10
   --  Initialize_With_Label does nothing if the object was already created
   --  with another call to Initialize* or G_New.
   --  "uri": a valid URI
   --  "label": the text of the button

   function Gtk_Link_Button_New_With_Label
      (URI   : UTF8_String;
       Label : UTF8_String := "") return Gtk_Link_Button;
   --  Creates a new Gtk.Link_Button.Gtk_Link_Button containing a label.
   --  Since: gtk+ 2.10
   --  "uri": a valid URI
   --  "label": the text of the button

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_link_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Uri
      (Widget : not null access Gtk_Link_Button_Record) return UTF8_String;
   --  Retrieves the URI set using Gtk.Link_Button.Set_Uri.
   --  Since: gtk+ 2.10

   procedure Set_Uri
      (Widget : not null access Gtk_Link_Button_Record;
       URI    : UTF8_String);
   --  Sets Uri as the URI where the Gtk.Link_Button.Gtk_Link_Button points.
   --  As a side-effect this unsets the "visited" state of the button.
   --  Since: gtk+ 2.10
   --  "uri": a valid URI

   function Get_Visited
      (Widget : not null access Gtk_Link_Button_Record) return Boolean;
   --  Retrieves the "visited" state of the URI where the
   --  Gtk.Link_Button.Gtk_Link_Button points. The button becomes visited when
   --  it is clicked. If the URI is changed on the button, the "visited" state
   --  is unset again.
   --  The state may also be changed using Gtk.Link_Button.Set_Visited.
   --  Since: gtk+ 2.14

   procedure Set_Visited
      (Widget  : not null access Gtk_Link_Button_Record;
       Visited : Boolean);
   --  Sets the "visited" state of the URI where the
   --  Gtk.Link_Button.Gtk_Link_Button points. See Gtk.Link_Button.Get_Visited
   --  for more details.
   --  Since: gtk+ 2.14
   --  "visited": the new "visited" state

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   function Get_Action_Name
      (Self : not null access Gtk_Link_Button_Record) return UTF8_String;

   procedure Set_Action_Name
      (Self        : not null access Gtk_Link_Button_Record;
       Action_Name : UTF8_String := "");

   function Get_Action_Target_Value
      (Self : not null access Gtk_Link_Button_Record)
       return Glib.Variant.Gvariant;

   procedure Set_Action_Target_Value
      (Self         : not null access Gtk_Link_Button_Record;
       Target_Value : Glib.Variant.Gvariant);

   procedure Set_Detailed_Action_Name
      (Self                 : not null access Gtk_Link_Button_Record;
       Detailed_Action_Name : UTF8_String);

   procedure Do_Set_Related_Action
      (Self   : not null access Gtk_Link_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Do_Set_Related_Action);

   function Get_Related_Action
      (Self : not null access Gtk_Link_Button_Record)
       return Gtk.Action.Gtk_Action;
   pragma Obsolescent (Get_Related_Action);

   procedure Set_Related_Action
      (Self   : not null access Gtk_Link_Button_Record;
       Action : not null access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Set_Related_Action);

   function Get_Use_Action_Appearance
      (Self : not null access Gtk_Link_Button_Record) return Boolean;
   pragma Obsolescent (Get_Use_Action_Appearance);

   procedure Set_Use_Action_Appearance
      (Self           : not null access Gtk_Link_Button_Record;
       Use_Appearance : Boolean);
   pragma Obsolescent (Set_Use_Action_Appearance);

   procedure Sync_Action_Properties
      (Self   : not null access Gtk_Link_Button_Record;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   pragma Obsolescent (Sync_Action_Properties);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   URI_Property : constant Glib.Properties.Property_String;
   --  The URI bound to this button.

   Visited_Property : constant Glib.Properties.Property_Boolean;
   --  The 'visited' state of this button. A visited link is drawn in a
   --  different color.

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
   --  The ::activate-link signal is emitted each time the
   --  Gtk.Link_Button.Gtk_Link_Button has been clicked.
   --
   --  The default handler will call gtk_show_uri_on_window with the URI
   --  stored inside the Gtk.Link_Button.Gtk_Link_Button:uri property.
   --
   --  To override the default behavior, you can connect to the
   --  ::activate-link signal and stop the propagation of the signal by
   --  returning True from your handler.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Actionable"
   --
   --  - "Activatable"
   --
   --  - "Buildable"

   package Implements_Gtk_Actionable is new Glib.Types.Implements
     (Gtk.Actionable.Gtk_Actionable, Gtk_Link_Button_Record, Gtk_Link_Button);
   function "+"
     (Widget : access Gtk_Link_Button_Record'Class)
   return Gtk.Actionable.Gtk_Actionable
   renames Implements_Gtk_Actionable.To_Interface;
   function "-"
     (Interf : Gtk.Actionable.Gtk_Actionable)
   return Gtk_Link_Button
   renames Implements_Gtk_Actionable.To_Object;

   package Implements_Gtk_Activatable is new Glib.Types.Implements
     (Gtk.Activatable.Gtk_Activatable, Gtk_Link_Button_Record, Gtk_Link_Button);
   function "+"
     (Widget : access Gtk_Link_Button_Record'Class)
   return Gtk.Activatable.Gtk_Activatable
   renames Implements_Gtk_Activatable.To_Interface;
   function "-"
     (Interf : Gtk.Activatable.Gtk_Activatable)
   return Gtk_Link_Button
   renames Implements_Gtk_Activatable.To_Object;

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

private
   Visited_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visited");
   URI_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("uri");
end Gtk.Link_Button;
