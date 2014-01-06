------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
--  This interface provides a convenient way of associating widgets with
--  actions on a Gtk.Application_Window.Gtk_Application_Window or
--  Gtk.Application.Gtk_Application.
--
--  It primarily consists of two properties:
--  Gtk.Actionable.Gtk_Actionable:action-name and
--  Gtk.Actionable.Gtk_Actionable:action-target. There are also some
--  convenience APIs for setting these properties.
--
--  This interface is presently only meaningful if used on a widget that is
--  (or will be) located inside of a
--  Gtk.Application_Window.Gtk_Application_Window and can only be used to
--  associate the widget with actions on that window, or its associated
--  Gtk.Application.Gtk_Application.
--
--  </description>
pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Glib.Variant;    use Glib.Variant;

package Gtk.Actionable is

   type Gtk_Actionable is new Glib.Types.GType_Interface;
   Null_Gtk_Actionable : constant Gtk_Actionable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_actionable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Action_Name (Self : Gtk_Actionable) return UTF8_String;
   --  Gets the action name for Actionable.
   --  See Gtk.Actionable.Set_Action_Name for more information.
   --  Since: gtk+ 3.4

   procedure Set_Action_Name
      (Self        : Gtk_Actionable;
       Action_Name : UTF8_String);
   --  Specifies the name of the action with which this widget should be
   --  associated. If Action_Name is null then the widget will be unassociated
   --  from any previous action.
   --  Usually this function is used when the widget is located (or will be
   --  located) within the hierarchy of a
   --  Gtk.Application_Window.Gtk_Application_Window.
   --  Names are of the form "win.save" or "app.quit" for actions on the
   --  containing Gtk.Application_Window.Gtk_Application_Window or its
   --  associated Gtk.Application.Gtk_Application, respectively. This is the
   --  same form used for actions in the Glib.Menu.Gmenu associated with the
   --  window.
   --  Since: gtk+ 3.4
   --  "action_name": an action name, or null

   function Get_Action_Target_Value
      (Self : Gtk_Actionable) return Glib.Variant.Gvariant;
   --  Gets the current target value of Actionabe.
   --  See Gtk.Actionable.Set_Action_Target_Value for more information.
   --  Since: gtk+ 3.4

   procedure Set_Action_Target_Value
      (Self         : Gtk_Actionable;
       Target_Value : Glib.Variant.Gvariant);
   --  Sets the target value of an actionable widget.
   --  If Target_Value is null then the target value is unset.
   --  The target value has two purposes. First, it is used as the parameter
   --  to activation of the action associated with the
   --  Gtk.Actionable.Gtk_Actionable widget. Second, it is used to determine if
   --  the widget should be rendered as "active" - the widget is active if the
   --  state is equal to the given target.
   --  Consider the example of associating a set of buttons with a
   --  Glib.Action.Gaction with string state in a typical "radio button"
   --  situation. Each button will be associated with the same action, but with
   --  a different target value for that action. Clicking on a particular
   --  button will activate the action with the target of that button, which
   --  will typically cause the action's state to change to that value. Since
   --  the action's state is now equal to the target value of the button, the
   --  button will now be rendered as active (and the other buttons, with
   --  different targets, rendered inactive).
   --  Since: gtk+ 3.4
   --  "target_value": a Glib.Variant.Gvariant to set as the target value, or
   --  null

   procedure Set_Detailed_Action_Name
      (Self                 : Gtk_Actionable;
       Detailed_Action_Name : UTF8_String);
   --  Sets the action-name and associated string target value of an
   --  actionable widget.
   --  This allows for the effect of both Gtk.Actionable.Set_Action_Name and
   --  Gtk.Actionable.Set_Action_Target_Value in the common case that the
   --  target is string-valued.
   --  Detailed_Action_Name is a string of the form '"action::target"' where
   --  'action' is the action name and 'target' is the string to use as the
   --  target.
   --  Since: gtk+ 3.4
   --  "detailed_action_name": the detailed action name

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Action_Name_Property : constant Glib.Properties.Property_String;

   Action_Target_Property : constant Glib.Properties.Property_Object;
   --  Type: Glib.Variant.Gvariant

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Actionable"

   function "+" (W : Gtk_Actionable) return Gtk_Actionable;
   pragma Inline ("+");

private
   Action_Target_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("action-target");
   Action_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("action-name");

Null_Gtk_Actionable : constant Gtk_Actionable :=
   Gtk_Actionable (Glib.Types.Null_Interface);
end Gtk.Actionable;
