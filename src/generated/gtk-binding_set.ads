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
--  A binding set maintains a list of activatable key bindings. A single
--  binding set can match multiple types of widgets. Similar to style contexts,
--  can be matched by any information contained in a widgets
--  Gtk.Widget.Gtk_Widget_Path. When a binding within a set is matched upon
--  activation, an action signal is emitted on the target widget to carry out
--  the actual activation.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;    use Gdk.Types;
with Glib;         use Glib;
with Glib.Object;  use Glib.Object;
with Gtk.Enums;    use Gtk.Enums;
with Gtkada.Types; use Gtkada.Types;

package Gtk.Binding_Set is

   type Gtk_Binding_Set is record
      Set_Name : Gtkada.Types.Chars_Ptr;
      Priority : Glib.Gint := 0;
      Widget_Path_Pspecs : System.Address := System.Null_Address;
      Widget_Class_Pspecs : System.Address := System.Null_Address;
      Class_Branch_Pspecs : System.Address := System.Null_Address;
      Entries : System.Address;
      Current : System.Address := System.Null_Address;
      Parsed : Guint;
   end record;
   pragma Convention (C, Gtk_Binding_Set);

   function From_Object_Free (B : access Gtk_Binding_Set) return Gtk_Binding_Set;
   pragma Inline (From_Object_Free);
   --  A binding set maintains a list of activatable key bindings. A single
   --  binding set can match multiple types of widgets. Similar to style
   --  contexts, can be matched by any information contained in a widgets
   --  Gtk.Widget.Gtk_Widget_Path. When a binding within a set is matched upon
   --  activation, an action signal is emitted on the target widget to carry
   --  out the actual activation.

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Binding_Set; Set_Name : UTF8_String);
   --  GTK+ maintains a global list of binding sets. Each binding set has a
   --  unique name which needs to be specified upon creation.
   --  "set_name": unique name of this binding set

   function Gtk_Binding_Set_New
      (Set_Name : UTF8_String) return Gtk_Binding_Set;
   --  GTK+ maintains a global list of binding sets. Each binding set has a
   --  unique name which needs to be specified upon creation.
   --  "set_name": unique name of this binding set

   -------------
   -- Methods --
   -------------

   function Activate
      (Self      : Gtk_Binding_Set;
       Keyval    : Guint;
       Modifiers : Gdk.Types.Gdk_Modifier_Type;
       Object    : not null access Glib.Object.GObject_Record'Class)
       return Boolean;
   --  Find a key binding matching Keyval and Modifiers within Binding_Set and
   --  activate the binding on Object.
   --  "keyval": key value of the binding
   --  "modifiers": key modifier of the binding
   --  "object": object to activate when binding found

   procedure Add_Path
      (Self         : Gtk_Binding_Set;
       Path_Type    : Gtk.Enums.Gtk_Path_Type;
       Path_Pattern : UTF8_String;
       Priority     : Gtk.Enums.Gtk_Path_Priority_Type);
   pragma Obsolescent (Add_Path);
   --  This function was used internally by the GtkRC parsing mechanism to
   --  assign match patterns to Gtk.Binding_Set.Gtk_Binding_Set structures.
   --  In GTK+ 3, these match patterns are unused.
   --  Deprecated since 3.0, 1
   --  "path_type": path type the pattern applies to
   --  "path_pattern": the actual match pattern
   --  "priority": binding priority

   ---------------
   -- Functions --
   ---------------

   function By_Class (Object_Class : System.Address) return Gtk_Binding_Set;
   pragma Import (C, By_Class, "gtk_binding_set_by_class");
   --  This function returns the binding set named after the type name of the
   --  passed in class structure. New binding sets are created on demand by
   --  this function.
   --  "object_class": a valid Glib.Object.GObject class

   function Find (Set_Name : UTF8_String) return Gtk_Binding_Set;
   --  Find a binding set by its globally unique name.
   --  The Set_Name can either be a name used for Gtk.Binding_Set.Gtk_New or
   --  the type name of a class used in Gtk.Binding_Set.By_Class.
   --  "set_name": unique binding set name

end Gtk.Binding_Set;
