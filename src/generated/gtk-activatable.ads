------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  Activatable widgets can be connected to a Gtk_Action and reflects the
--  state of its action. A Gtk_Activatable can also provide feedback through
--  its action, as they are responsible for activating their related actions.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Action;      use Gtk.Action;

package Gtk.Activatable is

   type Gtk_Activatable is new Glib.Types.GType_Interface;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_activatable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Do_Set_Related_Action
      (Self   : Gtk_Activatable;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   --  This is a utility function for Gtk.Activatable.Gtk_Activatable
   --  implementors.
   --  When implementing Gtk.Activatable.Gtk_Activatable you must call this
   --  when handling changes of the
   --  Gtk.Activatable.Gtk_Activatable:related-action, and you must also use
   --  this to break references in GObject->dispose.
   --  This function adds a reference to the currently set related action for
   --  you, it also makes sure the Gtk.Activatable.Gtk_Activatable->update
   --  method is called when the related Gtk.Action.Gtk_Action properties
   --  change and registers to the action's proxy list.
   --   Note: Be careful to call this before setting the local copy of the
   --  Gtk.Action.Gtk_Action property, since this function uses
   --  Gtk.Activatable.Get_Action to retrieve the previous action
   --  Since: gtk+ 2.16
   --  "action": the Gtk.Action.Gtk_Action to set

   function Get_Related_Action
      (Self : Gtk_Activatable) return Gtk.Action.Gtk_Action;
   procedure Set_Related_Action
      (Self   : Gtk_Activatable;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   --  Sets the related action on the Activatable object.
   --   Note: Gtk.Activatable.Gtk_Activatable implementors need to handle the
   --  Gtk.Activatable.Gtk_Activatable:related-action property and call
   --  Gtk.Toggle_Button.Do_Set_Related_Action when it changes.
   --  Since: gtk+ 2.16
   --  "action": the Gtk.Action.Gtk_Action to set

   function Get_Use_Action_Appearance
      (Self : Gtk_Activatable) return Boolean;
   procedure Set_Use_Action_Appearance
      (Self           : Gtk_Activatable;
       Use_Appearance : Boolean);
   --  Sets whether this activatable should reset its layout and appearance
   --  when setting the related action or when the action changes appearance
   --   Note: Gtk.Activatable.Gtk_Activatable implementors need to handle the
   --  Gtk.Activatable.Gtk_Activatable:use-action-appearance property and call
   --  Gtk.Toggle_Button.Sync_Action_Properties to update Activatable if
   --  needed.
   --  Since: gtk+ 2.16
   --  "use_appearance": whether to use the actions appearance

   procedure Sync_Action_Properties
      (Self   : Gtk_Activatable;
       Action : access Gtk.Action.Gtk_Action_Record'Class);
   --  This is called to update the activatable completely, this is called
   --  internally when the Gtk.Activatable.Gtk_Activatable::related-action
   --  property is set or unset and by the implementing class when
   --  Gtk.Activatable.Gtk_Activatable::use-action-appearance changes.
   --  Since: gtk+ 2.16
   --  "action": the related Gtk.Action.Gtk_Action or null

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Related_Action_Property
   --  Type: Gtk.Action.Gtk_Action
   --  Flags: read-write
   --  The action that this activatable will activate and receive updates from
   --  for various states and possibly appearance.
   --   Note: Gtk.Activatable.Gtk_Activatable implementors need to handle the
   --  this property and call Gtk.Activatable.Do_Set_Related_Action when it
   --  changes.
   --
   --  Name: Use_Action_Appearance_Property
   --  Type: Boolean
   --  Flags: read-write
   --  Whether this activatable should reset its layout and appearance when
   --  setting the related action or when the action changes appearance.
   --  See the Gtk.Action.Gtk_Action documentation directly to find which
   --  properties should be ignored by the Gtk.Activatable.Gtk_Activatable when
   --  this property is False.
   --   Note: Gtk.Activatable.Gtk_Activatable implementors need to handle this
   --  property and call Gtk.Activatable.Sync_Action_Properties on the
   --  activatable widget when it changes.

   Related_Action_Property : constant Glib.Properties.Property_Object;
   Use_Action_Appearance_Property : constant Glib.Properties.Property_Boolean;

private
   Related_Action_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("related-action");
   Use_Action_Appearance_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-action-appearance");
end Gtk.Activatable;
