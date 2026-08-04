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

--  Emits a signal on a widget.
--
--  Signals that are used in this way are referred to as keybinding signals,
--  and they are expected to be defined with the `G_SIGNAL_ACTION` flag.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                use Glib;
with Glib.Properties;     use Glib.Properties;
with Gtk.Shortcut_Action; use Gtk.Shortcut_Action;

package Gtk.Signal_Action is

   type Gtk_Signal_Action_Record is new Gtk_Shortcut_Action_Record with null record;
   type Gtk_Signal_Action is access all Gtk_Signal_Action_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self        : out Gtk_Signal_Action;
       Signal_Name : UTF8_String);
   procedure Initialize
      (Self        : not null access Gtk_Signal_Action_Record'Class;
       Signal_Name : UTF8_String);
   --  Creates an action that when activated, emits the given action signal on
   --  the provided widget.
   --  It will also unpack the args into arguments passed to the signal.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Signal_Name name of the signal to emit

   function Gtk_Signal_Action_New
      (Signal_Name : UTF8_String) return Gtk_Signal_Action;
   --  Creates an action that when activated, emits the given action signal on
   --  the provided widget.
   --  It will also unpack the args into arguments passed to the signal.
   --  @param Signal_Name name of the signal to emit

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_signal_action_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Signal_Name
      (Self : not null access Gtk_Signal_Action_Record) return UTF8_String;
   --  Returns the name of the signal that will be emitted.
   --  @return the name of the signal to emit

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Signal_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the signal to emit.

private
   Signal_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("signal-name");
end Gtk.Signal_Action;
