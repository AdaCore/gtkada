------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

private with Ada.Finalization;
with System;

with Glib;

package Gdk.Event is

	type Gdk_Event is tagged private;
	--  Opaque reference-counted wrapper around GdkEvent*.

	function Get_Type return Glib.GType;
	pragma Import (C, Get_Type, "gdk_event_get_type");

	function From_Object (Object : System.Address) return Gdk_Event;
	--  Create an Ada handle from a raw C pointer.
	--  Always acquires a reference with gdk_event_ref.

	function Get_Object (Self : Gdk_Event'Class) return System.Address;
	pragma Inline (Get_Object);

	procedure Set_Object
	  (Self   : in out Gdk_Event'Class;
		Object : System.Address);
	--  Assign a raw C pointer to Self.
	--  Releases any previous pointer in Self.
	--  Always acquires a reference on Object.

	function Is_Null (Self : Gdk_Event'Class) return Boolean;
	pragma Inline (Is_Null);

private

	type Gdk_Event is new Ada.Finalization.Controlled with record
		Data : System.Address := System.Null_Address;
	end record;

	overriding procedure Adjust (Self : in out Gdk_Event);
	overriding procedure Finalize (Self : in out Gdk_Event);

end Gdk.Event;
