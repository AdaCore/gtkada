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

--  Communicates with platform-specific assistive technologies API.
--
--  Each platform supported by GTK implements a `GtkATContext` subclass, and
--  is responsible for updating the accessible state in response to state
--  changes in `GtkAccessible`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;

package Gtk.Atcontext is

   type Gtk_Atcontext_Record is new GObject_Record with null record;
   type Gtk_Atcontext is access all Gtk_Atcontext_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_at_context_get_type");

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Atcontext_Void is not null access procedure (Self : access Gtk_Atcontext_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_State_Change : constant Glib.Signal_Name := "state-change";
   procedure On_State_Change
      (Self  : not null access Gtk_Atcontext_Record;
       Call  : Cb_Gtk_Atcontext_Void;
       After : Boolean := False);
   procedure On_State_Change
      (Self  : not null access Gtk_Atcontext_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the attributes of the accessible for the `GtkATContext`
   --  instance change.

end Gtk.Atcontext;
