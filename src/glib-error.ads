------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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
--
--  This package provides definitions for the error handling mechanism used in
--  Glib, Gdk and Gtk.
--
--  </description>
--  <c_version>1.3.11</c_version>
--  <group>Glib, the general-purpose library</group>

package Glib.Error is
   pragma Preelaborate;

   type GError is new C_Proxy;

   type GError_Access is access all GError;

   function Error_New
     (Domain : GQuark; Code : Gint; Message : String) return GError;
   --  Create a new GError object.

   procedure Error_Free (Error : GError);
   --  Free the memory associated with a GError.

   function Error_Copy (Error : GError) return GError;
   --  Duplicate a GError object.

   function Error_Matches
     (Error : GError; Domain : GQuark; Code : Gint) return Boolean;
   --  Return whether a given GError matches a domain/code.

   function Get_Domain (Error : GError) return GQuark;
   --  Return the domain associated with a GError.

   function Get_Code (Error : GError) return Gint;
   --  Return the code associated with a GError.

   function Get_Message (Error : GError) return String;
   --  Return the message associated with a GError.

private

   pragma Import (C, Error_Free, "g_error_free");
   pragma Import (C, Error_Copy, "g_error_copy");

end Glib.Error;
