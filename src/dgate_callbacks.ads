-----------------------------------------------------------------------
--                       DGATE Components                            --
--                                                                   --
--                      Copyright (C) 1999                           --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- Dynagate is free software;  you can redistribute it and/or modify --
-- it under the terms of the GNU General Public License as published --
-- by the Free Software Foundation; either version 2 of the License, --
-- or (at your option) any later version.                            --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------
 
--  This package provides to DGATE a generic callback
 
with Gtk.Util; use Gtk.Util;
with System;
 
package DGate_Callbacks is
 
   procedure Generic_Callback
     (Object : in Private_Object; Data : in System.Address);
   --  This callback will print on standard output the string pointed to by
   --  Data.

   function Quit (Id : Integer) return Boolean;
   --  This function is used to connect a timeout to DGATE
 
end DGate_Callbacks;
