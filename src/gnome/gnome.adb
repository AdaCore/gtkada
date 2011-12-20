------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
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

with System;

package body Gnome is

   ----------
   -- Init --
   ----------

   function Init (App_Id : String; App_Version : String) return Boolean is
      gnat_argc : Integer;
      pragma Import (C, gnat_argc);

      gnat_argv : System.Address;
      pragma Import (C, gnat_argv);

      function Internal
        (App_Id      : String;
         App_Version : String;
         Argc        : Integer;
         Argv        : System.Address;
         Popt_Option : System.Address;
         Flags       : Integer;
         Context     : System.Address) return Integer;
      pragma Import (C, Internal, "gnome_init_with_popt_table");

   begin
      return Internal
        (App_Id & ASCII.NUL,
         App_Version & ASCII.NUL,
         gnat_argc,
         gnat_argv, System.Null_Address, 0, System.Null_Address) /= 0;
   end Init;

end Gnome;
