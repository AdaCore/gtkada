------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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
--  This package provides wrapper code for dynamic module loading
--  </description>
--  <group>Glib, the general-purpose library</group>

package Glib.Module is
   pragma Preelaborate;

   type Module_Flags is mod 2 ** 32;
   Module_Bind_Lazy : constant Module_Flags := 2 ** 0;
   Module_Bind_Mask : constant Module_Flags := 16#1#;

   type G_Module is new C_Proxy;

   function Module_Supported return Boolean;
   --  Return True if dynamic module loading is supported

   function Module_Open
     (File_Name : String;
      Flags     : Module_Flags := Module_Bind_Lazy) return G_Module;
   --  Open a module `file_name' and return handle, which is null on error.

   function Module_Close (Module : G_Module) return Boolean;
   --  Close a previously opened module, return True on success.

   procedure Module_Make_Resident (Module : G_Module);
   --  Make a module resident so Module_Close on it will be ignored

   function Module_Error return String;
   --  Query the last module error as a string

   generic
      type Pointer is private;
      --  This is typically a pointer to procedure/function.

   procedure Generic_Module_Symbol
     (Module      : G_Module;
      Symbol_Name : String;
      Symbol      : out Pointer;
      Success     : out Boolean);
   --  Retrieve a symbol pointer from `module'.
   --  Success is set to True on success.

   function Module_Name (Module : G_Module) return String;
   --  Retrieve the file name from an existing module

   function Module_Build_Path
     (Directory   : String;
      Module_Name : String) return String;
   --  Build the actual file name containing a module.
   --  `directory' is the directory where the module file is supposed to be, or
   --  the null string in which case it should either be in the current
   --  directory or, on some operating systems, in some standard place, for
   --  instance on the PATH. Hence, to be absolutely sure to get the correct
   --  module, always pass in a directory. The file name consists of the
   --  directory, if supplied, and `module_name' suitably decorated accoring to
   --  the operating system's conventions (for instance lib*.so or *.dll).
   --
   --  No checks are made that the file exists, or is of correct type.

private
   pragma Import (C, Module_Make_Resident, "g_module_make_resident");
end Glib.Module;
