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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Glib.Error_Enums is

   type GFile_Error is (
      G_File_Error_Exist,
      G_File_Error_Isdir,
      G_File_Error_Acces,
      G_File_Error_Nametoolong,
      G_File_Error_Noent,
      G_File_Error_Notdir,
      G_File_Error_Nxio,
      G_File_Error_Nodev,
      G_File_Error_Rofs,
      G_File_Error_Txtbsy,
      G_File_Error_Fault,
      G_File_Error_Loop,
      G_File_Error_Nospc,
      G_File_Error_Nomem,
      G_File_Error_Mfile,
      G_File_Error_Nfile,
      G_File_Error_Badf,
      G_File_Error_Inval,
      G_File_Error_Pipe,
      G_File_Error_Again,
      G_File_Error_Intr,
      G_File_Error_Io,
      G_File_Error_Perm,
      G_File_Error_Nosys,
      G_File_Error_Failed);
   pragma Convention (C, GFile_Error);
   --  Values corresponding to Errno codes returned from file operations on
   --  UNIX. Unlike Errno codes, GFileError values are available on all
   --  systems, even Windows. The exact meaning of each code depends on what
   --  sort of file operation you were performing; the UNIX documentation gives
   --  more details. The following error code descriptions come from the GNU C
   --  Library manual, and are under the copyright of that manual.
   --
   --  It's not very portable to make detailed assumptions about exactly which
   --  errors will be returned from a given operation. Some errors don't occur
   --  on some systems, etc., sometimes there are subtle differences in when a
   --  system will report a given error, etc.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GFile_Error_Properties is
      new Generic_Internal_Discrete_Property (GFile_Error);
   type Property_GFile_Error is new GFile_Error_Properties.Property;

end Glib.Error_Enums;
