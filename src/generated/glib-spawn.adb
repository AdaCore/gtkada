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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
pragma Warnings(On);

package body Glib.Spawn is

   ------------------------------
   -- Spawn_Command_Line_Async --
   ------------------------------

   function Spawn_Command_Line_Async
     (Command_Line : Gtkada.Types.Chars_Ptr;
      Error        : access Glib.Error.GError)
   return Boolean
   is
      function Internal
        (Command_Line : Gtkada.Types.Chars_Ptr;
         Error        : access Glib.Error.GError)
      return Glib.Gboolean;
      pragma Import (C, Internal, "gnat_spawn_command_line_async");
   begin
      return Internal (Command_Line, Error) /= 0;
   end Spawn_Command_Line_Async;

   -----------------------------
   -- Spawn_Check_Exit_Status --
   -----------------------------

   function Spawn_Check_Exit_Status
     (Exit_Status : Glib.Gint;
      Error       : access Glib.Error.GError)
   return Boolean
   is
      function Internal
        (Exit_Status : Glib.Gint;
         Error       : access Glib.Error.GError)
      return Glib.Gboolean;
      pragma Import (C, Internal, "g_spawn_check_exit_status");
   begin
      return Internal (Exit_Status, Error) /= 0;
   end Spawn_Check_Exit_Status;

   -----------------
   -- Get_Environ --
   -----------------

   function Get_Environ return GNAT.Strings.String_List is
      function Internal return chars_ptr_array_access;
      pragma Import (C, Internal, "g_get_environ");
   begin
      return To_String_List_And_Free (Internal);
   end Get_Environ;

   ---------------------
   -- Spawn_Close_Pid --
   ---------------------

   procedure Spawn_Close_Pid (Pid : GPid) is
      procedure Internal (Pid : GPid);
      pragma Import (C, Internal, "g_spawn_close_pid");
   begin
      Internal (Pid);
   end Spawn_Close_Pid;

end Glib.Spawn;
