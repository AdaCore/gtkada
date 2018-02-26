------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Gtkada.Types; use Gtkada.Types;

package body Glib.Messages is

   procedure C_Log_Func
     (Log_Domain : Chars_Ptr;
      Log_Level  : Log_Level_Flags;
      Message    : Chars_Ptr;
      Ada_Func   : Log_Function);
   --  Low level log wrapper
   pragma Convention (C, C_Log_Func);

   ----------------
   -- C_Log_Func --
   ----------------

   procedure C_Log_Func
     (Log_Domain : Chars_Ptr;
      Log_Level  : Log_Level_Flags;
      Message    : Chars_Ptr;
      Ada_Func   : Log_Function) is
   begin
      if Log_Domain = Null_Ptr then
         Ada_Func ("", Log_Level, Value (Message));
      else
         Ada_Func (Value (Log_Domain), Log_Level, Value (Message));
      end if;
   end C_Log_Func;

   ---------------------
   -- Log_Set_Handler --
   ---------------------

   function Log_Set_Handler
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Log_Func   : Log_Function) return Log_Handler_Id
   is
      function Internal
        (Log_Domain : String;
         Log_Levels : Log_Level_Flags;
         Log_Func   : System.Address;
         User_Data  : System.Address) return Log_Handler_Id;
      pragma Import (C, Internal, "g_log_set_handler");

   begin
      return Internal
        (Log_Domain & ASCII.NUL, Log_Levels, C_Log_Func'Address,
         Log_Func.all'Address);
   end Log_Set_Handler;

   ------------------------
   -- Log_Remove_Handler --
   ------------------------

   procedure Log_Remove_Handler
     (Log_Domain : String;
      Handler_Id : Log_Handler_Id)
   is
      procedure Internal
        (Log_Domain : String;
         Handler_Id : Log_Handler_Id);
      pragma Import (C, Internal, "g_log_remove_handler");

   begin
      Internal (Log_Domain & ASCII.NUL, Handler_Id);
   end Log_Remove_Handler;

   -------------------------
   -- Log_Default_Handler --
   -------------------------

   procedure Log_Default_Handler
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Message    : UTF8_String)
   is
      procedure Internal
        (Log_Domain : String;
         Log_Levels : Log_Level_Flags;
         Message    : UTF8_String);
      pragma Import (C, Internal, "g_log_default_handler");

   begin
      Internal (Log_Domain & ASCII.NUL, Log_Levels, Message & ASCII.NUL);
   end Log_Default_Handler;

   ---------
   -- Log --
   ---------

   procedure Log
     (Log_Domain : String;
      Log_Levels : Log_Level_Flags;
      Message    : UTF8_String)
   is
      procedure Internal
        (Log_Domain : String;
         Log_Levels : Log_Level_Flags;
         Message     : UTF8_String);
      pragma Import (C, Internal, "ada_g_log");

   begin
      Internal (Log_Domain & ASCII.NUL, Log_Levels, Message & ASCII.NUL);
   end Log;

   ------------------------
   -- Log_Set_Fatal_Mask --
   ------------------------

   function Log_Set_Fatal_Mask
     (Log_Domain : String;
      Fatal_Mask : Log_Level_Flags) return Log_Level_Flags
   is
      function Internal
        (Log_Domain : String;
         Fatal_Mask : Log_Level_Flags) return Log_Level_Flags;
      pragma Import (C, Internal, "g_log_set_fatal_mask");

   begin
      return Internal (Log_Domain & ASCII.NUL, Fatal_Mask);
   end Log_Set_Fatal_Mask;

end Glib.Messages;
