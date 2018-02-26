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

with Gtkada.Types; use Gtkada.Types;

package body Glib.Module is

   ----------------------
   -- Module_Supported --
   ----------------------

   function Module_Supported return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "ada_g_module_supported");

   begin
      return Internal /= 0;
   end Module_Supported;

   -----------------
   -- Module_Open --
   -----------------

   function Module_Open
     (File_Name : String;
      Flags     : Module_Flags := Module_Bind_Lazy) return G_Module
   is
      function Internal
        (File_Name : String;
         Flags     : Module_Flags) return G_Module;
      pragma Import (C, Internal, "ada_g_module_open");

   begin
      return Internal (File_Name & ASCII.NUL, Flags);
   end Module_Open;

   ------------------
   -- Module_Close --
   ------------------

   function Module_Close (Module : G_Module) return Boolean is
      function Internal (Module : G_Module) return Gint;
      pragma Import (C, Internal, "ada_g_module_close");

   begin
      return Internal (Module) /= 0;
   end Module_Close;

   ------------------
   -- Module_Error --
   ------------------

   function Module_Error return String is
      function Internal return Chars_Ptr;
      pragma Import (C, Internal, "ada_g_module_error");

   begin
      return Value (Internal);
   end Module_Error;

   ---------------------------
   -- Generic_Module_Symbol --
   ---------------------------

   procedure Generic_Module_Symbol
     (Module      : G_Module;
      Symbol_Name : String;
      Symbol      : out Pointer;
      Success     : out Boolean)
   is
      function Internal
        (Module      : G_Module;
         Symbol_Name : String;
         Symbol      : System.Address) return Gint;
      pragma Import (C, Internal, "ada_g_module_symbol");

      Tmp : aliased Pointer;

   begin
      Success := Internal (Module, Symbol_Name & ASCII.NUL, Tmp'Address) /= 0;
      Symbol  := Tmp;
   end Generic_Module_Symbol;

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name (Module : G_Module) return String is
      function Internal (Module : G_Module) return Chars_Ptr;
      pragma Import (C, Internal, "ada_g_module_name");

   begin
      return Value (Internal (Module));
   end Module_Name;

   -----------------------
   -- Module_Build_Path --
   -----------------------

   function Module_Build_Path
     (Directory   : String;
      Module_Name : String) return String
   is
      function Internal
        (Directory : String; Module_Name : String) return Chars_Ptr;
      pragma Import (C, Internal, "ada_g_module_build_path");

   begin
      return Value (Internal (Directory & ASCII.NUL, Module_Name & ASCII.NUL));
   end Module_Build_Path;

end Glib.Module;
