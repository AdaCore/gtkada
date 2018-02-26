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

with Gtkada.Types; use Gtkada.Types;
with Ada.Unchecked_Conversion;

package body Glib.Error is

   type GError_Struct is record
      Domain  : GQuark;
      Code    : Gint;
      Message : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, GError_Struct);
   --  The underlying C struct as defined in gerror.h

   type GError_Struct_Access is access all GError_Struct;
   pragma Convention (C, GError_Struct_Access);

   pragma Warnings (Off);
   --  Kill non relevant warnings about To_Gerror

   function To_Gerror is new Ada.Unchecked_Conversion
     (GError, GError_Struct_Access);

   ---------------
   -- Error_New --
   ---------------

   function Error_New
     (Domain : GQuark; Code : Gint; Message : String) return GError
   is
      function Internal
        (Domain : GQuark; Code : Gint; Message : String) return GError;
      pragma Import (C, Internal, "g_error_new_literal");

   begin
      return Internal (Domain, Code, Message & ASCII.NUL);
   end Error_New;

   -------------------
   -- Error_Matches --
   -------------------

   function Error_Matches
     (Error : GError; Domain : GQuark; Code : Gint) return Boolean
   is
      function Internal
        (Error : GError; Domain : GQuark; Code : Gint) return Gboolean;
      pragma Import (C, Internal, "g_error_matches");

   begin
      return Internal (Error, Domain, Code) /= 0;
   end Error_Matches;

   ----------------
   -- Get_Domain --
   ----------------

   function Get_Domain (Error : GError) return GQuark is
   begin
      return To_Gerror (Error).Domain;
   end Get_Domain;

   --------------
   -- Get_Code --
   --------------

   function Get_Code (Error : GError) return Gint is
   begin
      return To_Gerror (Error).Code;
   end Get_Code;

   -----------------
   -- Get_Message --
   -----------------

   function Get_Message (Error : GError) return String is
   begin
      return Value (To_Gerror (Error).Message);
   end Get_Message;

end Glib.Error;
