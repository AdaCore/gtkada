-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings;

package body Gdk.Main is

   -------------------
   --  Get_Display  --
   -------------------

   function Get_Display return String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_get_display");
   begin
      return Interfaces.C.Strings.Value (Internal);
   end Get_Display;


   --------------------
   --  Get_Use_Xshm  --
   --------------------

   function Get_Use_Xshm return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_get_use_xshm");
   begin
      return To_Boolean (Internal);
   end Get_Use_Xshm;


   ------------------
   --  Set_Locale  --
   ------------------

   function Set_Locale return String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_set_locale");
   begin
      return C.Strings.Value (Internal);
   end Set_Locale;


   ------------------
   --  Set_Locale  --
   ------------------

   procedure Set_Locale is
      Dummy : constant String := Set_Locale;
      pragma Warnings (Off, Dummy);
   begin
      null;
   end Set_Locale;


   --------------------
   --  Set_Use_Xshm  --
   --------------------

   procedure Set_Use_Xshm (Use_Xshm : in Boolean := True) is
      procedure Internal (Use_Xshm : in Gint);
      pragma Import (C, Internal, "gdk_set_use_xshm");
   begin
      Internal (To_Gint (Use_Xshm));
   end Set_Use_Xshm;

end Gdk.Main;
