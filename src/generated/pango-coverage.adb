------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

package body Pango.Coverage is

   function From_Object_Free
     (B : access Pango_Coverage'Class) return Pango_Coverage
   is
      Result : constant Pango_Coverage := Pango_Coverage (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Pango_Coverage is
      S : Pango_Coverage;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   ----------
   -- Copy --
   ----------

   function Copy (Self : Pango_Coverage) return Pango_Coverage is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_coverage_copy");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Copy;

   ---------
   -- Get --
   ---------

   function Get
      (Self  : Pango_Coverage;
       Index : Gint) return Pango.Enums.Coverage_Level
   is
      function Internal
         (Self  : System.Address;
          Index : Gint) return Pango.Enums.Coverage_Level;
      pragma Import (C, Internal, "pango_coverage_get");
   begin
      return Internal (Get_Object (Self), Index);
   end Get;

   ---------
   -- Max --
   ---------

   procedure Max (Self : Pango_Coverage; Other : Pango_Coverage) is
      procedure Internal (Self : System.Address; Other : System.Address);
      pragma Import (C, Internal, "pango_coverage_max");
   begin
      Internal (Get_Object (Self), Get_Object (Other));
   end Max;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Pango_Coverage) return Pango_Coverage is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_coverage_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   ---------
   -- Set --
   ---------

   procedure Set
      (Self  : Pango_Coverage;
       Index : Gint;
       Level : Pango.Enums.Coverage_Level)
   is
      procedure Internal
         (Self  : System.Address;
          Index : Gint;
          Level : Pango.Enums.Coverage_Level);
      pragma Import (C, Internal, "pango_coverage_set");
   begin
      Internal (Get_Object (Self), Index, Level);
   end Set;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Pango_Coverage) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_coverage_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   ---------------
   -- Pango_New --
   ---------------

   function Pango_New return Pango_Coverage is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_coverage_new");
   begin
      return From_Object (Internal);
   end Pango_New;

end Pango.Coverage;
