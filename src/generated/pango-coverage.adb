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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Pango.Coverage is

   package Type_Conversion_Pango_Coverage is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Pango_Coverage_Record);
   pragma Unreferenced (Type_Conversion_Pango_Coverage);

   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Self : out Pango_Coverage) is
   begin
      Self := new Pango_Coverage_Record;
      Pango.Coverage.Initialize (Self);
   end Gdk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Pango_Coverage_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "pango_coverage_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ------------------------
   -- Pango_Coverage_New --
   ------------------------

   function Pango_Coverage_New return Pango_Coverage is
      Self : constant Pango_Coverage := new Pango_Coverage_Record;
   begin
      Pango.Coverage.Initialize (Self);
      return Self;
   end Pango_Coverage_New;

   ----------
   -- Copy --
   ----------

   function Copy
      (Self : not null access Pango_Coverage_Record) return Pango_Coverage
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_coverage_copy");
      Stub_Pango_Coverage : Pango_Coverage_Record;
   begin
      return Pango.Coverage.Pango_Coverage (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Coverage));
   end Copy;

   ---------
   -- Get --
   ---------

   function Get
      (Self  : not null access Pango_Coverage_Record;
       Index : Glib.Gint) return Pango.Enums.Coverage_Level
   is
      function Internal
         (Self  : System.Address;
          Index : Glib.Gint) return Pango.Enums.Coverage_Level;
      pragma Import (C, Internal, "pango_coverage_get");
   begin
      return Internal (Get_Object (Self), Index);
   end Get;

   ---------
   -- Max --
   ---------

   procedure Max
      (Self  : not null access Pango_Coverage_Record;
       Other : not null access Pango_Coverage_Record'Class)
   is
      procedure Internal (Self : System.Address; Other : System.Address);
      pragma Import (C, Internal, "pango_coverage_max");
   begin
      Internal (Get_Object (Self), Get_Object (Other));
   end Max;

   ---------
   -- Ref --
   ---------

   function Ref
      (Self : not null access Pango_Coverage_Record) return Pango_Coverage
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "pango_coverage_ref");
      Stub_Pango_Coverage : Pango_Coverage_Record;
   begin
      return Pango.Coverage.Pango_Coverage (Get_User_Data (Internal (Get_Object (Self)), Stub_Pango_Coverage));
   end Ref;

   ---------
   -- Set --
   ---------

   procedure Set
      (Self  : not null access Pango_Coverage_Record;
       Index : Glib.Gint;
       Level : Pango.Enums.Coverage_Level)
   is
      procedure Internal
         (Self  : System.Address;
          Index : Glib.Gint;
          Level : Pango.Enums.Coverage_Level);
      pragma Import (C, Internal, "pango_coverage_set");
   begin
      Internal (Get_Object (Self), Index, Level);
   end Set;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : not null access Pango_Coverage_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "pango_coverage_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Pango.Coverage;
