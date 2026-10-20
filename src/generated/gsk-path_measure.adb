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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gsk.Path_Measure is

   function From_Object_Free
     (B : access Gsk_Path_Measure'Class) return Gsk_Path_Measure
   is
      Result : constant Gsk_Path_Measure := Gsk_Path_Measure (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsk_Path_Measure is
      S : Gsk_Path_Measure;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   --------------------------
   -- Gsk_Path_Measure_New --
   --------------------------

   function Gsk_Path_Measure_New
      (Path : Gsk.Path.Gsk_Path) return Gsk_Path_Measure
   is
      function Internal (Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_measure_new");
      Self : Gsk_Path_Measure;
   begin
      Self.Set_Object (Internal (Get_Object (Path)));
      return Self;
   end Gsk_Path_Measure_New;

   -----------------------------------------
   -- Gsk_Path_Measure_New_With_Tolerance --
   -----------------------------------------

   function Gsk_Path_Measure_New_With_Tolerance
      (Path      : Gsk.Path.Gsk_Path;
       Tolerance : Interfaces.C.C_float) return Gsk_Path_Measure
   is
      function Internal
         (Path      : System.Address;
          Tolerance : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_path_measure_new_with_tolerance");
      Self : Gsk_Path_Measure;
   begin
      Self.Set_Object (Internal (Get_Object (Path), Tolerance));
      return Self;
   end Gsk_Path_Measure_New_With_Tolerance;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gsk_Path_Measure; Path : Gsk.Path.Gsk_Path) is
      function Internal (Path : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_measure_new");
   begin
      Self.Set_Object (Internal (Get_Object (Path)));
   end Gtk_New;

   ----------------------------
   -- Gtk_New_With_Tolerance --
   ----------------------------

   procedure Gtk_New_With_Tolerance
      (Self      : out Gsk_Path_Measure;
       Path      : Gsk.Path.Gsk_Path;
       Tolerance : Interfaces.C.C_float)
   is
      function Internal
         (Path      : System.Address;
          Tolerance : Interfaces.C.C_float) return System.Address;
      pragma Import (C, Internal, "gsk_path_measure_new_with_tolerance");
   begin
      Self.Set_Object (Internal (Get_Object (Path), Tolerance));
   end Gtk_New_With_Tolerance;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Self : Gsk_Path_Measure) return Interfaces.C.C_float is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_path_measure_get_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Length;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (Self : Gsk_Path_Measure) return Gsk.Path.Gsk_Path is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_measure_get_path");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Path;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point
      (Self     : Gsk_Path_Measure;
       Distance : Interfaces.C.C_float;
       Result   : not null access Gsk.Path_Point.Gsk_Path_Point)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Distance : Interfaces.C.C_float;
          Result   : access Gsk.Path_Point.Gsk_Path_Point)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_measure_get_point");
   begin
      return Internal (Get_Object (Self), Distance, Result) /= 0;
   end Get_Point;

   -------------------
   -- Get_Tolerance --
   -------------------

   function Get_Tolerance
      (Self : Gsk_Path_Measure) return Interfaces.C.C_float
   is
      function Internal (Self : System.Address) return Interfaces.C.C_float;
      pragma Import (C, Internal, "gsk_path_measure_get_tolerance");
   begin
      return Internal (Get_Object (Self));
   end Get_Tolerance;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gsk_Path_Measure) return Gsk_Path_Measure is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_measure_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsk_Path_Measure) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_path_measure_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Gsk.Path_Measure;
