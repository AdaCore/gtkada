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

package body Glib.Bytes is

   function From_Object_Free
     (B : access Gbytes'Class) return Gbytes
   is
      Result : constant Gbytes := Gbytes (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gbytes is
      S : Gbytes;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gbytes; Data : Gint_Array; Size : Gsize) is
      function Internal
         (Data : System.Address;
          Size : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new");
   begin
      Self.Set_Object (Internal (Data (Data'First)'Address, Size));
   end G_New;

   ------------------
   -- G_New_Static --
   ------------------

   procedure G_New_Static
      (Self : out Gbytes;
       Data : Gint_Array;
       Size : Gsize)
   is
      function Internal
         (Data : System.Address;
          Size : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new_static");
   begin
      Self.Set_Object (Internal (Data (Data'First)'Address, Size));
   end G_New_Static;

   ----------------
   -- G_New_Take --
   ----------------

   procedure G_New_Take (Self : out Gbytes; Data : Gint_Array; Size : Gsize) is
      function Internal
         (Data : System.Address;
          Size : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new_take");
   begin
      Self.Set_Object (Internal (Data (Data'First)'Address, Size));
   end G_New_Take;

   ----------------
   -- Gbytes_New --
   ----------------

   function Gbytes_New (Data : Gint_Array; Size : Gsize) return Gbytes is
      function Internal
         (Data : System.Address;
          Size : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new");
      Self : Gbytes;
   begin
      Self.Set_Object (Internal (Data (Data'First)'Address, Size));
      return Self;
   end Gbytes_New;

   -----------------------
   -- Gbytes_New_Static --
   -----------------------

   function Gbytes_New_Static
      (Data : Gint_Array;
       Size : Gsize) return Gbytes
   is
      function Internal
         (Data : System.Address;
          Size : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new_static");
      Self : Gbytes;
   begin
      Self.Set_Object (Internal (Data (Data'First)'Address, Size));
      return Self;
   end Gbytes_New_Static;

   ---------------------
   -- Gbytes_New_Take --
   ---------------------

   function Gbytes_New_Take (Data : Gint_Array; Size : Gsize) return Gbytes is
      function Internal
         (Data : System.Address;
          Size : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new_take");
      Self : Gbytes;
   begin
      Self.Set_Object (Internal (Data (Data'First)'Address, Size));
      return Self;
   end Gbytes_New_Take;

   --------------
   -- Get_Size --
   --------------

   function Get_Size (Self : Gbytes) return Gsize is
      function Internal (Self : System.Address) return Gsize;
      pragma Import (C, Internal, "g_bytes_get_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Size;

   --------------------
   -- New_From_Bytes --
   --------------------

   function New_From_Bytes
      (Self   : Gbytes;
       Offset : Gsize;
       Length : Gsize) return Gbytes
   is
      function Internal
         (Self   : System.Address;
          Offset : Gsize;
          Length : Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_new_from_bytes");
   begin
      return From_Object (Internal (Get_Object (Self), Offset, Length));
   end New_From_Bytes;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gbytes) return Gbytes is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_bytes_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gbytes) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_bytes_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   -------------------
   -- Unref_To_Data --
   -------------------

   function Unref_To_Data
      (Self : Gbytes;
       Size : in out Gsize) return System.Address
   is
      function Internal
         (Self     : System.Address;
          Acc_Size : access Gsize) return System.Address;
      pragma Import (C, Internal, "g_bytes_unref_to_data");
      Acc_Size   : aliased Gsize := Size;
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Size'Access);
      Size := Acc_Size;
      return Tmp_Return;
   end Unref_To_Data;

end Glib.Bytes;
