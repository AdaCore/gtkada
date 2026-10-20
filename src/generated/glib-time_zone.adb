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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Glib.Time_Zone is

   function From_Object_Free
     (B : access Gtime_Zone'Class) return Gtime_Zone
   is
      Result : constant Gtime_Zone := Gtime_Zone (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtime_Zone is
      S : Gtime_Zone;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   -----------
   -- G_New --
   -----------

   procedure G_New (Self : out Gtime_Zone; Identifier : UTF8_String := "") is
      function Internal
         (Identifier : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_time_zone_new");
      Tmp_Identifier : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      Tmp_Identifier :=
        (if Identifier = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Identifier));
      Tmp_Return := Internal (Tmp_Identifier);
      Self.Set_Object (Tmp_Return);
      Free (Tmp_Identifier);
   end G_New;

   ----------------------
   -- G_New_Identifier --
   ----------------------

   procedure G_New_Identifier
      (Self       : out Gtime_Zone;
       Identifier : UTF8_String := "")
   is
      function Internal
         (Identifier : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_identifier");
      Tmp_Identifier : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      Tmp_Identifier :=
        (if Identifier = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Identifier));
      Tmp_Return := Internal (Tmp_Identifier);
      Self.Set_Object (Tmp_Return);
      Free (Tmp_Identifier);
   end G_New_Identifier;

   -----------------
   -- G_New_Local --
   -----------------

   procedure G_New_Local (Self : out Gtime_Zone) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_local");
   begin
      Self.Set_Object (Internal);
   end G_New_Local;

   ------------------
   -- G_New_Offset --
   ------------------

   procedure G_New_Offset (Self : out Gtime_Zone; Seconds : Gint32) is
      function Internal (Seconds : Gint32) return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_offset");
   begin
      Self.Set_Object (Internal (Seconds));
   end G_New_Offset;

   ---------------
   -- G_New_Utc --
   ---------------

   procedure G_New_Utc (Self : out Gtime_Zone) is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_utc");
   begin
      Self.Set_Object (Internal);
   end G_New_Utc;

   --------------------
   -- Gtime_Zone_New --
   --------------------

   function Gtime_Zone_New
      (Identifier : UTF8_String := "") return Gtime_Zone
   is
      function Internal
         (Identifier : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_time_zone_new");
      Self           : Gtime_Zone;
      Tmp_Identifier : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      Tmp_Identifier :=
        (if Identifier = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Identifier));
      Tmp_Return := Internal (Tmp_Identifier);
      Self.Set_Object (Tmp_Return);
      Free (Tmp_Identifier);
      return Self;
   end Gtime_Zone_New;

   -------------------------------
   -- Gtime_Zone_New_Identifier --
   -------------------------------

   function Gtime_Zone_New_Identifier
      (Identifier : UTF8_String := "") return Gtime_Zone
   is
      function Internal
         (Identifier : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_identifier");
      Self           : Gtime_Zone;
      Tmp_Identifier : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      Tmp_Identifier :=
        (if Identifier = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Identifier));
      Tmp_Return := Internal (Tmp_Identifier);
      Self.Set_Object (Tmp_Return);
      Free (Tmp_Identifier);
      return Self;
   end Gtime_Zone_New_Identifier;

   --------------------------
   -- Gtime_Zone_New_Local --
   --------------------------

   function Gtime_Zone_New_Local return Gtime_Zone is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_local");
      Self : Gtime_Zone;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gtime_Zone_New_Local;

   ---------------------------
   -- Gtime_Zone_New_Offset --
   ---------------------------

   function Gtime_Zone_New_Offset (Seconds : Gint32) return Gtime_Zone is
      function Internal (Seconds : Gint32) return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_offset");
      Self : Gtime_Zone;
   begin
      Self.Set_Object (Internal (Seconds));
      return Self;
   end Gtime_Zone_New_Offset;

   ------------------------
   -- Gtime_Zone_New_Utc --
   ------------------------

   function Gtime_Zone_New_Utc return Gtime_Zone is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_time_zone_new_utc");
      Self : Gtime_Zone;
   begin
      Self.Set_Object (Internal);
      return Self;
   end Gtime_Zone_New_Utc;

   -----------------
   -- Adjust_Time --
   -----------------

   function Adjust_Time
      (Self     : Gtime_Zone;
       The_Type : GTime_Type;
       Time     : in out Gint64) return Glib.Gint
   is
      function Internal
         (Self     : System.Address;
          The_Type : GTime_Type;
          Acc_Time : access Gint64) return Glib.Gint;
      pragma Import (C, Internal, "g_time_zone_adjust_time");
      Acc_Time   : aliased Gint64 := Time;
      Tmp_Return : Glib.Gint;
   begin
      Tmp_Return := Internal (Get_Object (Self), The_Type, Acc_Time'Access);
      Time := Acc_Time;
      return Tmp_Return;
   end Adjust_Time;

   -------------------
   -- Find_Interval --
   -------------------

   function Find_Interval
      (Self     : Gtime_Zone;
       The_Type : GTime_Type;
       Time     : Gint64) return Glib.Gint
   is
      function Internal
         (Self     : System.Address;
          The_Type : GTime_Type;
          Time     : Gint64) return Glib.Gint;
      pragma Import (C, Internal, "g_time_zone_find_interval");
   begin
      return Internal (Get_Object (Self), The_Type, Time);
   end Find_Interval;

   ----------------------
   -- Get_Abbreviation --
   ----------------------

   function Get_Abbreviation
      (Self     : Gtime_Zone;
       Interval : Glib.Gint) return UTF8_String
   is
      function Internal
         (Self     : System.Address;
          Interval : Glib.Gint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_time_zone_get_abbreviation");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self), Interval));
   end Get_Abbreviation;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier (Self : Gtime_Zone) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "g_time_zone_get_identifier");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Identifier;

   ----------------
   -- Get_Offset --
   ----------------

   function Get_Offset
      (Self     : Gtime_Zone;
       Interval : Glib.Gint) return Gint32
   is
      function Internal
         (Self     : System.Address;
          Interval : Glib.Gint) return Gint32;
      pragma Import (C, Internal, "g_time_zone_get_offset");
   begin
      return Internal (Get_Object (Self), Interval);
   end Get_Offset;

   ------------
   -- Is_Dst --
   ------------

   function Is_Dst (Self : Gtime_Zone; Interval : Glib.Gint) return Boolean is
      function Internal
         (Self     : System.Address;
          Interval : Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "g_time_zone_is_dst");
   begin
      return Internal (Get_Object (Self), Interval) /= 0;
   end Is_Dst;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gtime_Zone) return Gtime_Zone is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "g_time_zone_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gtime_Zone) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "g_time_zone_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

end Glib.Time_Zone;
