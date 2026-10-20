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

package body Gsk.Path is

   function From_Object_Free
     (B : access Gsk_Path'Class) return Gsk_Path
   is
      Result : constant Gsk_Path := Gsk_Path (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gsk_Path is
      S : Gsk_Path;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   function Foreach
     (Self : Gsk_Path; Flags : Gsk.Enums.Gsk_Path_Foreach_Flags;
      Func : Gsk_Path_Foreach_Func;
      User_Data : System.Address := System.Null_Address) return Boolean is
      function Internal
        (Self : System.Address; Flags : Gsk.Enums.Gsk_Path_Foreach_Flags;
         Func : Gsk_Path_Foreach_Func; User_Data : System.Address)
      return Gboolean;
      pragma Import (C, Internal, "gsk_path_foreach");
   begin
      return Internal (Get_Object (Self), Flags, Func, User_Data) /= 0;
   end Foreach;

   -----------
   -- Equal --
   -----------

   function Equal (Self : Gsk_Path; Path2 : Gsk_Path) return Boolean is
      function Internal
         (Self  : System.Address;
          Path2 : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Path2)) /= 0;
   end Equal;

   ----------------
   -- Get_Bounds --
   ----------------

   function Get_Bounds
      (Self   : Gsk_Path;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Bounds : access Graphene.Rect.Graphene_Rect_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_bounds");
   begin
      return Internal (Get_Object (Self), Bounds) /= 0;
   end Get_Bounds;

   -----------------------
   -- Get_Closest_Point --
   -----------------------

   function Get_Closest_Point
      (Self      : Gsk_Path;
       Point     : not null access Graphene.Point.Graphene_Point_T;
       Threshold : Interfaces.C.C_float;
       Result    : not null access Gsk.Path_Point.Gsk_Path_Point;
       Distance  : access Interfaces.C.C_float := null) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Point     : access Graphene.Point.Graphene_Point_T;
          Threshold : Interfaces.C.C_float;
          Result    : access Gsk.Path_Point.Gsk_Path_Point;
          Distance  : access Interfaces.C.C_float) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_closest_point");
   begin
      return Internal (Get_Object (Self), Point, Threshold, Result, Distance) /= 0;
   end Get_Closest_Point;

   -------------------
   -- Get_End_Point --
   -------------------

   function Get_End_Point
      (Self   : Gsk_Path;
       Result : not null access Gsk.Path_Point.Gsk_Path_Point)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : access Gsk.Path_Point.Gsk_Path_Point)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_end_point");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Get_End_Point;

   --------------
   -- Get_Next --
   --------------

   function Get_Next
      (Self  : Gsk_Path;
       Point : in out Gsk.Path_Point.Gsk_Path_Point) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Point : access Gsk.Path_Point.Gsk_Path_Point)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_next");
      Acc_Point  : aliased Gsk.Path_Point.Gsk_Path_Point := Point;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Point'Access);
      Point := Acc_Point;
      return Tmp_Return /= 0;
   end Get_Next;

   ------------------
   -- Get_Previous --
   ------------------

   function Get_Previous
      (Self  : Gsk_Path;
       Point : in out Gsk.Path_Point.Gsk_Path_Point) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Acc_Point : access Gsk.Path_Point.Gsk_Path_Point)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_previous");
      Acc_Point  : aliased Gsk.Path_Point.Gsk_Path_Point := Point;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_Point'Access);
      Point := Acc_Point;
      return Tmp_Return /= 0;
   end Get_Previous;

   ---------------------
   -- Get_Start_Point --
   ---------------------

   function Get_Start_Point
      (Self   : Gsk_Path;
       Result : not null access Gsk.Path_Point.Gsk_Path_Point)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Result : access Gsk.Path_Point.Gsk_Path_Point)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_start_point");
   begin
      return Internal (Get_Object (Self), Result) /= 0;
   end Get_Start_Point;

   -----------------------
   -- Get_Stroke_Bounds --
   -----------------------

   function Get_Stroke_Bounds
      (Self   : Gsk_Path;
       Stroke : Gsk.Stroke.Gsk_Stroke;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Stroke : System.Address;
          Bounds : access Graphene.Rect.Graphene_Rect_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_stroke_bounds");
   begin
      return Internal (Get_Object (Self), Get_Object (Stroke), Bounds) /= 0;
   end Get_Stroke_Bounds;

   ----------------------
   -- Get_Tight_Bounds --
   ----------------------

   function Get_Tight_Bounds
      (Self   : Gsk_Path;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean
   is
      function Internal
         (Self   : System.Address;
          Bounds : access Graphene.Rect.Graphene_Rect_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_get_tight_bounds");
   begin
      return Internal (Get_Object (Self), Bounds) /= 0;
   end Get_Tight_Bounds;

   -------------
   -- In_Fill --
   -------------

   function In_Fill
      (Self      : Gsk_Path;
       Point     : not null access Graphene.Point.Graphene_Point_T;
       Fill_Rule : Gsk.Enums.Gsk_Fill_Rule) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Point     : access Graphene.Point.Graphene_Point_T;
          Fill_Rule : Gsk.Enums.Gsk_Fill_Rule) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_in_fill");
   begin
      return Internal (Get_Object (Self), Point, Fill_Rule) /= 0;
   end In_Fill;

   ---------------
   -- Is_Closed --
   ---------------

   function Is_Closed (Self : Gsk_Path) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_is_closed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Closed;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Self : Gsk_Path) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_path_is_empty");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Empty;

   -----------
   -- Print --
   -----------

   procedure Print (Self : Gsk_Path; String : Glib.String.Gstring) is
      procedure Internal
         (Self   : System.Address;
          String : Glib.String.Gstring);
      pragma Import (C, Internal, "gsk_path_print");
   begin
      Internal (Get_Object (Self), String);
   end Print;

   ---------
   -- Ref --
   ---------

   function Ref (Self : Gsk_Path) return Gsk_Path is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_path_ref");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Ref;

   --------------
   -- To_Cairo --
   --------------

   procedure To_Cairo (Self : Gsk_Path; Cr : Cairo.Cairo_Context) is
      procedure Internal (Self : System.Address; Cr : Cairo.Cairo_Context);
      pragma Import (C, Internal, "gsk_path_to_cairo");
   begin
      Internal (Get_Object (Self), Cr);
   end To_Cairo;

   ---------------
   -- To_String --
   ---------------

   function To_String (Self : Gsk_Path) return UTF8_String is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gsk_path_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   -----------
   -- Unref --
   -----------

   procedure Unref (Self : Gsk_Path) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gsk_path_unref");
   begin
      Internal (Get_Object (Self));
   end Unref;

   -----------
   -- Parse --
   -----------

   function Parse (String : UTF8_String) return Gsk_Path is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gsk_path_parse");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      Tmp_Return := Internal (Tmp_String);
      Free (Tmp_String);
      return From_Object (Tmp_Return);
   end Parse;

end Gsk.Path;
