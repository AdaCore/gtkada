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
with Interfaces.C;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types; use Gtkada.Types;
pragma Warnings(On);

package body Glib.Test is

   ----------
   -- Init --
   ----------

   procedure Init is
      use Interfaces.C;
      use Gtkada.Types;

      procedure Internal
        (Argc : access Glib.Gint;
         Argv : System.Address);
      pragma Import (C, Internal, "gnat_test_init");

      Count : constant Natural := Ada.Command_Line.Argument_Count;

      Argc  : aliased Glib.Gint := Glib.Gint (Count + 1);
      Args  : aliased Chars_Ptr_Array (0 .. size_t (Count) + 1);
      Argv  : aliased System.Address := Args (0)'Address;
   begin
      Args (0) := New_String (Ada.Command_Line.Command_Name);
      for I in 1 .. Count loop
         Args (size_t (I)) := New_String (Ada.Command_Line.Argument (I));
      end loop;
      Args (size_t (Count) + 1) := Null_Ptr;
      Internal (Argc'Access, Argv'Address);
   end Init;

   -------------
   -- Message --
   -------------

   procedure Message (Msg : UTF8_String) is
      procedure Internal (S : UTF8_String);
      pragma Import (C, Internal, "gnat_test_message");
   begin
      Internal (Msg & ASCII.NUL);
   end Message;

   --  GLib assertion sinks. The g_assert_* macros expand to these
   --  exported functions; we evaluate the condition in Ada and call the
   --  appropriate one on failure. The domain is passed as a null pointer
   --  (System.Null_Address), matching C code compiled without G_LOG_DOMAIN.

   procedure G_Assertion_Message
     (Domain  : System.Address;
      File    : Chars_Ptr;
      Line    : Glib.Gint;
      Func    : Chars_Ptr;
      Message : Chars_Ptr);
   pragma Import (C, G_Assertion_Message, "g_assertion_message");

   procedure G_Assertion_Message_Expr
     (Domain : System.Address;
      File   : Chars_Ptr;
      Line   : Glib.Gint;
      Func   : Chars_Ptr;
      Expr   : Chars_Ptr);
   pragma Import (C, G_Assertion_Message_Expr, "g_assertion_message_expr");
   pragma No_Return (G_Assertion_Message_Expr);

   procedure G_Assertion_Message_Error
     (Domain       : System.Address;
      File         : Chars_Ptr;
      Line         : Glib.Gint;
      Func         : Chars_Ptr;
      Expr         : Chars_Ptr;
      Error        : Glib.Error.GError;
      Error_Domain : Glib.GQuark;
      Error_Code   : Glib.Gint);
   pragma Import (C, G_Assertion_Message_Error, "g_assertion_message_error");

   type Comparison is (Eq, Ne, Lt, Le, Gt, Ge);

   function Op_Image (Op : Comparison) return UTF8_String is
   begin
      case Op is
         when Eq => return "==";
         when Ne => return "!=";
         when Lt => return "<";
         when Le => return "<=";
         when Gt => return ">";
         when Ge => return ">=";
      end case;
   end Op_Image;

   --  Order is the sign of the comparison of the two operands.

   function Holds (Op : Comparison; Order : Integer) return Boolean is
   begin
      case Op is
         when Eq => return Order = 0;
         when Ne => return Order /= 0;
         when Lt => return Order < 0;
         when Le => return Order <= 0;
         when Gt => return Order > 0;
         when Ge => return Order >= 0;
      end case;
   end Holds;

   function Img (V : Glib.Gint) return UTF8_String is
      S : constant String := Glib.Gint'Image (V);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;

   function Img (V : Glib.Guint) return UTF8_String is
      S : constant String := Glib.Guint'Image (V);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Img (V : Glib.Gdouble) return UTF8_String is
      S : constant String := Glib.Gdouble'Image (V);
   begin
      if S (S'First) = ' ' then
         return S (S'First + 1 .. S'Last);
      else
         return S;
      end if;
   end Img;

   function Hex_Image (V : Glib.Guint) return UTF8_String is
      Hex : constant String := "0123456789abcdef";
      Buf : String (1 .. 16);
      N   : Glib.Guint := V;
      Pos : Natural := Buf'Last + 1;
   begin
      if V = 0 then
         return "0x0";
      end if;
      while N > 0 loop
         Pos := Pos - 1;
         Buf (Pos) := Hex (Integer (N mod 16) + 1);
         N := N / 16;
      end loop;
      return "0x" & Buf (Pos .. Buf'Last);
   end Hex_Image;

   procedure Report (File, Func, Message : UTF8_String; Line : Natural) is
      C_File : Chars_Ptr := New_String (File);
      C_Func : Chars_Ptr := New_String (Func);
      C_Msg  : Chars_Ptr := New_String (Message);
   begin
      G_Assertion_Message
        (System.Null_Address, C_File, Glib.Gint (Line), C_Func, C_Msg);
      Free (C_File);
      Free (C_Func);
      Free (C_Msg);
   end Report;

   procedure Check_Int
     (N1 : Glib.Gint; Op : Comparison; N2 : Glib.Gint;
      File, Func : UTF8_String; Line : Natural)
   is
      Order : Integer;
   begin
      if N1 < N2 then
         Order := -1;
      elsif N1 > N2 then
         Order := 1;
      else
         Order := 0;
      end if;
      if not Holds (Op, Order) then
         Report
           (File, Func,
               "assertion failed: (" & Img (N1) & " " & Op_Image (Op) & " "
               & Img (N2) & ")",
            Line);
      end if;
   end Check_Int;

   procedure Check_Uint
     (N1 : Glib.Guint; Op : Comparison; N2 : Glib.Guint;
      File, Func : UTF8_String; Line : Natural)
   is
      Order : Integer;
   begin
      if N1 < N2 then
         Order := -1;
      elsif N1 > N2 then
         Order := 1;
      else
         Order := 0;
      end if;
      if not Holds (Op, Order) then
         Report
           (File, Func,
               "assertion failed: (" & Img (N1) & " " & Op_Image (Op) & " "
               & Img (N2) & ")",
            Line);
      end if;
   end Check_Uint;

   procedure Check_Hex
     (N1 : Glib.Guint; Op : Comparison; N2 : Glib.Guint;
      File, Func : UTF8_String; Line : Natural)
   is
      Order : Integer;
   begin
      if N1 < N2 then
         Order := -1;
      elsif N1 > N2 then
         Order := 1;
      else
         Order := 0;
      end if;
      if not Holds (Op, Order) then
         Report
           (File, Func,
               "assertion failed: (" & Hex_Image (N1) & " " & Op_Image (Op)
               & " " & Hex_Image (N2) & ")",
            Line);
      end if;
   end Check_Hex;

   procedure Check_Float
     (N1 : Glib.Gdouble; Op : Comparison; N2 : Glib.Gdouble;
      File, Func : UTF8_String; Line : Natural)
   is
      Order : Integer;
   begin
      if N1 < N2 then
         Order := -1;
      elsif N1 > N2 then
         Order := 1;
      else
         Order := 0;
      end if;
      if not Holds (Op, Order) then
         Report
           (File, Func,
               "assertion failed: (" & Img (N1) & " " & Op_Image (Op) & " "
               & Img (N2) & ")",
            Line);
      end if;
   end Check_Float;

   procedure Check_Str
     (S1 : UTF8_String; Op : Comparison; S2 : UTF8_String;
      File, Func : UTF8_String; Line : Natural)
   is
      Order : Integer;
   begin
      if S1 < S2 then
         Order := -1;
      elsif S1 > S2 then
         Order := 1;
      else
         Order := 0;
      end if;
      if not Holds (Op, Order) then
         Report
           (File, Func,
               "assertion failed: (""" & S1 & """ " & Op_Image (Op) & " """
               & S2 & """)",
            Line);
      end if;
   end Check_Str;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Condition : Boolean; Expr : UTF8_String := "";
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if not Condition then
         declare
            C_File : constant Chars_Ptr := New_String (File);
            C_Func : constant Chars_Ptr := New_String (Func);
            C_Expr : constant Chars_Ptr := New_String (Expr);
         begin
            G_Assertion_Message_Expr
              (System.Null_Address, C_File, Glib.Gint (Line), C_Func, C_Expr);
         end;
      end if;
   end Assert;

   procedure Assert_True
     (Condition : Boolean; Expr : UTF8_String := "";
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if not Condition then
         if Expr = "" then
            Report (File, Func, "should be TRUE", Line);
         else
            Report (File, Func, "'" & Expr & "' should be TRUE", Line);
         end if;
      end if;
   end Assert_True;

   procedure Assert_False
     (Condition : Boolean; Expr : UTF8_String := "";
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Condition then
         if Expr = "" then
            Report (File, Func, "should be FALSE", Line);
         else
            Report (File, Func, "'" & Expr & "' should be FALSE", Line);
         end if;
      end if;
   end Assert_False;

   procedure Assert_Null
     (Object : System.Address; Expr : UTF8_String := "";
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity)
   is
      use type System.Address;
   begin
      if Object /= System.Null_Address then
         if Expr = "" then
            Report (File, Func, "should be NULL", Line);
         else
            Report (File, Func, "'" & Expr & "' should be NULL", Line);
         end if;
      end if;
   end Assert_Null;

   procedure Assert_Nonnull
     (Object : System.Address; Expr : UTF8_String := "";
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity)
   is
      use type System.Address;
   begin
      if Object = System.Null_Address then
         if Expr = "" then
            Report (File, Func, "should not be NULL", Line);
         else
            Report (File, Func, "'" & Expr & "' should not be NULL", Line);
         end if;
      end if;
   end Assert_Nonnull;

   procedure Assert_Not_Reached
     (File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity)
   is
      C_File : constant Chars_Ptr := New_String (File);
      C_Func : constant Chars_Ptr := New_String (Func);
   begin
      G_Assertion_Message_Expr
        (System.Null_Address, C_File, Glib.Gint (Line), C_Func, Null_Ptr);
   end Assert_Not_Reached;

   --  Signed integer comparisons.

   procedure Assert_Cmpint_Eq
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Int (N1, Eq, N2, File, Func, Line);
   end Assert_Cmpint_Eq;

   procedure Assert_Cmpint_Ne
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Int (N1, Ne, N2, File, Func, Line);
   end Assert_Cmpint_Ne;

   procedure Assert_Cmpint_Lt
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Int (N1, Lt, N2, File, Func, Line);
   end Assert_Cmpint_Lt;

   procedure Assert_Cmpint_Le
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Int (N1, Le, N2, File, Func, Line);
   end Assert_Cmpint_Le;

   procedure Assert_Cmpint_Gt
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Int (N1, Gt, N2, File, Func, Line);
   end Assert_Cmpint_Gt;

   procedure Assert_Cmpint_Ge
     (N1, N2 : Glib.Gint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Int (N1, Ge, N2, File, Func, Line);
   end Assert_Cmpint_Ge;

   --  Unsigned integer comparisons.

   procedure Assert_Cmpuint_Eq
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Uint (N1, Eq, N2, File, Func, Line);
   end Assert_Cmpuint_Eq;

   procedure Assert_Cmpuint_Ne
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Uint (N1, Ne, N2, File, Func, Line);
   end Assert_Cmpuint_Ne;

   procedure Assert_Cmpuint_Lt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Uint (N1, Lt, N2, File, Func, Line);
   end Assert_Cmpuint_Lt;

   procedure Assert_Cmpuint_Le
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Uint (N1, Le, N2, File, Func, Line);
   end Assert_Cmpuint_Le;

   procedure Assert_Cmpuint_Gt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Uint (N1, Gt, N2, File, Func, Line);
   end Assert_Cmpuint_Gt;

   procedure Assert_Cmpuint_Ge
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Uint (N1, Ge, N2, File, Func, Line);
   end Assert_Cmpuint_Ge;

   --  Hexadecimal unsigned comparisons.

   procedure Assert_Cmphex_Eq
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Hex (N1, Eq, N2, File, Func, Line);
   end Assert_Cmphex_Eq;

   procedure Assert_Cmphex_Ne
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Hex (N1, Ne, N2, File, Func, Line);
   end Assert_Cmphex_Ne;

   procedure Assert_Cmphex_Lt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Hex (N1, Lt, N2, File, Func, Line);
   end Assert_Cmphex_Lt;

   procedure Assert_Cmphex_Le
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Hex (N1, Le, N2, File, Func, Line);
   end Assert_Cmphex_Le;

   procedure Assert_Cmphex_Gt
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Hex (N1, Gt, N2, File, Func, Line);
   end Assert_Cmphex_Gt;

   procedure Assert_Cmphex_Ge
     (N1, N2 : Glib.Guint;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Hex (N1, Ge, N2, File, Func, Line);
   end Assert_Cmphex_Ge;

   --  Floating-point comparisons.

   procedure Assert_Cmpfloat_Eq
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Float (N1, Eq, N2, File, Func, Line);
   end Assert_Cmpfloat_Eq;

   procedure Assert_Cmpfloat_Ne
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Float (N1, Ne, N2, File, Func, Line);
   end Assert_Cmpfloat_Ne;

   procedure Assert_Cmpfloat_Lt
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Float (N1, Lt, N2, File, Func, Line);
   end Assert_Cmpfloat_Lt;

   procedure Assert_Cmpfloat_Le
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Float (N1, Le, N2, File, Func, Line);
   end Assert_Cmpfloat_Le;

   procedure Assert_Cmpfloat_Gt
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Float (N1, Gt, N2, File, Func, Line);
   end Assert_Cmpfloat_Gt;

   procedure Assert_Cmpfloat_Ge
     (N1, N2 : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Float (N1, Ge, N2, File, Func, Line);
   end Assert_Cmpfloat_Ge;

   procedure Assert_Cmpfloat_With_Epsilon
     (N1, N2, Epsilon : Glib.Gdouble;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if abs (N1 - N2) >= Epsilon then
         Report
           (File, Func,
               "assertion failed: (" & Img (N1) & " == " & Img (N2)
               & " (+/- " & Img (Epsilon) & "))",
            Line);
      end if;
   end Assert_Cmpfloat_With_Epsilon;

   --  String comparisons.

   procedure Assert_Cmpstr_Eq
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Str (S1, Eq, S2, File, Func, Line);
   end Assert_Cmpstr_Eq;

   procedure Assert_Cmpstr_Ne
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Str (S1, Ne, S2, File, Func, Line);
   end Assert_Cmpstr_Ne;

   procedure Assert_Cmpstr_Lt
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Str (S1, Lt, S2, File, Func, Line);
   end Assert_Cmpstr_Lt;

   procedure Assert_Cmpstr_Le
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Str (S1, Le, S2, File, Func, Line);
   end Assert_Cmpstr_Le;

   procedure Assert_Cmpstr_Gt
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Str (S1, Gt, S2, File, Func, Line);
   end Assert_Cmpstr_Gt;

   procedure Assert_Cmpstr_Ge
     (S1, S2 : UTF8_String;
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      Check_Str (S1, Ge, S2, File, Func, Line);
   end Assert_Cmpstr_Ge;

   --  Error assertions.

   procedure Assert_No_Error
     (Error : Glib.Error.GError; Expr : UTF8_String := "";
      File : UTF8_String := GNAT.Source_Info.File;
      Line : Natural     := GNAT.Source_Info.Line;
      Func : UTF8_String := GNAT.Source_Info.Enclosing_Entity)
   is
      use type Glib.Error.GError;
   begin
      if Error /= null then
         declare
            C_File : Chars_Ptr := New_String (File);
            C_Func : Chars_Ptr := New_String (Func);
            C_Expr : Chars_Ptr := New_String (Expr);
         begin
            G_Assertion_Message_Error
              (System.Null_Address, C_File, Glib.Gint (Line), C_Func,
               C_Expr, Error, 0, 0);
            Free (C_File);
            Free (C_Func);
            Free (C_Expr);
         end;
      end if;
   end Assert_No_Error;

   procedure Assert_Error
     (Error  : Glib.Error.GError; Domain : Glib.GQuark; Code : Glib.Gint;
      Expr   : UTF8_String := "";
      File   : UTF8_String := GNAT.Source_Info.File;
      Line   : Natural     := GNAT.Source_Info.Line;
      Func   : UTF8_String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if not Glib.Error.Error_Matches (Error, Domain, Code) then
         declare
            C_File : Chars_Ptr := New_String (File);
            C_Func : Chars_Ptr := New_String (Func);
            C_Expr : Chars_Ptr := New_String (Expr);
         begin
            G_Assertion_Message_Error
              (System.Null_Address, C_File, Glib.Gint (Line), C_Func,
               C_Expr, Error, Domain, Code);
            Free (C_File);
            Free (C_Func);
            Free (C_Expr);
         end;
      end if;
   end Assert_Error;

   -------------------
   -- Add_Data_Func --
   -------------------

   procedure Add_Data_Func
      (Testpath  : UTF8_String;
       Test_Data : System.Address;
       Test_Func : Test_Data_Function)
   is
      procedure Internal
         (Testpath  : Gtkada.Types.Chars_Ptr;
          Test_Data : System.Address;
          Test_Func : Test_Data_Function);
      pragma Import (C, Internal, "g_test_add_data_func");
      Tmp_Testpath : Gtkada.Types.Chars_Ptr := New_String (Testpath);
   begin
      Internal (Tmp_Testpath, Test_Data, Test_Func);
      Free (Tmp_Testpath);
   end Add_Data_Func;

   --------------
   -- Add_Func --
   --------------

   procedure Add_Func (Testpath : UTF8_String; Test_Func : Test_Function) is
      procedure Internal
         (Testpath  : Gtkada.Types.Chars_Ptr;
          Test_Func : Test_Function);
      pragma Import (C, Internal, "g_test_add_func");
      Tmp_Testpath : Gtkada.Types.Chars_Ptr := New_String (Testpath);
   begin
      Internal (Tmp_Testpath, Test_Func);
      Free (Tmp_Testpath);
   end Add_Func;

   ------------
   -- Failed --
   ------------

   function Failed return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "g_test_failed");
   begin
      return Internal /= 0;
   end Failed;

   ----------
   -- Skip --
   ----------

   procedure Skip (Msg : UTF8_String := "") is
      procedure Internal (Msg : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "g_test_skip");
      Tmp_Msg : Gtkada.Types.Chars_Ptr;
   begin
      if Msg = "" then
         Tmp_Msg := Gtkada.Types.Null_Ptr;
      else
         Tmp_Msg := New_String (Msg);
      end if;
      Internal (Tmp_Msg);
      Free (Tmp_Msg);
   end Skip;

end Glib.Test;
