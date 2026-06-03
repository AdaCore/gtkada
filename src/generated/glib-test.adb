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
