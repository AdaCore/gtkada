------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with GNAT.IO;
with GNAT.Strings;         use GNAT.Strings;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtkada.Bindings is

   procedure Default_Exception_Handler
      (Occurrence : Ada.Exceptions.Exception_Occurrence);
   On_Exception : Exception_Handler := Default_Exception_Handler'Access;

   --------------------
   -- String_Or_Null --
   --------------------

   function String_Or_Null
     (S : String) return Interfaces.C.Strings.chars_ptr is
   begin
      if S = "" then
         return Null_Ptr;
      else
         return New_String (S);
      end if;
   end String_Or_Null;

   function String_Or_Null (S : String) return Gtkada.Types.Chars_Ptr is
   begin
      if S = "" then
         return Gtkada.Types.Null_Ptr;
      else
         return Gtkada.Types.New_String (S);
      end if;
   end String_Or_Null;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (C : Gtkada.Types.Chars_Ptr_Array) return String_List
   is
      Count : Natural := 0;
      use type Gtkada.Types.Chars_Ptr;
   begin
      while C (size_t (Count)) /= Gtkada.Types.Null_Ptr loop
         Count := Count + 1;
      end loop;

      return To_String_List (C, Gint (Count));
   end To_String_List;

   -----------------------------
   -- To_String_List_And_Free --
   -----------------------------

   function To_String_List_And_Free
     (C : chars_ptr_array_access) return String_List
   is
      Result : constant String_List := To_String_List (C.all);
   begin
      g_strfreev (C);
      return Result;
   end To_String_List_And_Free;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (C : Gtkada.Types.Chars_Ptr_Array; N : Gint)
      return GNAT.Strings.String_List
   is
      Result : String_List (1 .. Natural (N));
   begin
      for R in Result'Range loop
         Result (R) := new String'(Gtkada.Types.Value (C (size_t (R) - 1)));
      end loop;
      return Result;
   end To_String_List;

   ----------------------
   -- From_String_List --
   ----------------------

   function From_String_List
     (C : String_List) return Gtkada.Types.Chars_Ptr_Array
   is
      Result : Gtkada.Types.Chars_Ptr_Array (0 .. C'Length);
   begin
      for S in C'Range loop
         Result (size_t (S - C'First)) := Gtkada.Types.New_String (C (S).all);
      end loop;
      Result (Result'Last) := Gtkada.Types.Null_Ptr;
      return Result;
   end From_String_List;

   ------------------
   -- To_Chars_Ptr --
   ------------------

   function To_Chars_Ptr
     (C : chars_ptr_array_access) return Gtkada.Types.Chars_Ptr_Array
   is
      Count : size_t := 0;
      use type Gtkada.Types.Chars_Ptr;
   begin
      while C (Count) /= Gtkada.Types.Null_Ptr loop
         Count := Count + 1;
      end loop;

      declare
         Result : Gtkada.Types.Chars_Ptr_Array (0 .. Count - 1);
      begin
         for J in Result'Range loop
            Result (J) := C (J);
         end loop;
         return Result;
      end;
   end To_Chars_Ptr;

   --------------------------------
   -- Generic_To_Address_Or_Null --
   --------------------------------

   function Generic_To_Address_Or_Null
     (Val : System.Address) return System.Address
   is
      type T_Access is access all T;
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, T_Access);
   begin
      if Convert (Val).all = Null_T then
         return System.Null_Address;
      else
         return Val;
      end if;
   end Generic_To_Address_Or_Null;

   -----------------------------------
   -- To_Gint_Array_Zero_Terminated --
   -----------------------------------

   function To_Gint_Array_Zero_Terminated
     (Arr : Gint_Arrays.Unbounded_Array_Access) return Glib.Gint_Array
   is
      Count : Natural := Arr'First;
   begin
      while Arr (Count) /= 0 loop
         Count := Count + 1;
      end loop;

      declare
         Result : Gint_Array (1 .. Count);
      begin
         for R in Result'Range loop
            Result (R) := Arr (R - Result'First + Arr'First);
         end loop;
         return Result;
      end;
   end To_Gint_Array_Zero_Terminated;

   --------------------
   -- Value_And_Free --
   --------------------

   function Value_And_Free
     (Str : Interfaces.C.Strings.chars_ptr) return String
   is
   begin
      if Str = Null_Ptr then
         return "";
      end if;

      declare
         procedure C_Free (S : chars_ptr);
         pragma Import (C, C_Free, "free");

         Val : constant String := Value (Str);
      begin
         --  Since Str was allocated by gtk+ via malloc(), and not via
         --  System.Memory, we should not be using Interfaces.C.Strings.Free
         --  which goes through System.Memory. So we call free() directly
         --  instead.
         C_Free (Str);

         return Val;
      end;
   end Value_And_Free;

   --------------------
   -- Value_And_Free --
   --------------------

   function Value_And_Free
     (Str : Gtkada.Types.Chars_Ptr) return String
   is
      use type Gtkada.Types.Chars_Ptr;
   begin
      if Str = Gtkada.Types.Null_Ptr then
         return "";
      end if;

      declare
         Val : constant String := Gtkada.Types.Value (Str);
      begin
         Gtkada.Types.g_free (Str);
         return Val;
      end;
   end Value_And_Free;

   -------------------------
   -- Value_Allowing_Null --
   -------------------------

   function Value_Allowing_Null
     (Str : Interfaces.C.Strings.chars_ptr) return String
   is
   begin
      if Str = Null_Ptr then
         return "";
      end if;

      return Interfaces.C.Strings.Value (Str);
   end Value_Allowing_Null;

   function Value_Allowing_Null
     (Str : Gtkada.Types.Chars_Ptr) return String
   is
      use type Gtkada.Types.Chars_Ptr;
   begin
      if Str = Gtkada.Types.Null_Ptr then
         return "";
      end if;

      return Gtkada.Types.Value (Str);
   end Value_Allowing_Null;

   function Value_Allowing_Null
     (Str : Interfaces.C.Strings.chars_ptr) return Glib.Signal_Name
   is
   begin
      if Str = Null_Ptr then
         return "";
      end if;

      return Glib.Signal_Name (String'(Interfaces.C.Strings.Value (Str)));
   end Value_Allowing_Null;

   function Value_Allowing_Null
     (Str : Gtkada.Types.Chars_Ptr) return Glib.Signal_Name
   is
      use type Gtkada.Types.Chars_Ptr;
   begin
      if Str = Gtkada.Types.Null_Ptr then
         return "";
      end if;

      return Glib.Signal_Name (String'(Gtkada.Types.Value (Str)));
   end Value_Allowing_Null;

   ---------------------------------
   -- Unchecked_Do_Signal_Connect --
   ---------------------------------

   procedure Unchecked_Do_Signal_Connect
     (Object              : not null access Glib.Object.GObject_Record'Class;
      C_Name              : Glib.Signal_Name;
      Marshaller          : C_Marshaller;
      Handler             : System.Address;
      Destroy             : System.Address := System.Null_Address;
      After               : Boolean := False;
      Slot_Object         : access Glib.Object.GObject_Record'Class := null)
   is
      function Internal
        (Instance : System.Address;
         Detailed_Signal : Glib.Signal_Name;
         Closure  : GClosure;
         After    : Gint := 0) return Gulong;
      pragma Import (C, Internal, "g_signal_connect_closure");

      use type System.Address;
      Id : Gulong;
      Closure : GClosure;
      pragma Unreferenced (Id);

   begin
      if Slot_Object /= null then
         Closure := CClosure_New (Handler, Get_Object (Slot_Object), Destroy);
         Watch_Closure (Get_Object (Slot_Object), Closure);
      else
         Closure := CClosure_New (Handler, System.Null_Address, Destroy);
      end if;

      --  Could also use Set_Meta_Marshal to pass user data to the marshaller
      Set_Marshal (Closure, Marshaller);

      Id := Internal
        (Get_Object (Object),
         C_Name,
         Closure => Closure,
         After   => Boolean'Pos (After));
   end Unchecked_Do_Signal_Connect;

   ---------------------------------
   -- Unchecked_Do_Signal_Connect --
   ---------------------------------

   procedure Unchecked_Do_Signal_Connect
     (Object              : Glib.Types.GType_Interface;
      C_Name              : Glib.Signal_Name;
      Marshaller          : C_Marshaller;
      Handler             : System.Address;
      Destroy             : System.Address := System.Null_Address;
      After               : Boolean := False;
      Slot_Object         : access Glib.Object.GObject_Record'Class := null)
   is
      function Internal
        (Instance : Glib.Types.GType_Interface;
         Detailed_Signal : Glib.Signal_Name;
         Closure  : GClosure;
         After    : Gint := 0) return Gulong;
      pragma Import (C, Internal, "g_signal_connect_closure");

      use type System.Address;
      Id      : Gulong;
      Closure : GClosure;
      pragma Unreferenced (Id);

   begin
      if Slot_Object /= null then
         Closure := CClosure_New (Handler, Get_Object (Slot_Object), Destroy);
         Watch_Closure (Get_Object (Slot_Object), Closure);
      else
         Closure := CClosure_New (Handler, System.Null_Address, Destroy);
      end if;

      --  Could also use Set_Meta_Marshal to pass user data to the marshaller
      Set_Marshal (Closure, Marshaller);

      Id := Internal
        (Object,
         C_Name,
         Closure => Closure,
         After   => Boolean'Pos (After));
   end Unchecked_Do_Signal_Connect;

   -----------------------
   -- Process_Exception --
   -----------------------

   procedure Process_Exception (E : Ada.Exceptions.Exception_Occurrence) is
   begin
      On_Exception (E);
   exception
      when E : others =>
         --  never propagate the exception to C
         Default_Exception_Handler (E);
   end Process_Exception;

   -------------------------------
   -- Default_Exception_Handler --
   -------------------------------

   procedure Default_Exception_Handler
      (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      GNAT.IO.Put_Line
         (GNAT.IO.Standard_Error,
          Ada.Exceptions.Exception_Information (Occurrence));
   end Default_Exception_Handler;

   ----------------------
   -- Set_On_Exception --
   ----------------------

   procedure Set_On_Exception (Handler : Exception_Handler) is
   begin
      On_Exception := Handler;
   end Set_On_Exception;

end Gtkada.Bindings;
