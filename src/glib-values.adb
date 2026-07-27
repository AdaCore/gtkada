------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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
with Gtkada.Types; use Gtkada.Types;

package body Glib.Values is

   ----------
   -- Free --
   ----------

   procedure Free (Val : in out GValues) is
      procedure Internal (Value_Array : C_GValues);
      pragma Import (C, Internal, "g_value_array_free");
   begin
      Internal (Val.Arr);
      Val := (Nb => 0, Arr => C_GValues (System.Null_Address));
   end Free;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean (Value : GValue) return Boolean is
      function Internal (Value : GValue) return Gboolean;
      pragma Import (C, Internal, "g_value_get_boolean");
   begin
      return Boolean'Val (Internal (Value));
   end Get_Boolean;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Value : GValue) return Glib.Object.GObject is
      function Internal (Value : GValue) return System.Address;
      pragma Import (C, Internal, "g_value_get_object");

   begin
      return Glib.Object.Convert (Internal (Value));
   end Get_Object;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Value : GValue) return String is
      function Internal (Value : GValue) return Chars_Ptr;
      pragma Import (C, Internal, "g_value_get_string");
      C : constant Chars_Ptr := Internal (Value);
   begin
      if C = Null_Ptr then
         return "";
      else
         return Gtkada.Types.Value (C);
      end if;
   end Get_String;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Value : GValue; Length : Gint) return String is
      function Internal (Value : GValue) return Chars_Ptr;
      pragma Import (C, Internal, "g_value_get_string");
      C : constant Chars_Ptr := Internal (Value);
   begin
      if C = Null_Ptr then
         return "";
      else
         return Gtkada.Types.Value
           (C, Interfaces.C.size_t (Length));
      end if;
   end Get_String;

   -----------------
   -- Make_Values --
   -----------------

   function Make_Values (Nb : Guint) return GValues is
      function Internal (N_Prealloced : Guint) return C_GValues;
      pragma Import (C, Internal, "g_value_array_new");
   begin
      return (Nb => Nb, Arr => Internal (Nb));
   end Make_Values;

   function Make_Values (Nb : Guint; Val : C_GValues) return GValues is
   begin
      return (Nb => Nb, Arr => Val);
   end Make_Values;

   ---------
   -- Nth --
   ---------

   function Nth (Val : GValues; Num : Guint) return GValue is
      procedure Internal (Val : C_GValues; Num : Guint; V : in out GValue);
      pragma Import (C, Internal, "ada_gvalue_nth");
      V : GValue;
   begin
      --   Should this be greater or greater or equal ???
      if Num > Val.Nb then
         raise Constraint_Error;
      end if;

      Internal (Val.Arr, Num, V);
      return V;
   end Nth;

   -----------------
   -- Set_Boolean --
   -----------------

   procedure Set_Boolean (Value : in out GValue; V_Boolean : Boolean) is
      procedure Internal (Value : GValue; V_Boolean : Gboolean);
      pragma Import (C, Internal, "g_value_set_boolean");
   begin
      Internal (Value, Boolean'Pos (V_Boolean));
   end Set_Boolean;

   ----------------------
   -- Init_Set_Boolean --
   ----------------------

   procedure Init_Set_Boolean (Value : in out GValue; V : Boolean) is
   begin
      Init (Value, GType_Boolean);
      Set_Boolean (Value, V);
   end Init_Set_Boolean;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
      (Value : in out GValue; To : access Glib.Object.GObject_Record'Class)
   is
      procedure Internal
        (Value : in out Glib.Values.GValue;
         To    : System.Address);
      pragma Import (C, Internal, "g_value_set_object");

   begin
      Internal (Value, Glib.Object.Convert (Glib.Object.GObject (To)));
   end Set_Object;

   ---------------------
   -- Init_Set_String --
   ---------------------

   procedure Init_Set_String (Value : in out GValue; V : String) is
   begin
      Init (Value, GType_String);
      Set_String (Value, V);
   end Init_Set_String;

   ------------------
   -- Init_Set_Int --
   ------------------

   procedure Init_Set_Int (Value : in out GValue; V : Gint) is
   begin
      Init (Value, GType_Int);
      Set_Int (Value, V);
   end Init_Set_Int;

   -------------------
   -- Init_Set_Uint --
   -------------------

   procedure Init_Set_Uint (Value : in out GValue; V : Guint) is
   begin
      Init (Value, GType_Uint);
      Set_Uint (Value, V);
   end Init_Set_Uint;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (Value : in out GValue; V_String : String) is
      procedure Internal (Value : in out GValue; V_String : String);
      pragma Import (C, Internal, "g_value_set_string");
   begin
      Internal (Value, V_String & ASCII.NUL);
   end Set_String;

   ----------
   -- Init --
   ----------

   procedure Init (Value : in out GValue; G_Type : Glib.GType) is
      procedure Internal (Value : in out GValue; G_Type : Glib.GType);
      pragma Import (C, Internal, "g_value_init");
   begin
      Value := (g_type => 0, data => (others => 0));
      Internal (Value, G_Type);
   end Init;

   ----------------------
   -- Unsafe_Proxy_Nth --
   ----------------------

   function Unsafe_Proxy_Nth (Values : C_GValues; Num : Guint) return T is
      type T_Access is access T;
      function Convert is new Ada.Unchecked_Conversion (C_Proxy, T_Access);
      Val : GValue;
   begin
      Unsafe_Nth (Values, Num, Val);
      return Convert (Get_Proxy (Val)).all;
   end Unsafe_Proxy_Nth;

   ---------------------
   -- Unsafe_Enum_Nth --
   ---------------------

   function Unsafe_Enum_Nth
     (Values : C_GValues; Num : Guint) return T
   is
      Val : GValue;
   begin
      Unsafe_Nth (Values, Num, Val);
      return T'Val (Get_Int (Val));
   end Unsafe_Enum_Nth;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Value : GValue) return Glib.GType is
   begin
      return Value.g_type;
   end Type_Of;

   ----------
   -- Free --
   ----------

   procedure Free (Params : in out GParameter_Array) is
   begin
      for P in Params'Range loop
         Gtkada.Types.g_free (Params (P).Name);
         Unset (Params (P).Value);
      end loop;
   end Free;

   -----------
   -- G_New --
   -----------

   procedure G_New
      (Object  : not null access Glib.Object.GObject_Record'Class;
       Typ     : GType;
       Params  : GParameter_Array)
   is
      function Internal
         (Typ      : GType;
          N_Params : Guint;
          Params   : System.Address) return System.Address;
      pragma Import (C, Internal, "g_object_newv");
   begin
      if not Object.Is_Created then
         Glib.Object.Set_Object
            (Object,
             Internal (Typ, Params'Length, Params (Params'First)'Address));
      end if;
   end G_New;

   -------------------
   -- C_Gvalue_Size --
   -------------------

   function C_Gvalue_Size return Natural;
   pragma Import (C, C_Gvalue_Size, "ada_c_gvalue_size");

begin
   if GValue'Size /= C_Gvalue_Size * 8 then
      raise Program_Error;
   end if;

end Glib.Values;
