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
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Conversion;
with Glib.Object;
pragma Warnings(Off);  --  might be unused
with Gtk.Cclosure_Expression;
with Gtk.Closure_Expression;
with Gtk.Constant_Expression;
with Gtk.Object_Expression;
with Gtk.Property_Expression;
with Gtk.Try_Expression;
with Gtkada.Types;                             use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Expression is

   ----------
   -- To_C --
   ----------

   function To_C (Value : Gtk_Expression_Array) return C_GtkExpressionArray
   is
      Result : C_GtkExpressionArray(Value'Range);
   begin
      for Index in Value'Range loop
         Result (Index) := Value (Index).Ptr;
      end loop;

      return Result;
   end To_C;

   function C_Gtk_Expression_Watch
      (Self         : System.Address;
       This         : System.Address;
       Notify       : System.Address;
       User_Data    : System.Address;
       User_Destroy : System.Address) return System.Address;
   pragma Import (C, C_Gtk_Expression_Watch, "gtk_expression_watch");
   --  Watch the given `expression` for changes.
   --  The Notify function will be called whenever the evaluation of `self`
   --  may have changed.
   --  GTK cannot guarantee that the evaluation did indeed change when the
   --  Notify gets invoked, but it guarantees the opposite: When it did in fact
   --  change, the Notify will be invoked.
   --  @param This the `this` argument to watch
   --  @param Notify callback to invoke when the expression changes
   --  @param User_Data user data to pass to the `notify` callback
   --  @param User_Destroy destroy notify for `user_data`
   --  @return The newly installed watch. Note that the only reference held to
   --  the watch will be released when the watch is unwatched which can happen
   --  automatically, and not just via [methodGtk.ExpressionWatch.unwatch]. You
   --  should call [methodGtk.ExpressionWatch.ref] if you want to keep the
   --  watch around.
   --  Return has transfer-ownership='none'

   function To_Gtk_Expression_Notify is new Ada.Unchecked_Conversion
     (System.Address, Gtk_Expression_Notify);

   function To_Address is new Ada.Unchecked_Conversion
     (Gtk.Expression.Gtk_Expression_Notify, System.Address);

   procedure Internal_Gtk_Expression_Notify (User_Data : System.Address);
   pragma Convention (C, Internal_Gtk_Expression_Notify);
   --  @param User_Data data passed to Gtk.Expression.Watch

   ------------------------------------
   -- Internal_Gtk_Expression_Notify --
   ------------------------------------

   procedure Internal_Gtk_Expression_Notify (User_Data : System.Address) is
      Func : constant Gtk_Expression_Notify := To_Gtk_Expression_Notify (User_Data);
   begin
      Func.all;
   end Internal_Gtk_Expression_Notify;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Object : in out Gtk_Expression_Record) is
      function Ref (Self : System.Address) return System.Address;
      pragma Import (C, Ref, "gtk_expression_ref");
   begin
      if Object.Ptr /= System.Null_Address then
         Object.Ptr := Ref (Object.Ptr);
      end if;
   end Adjust;

   ----------
   -- Bind --
   ----------

   function Bind
      (Self     : Gtk_Expression;
       Target   : System.Address;
       Property : UTF8_String;
       This     : System.Address)
       return Gtk.Expression_Watch.Gtk_Expression_Watch
   is
      function Internal
         (Self     : System.Address;
          Target   : System.Address;
          Property : Gtkada.Types.Chars_Ptr;
          This     : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_expression_bind");
      Tmp_Property : Gtkada.Types.Chars_Ptr := New_String (Property);
      Tmp_Return   : System.Address;
   begin
      if Self /= null then
         --  transfer-ownership='full'
         Adjust (Self.all);
      end if;
      Tmp_Return := Internal (Get_Object (Self), Target, Tmp_Property, This);
      Free (Tmp_Property);
      return From_Object (Tmp_Return);
   end Bind;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
      (Self  : Gtk_Expression;
       This  : System.Address;
       Value : in out Glib.Values.GValue) return Boolean
   is
      function Internal
         (Self      : System.Address;
          This      : System.Address;
          Acc_Value : access Glib.Values.GValue) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expression_evaluate");
      Acc_Value  : aliased Glib.Values.GValue := Value;
      Tmp_Return : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), This, Acc_Value'Access);
      Value := Acc_Value;
      return Tmp_Return /= 0;
   end Evaluate;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Gtk_Expression_Record) is
      procedure Unref (Self : System.Address);
      pragma Import (C, Unref, "gtk_expression_unref");
   begin
      if Object.Ptr /= System.Null_Address then
         Unref (Object.Ptr);
         Object.Ptr := System.Null_Address;
      end if;
   end Finalize;

   --------------------
   -- Get_Value_Type --
   --------------------

   function Get_Value_Type (Self : Gtk_Expression) return GType is
      function Internal (Self : System.Address) return GType;
      pragma Import (C, Internal, "gtk_expression_get_value_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Value_Type;

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (Self : Gtk_Expression) return Boolean is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_expression_is_static");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Static;

   -----------
   -- Watch --
   -----------

   function Watch
      (Self   : Gtk_Expression;
       This   : System.Address;
       Notify : Gtk_Expression_Notify)
       return Gtk.Expression_Watch.Gtk_Expression_Watch
   is
   begin
      if Notify = null then
         return From_Object (C_Gtk_Expression_Watch (Get_Object (Self), This, System.Null_Address, System.Null_Address, System.Null_Address));
      else
         return From_Object (C_Gtk_Expression_Watch (Get_Object (Self), This, Internal_Gtk_Expression_Notify'Address, To_Address (Notify), System.Null_Address));
      end if;
   end Watch;

   package body Watch_User_Data is

      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Gtk_Expression_Notify is new Ada.Unchecked_Conversion
        (System.Address, Gtk_Expression_Notify);

      function To_Address is new Ada.Unchecked_Conversion
        (Gtk_Expression_Notify, System.Address);

      procedure Internal_Cb (User_Data : System.Address);
      pragma Convention (C, Internal_Cb);
      --  Callback called by Gtk.Expression.Watch when the expression value
      --  changes.
      --  @param User_Data data passed to Gtk.Expression.Watch

      -----------------
      -- Internal_Cb --
      -----------------

      procedure Internal_Cb (User_Data : System.Address) is
         D : constant Users.Internal_Data_Access := Users.Convert (User_Data);
      begin
         To_Gtk_Expression_Notify (D.Func) (D.Data.all);
      end Internal_Cb;

      -----------
      -- Watch --
      -----------

      function Watch
         (Self      : Gtk.Expression.Gtk_Expression;
          This      : System.Address;
          Notify    : Gtk_Expression_Notify;
          User_Data : User_Data_Type)
          return Gtk.Expression_Watch.Gtk_Expression_Watch
      is
         D : System.Address;
      begin
         if Notify = null then
            return From_Object (C_Gtk_Expression_Watch (Get_Object (Self), This, System.Null_Address, System.Null_Address, Users.Free_Data'Address));
         else
            D := Users.Build (To_Address (Notify), User_Data);
            return From_Object (C_Gtk_Expression_Watch (Get_Object (Self), This, Internal_Cb'Address, D, Users.Free_Data'Address));
         end if;
      end Watch;

   end Watch_User_Data;

   function Dispatching_Constructor is
      new Ada.Tags.Generic_Dispatching_Constructor
     (Gtk_Expression_Record, System.Address, Create);

   ------------
   -- Create --
   ------------

   function Create
      (Ptr : not null access System.Address)
       return Dummy_Gtk_Expression_Record
   is
      pragma Unreferenced (Ptr);
      Result : Dummy_Gtk_Expression_Record;
   begin
      return Result;
   end Create;

   --------------------------------
   -- From_Object_Full_Ownership --
   --------------------------------

   function From_Object_Full_Ownership
      (Object : System.Address) return Gtk_Expression
   is
      T      : Glib.GType;
      O      : aliased System.Address := System.Null_Address;
      Result : Gtk_Expression;
   begin
      if Object /= System.Null_Address then
         T := Glib.Instance_Get_Type (Object);
         Result := new Gtk_Expression_Record'Class'
           (Dispatching_Constructor
              (Ada.Tags.Internal_Tag (Glib.Type_Name (T)), O'Access));
         Set_Object (Result, Object);
         return Result;
      else
         return null;
      end if;
   exception
      when Ada.Tags.Tag_Error =>
      Result := new Dummy_Gtk_Expression_Record'(Create (O'Access));
      Set_Object (Result, Object);
      return Result;
   end From_Object_Full_Ownership;

   --------------------------------
   -- From_Object_None_Ownership --
   --------------------------------

   function From_Object_None_Ownership
      (Object : System.Address) return Gtk_Expression
   is
      Result : Gtk_Expression;
   begin
      Result := From_Object_Full_Ownership (Object);
      if Result /= null then
         --  To call Ref
         Adjust (Result.all);
      end if;
      return Result;
   end From_Object_None_Ownership;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Self : access Gtk_Expression_Record'Class) return System.Address
   is
   begin
      if Self = null then
         return System.Null_Address;
      else
         return Self.Ptr;
      end if;
   end Get_Object;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created
      (Self : not null access Gtk_Expression_Record'Class) return Boolean
   is
   begin
      return Self.Ptr /= System.Null_Address;
   end Is_Created;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
      (Self   : not null access Gtk_Expression_Record'Class;
       Object : System.Address)
   is
   begin
      Self.Ptr := Object;
   end Set_Object;

end Gtk.Expression;
