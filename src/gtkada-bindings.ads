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

--  This is a unit purely internal to GtkAda, to ease binding and avoid code
--  duplication.
--  Do not use in your own applications, since the interface might change from
--  release to release.
--  See also Gtkada.Types

with Ada.Exceptions;
with Glib;
with Glib.Object;
with Glib.Types;
with Glib.Values;
with Gtkada.C;
with Gtkada.Types;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

package Gtkada.Bindings is
   package ICS renames Interfaces.C.Strings;

   generic
      type T is private;
      Null_T : T;
      with function "=" (T1, T2 : T) return Boolean is <>;
   function Generic_To_Address_Or_Null
     (Val : System.Address) return System.Address;
   --  Return either a Null_Address or a pointer to Val, depending on
   --  whether Val is the null value for the type.
   --  In all cases, Val is supposed to be an access to T.
   --  In Ada2012, these could be replaced with expression functions instead.

   function Value_And_Free
     (Str : Interfaces.C.Strings.chars_ptr) return String;
   function Value_And_Free
     (Str : Gtkada.Types.Chars_Ptr) return String;
   --  Returns the value stored in Str, and free the memory occupied by Str.

   function Value_Allowing_Null
     (Str : Interfaces.C.Strings.chars_ptr) return String;
   function Value_Allowing_Null
     (Str : Gtkada.Types.Chars_Ptr) return String;
   function Value_Allowing_Null
     (Str : Interfaces.C.Strings.chars_ptr) return Glib.Signal_Name;
   function Value_Allowing_Null
     (Str : Gtkada.Types.Chars_Ptr) return Glib.Signal_Name;
   --  Return the value stored in Str, and an empty string if Str is null.

   -------------
   -- Strings --
   -------------

   function String_Or_Null (S : String) return ICS.chars_ptr;
   function String_Or_Null (S : String) return Gtkada.Types.Chars_Ptr;
   --  Return Null_Ptr if S is the empty string, or a newly allocated string
   --  otherwise. This is intended mostly for the binding itself.

   type chars_ptr_array_access
     is access Gtkada.Types.Chars_Ptr_Array (Interfaces.C.size_t);
   pragma Convention (C, chars_ptr_array_access);
   --  Suitable for a C function that returns a gchar**

   procedure g_strfreev (Str_Array : chars_ptr_array_access);
   --  Thin binding to C function of the same name.  Frees a null-terminated
   --  array of strings, and the array itself.  If called on a null value,
   --  simply return.

   function To_String_List
     (C : Gtkada.Types.Chars_Ptr_Array) return GNAT.Strings.String_List;
   --  Converts C into a String_List. Returned value must be freed by caller,
   --  as well as C. C is NULL terminated.

   function To_String_List_And_Free
     (C : chars_ptr_array_access) return GNAT.Strings.String_List;
   --  Converts C into a String_List, and frees C.
   --  Returned value must be freed by caller.

   function To_String_List
     (C : Gtkada.Types.Chars_Ptr_Array; N : Glib.Gint)
      return GNAT.Strings.String_List;
   --  Converts C into a String_List. N is the number of elements in C.
   --  Returned value must be freed by caller, as well as C.

   function From_String_List
     (C : GNAT.Strings.String_List) return Gtkada.Types.Chars_Ptr_Array;
   --  Converts C into a chars_ptr_array. Returned value must be freed by
   --  caller, as well as C.

   function To_Chars_Ptr
     (C : chars_ptr_array_access) return Gtkada.Types.Chars_Ptr_Array;
   --  Return a bounded array that contains the same strings as C (so you
   --  shouldn't free C). 'Last applies to the result, whereas it doesn't to C.

   ------------
   -- Arrays --
   ------------
   --  See Gtkada.C for more information.
   --  The packages that are commented out are instanciated in various,
   --  possibly duplicated places. This is because of elaboration circularity
   --  issues.

   package Gint_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Gint, 0, Natural, Glib.Gint_Array);
   package Pspec_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.Param_Spec, null, Natural, Glib.Param_Spec_Array);
   package GType_Arrays is new Gtkada.C.Unbounded_Arrays
     (Glib.GType, Glib.GType_None, Glib.Guint, Glib.GType_Array);

   function To_Gint_Array_Zero_Terminated
     (Arr : Gint_Arrays.Unbounded_Array_Access)
      return Glib.Gint_Array;
   --  Converts Arr, stopping at the first 0 encountered

   -------------
   -- Signals --
   -------------

   type GClosure is new Glib.C_Proxy;

   type C_Marshaller is access procedure
     (Closure         : GClosure;
      Return_Value    : Glib.Values.GValue;  --  Will contain returned value
      N_Params        : Glib.Guint;          --  Number of entries in Params
      Params          : Glib.Values.C_GValues;
      Invocation_Hint : System.Address;
      Marsh_Data      : System.Address);
   pragma Convention (C, C_Marshaller);
   --  A function called directly from gtk+ when dispatching signals to
   --  handlers. This procedure is in charge of converting the parameters from
   --  the array of GValues in Params to suitable formats for calling the
   --  proper Ada handler given by the user. This handler is encoded in the
   --  user_data, which has an actual type specific to each of the generic
   --  packages below.
   --  Marsh_Data is the data passed via Set_Meta_Marshal, null otherwise.
   --  This is meant for internal GtkAda use only.

   function CClosure_New
     (Callback  : System.Address;
      User_Data : System.Address;
      Destroy   : System.Address) return GClosure;
   pragma Import (C, CClosure_New, "g_cclosure_new");

   procedure Set_Marshal (Closure : GClosure; Marshaller : C_Marshaller);
   pragma Import (C, Set_Marshal, "g_closure_set_marshal");

   procedure Set_Meta_Marshal
     (Closure    : GClosure;
      Marsh_Data : System.Address;
      Marshaller : C_Marshaller);
   pragma Import (C, Set_Meta_Marshal, "g_closure_set_meta_marshal");

   function Get_Data (Closure : GClosure) return System.Address;
   pragma Import (C, Get_Data, "ada_gclosure_get_data");

   function Get_Callback (C : GClosure) return System.Address;
   pragma Import (C, Get_Callback, "ada_cclosure_get_callback");
   --  Return the user handler set in the closure. This is the procedure that
   --  should process the signal.

   procedure Watch_Closure (Object : System.Address; Closure : GClosure);
   pragma Import (C, Watch_Closure, "g_object_watch_closure");
   --  The closure will be destroyed when Object is destroyed.

   procedure Unchecked_Do_Signal_Connect
     (Object              : not null access Glib.Object.GObject_Record'Class;
      C_Name              : Glib.Signal_Name;
      Marshaller          : C_Marshaller;
      Handler             : System.Address;
      Destroy             : System.Address := System.Null_Address;
      After               : Boolean := False;
      Slot_Object         : access Glib.Object.GObject_Record'Class := null);
   procedure Unchecked_Do_Signal_Connect
     (Object              : Glib.Types.GType_Interface;
      C_Name              : Glib.Signal_Name;
      Marshaller          : C_Marshaller;
      Handler             : System.Address;
      Destroy             : System.Address := System.Null_Address;
      After               : Boolean := False;
      Slot_Object         : access Glib.Object.GObject_Record'Class := null);
   --  Same as above, but this removes a number of check, like whether the
   --  signal exists, and whether the user has properly passed a procedure or
   --  function depending on the signal type.
   --
   --  * C_Name must be NUL-terminated.

   procedure Set_Value (Value : Glib.Values.GValue; Val : System.Address);
   pragma Import (C, Set_Value, "ada_gvalue_set");
   --  Function used internally to specify the value returned by a callback.
   --  Val will be dereferenced as appropriate, depending on the type expected
   --  by Value.

   type Exception_Handler is not null access procedure
      (Occurrence : Ada.Exceptions.Exception_Occurrence);

   procedure Set_On_Exception (Handler : Exception_Handler);
   --  See user documentation in Gtk.Handlers.Set_On_Exception

   procedure Process_Exception (E : Ada.Exceptions.Exception_Occurrence);
   --  Process the exception through the handler set by Set_On_Exception.
   --  This procedure never raises an exception.

private
   pragma Import (C, g_strfreev, "g_strfreev");
end Gtkada.Bindings;
