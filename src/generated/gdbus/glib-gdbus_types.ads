
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


pragma Warnings (Off, "*is already use-visible*");
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Gtkada.Types;            use Gtkada.Types;
with Interfaces.C.Pointers;

package Glib.GDBus_Types is

   type GDBus_Property_Info_Flags is mod 2 ** Integer'Size;
   pragma Convention (C, GDBus_Property_Info_Flags);
   --  Flags describing the access control of a D-Bus property.

   G_Dbus_Property_Info_Flags_None : constant GDBus_Property_Info_Flags := 0;
   G_Dbus_Property_Info_Flags_Readable : constant GDBus_Property_Info_Flags := 1;
   G_Dbus_Property_Info_Flags_Writable : constant GDBus_Property_Info_Flags := 2;

   type Gdbus_Annotation_Info;
   type Gdbus_Annotation_Info_Access is access all Gdbus_Annotation_Info;
   type Aliased_Gdbus_Annotation_Info_Access_Array is
      array (Positive range <>) of aliased Gdbus_Annotation_Info_Access
     with Convention => C;
   package Gdbus_Annotation_Info_Array_Pointers is new Interfaces.C.Pointers
     (Positive, Gdbus_Annotation_Info_Access,
      Aliased_Gdbus_Annotation_Info_Access_Array, null);
   subtype Gdbus_Annotation_Info_Array_Pointer is
     Gdbus_Annotation_Info_Array_Pointers.Pointer;

   type Gdbus_Annotation_Info is record
      Ref_Count : Glib.Gint := 0;
      Key : Gtkada.Types.Chars_Ptr;
      Value : Gtkada.Types.Chars_Ptr;
      Annotations : Gdbus_Annotation_Info_Array_Pointer;
   end record;
   pragma Convention (C, Gdbus_Annotation_Info);
   --  Information about an annotation.

   type Gdbus_Arg_Info is record
      Ref_Count : Glib.Gint := 0;
      Name : Gtkada.Types.Chars_Ptr;
      Signature : Gtkada.Types.Chars_Ptr;
      Annotations : Gdbus_Annotation_Info_Array_Pointer;
   end record;
   pragma Convention (C, Gdbus_Arg_Info);
   --  Information about an argument for a method or a signal.

   type Gdbus_Arg_Info_Access is access all Gdbus_Arg_Info;
   type Aliased_Gdbus_Arg_Info_Access_Array is
      array (Positive range <>) of aliased Gdbus_Arg_Info_Access
     with Convention => C;
   package Gdbus_Arg_Info_Array_Pointers is new Interfaces.C.Pointers
     (Positive, Gdbus_Arg_Info_Access,
      Aliased_Gdbus_Arg_Info_Access_Array, null);
   subtype Gdbus_Arg_Info_Array_Pointer is
     Gdbus_Arg_Info_Array_Pointers.Pointer;

   type Gdbus_Method_Info is record
      Ref_Count : Glib.Gint := 0;
      Name : Gtkada.Types.Chars_Ptr;
      In_Args : Gdbus_Arg_Info_Array_Pointer;
      Out_Args : Gdbus_Arg_Info_Array_Pointer;
      Annotations : Gdbus_Annotation_Info_Array_Pointer;
   end record;
   pragma Convention (C, Gdbus_Method_Info);
   --  Information about a method on a D-Bus interface.

   type Gdbus_Signal_Info is record
      Ref_Count : Glib.Gint := 0;
      Name : Gtkada.Types.Chars_Ptr;
      Args : Gdbus_Arg_Info_Array_Pointer;
      Annotations : Gdbus_Annotation_Info_Array_Pointer;
   end record;
   pragma Convention (C, Gdbus_Signal_Info);
   --  Information about a signal on a D-Bus interface.

   type Gdbus_Property_Info is record
      Ref_Count : Glib.Gint := 0;
      Name : Gtkada.Types.Chars_Ptr;
      Signature : Gtkada.Types.Chars_Ptr;
      Flags : GDBus_Property_Info_Flags;
      Annotations : Gdbus_Annotation_Info_Array_Pointer;
   end record;
   pragma Convention (C, Gdbus_Property_Info);
   --  Information about a D-Bus property on a D-Bus interface.

   type Gdbus_Method_Info_Access is access all Gdbus_Method_Info;
   type Aliased_Gdbus_Method_Info_Access_Array is
      array (Positive range <>) of aliased Gdbus_Method_Info_Access
     with Convention => C;
   package Gdbus_Method_Info_Array_Pointers is new Interfaces.C.Pointers
     (Positive, Gdbus_Method_Info_Access,
      Aliased_Gdbus_Method_Info_Access_Array, null);
   subtype Gdbus_Method_Info_Array_Pointer is
     Gdbus_Method_Info_Array_Pointers.Pointer;

   type Gdbus_Signal_Info_Access is access all Gdbus_Signal_Info;
   type Aliased_Gdbus_Signal_Info_Access_Array is
      array (Positive range <>) of aliased Gdbus_Signal_Info_Access
     with Convention => C;
   package Gdbus_Signal_Info_Array_Pointers is new Interfaces.C.Pointers
     (Positive, Gdbus_Signal_Info_Access,
      Aliased_Gdbus_Signal_Info_Access_Array, null);
   subtype Gdbus_Signal_Info_Array_Pointer is
     Gdbus_Signal_Info_Array_Pointers.Pointer;

   type Gdbus_Property_Info_Access is access all Gdbus_Property_Info;
   type Aliased_Gdbus_Property_Info_Access_Array is
      array (Positive range <>) of aliased Gdbus_Property_Info_Access
     with Convention => C;
   package Gdbus_Property_Info_Array_Pointers is new Interfaces.C.Pointers
     (Positive, Gdbus_Property_Info_Access,
      Aliased_Gdbus_Property_Info_Access_Array, null);
   subtype Gdbus_Property_Info_Array_Pointer is
     Gdbus_Property_Info_Array_Pointers.Pointer;

   type Gdbus_Interface_Info is record
      Ref_Count : Glib.Gint := 0;
      Name : Gtkada.Types.Chars_Ptr;
      Methods : Gdbus_Method_Info_Array_Pointer;
      Signals : Gdbus_Signal_Info_Array_Pointer;
      Properties : Gdbus_Property_Info_Array_Pointer;
      Annotations : Gdbus_Annotation_Info_Array_Pointer;
   end record;
   pragma Convention (C, Gdbus_Interface_Info);
   --  Information about a D-Bus interface.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GDBus_Property_Info_Flags_Properties is
      new Generic_Internal_Flags_Property (GDBus_Property_Info_Flags);
   type Property_GDBus_Property_Info_Flags is new GDBus_Property_Info_Flags_Properties.Property;

   ----------------------
   -- GtkAda additions --
   ----------------------

   function From_Object_Free
     (B : not null access Gdbus_Annotation_Info) return Gdbus_Annotation_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gdbus_Arg_Info) return Gdbus_Arg_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gdbus_Method_Info) return Gdbus_Method_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gdbus_Signal_Info) return Gdbus_Signal_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gdbus_Property_Info) return Gdbus_Property_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gdbus_Interface_Info) return Gdbus_Interface_Info;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

end Glib.GDBus_Types;
