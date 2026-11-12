--  Exercise the public Glib.Bytes constructors, data access, slices, and
--  references. No focused g_bytes_* C test exists to port, so the cases check
--  the documented GLib behavior directly through GtkAda.

with Ada.Command_Line;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

with Glib;       use Glib;
with Glib.Bytes; use Glib.Bytes;
with Glib.Test;  use Glib.Test;

procedure Main is
   use type Glib.Gsize;
   use type Glib.Guint8;
   use type System.Address;
   use System.Storage_Elements;

   subtype Sample_Array is Guint8_Array (0 .. 3);
   package Byte_Access is new System.Address_To_Access_Conversions (Guint8);
   package Sample_Access is new System.Address_To_Access_Conversions
     (Sample_Array);
   package Bytes_Access is new System.Address_To_Access_Conversions (Gbytes);

   --  Static constructors retain their input pointer, so this data must outlive
   --  each Gbytes made from it and must not be modified.
   Static_Data : aliased constant Sample_Array := (11, 22, 33, 44);

   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Constructors with Convention => C;
   procedure Test_Slices_And_References with Convention => C;
   procedure Test_From_Object_Free with Convention => C;

   --  Read a byte from the region returned by Get_Region. The data pointer is
   --  valid while the corresponding Gbytes reference remains alive.
   function Byte_At (Base : System.Address; Offset : Natural) return Guint8 is
   begin
      return Byte_Access.To_Pointer
        (Base + Storage_Offset (Offset)).all;
   end Byte_At;

   --  Check both the reported size and every byte, so constructors and slices
   --  are verified for their contents rather than only their non-null handles.
   procedure Assert_Data
     (Bytes    : Gbytes;
      Expected : Sample_Array;
      Length   : Natural := Sample_Array'Length)
   is
      Region : constant System.Address :=
        Get_Region (Bytes, 1, 0, Gsize (Length));
   begin
      Assert_True (Get_Size (Bytes) = Gsize (Length));
      Assert_Nonnull (Region);
      for Index in 0 .. Length - 1 loop
         Assert_True (Byte_At (Region, Index) = Expected (Index));
      end loop;
   end Assert_Data;

   --  Cover both copying constructor forms and both static, non-copying forms.
   procedure Test_Constructors is
      Via_Procedure : Gbytes;
      Via_Function  : Gbytes;
      Static_Proc   : Gbytes;
      Static_Func   : Gbytes;
   begin
      G_New (Via_Procedure, Static_Data, Static_Data'Length);
      Assert_Data (Via_Procedure, Static_Data);
      Via_Function := Gbytes_New (Static_Data, Static_Data'Length);
      Assert_Data (Via_Function, Static_Data);

      G_New_Static (Static_Proc, Static_Data, Static_Data'Length);
      Assert_Data (Static_Proc, Static_Data);
      Static_Func := Gbytes_New_Static (Static_Data, Static_Data'Length);
      Assert_Data (Static_Func, Static_Data);

      Unref (Via_Procedure);
      Unref (Via_Function);
      Unref (Static_Proc);
      Unref (Static_Func);
   end Test_Constructors;

   --  GLib owns and frees buffers passed to the take constructors. Allocate
   --  these with g_malloc and view the storage as a fixed-size Ada array.
   procedure Test_Take_Constructors with Convention => C;
   procedure Test_Take_Constructors is
      Via_Procedure : Gbytes;
      Via_Function  : Gbytes;
      Raw_Procedure : constant System.Address :=
        G_Malloc (Gsize (Sample_Array'Size / System.Storage_Unit));
      Raw_Function  : constant System.Address :=
        G_Malloc (Gsize (Sample_Array'Size / System.Storage_Unit));
      Data_Procedure : constant Sample_Access.Object_Pointer :=
        Sample_Access.To_Pointer (Raw_Procedure);
      Data_Function : constant Sample_Access.Object_Pointer :=
        Sample_Access.To_Pointer (Raw_Function);
   begin
      Data_Procedure.all := Static_Data;
      Data_Function.all := Static_Data;
      G_New_Take (Via_Procedure, Data_Procedure.all, Sample_Array'Length);
      Via_Function := Gbytes_New_Take (Data_Function.all, Sample_Array'Length);
      Assert_Data (Via_Procedure, Static_Data);
      Assert_Data (Via_Function, Static_Data);
      --  Each take constructor owns its GLib-allocated input buffer until its
      --  final reference is released.
      Unref (Via_Procedure);
      Unref (Via_Function);
   end Test_Take_Constructors;

   --  Check valid subregions, an out-of-range request, a copied slice, and the
   --  extra reference returned by Ref. Each returned Gbytes owns one ref.
   procedure Test_Slices_And_References is
      Original : constant Gbytes := Gbytes_New (Static_Data, Static_Data'Length);
      Slice    : constant Gbytes := New_From_Bytes (Original, 1, 2);
      Extra    : constant Gbytes := Ref (Original);
      Region   : System.Address;
      Slice_Data : constant Sample_Array := (22, 33, 0, 0);
   begin
      Region := Get_Region (Original, 2, 0, 2);
      Assert_Nonnull (Region);
      Assert_True (Byte_At (Region, 0) = 11);
      Assert_True (Byte_At (Region, 3) = 44);
      Assert_Null (Get_Region (Original, 1, 4, 1));
      Assert_Data (Slice, Slice_Data, 2);
      Assert_Data (Extra, Static_Data);

      Unref (Slice);
      Unref (Extra);
      Unref (Original);
   end Test_Slices_And_References;

   --  From_Object_Free consumes a temporary wrapper allocated with g_malloc,
   --  while the copied Gbytes value keeps the underlying bytes reference.
   procedure Test_From_Object_Free is
      Original : Gbytes := Gbytes_New (Static_Data, Static_Data'Length);
      Wrapper_Address : constant System.Address :=
        G_Malloc (Gsize (Gbytes'Size / System.Storage_Unit));
      Wrapper : constant Bytes_Access.Object_Pointer :=
        Bytes_Access.To_Pointer (Wrapper_Address);
      Converted : Gbytes;
   begin
      --  Move the sole reference into the temporary wrapper: C_Boxed values
      --  are handles, so copying one does not increment the GLib refcount.
      Wrapper.all := Original;
      Original.Set_Object (System.Null_Address);
      Converted := From_Object_Free (Wrapper);
      Assert_Data (Converted, Static_Data);
      Unref (Converted);
   end Test_From_Object_Free;

begin
   Glib.Test.Init;
   Add_Func ("/glib-bytes/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/glib-bytes/take-constructors",
     Test_Take_Constructors'Unrestricted_Access);
   Add_Func ("/glib-bytes/slices-and-references",
     Test_Slices_And_References'Unrestricted_Access);
   Add_Func ("/glib-bytes/from-object-free",
     Test_From_Object_Free'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
