--  Goutput_Stream tests: writing to a GMemoryOutputStream.
--
--  GMemoryOutputStream is not bound, so the concrete stream and the
--  inspection of what was written to it are imported here directly.
--
--  The binary fixture sample.gresource is generated from sample.gresource.xml,
--  alpha.txt and beta.txt. Regenerate it from testsuite/tests/goutputstream
--  with:
--    glib-compile-resources sample.gresource.xml --target=sample.gresource
--      --sourcedir=.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;               use Glib;
with Glib.Input_Stream;  use Glib.Input_Stream;
with Glib.Object;
with Glib.Output_Stream; use Glib.Output_Stream;
with Glib.Resource;      use Glib.Resource;
with Glib.Test;          use Glib.Test;

procedure Main is

   Alpha_Text : constant UTF8_String := "alpha resource" & ASCII.LF;

   function New_Memory_Stream return Goutput_Stream;
   --  A resizable GMemoryOutputStream, which grows as it is written to.

   function Contents (Stream : Goutput_Stream) return String;
   --  Whatever has been written to the memory stream so far.

   function Open_Alpha return Ginput_Stream;
   --  alpha.txt, out of sample.gresource.

   function To_Buffer (Text : String) return Guint8_Array;

   -----------------------
   -- New_Memory_Stream --
   -----------------------

   function New_Memory_Stream return Goutput_Stream is
      function Internal return System.Address;
      pragma Import (C, Internal, "g_memory_output_stream_new_resizable");

      Stub : Goutput_Stream_Record;
   begin
      return Goutput_Stream
        (Glib.Object.Get_User_Data (Internal, Stub));
   end New_Memory_Stream;

   --------------
   -- Contents --
   --------------

   function Contents (Stream : Goutput_Stream) return String is
      function Get_Data (Self : System.Address) return System.Address;
      pragma Import (C, Get_Data, "g_memory_output_stream_get_data");

      function Get_Data_Size (Self : System.Address) return Gsize;
      pragma Import (C, Get_Data_Size, "g_memory_output_stream_get_data_size");

      Object : constant System.Address :=
        Glib.Object.Get_Object (Glib.Object.GObject (Stream));
      Size   : constant Natural := Natural (Get_Data_Size (Object));

      subtype Data is Guchar_Array (1 .. Size);
      type Data_Access is access all Data;
      function To_Data is new Ada.Unchecked_Conversion
        (System.Address, Data_Access);

      Bytes : constant Data_Access := To_Data (Get_Data (Object));

      Result : String (1 .. Size);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Integer (Bytes (I)));
      end loop;

      return Result;
   end Contents;

   ----------------
   -- Open_Alpha --
   ----------------

   function Open_Alpha return Ginput_Stream is
      Resource : Gresource := Load ("sample.gresource");
      Stream   : constant Ginput_Stream := Open_Stream
        (Self         => Resource,
         Path         => "/org/gtkada/test/alpha.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None);
   begin
      Assert_True (Resource /= Null_Gresource);
      Unref (Resource);

      return Stream;
   end Open_Alpha;

   ---------------
   -- To_Buffer --
   ---------------

   function To_Buffer (Text : String) return Guint8_Array is
      Result : Guint8_Array (1 .. Text'Length);
      Index  : Natural := Result'First;
   begin
      for C of Text loop
         Result (Index) := Character'Pos (C);
         Index := Index + 1;
      end loop;

      return Result;
   end To_Buffer;

   procedure Test_Write
   with Convention => C;

   procedure Test_Write_Slice
   with Convention => C;

   procedure Test_Write_All
   with Convention => C;

   procedure Test_Splice
   with Convention => C;

   ----------------
   -- Test_Write --
   ----------------

   procedure Test_Write is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer ("hello");
      Written : Gssize;
   begin
      Assert_True (Stream /= null);

      Written := Write
        (Self        => Stream,
         Buffer      => Buffer,
         Cancellable => null);

      Assert_Cmpint_Eq (Gint (Written), 5);
      Assert_Cmpstr_Eq (Contents (Stream), "hello");

      Assert_True (Close (Self => Stream, Cancellable => null));
      Assert_True (Is_Closed (Stream));

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write;

   ----------------------
   -- Test_Write_Slice --
   ----------------------

   procedure Test_Write_Slice is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer ("hello");
      Written : Gssize;
   begin
      Assert_True (Stream /= null);

      --  "Count" is not part of the Ada profile: a caller who wants to write
      --  less passes a slice, whose 'Length is handed to C.

      Written := Write
        (Self        => Stream,
         Buffer      => Buffer (1 .. 3),
         Cancellable => null);

      Assert_Cmpint_Eq (Gint (Written), 3);
      Assert_Cmpstr_Eq (Contents (Stream), "hel");

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write_Slice;

   --------------------
   -- Test_Write_All --
   --------------------

   procedure Test_Write_All is
      Stream  : constant Goutput_Stream := New_Memory_Stream;
      Buffer  : constant Guint8_Array := To_Buffer (Alpha_Text);
      Written : aliased Gsize := 0;
   begin
      Assert_True (Stream /= null);

      Assert_True
        (Write_All
           (Self          => Stream,
            Buffer        => Buffer,
            Bytes_Written => Written'Access,
            Cancellable   => null));

      Assert_Cmpint_Eq (Gint (Written), Gint (Alpha_Text'Length));
      Assert_Cmpstr_Eq (Contents (Stream), Alpha_Text);

      --  Bytes_Written is optional: C accepts a NULL there.

      Assert_True
        (Write_All
           (Self          => Stream,
            Buffer        => Buffer,
            Bytes_Written => null,
            Cancellable   => null));

      Assert_Cmpstr_Eq (Contents (Stream), Alpha_Text & Alpha_Text);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Write_All;

   -----------------
   -- Test_Splice --
   -----------------

   procedure Test_Splice is
      Source : constant Ginput_Stream := Open_Alpha;
      Target : constant Goutput_Stream := New_Memory_Stream;
      Spliced : Gssize;
   begin
      Assert_True (Source /= null);
      Assert_True (Target /= null);

      Spliced := Splice
        (Self        => Target,
         Source      => Source,
         Flags       => G_Output_Stream_Splice_Close_Source,
         Cancellable => null);

      Assert_Cmpint_Eq (Gint (Spliced), Gint (Alpha_Text'Length));
      Assert_Cmpstr_Eq (Contents (Target), Alpha_Text);
      Assert_True (Is_Closed (Source));

      Glib.Object.Unref (Glib.Object.GObject (Target));
      Glib.Object.Unref (Glib.Object.GObject (Source));
   end Test_Splice;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/goutputstream/write", Test_Write'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/write-slice", Test_Write_Slice'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/write-all", Test_Write_All'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/goutputstream/splice", Test_Splice'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
