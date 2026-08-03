--  Ginput_Stream tests: reading from the stream returned by
--  Glib.Resource.Open_Stream.
--
--  The binary fixture sample.gresource is generated from sample.gresource.xml,
--  alpha.txt and beta.txt. Regenerate it from testsuite/tests/ginputstream
--  with:
--    glib-compile-resources sample.gresource.xml --target=sample.gresource
--      --sourcedir=.

with Ada.Command_Line;

with Glib;              use Glib;
with Glib.Input_Stream; use Glib.Input_Stream;
with Glib.Object;
with Glib.Resource;     use Glib.Resource;
with Glib.Test;         use Glib.Test;

procedure Main is

   Alpha_Text : constant UTF8_String := "alpha resource" & ASCII.LF;
   Beta_Text  : constant UTF8_String := "beta value" & ASCII.LF;

   function Open (Path : UTF8_String) return Ginput_Stream;
   --  Open Path in sample.gresource. The resource itself is released at once:
   --  the stream keeps it alive for as long as it needs it.

   function To_String (Buffer : Guint8_Array) return String;

   ----------
   -- Open --
   ----------

   function Open (Path : UTF8_String) return Ginput_Stream is
      Resource : Gresource := Load ("sample.gresource");
      Stream   : constant Ginput_Stream := Open_Stream
        (Self         => Resource,
         Path         => Path,
         Lookup_Flags => G_Resource_Lookup_Flags_None);
   begin
      Assert_True (Resource /= Null_Gresource);
      Unref (Resource);

      return Stream;
   end Open;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buffer : Guint8_Array) return String is
      Result : String (1 .. Buffer'Length);
      Index  : Natural := Result'First;
   begin
      for B of Buffer loop
         Result (Index) := Character'Val (Integer (B));
         Index := Index + 1;
      end loop;

      return Result;
   end To_String;

   procedure Test_Read
   with Convention => C;

   procedure Test_Read_Slice
   with Convention => C;

   procedure Test_Read_All
   with Convention => C;

   procedure Test_Close
   with Convention => C;

   ---------------
   -- Test_Read --
   ---------------

   procedure Test_Read is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Buffer : Guint8_Array (1 .. 64) := (others => 0);
      Read_Count : Gssize;
   begin
      Assert_True (Stream /= null);

      Read_Count := Read
        (Self        => Stream,
         Buffer      => Buffer,
         Cancellable => null);

      Assert_Cmpint_Eq (Gint (Read_Count), Gint (Alpha_Text'Length));
      Assert_Cmpstr_Eq
        (To_String (Buffer (1 .. Natural (Read_Count))), Alpha_Text);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read;

   ---------------------
   -- Test_Read_Slice --
   ---------------------

   procedure Test_Read_Slice is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
      Buffer : Guint8_Array (1 .. 64) := (others => 0);
      Read_Count : Gssize;
   begin
      Assert_True (Stream /= null);

      --  "Count" is not part of the Ada profile: a caller who wants to read
      --  less passes a slice, whose 'Length is handed to C.

      Read_Count := Read
        (Self        => Stream,
         Buffer      => Buffer (1 .. 5),
         Cancellable => null);

      Assert_Cmpint_Eq (Gint (Read_Count), 5);
      Assert_Cmpstr_Eq (To_String (Buffer (1 .. 5)), Alpha_Text (1 .. 5));

      --  Nothing was written past the slice.

      Assert_True (Buffer (6) = 0);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read_Slice;

   -------------------
   -- Test_Read_All --
   -------------------

   procedure Test_Read_All is
      Stream     : constant Ginput_Stream := Open ("/org/gtkada/test/beta.txt");
      Buffer     : Guint8_Array (1 .. 64) := (others => 0);
      Bytes_Read : Gsize;
   begin
      Assert_True (Stream /= null);

      Assert_True
        (Read_All
           (Self        => Stream,
            Buffer      => Buffer,
            Bytes_Read  => Bytes_Read,
            Cancellable => null));

      Assert_Cmpint_Eq (Gint (Bytes_Read), Gint (Beta_Text'Length));
      Assert_Cmpstr_Eq
        (To_String (Buffer (1 .. Natural (Bytes_Read))), Beta_Text);

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Read_All;

   ----------------
   -- Test_Close --
   ----------------

   procedure Test_Close is
      Stream : constant Ginput_Stream := Open ("/org/gtkada/test/alpha.txt");
   begin
      Assert_True (Stream /= null);
      Assert_False (Is_Closed (Stream));

      Assert_True (Close (Self => Stream, Cancellable => null));
      Assert_True (Is_Closed (Stream));

      Glib.Object.Unref (Glib.Object.GObject (Stream));
   end Test_Close;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func ("/ginputstream/read", Test_Read'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/read-slice", Test_Read_Slice'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/ginputstream/read-all", Test_Read_All'Unrestricted_Access);
   Glib.Test.Add_Func ("/ginputstream/close", Test_Close'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
