--  Glib.Convert tests for character-set, locale, filename, URI, and markup
--  conversions. No matching GLib C tests are present in testsuite/c_tests.
--
--  The tests use fixed ASCII paths and explicit UTF-8/ISO-8859-1 inputs so
--  their successful conversion checks do not depend on files in the checkout.

with Ada.Command_Line;

with Glib.Convert; use Glib.Convert;
with Glib.Error;
with Glib.Test;    use Glib.Test;
with Gtkada.Types;

procedure Main is

   use type Glib.Error.GError;
   use type Gtkada.Types.Chars_Ptr;

   Latin1_Text : constant String := "caf" & Character'Val (16#E9#);
   UTF8_Text   : constant String :=
     "caf" & Character'Val (16#C3#) & Character'Val (16#A9#);
   Invalid_Byte : constant String := String'(1 => Character'Val (16#FF#));
   UTF8_Byte    : constant String :=
     Character'Val (16#C3#) & Character'Val (16#BF#);

   procedure Test_Convert
   with Convention => C;

   procedure Test_Filenames
   with Convention => C;

   procedure Test_Locale
   with Convention => C;

   procedure Test_Escape_Text
   with Convention => C;

   -------------------
   -- Test_Convert --
   -------------------

   procedure Test_Convert is
      Bytes_Read    : aliased Natural := 0;
      Bytes_Written : aliased Natural := 0;
      Raw_Result    : Gtkada.Types.Chars_Ptr;
      Error         : aliased Glib.Error.GError := null;
   begin
      --  Exercise the output-buffer overload and check both reported lengths.
      declare
         Result : String (1 .. 8);
      begin
         Convert
           (Latin1_Text, "UTF-8", "ISO-8859-1", Bytes_Read, Bytes_Written,
            Result => Result);
         Assert_True (Bytes_Read = Latin1_Text'Length);
         Assert_True (Bytes_Written = UTF8_Text'Length);
         Assert_Cmpstr_Eq (Result (1 .. Bytes_Written), UTF8_Text);
      end;

      --  The direct String overload must produce the same UTF-8 byte sequence.
      Assert_Cmpstr_Eq
        (Convert (UTF8_Text, "ISO-8859-1", "UTF-8"), Latin1_Text);

      --  The raw-pointer overload transfers ownership to the caller; copy
      --  its contents before freeing the GLib allocation.
      Raw_Result := Convert
        (Latin1_Text, "UTF-8", "ISO-8859-1", Bytes_Read'Access,
         Bytes_Written'Access);
      Assert_True (Raw_Result /= Gtkada.Types.Null_Ptr);
      Assert_True (Bytes_Read = Latin1_Text'Length);
      Assert_True (Bytes_Written = UTF8_Text'Length);
      Assert_Cmpstr_Eq (Gtkada.Types.Value (Raw_Result), UTF8_Text);
      Gtkada.Types.Free (Raw_Result);

      --  The Latin-1 e-acute is valid input but cannot be represented in
      --  ASCII, so both error paths fail for a real unrepresentable character.
      Assert_Cmpstr_Eq (Convert (Latin1_Text, "ASCII", "ISO-8859-1"), "");
      Raw_Result := Convert
        (Latin1_Text, "ASCII", "ISO-8859-1", Bytes_Read'Access,
         Bytes_Written'Access, Error'Access);
      Assert_True (Raw_Result = Gtkada.Types.Null_Ptr);
      Assert_True (Error /= null);
      Assert_True
        (Glib.Error.Error_Matches
           (Error, Convert_Error_Domain, Illegal_Sequence));
      Glib.Error.Error_Free (Error);
   end Test_Convert;

   ---------------------
   -- Test_Filenames --
   ---------------------

   procedure Test_Filenames is
      Filename : constant String := "/tmp/gtkada convert sample";
      URI      : constant String := Filename_To_URI (Filename);
      Hostname : aliased Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
   begin
      --  Filename conversions preserve ASCII filenames on this test host.
      Assert_Cmpstr_Eq (Filename_To_UTF8 (Filename), Filename);
      Assert_Cmpstr_Eq (Filename_From_UTF8 (Filename), Filename);

      --  Invalid filename bytes are rejected in UTF-8 filename locales. Some
      --  platforms accept FF as a native filename byte, in which case GLib
      --  returns its UTF-8 encoding instead.
      declare
         Error     : aliased Glib.Error.GError := null;
         Converted : constant String :=
           Filename_To_UTF8 (Invalid_Byte, Error'Access);
      begin
         if Error = null then
            Assert_Cmpstr_Eq (Converted, UTF8_Byte);
         else
            Assert_Cmpstr_Eq (Converted, "");
            Glib.Error.Error_Free (Error);
         end if;
      end;

      --  URI conversion escapes spaces and round-trips an absolute local path.
      Assert_Cmpstr_Eq
        (URI, "file:///tmp/gtkada%20convert%20sample");
      Assert_Cmpstr_Eq
        (Filename_From_URI (URI, Hostname'Access), Filename);
      Assert_True (Hostname = Gtkada.Types.Null_Ptr);

      --  A non-empty hostname uses the alternate g_filename_to_uri argument
      --  path and is retained in the returned file URI.
      Assert_Cmpstr_Eq
        (Filename_To_URI (Filename, "localhost"),
         "file://localhost/tmp/gtkada%20convert%20sample");
   end Test_Filenames;

   ------------------
   -- Test_Locale --
   ------------------

   procedure Test_Locale is
      Bytes_Read    : aliased Natural := 0;
      Bytes_Written : aliased Natural := 0;
      Raw_Result    : Gtkada.Types.Chars_Ptr;
      Error         : aliased Glib.Error.GError := null;
   begin
      --  ASCII is valid in every locale encoding, so these round trips are
      --  independent of the machine's selected codeset.
      declare
         Result : String (1 .. 8);
      begin
         Locale_To_UTF8
           ("plain", Bytes_Read, Bytes_Written, Result => Result);
         Assert_True (Bytes_Read = 5 and Bytes_Written = 5);
         Assert_Cmpstr_Eq (Result (1 .. Bytes_Written), "plain");

         Locale_From_UTF8
           ("plain", Bytes_Read, Bytes_Written, Result => Result);
         Assert_True (Bytes_Read = 5 and Bytes_Written = 5);
         Assert_Cmpstr_Eq (Result (1 .. Bytes_Written), "plain");
      end;

      --  Check both raw-pointer overloads and release their returned buffers.
      Raw_Result := Locale_To_UTF8
        ("plain", Bytes_Read'Access, Bytes_Written'Access);
      Assert_True (Raw_Result /= Gtkada.Types.Null_Ptr);
      Assert_True (Bytes_Read = 5 and Bytes_Written = 5);
      Assert_Cmpstr_Eq (Gtkada.Types.Value (Raw_Result), "plain");
      Gtkada.Types.Free (Raw_Result);

      Raw_Result := Locale_From_UTF8
        ("plain", Bytes_Read'Access, Bytes_Written'Access);
      Assert_True (Raw_Result /= Gtkada.Types.Null_Ptr);
      Assert_True (Bytes_Read = 5 and Bytes_Written = 5);
      Assert_Cmpstr_Eq (Gtkada.Types.Value (Raw_Result), "plain");
      Gtkada.Types.Free (Raw_Result);

      --  The simple overloads also return only the converted String.
      Assert_Cmpstr_Eq (Locale_To_UTF8 ("plain"), "plain");
      Assert_Cmpstr_Eq (Locale_From_UTF8 ("plain"), "plain");

      --  UTF-8 input validation is independent of the current locale. The
      --  simple overload returns an empty string on malformed UTF-8.
      Assert_Cmpstr_Eq (Locale_From_UTF8 (Invalid_Byte), "");

      --  The locale's source encoding is platform-dependent. Exercise and
      --  verify either its rejection path (with a GError) or its valid
      --  conversion result for byte FF when the locale accepts that byte.
      Raw_Result := Locale_To_UTF8
        (Invalid_Byte, Bytes_Read'Access, Bytes_Written'Access, Error'Access);
      if Raw_Result = Gtkada.Types.Null_Ptr then
         Assert_True (Error /= null);
         Glib.Error.Error_Free (Error);
         Error := null;

         --  Exercise both convenience overloads' empty-result error paths.
         Assert_Cmpstr_Eq (Locale_To_UTF8 (Invalid_Byte), "");
         declare
            Procedure_Error : aliased Glib.Error.GError := null;
            Result          : String (1 .. 8);
         begin
            Locale_To_UTF8
              (Invalid_Byte, Bytes_Read, Bytes_Written,
               Procedure_Error'Access, Result);
            Assert_True (Procedure_Error /= null);
            Glib.Error.Error_Free (Procedure_Error);
         end;
      else
         Assert_True (Error = null);
         Assert_Cmpstr_Eq (Gtkada.Types.Value (Raw_Result), UTF8_Byte);
         Assert_Cmpstr_Eq (Locale_To_UTF8 (Invalid_Byte), UTF8_Byte);
         Gtkada.Types.Free (Raw_Result);
      end if;
   end Test_Locale;

   -----------------------
   -- Test_Escape_Text --
   -----------------------

   procedure Test_Escape_Text is
   begin
      --  Markup metacharacters become entities, while ordinary text is kept.
      Assert_Cmpstr_Eq (Escape_Text ("<tag>&"), "&lt;tag&gt;&amp;");
      Assert_Cmpstr_Eq (Escape_Text ("plain text"), "plain text");
      Assert_Cmpstr_Eq (Escape_Text (""), "");
   end Test_Escape_Text;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-convert/convert", Test_Convert'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-convert/filenames", Test_Filenames'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-convert/locale", Test_Locale'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-convert/escape-text", Test_Escape_Text'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
