--  Gresource tests for loading, lookup, enumeration and registration.
--
--  The binary fixture sample.gresource is generated from sample.gresource.xml,
--  alpha.txt and beta.txt. Regenerate it from testsuite/tests/gresource with:
--    glib-compile-resources sample.gresource.xml --target=sample.gresource
--      --sourcedir=.
--
--  invalid.gresource is a plain-text fixture (not a valid gresource bundle)
--  used to exercise the "valid file, invalid content" GError failure path.

with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with System;

with GNAT.Strings; use GNAT.Strings;

with Glib;              use Glib;
with Glib.Bytes;        use Glib.Bytes;
with Glib.Error;
with Glib.Error_Enums;  use Glib.Error_Enums;
with Glib.Input_Stream; use Glib.Input_Stream;
with Glib.Object;
with Glib.Resource;     use Glib.Resource;
with Glib.Test;         use Glib.Test;

procedure Main is

   Alpha_Text : constant UTF8_String := "alpha resource" & ASCII.LF;
   Beta_Text  : constant UTF8_String := "beta value" & ASCII.LF;

   function Read_File_As_Gbytes (Filename : String) return Gbytes;

   function Garbage_Bytes return Gbytes;
   --  Bytes that are not a valid gresource bundle, built in memory - no
   --  file I/O, so this exercises only GResource's own bundle-format
   --  validation, not Gbytes/file-reading machinery.

   function Has_Entry
     (Children : String_List;
      Name     : String) return Boolean;

   procedure Free_String_List (Children : in out String_List);

   -------------------------
   -- Read_File_As_Gbytes --
   -------------------------

   function Read_File_As_Gbytes (Filename : String) return Gbytes is
      package SIO renames Ada.Streams.Stream_IO;
      use Ada.Streams;

      File_Handle : SIO.File_Type;
      File_Size   : Stream_Element_Offset;
      function Internal_New
        (Data : System.Address;
         Size : Gsize) return System.Address;
      pragma Import (C, Internal_New, "g_bytes_new");
   begin
      SIO.Open (File_Handle, SIO.In_File, Filename);
      File_Size := Stream_Element_Offset (SIO.Size (File_Handle));

      declare
         Buffer : Stream_Element_Array (1 .. File_Size);
         Last   : Stream_Element_Offset;
      begin
         SIO.Read (File_Handle, Buffer, Last);
         SIO.Close (File_Handle);

         return From_Object (Internal_New (Buffer (Buffer'First)'Address, Gsize (Last)));
      end;
   end Read_File_As_Gbytes;

   -------------------
   -- Garbage_Bytes --
   -------------------

   function Garbage_Bytes return Gbytes is
      Data   : constant Guint8_Array (0 .. 15) := (others => 16#AA#);
      Result : Gbytes;
   begin
      --  G_New/g_bytes_new copies Data into a fresh g_malloc'd buffer, which
      --  is always pointer-aligned regardless of this stack array's own
      --  alignment - unlike a zero-copy constructor (G_New_Static/_Take),
      --  this is safe for G(_Resource)_New_From_Data's documented alignment
      --  requirement.
      G_New (Result, Data, Data'Length);
      return Result;
   end Garbage_Bytes;

   ---------------
   -- Has_Entry --
   ---------------

   function Has_Entry
     (Children : String_List;
      Name     : String) return Boolean
   is
   begin
      for I in Children'Range loop
         if Children (I).all = Name then
            return True;
         end if;
      end loop;

      return False;
   end Has_Entry;

   ----------------------
   -- Free_String_List --
   ----------------------

   procedure Free_String_List (Children : in out String_List) is
   begin
      for I in Children'Range loop
         Free (Children (I));
      end loop;
   end Free_String_List;

   procedure Test_Load_And_Get_Info
   with Convention => C;

   procedure Test_Enumerate_Children
   with Convention => C;

   procedure Test_Lookup_Data
   with Convention => C;

   procedure Test_Register_Unregister
   with Convention => C;

   procedure Test_New_From_Data
   with Convention => C;

   procedure Test_G_New_From_Data
   with Convention => C;

   procedure Test_G_New_From_Data_Invalid_Content
   with Convention => C;

   procedure Test_New_From_Data_Invalid_Content
   with Convention => C;

   procedure Test_Open_Stream
   with Convention => C;

   procedure Test_Open_Stream_Nonexistent_Path
   with Convention => C;

   procedure Test_Lookup_Nonexistent_Path
   with Convention => C;

   procedure Test_Global_Lookup_Data
   with Convention => C;

   procedure Test_Global_Lookup_Data_Nonexistent_Path
   with Convention => C;

   procedure Test_Load_Nonexistent_File
   with Convention => C;

   procedure Test_Load_Invalid_Content
   with Convention => C;

   procedure Test_Get_Info_Nonexistent_Path
   with Convention => C;

   procedure Test_Enumerate_Children_Nonexistent_Path
   with Convention => C;

   ----------------------------
   -- Test_Load_And_Get_Info --
   ----------------------------

   procedure Test_Load_And_Get_Info is
      Error    : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Error);
      Size     : aliased Gsize;
      Flags    : aliased Guint32;
      Found    : Boolean;

      use type Glib.Error.GError;
   begin
      Assert_True (Resource /= Null_Gresource);

      Found := Get_Info
        (Self         => Resource,
         Path         => "/org/gtkada/test/alpha.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Size         => Size'Access,
         Flags        => Flags'Access,
         Error        => Error);

      Assert_True (Error = null);
      Assert_True (Found);
      Assert_Cmpint_Eq (Gint (Size), Gint (Alpha_Text'Length));
      Assert_True (Flags = 0);

      --  Both outputs are optional: C accepts a NULL for either.

      Found := Get_Info
        (Self         => Resource,
         Path         => "/org/gtkada/test/alpha.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Size         => null,
         Flags        => null,
         Error        => Error);

      Assert_True (Found);
      Assert_True (Error = null);

      Unref (Resource);
   end Test_Load_And_Get_Info;

   -----------------------------
   -- Test_Enumerate_Children --
   -----------------------------

   procedure Test_Enumerate_Children is
      Error    : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Error);
      Children : String_List := Enumerate_Children
        (Self         => Resource,
         Path         => "/org/gtkada/test",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Error);
      use type Glib.Error.GError;
   begin
      Assert_True (Error = null);
      Assert_True (Resource /= Null_Gresource);
      Assert_Cmpint_Eq (Gint (Children'Length), 2);
      Assert_True (Has_Entry (Children, "alpha.txt"));
      Assert_True (Has_Entry (Children, "beta.txt"));

      Free_String_List (Children);
      Unref (Resource);
   end Test_Enumerate_Children;

   ----------------------
   -- Test_Lookup_Data --
   ----------------------

   procedure Test_Lookup_Data is
      Load_E, Lookup_E    : Glib.Error.GError;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Bytes    : Gbytes := Lookup_Data
        (Self         => Resource,
         Path         => "/org/gtkada/test/beta.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Lookup_E);
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);
      Assert_True (Lookup_E = null);
      Assert_True (Resource /= Null_Gresource);
      Assert_True (Bytes /= Null_Gbytes);
      Assert_Cmpint_Eq (Gint (Get_Size (Bytes)), Gint (Beta_Text'Length));

      Unref (Bytes);
      Unref (Resource);
   end Test_Lookup_Data;

   ------------------------------
   -- Test_Register_Unregister --
   ------------------------------

   procedure Test_Register_Unregister is
      Error    : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Error);
      use type Glib.Error.GError;
   begin
      Assert_True (Error = null);
      Assert_True (Resource /= Null_Gresource);
      Register (Resource);
      Unregister (Resource);
      Unref (Resource);
   end Test_Register_Unregister;

   ------------------------
   -- Test_New_From_Data --
   ------------------------

   procedure Test_New_From_Data is
      Error    : Glib.Error.GError := null;
      Bundle   : Gbytes := Read_File_As_Gbytes ("sample.gresource");
      Resource : Gresource := Gresource_New_From_Data (Bundle, Error);
      Size     : aliased Gsize;
      Found    : Boolean;
      use type Glib.Error.GError;
   begin
      Assert_True (Error = null);
      Assert_True (Bundle /= Null_Gbytes);
      Assert_True (Resource /= Null_Gresource);

      --  Flags is declined altogether, which the C contract allows.

      Found := Get_Info
        (Self         => Resource,
         Path         => "/org/gtkada/test/beta.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Size         => Size'Access,
         Flags        => null,
         Error        => Error);

      Assert_True (Error = null);
      Assert_True (Found);
      Assert_Cmpint_Eq (Gint (Size), Gint (Beta_Text'Length));

      Unref (Resource);
      Unref (Bundle);
   end Test_New_From_Data;

   --------------------------
   -- Test_G_New_From_Data --
   --------------------------

   procedure Test_G_New_From_Data is
      Bundle   : Gbytes := Read_File_As_Gbytes ("sample.gresource");
      Error    : Glib.Error.GError;
      Resource : Gresource;
      use type Glib.Error.GError;
   begin
      G_New_From_Data (Resource, Bundle, Error);

      Assert_True (Error = null);
      Assert_True (Resource /= Null_Gresource);

      Unref (Resource);
      Unref (Bundle);
   end Test_G_New_From_Data;

   -------------------------------------------
   -- Test_G_New_From_Data_Invalid_Content --
   -------------------------------------------

   procedure Test_G_New_From_Data_Invalid_Content is
      Bundle   : Gbytes := Garbage_Bytes;
      Error    : Glib.Error.GError;
      Resource : Gresource;
      use type Glib.Error.GError;
   begin
      G_New_From_Data (Resource, Bundle, Error);

      Assert_True (Resource = Null_Gresource);
      Assert_True (Error /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Error)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Error),
         Gint (GResource_Error'Pos (G_Resource_Error_Internal)));

      Glib.Error.Error_Free (Error);
      Unref (Bundle);
   end Test_G_New_From_Data_Invalid_Content;

   --------------------------------------
   -- Test_New_From_Data_Invalid_Content --
   --------------------------------------

   procedure Test_New_From_Data_Invalid_Content is
      Bundle   : Gbytes := Garbage_Bytes;
      Error    : Glib.Error.GError;
      Resource : Gresource;
      use type Glib.Error.GError;
   begin
      Resource := Gresource_New_From_Data (Bundle, Error);

      Assert_True (Resource = Null_Gresource);
      Assert_True (Error /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Error)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Error),
         Gint (GResource_Error'Pos (G_Resource_Error_Internal)));

      Glib.Error.Error_Free (Error);
      Unref (Bundle);
   end Test_New_From_Data_Invalid_Content;

   -----------------------
   -- Test_Open_Stream --
   -----------------------

   procedure Test_Open_Stream is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Stream_E : Glib.Error.GError;
      Stream   : constant Ginput_Stream := Open_Stream
        (Self         => Resource,
         Path         => "/org/gtkada/test/alpha.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Stream_E);
      Buffer   : Guint8_Array (1 .. 64) := (others => 0);
      Read_E   : Glib.Error.GError;
      Count    : Gssize;
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);
      Assert_True (Stream_E = null);
      Assert_True (Stream /= null);

      --  Prove the stream is actually usable, not merely non-null.

      Count := Glib.Input_Stream.Read
        (Self        => Stream,
         Buffer      => Buffer,
         Cancellable => null,
         Error       => Read_E);

      Assert_True (Read_E = null);
      Assert_Cmpint_Eq (Gint (Count), Gint (Alpha_Text'Length));

      Glib.Object.Unref (Glib.Object.GObject (Stream));
      Unref (Resource);
   end Test_Open_Stream;

   -----------------------------------------
   -- Test_Open_Stream_Nonexistent_Path --
   -----------------------------------------

   procedure Test_Open_Stream_Nonexistent_Path is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Stream_E : Glib.Error.GError;
      Stream   : Ginput_Stream;
      Exp_Err  : constant Glib.Gint :=
        Glib.Gint (GResource_Error'Pos (G_Resource_Error_Not_Found));
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);

      Stream :=
        Open_Stream
          (Self         => Resource,
           Path         => "/org/gtkada/test/does-not-exist.txt",
           Lookup_Flags => G_Resource_Lookup_Flags_None,
           Error        => Stream_E);

      Assert_True (Stream = null);
      Assert_True (Stream_E /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Stream_E)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq (Glib.Error.Get_Code (Stream_E), Exp_Err);
      Assert
        (Glib.Error_Enums.GResource_Error_Matches
           (Stream_E, G_Resource_Error_Not_Found));
      Assert_Error
        (Error  => Stream_E,
         Domain => Glib.Error_Enums.GResource_Error_Domain,
         Code   => Exp_Err);

      Glib.Error.Error_Free (Stream_E);
      Unref (Resource);
   end Test_Open_Stream_Nonexistent_Path;

   -------------------------------------
   -- Test_Lookup_Nonexistent_Path --
   -------------------------------------

   procedure Test_Lookup_Nonexistent_Path is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Lookup_E : Glib.Error.GError;
      Bytes    : Gbytes;
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);

      Bytes := Lookup_Data
        (Self         => Resource,
         Path         => "/org/gtkada/test/does-not-exist.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Lookup_E);

      Assert_True (Bytes = Null_Gbytes);
      Assert_True (Lookup_E /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Lookup_E)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Lookup_E),
         Gint (GResource_Error'Pos (G_Resource_Error_Not_Found)));

      Glib.Error.Error_Free (Lookup_E);
      Unref (Resource);
   end Test_Lookup_Nonexistent_Path;

   -----------------------------
   -- Test_Global_Lookup_Data --
   -----------------------------

   --  g_resources_lookup_data - the global lookup across ALL registered
   --  resources, as opposed to Lookup_Data's Self-taking instance method
   --  (g_resource_lookup_data) tested above. Overloaded onto the same Ada
   --  name; disambiguated here by omitting the Self association.

   procedure Test_Global_Lookup_Data is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Lookup_E : Glib.Error.GError;
      Bytes    : Gbytes;
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);
      Register (Resource);

      Bytes := Resource.Lookup_Data
        (Path         => "/org/gtkada/test/beta.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Lookup_E);

      Assert_True (Lookup_E = null);
      Assert_True (Bytes /= Null_Gbytes);
      Assert_Cmpint_Eq (Gint (Get_Size (Bytes)), Gint (Beta_Text'Length));

      Unref (Bytes);
      Unregister (Resource);
      Unref (Resource);
   end Test_Global_Lookup_Data;

   ----------------------------------------------
   -- Test_Global_Lookup_Data_Nonexistent_Path --
   ----------------------------------------------

   procedure Test_Global_Lookup_Data_Nonexistent_Path is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Lookup_E : Glib.Error.GError;
      Bytes    : Gbytes;
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);
      Register (Resource);

      Bytes := Resource.Lookup_Data
        (Path         => "/org/gtkada/test/does-not-exist.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Lookup_E);

      Assert_True (Bytes = Null_Gbytes);
      Assert_True (Lookup_E /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Lookup_E)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Lookup_E),
         Gint (GResource_Error'Pos (G_Resource_Error_Not_Found)));

      Glib.Error.Error_Free (Lookup_E);
      Unregister (Resource);
      Unref (Resource);
   end Test_Global_Lookup_Data_Nonexistent_Path;

   --------------------------------
   -- Test_Load_Nonexistent_File --
   --------------------------------

   procedure Test_Load_Nonexistent_File is
      Error    : Glib.Error.GError;
      Resource : Gresource;
      use type Glib.Error.GError;
   begin
      Resource := Load ("/no/such/file-gtkada-gresource-test.gresource", Error);

      Assert_True (Resource = Null_Gresource);
      Assert_True (Error /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Error)),
         Glib.Guint (Glib.Error_Enums.GFile_Error_Domain));

      --  Deliberately no Get_Code assertion here. GFileError's own docs:
      --  "It's not very portable to make detailed assumptions about exactly
      --  which errors will be returned from a given operation. Some errors
      --  don't occur on some systems, etc." - unlike GResource_Error (its
      --  own enum, not errno-derived), the specific code here isn't
      --  guaranteed stable across platforms.

      Glib.Error.Error_Free (Error);
   end Test_Load_Nonexistent_File;

   -------------------------------
   -- Test_Load_Invalid_Content --
   -------------------------------

   procedure Test_Load_Invalid_Content is
      Error    : Glib.Error.GError;
      Resource : Gresource;
      use type Glib.Error.GError;
   begin
      Resource := Load ("invalid.gresource", Error);

      Assert_True (Resource = Null_Gresource);
      Assert_True (Error /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Error)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Error),
         Gint (GResource_Error'Pos (G_Resource_Error_Internal)));

      Glib.Error.Error_Free (Error);
   end Test_Load_Invalid_Content;

   -----------------------------------
   -- Test_Get_Info_Nonexistent_Path --
   -----------------------------------

   procedure Test_Get_Info_Nonexistent_Path is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Info_E   : Glib.Error.GError;
      Found    : Boolean;
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);

      Found := Get_Info
        (Self         => Resource,
         Path         => "/org/gtkada/test/does-not-exist.txt",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Info_E);

      Assert_False (Found);
      Assert_True (Info_E /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Info_E)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Info_E),
         Gint (GResource_Error'Pos (G_Resource_Error_Not_Found)));

      Glib.Error.Error_Free (Info_E);
      Unref (Resource);
   end Test_Get_Info_Nonexistent_Path;

   ----------------------------------------------
   -- Test_Enumerate_Children_Nonexistent_Path --
   ----------------------------------------------

   procedure Test_Enumerate_Children_Nonexistent_Path is
      Load_E   : Glib.Error.GError := null;
      Resource : Gresource := Load ("sample.gresource", Load_E);
      Enum_E   : Glib.Error.GError;
      Children : String_List := Enumerate_Children
        (Self         => Resource,
         Path         => "/org/gtkada/test/does-not-exist",
         Lookup_Flags => G_Resource_Lookup_Flags_None,
         Error        => Enum_E);
      use type Glib.Error.GError;
   begin
      Assert_True (Load_E = null);
      Assert_Cmpint_Eq (Gint (Children'Length), 0);
      Assert_True (Enum_E /= null);
      Assert_Cmpuint_Eq
        (Glib.Guint (Glib.Error.Get_Domain (Enum_E)),
         Glib.Guint (Glib.Error_Enums.GResource_Error_Domain));
      Assert_Cmpint_Eq
        (Glib.Error.Get_Code (Enum_E),
         Gint (GResource_Error'Pos (G_Resource_Error_Not_Found)));

      Free_String_List (Children);
      Glib.Error.Error_Free (Enum_E);
      Unref (Resource);
   end Test_Enumerate_Children_Nonexistent_Path;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/gresource/load-get-info", Test_Load_And_Get_Info'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/enumerate-children",
      Test_Enumerate_Children'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/lookup-data", Test_Lookup_Data'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/register-unregister",
      Test_Register_Unregister'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/new-from-data", Test_New_From_Data'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/g-new-from-data", Test_G_New_From_Data'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/g-new-from-data-invalid-content",
      Test_G_New_From_Data_Invalid_Content'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/new-from-data-invalid-content",
      Test_New_From_Data_Invalid_Content'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/open-stream", Test_Open_Stream'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/open-stream-nonexistent-path",
      Test_Open_Stream_Nonexistent_Path'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/lookup-nonexistent-path",
      Test_Lookup_Nonexistent_Path'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/global-lookup-data",
      Test_Global_Lookup_Data'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/global-lookup-data-nonexistent-path",
      Test_Global_Lookup_Data_Nonexistent_Path'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/load-nonexistent-file",
      Test_Load_Nonexistent_File'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/load-invalid-content",
      Test_Load_Invalid_Content'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gresource/get-info-nonexistent-path",
      Test_Get_Info_Nonexistent_Path'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
