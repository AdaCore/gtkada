--  Gresource tests for loading, lookup, enumeration and registration.
--
--  The binary fixture sample.gresource is generated from sample.gresource.xml,
--  alpha.txt and beta.txt. Regenerate it from testsuite/tests/gresource with:
--    glib-compile-resources sample.gresource.xml --target=sample.gresource
--      --sourcedir=.

with Ada.Command_Line;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with System;

with GNAT.Strings; use GNAT.Strings;

with Glib;           use Glib;
with Glib.Bytes;     use Glib.Bytes;
with Glib.Error;
with Glib.Resource;  use Glib.Resource;
with Glib.Test;      use Glib.Test;

procedure Main is

   Alpha_Text : constant UTF8_String := "alpha resource" & ASCII.LF;
   Beta_Text  : constant UTF8_String := "beta value" & ASCII.LF;

   function Read_File_As_Gbytes (Filename : String) return Gbytes;

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

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
