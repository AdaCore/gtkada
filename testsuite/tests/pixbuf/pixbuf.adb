with Ada.Command_Line;
with Ada.Directories;
with Gdk.Paintable;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gdk.Pixbuf_Format; use Gdk.Pixbuf_Format;
with Gdk.Texture;       use Gdk.Texture;
with Glib;              use Glib;
with Glib.Bytes;        use Glib.Bytes;
with Glib.Error;        use Glib.Error;
with Glib.Input_Stream;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Glib.Test;         use Glib.Test;
with GNAT.Strings;
with Gtk.Main;
with Gtk.Picture;       use Gtk.Picture;
with System;            use System;
with System.Storage_Elements; use System.Storage_Elements;

procedure Pixbuf is

   procedure Test_Pixels with Convention => C;
   procedure Test_Transforms with Convention => C;
   procedure Test_Data with Convention => C;
   procedure Test_Bytes with Convention => C;
   procedure Test_File with Convention => C;
   procedure Test_Texture with Convention => C;
   procedure Test_Format_Layout with Convention => C;
   procedure Test_Initialize_Existing with Convention => C;

   procedure Check_Pixel
     (P : Gdk_Pixbuf; X, Y : Gint; R, G, B, A : Guint8);

   procedure Test_Format_Layout is
      function C_Layout (Field : Guint) return Gsize;
      pragma Import (C, C_Layout, "pixbuf_format_layout");
      Format : aliased Gdk_Pixbuf_Format;
      Fields : constant array (Guint range 1 .. 9) of Address :=
        (Format.Name'Address, Format.Signature'Address, Format.Domain'Address,
         Format.Description'Address, Format.Mime_Types'Address,
         Format.Extensions'Address, Format.Flags'Address,
         Format.Disabled'Address, Format.License'Address);
   begin
      Assert_True (Format'Size / System.Storage_Unit = C_Layout (0));
      for Field in Fields'Range loop
         Assert_True
           (Gsize (To_Integer (Fields (Field)) - To_Integer (Format'Address))
            = C_Layout (Field));
      end loop;
      Assert_True (Format.Signature'Size = Address'Size);
      Assert_True (Format.Disabled'Size = Gboolean'Size);
   end Test_Format_Layout;

   procedure Test_Initialize_Existing is
      P : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 3, 2);
      Original : constant Address := Get_Object (P);
      Bad : aliased Gdk_Pixbuf_Record;
      Stream : aliased Glib.Input_Stream.Ginput_Stream_Record;
      Error, Saved_Error : GError;
   begin
      P.Fill (16#112233FF#);
      Initialize_From_File (Bad'Access, "/gtkada-missing/pixbuf.png", Saved_Error);
      Assert_True (Saved_Error /= null);
      --  Seed the out parameter with a real error before every call. The
      --  no-op must clear it and leave the existing object and pixels intact.
      --  Invalid inputs also ensure that none of the C loaders is invoked.
      for Loader in 1 .. 9 loop
         Error := Saved_Error;
         case Loader is
            when 1 =>
               Initialize_From_File (P, "/gtkada-missing/pixbuf.png", Error);
            when 2 =>
               Initialize_From_File_At_Scale
                 (P, "/gtkada-missing/pixbuf.png", -1, -1, True, Error);
            when 3 =>
               Initialize_From_File_At_Size
                 (P, "/gtkada-missing/pixbuf.png", -1, -1, Error);
            when 4 =>
               Initialize_From_Inline (P, 0, (1 .. 0 => 0), Error);
            when 5 =>
               Initialize_From_Resource (P, "/gtkada-missing/pixbuf", Error);
            when 6 =>
               Initialize_From_Resource_At_Scale
                 (P, "/gtkada-missing/pixbuf", -1, -1, True, Error);
            when 7 =>
               Initialize_From_Stream (P, Stream'Access, null, Error);
            when 8 =>
               Initialize_From_Stream_At_Scale
                 (P, Stream'Access, -1, -1, True, null, Error);
            when 9 =>
               Initialize_From_Stream_Finish (P, null, Error);
         end case;
         Assert_True (Error = null);
         Assert_True (Get_Object (P) = Original);
         Assert_True (P.Get_Width = 3 and P.Get_Height = 2);
         Check_Pixel (P, 2, 1, 16#11#, 16#22#, 16#33#, 255);
      end loop;
      Error_Free (Saved_Error);
      Unref (P);
   end Test_Initialize_Existing;

   procedure Check_Pixel
     (P : Gdk_Pixbuf; X, Y : Gint; R, G, B, A : Guint8)
   is
      Pixels : Guint8_Array (1 .. Natural (P.Get_Byte_Length));
      for Pixels'Address use P.Get_Pixels;
      Offset : constant Natural :=
        Natural (Y * P.Get_Rowstride + X * P.Get_N_Channels);
   begin
      Assert_True (Pixels (Offset + 1) = R);
      Assert_True (Pixels (Offset + 2) = G);
      Assert_True (Pixels (Offset + 3) = B);
      if P.Get_Has_Alpha then
         Assert_True (Pixels (Offset + 4) = A);
      end if;
   end Check_Pixel;

   procedure Test_Pixels is
      P : Gdk_Pixbuf;
      Length : Guint;
   begin
      Gdk_New (P, Gdk_Colorspace_Rgb, True, 8, 3, 2);
      Assert_True (P.Is_Created);
      Assert_True (P.Get_Width = 3 and P.Get_Height = 2);
      Assert_True (P.Get_Bits_Per_Sample = 8);
      Assert_True (P.Get_N_Channels = 4);
      Assert_True (P.Get_Colorspace = Gdk_Colorspace_Rgb);
      Assert_True (Get_Property (P, Colorspace_Property) = Gdk_Colorspace_Rgb);
      Assert_True (Get_Property (P, Gdk.Pixbuf.Width_Property) = 3);
      Assert_True (Get_Property (P, Pixels_Property) = P.Get_Pixels);
      P.Fill (16#12345678#);
      Check_Pixel (P, 0, 0, 16#12#, 16#34#, 16#56#, 16#78#);
      Check_Pixel (P, 2, 1, 16#12#, 16#34#, 16#56#, 16#78#);
      Assert_True (P.Get_Pixels_With_Length (Length) = P.Get_Pixels);
      Assert_True (Gsize (Length) = P.Get_Byte_Length);
      Assert_True (P.Read_Pixels = P.Get_Pixels);
      Assert_True (P.Set_Option ("test-key", "test-value"));
      Assert_True (P.Get_Option ("test-key") = "test-value");
      Assert_True (P.Remove_Option ("test-key"));
      Unref (P);
   end Test_Pixels;

   procedure Test_Transforms is
      P : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 1);
      C, F, R, S, Sub : Gdk_Pixbuf;
   begin
      P.Fill (16#FF0000FF#);
      declare
         Pixels : Guint8_Array (1 .. 8);
         for Pixels'Address use P.Get_Pixels;
      begin
         Pixels (5 .. 8) := (0, 0, 255, 255);
      end;
      C := P.Copy;
      F := P.Flip (True);
      R := P.Rotate_Simple (Gdk_Pixbuf_Rotate_Clockwise);
      S := P.Scale_Simple (4, 2, Gdk_Interp_Nearest);
      Sub := P.New_Subpixbuf (1, 0, 1, 1);
      Check_Pixel (F, 0, 0, 0, 0, 255, 255);
      Check_Pixel (F, 1, 0, 255, 0, 0, 255);
      Assert_True (R.Get_Width = 1 and R.Get_Height = 2);
      Check_Pixel (R, 0, 0, 255, 0, 0, 255);
      Check_Pixel (R, 0, 1, 0, 0, 255, 255);
      Assert_True (S.Get_Width = 4 and S.Get_Height = 2);
      Check_Pixel (S, 3, 1, 0, 0, 255, 255);
      Sub.Fill (16#00FF00FF#);
      Check_Pixel (P, 1, 0, 0, 255, 0, 255);
      Check_Pixel (C, 1, 0, 0, 0, 255, 255);
      C.Copy_Area (0, 0, 1, 1, P, 1, 0);
      Check_Pixel (P, 1, 0, 255, 0, 0, 255);
      Unref (P);
      Check_Pixel (Sub, 0, 0, 255, 0, 0, 255);
      Unref (Sub);
      Unref (C);
      Unref (F);
      Unref (R);
      Unref (S);
   end Test_Transforms;

   procedure Test_Data is
      Pixels : aliased Guint8_Array := (10, 20, 30, 255);
      Token : aliased Integer := 42;
      Released : Boolean := False;
      P : Gdk_Pixbuf;
      procedure Destroy (Data, User_Data : Address) with Convention => C;
      procedure Destroy (Data, User_Data : Address) is
      begin
         Assert_True (Data = Pixels'Address);
         Assert_True (User_Data = Token'Address);
         Released := True;
      end Destroy;
   begin
      Gdk_New_From_Data
        (P, Pixels'Address, Gdk_Colorspace_Rgb, True, 8, 1, 1, 4,
         Destroy'Unrestricted_Access, Token'Address);
      Assert_False (Released);
      Check_Pixel (P, 0, 0, 10, 20, 30, 255);
      P.Fill (16#010203FF#);
      Assert_True (Pixels = (1, 2, 3, 255));
      Unref (P);
      Assert_True (Released);
   end Test_Data;

   procedure Test_Bytes is
      Data : Gbytes := Gbytes_New ((11, 22, 33, 255), 4);
      P : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New_From_Bytes (Data, Gdk_Colorspace_Rgb, True, 8, 1, 1, 4);
      Snapshot : Gbytes;
   begin
      Data.Unref;
      Check_Pixel (P, 0, 0, 11, 22, 33, 255);
      Snapshot := P.Read_Pixel_Bytes;
      Assert_True (Snapshot.Get_Size = 4);
      Unref (P);
      Assert_True (Snapshot.Get_Size = 4);
      Snapshot.Unref;
   end Test_Bytes;

   procedure Test_File is
      Filename : constant String := "pixbuf-test.png";
      No_Options : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      P : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 3, 2);
      Loaded, Scaled : Gdk_Pixbuf;
      Bad : aliased Gdk_Pixbuf_Record;
      Error : GError;
      Width, Height : aliased Gint;
      Format, Format_Copy : Gdk_Pixbuf_Format_Access;
      Formats : Format_List.GSlist;
      Buffer : Address;
      Size : Gsize;
   begin
      Formats := Get_Formats;
      Assert_True (Format_List.Length (Formats) > 0);
      Assert_True (Get_Name (Format_List.Get_Data (Formats)) /= "");
      Format_List.Free (Formats);
      P.Fill (16#112233FF#);
      Assert_True (P.Savev (Filename, "png", No_Options, No_Options, Error));
      Assert_True (Error = null);
      Assert_True (P.Save_To_Bufferv
        (Buffer, Size, "png", No_Options, No_Options, Error));
      Assert_True (Error = null and Size > 8);
      declare
         Signature : Guint8_Array (1 .. 8);
         for Signature'Address use Buffer;
      begin
         Assert_True (Signature = (137, 80, 78, 71, 13, 10, 26, 10));
      end;
      G_Free (Buffer);
      Gdk_New_From_File (Loaded, Filename, Error);
      Assert_True (Error = null and Loaded.Is_Created);
      Check_Pixel (Loaded, 2, 1, 16#11#, 16#22#, 16#33#, 255);
      Scaled := Gdk_Pixbuf_New_From_File_At_Scale (Filename, 6, 4, True, Error);
      Assert_True (Error = null);
      Assert_True (Scaled.Get_Width = 6 and Scaled.Get_Height = 4);
      Format := Get_File_Info (Filename, Width'Access, Height'Access);
      Assert_True (Format /= null);
      Assert_True (Get_Name (Format) = "png");
      Assert_True (Width = 3 and Height = 2);
      Format_Copy := Copy (Format);
      Assert_True (Get_Name (Format_Copy) = "png");
      Set_Disabled (Format_Copy, True);
      Assert_True (Is_Disabled (Format_Copy));
      Assert_True (Format_Copy.Disabled = 1);
      Set_Disabled (Format_Copy, False);
      Assert_False (Is_Disabled (Format_Copy));
      Assert_True (Format_Copy.Disabled = 0);
      Free (Format_Copy);
      Unref (Scaled);
      Unref (Loaded);
      Unref (P);
      Ada.Directories.Delete_File (Filename);
      Initialize_From_File (Bad'Access, Filename, Error);
      Assert_False (Bad.Is_Created);
      Assert_True (Error /= null);
      Error_Free (Error);
      Assert_True (Get_File_Info (Filename) = null);
   end Test_File;

   procedure Test_Texture is
      P : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 7, 5);
      T : Gdk_Texture;
      Picture : Gtk_Picture;
   begin
      P.Fill (16#FF8000FF#);
      Gdk_New_For_Pixbuf (T, P);
      Picture := Gtk_Picture_New_For_Paintable (+T);
      Ref_Sink (Picture);
      Assert_True (T.Get_Width = 7 and T.Get_Height = 5);
      Unref (P);
      Unref (T);
      Assert_True (Gdk.Paintable.Get_Intrinsic_Width (Picture.Get_Paintable) = 7);
      Assert_True (Gdk.Paintable.Get_Intrinsic_Height (Picture.Get_Paintable) = 5);
      Unref (Picture);
   end Test_Texture;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/pixbuf/pixels", Test_Pixels'Unrestricted_Access);
   Add_Func ("/pixbuf/transforms", Test_Transforms'Unrestricted_Access);
   Add_Func ("/pixbuf/data", Test_Data'Unrestricted_Access);
   Add_Func ("/pixbuf/bytes", Test_Bytes'Unrestricted_Access);
   Add_Func ("/pixbuf/file", Test_File'Unrestricted_Access);
   Add_Func ("/pixbuf/texture", Test_Texture'Unrestricted_Access);
   Add_Func ("/pixbuf/format-layout", Test_Format_Layout'Unrestricted_Access);
   Add_Func ("/pixbuf/initialize-existing", Test_Initialize_Existing'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Pixbuf;
