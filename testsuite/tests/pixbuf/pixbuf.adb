with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Ada.Directories;
with Gdk.Paintable;
with Gdk.Pixbuf;        use Gdk.Pixbuf;
with Gdk.Pixbuf_Format; use Gdk.Pixbuf_Format;
with Gdk.Texture;       use Gdk.Texture;
with Glib;              use Glib;
with Glib.Bytes;        use Glib.Bytes;
with Glib.Error;        use Glib.Error;
with Glib.File_Input_Stream;
with Glib.File_Output_Stream;
with Glib.GFile;
with Glib.Input_Stream;
with Glib.Main;
with Glib.Object;       use Glib.Object;
with Glib.Output_Stream;
with Glib.Properties;   use Glib.Properties;
with Glib.Resource;     use Glib.Resource;
with Glib.Test;         use Glib.Test;
with Glib.Types;
with GNAT.Strings;
with Gtkada.Types;
with Gtk.Main;
with Gtk.Picture;       use Gtk.Picture;
with System;            use System;
with System.Storage_Elements; use System.Storage_Elements;

procedure Pixbuf is

   use type Glib.Input_Stream.Ginput_Stream;
   use type Glib.File_Input_Stream.Gfile_Input_Stream;
   use type Glib.File_Output_Stream.Gfile_Output_Stream;

   procedure Test_Pixels with Convention => C;
   procedure Test_Transforms with Convention => C;
   procedure Test_Data with Convention => C;
   procedure Test_Bytes with Convention => C;
   procedure Test_File with Convention => C;
   procedure Test_Texture with Convention => C;
   procedure Test_Format_Layout with Convention => C;
   procedure Test_Initialize_Existing with Convention => C;
   procedure Test_Constructors with Convention => C;
   procedure Test_Constructors_Invalid with Convention => C;
   procedure Test_Xpm with Convention => C;
   procedure Test_Transforms_And_Options with Convention => C;
   procedure Test_Rowstride with Convention => C;
   procedure Test_Streams_And_Async with Convention => C;

   type Async_Operation is
     (No_Async, File_Info, Stream_Procedure, Stream_Initialize,
      Stream_Initialize_No_Op, Stream_Function, Stream_Scaled, Load_Icon,
      Save_Stream);
   Async_Kind : Async_Operation := No_Async;
   Async_Done : Boolean := False;
   Async_Error : GError;
   Async_Pixbuf, Async_Target : Gdk_Pixbuf;
   Async_Stream : Glib.Input_Stream.Ginput_Stream;
   Async_Format : Gdk_Pixbuf_Format_Access;
   Async_Width, Async_Height : Gint;
   Async_Save_Succeeded : Boolean := False;
   Async_Expect_Error : Boolean := False;
   Async_Type : aliased UTF8_String := "image/png";

   procedure On_Async_Ready
     (Source_Object : access GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   type Route is (Procedure_Form, Function_Form, Initialize_Form);
   --  The three Ada subprograms generated for each C constructor

   type Source is
     (File, File_At_Scale, File_At_Size, Inline, Resource, Resource_At_Scale,
      Stream, Stream_At_Scale);
   --  The throwing constructors. From_Stream_Finish needs a real
   --  GAsyncResult, so only Test_Initialize_Existing covers it.

   function Construct
     (Via : Route; From : Source; Valid : Boolean; Error : out GError)
      return Gdk_Pixbuf;
   --  Valid inputs all hold the 3 x 2, 16#112233FF# image of the pixbuf.png
   --  fixture (scaled to 6 x 4 by the _At_Scale/_At_Size sources); invalid
   --  ones are missing or not an image.

   function Pixdata return Guint8_Array;
   --  pixbuf.png's image as a serialized GdkPixdata, for From_Inline

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

   function Pixdata return Guint8_Array is
      Header : constant Guint8_Array :=
        (16#47#, 16#64#, 16#6B#, 16#50#,  --  magic "GdkP"
         0, 0, 0, 48,                      --  total length
         1, 1, 0, 2,                       --  raw, 8 bits/sample, RGBA
         0, 0, 0, 12,                      --  rowstride
         0, 0, 0, 3,                       --  width
         0, 0, 0, 2);                      --  height
      Pixel  : constant Guint8_Array := (16#11#, 16#22#, 16#33#, 16#FF#);
   begin
      return Header & Pixel & Pixel & Pixel & Pixel & Pixel & Pixel;
   end Pixdata;

   function Construct
     (Via : Route; From : Source; Valid : Boolean; Error : out GError)
      return Gdk_Pixbuf
   is
      Path : constant String :=
        (if Valid then "pixbuf.png" else "/gtkada-missing/pixbuf.png");
      Resource_Path : constant String :=
        (if Valid then "/org/gtkada/test/pixbuf.png"
         else "/gtkada-missing/pixbuf");
      Data : constant Guint8_Array :=
        (if Valid then Pixdata else (1 .. 32 => 16#AA#));
      --  Longer than the pixdata header: shorter data is a programming
      --  error (g_return_val_if_fail), not a GError
      use type Glib.File_Input_Stream.Gfile_Input_Stream;
      In_Stream : Glib.File_Input_Stream.Gfile_Input_Stream;
      P : Gdk_Pixbuf;
   begin
      Message (Via'Image & " " & From'Image & " " & Valid'Image);
      if From in Stream | Stream_At_Scale then
         --  An invalid stream is readable, but not an image
         declare
            Input : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path
              (if Valid then "pixbuf.png" else "pixbuf.gresource.xml");
         begin
            In_Stream := Glib.GFile.Read (Input, null, Error);
            Assert_True (Error = null);
            Glib.Object.Unref
              (Glib.Types.To_Object (Glib.Types.GType_Interface (Input)));
         end;
      end if;
      if Via = Initialize_Form then
         P := new Gdk_Pixbuf_Record;
      end if;

      case From is
         when File =>
            case Via is
               when Procedure_Form => Gdk_New_From_File (P, Path, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_File (Path, Error);
               when Initialize_Form => Initialize_From_File (P, Path, Error);
            end case;
         when File_At_Scale =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_File_At_Scale (P, Path, 6, 4, True, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_File_At_Scale
                    (Path, 6, 4, True, Error);
               when Initialize_Form =>
                  Initialize_From_File_At_Scale (P, Path, 6, 4, True, Error);
            end case;
         when File_At_Size =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_File_At_Size (P, Path, 6, 4, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_File_At_Size (Path, 6, 4, Error);
               when Initialize_Form =>
                  Initialize_From_File_At_Size (P, Path, 6, 4, Error);
            end case;
         when Inline =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_Inline (P, Data'Length, Data, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_Inline (Data'Length, Data, Error);
               when Initialize_Form =>
                  Initialize_From_Inline (P, Data'Length, Data, Error);
            end case;
         when Resource =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_Resource (P, Resource_Path, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_Resource (Resource_Path, Error);
               when Initialize_Form =>
                  Initialize_From_Resource (P, Resource_Path, Error);
            end case;
         when Resource_At_Scale =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_Resource_At_Scale
                    (P, Resource_Path, 6, 4, True, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_Resource_At_Scale
                    (Resource_Path, 6, 4, True, Error);
               when Initialize_Form =>
                  Initialize_From_Resource_At_Scale
                    (P, Resource_Path, 6, 4, True, Error);
            end case;
         when Stream =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_Stream (P, In_Stream, null, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_Stream (In_Stream, null, Error);
               when Initialize_Form =>
                  Initialize_From_Stream (P, In_Stream, null, Error);
            end case;
         when Stream_At_Scale =>
            case Via is
               when Procedure_Form =>
                  Gdk_New_From_Stream_At_Scale
                    (P, In_Stream, 6, 4, True, null, Error);
               when Function_Form =>
                  P := Gdk_Pixbuf_New_From_Stream_At_Scale
                    (In_Stream, 6, 4, True, null, Error);
               when Initialize_Form =>
                  Initialize_From_Stream_At_Scale
                    (P, In_Stream, 6, 4, True, null, Error);
            end case;
      end case;

      if In_Stream /= null then
         In_Stream.Unref;
      end if;
      return P;
   end Construct;

   procedure Test_Constructors is
      Error : GError;
      P : Gdk_Pixbuf;
   begin
      for From in Source loop
         for Via in Route loop
            P := Construct (Via, From, True, Error);
            Assert_True (Error = null);
            Assert_True (P /= null and then P.Is_Created);
            if From in File_At_Scale | File_At_Size | Resource_At_Scale
              | Stream_At_Scale
            then
               Assert_True (P.Get_Width = 6 and P.Get_Height = 4);
            else
               Assert_True (P.Get_Width = 3 and P.Get_Height = 2);
            end if;
            Check_Pixel (P, 0, 0, 16#11#, 16#22#, 16#33#, 255);
            Unref (P);
         end loop;
      end loop;
   end Test_Constructors;

   procedure Test_Constructors_Invalid is
      Error : GError;
      P : Gdk_Pixbuf;
   begin
      for From in Source loop
         for Via in Route loop
            P := Construct (Via, From, False, Error);
            Assert_True (Error /= null);
            Assert_True (Get_Message (Error)'Length > 0);
            Error_Free (Error);
            case Via is
               when Procedure_Form | Function_Form =>
                  --  The generated constructor frees its record
                  Assert_True (P = null);
               when Initialize_Form =>
                  --  The caller's record is left allocated but not created
                  Assert_True (P /= null);
                  Assert_False (P.Is_Created);
                  P.Deallocate;
            end case;
         end loop;
      end loop;
   end Test_Constructors_Invalid;

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

   --  This callback completes every async Pixbuf operation in the test.
   --  The selected finish function is kept in Async_Kind because GtkAda's
   --  callback type does not carry a user-data parameter.
   procedure On_Async_Ready
     (Source_Object : access GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
   begin
      Async_Error := null;
      case Async_Kind is
         when File_Info =>
            Async_Format := Get_File_Info_Finish
              (Res, Async_Width, Async_Height, Async_Error);
         when Stream_Procedure =>
            Gdk_New_From_Stream_Finish
              (Async_Pixbuf, Res, Async_Error);
         when Stream_Initialize | Stream_Initialize_No_Op =>
            Initialize_From_Stream_Finish
              (Async_Target, Res, Async_Error);
         when Stream_Function | Stream_Scaled =>
            Async_Pixbuf := Gdk_Pixbuf_New_From_Stream_Finish
              (Res, Async_Error);
         when Load_Icon =>
            Async_Stream := Async_Pixbuf.Load_Finish
              (Res, Async_Type'Access, Async_Error);
         when Save_Stream =>
            Async_Save_Succeeded := Save_To_Stream_Finish
              (Res, Async_Error);
         when No_Async =>
            Assert_True (False);
      end case;
      if Async_Expect_Error then
         Assert_True (Async_Error /= null);
      else
         Assert_True (Async_Error = null);
      end if;
      Async_Done := True;
   end On_Async_Ready;

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
      P_From_Function : Gdk_Pixbuf;
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
      --  The function constructor uses caller-owned pixels when no destroy
      --  callback is supplied; keep the aliased array alive through unref.
      P_From_Function := Gdk_Pixbuf_New_From_Data
        (Pixels'Address, Gdk_Colorspace_Rgb, True, 8, 1, 1, 4);
      Check_Pixel (P_From_Function, 0, 0, 1, 2, 3, 255);
      Unref (P_From_Function);
   end Test_Data;

   procedure Test_Bytes is
      Data : Gbytes := Gbytes_New ((11, 22, 33, 255), 4);
      P : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New_From_Bytes (Data, Gdk_Colorspace_Rgb, True, 8, 1, 1, 4);
      Via_Procedure, Via_Initialize : Gdk_Pixbuf;
      Snapshot : Gbytes;
   begin
      Check_Pixel (P, 0, 0, 11, 22, 33, 255);
      --  Cover the procedure and initialize constructor forms as well as the
      --  function form above. The byte object remains referenced by pixbufs.
      Gdk_New_From_Bytes
        (Via_Procedure, Data, Gdk_Colorspace_Rgb, True, 8, 1, 1, 4);
      Via_Initialize := new Gdk_Pixbuf_Record;
      Initialize_From_Bytes
        (Via_Initialize, Data, Gdk_Colorspace_Rgb, True, 8, 1, 1, 4);
      Check_Pixel (Via_Procedure, 0, 0, 11, 22, 33, 255);
      Check_Pixel (Via_Initialize, 0, 0, 11, 22, 33, 255);
      Data.Unref;
      Snapshot := P.Read_Pixel_Bytes;
      Assert_True (Snapshot.Get_Size = 4);
      Unref (P);
      Unref (Via_Procedure);
      Unref (Via_Initialize);
      Assert_True (Snapshot.Get_Size = 4);
      Snapshot.Unref;
   end Test_Bytes;

   procedure Test_Xpm is
      Formats : Format_List.GSlist := Get_Formats;
      Item : Format_List.GSlist := Formats;
      Has_Xpm : Boolean := False;
   begin
      --  Some distributions omit the optional XPM loader. Skip before calling
      --  its constructor, which otherwise emits a fatal warning on this test
      --  runner.
      for Index in 1 .. Format_List.Length (Formats) loop
         if Get_Name (Format_List.Get_Data (Item)) = "xpm" then
            Has_Xpm := True;
         end if;
         Item := Format_List.Next (Item);
      end loop;
      Format_List.Free (Formats);
      if not Has_Xpm then
         Skip ("The runtime has no XPM loader module");
         return;
      end if;
      declare
         --  A two-color XPM image with a transparent background and one red dot.
         Data : GNAT.Strings.String_List (1 .. 5) :=
           (new String'("2 2 2 1"), new String'("  c None"),
            new String'("x c #ff0000"), new String'("x "), new String'(" x"));
         P, Via_Procedure, Via_Initialize : Gdk_Pixbuf;
      begin
         --  Exercise all three generated entry points and the initialize no-op.
         P := Gdk_Pixbuf_New_From_Xpm_Data (Data);
         Gdk_New_From_Xpm_Data (Via_Procedure, Data);
         Via_Initialize := new Gdk_Pixbuf_Record;
         Initialize_From_Xpm_Data (Via_Initialize, Data);
         Assert_True (P.Get_Width = 2 and P.Get_Height = 2);
         Assert_True (Via_Procedure.Get_Width = 2 and Via_Initialize.Get_Width = 2);
         --  Initializing an already-created object is documented as a no-op.
         Initialize_From_Xpm_Data (Via_Initialize, Data);
         Assert_True (Via_Initialize.Get_Width = 2);
         Check_Pixel (P, 0, 0, 255, 0, 0, 255);
         Check_Pixel (P, 1, 0, 0, 0, 0, 0);
         Check_Pixel (Via_Procedure, 0, 1, 0, 0, 0, 0);
         --  GNAT.Strings.String_List owns each allocated element.
         for Text of Data loop
            GNAT.Strings.Free (Text);
         end loop;
         Unref (P);
         Unref (Via_Procedure);
         Unref (Via_Initialize);
      end;
   end Test_Xpm;

   procedure Test_Transforms_And_Options is
      --  Keep small images so each transform's output can be checked exactly.
      Source : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 1);
      Dest : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 1);
      Added_Alpha, Oriented, Checker, Simple_Checker, Copy : Gdk_Pixbuf;
      No_Options : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Pixels : Guint8_Array (1 .. 8);
      for Pixels'Address use Source.Get_Pixels;
   begin
      --  The source has two opaque pixels of different colors.
      Source.Fill (16#FF0000FF#);
      Pixels (5 .. 8) := (0, 0, 255, 255);
      Assert_True (Source.Set_Option ("test-option", "kept"));
      Assert_True (Source.Get_Options /= Null_Address);

      --  Adding alpha can make the selected color transparent and leaves the
      --  other pixel fully opaque.
      Added_Alpha := Source.Add_Alpha (True, 255, 0, 0);
      Check_Pixel (Added_Alpha, 0, 0, 255, 0, 0, 0);
      Check_Pixel (Added_Alpha, 1, 0, 0, 0, 255, 255);

      --  With no orientation metadata, applying orientation preserves pixels.
      Oriented := Source.Apply_Embedded_Orientation;
      Assert_True (Oriented.Get_Width = 2 and Oriented.Get_Height = 1);
      Check_Pixel (Oriented, 1, 0, 0, 0, 255, 255);

      --  The general composite and scale procedures write into an existing
      --  destination; checking its pixel verifies that the call took effect.
      Dest.Fill (16#000000FF#);
      Source.Composite
        (Dest, 0, 0, 2, 1, 0.0, 0.0, 1.0, 1.0,
         Gdk_Interp_Nearest, 255);
      Check_Pixel (Dest, 0, 0, 255, 0, 0, 255);
      Dest.Fill (16#000000FF#);
      Source.Scale
        (Dest, 0, 0, 2, 1, 0.0, 0.0, 1.0, 1.0, Gdk_Interp_Nearest);
      Check_Pixel (Dest, 1, 0, 0, 0, 255, 255);

      --  Checkerboard compositing uses the supplied colors behind transparency.
      Added_Alpha.Composite_Color
        (Dest, 0, 0, 2, 1, 0.0, 0.0, 1.0, 1.0,
         Gdk_Interp_Nearest, 255, 0, 0, 1, 16#112233#, 16#445566#);
      Check_Pixel (Dest, 0, 0, 16#11#, 16#22#, 16#33#, 255);
      Checker := Added_Alpha.Composite_Color_Simple
        (2, 1, Gdk_Interp_Nearest, 255, 1, 16#112233#, 16#445566#);
      Assert_True (Checker.Get_Width = 2 and Checker.Get_Height = 1);
      Check_Pixel (Checker, 0, 0, 16#11#, 16#22#, 16#33#, 255);
      Simple_Checker := Source.Copy;
      Source.Saturate_And_Pixelate (Simple_Checker, 1.0, False);
      Check_Pixel (Simple_Checker, 1, 0, 0, 0, 255, 255);

      --  Pixbuf copies omit metadata until Copy_Options is requested.
      Copy := Source.Copy;
      Assert_True (Copy.Get_Option ("test-option") = "");
      Assert_True (Source.Copy_Options (Copy));
      Assert_True (Copy.Get_Option ("test-option") = "kept");
      Assert_True (Copy.Remove_Option ("test-option"));
      Assert_False (Copy.Remove_Option ("test-option"));

      Unref (Copy);
      Unref (Simple_Checker);
      Unref (Checker);
      Unref (Oriented);
      Unref (Added_Alpha);
      Unref (Dest);
      Unref (Source);
   end Test_Transforms_And_Options;

   procedure Test_Rowstride is
   begin
      --  RGB rows are padded to four-byte alignment; RGBA rows already align.
      Assert_True
        (Calculate_Rowstride (Gdk_Colorspace_Rgb, False, 8, 3, 2) = 12);
      Assert_True
        (Calculate_Rowstride (Gdk_Colorspace_Rgb, True, 8, 3, 2) = 12);
      Assert_True
        (Calculate_Rowstride (Gdk_Colorspace_Rgb, False, 8, 1, 2) = 4);
   end Test_Rowstride;

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

      --  A PNG format reports its descriptive metadata, common file and MIME
      --  names, and the save capabilities documented by the loader.
      declare
         --  These returned arrays own their strings and must be freed below.
         Extensions : GNAT.Strings.String_List := Get_Extensions (Format);
         Mime_Types : GNAT.Strings.String_List := Get_Mime_Types (Format);
         Has_Png_Extension : Boolean := False;
         Has_Png_Mime_Type : Boolean := False;
      begin
         Assert_True (Get_Description (Format)'Length > 0);
         Assert_True (Get_License (Format)'Length > 0);
         --  Check the documented PNG identifiers while releasing every item.
         for Index in Extensions'Range loop
            Has_Png_Extension := Has_Png_Extension or else
              Extensions (Index).all = "png";
            GNAT.Strings.Free (Extensions (Index));
         end loop;
         for Index in Mime_Types'Range loop
            Has_Png_Mime_Type := Has_Png_Mime_Type or else
              Mime_Types (Index).all = "image/png";
            GNAT.Strings.Free (Mime_Types (Index));
         end loop;
         Assert_True (Has_Png_Extension);
         Assert_True (Has_Png_Mime_Type);
         --  PNG supports compression metadata, while arbitrary keys are false.
         Assert_True (Is_Save_Option_Supported (Format, "compression"));
         Assert_False
           (Is_Save_Option_Supported (Format, "unsupported-option"));
         --  PNG is writable and raster based, so it is not scalable.
         Assert_False (Is_Scalable (Format));
         Assert_True (Is_Writable (Format));
      end;

      Format_Copy := Copy (Format);
      Assert_True (Get_Name (Format_Copy) = "png");
      Set_Disabled (Format_Copy, True);
      Assert_True (Is_Disabled (Format_Copy));
      Assert_True (Format_Copy.Disabled = 1);
      Set_Disabled (Format_Copy, False);
      Assert_False (Is_Disabled (Format_Copy));
      Assert_True (Format_Copy.Disabled = 0);
      Free (Format_Copy);

      --  From_Object_Free copies the value and releases only its GLib-allocated
      --  wrapper, so both helpers can safely be checked with shallow copies.
      declare
         type Format_Access is access all Gdk_Pixbuf_Format;
         type Pattern_Access is access all Gdk_Pixbuf_Module_Pattern;
         function To_Format_Access is new Ada.Unchecked_Conversion
           (Address, Format_Access);
         function To_Pattern_Access is new Ada.Unchecked_Conversion
           (Address, Pattern_Access);
         function G_Malloc (Size : Gsize) return Address;
         pragma Import (C, G_Malloc, "g_malloc");

         --  Round the record sizes up to whole bytes for g_malloc.
         Format_Bytes : constant Gsize :=
           Gsize
             ((Gdk_Pixbuf_Format'Size + System.Storage_Unit - 1)
              / System.Storage_Unit);
         Pattern_Bytes : constant Gsize :=
           Gsize
             ((Gdk_Pixbuf_Module_Pattern'Size + System.Storage_Unit - 1)
              / System.Storage_Unit);
         Format_Wrapper : constant Format_Access :=
           To_Format_Access (G_Malloc (Format_Bytes));
         Pattern_Wrapper : constant Pattern_Access :=
           To_Pattern_Access (G_Malloc (Pattern_Bytes));
         Format_Value : Gdk_Pixbuf_Format;
         Pattern_Value : Gdk_Pixbuf_Module_Pattern;
      begin
         --  The format's internal strings remain owned by the loader.
         Format_Wrapper.all := Format.all;
         Format_Value := From_Object_Free (Format_Wrapper);
         Assert_True
           (Format_Value.Flags = Format.Flags
            and Format_Value.Disabled = Format.Disabled);

         --  Null strings suffice here because the helper copies only fields.
         Pattern_Wrapper.all :=
           (Prefix => Gtkada.Types.Null_Ptr,
            Mask => Gtkada.Types.Null_Ptr,
            Relevance => 100);
         Pattern_Value := From_Object_Free (Pattern_Wrapper);
         Assert_Cmpint_Eq (Pattern_Value.Relevance, 100);
      end;

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

   procedure Test_Streams_And_Async is
      --  Open fresh streams for every asynchronous load: each request consumes
      --  its input, and GIO keeps the source alive through its callback.
      No_Options : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Input : Glib.File_Input_Stream.Gfile_Input_Stream;
      Output : Glib.File_Output_Stream.Gfile_Output_Stream;
      Error : GError;
      P, Loaded : Gdk_Pixbuf;
      Saved_Name : constant String := "pixbuf-stream-save.png";
      Async_Saved_Name : constant String := "pixbuf-async-save.png";
      Sync_Stream : Glib.Input_Stream.Ginput_Stream;
      Original : Address;
      Type_Name : aliased UTF8_String := "image/png";

      function Open_Input (Path : String)
        return Glib.File_Input_Stream.Gfile_Input_Stream
      is
         Source_File : constant Glib.GFile.Gfile :=
           Glib.GFile.New_For_Path (Path);
         Open_Error : GError;
         Result : constant Glib.File_Input_Stream.Gfile_Input_Stream :=
           Glib.GFile.Read (Source_File, null, Open_Error);
      begin
         Assert_True (Open_Error = null and Result /= null);
         --  GFile is an interface wrapper, so release its GObject reference
         --  through the interface conversion used elsewhere in this test.
         Glib.Object.Unref
           (Glib.Types.To_Object
              (Glib.Types.GType_Interface (Source_File)));
         return Result;
      end Open_Input;

      function Open_Output (Path : String)
        return Glib.File_Output_Stream.Gfile_Output_Stream
      is
         Target_File : constant Glib.GFile.Gfile :=
           Glib.GFile.New_For_Path (Path);
         Open_Error : GError;
         Result : constant Glib.File_Output_Stream.Gfile_Output_Stream :=
           Glib.GFile.Replace
             (Target_File, "", False, Glib.GFile.G_File_Create_None,
              null, Open_Error);
      begin
         Assert_True (Open_Error = null and Result /= null);
         Glib.Object.Unref
           (Glib.Types.To_Object
              (Glib.Types.GType_Interface (Target_File)));
         return Result;
      end Open_Output;

      procedure Wait_For_Callback is
      begin
         --  GIO delivers completion on the default context; blocking one
         --  iteration avoids a timing-based poll or an external event loop.
         while not Async_Done loop
            Assert_True
              (Glib.Main.Main_Context_Iteration (May_Block => True)
               or else not Async_Done);
         end loop;
      end Wait_For_Callback;
   begin
      --  Async file parsing reports the same metadata as the sync API.
      Async_Kind := File_Info;
      Async_Done := False;
      Get_File_Info_Async
        ("pixbuf.png", null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Format /= null);
      Assert_True (Get_Name (Async_Format) = "png");
      Assert_True (Async_Width = 3 and Async_Height = 2);

      --  The procedure-form finish wrapper returns a newly loaded object.
      Input := Open_Input ("pixbuf.png");
      Async_Kind := Stream_Procedure;
      Async_Done := False;
      New_From_Stream_Async
        (Input, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Pixbuf /= null and Async_Pixbuf.Is_Created);
      Check_Pixel (Async_Pixbuf, 0, 0, 16#11#, 16#22#, 16#33#, 255);
      Unref (Async_Pixbuf);
      Glib.Object.Unref (Glib.Object.GObject (Input));

      --  The initialize finish form creates an uninitialized target in place.
      Input := Open_Input ("pixbuf.png");
      Async_Target := new Gdk_Pixbuf_Record;
      Async_Kind := Stream_Initialize;
      Async_Done := False;
      New_From_Stream_Async
        (Input, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Target.Is_Created and Async_Target.Get_Width = 3);
      Unref (Async_Target);
      Glib.Object.Unref (Glib.Object.GObject (Input));

      --  The same finish call is a no-op when the target already has pixels.
      Input := Open_Input ("pixbuf.png");
      Async_Target := Gdk_Pixbuf_New
        (Gdk_Colorspace_Rgb, True, 8, 1, 1);
      Async_Target.Fill (16#AABBCCFF#);
      Original := Get_Object (Async_Target);
      Async_Kind := Stream_Initialize_No_Op;
      Async_Done := False;
      New_From_Stream_Async
        (Input, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Get_Object (Async_Target) = Original);
      Check_Pixel (Async_Target, 0, 0, 16#AA#, 16#BB#, 16#CC#, 255);
      Unref (Async_Target);
      Glib.Object.Unref (Glib.Object.GObject (Input));

      --  A readable non-image exercises the finish error path and its null
      --  return value without relying on a missing file or host configuration.
      Input := Open_Input ("pixbuf.gresource.xml");
      Async_Kind := Stream_Function;
      Async_Expect_Error := True;
      Async_Done := False;
      New_From_Stream_Async
        (Input, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Pixbuf = null);
      Error_Free (Async_Error);
      Async_Expect_Error := False;
      Glib.Object.Unref (Glib.Object.GObject (Input));

      --  The procedure constructor finish form also returns null and releases
      --  its temporary wrapper when decoding fails.
      Input := Open_Input ("pixbuf.gresource.xml");
      Async_Kind := Stream_Procedure;
      Async_Expect_Error := True;
      Async_Done := False;
      New_From_Stream_Async
        (Input, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Pixbuf = null);
      Error_Free (Async_Error);
      Async_Expect_Error := False;
      Glib.Object.Unref (Glib.Object.GObject (Input));

      --  The scaled async entry point uses the function-form finish wrapper.
      Input := Open_Input ("pixbuf.png");
      Async_Kind := Stream_Scaled;
      Async_Done := False;
      New_From_Stream_At_Scale_Async
        (Input, 6, 4, True, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True
        (Async_Pixbuf.Get_Width = 6 and Async_Pixbuf.Get_Height = 4);
      Unref (Async_Pixbuf);
      Glib.Object.Unref (Glib.Object.GObject (Input));

      --  The GLoadableIcon methods return the encoded data for a pixbuf.
      P := Gdk_Pixbuf_New_From_File ("pixbuf.png", Error);
      Assert_True (Error = null and P /= null);
      Sync_Stream := P.Load (32, Type_Name'Access, null, Error);
      Assert_True (Error = null and Sync_Stream /= null);
      Assert_True (Type_Name = "image/png");
      Glib.Object.Unref (Glib.Object.GObject (Sync_Stream));
      Async_Pixbuf := P;
      Async_Kind := Load_Icon;
      Async_Type := "image/png";
      Async_Done := False;
      P.Load_Async (32, null, On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Stream /= null);
      Assert_True (Async_Type = "image/png");
      Glib.Object.Unref (Glib.Object.GObject (Async_Stream));

      --  Sync and async stream saves both produce a readable PNG file.
      Output := Open_Output (Saved_Name);
      Assert_True
        (P.Save_To_Streamv
           (Output, "png", No_Options, No_Options, null, Error));
      Assert_True (Error = null);
      Assert_True (Output.Close (null, Error) and Error = null);
      Loaded := Gdk_Pixbuf_New_From_File (Saved_Name, Error);
      Assert_True (Error = null and Loaded.Get_Width = 3);
      Unref (Loaded);
      Glib.Object.Unref (Glib.Object.GObject (Output));
      Ada.Directories.Delete_File (Saved_Name);

      Output := Open_Output (Async_Saved_Name);
      Async_Kind := Save_Stream;
      Async_Done := False;
      P.Save_To_Streamv_Async
        (Output, "png", No_Options, No_Options, null,
         On_Async_Ready'Unrestricted_Access);
      Wait_For_Callback;
      Assert_True (Async_Save_Succeeded and Async_Error = null);
      Assert_True (Output.Close (null, Error) and Error = null);
      Loaded := Gdk_Pixbuf_New_From_File (Async_Saved_Name, Error);
      Assert_True (Error = null and Loaded.Get_Width = 3);
      Unref (Loaded);
      Glib.Object.Unref (Glib.Object.GObject (Output));
      Unref (P);
      Ada.Directories.Delete_File (Async_Saved_Name);

      --  A nonexistent module cache returns a recoverable loader error.
      Assert_False (Init_Modules ("/gtkada-missing/pixbuf-modules", Error));
      Assert_True (Error /= null);
      Error_Free (Error);
   end Test_Streams_And_Async;

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
   declare
      --  pixbuf.gresource is generated from pixbuf.gresource.xml with
      --    glib-compile-resources pixbuf.gresource.xml --target=pixbuf.gresource
      Error : GError;
      Bundle : constant Gresource := Load ("pixbuf.gresource", Error);
   begin
      Assert_True (Error = null);
      Register (Bundle);
   end;
   Add_Func ("/pixbuf/pixels", Test_Pixels'Unrestricted_Access);
   Add_Func ("/pixbuf/transforms", Test_Transforms'Unrestricted_Access);
   Add_Func ("/pixbuf/data", Test_Data'Unrestricted_Access);
   Add_Func ("/pixbuf/bytes", Test_Bytes'Unrestricted_Access);
   Add_Func ("/pixbuf/file", Test_File'Unrestricted_Access);
   Add_Func ("/pixbuf/texture", Test_Texture'Unrestricted_Access);
   Add_Func ("/pixbuf/format-layout", Test_Format_Layout'Unrestricted_Access);
   Add_Func ("/pixbuf/initialize-existing", Test_Initialize_Existing'Unrestricted_Access);
   Add_Func ("/pixbuf/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func
     ("/pixbuf/constructors-invalid",
      Test_Constructors_Invalid'Unrestricted_Access);
   Add_Func ("/pixbuf/xpm", Test_Xpm'Unrestricted_Access);
   Add_Func
     ("/pixbuf/transforms-options",
      Test_Transforms_And_Options'Unrestricted_Access);
   Add_Func ("/pixbuf/rowstride", Test_Rowstride'Unrestricted_Access);
   Add_Func
     ("/pixbuf/streams-async",
      Test_Streams_And_Async'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Pixbuf;
