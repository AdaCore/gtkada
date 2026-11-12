--  Tests GdkTexture construction, pixel access, paintable behavior,
--  serialization, and GLoadableIcon loading. The bundled resource path is
--  also used by testsuite/c_tests/textbuffer.c, and PNG saving follows the
--  helper in testsuite/c_tests/symbolic.c; no complete C case is ported.

--  The valid PNG is produced in memory (pixbuf -> texture -> PNG bytes), so
--  no data file is needed. Decoding a texture needs no display, and no
--  widget is created, so this test needs no display.

with Ada.Command_Line;
with Ada.Directories;

with Glib;         use Glib;
with Glib.Bytes;   use Glib.Bytes;
with Glib.Cancellable;
with Glib.Error;   use Glib.Error;
with Glib.GFile;
with Glib.Input_Stream;
with Glib.Main;
with Glib.Object;  use Glib.Object;
with Glib.Test;    use Glib.Test;
with Glib.Types;
with Gdk.Color_State;
with Gdk.Paintable;
with Gdk.Pixbuf;   use Gdk.Pixbuf;
with Gdk.Snapshot;
with Gdk.Texture;  use Gdk.Texture;
with Gsk.Render_Node;
with Gtk.Main;
with Gtk.Snapshot;
with Gtkada.Types;
with System;       use System;

procedure Main is
   use type Gdk.Color_State.Gdk_Color_State;
   use type Gdk.Paintable.Gdk_Paintable;
   use type Gdk.Paintable.Gdk_Paintable_Flags;
   use type Gdk.Texture.Gdk_Memory_Format;
   use type Gdk.Texture.Gdk_Texture;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type Glib.Input_Stream.Ginput_Stream;

   type Route is (Procedure_Form, Function_Form, Initialize_Form);
   --  The three Ada subprograms generated for each C constructor

   Width  : constant := 3;
   Height : constant := 2;

   Missing_Path : constant String := "/no/such/file-gtkada-texture-test.png";
   Resource_Path : constant String :=
     "/org/gtk/libgtk/icons/16x16/status/image-missing.png";

   Async_Texture : Gdk_Texture;
   Async_Done : Boolean := False;
   Async_Source_Matches : Boolean := False;
   Async_Error : GError;
   Async_Stream : Glib.Input_Stream.Ginput_Stream;

   --  The generic paintable signal callbacks check both the emitted interface
   --  and the GObject supplied as the callback slot.
   Expected_Paintable : Gdk.Paintable.Gdk_Paintable;
   Expected_Slot : Gtk.Snapshot.Gtk_Snapshot;
   Typed_Invalidate_Calls : Natural := 0;
   Slot_Invalidate_Calls : Natural := 0;
   Typed_Exception_Calls : Natural := 0;
   Slot_Exception_Calls : Natural := 0;

   --  GdkPaintable's two parameterless signals can be emitted directly to
   --  test their generated signal adapters deterministically.
   procedure Emit_Paintable_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Paintable_Invalidated
     (Self : Gdk.Paintable.Gdk_Paintable);
   procedure On_Paintable_Invalidated_Slot
     (Self : access Glib.Object.GObject_Record'Class);
   procedure Raise_From_Paintable_Callback
     (Self : Gdk.Paintable.Gdk_Paintable);
   procedure Raise_From_Paintable_Slot
     (Self : access Glib.Object.GObject_Record'Class);

   function Png_Bytes return Gbytes;
   --  A valid Width x Height PNG, encoded in memory

   function Garbage_Bytes return Gbytes;
   --  Bytes that are not a valid image of any supported format, built in
   --  memory (copying constructor - alignment-safe regardless of this
   --  array's own alignment).

   function Png_File return String;
   --  Write Png_Bytes' image to a file in the current directory and return
   --  its full name. The caller deletes it.

   procedure Release (File : Glib.GFile.Gfile);

   function From_Bytes
     (Via   : Route;
      Bytes : Gbytes;
      Error : out GError) return Gdk_Texture;

   function From_File
     (Via   : Route;
      File  : Glib.GFile.Gfile;
      Error : out GError) return Gdk_Texture;

   function From_Filename
     (Via   : Route;
      Path  : String;
      Error : out GError) return Gdk_Texture;

   procedure Check_Success (Texture : in out Gdk_Texture; Error : GError);
   --  Texture was decoded from Png_Bytes' image. Unrefs it.

   procedure Check_Failure
     (Via     : Route;
      Texture : in out Gdk_Texture;
      Error   : in out GError);
   --  The constructor failed. Frees Error, and Texture if still allocated.

   procedure Test_From_Bytes
   with Convention => C;

   procedure Test_From_Bytes_Invalid
   with Convention => C;

   procedure Test_From_File
   with Convention => C;

   procedure Test_From_File_Missing
   with Convention => C;

   procedure Test_From_Filename
   with Convention => C;

   procedure Test_From_Filename_Missing
   with Convention => C;

   procedure Test_Initialize_Already_Created
   with Convention => C;

   procedure Test_Pixbuf_And_Paintable
   with Convention => C;

   procedure Test_Empty_Paintable_And_Signals
   with Convention => C;

   procedure Test_From_Resource
   with Convention => C;

   procedure Test_Save_And_Loadable_Icon
   with Convention => C;

   procedure On_Icon_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure Wait_For_Icon_Load;

   --  Gdk.Texture's C callback bridge calls this adapter with a temporary
   --  GAsyncResult, which must be finished before the callback returns.
   procedure On_Icon_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result) is
   begin
      Async_Source_Matches :=
        Source_Object.Get_Object = Async_Texture.Get_Object;
      Async_Stream := Async_Texture.Load_Finish (Res, Error => Async_Error);
      Async_Done := True;
   end On_Icon_Ready;

   --  The typed overload receives the paintable that emitted the signal.
   procedure On_Paintable_Invalidated
     (Self : Gdk.Paintable.Gdk_Paintable) is
   begin
      Assert_True (Self = Expected_Paintable);
      Typed_Invalidate_Calls := Typed_Invalidate_Calls + 1;
   end On_Paintable_Invalidated;

   --  The slotted overload receives the object retained by its closure.
   procedure On_Paintable_Invalidated_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Self.Get_Object = Expected_Slot.Get_Object);
      Slot_Invalidate_Calls := Slot_Invalidate_Calls + 1;
   end On_Paintable_Invalidated_Slot;

   --  Signal marshallers must contain Ada exceptions before control returns
   --  through GLib's C callback boundary.
   procedure Raise_From_Paintable_Callback
     (Self : Gdk.Paintable.Gdk_Paintable) is
   begin
      Assert_True (Self = Expected_Paintable);
      Typed_Exception_Calls := Typed_Exception_Calls + 1;
      raise Program_Error with "paintable typed callback containment";
   end Raise_From_Paintable_Callback;

   procedure Raise_From_Paintable_Slot
     (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Assert_True (Self.Get_Object = Expected_Slot.Get_Object);
      Slot_Exception_Calls := Slot_Exception_Calls + 1;
      raise Program_Error with "paintable slot callback containment";
   end Raise_From_Paintable_Slot;

   --  Drive the default context until the asynchronous load invokes its
   --  callback; the bounded loop prevents a broken binding from hanging.
   procedure Wait_For_Icon_Load is
   begin
      for Attempt in 1 .. 300 loop
         exit when Async_Done;
         if not Glib.Main.Main_Context_Iteration (May_Block => False) then
            delay 0.01;
         end if;
      end loop;
      Assert_True (Async_Done);
      Assert_True (Async_Source_Matches);
   end Wait_For_Icon_Load;

   ---------------
   -- Png_Bytes --
   ---------------

   function Png_Bytes return Gbytes is
      Pixbuf  : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, Width, Height);
      Texture : Gdk_Texture;
      Result  : Gbytes;
   begin
      Pixbuf.Fill (16#112233FF#);
      Texture := Gdk_Texture_New_For_Pixbuf (Pixbuf);
      Result := Texture.Save_To_Png_Bytes;
      Unref (Texture);
      Unref (Pixbuf);
      return Result;
   end Png_Bytes;

   -------------------
   -- Garbage_Bytes --
   -------------------

   function Garbage_Bytes return Gbytes is
      Data   : constant Guint8_Array (0 .. 15) := (others => 16#AA#);
      Result : Gbytes;
   begin
      G_New (Result, Data, Data'Length);
      return Result;
   end Garbage_Bytes;

   --------------
   -- Png_File --
   --------------

   function Png_File return String is
      Path    : constant String :=
        Ada.Directories.Full_Name ("gtkada-texture-test.png");
      Bytes   : Gbytes := Png_Bytes;
      Error   : GError;
      Texture : Gdk_Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
   begin
      Assert_True (Error = null);
      Assert_True (Texture.Save_To_Png (Path));
      Unref (Texture);
      Unref (Bytes);
      return Path;
   end Png_File;

   -------------
   -- Release --
   -------------

   procedure Release (File : Glib.GFile.Gfile) is
   begin
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Release;

   ----------------
   -- From_Bytes --
   ----------------

   function From_Bytes
     (Via   : Route;
      Bytes : Gbytes;
      Error : out GError) return Gdk_Texture
   is
      Texture : Gdk_Texture;
   begin
      case Via is
         when Procedure_Form =>
            Gdk_New_From_Bytes (Texture, Bytes, Error);
         when Function_Form =>
            Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
         when Initialize_Form =>
            Texture := new Gdk_Texture_Record;
            Initialize_From_Bytes (Texture, Bytes, Error);
      end case;
      return Texture;
   end From_Bytes;

   ---------------
   -- From_File --
   ---------------

   function From_File
     (Via   : Route;
      File  : Glib.GFile.Gfile;
      Error : out GError) return Gdk_Texture
   is
      Texture : Gdk_Texture;
   begin
      case Via is
         when Procedure_Form =>
            Gdk_New_From_File (Texture, File, Error);
         when Function_Form =>
            Texture := Gdk_Texture_New_From_File (File, Error);
         when Initialize_Form =>
            Texture := new Gdk_Texture_Record;
            Initialize_From_File (Texture, File, Error);
      end case;
      return Texture;
   end From_File;

   -------------------
   -- From_Filename --
   -------------------

   function From_Filename
     (Via   : Route;
      Path  : String;
      Error : out GError) return Gdk_Texture
   is
      Texture : Gdk_Texture;
   begin
      case Via is
         when Procedure_Form =>
            Gdk_New_From_Filename (Texture, Path, Error);
         when Function_Form =>
            Texture := Gdk_Texture_New_From_Filename (Path, Error);
         when Initialize_Form =>
            Texture := new Gdk_Texture_Record;
            Initialize_From_Filename (Texture, Path, Error);
      end case;
      return Texture;
   end From_Filename;

   -------------------
   -- Check_Success --
   -------------------

   procedure Check_Success (Texture : in out Gdk_Texture; Error : GError) is
   begin
      Assert_True (Error = null);
      Assert_True (Texture /= null);
      Assert_True (Texture.Is_Created);
      Assert_True (Texture.Get_Width = Width);
      Assert_True (Texture.Get_Height = Height);
      Unref (Texture);
   end Check_Success;

   -------------------
   -- Check_Failure --
   -------------------

   procedure Check_Failure
     (Via     : Route;
      Texture : in out Gdk_Texture;
      Error   : in out GError) is
   begin
      Assert_True (Error /= null);
      Assert_True (Get_Message (Error)'Length > 0);
      Error_Free (Error);

      case Via is
         when Procedure_Form | Function_Form =>
            --  The generated constructor frees its record and returns null
            Assert_True (Texture = null);
         when Initialize_Form =>
            --  The caller's record is left allocated but not created
            Assert_True (Texture /= null);
            Assert_False (Texture.Is_Created);
            Texture.Deallocate;
            Texture := null;
      end case;
   end Check_Failure;

   ---------------------
   -- Test_From_Bytes --
   ---------------------

   procedure Test_From_Bytes is
      Bytes   : Gbytes := Png_Bytes;
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Bytes (Via, Bytes, Error);
         Check_Success (Texture, Error);
      end loop;
      Unref (Bytes);
   end Test_From_Bytes;

   -----------------------------
   -- Test_From_Bytes_Invalid --
   -----------------------------

   procedure Test_From_Bytes_Invalid is
      Bytes   : Gbytes := Garbage_Bytes;
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Bytes (Via, Bytes, Error);
         Check_Failure (Via, Texture, Error);
      end loop;
      Unref (Bytes);
   end Test_From_Bytes_Invalid;

   --------------------
   -- Test_From_File --
   --------------------

   procedure Test_From_File is
      Path    : constant String := Png_File;
      File    : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Path);
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_File (Via, File, Error);
         Check_Success (Texture, Error);
      end loop;
      Release (File);
      Ada.Directories.Delete_File (Path);
   end Test_From_File;

   ----------------------------
   -- Test_From_File_Missing --
   ----------------------------

   procedure Test_From_File_Missing is
      File    : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Missing_Path);
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_File (Via, File, Error);
         Check_Failure (Via, Texture, Error);
      end loop;
      Release (File);
   end Test_From_File_Missing;

   ------------------------
   -- Test_From_Filename --
   ------------------------

   procedure Test_From_Filename is
      Path    : constant String := Png_File;
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Filename (Via, Path, Error);
         Check_Success (Texture, Error);
      end loop;
      Ada.Directories.Delete_File (Path);
   end Test_From_Filename;

   --------------------------------
   -- Test_From_Filename_Missing --
   --------------------------------

   procedure Test_From_Filename_Missing is
      Error   : GError;
      Texture : Gdk_Texture;
   begin
      for Via in Route loop
         Texture := From_Filename (Via, Missing_Path, Error);
         Check_Failure (Via, Texture, Error);
      end loop;
   end Test_From_Filename_Missing;

   -------------------------------------
   -- Test_Initialize_Already_Created --
   -------------------------------------

   procedure Test_Initialize_Already_Created is
      --  Initialize_* on an existing texture is a no-op: it must clear a
      --  stale Error and leave the object alone. The inputs are invalid,
      --  so a loader that did run would set Error.
      Bytes       : Gbytes := Png_Bytes;
      Garbage     : Gbytes := Garbage_Bytes;
      File        : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Missing_Path);
      Error       : GError;
      Stale_Error : GError;
      Texture     : Gdk_Texture;
      Failed      : Gdk_Texture;
      Original    : System.Address;
   begin
      --  Any real error will do as the stale value
      Failed := Gdk_Texture_New_From_Bytes (Garbage, Stale_Error);
      Assert_True (Failed = null and Stale_Error /= null);

      Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
      Assert_True (Error = null);
      Original := Texture.Get_Object;

      for Step in 1 .. 3 loop
         Error := Stale_Error;
         case Step is
            when 1 => Initialize_From_Bytes (Texture, Garbage, Error);
            when 2 => Initialize_From_File (Texture, File, Error);
            when 3 => Initialize_From_Filename (Texture, Missing_Path, Error);
         end case;
         Assert_True (Error = null);
         Assert_True (Texture.Get_Object = Original);
      end loop;

      Error_Free (Stale_Error);
      Unref (Texture);
      Release (File);
      Unref (Garbage);
      Unref (Bytes);
   end Test_Initialize_Already_Created;

   --------------------------------
   -- Test_Pixbuf_And_Paintable --
   --------------------------------

   procedure Test_Pixbuf_And_Paintable is
      Pixbuf : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, Width, Height);
      Texture : Gdk_Texture;
      Initialized : aliased Gdk_Texture_Record;
      Paintable : Gdk.Paintable.Gdk_Paintable;
      Current : Gdk.Paintable.Gdk_Paintable;
      Color_State : Gdk.Color_State.Gdk_Color_State;
      Pixel_Data : Guint8_Array (0 .. Width * Height * 4 - 1);
      Original : System.Address;
      Concrete_Width, Concrete_Height : Gdouble;
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      Pixbuf.Fill (16#112233FF#);

      --  Exercise the procedure constructor and the initialize-once form.
      Gdk_New_For_Pixbuf (Texture, Pixbuf);
      Assert_True (Texture.Is_Created);
      Assert_True (Texture.Get_Width = Width and Texture.Get_Height = Height);
      Unref (Texture);

      Initialize_For_Pixbuf (Initialized'Access, Pixbuf);
      Texture := Initialized'Unchecked_Access;
      Assert_True (Texture.Is_Created);
      Original := Texture.Get_Object;
      Initialize_For_Pixbuf (Texture, Pixbuf);
      Assert_True (Texture.Get_Object = Original);

      --  A texture downloads in Cairo ARGB32 order; the four nonzero source
      --  channels remain present on either byte order supported by the host.
      Texture.Download (Pixel_Data, Width * 4);
      for Pixel of Pixel_Data loop
         Assert_True
           (Pixel in 16#11# | 16#22# | 16#33# | 16#FF#);
      end loop;

      --  The format and color state describe this usable texture, and the
      --  paintable interface reports its immutable dimensions and ratio.
      Assert_True (Texture.Get_Format /= Gdk_Memory_N_Formats);
      Color_State := Texture.Get_Color_State;
      Assert_True
        (Color_State /= Gdk.Color_State.Null_Gdk_Color_State);
      Paintable := Gdk.Texture."+" (Texture);
      Assert_True
        (Texture.Get_Flags =
           Gdk.Paintable.Gdk_Paintable_Static_Size +
             Gdk.Paintable.Gdk_Paintable_Static_Contents);
      Assert_True (Texture.Get_Intrinsic_Width = Width);
      Assert_True (Texture.Get_Intrinsic_Height = Height);
      Assert_True (Texture.Get_Intrinsic_Aspect_Ratio = 1.5);
      Current := Texture.Get_Current_Image;
      Assert_True (Gdk.Texture."-" (Current) = Texture);

      --  Concrete sizing preserves the 3:2 source ratio when only width is
      --  specified. Gdk.Texture has static contents, so GDK intentionally
      --  rejects its paintable invalidation methods with fatal criticals.
      Texture.Compute_Concrete_Size
        (6.0, 0.0, 3.0, 2.0, Concrete_Width, Concrete_Height);
      Assert_True (Concrete_Width = 6.0 and Concrete_Height = 4.0);
      --  Snapshotting a texture creates a render node without displaying it.
      Texture.Snapshot (Snapshot, 6.0, 4.0);
      Node := Snapshot.To_Node;
      Assert_True (Node /= null);

      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Current)));
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      Unref (Pixbuf);
      --  Initialized owns its texture reference and releases it at scope exit.
   end Test_Pixbuf_And_Paintable;

   -------------------------------------------
   -- Test_Empty_Paintable_And_Signals --
   -------------------------------------------

   --  Exercise the Gdk.Paintable body wrappers with a static, display-free
   --  paintable. Signals are emitted explicitly because this static object
   --  correctly rejects the public invalidation methods.
   procedure Test_Empty_Paintable_And_Signals is
      Paintable : constant Gdk.Paintable.Gdk_Paintable :=
        Gdk.Paintable.New_Empty (4, 3);
      Current : Gdk.Paintable.Gdk_Paintable;
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Node : Gsk.Render_Node.Gsk_Render_Node;
      Contents_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("invalidate-contents");
      Size_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("invalidate-size");
   begin
      Assert_True (Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Gdk.Paintable."+" (Paintable) = Paintable);

      --  An immutable empty paintable returns itself as its current image.
      Current := Gdk.Paintable.Get_Current_Image (Paintable);
      Assert_True (Current = Paintable);
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Current)));

      --  Snapshotting an empty paintable leaves the snapshot without a node.
      Gdk.Paintable.Snapshot (Paintable, Snapshot, 12.0, 9.0);
      Node := Gtk.Snapshot.To_Node (Snapshot);
      Assert_True (Node = null);

      Expected_Paintable := Paintable;
      Expected_Slot := Snapshot;
      Typed_Invalidate_Calls := 0;
      Slot_Invalidate_Calls := 0;
      Typed_Exception_Calls := 0;
      Slot_Exception_Calls := 0;
      Gdk.Paintable.On_Invalidate_Contents
        (Paintable, On_Paintable_Invalidated'Unrestricted_Access,
         After => True);
      Gdk.Paintable.On_Invalidate_Contents
        (Paintable, On_Paintable_Invalidated_Slot'Unrestricted_Access,
         Slot => Snapshot);
      Gdk.Paintable.On_Invalidate_Size
        (Paintable, On_Paintable_Invalidated'Unrestricted_Access);
      Gdk.Paintable.On_Invalidate_Size
        (Paintable, On_Paintable_Invalidated_Slot'Unrestricted_Access,
         Slot => Snapshot, After => True);
      Gdk.Paintable.On_Invalidate_Contents
        (Paintable, Raise_From_Paintable_Callback'Unrestricted_Access);
      Gdk.Paintable.On_Invalidate_Contents
        (Paintable, Raise_From_Paintable_Slot'Unrestricted_Access,
         Slot => Snapshot);
      Gdk.Paintable.On_Invalidate_Size
        (Paintable, Raise_From_Paintable_Callback'Unrestricted_Access);
      Gdk.Paintable.On_Invalidate_Size
        (Paintable, Raise_From_Paintable_Slot'Unrestricted_Access,
         Slot => Snapshot);

      --  Direct signal emission avoids relying on file monitors, theme reloads
      --  or a display event to drive these parameterless interface signals.
      Emit_Paintable_Signal (System.Address (Paintable), Contents_Name);
      Emit_Paintable_Signal (System.Address (Paintable), Size_Name);
      Assert_Cmpint_Eq (Glib.Gint (Typed_Invalidate_Calls), 2);
      Assert_Cmpint_Eq (Glib.Gint (Slot_Invalidate_Calls), 2);
      Assert_Cmpint_Eq (Glib.Gint (Typed_Exception_Calls), 2);
      Assert_Cmpint_Eq (Glib.Gint (Slot_Exception_Calls), 2);

      Gtkada.Types.Free (Contents_Name);
      Gtkada.Types.Free (Size_Name);
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Paintable)));
      Expected_Paintable := Gdk.Paintable.Null_Gdk_Paintable;
      Expected_Slot := null;
   end Test_Empty_Paintable_And_Signals;

   -------------------------
   -- Test_From_Resource --
   -------------------------

   procedure Test_From_Resource is
      Texture : Gdk_Texture;
      Initialized : aliased Gdk_Texture_Record;
      Original : System.Address;
   begin
      --  This installed GTK resource is also used by the C textbuffer test.
      Gdk_New_From_Resource (Texture, Resource_Path);
      Assert_True
        (Texture.Is_Created and then Texture.Get_Width > 0
           and then Texture.Get_Height > 0);
      Unref (Texture);

      Texture := Gdk_Texture_New_From_Resource (Resource_Path);
      Assert_True
        (Texture.Is_Created and then Texture.Get_Width > 0
           and then Texture.Get_Height > 0);
      Unref (Texture);

      Initialize_From_Resource (Initialized'Access, Resource_Path);
      Texture := Initialized'Unchecked_Access;
      Assert_True (Texture.Is_Created);
      Original := Texture.Get_Object;
      --  An initialized object ignores later resource paths.
      Initialize_From_Resource (Texture, Resource_Path);
      Assert_True (Texture.Get_Object = Original);
   end Test_From_Resource;

   -------------------------------------
   -- Test_Save_And_Loadable_Icon --
   -------------------------------------

   procedure Test_Save_And_Loadable_Icon is
      Bytes : Gbytes := Png_Bytes;
      Error : GError;
      Texture : Gdk_Texture := Gdk_Texture_New_From_Bytes (Bytes, Error);
      Png_Bytes_Out : Gbytes;
      Tiff_Bytes_Out : Gbytes;
      Png_Roundtrip : Gdk_Texture;
      Tiff_Roundtrip : Gdk_Texture;
      Png_Path : constant String :=
        Ada.Directories.Full_Name ("gtkada-texture-save.png");
      Tiff_Path : constant String :=
        Ada.Directories.Full_Name ("gtkada-texture-save.tiff");
      Sync_Stream : Glib.Input_Stream.Ginput_Stream;
      Cancellable : Glib.Cancellable.Gcancellable :=
        Glib.Cancellable.Gcancellable_New;
   begin
      Assert_True (Error = null and Texture /= null);

      --  PNG and TIFF can be serialized to bytes and decoded again.
      Png_Bytes_Out := Texture.Save_To_Png_Bytes;
      Assert_True (Png_Bytes_Out.Get_Size > 0);
      Png_Roundtrip := Gdk_Texture_New_From_Bytes (Png_Bytes_Out, Error);
      Assert_True
        (Error = null and then Png_Roundtrip.Get_Width = Width);
      Unref (Png_Roundtrip);

      Tiff_Bytes_Out := Texture.Save_To_Tiff_Bytes;
      Assert_True (Tiff_Bytes_Out.Get_Size > 0);
      Tiff_Roundtrip := Gdk_Texture_New_From_Bytes (Tiff_Bytes_Out, Error);
      Assert_True
        (Error = null and then Tiff_Roundtrip.Get_Height = Height);
      Unref (Tiff_Roundtrip);

      --  File saves report success for writable paths and failure when the
      --  parent directory does not exist.
      Assert_True (Texture.Save_To_Png (Png_Path));
      Assert_True (Texture.Save_To_Tiff (Tiff_Path));
      Assert_False
        (Texture.Save_To_Png ("/no/such/directory/gtkada-texture.png"));
      Assert_False
        (Texture.Save_To_Tiff ("/no/such/directory/gtkada-texture.tiff"));
      Ada.Directories.Delete_File (Png_Path);
      Ada.Directories.Delete_File (Tiff_Path);

      --  LoadableIcon.Load returns a readable stream. Omit the optional type
      --  output because its Ada binding cannot safely resize a String result.
      Sync_Stream := Texture.Load (32, null, null, Error);
      Assert_True (Error = null and Sync_Stream /= null);
      Glib.Object.Unref (Glib.Object.GObject (Sync_Stream));

      --  Finish a successful asynchronous request while its result is live.
      Async_Texture := Texture;
      Async_Done := False;
      Async_Error := null;
      Texture.Load_Async (32, null, On_Icon_Ready'Unrestricted_Access);
      Wait_For_Icon_Load;
      Assert_True (Async_Error = null and Async_Stream /= null);
      Glib.Object.Unref (Glib.Object.GObject (Async_Stream));

      --  Cancellation reaches Load_Finish as an error with no type output.
      Cancellable.Cancel;
      Async_Done := False;
      Async_Error := null;
      Async_Stream := null;
      Texture.Load_Async
        (32, Cancellable, On_Icon_Ready'Unrestricted_Access);
      Wait_For_Icon_Load;
      Assert_True (Async_Error /= null and Async_Stream = null);
      Error_Free (Async_Error);

      Glib.Object.Unref (Glib.Object.GObject (Cancellable));
      Unref (Texture);
      Unref (Png_Bytes_Out);
      Unref (Tiff_Bytes_Out);
      Unref (Bytes);
   end Test_Save_And_Loadable_Icon;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/gdk-texture/new-from-bytes", Test_From_Bytes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-bytes-invalid",
      Test_From_Bytes_Invalid'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-file", Test_From_File'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-file-missing",
      Test_From_File_Missing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-filename",
      Test_From_Filename'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/new-from-filename-missing",
      Test_From_Filename_Missing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/initialize-already-created",
      Test_Initialize_Already_Created'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/pixbuf-and-paintable",
      Test_Pixbuf_And_Paintable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/empty-paintable-and-signals",
      Test_Empty_Paintable_And_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/from-resource",
      Test_From_Resource'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gdk-texture/save-and-loadable-icon",
      Test_Save_And_Loadable_Icon'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
