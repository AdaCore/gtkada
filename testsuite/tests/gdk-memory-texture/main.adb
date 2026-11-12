--  Exercise Gdk.Memory_Texture construction, paintable behavior, and the
--  GLoadableIcon sync and async paths. No C test covers this unit directly.

with Ada.Command_Line;
with Gdk.Memory_Texture;
with Gdk.Paintable;
with Gdk.Snapshot;
with Gdk.Texture;
with Gsk.Render_Node;
with Glib;
with Glib.Bytes;
with Glib.Cancellable;
with Glib.Error;
with Glib.GFile;
with Glib.Input_Stream;
with Glib.Loadable_Icon;
with Glib.Main;
with Glib.Object;
with Glib.Test;
with Glib.Types;
with Gtk.Main;
with Gtk.Snapshot;
with System;

procedure Main is
   use Glib;
   use Glib.Test;
   use Gdk.Memory_Texture;
   use Gdk.Texture;
   use type Gdk.Memory_Texture.Gdk_Memory_Texture;
   use type Gdk.Paintable.Gdk_Paintable;
   use type Gdk.Paintable.Gdk_Paintable_Flags;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type Glib.Error.GError;
   use type Glib.Loadable_Icon.Gloadable_Icon;
   use type System.Address;
   use type Glib.Input_Stream.Ginput_Stream;

   Data : constant Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
     ((16#11#, 16#22#, 16#33#, 16#FF#, 16#44#, 16#55#, 16#66#, 16#FF#), 8);
   Texture_Record : aliased Gdk.Memory_Texture.Gdk_Memory_Texture_Record;
   Texture : Gdk.Memory_Texture.Gdk_Memory_Texture;
   Async_Done : Boolean := False;
   Async_Source_Matches : Boolean := False;
   Async_Expect_Error : Boolean := False;
   Async_Error : Glib.Error.GError;
   Async_Stream : Glib.Input_Stream.Ginput_Stream;
   Async_Type : access UTF8_String;
   Async_Icon : Glib.Loadable_Icon.Gloadable_Icon :=
     Glib.Loadable_Icon.Null_Gloadable_Icon;

   --  GFileIcon is not exposed by GtkAda, so use its GLib constructor to
   --  create a real loadable icon for the missing-file error case.
   function New_File_Icon (File : System.Address) return System.Address;
   pragma Import (C, New_File_Icon, "g_file_icon_new");

   --  The adapter completes Load_Finish while its GAsyncResult is valid.
   procedure On_Load_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure Test_Texture_Behavior with Convention => C;

   procedure On_Load_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result) is
   begin
      Async_Source_Matches :=
        Source_Object.Get_Object = Glib.Object.Get_Object (Texture);
      if Async_Expect_Error then
         Async_Stream := Glib.Loadable_Icon.Load_Finish
           (Async_Icon, Res, Error => Async_Error);
      else
         Async_Stream := Glib.Loadable_Icon.Load_Finish
           (Async_Icon, Res, Async_Type, Async_Error);
      end if;
      Async_Done := True;
   end On_Load_Ready;

   --  Iterate the main context until the asynchronous icon operation returns.
   procedure Wait_For_Load is
   begin
      for Attempt in 1 .. 300 loop
         exit when Async_Done;
         if not Glib.Main.Main_Context_Iteration (May_Block => False) then
            delay 0.01;
         end if;
      end loop;
      Assert_True (Async_Done);
      Assert_True (Async_Source_Matches);
   end Wait_For_Load;

      --  Check constructors, the already-created Initialize no-op, both
      --  interface conversions, paintable properties, and snapshot behavior.
   procedure Test_Texture_Behavior is
      Function_Texture : Gdk.Memory_Texture.Gdk_Memory_Texture;
      Procedure_Texture : Gdk.Memory_Texture.Gdk_Memory_Texture;
      Paintable : Gdk.Paintable.Gdk_Paintable;
      Icon : Glib.Loadable_Icon.Gloadable_Icon;
      Current : Gdk.Paintable.Gdk_Paintable;
      Concrete_Width, Concrete_Height : Gdouble;
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Node : Gsk.Render_Node.Gsk_Render_Node;
      Flags : Gdk.Paintable.Gdk_Paintable_Flags;
      Sync_Stream : Glib.Input_Stream.Ginput_Stream;
      Error : Glib.Error.GError;
      Cancellable : Glib.Cancellable.Gcancellable :=
        Glib.Cancellable.Gcancellable_New;
      Missing_File : Glib.GFile.Gfile;
      Missing_Icon : Glib.Loadable_Icon.Gloadable_Icon;
   begin
      --  Both constructors initialize uncreated Ada wrappers; retain a
      --  separate aliased record so Load_Async resolves to the derived binding.
      Function_Texture := Gdk.Memory_Texture.Gdk_Memory_Texture_New
        (2, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Data, 8);
      Assert_True (Glib.Object.Is_Created (Function_Texture.all));
      Assert_True
        (Glib.Object.Get_Type (Glib.Object.GObject (Function_Texture)) =
           Gdk.Memory_Texture.Get_Type);

      Gdk.Memory_Texture.Initialize
        (Texture_Record'Access, 2, 1,
         Gdk.Texture.Gdk_Memory_R8G8B8A8, Data, 8);
      Texture := Texture_Record'Unchecked_Access;
      Paintable := Gdk.Memory_Texture."+" (Texture);
      Icon := Gdk.Memory_Texture."+" (Texture);
      Async_Icon := Glib.Loadable_Icon."+" (Icon);
      Assert_True (Async_Icon = Icon);
      --  Initialize is a no-op for an object that already has a GObject.
      Gdk.Memory_Texture.Initialize
        (Texture_Record'Access, 7, 5,
         Gdk.Texture.Gdk_Memory_R8G8B8A8, Data, 8);
      Assert_True (Glib.Object.Is_Created (Texture_Record));
      Assert_True (Texture.Get_Intrinsic_Width = 2);
      Assert_True (Texture.Get_Intrinsic_Height = 1);

      --  The procedure constructor is a separate public construction path.
      Gdk.Memory_Texture.Gdk_New
        (Procedure_Texture, 2, 1, Gdk.Texture.Gdk_Memory_R8G8B8, Data, 8);
      Assert_True (Glib.Object.Is_Created (Procedure_Texture.all));
      Assert_True (Procedure_Texture.Get_Intrinsic_Aspect_Ratio = 2.0);

      --  Both implemented interfaces round-trip to the same Ada object.
      Assert_True (Gdk.Memory_Texture."-" (Paintable) = Texture);
      Assert_True (Gdk.Memory_Texture."-" (Icon) = Texture);

      --  Static memory textures report fixed size and fixed contents.
      Flags := Texture.Get_Flags;
      Assert_True
        (Flags = Gdk.Paintable.Gdk_Paintable_Static_Size +
           Gdk.Paintable.Gdk_Paintable_Static_Contents);
      Assert_True (Texture.Get_Intrinsic_Width = 2);
      Assert_True (Texture.Get_Intrinsic_Height = 1);
      Assert_True (Texture.Get_Intrinsic_Aspect_Ratio = 2.0);
      Current := Texture.Get_Current_Image;
      Assert_True (Gdk.Memory_Texture."-" (Current) = Texture);
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Current)));

      --  Supplying only a width preserves the reported aspect ratio.
      Texture.Compute_Concrete_Size
        (8.0, 0.0, 2.0, 1.0, Concrete_Width, Concrete_Height);
      Assert_True (Concrete_Width = 8.0 and Concrete_Height = 4.0);

      --  Snapshot creates a render node using the in-memory pixel data.
      Texture.Snapshot (Snapshot, 2.0, 1.0);
      Node := Snapshot.To_Node;
      Assert_True (Node /= null);

      --  Memory textures do not set a content type, so pass the zero-length
      --  buffer matching the empty string returned by this implementation.
      declare
         Sync_Type : aliased UTF8_String := "";
         Async_Type_Buffer : aliased UTF8_String := "";
      begin
         --  Synchronous loading returns a stream and the empty content type.
         Sync_Stream := Glib.Loadable_Icon.Load
           (Icon, 16, Sync_Type'Access, null, Error);
         Assert_True (Error = null and Sync_Stream /= null);
         Assert_True (Sync_Type = "");
         Glib.Object.Unref (Glib.Object.GObject (Sync_Stream));

         --  GFileIcon supplies the failing implementation needed to check
         --  Load's error result without depending on a real user file.
         Missing_File := Glib.GFile.New_For_Path
           ("/no/such/file-gtkada-loadable-icon-test.png");
         Missing_Icon := Glib.Loadable_Icon.Gloadable_Icon
           (New_File_Icon (System.Address (Missing_File)));
         Sync_Stream := Glib.Loadable_Icon.Load
           (Missing_Icon, 16, null, null, Error);
         Assert_True (Error /= null and Sync_Stream = null);
         Glib.Error.Error_Free (Error);
         Glib.Object.Unref
           (Glib.Types.To_Object (Glib.Types.GType_Interface (Missing_Icon)));
         Glib.Object.Unref
           (Glib.Types.To_Object (Glib.Types.GType_Interface (Missing_File)));

         --  The base interface callback finishes the operation while its
         --  result lives, covering GtkAda's callback conversion bridge.
         Async_Done := False;
         Async_Expect_Error := False;
         Async_Error := null;
         Async_Type := Async_Type_Buffer'Unchecked_Access;
         Glib.Loadable_Icon.Load_Async
           (Icon, 16, null, On_Load_Ready'Unrestricted_Access);
         Wait_For_Load;
         Assert_True (Async_Error = null and Async_Stream /= null);
         Assert_True (Async_Type_Buffer = "");
         Glib.Object.Unref (Glib.Object.GObject (Async_Stream));
         Async_Type := null;
      end;

      --  A pre-cancelled operation returns a GIO error and a null stream.
      Cancellable.Cancel;
      Async_Done := False;
      Async_Expect_Error := True;
      Async_Error := null;
      Glib.Loadable_Icon.Load_Async
        (Icon, 16, Cancellable, On_Load_Ready'Unrestricted_Access);
      Wait_For_Load;
      Assert_True (Async_Error /= null and Async_Stream = null);
      Glib.Error.Error_Free (Async_Error);
      Glib.Object.Unref (Glib.Object.GObject (Cancellable));

      --  Release the objects whose constructors return owned references.
      Glib.Object.Unref (Glib.Object.GObject (Procedure_Texture));
      Glib.Object.Unref (Glib.Object.GObject (Function_Texture));
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      --  Texture_Record owns its GObject wrapper and releases it at scope exit.
   end Test_Texture_Behavior;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gdk-memory-texture/behavior",
      Test_Texture_Behavior'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
