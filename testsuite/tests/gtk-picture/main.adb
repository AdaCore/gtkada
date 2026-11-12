--  Gtk.Picture binding tests for empty, file, filename, paintable, pixbuf,
--  and resource construction; picture properties; and accessible methods.
--  GTK's C testsuite has no direct gtk_picture_* API test to port. Its
--  builder.c:test_picture case checks GtkBuilder's relative-file resolution,
--  so these tests exercise the Ada picture APIs directly instead.

with Ada.Command_Line;
with Ada.Directories;
with GNAT.Strings;

with Gdk.Paintable;
with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Gdk.Texture;     use Gdk.Texture;
with Glib;            use Glib;
with Glib.Error;      use Glib.Error;
with Glib.GFile;
with Glib.Object;     use Glib.Object;
with Glib.Resource;   use Glib.Resource;
with Glib.Test;       use Glib.Test;
with Glib.Types;
with Gtk.Accessible;  use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Main;
with Gtk.Picture;     use Gtk.Picture;
with Gtkada.Types;

use type Gdk.Paintable.Gdk_Paintable;
use type Glib.GFile.Gfile;
use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;

procedure Main is

   procedure Test_Constructors_And_Sources with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Accessibility with Convention => C;

   --  Savev requires parallel option arrays; this test uses no encoder
   --  options while writing its deterministic one-pixel PNG file.
   No_Options : constant GNAT.Strings.String_List (1 .. 0) := (others => null);

   procedure Test_Constructors_And_Sources is
      Filename : constant String := "gtk-picture-test.png";
      Resource_Path : constant String :=
        "/org/gtkada/coverage/gtk-picture/picture.png";
      Pixbuf : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 2);
      Texture : constant Gdk_Texture := Gdk_Texture_New_For_Pixbuf (Pixbuf);
      File : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path (Filename);
      Resource : Gresource;
      Error : GError;

      --  Exercise both public constructor forms for each supported source.
      Empty_Procedure : Gtk_Picture;
      Empty_Function : Gtk_Picture;
      File_Procedure : Gtk_Picture;
      File_Function : Gtk_Picture;
      Filename_Procedure : Gtk_Picture;
      Filename_Function : Gtk_Picture;
      Paintable_Procedure : Gtk_Picture;
      Paintable_Function : Gtk_Picture;
      Pixbuf_Procedure : Gtk_Picture;
      Pixbuf_Function : Gtk_Picture;
      Resource_Procedure : Gtk_Picture;
      Resource_Function : Gtk_Picture;

      --  Direct Initialize calls cover the in-place constructor API as well.
      Initialized_File : constant Gtk_Picture := new Gtk_Picture_Record;
      Initialized_Filename : constant Gtk_Picture := new Gtk_Picture_Record;
      Initialized_Paintable : constant Gtk_Picture := new Gtk_Picture_Record;
      Initialized_Pixbuf : constant Gtk_Picture := new Gtk_Picture_Record;
      Initialized_Resource : constant Gtk_Picture := new Gtk_Picture_Record;
      Changed : Gtk_Picture;
   begin
      --  The file-backed constructors below use a real image, so they can be
      --  checked without relying on GTK's broken-image fallback behavior.
      Pixbuf.Fill (16#336699FF#);
      Assert_True (Pixbuf.Savev (Filename, "png", No_Options, No_Options, Error));
      Assert_True (Error = null);

      --  GTK resolves resources through the process registry. Load and
      --  register a small bundle local to this test directory.
      Resource := Load ("picture.gresource", Error);
      Assert_True (Error = null and Resource /= Null_Gresource);
      Resource.Register;

      --  Function constructors run only after the local file exists and the
      --  resource bundle is registered.
      Empty_Function := Gtk_Picture_New;
      File_Function := Gtk_Picture_New_For_File (File);
      Filename_Function := Gtk_Picture_New_For_Filename (Filename);
      Paintable_Function := Gtk_Picture_New_For_Paintable (+Texture);
      Pixbuf_Function := Gtk_Picture_New_For_Pixbuf (Pixbuf);
      Resource_Function := Gtk_Picture_New_For_Resource (Resource_Path);
      Changed := Gtk_Picture_New;

      Gtk_New (Empty_Procedure);
      Gtk_New_For_File (File_Procedure, File);
      Gtk_New_For_Filename (Filename_Procedure, Filename);
      Gtk_New_For_Paintable (Paintable_Procedure, +Texture);
      Gtk_New_For_Pixbuf (Pixbuf_Procedure, Pixbuf);
      Gtk_New_For_Resource (Resource_Procedure, Resource_Path);

      --  Ref_Sink each floating widget before later releasing its own
      --  reference. The input pixbuf, texture, file, and resource are owned
      --  separately and stay alive for the duration of the assertions.
      Ref_Sink (Empty_Procedure);
      Ref_Sink (Empty_Function);
      Ref_Sink (File_Procedure);
      Ref_Sink (File_Function);
      Ref_Sink (Filename_Procedure);
      Ref_Sink (Filename_Function);
      Ref_Sink (Paintable_Procedure);
      Ref_Sink (Paintable_Function);
      Ref_Sink (Pixbuf_Procedure);
      Ref_Sink (Pixbuf_Function);
      Ref_Sink (Resource_Procedure);
      Ref_Sink (Resource_Function);
      Ref_Sink (Changed);

      --  Empty constructors begin with no paintable; file and filename
      --  constructors retain a GFile describing their source.
      Assert_True (Empty_Procedure.Get_Paintable = Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Empty_Function.Get_Paintable = Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (File_Procedure.Get_File /= Glib.GFile.Null_Gfile);
      Assert_True (File_Function.Get_File /= Glib.GFile.Null_Gfile);
      Assert_True (Filename_Procedure.Get_File /= Glib.GFile.Null_Gfile);
      Assert_True (Filename_Function.Get_File /= Glib.GFile.Null_Gfile);

      --  The paintable and pixbuf constructors both expose the supplied image
      --  through Gtk.Picture.Get_Paintable.
      Assert_True (Paintable_Procedure.Get_Paintable = +Texture);
      Assert_True (Paintable_Function.Get_Paintable = +Texture);
      Assert_True (Pixbuf_Procedure.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Pixbuf_Function.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Resource_Procedure.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Resource_Function.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);

      --  Initialize* builds a widget in-place. These objects start as Ada
      --  records without a GObject, so the generated wrappers call GTK.
      Gtk.Picture.Initialize (Empty_Procedure);
      Gtk.Picture.Initialize_For_File (Initialized_File, File);
      Gtk.Picture.Initialize_For_Filename (Initialized_Filename, Filename);
      Gtk.Picture.Initialize_For_Paintable (Initialized_Paintable, +Texture);
      Gtk.Picture.Initialize_For_Pixbuf (Initialized_Pixbuf, Pixbuf);
      Gtk.Picture.Initialize_For_Resource (Initialized_Resource, Resource_Path);
      Ref_Sink (Initialized_File);
      Ref_Sink (Initialized_Filename);
      Ref_Sink (Initialized_Paintable);
      Ref_Sink (Initialized_Pixbuf);
      Ref_Sink (Initialized_Resource);
      Assert_True (Initialized_File.Get_File /= Glib.GFile.Null_Gfile);
      Assert_True (Initialized_Filename.Get_File /= Glib.GFile.Null_Gfile);
      Assert_True (Initialized_Paintable.Get_Paintable = +Texture);
      Assert_True (Initialized_Pixbuf.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Initialized_Resource.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);

      --  The ordinary setters switch source types and accept an empty value to
      --  clear file or resource content. The pixbuf setter remains callable
      --  for compatibility with the GTK API, although it is obsolete.
      Changed.Set_File (File);
      Assert_True (Changed.Get_File /= Glib.GFile.Null_Gfile);
      Changed.Set_Filename (Filename);
      Assert_True (Changed.Get_File /= Glib.GFile.Null_Gfile);
      Changed.Set_Paintable (+Texture);
      Assert_True (Changed.Get_Paintable = +Texture);
      Changed.Set_Pixbuf (Pixbuf);
      Assert_True (Changed.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Changed.Set_Resource (Resource_Path);
      Assert_True (Changed.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);
      Changed.Set_Filename ("");
      Assert_True (Changed.Get_File = Glib.GFile.Null_Gfile);
      Changed.Set_Resource ("");
      Assert_True (Changed.Get_Paintable = Gdk.Paintable.Null_Gdk_Paintable);
      Changed.Set_Paintable (Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Changed.Get_Paintable = Gdk.Paintable.Null_Gdk_Paintable);

      --  Initializing an already-created picture must leave that object alone.
      Gtk.Picture.Initialize (Empty_Procedure);
      Assert_True (Empty_Procedure.Get_Paintable = Gdk.Paintable.Null_Gdk_Paintable);

      Resource.Unregister;
      Unref (Resource);
      Unref (Initialized_Resource);
      Unref (Initialized_Pixbuf);
      Unref (Initialized_Paintable);
      Unref (Initialized_Filename);
      Unref (Initialized_File);
      Unref (Changed);
      Unref (Resource_Function);
      Unref (Resource_Procedure);
      Unref (Pixbuf_Function);
      Unref (Pixbuf_Procedure);
      Unref (Paintable_Function);
      Unref (Paintable_Procedure);
      Unref (Filename_Function);
      Unref (Filename_Procedure);
      Unref (File_Function);
      Unref (File_Procedure);
      Unref (Empty_Function);
      Unref (Empty_Procedure);
      Unref (Texture);
      Unref (Pixbuf);
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      Ada.Directories.Delete_File (Filename);
   end Test_Constructors_And_Sources;

   procedure Test_Properties is
      Picture : constant Gtk_Picture := Gtk_Picture_New;
   begin
      Ref_Sink (Picture);
      --  Check both string conversion paths: non-empty text is retained, and
      --  the empty string clears the optional alternative description.
      Assert_Cmpstr_Eq (Picture.Get_Alternative_Text, "");
      Picture.Set_Alternative_Text ("A blue square");
      Assert_Cmpstr_Eq (Picture.Get_Alternative_Text, "A blue square");
      Picture.Set_Alternative_Text ("");
      Assert_Cmpstr_Eq (Picture.Get_Alternative_Text, "");

      --  Verify each sizing and rendering setter by reading the resulting
      --  property back from Gtk.Picture.
      Picture.Set_Can_Shrink (False);
      Assert_False (Picture.Get_Can_Shrink);
      Picture.Set_Can_Shrink (True);
      Assert_True (Picture.Get_Can_Shrink);
      Picture.Set_Content_Fit (Content_Fit_Contain);
      Assert_True (Picture.Get_Content_Fit = Content_Fit_Contain);
      Picture.Set_Content_Fit (Content_Fit_Cover);
      Assert_True (Picture.Get_Content_Fit = Content_Fit_Cover);
      Picture.Set_Isolate_Contents (True);
      Assert_True (Picture.Get_Isolate_Contents);
      Picture.Set_Isolate_Contents (False);
      Assert_False (Picture.Get_Isolate_Contents);
      Picture.Set_Keep_Aspect_Ratio (False);
      Assert_False (Picture.Get_Keep_Aspect_Ratio);
      Picture.Set_Keep_Aspect_Ratio (True);
      Assert_True (Picture.Get_Keep_Aspect_Ratio);
      Unref (Picture);
   end Test_Properties;

   procedure Test_Accessibility is
      Picture : constant Gtk_Picture := Gtk_Picture_New;
      Parent : Gtk_Accessible;
      Sibling : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Picture);
      --  A detached picture has the image role, no accessible parent or
      --  siblings, and no AT context or screen bounds.
      Assert_True (Picture.Get_Accessible_Role = Accessible_Role_Img);
      Assert_Cmpstr_Eq (Picture.Get_Accessible_Id, "");
      Context := Picture.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Picture.Get_Bounds (X, Y, Width, Height));
      Parent := Picture.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Sibling := Picture.Get_Next_Accessible_Sibling;
      Assert_True (Sibling = Null_Gtk_Accessible);
      Assert_True (Picture.Get_First_Accessible_Child = Null_Gtk_Accessible);
      Assert_False
        (Picture.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These operations are safe while detached and exercise the accessible
      --  mutation wrappers without requiring a screen reader or live window.
      Picture.Announce
        ("picture test", Accessible_Announcement_Priority_Low);
      Picture.Reset_Property (Accessible_Property_Label);
      Picture.Reset_Relation (Accessible_Relation_Labelled_By);
      Picture.Reset_State (Accessible_State_Busy);
      Picture.Set_Accessible_Parent
        (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Picture.Get_Accessible_Parent = Null_Gtk_Accessible);
      Picture.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Picture.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Picture.Update_Platform_State (Accessible_Platform_State_Focused);
      Unref (Picture);
   end Test_Accessibility;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func
     ("/gtk-picture/constructors-sources",
      Test_Constructors_And_Sources'Unrestricted_Access);
   Add_Func ("/gtk-picture/properties", Test_Properties'Unrestricted_Access);
   Add_Func
     ("/gtk-picture/accessibility",
      Test_Accessibility'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
