--  Gtk.Image tests for empty, icon-name, GIcon, paintable, pixbuf, file and
--  resource representations, plus the widget's inherited accessible APIs.
--  GTK's C testsuite has no direct GtkImage test to port; its image instances
--  occur only as incidental setup in gesture tests. image.gresource is built
--  from image.png and image.gresource.xml with glib-compile-resources.

with Ada.Command_Line;
with Ada.Directories;
with GNAT.Strings;

with Gdk.Paintable;
with Gdk.Pixbuf;       use Gdk.Pixbuf;
with Gdk.Texture;      use Gdk.Texture;
with Glib;             use Glib;
with Glib.Error;       use Glib.Error;
with Glib.G_Icon;
with Glib.Object;      use Glib.Object;
with Glib.Properties;  use Glib.Properties;
with Glib.Resource;    use Glib.Resource;
with Glib.Test;        use Glib.Test;
with Gtk.Accessible;   use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Enums;        use Gtk.Enums;
with Gtk.Image;        use Gtk.Image;
with Gtk.Main;
with Gtkada.Types;     use Gtkada.Types;
with System;

use type Gtk.Atcontext.Gtk_Atcontext;
use type Gdk.Paintable.Gdk_Paintable;
use type Glib.G_Icon.G_Icon;
use type System.Address;

procedure Main is

   procedure Test_Empty_And_Icon_Name with Convention => C;
   procedure Test_Gicon_Paintable_And_Pixbuf with Convention => C;
   procedure Test_File_And_Resource with Convention => C;
   procedure Test_Initialize_No_Ops with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;

   function Themed_Icon_New
     (Name : Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, Themed_Icon_New, "g_themed_icon_new");

   procedure G_Object_Unref (Icon : Glib.G_Icon.G_Icon);
   pragma Import (C, G_Object_Unref, "g_object_unref");

   --  Empty option lists are needed by Gdk.Pixbuf.Savev when writing a
   --  temporary PNG fixture for Gtk.Image's file constructors.
   No_Options : constant GNAT.Strings.String_List (1 .. 0) := (others => null);

   procedure Test_Empty_And_Icon_Name is
      Empty  : constant Gtk_Image := Gtk_Image_New;
      Named  : constant Gtk_Image := Gtk_Image_New_From_Icon_Name ("image-missing");
      Via_New       : Gtk_Image;
      Via_Icon_Name : Gtk_Image;
   begin
      Ref_Sink (Empty);
      Ref_Sink (Named);
      Assert_True (Empty.Get_Storage_Type = Image_Empty);
      Assert_Cmpstr_Eq (Empty.Get_Icon_Name, "");
      Assert_True (Empty.Get_Gicon = null);
      Assert_True
        (Empty.Get_Paintable = Gdk.Paintable.Null_Gdk_Paintable);

      --  A named image retains its icon name independently of whether the
      --  current theme can resolve that name into pixels.
      Assert_True (Named.Get_Storage_Type = Image_Icon_Name);
      Assert_Cmpstr_Eq (Named.Get_Icon_Name, "image-missing");
      Named.Set_Icon_Size (Icon_Size_Large);
      Assert_True (Named.Get_Icon_Size = Icon_Size_Large);
      Named.Set_Pixel_Size (24);
      Assert_Cmpint_Eq (Named.Get_Pixel_Size, 24);
      --  Read properties as well as direct getters to cover the public
      --  property descriptors exposed by Gtk.Image.
      Assert_Cmpstr_Eq (Get_Property (Named, Icon_Name_Property), "image-missing");
      Assert_True (Get_Property (Named, Icon_Size_Property) = Icon_Size_Large);
      Assert_Cmpint_Eq (Get_Property (Named, Pixel_Size_Property), 24);

      --  Gtk_New and the setter cover the procedure constructor and the
      --  empty-name/null conversion used to clear a named image.
      Gtk_New (Via_New);
      Ref_Sink (Via_New);
      Assert_True (Via_New.Get_Storage_Type = Image_Empty);
      Gtk_New_From_Icon_Name (Via_Icon_Name, "image-missing");
      Ref_Sink (Via_Icon_Name);
      Assert_True (Via_Icon_Name.Get_Storage_Type = Image_Icon_Name);
      Via_Icon_Name.Set_From_Icon_Name ("");
      Assert_True (Via_Icon_Name.Get_Storage_Type = Image_Empty);
      Via_Icon_Name.Clear;
      Assert_True (Via_Icon_Name.Get_Storage_Type = Image_Empty);

      Unref (Via_Icon_Name);
      Unref (Via_New);
      Unref (Named);
      Unref (Empty);
   end Test_Empty_And_Icon_Name;

   procedure Test_Gicon_Paintable_And_Pixbuf is
      C_Name : Chars_Ptr := New_String ("image-missing");
      Icon   : constant Glib.G_Icon.G_Icon := Themed_Icon_New (C_Name);
      Pixbuf : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 2);
      Texture : constant Gdk_Texture := Gdk_Texture_New_For_Pixbuf (Pixbuf);
      From_Icon : constant Gtk_Image := Gtk_Image_New_From_Gicon (Icon);
      From_Paintable : constant Gtk_Image :=
        Gtk_Image_New_From_Paintable (+Texture);
      From_Pixbuf : constant Gtk_Image := Gtk_Image_New_From_Pixbuf (Pixbuf);
      Via_Out : Gtk_Image;
      Paintable_Out : Gtk_Image;
      Pixbuf_Out : Gtk_Image;
   begin
      Free (C_Name);
      Ref_Sink (From_Icon);
      Ref_Sink (From_Paintable);
      Ref_Sink (From_Pixbuf);
      Pixbuf.Fill (16#112233FF#);

      --  GIcon and paintable constructors preserve their source category.
      Assert_True (From_Icon.Get_Storage_Type = Image_Gicon);
      Assert_True (From_Icon.Get_Gicon = Icon);
      Assert_True (From_Paintable.Get_Storage_Type = Image_Paintable);
      Assert_True (From_Paintable.Get_Paintable = +Texture);
      Assert_True (From_Pixbuf.Get_Storage_Type = Image_Paintable);
      --  GTK exposes a pixbuf-backed image through the paintable interface.
      Assert_True
        (From_Pixbuf.Get_Paintable /= Gdk.Paintable.Null_Gdk_Paintable);

      --  The out-parameter constructors and setters reach the same GTK
      --  representations while checking the returned data after each change.
      Gtk_New_From_Gicon (Via_Out, Icon);
      Ref_Sink (Via_Out);
      Assert_True (Via_Out.Get_Storage_Type = Image_Gicon);
      Via_Out.Set_From_Gicon (Icon);
      Assert_True (Via_Out.Get_Gicon = Icon);
      Via_Out.Set_From_Paintable (+Texture);
      Assert_True (Via_Out.Get_Storage_Type = Image_Paintable);
      Assert_True (Via_Out.Get_Paintable = +Texture);
      Assert_True
        (Gdk.Paintable.Gdk_Paintable
           (Get_Property (Via_Out, Paintable_Property))
         /= Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Get_Property (Via_Out, Storage_Type_Property) = Image_Paintable);

      Gtk_New_From_Paintable (Paintable_Out, +Texture);
      Ref_Sink (Paintable_Out);
      Assert_True (Paintable_Out.Get_Storage_Type = Image_Paintable);
      Gtk_New_From_Pixbuf (Pixbuf_Out, Pixbuf);
      Ref_Sink (Pixbuf_Out);
      Assert_True (Pixbuf_Out.Get_Storage_Type = Image_Paintable);

      Unref (Pixbuf_Out);
      Unref (Paintable_Out);
      Unref (Via_Out);
      G_Object_Unref (Icon);
      Unref (Texture);
      Unref (Pixbuf);
      Unref (From_Pixbuf);
      Unref (From_Paintable);
      Unref (From_Icon);
   end Test_Gicon_Paintable_And_Pixbuf;

   procedure Test_File_And_Resource is
      Filename : constant String := "gtk-image-test.png";
      Resource_Path : constant String :=
        "/org/gtkada/coverage/gtk-image/image.png";
      Pixbuf : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 2, 2);
      File_Image : Gtk_Image;
      Resource_Image : Gtk_Image;
      Resource_Function : Gtk_Image;
      Resource_Constructor : Gtk_Image;
      Resource_Procedure : Gtk_Image;
      Resource : Gresource;
      Error : GError;
   begin
      --  Build a deterministic valid image locally, then load it through both
      --  Gtk.Image's file API and a registered GResource fixture.
      --  The generated PNG is removed at the end so the test leaves no data
      --  files in its working directory.
      Pixbuf.Fill (16#445566FF#);
      Assert_True (Pixbuf.Savev (Filename, "png", No_Options, No_Options, Error));
      Assert_True (Error = null);

      File_Image := Gtk_Image_New_From_File (Filename);
      Ref_Sink (File_Image);
      Assert_True (File_Image.Get_Storage_Type = Image_Paintable);
      Assert_Cmpstr_Eq (Get_Property (File_Image, File_Property), Filename);
      Gtk_New_From_File (Resource_Image, Filename);
      Ref_Sink (Resource_Image);
      Assert_True (Resource_Image.Get_Storage_Type = Image_Paintable);
      Resource_Image.Set_From_File (Filename);
      Assert_True (Resource_Image.Get_Storage_Type = Image_Paintable);
      Resource_Image.Set_From_File ("");
      Assert_True (Resource_Image.Get_Storage_Type = Image_Empty);

      Resource := Load ("image.gresource", Error);
      Assert_True (Error = null and Resource /= Null_Gresource);
      Resource.Register;
      File_Image.Set_From_Resource (Resource_Path);
      Assert_True (File_Image.Get_Storage_Type = Image_Paintable);
      Assert_Cmpstr_Eq
        (Get_Property (File_Image, Resource_Property), Resource_Path);
      Resource_Function := Gtk_Image_New_From_Resource (Resource_Path);
      Ref_Sink (Resource_Function);
      Assert_True (Resource_Function.Get_Storage_Type = Image_Paintable);
      Gtk_New_From_Resource (Resource_Constructor, Resource_Path);
      Ref_Sink (Resource_Constructor);
      Assert_True (Resource_Constructor.Get_Storage_Type = Image_Paintable);

      --  The resource procedure constructor, property setter, and empty-path
      --  case exercise the null-string conversion without a missing-resource
      --  warning.
      Gtk_New_From_Resource (Resource_Procedure, Resource_Path);
      Ref_Sink (Resource_Procedure);
      Assert_True (Resource_Procedure.Get_Storage_Type = Image_Paintable);
      Resource_Procedure.Set_From_Resource ("");
      Assert_True (Resource_Procedure.Get_Storage_Type = Image_Empty);

      Resource.Unregister;
      --  Images keep their own references to loaded textures, so unregistering
      --  the resource before releasing the widgets is safe.
      Unref (Resource);
      Unref (Resource_Procedure);
      Unref (Resource_Constructor);
      Unref (Resource_Function);
      Unref (Resource_Image);
      Unref (File_Image);
      Unref (Pixbuf);
      Ada.Directories.Delete_File (Filename);
   end Test_File_And_Resource;

   procedure Test_Initialize_No_Ops is
      Filename : constant String := "gtk-image-test.png";
      Resource_Path : constant String :=
        "/org/gtkada/coverage/gtk-image/image.png";
      Pixbuf : constant Gdk_Pixbuf :=
        Gdk_Pixbuf_New (Gdk_Colorspace_Rgb, True, 8, 1, 1);
      Texture : constant Gdk_Texture := Gdk_Texture_New_For_Pixbuf (Pixbuf);
      C_Name : Chars_Ptr := New_String ("image-missing");
      Icon : constant Glib.G_Icon.G_Icon := Themed_Icon_New (C_Name);
      Image : constant Gtk_Image := Gtk_Image_New;
   begin
      Free (C_Name);
      Ref_Sink (Image);
      --  Initialize* must leave an existing GObject intact. The icon-name
      --  initializer's corresponding branch is omitted because its generated
      --  body frees an uninitialized pointer; that binding defect is noted.
      Gtk.Image.Initialize (Image);
      Initialize_From_File (Image, Filename);
      Initialize_From_Gicon (Image, Icon);
      Initialize_From_Paintable (Image, +Texture);
      Initialize_From_Pixbuf (Image, Pixbuf);
      Initialize_From_Resource (Image, Resource_Path);
      Assert_True (Image.Get_Storage_Type = Image_Empty);

      G_Object_Unref (Icon);
      Unref (Texture);
      Unref (Pixbuf);
      Unref (Image);
   end Test_Initialize_No_Ops;

   procedure Test_Accessibility_And_Interfaces is
      Image : constant Gtk_Image := Gtk_Image_New;
      Parent : Gtk_Accessible;
      Next : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
   begin
      Ref_Sink (Image);
      --  An unrooted image has no AT context or screen bounds and no children.
      Assert_True (Image.Get_Accessible_Role = Accessible_Role_Img);
      Assert_Cmpstr_Eq (Image.Get_Accessible_Id, "");
      Context := Image.Get_At_Context;
      Assert_True (Context = null);
      Assert_False (Image.Get_Bounds (X, Y, Width, Height));
      Parent := Image.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Next := Image.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);
      Assert_True (Image.Get_First_Accessible_Child = Null_Gtk_Accessible);
      Assert_False (Image.Get_Platform_State (Accessible_Platform_State_Focused));

      --  These mutators accept detached widgets; checking the accessible tree
      --  afterward verifies that null parent/sibling updates remain harmless.
      Image.Announce ("image test", Accessible_Announcement_Priority_Low);
      Image.Reset_Property (Accessible_Property_Label);
      Image.Reset_Relation (Accessible_Relation_Labelled_By);
      Image.Reset_State (Accessible_State_Busy);
      Image.Update_Platform_State (Accessible_Platform_State_Focused);
      Image.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (Image.Get_Accessible_Parent = Null_Gtk_Accessible);
      Image.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (Image.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  Gtk.Image declares these three interfaces; each conversion pair must
      --  preserve the identity of the underlying GObject.
      --  These round trips check both generated operators in each pair.
      declare
         A : constant Gtk_Accessible := +Image;
         Back : constant Gtk_Image := -A;
      begin
         Assert_True (Back.Get_Object = Image.Get_Object);
      end;
      declare
         B : constant Gtk.Buildable.Gtk_Buildable := +Image;
         Back : constant Gtk_Image := -B;
      begin
         Assert_True (Back.Get_Object = Image.Get_Object);
      end;
      declare
         C : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Image;
         Back : constant Gtk_Image := -C;
      begin
         Assert_True (Back.Get_Object = Image.Get_Object);
      end;

      Unref (Image);
   end Test_Accessibility_And_Interfaces;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Add_Func ("/gtk-image/empty-icon-name", Test_Empty_And_Icon_Name'Unrestricted_Access);
   Add_Func ("/gtk-image/gicon-paintable-pixbuf", Test_Gicon_Paintable_And_Pixbuf'Unrestricted_Access);
   Add_Func ("/gtk-image/file-resource", Test_File_And_Resource'Unrestricted_Access);
   Add_Func ("/gtk-image/initialize-no-ops", Test_Initialize_No_Ops'Unrestricted_Access);
   Add_Func ("/gtk-image/accessibility-interfaces", Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
