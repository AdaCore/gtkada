--  Gtk.Icon_Theme tests, including a port of the list and lookup checks from
--  testsuite/c_tests/icontheme.c (/icontheme/list and /icontheme/basics).
--  The local icons/ and icons2/ fixtures are copies of that C test's themes.

with Ada.Command_Line;
with Ada.Directories;
with GNAT.Strings;
with Glib;
with Glib.G_Icon;
with Glib.GFile;
with Glib.Object;
with Glib.Properties;
with Glib.Test;
with Glib.Types;
with Gdk;
with Gdk.Display;
with Gdk.Paintable;
with Gdk.RGBA;
with Gsk.Render_Node;
with Gtk.Enums;
with Gtk.Icon_Paintable;
with Gtk.Icon_Theme;
with Gtk.Main;
with Gtk.Snapshot;
with Gtk.Symbolic_Paintable;
with Gtkada.Types;
with System;

procedure Icon_Theme is
   use type GNAT.Strings.String_Access;
   use type Glib.GType;
   use type Glib.Gdouble;
   use type Glib.Gint;
   use type Gdk.Gdk_Display;
   use type Glib.G_Icon.G_Icon;
   use type Glib.Object.GObject;
   use type Gdk.Paintable.Gdk_Paintable;
   use type Gdk.Paintable.Gdk_Paintable_Flags;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type Gtk.Icon_Theme.Gtk_Icon_Theme;
   use type Gtk.Icon_Paintable.Gtk_Icon_Paintable;
   use type Gtk.Symbolic_Paintable.Gtk_Symbolic_Paintable;
   use type System.Address;

   Empty_List : constant GNAT.Strings.String_List (1 .. 0) := (others => null);

   Expected_Theme : Gtk.Icon_Theme.Gtk_Icon_Theme := null;
   Expected_Slot  : Gtk.Icon_Theme.Gtk_Icon_Theme := null;
   Typed_Calls    : Natural := 0;
   Slot_Calls     : Natural := 0;

   --  GLib's themed icon constructor gives the theme methods a real GIcon.
   function New_Themed_Icon
     (Name : Gtkada.Types.Chars_Ptr) return Glib.G_Icon.G_Icon;
   pragma Import (C, New_Themed_Icon, "g_themed_icon_new");

   procedure Unref_Icon (Icon : Glib.G_Icon.G_Icon);
   pragma Import (C, Unref_Icon, "g_object_unref");

   --  The changed signal has no arguments, so C_Variadic_2 supplies only its
   --  instance and signal name, as in GtkAda's other signal tests.
   procedure Emit_Changed
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Free_List (Items : in out GNAT.Strings.String_List);
   function Contains
     (Items : GNAT.Strings.String_List; Value : String) return Boolean;
   procedure On_Theme_Changed
     (Self : access Gtk.Icon_Theme.Gtk_Icon_Theme_Record'Class);
   procedure On_Slot_Changed
     (Self : access Glib.Object.GObject_Record'Class);

   --  GNAT.Strings lists own each allocated string, so release every element
   --  after checking values returned by the binding.
   procedure Free_List (Items : in out GNAT.Strings.String_List) is
   begin
      for Index in Items'Range loop
         GNAT.Strings.Free (Items (Index));
      end loop;
   end Free_List;

   --  Keep list assertions independent of GTK's chosen lower array bound.
   function Contains
     (Items : GNAT.Strings.String_List; Value : String) return Boolean
   is
   begin
      for Item of Items loop
         if Item /= null and then Item.all = Value then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --  Typed callbacks receive the icon theme that emitted the signal.
   procedure On_Theme_Changed
     (Self : access Gtk.Icon_Theme.Gtk_Icon_Theme_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.GObject (Gtk.Icon_Theme.Gtk_Icon_Theme (Self)) =
           Glib.Object.GObject (Expected_Theme));
      Typed_Calls := Typed_Calls + 1;
   end On_Theme_Changed;

   --  Slot callbacks receive the object supplied as the closure's user data.
   procedure On_Slot_Changed
     (Self : access Glib.Object.GObject_Record'Class)
   is
   begin
      Glib.Test.Assert_True
        (Glib.Object.GObject (Self) = Glib.Object.GObject (Expected_Slot));
      Slot_Calls := Slot_Calls + 1;
   end On_Slot_Changed;

   --  Exercise constructors, getters, setters, both lookup entry points,
   --  list and size conversions, and the display-associated accessor.
   procedure Test_Methods with Convention => C;
   procedure Test_Icon_Paintable with Convention => C;
   procedure Test_Symbolic_Paintable with Convention => C;
   procedure Test_Methods is
      Directory : constant String := Ada.Directories.Current_Directory;
      Added_Path : constant String :=
        Ada.Directories.Compose (Directory, "unused-icon-search-path");
      Theme : constant Gtk.Icon_Theme.Gtk_Icon_Theme :=
        Gtk.Icon_Theme.Gtk_Icon_Theme_New;
      Output_Theme : Gtk.Icon_Theme.Gtk_Icon_Theme;
      Search_Paths : GNAT.Strings.String_List (1 .. 1) :=
        (1 => new String'(Directory));
      Resource_Paths : GNAT.Strings.String_List (1 .. 2) :=
        (1 => new String'("resource-one"),
         2 => new String'("resource-two"));
      Has_Size : Boolean := False;
      Display : Gdk.Gdk_Display;
      Shared_Theme : Gtk.Icon_Theme.Gtk_Icon_Theme;
      Shared_Theme_Again : Gtk.Icon_Theme.Gtk_Icon_Theme;
      Icon_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("simple");
      Icon : Glib.G_Icon.G_Icon;
      Missing_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("missing-icon-theme-test");
      Paintable : Gtk.Icon_Paintable.Gtk_Icon_Paintable;
      Fallbacks : GNAT.Strings.String_List (1 .. 1) :=
        (1 => new String'("simple"));
   begin
      --  Both public constructors use the same GObject type and initialization.
      Glib.Test.Assert_True (Gtk.Icon_Theme.Get_Type /= 0);
      Gtk.Icon_Theme.Gtk_New (Output_Theme);
      Glib.Test.Assert_True (Output_Theme /= null);

      --  Reinitializing an existing object is documented as a no-op.
      Gtk.Icon_Theme.Initialize (Output_Theme);
      Glib.Test.Assert_Cmpstr_Eq
        (Output_Theme.Get_Theme_Name,
         Glib.Properties.Get_Property
           (Output_Theme, Gtk.Icon_Theme.Theme_Name_Property));

      --  Point the scratch theme at the checked-in C test theme and verify the
      --  string-array setter/getter round trip, including the append method.
      Theme.Set_Search_Path (Search_Paths);
      Theme.Add_Search_Path (Added_Path);
      declare
         Got_Search_Paths : GNAT.Strings.String_List := Theme.Get_Search_Path;
      begin
         Glib.Test.Assert_True (Contains (Got_Search_Paths, Directory));
         Glib.Test.Assert_True (Contains (Got_Search_Paths, Added_Path));
         Free_List (Got_Search_Paths);
      end;

      Theme.Set_Theme_Name ("icons");
      Glib.Test.Assert_Cmpstr_Eq (Theme.Get_Theme_Name, "icons");
      Glib.Properties.Set_Property
        (Theme, Gtk.Icon_Theme.Theme_Name_Property, "icons");
      Glib.Test.Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property
           (Theme, Gtk.Icon_Theme.Theme_Name_Property),
         "icons");

      --  Resource paths are independent from filesystem theme search paths.
      Theme.Set_Resource_Path (Resource_Paths);
      Theme.Add_Resource_Path ("resource-three");
      declare
         Got_Resource_Paths : GNAT.Strings.String_List :=
           Theme.Get_Resource_Path;
      begin
         Glib.Test.Assert_True (Contains (Got_Resource_Paths, "resource-one"));
         Glib.Test.Assert_True (Contains (Got_Resource_Paths, "resource-two"));
         Glib.Test.Assert_True (Contains (Got_Resource_Paths, "resource-three"));
         Free_List (Got_Resource_Paths);
      end;

      --  Port the C list test: the known fixture names appear and an unrelated
      --  name does not. Icon sizes include the fixed 15-pixel fixture size.
      declare
         Names : GNAT.Strings.String_List := Theme.Get_Icon_Names;
      begin
         Glib.Test.Assert_True (Contains (Names, "simple"));
         Glib.Test.Assert_True (Contains (Names, "size-test"));
         Glib.Test.Assert_False
           (Contains (Names, "missing-icon-theme-test"));
         Free_List (Names);
      end;
      declare
         Sizes : constant Glib.Gint_Array := Theme.Get_Icon_Sizes ("size-test");
      begin
         for Size of Sizes loop
            if Size = 15 then
               Has_Size := True;
            end if;
         end loop;
      end;
      Glib.Test.Assert_True (Has_Size);

      --  Name and GIcon queries check both the positive and negative answers.
      Glib.Test.Assert_True (Theme.Has_Icon ("simple"));
      Glib.Test.Assert_False (Theme.Has_Icon ("missing-icon-theme-test"));
      Icon := New_Themed_Icon (Icon_Name);
      Gtkada.Types.Free (Icon_Name);
      Glib.Test.Assert_True (Icon /= null);
      Glib.Test.Assert_True (Theme.Has_Gicon (Icon));
      declare
         Missing_Icon : constant Glib.G_Icon.G_Icon :=
           New_Themed_Icon (Missing_Name);
      begin
         Gtkada.Types.Free (Missing_Name);
         Glib.Test.Assert_False (Theme.Has_Gicon (Missing_Icon));
         Unref_Icon (Missing_Icon);
      end;

      --  Port the C basic lookup and exercise empty and nonempty fallback arrays.
      Paintable := Theme.Lookup_Icon
        ("simple", Empty_List, 16, 1, Gtk.Enums.Text_Dir_None,
         Gtk.Icon_Theme.Icon_Lookup_None);
      Glib.Test.Assert_True (Paintable /= null);
      Glib.Object.Unref (Glib.Object.GObject (Paintable));
      Paintable := Theme.Lookup_Icon
        ("missing-icon-theme-test", Fallbacks, 16, 1,
         Gtk.Enums.Text_Dir_None, Gtk.Icon_Theme.Icon_Lookup_Force_Regular);
      Glib.Test.Assert_True (Paintable /= null);
      Glib.Object.Unref (Glib.Object.GObject (Paintable));
      Paintable := Theme.Lookup_By_Gicon
        (Icon, 16, 1, Gtk.Enums.Text_Dir_None,
         Gtk.Icon_Theme.Icon_Lookup_Force_Symbolic);
      Glib.Test.Assert_True (Paintable /= null);
      Glib.Object.Unref (Glib.Object.GObject (Paintable));

      --  The shared display theme is borrowed and stable for a display.
      Display := Gdk.Display.Get_Default;
      Glib.Test.Assert_True (Display /= null);
      Shared_Theme := Gtk.Icon_Theme.Get_For_Display (Display);
      Shared_Theme_Again := Gtk.Icon_Theme.Get_For_Display (Display);
      Glib.Test.Assert_True (Shared_Theme = Shared_Theme_Again);
      Glib.Test.Assert_True (Shared_Theme.Get_Display = Display);

      --  An empty name restores the configured theme name; compare both public
      --  access paths without depending on a desktop theme selected by Xvfb.
      Theme.Set_Theme_Name ("");
      Glib.Test.Assert_Cmpstr_Eq
        (Theme.Get_Theme_Name,
         Glib.Properties.Get_Property
           (Theme, Gtk.Icon_Theme.Theme_Name_Property));

      --  The outputs from GObject constructors and interface constructors are
      --  owned references, and the temporary themed icon also needs unref.
      Glib.Object.Unref (Glib.Object.GObject (Output_Theme));
      Glib.Object.Unref (Glib.Object.GObject (Theme));
      Unref_Icon (Icon);
      Free_List (Search_Paths);
      Free_List (Resource_Paths);
      Free_List (Fallbacks);
   end Test_Methods;

   --  Exercise GtkIconPaintable's file constructors and its GdkPaintable
   --  methods against the local 16-by-16 PNG used by the icon theme tests.
   procedure Test_Icon_Paintable is
      File : constant Glib.GFile.Gfile :=
        Glib.GFile.New_For_Path ("icons/16x16/simple.png");
      Other_File : constant Glib.GFile.Gfile :=
        Glib.GFile.New_For_Path ("icons/16x16/absent.png");
      Function_Form : constant Gtk.Icon_Paintable.Gtk_Icon_Paintable :=
        Gtk.Icon_Paintable.Gtk_Icon_Paintable_New_For_File (File, 16, 1);
      Procedure_Form : Gtk.Icon_Paintable.Gtk_Icon_Paintable;
      Initialized : constant Gtk.Icon_Paintable.Gtk_Icon_Paintable :=
        new Gtk.Icon_Paintable.Gtk_Icon_Paintable_Record;
      Loaded_File : Glib.GFile.Gfile;
      Current : Gdk.Paintable.Gdk_Paintable;
      Flags : Gdk.Paintable.Gdk_Paintable_Flags;
      Concrete_Width, Concrete_Height : Glib.Gdouble;
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  The function and procedure constructors both produce live objects.
      Glib.Test.Assert_True (Gtk.Icon_Paintable.Get_Type /= 0);
      Gtk.Icon_Paintable.Gtk_New_For_File (Procedure_Form, File, 16, 1);
      Glib.Test.Assert_True (Function_Form /= null);
      Glib.Test.Assert_True (Procedure_Form /= null);

      --  Get_File returns an owned interface reference, as in
      --  testsuite/c_tests/icontheme.c's icon lookup checks.
      Loaded_File := Function_Form.Get_File;
      Glib.Test.Assert_True (Glib.GFile.Equal (Loaded_File, File));
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Loaded_File)));

      --  Initializing a raw Ada record creates its GObject; later initialize
      --  calls are documented no-ops and must retain the first source and size.
      Gtk.Icon_Paintable.Initialize_For_File (Initialized, File, 16, 1);
      Gtk.Icon_Paintable.Initialize_For_File (Initialized, Other_File, 32, 2);
      Loaded_File := Initialized.Get_File;
      Glib.Test.Assert_True (Glib.GFile.Equal (Loaded_File, File));
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Loaded_File)));

      --  The decoded fixture is square and sized to the requested 16 pixels.
      Glib.Test.Assert_Cmpint_Eq (Function_Form.Get_Intrinsic_Width, 16);
      Glib.Test.Assert_Cmpint_Eq (Function_Form.Get_Intrinsic_Height, 16);
      Glib.Test.Assert_True
        (Function_Form.Get_Intrinsic_Aspect_Ratio = 1.0);
      Flags := Function_Form.Get_Flags;
      Glib.Test.Assert_True
        ((Flags and Gdk.Paintable.Gdk_Paintable_Static_Size) /= 0);

      --  Current_Image transfers an owned paintable interface to the caller.
      Current := Function_Form.Get_Current_Image;
      Glib.Test.Assert_True (Current /= Gdk.Paintable.Null_Gdk_Paintable);
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Current)));

      --  Supplying only a width preserves the file's square aspect ratio.
      Function_Form.Compute_Concrete_Size
        (8.0, 0.0, 16.0, 16.0, Concrete_Width, Concrete_Height);
      Glib.Test.Assert_True
        (Concrete_Width = 8.0 and Concrete_Height = 8.0);

      --  Snapshotting the image creates a render node without a visible window.
      Function_Form.Snapshot (Snapshot, 16.0, 16.0);
      Node := Gtk.Snapshot.To_Node (Snapshot);
      Glib.Test.Assert_True (Node /= null);

      --  GDK forbids invalidating paintables that advertise the corresponding
      --  static flag; invoke each operation only when its contract allows it.
      if (Flags and Gdk.Paintable.Gdk_Paintable_Static_Contents) = 0 then
         Function_Form.Invalidate_Contents;
         Glib.Test.Assert_True (Function_Form.Get_Flags = Flags);
      else
         Glib.Test.Assert_True
           ((Flags and Gdk.Paintable.Gdk_Paintable_Static_Contents) /= 0);
      end if;
      if (Flags and Gdk.Paintable.Gdk_Paintable_Static_Size) = 0 then
         Function_Form.Invalidate_Size;
         Glib.Test.Assert_True (Function_Form.Get_Flags = Flags);
      else
         Glib.Test.Assert_True
           ((Flags and Gdk.Paintable.Gdk_Paintable_Static_Size) /= 0);
      end if;

      --  Release each full reference; the local node is finalized by GtkAda.
      Glib.Object.Unref (Glib.Object.GObject (Function_Form));
      Glib.Object.Unref (Glib.Object.GObject (Procedure_Form));
      Glib.Object.Unref (Glib.Object.GObject (Initialized));
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (Other_File)));
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Test_Icon_Paintable;

   --  Port the symbolic snapshot call from testsuite/c_tests/symbolic.c's
   --  svg_to_texture helper. The local symbolic SVG makes this independent of
   --  the desktop icon theme and needs no visible window or renderer.
   procedure Test_Symbolic_Paintable is
      File : constant Glib.GFile.Gfile :=
        Glib.GFile.New_For_Path
          ("icons/32x32/only32-symbolic.svg");
      Icon : constant Gtk.Icon_Paintable.Gtk_Icon_Paintable :=
        Gtk.Icon_Paintable.Gtk_Icon_Paintable_New_For_File (File, 32, 1);
      Symbolic : constant Gtk.Symbolic_Paintable.Gtk_Symbolic_Paintable :=
        Gtk.Icon_Paintable."+" (Icon);
      Same_Symbolic : constant Gtk.Symbolic_Paintable.Gtk_Symbolic_Paintable :=
        Gtk.Symbolic_Paintable."+" (Symbolic);
      Colors : constant Gdk.RGBA.Gdk_RGBA_Array (1 .. 5) :=
        ((Red => 0.0, Green => 0.0, Blue => 0.0, Alpha => 1.0),
         (Red => 1.0, Green => 0.0, Blue => 0.0, Alpha => 1.0),
         (Red => 1.0, Green => 1.0, Blue => 0.0, Alpha => 1.0),
         (Red => 0.0, Green => 1.0, Blue => 0.0, Alpha => 1.0),
         (Red => 0.0, Green => 0.0, Blue => 1.0, Alpha => 1.0));
      Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Weighted_Snapshot : constant Gtk.Snapshot.Gtk_Snapshot :=
        Gtk.Snapshot.Gtk_Snapshot_New;
      Node : Gsk.Render_Node.Gsk_Render_Node;
      Weighted_Node : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  The interface conversion and its identity operator preserve the
      --  underlying icon paintable.
      Glib.Test.Assert_True (Icon /= null);
      Glib.Test.Assert_True
        (Symbolic /= Gtk.Symbolic_Paintable.Null_Gtk_Symbolic_Paintable);
      Glib.Test.Assert_True (Same_Symbolic = Symbolic);

      --  Both wrappers should append a render node using the supplied colors;
      --  use separate snapshots because To_Node consumes each snapshot.
      Gtk.Symbolic_Paintable.Snapshot_Symbolic
        (Symbolic, Snapshot, 32.0, 32.0, Colors);
      Node := Gtk.Snapshot.To_Node (Snapshot);
      Glib.Test.Assert_True (Node /= null);

      Gtk.Symbolic_Paintable.Snapshot_With_Weight
        (Symbolic, Weighted_Snapshot, 32.0, 32.0, Colors, 400.0);
      Weighted_Node := Gtk.Snapshot.To_Node (Weighted_Snapshot);
      Glib.Test.Assert_True (Weighted_Node /= null);

      --  The icon and snapshots are full references returned by constructors.
      Glib.Object.Unref (Glib.Object.GObject (Icon));
      Glib.Object.Unref (Glib.Object.GObject (Snapshot));
      Glib.Object.Unref (Glib.Object.GObject (Weighted_Snapshot));
      Glib.Object.Unref
        (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Test_Symbolic_Paintable;

   --  Both generated signal overloads route through GtkAda marshallers.
   procedure Test_Changed_Signal with Convention => C;
   procedure Test_Changed_Signal is
      Theme : constant Gtk.Icon_Theme.Gtk_Icon_Theme :=
        Gtk.Icon_Theme.Gtk_Icon_Theme_New;
      Slot : constant Gtk.Icon_Theme.Gtk_Icon_Theme :=
        Gtk.Icon_Theme.Gtk_Icon_Theme_New;
      Signal_Name : Gtkada.Types.Chars_Ptr :=
        Gtkada.Types.New_String ("changed");
   begin
      Expected_Theme := Theme;
      Expected_Slot := Slot;
      Typed_Calls := 0;
      Slot_Calls := 0;
      Theme.On_Changed (On_Theme_Changed'Unrestricted_Access, After => True);
      Theme.On_Changed
        (On_Slot_Changed'Unrestricted_Access, Slot, After => False);

      --  Emitting directly makes signal delivery deterministic and avoids any
      --  dependence on filesystem monitoring or theme reload timing.
      Emit_Changed (Theme.Get_Object, Signal_Name);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Typed_Calls), 1);
      Glib.Test.Assert_Cmpint_Eq (Glib.Gint (Slot_Calls), 1);

      Gtkada.Types.Free (Signal_Name);
      Glib.Object.Unref (Glib.Object.GObject (Slot));
      Glib.Object.Unref (Glib.Object.GObject (Theme));
      Expected_Theme := null;
      Expected_Slot := null;
   end Test_Changed_Signal;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/icon-theme/methods", Test_Methods'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/icon-theme/icon-paintable", Test_Icon_Paintable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/icon-theme/symbolic-paintable",
      Test_Symbolic_Paintable'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/icon-theme/changed-signal", Test_Changed_Signal'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Icon_Theme;
