--  Gtk.About_Dialog tests. Test_Construction ports the headless
--  test_about_dialog_basic case from testsuite/c_tests/templates.c; the
--  interactive test_about_dialog_show case is not suitable for unattended
--  execution. The remaining cases exercise the Ada binding's accessors,
--  properties, inherited interfaces, and activate-link marshallers.

with Ada.Command_Line;
with GNAT.Strings;
with System;

with Gdk.Paintable;
with Gsk.Renderer;
with Glib;
with Glib.Properties;  use Glib.Properties;
with Glib.Object;
with Glib.Main;       use Glib.Main;
with Glib.Test;        use Glib.Test;
with Glib.Types;
with Gtk.About_Dialog; use Gtk.About_Dialog;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Main;
with Gtk.Native;
with Gtk.Root;
with Gtk.Shortcut_Manager;
with Gtk.Widget;

with Gtkada.Types;

procedure About_Dialog is

   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Surface;
   use type Gdk.Paintable.Gdk_Paintable;
   use type Glib.Gboolean;
   use type Glib.Gdouble;
   use type Glib.Guint;
   use type Glib.Signal_Name;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Widget.Gtk_Widget;
   use type Gsk.Renderer.Gsk_Renderer;
   use type Gtk_License;
   use type System.Address;

   Signal_Count : Natural := 0;
   Last_URI     : Glib.UTF8_String (1 .. 200) := (others => ASCII.NUL);
   Expected_Object : System.Address := System.Null_Address;
   --  The callback state lets the test check the URI and the slot object
   --  after GObject dispatches each activate-link callback.

   procedure Emit_Boolean_Signal
     (Object       : System.Address;
      Name         : Gtkada.Types.Chars_Ptr;
      URI          : Gtkada.Types.Chars_Ptr;
      Return_Value : access Glib.Gboolean);
   pragma Import (C, Emit_Boolean_Signal, "g_signal_emit_by_name");

   procedure Test_Construction with Convention => C;
   procedure Test_Content with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Activate_Link with Convention => C;

   function On_Activate_Link
     (Self : access Gtk_About_Dialog_Record'Class;
      URI  : Glib.UTF8_String) return Boolean;

   function On_Activate_Link_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      URI  : Glib.UTF8_String) return Boolean;

   procedure Check_List
     (Actual   : GNAT.Strings.String_List;
      Expected : GNAT.Strings.String_List);
   --  The getters return newly allocated strings; Check_List frees those
   --  copies after comparing them with the expected list.

   procedure Check_List
     (Actual   : GNAT.Strings.String_List;
      Expected : GNAT.Strings.String_List) is
   begin
      Assert_Cmpuint_Eq (Glib.Guint (Actual'Length), Glib.Guint (Expected'Length));
      for Index in Actual'Range loop
         declare
            Person : GNAT.Strings.String_Access := Actual (Index);
            Expected_Person : GNAT.Strings.String_Access :=
              Expected (Expected'First + Index - Actual'First);
         begin
            Assert_Cmpstr_Eq (Person.all, Expected_Person.all);
            GNAT.Strings.Free (Person);
            GNAT.Strings.Free (Expected_Person);
         end;
      end loop;
   end Check_List;

   -----------------------
   -- Test_Construction --
   -----------------------

   procedure Test_Construction is
      Dialog : Gtk_About_Dialog;
      By_Function : Gtk_About_Dialog;
   begin
      --  Preserve the C test's check that gtk_about_dialog_new returns an
      --  AboutDialog, and exercise both generated constructor forms.
      Gtk_New (Dialog);
      Assert_True (Dialog /= null);
      Assert_True (Dialog.Get_Object /= System.Null_Address);

      --  Initialize is idempotent once Gtk_New has created the underlying
      --  GObject; this also covers its already-created branch.
      Gtk.About_Dialog.Initialize (Dialog);
      Assert_True (Dialog.Get_Object /= System.Null_Address);
      Dialog.Destroy;

      By_Function := Gtk_About_Dialog_New;
      Assert_True (By_Function /= null);
      Assert_True (By_Function.Get_Object /= System.Null_Address);
      By_Function.Destroy;
   end Test_Construction;

   -----------------
   -- Test_Content --
   -----------------

   procedure Test_Content is
      Dialog : constant Gtk_About_Dialog := Gtk_About_Dialog_New;
      Artists : GNAT.Strings.String_List :=
        (new String'("Ada Artist"), new String'("Grace Painter"));
      Authors : GNAT.Strings.String_List :=
        (new String'("Ada Author"), new String'("Grace Author"));
      Documenters : GNAT.Strings.String_List :=
        (new String'("Ada Writer"), new String'("Grace Writer"));
      Empty : GNAT.Strings.String_List (1 .. 0);
      Credits : GNAT.Strings.String_List :=
        (new String'("Lin Contributor"), new String'("Margaret Contributor"));
      Listed_Authors : GNAT.Strings.String_List :=
        (1 => new String'("Listed author"));
   begin
      --  Exercise all three strv getters/setters with non-empty and empty
      --  inputs. GtkAboutDialog copies the arrays, so the caller frees inputs.
      Dialog.Set_Artists (Artists);
      Dialog.Set_Authors (Authors);
      Dialog.Set_Documenters (Documenters);
      for Person of Artists loop GNAT.Strings.Free (Person); end loop;
      for Person of Authors loop GNAT.Strings.Free (Person); end loop;
      for Person of Documenters loop GNAT.Strings.Free (Person); end loop;

      Check_List
        (Dialog.Get_Artists,
         (1 => new String'("Ada Artist"), 2 => new String'("Grace Painter")));
      Check_List
        (Dialog.Get_Authors,
         (1 => new String'("Ada Author"), 2 => new String'("Grace Author")));
      Check_List
        (Dialog.Get_Documenters,
         (1 => new String'("Ada Writer"), 2 => new String'("Grace Writer")));

      Dialog.Set_Artists (Empty);
      Dialog.Set_Authors (Empty);
      Dialog.Set_Documenters (Empty);
      Check_List (Dialog.Get_Artists, Empty);
      Check_List (Dialog.Get_Authors, Empty);
      Check_List (Dialog.Get_Documenters, Empty);

      --  Credit sections are displayed separately from the standard authors
      --  list. Check that adding one leaves that independent list intact.
      Dialog.Set_Authors (Listed_Authors);
      GNAT.Strings.Free (Listed_Authors (1));
      Dialog.Add_Credit_Section ("Contributors", Credits);
      for Person of Credits loop GNAT.Strings.Free (Person); end loop;
      Check_List
        (Dialog.Get_Authors, (1 => new String'("Listed author")));

      --  Each nullable UTF-8 setter is tested with text and with the empty
      --  string, which the binding translates to a C NULL pointer.
      Dialog.Set_Comments ("A small GTK application");
      Assert_Cmpstr_Eq (Dialog.Get_Comments, "A small GTK application");
      Dialog.Set_Comments;
      Assert_Cmpstr_Eq (Dialog.Get_Comments, "");

      Dialog.Set_Copyright ("Copyright 2026");
      Assert_Cmpstr_Eq (Dialog.Get_Copyright, "Copyright 2026");
      Dialog.Set_Copyright;
      Assert_Cmpstr_Eq (Dialog.Get_Copyright, "");

      Dialog.Set_License ("A custom license");
      Assert_Cmpstr_Eq (Dialog.Get_License, "A custom license");
      Assert_True (Dialog.Get_License_Type = License_Custom);
      Dialog.Set_License;
      Assert_Cmpstr_Eq (Dialog.Get_License, "");

      Dialog.Set_License_Type (License_Mit_X11);
      Assert_True (Dialog.Get_License_Type = License_Mit_X11);

      Dialog.Set_Logo (Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True (Dialog.Get_Logo = Gdk.Paintable.Null_Gdk_Paintable);

      Dialog.Set_Logo_Icon_Name ("applications-graphics");
      Assert_Cmpstr_Eq (Dialog.Get_Logo_Icon_Name, "applications-graphics");
      Dialog.Set_Logo_Icon_Name;
      Assert_Cmpstr_Eq (Dialog.Get_Logo_Icon_Name, "");

      Dialog.Set_Program_Name ("GtkAda Test");
      Assert_Cmpstr_Eq (Dialog.Get_Program_Name, "GtkAda Test");
      Dialog.Set_Program_Name;
      --  With no explicit name GTK falls back to the application name, which
      --  the test runner sets to its executable name.
      Assert_True (Dialog.Get_Program_Name'Length > 0);

      Dialog.Set_System_Information ("Test platform details");
      Assert_Cmpstr_Eq (Dialog.Get_System_Information, "Test platform details");
      Dialog.Set_System_Information;
      Assert_Cmpstr_Eq (Dialog.Get_System_Information, "");

      Dialog.Set_Translator_Credits ("Translated by Ada");
      Assert_Cmpstr_Eq (Dialog.Get_Translator_Credits, "Translated by Ada");
      Dialog.Set_Translator_Credits;
      Assert_Cmpstr_Eq (Dialog.Get_Translator_Credits, "");

      Dialog.Set_Version ("1.2.3");
      Assert_Cmpstr_Eq (Dialog.Get_Version, "1.2.3");
      Dialog.Set_Version;
      Assert_Cmpstr_Eq (Dialog.Get_Version, "");

      Dialog.Set_Website ("https://www.example.test/");
      Assert_Cmpstr_Eq (Dialog.Get_Website, "https://www.example.test/");
      Dialog.Set_Website;
      Assert_Cmpstr_Eq (Dialog.Get_Website, "");

      Dialog.Set_Website_Label ("GtkAda project page");
      Assert_Cmpstr_Eq (Dialog.Get_Website_Label, "GtkAda project page");

      --  This setter takes a Boolean directly rather than a nullable string.
      Dialog.Set_Wrap_License (True);
      Assert_True (Dialog.Get_Wrap_License);
      Dialog.Set_Wrap_License (False);
      Assert_False (Dialog.Get_Wrap_License);

      --  The C enum values are all valid inputs, including the terminal 0BSD
      --  value. Round-trip each value to catch enum conversion mistakes.
      for License in Gtk_License'Range loop
         Dialog.Set_License_Type (License);
         Assert_True (Dialog.Get_License_Type = License);
      end loop;

      Dialog.Destroy;
   end Test_Content;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Dialog : constant Gtk_About_Dialog := Gtk_About_Dialog_New;
   begin
      --  Exercise the typed GObject property descriptors as well as the
      --  method wrappers. The strv property descriptors for artists, authors
      --  and documenters are generated as Property_String and are therefore
      --  not used here; their correct array API is covered above.
      Set_Property (Dialog, Comments_Property, "Property comment");
      Assert_Cmpstr_Eq (Get_Property (Dialog, Comments_Property), "Property comment");
      Set_Property (Dialog, Copyright_Property, "Property copyright");
      Assert_Cmpstr_Eq (Get_Property (Dialog, Copyright_Property), "Property copyright");
      Set_Property (Dialog, License_Property, "Property license");
      Assert_Cmpstr_Eq (Get_Property (Dialog, License_Property), "Property license");
      Assert_True
        (Gtk_License_Properties.Get_Property
           (Dialog, Gtk_License_Properties.Property (License_Type_Property)) =
             License_Custom);
      Gtk_License_Properties.Set_Property
        (Dialog, Gtk_License_Properties.Property (License_Type_Property),
         License_Agpl_3_0);
      Assert_True
        (Gtk_License_Properties.Get_Property
           (Dialog, Gtk_License_Properties.Property (License_Type_Property)) =
             License_Agpl_3_0);

      Set_Property
        (Dialog, Logo_Property,
         Glib.Types.GType_Interface (Gdk.Paintable.Null_Gdk_Paintable));
      Assert_True
        (Gdk.Paintable.Gdk_Paintable (Get_Property (Dialog, Logo_Property)) =
           Gdk.Paintable.Null_Gdk_Paintable);

      Set_Property (Dialog, Logo_Icon_Name_Property, "applications-help");
      Assert_Cmpstr_Eq
        (Get_Property (Dialog, Logo_Icon_Name_Property), "applications-help");
      Set_Property (Dialog, Program_Name_Property, "Property program");
      Assert_Cmpstr_Eq (Get_Property (Dialog, Program_Name_Property), "Property program");
      Set_Property (Dialog, System_Information_Property, "Property system");
      Assert_Cmpstr_Eq
        (Get_Property (Dialog, System_Information_Property), "Property system");
      Set_Property (Dialog, Translator_Credits_Property, "Property translators");
      Assert_Cmpstr_Eq
        (Get_Property (Dialog, Translator_Credits_Property), "Property translators");
      Set_Property (Dialog, Version_Property, "9.8.7");
      Assert_Cmpstr_Eq (Get_Property (Dialog, Version_Property), "9.8.7");
      Set_Property (Dialog, Website_Property, "https://example.test/");
      Assert_Cmpstr_Eq (Get_Property (Dialog, Website_Property), "https://example.test/");
      Set_Property (Dialog, Website_Label_Property, "Property label");
      Assert_Cmpstr_Eq (Get_Property (Dialog, Website_Label_Property), "Property label");
      Set_Property (Dialog, Wrap_License_Property, True);
      Assert_True (Get_Property (Dialog, Wrap_License_Property));
      Set_Property (Dialog, Wrap_License_Property, False);
      Assert_False (Get_Property (Dialog, Wrap_License_Property));

      Dialog.Destroy;
   end Test_Properties;

   ---------------------------------------
   -- Test_Accessibility_And_Interfaces --
   ---------------------------------------

   procedure Test_Accessibility_And_Interfaces is
      Dialog : constant Gtk_About_Dialog := Gtk_About_Dialog_New;
      X, Y, Width, Height : Glib.Gint;
      Surface_X, Surface_Y : Glib.Gdouble;
      Has_Bounds : Boolean;
   begin
      --  The unrooted dialog has no accessible parent, sibling, AT context,
      --  focus, or bounds. GTK still supplies its stable window role.
      Assert_True (Dialog.Get_Accessible_Id'Length > 0);
      Assert_True
        (Dialog.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Window);
      Assert_True (Dialog.Get_At_Context = null);
      Has_Bounds := Dialog.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_True
        (Dialog.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Dialog.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Dialog.Get_Focus = null);
      Assert_True (Dialog.Get_Display /= null);
      Assert_False
        (Dialog.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      Dialog.Announce
        ("About dialog accessibility test",
         Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Dialog.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Dialog.Reset_Relation (Gtk.Accessible.Accessible_Relation_Labelled_By);
      Dialog.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Dialog.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Dialog.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Dialog.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Dialog.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Dialog.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focused);
      Dialog.Set_Focus (null);

      --  Mapping on the private Xvfb display creates the native surface and
      --  renderer. A few nonblocking iterations let GTK dispatch map events.
      Dialog.Present;
      for Iteration in 1 .. 5 loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => False);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      Assert_True (Dialog.Get_Realized);
      Assert_True
        (Dialog.Get_First_Accessible_Child /=
           Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Dialog.Get_Surface /= null);
      Assert_True (Dialog.Get_Renderer /= null);
      Dialog.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X >= 0.0 and Surface_Y >= 0.0);

      --  Round-trip every interface conversion pair declared by the spec.
      declare
         I : constant Gtk.Accessible.Gtk_Accessible := +Dialog;
         Back : constant Gtk_About_Dialog := -I;
      begin
         Assert_True (Back.Get_Object = Dialog.Get_Object);
      end;
      declare
         I : constant Gtk.Buildable.Gtk_Buildable := +Dialog;
         Back : constant Gtk_About_Dialog := -I;
      begin
         Assert_True (Back.Get_Object = Dialog.Get_Object);
      end;
      declare
         I : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Dialog;
         Back : constant Gtk_About_Dialog := -I;
      begin
         Assert_True (Back.Get_Object = Dialog.Get_Object);
      end;
      declare
         I : constant Gtk.Native.Gtk_Native := +Dialog;
         Back : constant Gtk_About_Dialog := -I;
      begin
         Assert_True (Back.Get_Object = Dialog.Get_Object);
      end;
      declare
         I : constant Gtk.Root.Gtk_Root := +Dialog;
         Back : constant Gtk_About_Dialog := -I;
      begin
         Assert_True (Back.Get_Object = Dialog.Get_Object);
      end;
      declare
         I : constant Gtk.Shortcut_Manager.Gtk_Shortcut_Manager := +Dialog;
         Back : constant Gtk_About_Dialog := -I;
      begin
         Assert_True (Back.Get_Object = Dialog.Get_Object);
      end;

      Dialog.Destroy;
   end Test_Accessibility_And_Interfaces;

   -------------------
   -- Signal handlers --
   -------------------

   function On_Activate_Link
     (Self : access Gtk_About_Dialog_Record'Class;
      URI  : Glib.UTF8_String) return Boolean is
      pragma Unreferenced (Self);
   begin
      Signal_Count := Signal_Count + 1;
      Last_URI := (others => ASCII.NUL);
      Last_URI (1 .. URI'Length) := URI;
      return True;
   end On_Activate_Link;

   function On_Activate_Link_Slot
     (Self : access Glib.Object.GObject_Record'Class;
      URI  : Glib.UTF8_String) return Boolean is
   begin
      Expected_Object := Self.Get_Object;
      Signal_Count := Signal_Count + 1;
      Last_URI := (others => ASCII.NUL);
      Last_URI (1 .. URI'Length) := URI;
      return True;
   end On_Activate_Link_Slot;

   ------------------------
   -- Test_Activate_Link --
   ------------------------

   procedure Test_Activate_Link is
      Dialog : constant Gtk_About_Dialog := Gtk_About_Dialog_New;
      Slot_Dialog : constant Gtk_About_Dialog := Gtk_About_Dialog_New;
      Signal_Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("activate-link");
      URI : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("https://example.test/help");
      Result : aliased Glib.Gboolean := 0;
   begin
      --  Emit the boolean-returning signal directly. Returning True prevents
      --  GtkAboutDialog from attempting to launch the URI on the test host.
      Assert_True (Gtk.About_Dialog.Signal_Activate_Link = "activate-link");
      Dialog.On_Activate_Link (On_Activate_Link'Unrestricted_Access);
      Emit_Boolean_Signal (Dialog.Get_Object, Signal_Name, URI, Result'Access);
      Assert_True (Result /= 0);
      Assert_Cmpuint_Eq (Glib.Guint (Signal_Count), 1);
      Assert_Cmpstr_Eq (Last_URI (1 .. 25), "https://example.test/help");

      --  Exercise the slot-based overload as well; its callback receives the
      --  explicitly supplied GObject slot instead of the AboutDialog.
      Result := 0;
      Expected_Object := System.Null_Address;
      Slot_Dialog.On_Activate_Link
        (On_Activate_Link_Slot'Unrestricted_Access, Dialog);
      Emit_Boolean_Signal
        (Slot_Dialog.Get_Object, Signal_Name, URI, Result'Access);
      Assert_True (Result /= 0);
      Assert_Cmpuint_Eq (Glib.Guint (Signal_Count), 2);
      Assert_True (Expected_Object = Dialog.Get_Object);
      Assert_Cmpstr_Eq (Last_URI (1 .. 25), "https://example.test/help");

      Gtkada.Types.Free (URI);
      Gtkada.Types.Free (Signal_Name);
      Dialog.Destroy;
      Slot_Dialog.Destroy;
   end Test_Activate_Link;

begin
   Glib.Test.Init;
   --  Gtk.Test_Init initializes GTK as well, so the Ada port must do this
   --  explicitly before it creates widgets.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/about-dialog/construction", Test_Construction'Unrestricted_Access);
   Glib.Test.Add_Func ("/about-dialog/content", Test_Content'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/about-dialog/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/about-dialog/accessibility-and-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/about-dialog/activate-link", Test_Activate_Link'Unrestricted_Access);

   --  Return with the test runner's exit status.
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end About_Dialog;
