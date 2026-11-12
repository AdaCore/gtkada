--  Exercises Gtk.Builder constructors, source-loading methods, object pool,
--  scope and translation-domain accessors, and value conversion. The error
--  cases are based on glib's testsuite/c_tests/builderparser.c and the value
--  conversion cases on testsuite/c_tests/builder.c (test_value_from_string).
--
--  The valid UI fixture contains only an empty <interface/>. Loading an
--  object from UI is left out because that path is known to crash in this
--  checkout; these tests use Expose_Object to test Builder's object pool
--  without entering that path.
--
--  No widget is loaded from UI. The template parse-error case constructs one
--  GtkLabel directly and initializes GTK on the test driver's display.

with Ada.Command_Line;

with Glib;         use Glib;
with Glib.Error;   use Glib.Error;
with Glib.Error_Enums;
with Glib.Object;  use Glib.Object;
with Glib.Properties.Creation;
with Glib.Resource; use Glib.Resource;
with Glib.Test;    use Glib.Test;
with Glib.Values;
with GNAT.Strings;
with Gtk.Builder;  use Gtk.Builder;
with Gtk.Builder_CScope;
with Gtk.Builder_Scope;
with Gtk.Label;
with Gtk.Main;
with Gtk.String_List;

procedure Main is

   Malformed_UI : constant UTF8_String := "this is not a ui definition";
   --  Not XML at all, and references no GTK class: fails at the pure
   --  markup-parse stage, before any object/type resolution is attempted
   --  (which would otherwise hit the missing-GtkBuilderScope crash noted
   --  above).
   Empty_UI : constant UTF8_String := "<interface/>";
   Ui_Resource_Path : constant UTF8_String :=
     "/org/gtkada/test/builder/empty.ui";

   use type Glib.Error.GError;
   use type Glib.GType;
   use type Glib.Guint;
   use type Gtk.Builder_Scope.Gtk_Builder_Scope;
   use type Glib.Object.GObject;

   procedure Test_Constructor_Families
   with Convention => C;

   procedure Test_Add_From_All_Sources
   with Convention => C;

   procedure Test_Object_And_Metadata_Accessors
   with Convention => C;

   procedure Test_Value_Conversions
   with Convention => C;

   procedure Test_Extend_With_Template_Parse_Error
   with Convention => C;

   procedure Test_Add_From_String_Malformed
   with Convention => C;

   procedure Test_Add_From_File_Missing
   with Convention => C;

   ------------------------------------
   -- Test_Add_From_String_Malformed --
   ------------------------------------

   procedure Test_Add_From_String_Malformed is
      Builder : Gtk_Builder;
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      Gtk_New (Builder);

      Success := Builder.Add_From_String (Malformed_UI, Error);

      Assert_False (Success);
      Assert_True (Error /= null);
      Assert_Error
        (Error,
         Glib.Error_Enums.GMarkup_Error_Domain,
         Glib.Gint
           (Glib.Error_Enums.GMarkup_Error'Pos
              (Glib.Error_Enums.G_Markup_Error_Parse)));

      Error_Free (Error);
      Unref (Builder);
   end Test_Add_From_String_Malformed;

   -------------------------------
   -- Test_Add_From_File_Missing --
   -------------------------------

   procedure Test_Add_From_File_Missing is
      Builder : Gtk_Builder;
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      Gtk_New (Builder);

      Success := Builder.Add_From_File
        ("/no/such/file-gtkada-builder-test.ui", Error);

      Assert_False (Success);
      Assert_True (Error /= null);

      Error_Free (Error);
      Unref (Builder);
   end Test_Add_From_File_Missing;

   ----------------------------------
   -- Test_Constructor_Families --
   ----------------------------------

   procedure Test_Constructor_Families is
      Error    : Glib.Error.GError;
      Resource : Glib.Resource.Gresource := Load ("builder.gresource", Error);
      Builder  : Gtk_Builder;
   begin
      --  Register the local bundle so both the resource constructor overloads
      --  can parse the same inert interface fixture as the string and file
      --  constructors.
      Assert_True (Error = null);
      Assert_True (Resource /= Null_Gresource);
      Register (Resource);

      Builder := Gtk_Builder_New;
      Assert_True (Builder /= null);
      --  Initialize is idempotent on an already-created Builder. Invalid
      --  inputs here are ignored, so this checks the no-op contract safely.
      Gtk.Builder.Initialize (Builder);
      Builder.Initialize_From_String (Malformed_UI, -1);
      Builder.Initialize_From_File ("missing-after-initialize.ui");
      Builder.Initialize_From_Resource ("/missing-after-initialize.ui");
      Unref (Builder);

      Builder := Gtk_Builder_New_From_String (Empty_UI, -1);
      Assert_True (Builder /= null);
      Unref (Builder);

      Gtk_New_From_String (Builder, Empty_UI, Gssize (Empty_UI'Length));
      Assert_True (Builder /= null);
      Unref (Builder);

      Builder := Gtk_Builder_New_From_File ("empty.ui");
      Assert_True (Builder /= null);
      Unref (Builder);

      Gtk_New_From_File (Builder, "empty.ui");
      Assert_True (Builder /= null);
      Unref (Builder);

      Builder := Gtk_Builder_New_From_Resource (Ui_Resource_Path);
      Assert_True (Builder /= null);
      Unref (Builder);

      Gtk_New_From_Resource (Builder, Ui_Resource_Path);
      Assert_True (Builder /= null);
      Unref (Builder);

      --  The procedural Initializers also take their non-no-op path when
      --  called on a fresh object.
      Gtk_New (Builder);
      Builder.Initialize_From_String (Empty_UI, -1);
      Unref (Builder);

      Gtk_New (Builder);
      Builder.Initialize_From_File ("empty.ui");
      Unref (Builder);

      Gtk_New (Builder);
      Builder.Initialize_From_Resource (Ui_Resource_Path);
      Unref (Builder);

      Unregister (Resource);
      Unref (Resource);
   end Test_Constructor_Families;

   --------------------------------
   -- Test_Add_From_All_Sources --
   --------------------------------

   procedure Test_Add_From_All_Sources is
      Error    : Glib.Error.GError;
      Resource : Glib.Resource.Gresource := Load ("builder.gresource", Error);
      Builder  : Gtk_Builder := Gtk_Builder_New;
      Success  : Boolean;
      Object_Ids : GNAT.Strings.String_List :=
        (1 => new String'("absent-object"));
   begin
      --  Successful parses exercise each Add_From_* wrapper using an empty
      --  document, which never asks GtkBuilder to instantiate a GTK object.
      Assert_True (Error = null);
      Register (Resource);

      Success := Builder.Add_From_String (Empty_UI, Error);
      Assert_True (Success);
      Assert_True (Error = null);

      Success := Builder.Add_From_File ("empty.ui", Error);
      Assert_True (Success);
      Assert_True (Error = null);

      Success := Builder.Add_From_Resource (Ui_Resource_Path, Error);
      Assert_True (Success);
      Assert_True (Error = null);

      --  The object-filtered GTK calls require a non-empty, null-terminated
      --  id list. An absent id in an empty document still parses safely.
      Success := Builder.Add_Objects_From_String (Empty_UI, Object_Ids, Error);
      Assert_True (Success);
      Assert_True (Error = null);

      Success := Builder.Add_Objects_From_File
        ("empty.ui", Object_Ids, Error);
      Assert_True (Success);
      Assert_True (Error = null);

      Success := Builder.Add_Objects_From_Resource
        (Ui_Resource_Path, Object_Ids, Error);
      Assert_True (Success);
      Assert_True (Error = null);

      --  Missing sources return recoverable errors through the GError out
      --  parameter. Free each error before reusing the variable.
      Success := Builder.Add_From_Resource
        ("/no/such/gtkada-builder-test.ui", Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Error_Free (Error);

      Success := Builder.Add_Objects_From_File
        ("missing-gtkada-builder-test.ui", Object_Ids, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Error_Free (Error);

      Success := Builder.Add_Objects_From_Resource
        ("/no/such/gtkada-builder-test.ui", Object_Ids, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Error_Free (Error);

      --  The object-filtered string method reports malformed markup just like
      --  Add_From_String, including a populated GError.
      Success := Builder.Add_Objects_From_String
        (Malformed_UI, Object_Ids, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Error_Free (Error);

      GNAT.Strings.Free (Object_Ids (Object_Ids'First));
      Unref (Builder);
      Unregister (Resource);
      Unref (Resource);
   end Test_Add_From_All_Sources;

   ----------------------------------------
   -- Test_Object_And_Metadata_Accessors --
   ----------------------------------------

   procedure Test_Object_And_Metadata_Accessors is
      Builder : constant Gtk_Builder := Gtk_Builder_New;
      Empty   : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Model   : constant Gtk.String_List.Gtk_String_List :=
        Gtk.String_List.Gtk_String_List_New (Empty);
      Current : Glib.Object.GObject;
      Found   : Glib.Object.GObject;
      Objects : Glib.Object.Object_Simple_List.Glist;
      Scope   : constant Gtk.Builder_CScope.Gtk_Builder_C_Scope :=
        Gtk.Builder_CScope.Gtk_Builder_C_Scope_New;
      Scope_Interface : constant Gtk.Builder_Scope.Gtk_Builder_Scope :=
        Gtk.Builder_CScope."+" (Scope);
   begin
      --  The exposed object is already an Ada wrapper. Looking it up verifies
      --  that Get_Object returns that wrapper and Get_Objects returns a list
      --  containing the exposed object.
      Builder.Expose_Object ("model", Model);
      Found := Builder.Get_Object ("model");
      Assert_True (Found = Glib.Object.GObject (Model));
      Assert_True (Builder.Get_Object ("not-present") = null);

      Objects := Builder.Get_Objects;
      Assert_Cmpuint_Eq
        (Glib.Object.Object_Simple_List.Length (Objects), 1);
      Glib.Object.Object_Simple_List.Free (Objects);

      --  Current_Object is a borrowed GObject reference. Exercise both a
      --  real object and the documented null value without taking ownership.
      Assert_True (Builder.Get_Current_Object = null);
      Builder.Set_Current_Object (Model);
      Current := Builder.Get_Current_Object;
      Assert_True (Current = Glib.Object.GObject (Model));
      Builder.Set_Current_Object (null);
      Assert_True (Builder.Get_Current_Object = null);

      --  The C scope provides a concrete implementation of Builder_Scope;
      --  the interface conversion round-trips through GtkBuilder's setter.
      Builder.Set_Scope (Scope_Interface);
      Assert_True (Builder.Get_Scope = Scope_Interface);
      --  Gtk.Builder_Scope's identity operator is also part of its public
      --  interface API; check that it preserves the interface value.
      Assert_True
        (Gtk.Builder_Scope."+" (Scope_Interface) = Scope_Interface);
      Builder.Set_Scope (Gtk.Builder_Scope.Null_Gtk_Builder_Scope);

      --  Type lookup returns a registered type and G_TYPE_INVALID for a name
      --  the default C scope cannot resolve.
      Assert_True
        (Builder.Get_Type_From_Name ("GtkStringList") =
           Gtk.String_List.Get_Type);
      Assert_True
        (Builder.Get_Type_From_Name ("GtkAdaNoSuchType") = GType_Invalid);

      --  The getter initially returns an empty string. Setting and clearing
      --  the translation domain exercises both null and allocated C strings.
      Assert_Cmpstr_Eq (Builder.Get_Translation_Domain, "");
      Builder.Set_Translation_Domain ("gtkada-builder-test");
      Assert_Cmpstr_Eq
        (Builder.Get_Translation_Domain, "gtkada-builder-test");
      Builder.Set_Translation_Domain;
      Assert_Cmpstr_Eq (Builder.Get_Translation_Domain, "");

      Scope.Unref;
      Model.Unref;
      Unref (Builder);
   end Test_Object_And_Metadata_Accessors;

   ----------------------------
   -- Test_Value_Conversions --
   ----------------------------

   procedure Test_Value_Conversions is
      Builder : constant Gtk_Builder := Gtk_Builder_New;
      Value   : Glib.Values.GValue;
      Error   : Glib.Error.GError;
      Success : Boolean;
      Int_Spec : constant Glib.Param_Spec :=
        Glib.Properties.Creation.Gnew_Int
          ("builder-test-value", "Builder test value", "integer value",
           -100, 100, 0);
   begin
      --  These successful conversions follow builder.c's test_value_from_string
      --  and check both the GValue type and payload returned by GtkBuilder.
      Success := Builder.Value_From_String_Type
        (GType_String, "Ada", Value, Error);
      Assert_True (Success and then Error = null);
      Assert_True (Glib.Values.Type_Of (Value) = GType_String);
      Assert_Cmpstr_Eq (Glib.Values.Get_String (Value), "Ada");
      Glib.Values.Unset (Value);

      Success := Builder.Value_From_String_Type
        (GType_Boolean, "true", Value, Error);
      Assert_True (Success and then Error = null);
      Assert_True (Glib.Values.Get_Boolean (Value));
      Glib.Values.Unset (Value);

      --  The ParamSpec overload uses the property's declared value type.
      Success := Builder.Value_From_String (Int_Spec, "42", Value, Error);
      Assert_True (Success and then Error = null);
      Assert_Cmpint_Eq (Glib.Values.Get_Int (Value), 42);
      Glib.Values.Unset (Value);

      --  An invalid scalar has GtkBuilder's Invalid_Value code. This also
      --  exercises the Ada Gtk_Builder_Error_Matches convenience predicate.
      Success := Builder.Value_From_String_Type
        (GType_Int, "not-an-integer", Value, Error);
      Assert_False (Success);
      Assert_True (Gtk_Builder_Error_Matches
        (Error, Builder_Error_Invalid_Value));
      Assert_False (Gtk_Builder_Error_Matches
        (Error, Builder_Error_Invalid_Attribute));
      Glib.Values.Unset (Value);
      Error_Free (Error);

      Unref (Builder);
   end Test_Value_Conversions;

   -------------------------------------------
   -- Test_Extend_With_Template_Parse_Error --
   -------------------------------------------

   procedure Test_Extend_With_Template_Parse_Error is
      Builder : constant Gtk_Builder := Gtk_Builder_New;
      Label   : constant Gtk.Label.Gtk_Label := Gtk.Label.Gtk_Label_New ("safe");
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      --  The label's runtime type matches Template_Type. Malformed markup is
      --  rejected before GtkBuilder tries to instantiate template children.
      --  GtkLabel is initially floating, so sink it before unref at teardown.
      Label.Ref_Sink;
      Success := Builder.Extend_With_Template
        (Label, Gtk.Label.Get_Type, Malformed_UI, -1, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Error_Free (Error);

      Label.Unref;
      Unref (Builder);
   end Test_Extend_With_Template_Parse_Error;

begin
   Glib.Test.Init;
   --  This suite includes one GtkLabel value for the template error path.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/builder/add-from-string/malformed",
      Test_Add_From_String_Malformed'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/add-from-file/missing",
      Test_Add_From_File_Missing'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/constructors", Test_Constructor_Families'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/add-from-all-sources",
      Test_Add_From_All_Sources'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/objects-and-metadata",
      Test_Object_And_Metadata_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/value-conversions", Test_Value_Conversions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/builder/extend-template/parse-error",
      Test_Extend_With_Template_Parse_Error'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
