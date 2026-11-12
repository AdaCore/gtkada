--  Exercise the parameter-spec constructors and accessors in
--  Glib.Properties.Creation, plus the property-handler bridge for a small
--  custom GObject.  No dedicated C test covers this creation API.

with Ada.Command_Line;

with Gdk.Enums;
with Glib;
with Glib.Bytes;
with Glib.Object;
with Glib.Properties;
with Glib.Properties.Creation;
with Glib.Test;
with Glib.Values;
with Gtkada.Types;
with Gtk.Button;
with Gtk.Main;

procedure Main is
   use Glib;
   use Glib.Properties.Creation;
   use Glib.Test;
   use Glib.Values;

   use type Glib.GType;
   use type Glib.Gint;
   use type Glib.Gint8;
   use type Glib.Guint8;
   use type Glib.Glong;
   use type Glib.Gfloat;
   use type Glib.Gdouble;
   use type Glib.Param_Flags;
   use type Glib.Object.GObject;

   procedure Check_Common
     (Param      : Glib.Param_Spec;
      Name       : String;
      Nick       : String;
      Blurb      : String;
      Value_Type : Glib.GType);

   procedure Test_Scalar_Specs with Convention => C;
   procedure Test_String_And_Other_Specs with Convention => C;
   procedure Test_Enum_And_Flags with Convention => C;
   procedure Test_Property_Handlers with Convention => C;

   --  A simple Ada GObject subclass lets this test install a property and
   --  exercise both callbacks installed by Set_Properties_Handlers.
   type Property_Object_Record is new Gtk.Button.Gtk_Button_Record
     with null record;
   type Property_Object is access all Property_Object_Record'Class;

   Property_Class : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;
   Property_Value : Glib.Gint := 0;

   procedure Set_Handler
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Glib.Param_Spec);

   procedure Get_Handler
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Glib.Param_Spec);

   procedure Class_Init (Self : Glib.Object.GObject_Class)
     with Convention => C;

   function Property_Type return Glib.GType;

   ------------------
   -- Check_Common --
   ------------------

   procedure Check_Common
     (Param      : Glib.Param_Spec;
      Name       : String;
      Nick       : String;
      Blurb      : String;
      Value_Type : Glib.GType)
   is
   begin
      --  Every constructor keeps its three strings and its declared value
      --  type, and the default flags permit both reading and writing.
      Assert_Cmpstr_Eq (Pspec_Name (Param), Name);
      Assert_Cmpstr_Eq (Nick_Name (Param), Nick);
      Assert_Cmpstr_Eq (Description (Param), Blurb);
      Assert_True (Glib.Properties.Creation.Value_Type (Param) = Value_Type);
      Assert_True
        (Glib.Properties.Creation.Flags (Param) =
           (Param_Readable or Param_Writable));
   end Check_Common;

   ------------------------
   -- Test_Scalar_Specs --
   ------------------------

   procedure Test_Scalar_Specs is
      Char_Spec : constant Glib.Param_Spec :=
        Gnew_Char ("char-value", "Char", "signed byte", -10, 10, 2);
      Uchar_Spec : constant Glib.Param_Spec :=
        Gnew_Uchar ("uchar-value", "Uchar", "unsigned byte", 1, 250, 7);
      Bool_Spec : constant Glib.Param_Spec :=
        Gnew_Boolean ("bool-value", "Boolean", "switch", True);
      Int_Spec : constant Glib.Param_Spec :=
        Gnew_Int ("int-value", "Integer", "signed integer", -20, 20, 3);
      Uint_Spec : constant Glib.Param_Spec :=
        Gnew_Uint ("uint-value", "Unsigned", "unsigned integer", 2, 200, 8);
      Long_Spec : constant Glib.Param_Spec :=
        Gnew_Long ("long-value", "Long", "signed long", -30, 30, 4);
      Ulong_Spec : constant Glib.Param_Spec :=
        Gnew_Ulong ("ulong-value", "Ulong", "unsigned long", 3, 300, 9);
      Unichar_Spec : constant Glib.Param_Spec :=
        Gnew_Unichar ("character", "Character", "Unicode value", 16#03BB#);
      Float_Spec : constant Glib.Param_Spec :=
        Gnew_Float ("float-value", "Float", "single precision", -1.5, 3.5, 1.25);
      Double_Spec : constant Glib.Param_Spec :=
        Gnew_Double ("double-value", "Double", "double precision", -2.5, 4.5, 1.5);
   begin
      --  Check metadata, numeric bounds, and default values for signed types.
      Check_Common (Char_Spec, "char-value", "Char", "signed byte", GType_Char);
      Assert_True (Minimum (Param_Spec_Char (Char_Spec)) = -10);
      Assert_True (Maximum (Param_Spec_Char (Char_Spec)) = 10);
      Assert_True (Default (Param_Spec_Char (Char_Spec)) = 2);
      Check_Common (Int_Spec, "int-value", "Integer", "signed integer", GType_Int);
      Assert_True (Minimum (Param_Spec_Int (Int_Spec)) = -20);
      Assert_True (Maximum (Param_Spec_Int (Int_Spec)) = 20);
      Assert_True (Default (Param_Spec_Int (Int_Spec)) = 3);
      Check_Common (Long_Spec, "long-value", "Long", "signed long", GType_Long);
      Assert_True (Minimum (Param_Spec_Long (Long_Spec)) = -30);
      Assert_True (Maximum (Param_Spec_Long (Long_Spec)) = 30);
      Assert_True (Default (Param_Spec_Long (Long_Spec)) = 4);

      --  Unsigned accessors preserve their full scalar values.
      Check_Common
        (Uchar_Spec, "uchar-value", "Uchar", "unsigned byte", GType_Uchar);
      Assert_True (Minimum (Param_Spec_Uchar (Uchar_Spec)) = 1);
      Assert_True (Maximum (Param_Spec_Uchar (Uchar_Spec)) = 250);
      Assert_True (Default (Param_Spec_Uchar (Uchar_Spec)) = 7);
      Check_Common
        (Uint_Spec, "uint-value", "Unsigned", "unsigned integer", GType_Uint);
      Assert_True (Minimum (Param_Spec_Uint (Uint_Spec)) = 2);
      Assert_True (Maximum (Param_Spec_Uint (Uint_Spec)) = 200);
      Assert_True (Default (Param_Spec_Uint (Uint_Spec)) = 8);
      Check_Common
        (Ulong_Spec, "ulong-value", "Ulong", "unsigned long", GType_Ulong);
      Assert_True (Minimum (Param_Spec_Ulong (Ulong_Spec)) = 3);
      Assert_True (Maximum (Param_Spec_Ulong (Ulong_Spec)) = 300);
      Assert_True (Default (Param_Spec_Ulong (Ulong_Spec)) = 9);

      --  Boolean, Unicode, and floating point specs expose their defaults.
      Check_Common (Bool_Spec, "bool-value", "Boolean", "switch", GType_Boolean);
      Assert_True (Default (Param_Spec_Boolean (Bool_Spec)));
      Check_Common
        (Unichar_Spec, "character", "Character", "Unicode value", GType_Uint);
      Assert_True (Default (Param_Spec_Unichar (Unichar_Spec)) = 16#03BB#);
      Check_Common
        (Float_Spec, "float-value", "Float", "single precision", GType_Float);
      Assert_True (Minimum (Param_Spec_Float (Float_Spec)) = -1.5);
      Assert_True (Maximum (Param_Spec_Float (Float_Spec)) = 3.5);
      Assert_True (Default (Param_Spec_Float (Float_Spec)) = 1.25);
      Assert_True (Epsilon (Param_Spec_Float (Float_Spec)) >= 0.0);
      Check_Common
        (Double_Spec, "double-value", "Double", "double precision", GType_Double);
      Assert_True (Minimum (Param_Spec_Double (Double_Spec)) = -2.5);
      Assert_True (Maximum (Param_Spec_Double (Double_Spec)) = 4.5);
      Assert_True (Default (Param_Spec_Double (Double_Spec)) = 1.5);
      Assert_True (Epsilon (Param_Spec_Double (Double_Spec)) >= 0.0);

      --  Constructors return owned references, released after inspection.
      Unref (Char_Spec);
      Unref (Uchar_Spec);
      Unref (Bool_Spec);
      Unref (Int_Spec);
      Unref (Uint_Spec);
      Unref (Long_Spec);
      Unref (Ulong_Spec);
      Unref (Unichar_Spec);
      Unref (Float_Spec);
      Unref (Double_Spec);
   end Test_Scalar_Specs;

   -----------------------------------
   -- Test_String_And_Other_Specs --
   -----------------------------------

   procedure Test_String_And_Other_Specs is
      String_Spec : constant Glib.Param_Spec :=
        Gnew_String ("text-value", "Text", "string data", "initial");
      Empty_Spec : constant Glib.Param_Spec :=
        Gnew_String ("empty-value", "Empty", "empty string", "");
      Param_Spec_Value : constant Glib.Param_Spec :=
        Gnew_Param ("param-value", "Param", "parameter reference", GType_Param);
      Boxed_Spec : constant Glib.Param_Spec :=
        Gnew_Boxed
          ("bytes-value", "Bytes", "boxed bytes", Glib.Bytes.Get_Type);
      Pointer_Spec : constant Glib.Param_Spec :=
        Gnew_Pointer ("pointer-value", "Pointer", "opaque pointer");
      Object_Spec : constant Glib.Param_Spec :=
        Gnew_Object ("object-value", "Object", "object reference", GType_Object);
   begin
      --  A supplied string default is preserved; unset character-set fields
      --  are reported as empty strings by the binding's null-pointer branches.
      Check_Common
        (String_Spec, "text-value", "Text", "string data", GType_String);
      Assert_Cmpstr_Eq (Default (Param_Spec_String (String_Spec)), "initial");
      Assert_Cmpstr_Eq (Cset_First (Param_Spec_String (String_Spec)), "");
      Assert_Cmpstr_Eq (Cset_Nth (Param_Spec_String (String_Spec)), "");
      Assert_True (Substitutor (Param_Spec_String (String_Spec)) = '_');
      Assert_False (Ensure_Non_Null (Param_Spec_String (String_Spec)));

      --  Empty defaults are valid strings and remain distinct from null data.
      Assert_Cmpstr_Eq (Default (Param_Spec_String (Empty_Spec)), "");
      Check_Common
        (Empty_Spec, "empty-value", "Empty", "empty string", GType_String);

      --  The remaining constructors select their documented value types.
      Check_Common
        (Param_Spec_Value, "param-value", "Param", "parameter reference", GType_Param);
      Check_Common
        (Boxed_Spec, "bytes-value", "Bytes", "boxed bytes", Glib.Bytes.Get_Type);
      Check_Common
        (Pointer_Spec, "pointer-value", "Pointer", "opaque pointer", GType_Pointer);
      Check_Common
        (Object_Spec, "object-value", "Object", "object reference", GType_Object);

      Unref (String_Spec);
      Unref (Empty_Spec);
      Unref (Param_Spec_Value);
      Unref (Boxed_Spec);
      Unref (Pointer_Spec);
      Unref (Object_Spec);
   end Test_String_And_Other_Specs;

   -------------------------
   -- Test_Enum_And_Flags --
   -------------------------

   procedure Test_Enum_And_Flags is
      Names : Gtkada.Types.Chars_Ptr_Array (0 .. 1) :=
        (0 => Gtkada.Types.New_String ("COVERAGE_FIRST"),
         1 => Gtkada.Types.New_String ("COVERAGE_SECOND"));
      Enum_Type : constant Glib.GType :=
        Register_Static_Enum ("GtkAdaCoverageEnum", Names);
      Enum_Spec : constant Glib.Param_Spec :=
        Gnew_Enum
          ("enum-value", "Enum", "enum data", Enum_Type, Default => 1);
      Enum_Class_Value : constant Enum_Class :=
        Enumeration (Param_Spec_Enum (Enum_Spec));
      First_Enum_Value : constant Enum_Value := Nth_Value (Enum_Class_Value, 0);
      Flags_Spec : constant Glib.Param_Spec :=
        Gnew_Flags
          ("flags-value", "Flags", "flag data",
           Gdk.Enums.Gdk_Modifier_Type_Get_Type,
           Guint (Gdk.Enums.Gdk_Control_Mask));
      Flags_Class_Value : constant Flags_Class :=
        Flags_Enumeration (Param_Spec_Flags (Flags_Spec));
      First_Flag_Value : constant Flags_Value := Nth_Value (Flags_Class_Value, 0);
   begin
      --  The registration API duplicates the input names, so release the
      --  caller-owned C strings immediately after type registration.
      Gtkada.Types.Free (Names);
      Check_Common
        (Enum_Spec, "enum-value", "Enum", "enum data", Enum_Type);
      Assert_True (Default (Param_Spec_Enum (Enum_Spec)) = 1);
      Assert_True (Value (First_Enum_Value) = 0);
      Assert_Cmpstr_Eq (Name (First_Enum_Value), "COVERAGE_FIRST");
      Assert_Cmpstr_Eq (Nick (First_Enum_Value), "COVERAGE_FIRST");

      --  Gdk's registered modifier flags provide a real GFlags type for the
      --  flags constructor and its enum-class accessors.
      Check_Common
        (Flags_Spec, "flags-value", "Flags", "flag data",
         Gdk.Enums.Gdk_Modifier_Type_Get_Type);
      Assert_True
        (Default (Param_Spec_Flags (Flags_Spec)) =
           Glong (Gdk.Enums.Gdk_Control_Mask));
      Assert_True (Value (First_Flag_Value) >= 0);
      Assert_True (Name (First_Flag_Value)'Length > 0);
      Assert_True (Nick (First_Flag_Value)'Length > 0);

      Unref (Enum_Spec);
      Unref (Flags_Spec);
   end Test_Enum_And_Flags;

   ------------------
   -- Set_Handler --
   ------------------

   procedure Set_Handler
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Glib.Param_Spec)
   is
      pragma Unreferenced (Object, Property_Spec);
   begin
      if Prop_Id = 1 then
         Property_Value := Get_Int (Value);
      end if;
   end Set_Handler;

   ------------------
   -- Get_Handler --
   ------------------

   procedure Get_Handler
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Glib.Param_Spec)
   is
      pragma Unreferenced (Object, Property_Spec);
   begin
      if Prop_Id = 1 then
         Set_Int (Value, Property_Value);
      end if;
   end Get_Handler;

   ----------------
   -- Class_Init --
   ----------------

   procedure Class_Init (Self : Glib.Object.GObject_Class) is
   begin
      --  Install the callback bridge before adding the writable property.
      Set_Properties_Handlers
        (Self, Set_Handler'Unrestricted_Access, Get_Handler'Unrestricted_Access);
      --  GtkButton owns the label property, so overriding it exercises the
      --  inherited-property path on this custom subclass.
      Override_Property (Self, 2, "label");
      Install_Property
        (Self, 1,
         Gnew_Int
           ("coverage-value", "Coverage", "test property", -100, 100, 0,
            Flags => Param_Readable or Param_Writable or Param_Construct));
   end Class_Init;

   -------------------
   -- Property_Type --
   -------------------

   function Property_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Button.Get_Type,
         Class_Record => Property_Class,
         Type_Name    => "GtkAdaCoveragePropertyObject",
         Class_Init   => Class_Init'Unrestricted_Access);
      return Property_Class.The_Type;
   end Property_Type;

   ---------------------------
   -- Test_Property_Handlers --
   ---------------------------

   procedure Test_Property_Handlers is
      Object : constant Property_Object := new Property_Object_Record;
      Value  : Glib.Values.GValue;
   begin
      --  Creating the C object initializes its class and installs the property.
      Glib.Object.G_New (Object.all'Access, Property_Type);
      --  GtkWidget instances are initially floating; take ownership before
      --  releasing the wrapper at the end of this case.
      Glib.Object.Ref_Sink
        (Glib.Object.GObject_Record (Object.all)'Access);
      Init (Value, GType_Int);
      Set_Int (Value, 42);
      Glib.Properties.Set_Property
        (Object.all'Access, "coverage-value", Value);
      Unset (Value);

      --  Reading the same property invokes the Ada getter and returns the
      --  value retained by the setter callback.
      Init (Value, GType_Int);
      Glib.Properties.Get_Property
        (Object.all'Access, "coverage-value", Value);
      Assert_True (Get_Int (Value) = 42);
      Unset (Value);
      Glib.Object.Unref
        (Glib.Object.GObject_Record (Object.all)'Access);
   end Test_Property_Handlers;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/glib/properties-creation/scalar-specs",
      Test_Scalar_Specs'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/properties-creation/string-and-other-specs",
      Test_String_And_Other_Specs'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/properties-creation/enum-and-flags",
      Test_Enum_And_Flags'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/glib/properties-creation/property-handlers",
      Test_Property_Handlers'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
