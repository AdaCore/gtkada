--  Checks Gtk.Arguments' checked and unchecked conversions from GValues.
--  The inputs are small C-layout GValue arrays, as GtkAda signal marshallers
--  receive them.  Each test verifies the extracted value or preserved pointer.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with Cairo;
with Cairo.Image_Surface;
with Cairo.Region;
with Gdk.Rectangle;
with Glib;
with Glib.Arrays;
with Glib.Object;
with Glib.Test;          use Glib.Test;
with Glib.Types;
with Glib.Values;        use Glib.Values;
with Gtk.Arguments;
with Gtk.Drawing_Area;
with Gtk.Main;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Gtk_Arguments is

   use type Glib.C_Proxy;
   use type Glib.Gint;
   use type Glib.Guint;
   use type Glib.Gint64;
   use type Glib.Gdouble;
   use type Cairo.Cairo_Context;
   use type Gtk.Gtk_Notebook_Page;
   use type System.Address;

   --  Convert addresses to the opaque pointer type used by Gtk.Arguments.
   function To_Proxy is new Ada.Unchecked_Conversion
     (System.Address, Glib.C_Proxy);
   function To_Address is new Ada.Unchecked_Conversion
     (Cairo.Cairo_Context, System.Address);

   --  GLib exposes the int64 setter in C even though Glib.Values only binds
   --  its getter; this initializes the standard GValue used by the test.
   procedure Set_Int64 (Value : in out GValue; Number : Glib.Gint64);
   pragma Import (C, Set_Int64, "g_value_set_int64");

   procedure Test_Checked_Conversions with Convention => C;
   procedure Test_Unchecked_Conversions with Convention => C;

   -----------------------------
   -- Test_Checked_Conversions --
   -----------------------------

   procedure Test_Checked_Conversions is
      --  Signal callbacks include the instance in slot zero; Gtk_Args' first
      --  positive argument index therefore addresses slot one.
      type C_Value_Array is array (Glib.Gint range 0 .. 7) of GValue;
      pragma Convention (C, C_Value_Array);

      Values       : C_Value_Array;
      Args         : Gtk.Arguments.Gtk_Args;
      Pointer_Data : aliased Glib.Gint := 91;
      Widget       : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Proxy        : constant Glib.C_Proxy := To_Proxy (Pointer_Data'Address);
   begin
      --  Put the callback instance first, followed by each value under test.
      Gtk.Drawing_Area.Gtk_New (Widget);
      Init (Values (0), Glib.GType_Object);
      Set_Object (Values (0), Widget);
      Init_Set_Int (Values (1), -17);
      Init_Set_Uint (Values (2), 42);
      Init_Set_Boolean (Values (3), True);
      Init_Set_String (Values (4), "checked string");
      Init (Values (5), Glib.GType_Pointer);
      Set_Address (Values (5), Pointer_Data'Address);
      Init (Values (6), Glib.GType_Object);
      Set_Object (Values (6), Widget);
      Init (Values (7), Glib.GType_Pointer);
      Set_Proxy (Values (7), Proxy);

      --  Make_Args wraps this C array; it does not own the array's storage.
      Args := Gtk.Arguments.Make_Args
        (8, Glib.Values.C_GValues (Values'Address));

      --  Check each public Gtk_Args conversion against its source value.
      Assert_Cmpint_Eq (Gtk.Arguments.To_Gint (Args, 1), -17);
      Assert_Cmpuint_Eq (Gtk.Arguments.To_Guint (Args, 2), 42);
      Assert_True (Gtk.Arguments.To_Boolean (Args, 3));
      Assert_Cmpstr_Eq (Gtk.Arguments.To_String (Args, 4), "checked string");
      Assert_True
        (Gtk.Arguments.To_Address (Args, 5) = Pointer_Data'Address);
      Assert_True (Gtk.Arguments.To_C_Proxy (Args, 7) = Proxy);
      Assert_True
        (Gtk.Arguments.To_Notebook_Page (Args, 7) =
           Gtk.Gtk_Notebook_Page (Proxy));
      Assert_True
        (Glib.Object.Get_Object (Gtk.Arguments.To_Object (Args, 6)) =
           Glib.Object.Get_Object (Widget));

      --  These GValues are owned by this test and must be unset after use.
      for Value of Values loop
         Unset (Value);
      end loop;
   end Test_Checked_Conversions;

   --------------------------------
   -- Test_Unchecked_Conversions --
   --------------------------------

   procedure Test_Unchecked_Conversions is
      --  Unchecked conversions use zero-based indexing over the incoming
      --  GValue array, matching the C signal callback convention.
      type C_Value_Array is array (Glib.Gint range 0 .. 14) of GValue;
      pragma Convention (C, C_Value_Array);

      Values         : C_Value_Array;
      Pointer_Int    : aliased Glib.Gint := 123;
      Pointer_Double : aliased Glib.Gdouble := 6.5;
      Rect           : aliased Cairo.Region.Cairo_Rectangle_Int :=
        (X => 3, Y => 4, Width => 25, Height => 16);
      Array_Data     : aliased Glib.Arrays.Garray :=
        (Data => Gtkada.Types.Null_Ptr, Len => 2);
      Strings        : aliased Gtkada.Types.Chars_Ptr_Array (0 .. 1) :=
        (others => Gtkada.Types.Null_Ptr);
      Widget         : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Surface        : Cairo.Cairo_Surface;
      Context        : Cairo.Cairo_Context;
      Args           : constant Glib.Values.C_GValues :=
        Glib.Values.C_GValues (Values'Address);
   begin
      --  Initialize values for the primitive getters and access conversions.
      Init_Set_Boolean (Values (0), True);
      Init (Values (1), Glib.GType_Double);
      Set_Double (Values (1), 6.5);
      Init_Set_Int (Values (2), -23);
      Init (Values (3), Glib.GType_Int64);
      Set_Int64 (Values (3), 1234567890123);
      Init (Values (4), Glib.GType_Pointer);
      Set_Address (Values (4), Pointer_Int'Address);
      Init (Values (5), Glib.GType_Pointer);
      Set_Address (Values (5), Pointer_Double'Address);
      Init_Set_Uint (Values (6), 314);
      Init_Set_String (Values (7), "unchecked string");
      Init (Values (8), Glib.GType_Pointer);
      Set_Address (Values (8), Pointer_Int'Address);

      --  Object and interface conversions preserve a live Gtk object pointer.
      Gtk.Drawing_Area.Gtk_New (Widget);
      Init (Values (9), Glib.GType_Object);
      Set_Object (Values (9), Widget);
      Init (Values (10), Glib.GType_Pointer);
      Set_Address (Values (10), Glib.Object.Get_Object (Widget));

      --  Cairo and GDK rectangle views share the same four C integer fields.
      Init (Values (11), Glib.GType_Pointer);
      Set_Proxy (Values (11), To_Proxy (Rect'Address));

      --  Cairo contexts are pointers carried by signal arguments.
      Surface := Cairo.Image_Surface.Create
        (Cairo.Image_Surface.Cairo_Format_ARGB32, 2, 2);
      Context := Cairo.Create (Surface);
      Init (Values (12), Glib.GType_Pointer);
      Set_Address (Values (12), To_Address (Context));

      --  The array conversions only expose the addresses and fields supplied.
      Init (Values (13), Glib.GType_Pointer);
      Set_Address (Values (13), Array_Data'Address);
      Init (Values (14), Glib.GType_Pointer);
      Set_Address (Values (14), Strings'Address);

      --  Check the scalar, pointer, rectangle, context, and array results.
      Assert_True (Gtk.Arguments.Unchecked_To_Boolean (Args, 0));
      Assert_True (Gtk.Arguments.Unchecked_To_Gdouble (Args, 1) = 6.5);
      Assert_Cmpint_Eq (Gtk.Arguments.Unchecked_To_Gint (Args, 2), -23);
      Assert_True
        (Gtk.Arguments.Unchecked_To_Gint64 (Args, 3) = 1234567890123);
      Assert_Cmpint_Eq
        (Gtk.Arguments.Unchecked_To_Gint_Access (Args, 4).all, 123);
      Assert_True
        (Gtk.Arguments.Unchecked_To_Gdouble_Access (Args, 5).all = 6.5);
      Assert_Cmpuint_Eq (Gtk.Arguments.Unchecked_To_Guint (Args, 6), 314);
      Assert_Cmpstr_Eq
        (Gtk.Arguments.Unchecked_To_UTF8_String (Args, 7),
         "unchecked string");
      Assert_True
        (Gtk.Arguments.Unchecked_To_Address (Args, 8) = Pointer_Int'Address);
      Assert_True
        (Glib.Object.Get_Object (Gtk.Arguments.Unchecked_To_Object (Args, 9)) =
           Glib.Object.Get_Object (Widget));
      Assert_True
        (System.Address (Gtk.Arguments.Unchecked_To_Interface (Args, 10)) =
           Glib.Object.Get_Object (Widget));

      Assert_Cmpint_Eq
        (Gtk.Arguments.Unchecked_To_Cairo_Rectangle_Int_Access
           (Args, 11).X, 3);
      Assert_Cmpint_Eq
        (Gtk.Arguments.Unchecked_To_Cairo_Rectangle_Int (Args, 11).Height, 16);
      Assert_Cmpint_Eq
        (Gtk.Arguments.Unchecked_To_Gdk_Rectangle (Args, 11).Width, 25);
      Assert_Cmpint_Eq
        (Gtk.Arguments.Unchecked_To_Gdk_Rectangle_Access
           (Args, 11).Y, 4);
      Assert_True
        (Gtk.Arguments.Unchecked_To_Cairo_Context (Args, 12) = Context);
      Assert_Cmpuint_Eq
        (Gtk.Arguments.Unchecked_To_Garray_Access (Args, 13).Len, 2);
      Assert_True
        (Gtk.Arguments.Unchecked_To_chars_ptr_array_access
           (Args, 14).all'Address = Strings'Address);

      --  Release the owned values and Cairo resources after the assertions.
      for Value of Values loop
         Unset (Value);
      end loop;
      Cairo.Destroy (Context);
      Cairo.Surface_Destroy (Surface);
   end Test_Unchecked_Conversions;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gtk/arguments/checked", Test_Checked_Conversions'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk/arguments/unchecked",
      Test_Unchecked_Conversions'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gtk_Arguments;
