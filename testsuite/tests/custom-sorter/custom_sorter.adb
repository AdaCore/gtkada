--  Focused tests for Gtk.Custom_Sorter, including both callback adapters.
--  The standard callback case ports the custom-sorter behavior from
--  testsuite/c_tests/sorter.c (/sorter/simple).

with Ada.Command_Line;

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Test;            use Glib.Test;
with Gtk.Custom_Sorter;    use Gtk.Custom_Sorter;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Main;
with Gtk.Sorter;           use Gtk.Sorter;
with Gtk.String_Object;    use Gtk.String_Object;

procedure Custom_Sorter is

   use type Gtk.Enums.Gtk_Ordering;

   Destroy_Count  : Natural := 0;
   Last_User_Data : Gint := 0;

   procedure Test_Standard_Callbacks with Convention => C;
   procedure Test_User_Data_Callbacks with Convention => C;

   --  Gtk.Custom_Sorter's ordinary callback follows the C convention.
   function Compare_Lexical
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   function Compare_Raises
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   function Compare_Reverse
     (A, B : not null access GObject_Record'Class) return Gint
   with Convention => C;

   --  The generic user-data callback is called by GtkAda's Ada adapter.
   function Compare_With_Data
     (A, B : not null access GObject_Record'Class;
      Data : Gint) return Gint;

   procedure Destroy_Data (Data : in out Gint);

   package Sorter_Data is new Gtk.Custom_Sorter.Custom_Sorter_User_Data
     (Gint, Destroy_Data);

   --------------------
   -- Compare_Lexical --
   --------------------

   function Compare_Lexical
     (A, B : not null access GObject_Record'Class) return Gint
   is
      Left  : constant String := Get_String (Gtk_String_Object (A));
      Right : constant String := Get_String (Gtk_String_Object (B));
   begin
      if Left < Right then
         return -1;
      elsif Left > Right then
         return 1;
      else
         return 0;
      end if;
   end Compare_Lexical;

   --------------------
   -- Compare_Reverse --
   --------------------

   function Compare_Reverse
     (A, B : not null access GObject_Record'Class) return Gint
   is
   begin
      return Compare_Lexical (B, A);
   end Compare_Reverse;

   -------------------
   -- Compare_Raises --
   -------------------

   function Compare_Raises
     (A, B : not null access GObject_Record'Class) return Gint
   is
      pragma Unreferenced (A, B);
   begin
      --  The adapter documents a neutral result when a callback raises.
      raise Program_Error;
      return 0;
   end Compare_Raises;

   ----------------------
   -- Compare_With_Data --
   ----------------------

   function Compare_With_Data
     (A, B : not null access GObject_Record'Class;
      Data : Gint) return Gint
   is
      Left  : constant String := Get_String (Gtk_String_Object (A));
      Right : constant String := Get_String (Gtk_String_Object (B));
      Order : Gint := 0;
   begin
      Last_User_Data := Data;
      if Data = Gint'Last then
         --  Exercise the generic adapter's exception fallback.
         raise Program_Error;
      elsif Left < Right then
         Order := -1;
      elsif Left > Right then
         Order := 1;
      end if;
      return Order * Data;
   end Compare_With_Data;

   ------------------
   -- Destroy_Data --
   ------------------

   procedure Destroy_Data (Data : in out Gint) is
   begin
      --  Replacing or releasing a callback must invoke its destroy handler.
      Destroy_Count := Destroy_Count + 1;
      Data := 0;
   end Destroy_Data;

   ------------------------------
   -- Test_Standard_Callbacks --
   ------------------------------

   procedure Test_Standard_Callbacks is
      Low  : constant Gtk_String_Object := Gtk_String_Object_New ("alpha");
      High : constant Gtk_String_Object := Gtk_String_Object_New ("omega");
      Sorter : Gtk_Custom_Sorter;
      Procedure_Sorter : Gtk_Custom_Sorter;
   begin
      --  The function constructor forwards the callback and returns its order.
      Sorter := Gtk_Custom_Sorter_New (Compare_Lexical'Unrestricted_Access);
      Assert_True (Compare (Sorter, Low, High) = Smaller);
      Assert_True (Compare (Sorter, High, Low) = Larger);

      --  Initialize is a no-op after construction, even with a null callback.
      Initialize (Sorter, null);
      Assert_True (Compare (Sorter, Low, High) = Smaller);

      --  Replacing the callback changes the sort direction.
      Set_Sort_Func (Sorter, Compare_Reverse'Unrestricted_Access);
      Assert_True (Compare (Sorter, Low, High) = Larger);

      --  The ordinary callback wrapper converts an Ada exception to equality.
      Set_Sort_Func (Sorter, Compare_Raises'Unrestricted_Access);
      Assert_True (Compare (Sorter, Low, High) = Equal);

      --  The procedure constructor also initializes and uses a callback.
      Gtk_New (Procedure_Sorter, Compare_Lexical'Unrestricted_Access);
      Assert_True (Compare (Procedure_Sorter, Low, High) = Smaller);

      Unref (Procedure_Sorter);
      Unref (Sorter);
      Unref (Low);
      Unref (High);
   end Test_Standard_Callbacks;

   -------------------------------
   -- Test_User_Data_Callbacks --
   -------------------------------

   procedure Test_User_Data_Callbacks is
      Low   : constant Gtk_String_Object := Gtk_String_Object_New ("alpha");
      High  : constant Gtk_String_Object := Gtk_String_Object_New ("omega");
      Sorter : Gtk_Custom_Sorter;
      Procedure_Sorter : Gtk_Custom_Sorter;
   begin
      Destroy_Count := 0;

      --  The generic constructor stores user data and passes it to comparison.
      Sorter := Sorter_Data.Gtk_Custom_Sorter_New_User
        (Compare_With_Data'Unrestricted_Access, 1);
      Assert_True (Compare (Sorter, Low, High) = Smaller);
      Assert_Cmpint_Eq (Last_User_Data, 1);

      --  Initialize_User also leaves an already-created object untouched.
      Sorter_Data.Initialize_User (Sorter, null, -1);
      Assert_True (Compare (Sorter, Low, High) = Smaller);
      Assert_Cmpint_Eq (Last_User_Data, 1);

      --  Gtk_New_User exercises the procedure constructor with user data.
      Sorter_Data.Gtk_New_User
        (Procedure_Sorter, Compare_With_Data'Unrestricted_Access, -1);
      Assert_True (Compare (Procedure_Sorter, Low, High) = Larger);
      Assert_Cmpint_Eq (Last_User_Data, -1);

      --  Replacing the callback frees its old closure and changes sort order.
      Sorter_Data.Set_Sort_Func_User
        (Sorter, Compare_With_Data'Unrestricted_Access, -1);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 1);
      Assert_True (Compare (Sorter, Low, High) = Larger);
      Assert_Cmpint_Eq (Last_User_Data, -1);

      --  Replacing again triggers the old destroy hook; exceptions map to equal.
      Sorter_Data.Set_Sort_Func_User
        (Sorter, Compare_With_Data'Unrestricted_Access, Gint'Last);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 2);
      Assert_True (Compare (Sorter, Low, High) = Equal);

      --  Final unref releases the active generic callback's user data.
      Unref (Procedure_Sorter);
      Unref (Sorter);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 4);
      Unref (Low);
      Unref (High);
   end Test_User_Data_Callbacks;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/custom-sorter/standard-callbacks",
      Test_Standard_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/custom-sorter/user-data-callbacks",
      Test_User_Data_Callbacks'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Custom_Sorter;
