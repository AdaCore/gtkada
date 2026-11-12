--  Exercise Pango tab-array construction, copying, resizing, and ownership.
--  No C testsuite case directly covers pango_tab_array_*.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Pango.Tabs; use Pango.Tabs;

procedure Main is
   use type Glib.Gint;
   use type Pango.Tabs.Pango_Tab_Align;

   --  The helper frees its wrapper with g_free, so allocate wrapper storage
   --  through GLib and convert that address to the appropriate Ada access.
   type Tab_Array_Access is access all Pango_Tab_Array;
   function To_Tab_Array_Access is new Ada.Unchecked_Conversion
     (System.Address, Tab_Array_Access);

   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   procedure Test_Array_Lifecycle with Convention => C;
   procedure Test_Wrapper_Conversion with Convention => C;

   -------------------------
   -- Test_Array_Lifecycle --
   -------------------------

   --  Check both constructors, then verify that resize and copy preserve
   --  the size, unit choice, and tab locations expected by Pango.
   procedure Test_Array_Lifecycle is
      Function_Array : Pango_Tab_Array :=
        Pango_Tab_Array_New (2, Positions_In_Pixels => False);
      Procedure_Array : Pango_Tab_Array;
      Copy_Array : Pango_Tab_Array;
      Alignment : Pango_Tab_Align;
      Location : Gint;
   begin
      Gdk_New (Procedure_Array, 1, Positions_In_Pixels => True);
      Assert_Cmpint_Eq (Get_Size (Procedure_Array), 1);
      Assert_True (Get_Positions_In_Pixels (Procedure_Array));
      Get_Tab (Procedure_Array, 0, Alignment, Location);
      Assert_True (Alignment = Pango_Tab_Left);
      Assert_Cmpint_Eq (Location, 0);

      --  Resize adds a new slot; initialize it before reading its value.
      Set_Tab (Procedure_Array, 0, Pango_Tab_Left, 24);
      Resize (Procedure_Array, 2);
      Assert_Cmpint_Eq (Get_Size (Procedure_Array), 2);
      Set_Tab (Procedure_Array, 1, Pango_Tab_Left, 48);

      --  A copy owns an independent array, so changing it leaves the source
      --  tab stop untouched.
      Copy_Array := Copy (Procedure_Array);
      Assert_Cmpint_Eq (Get_Size (Copy_Array), 2);
      Get_Tab (Copy_Array, 1, Alignment, Location);
      Assert_True (Alignment = Pango_Tab_Left);
      Assert_Cmpint_Eq (Location, 48);
      Set_Tab (Copy_Array, 1, Pango_Tab_Left, 72);
      Get_Tab (Procedure_Array, 1, Alignment, Location);
      Assert_Cmpint_Eq (Location, 48);
      Get_Tab (Copy_Array, 1, Alignment, Location);
      Assert_Cmpint_Eq (Location, 72);

      Assert_Cmpint_Eq (Get_Size (Function_Array), 2);
      Assert_False (Get_Positions_In_Pixels (Function_Array));
      Set_Tab (Function_Array, 0, Pango_Tab_Left, 128);
      Get_Tab (Function_Array, 0, Alignment, Location);
      Assert_Cmpint_Eq (Location, 128);

      Free (Copy_Array);
      Free (Procedure_Array);
      Free (Function_Array);
   end Test_Array_Lifecycle;

   -----------------------------
   -- Test_Wrapper_Conversion --
   -----------------------------

   --  From_Object_Free copies the wrapper's boxed handle and releases only
   --  the temporary record. The returned value then owns the tab array.
   procedure Test_Wrapper_Conversion is
      Original : constant Pango_Tab_Array :=
        Pango_Tab_Array_New (1, Positions_In_Pixels => True);
      Wrapper_Bytes : constant Gsize :=
        Gsize ((Pango_Tab_Array'Size + System.Storage_Unit - 1)
               / System.Storage_Unit);
      Wrapper : constant Tab_Array_Access :=
        To_Tab_Array_Access (G_Malloc (Wrapper_Bytes));
      Converted : Pango_Tab_Array;
      Alignment : Pango_Tab_Align;
      Location : Gint;
   begin
      Set_Tab (Original, 0, Pango_Tab_Left, 96);
      Wrapper.all.Set_Object (Get_Object (Original));
      Converted := From_Object_Free (Wrapper);
      Assert_Cmpint_Eq (Get_Size (Converted), 1);
      Assert_True (Get_Positions_In_Pixels (Converted));
      Get_Tab (Converted, 0, Alignment, Location);
      Assert_True (Alignment = Pango_Tab_Left);
      Assert_Cmpint_Eq (Location, 96);

      --  The wrapper and Original refer to the same C allocation. Free the
      --  converted owner exactly once; do not free the stale alias Original.
      Free (Converted);
   end Test_Wrapper_Conversion;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/pango-tabs/array-lifecycle",
      Test_Array_Lifecycle'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/pango-tabs/wrapper-conversion",
      Test_Wrapper_Conversion'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
