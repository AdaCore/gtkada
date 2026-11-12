--  Ports the creation, setter, and section cases from
--  testsuite/c_tests/slicelistmodel.c (test_create_empty, test_create,
--  test_set_model, test_set_slice, and test_sections). It also checks the
--  GtkAda notification wrappers by observing their emitted signal arguments.

with Ada.Command_Line;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

with Glib;                       use Glib;
with Glib.List_Model;            use Glib.List_Model;
with Glib.Object;                use Glib.Object;
with Glib.Test;                  use Glib.Test;
with Gtk.Main;
with Gtk.Section_Model;          use Gtk.Section_Model;
with Gtk.Slice_List_Model;       use Gtk.Slice_List_Model;
with Gtk.String_List;            use Gtk.String_List;
with Gtk.String_Object;          use Gtk.String_Object;

procedure Main is

   use type Glist_Model;
   use type GObject;
   use type Gtk_Section_Model;
   use type Gtk_Slice_List_Model;
   use type GType;

   --  Save signal arguments so the inherited wrappers can be checked below.
   Items_Position : Guint := Guint'Last;
   Items_Removed  : Guint := Guint'Last;
   Items_Added    : Guint := Guint'Last;
   Sections_Position : Guint := Guint'Last;
   Sections_Count    : Guint := Guint'Last;

   procedure Test_Create_Empty with Convention => C;
   procedure Test_Create_And_Access with Convention => C;
   procedure Test_Set_Model_And_Slice with Convention => C;
   procedure Test_Sections_And_Notifications with Convention => C;

   --  The signal callbacks use the C ABI expected by g_signal_connect_data.
   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   procedure On_Sections_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Items_Handler is access procedure
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   with Convention => C;

   type Sections_Handler is access procedure
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   with Convention => C;

   function Signal_Connect_Items
     (Instance : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Handler : Items_Handler;
      User_Data, Destroy_Data : System.Address;
      Flags : Gint) return Gulong
   with Import, Convention => C, External_Name => "g_signal_connect_data";

   function Signal_Connect_Sections
     (Instance : System.Address;
      Name : Interfaces.C.Strings.chars_ptr;
      Handler : Sections_Handler;
      User_Data, Destroy_Data : System.Address;
      Flags : Gint) return Gulong
   with Import, Convention => C, External_Name => "g_signal_connect_data";

   --  Use predictable string objects as the source items for every case.
   function New_Store return Gtk_String_List is
      Empty : constant GNAT.Strings.String_List (1 .. 0) := (others => null);
      Store : constant Gtk_String_List := Gtk_String_List_New (Empty);
   begin
      Append (Store, "one");
      Append (Store, "two");
      Append (Store, "three");
      Append (Store, "four");
      return Store;
   end New_Store;

   -------------------------
   -- Test_Create_Empty --
   -------------------------

   --  Both constructor forms accept a null source and produce an empty slice.
   procedure Test_Create_Empty is
      Slice : Gtk_Slice_List_Model;
   begin
      Gtk_New (Slice, Null_Glist_Model, 0, 0);
      Assert_True (Get_Model (Slice) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_Offset (Slice), 0);
      Assert_Cmpuint_Eq (Get_Size (Slice), 0);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 0);
      Assert_True (Get_Item_Type (Slice) = GType_Object);

      --  Initialize is documented as a no-op after the object is created.
      declare
         Store : constant Gtk_String_List := New_Store;
      begin
         Initialize (Slice, +Store, 1, 2);
         Assert_True (Get_Model (Slice) = Null_Glist_Model);
         Unref (Store);
      end;

      Unref (Slice);
   end Test_Create_Empty;

   -----------------------------
   -- Test_Create_And_Access --
   -----------------------------

   --  Check the function constructor, inherited list API, and interface casts.
   procedure Test_Create_And_Access is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Slice : Gtk_Slice_List_Model;
      List_View : Glist_Model;
      Section_View : Gtk_Section_Model;
      Round_Trip : Gtk_Slice_List_Model;
      Item : GObject;
   begin
      --  GTK takes the source reference; retain the caller's reference too.
      Ref (Store);
      Slice := Gtk_Slice_List_Model_New (Source, 1, 2);
      Assert_True (Get_Model (Slice) = Source);
      Assert_Cmpuint_Eq (Get_Offset (Slice), 1);
      Assert_Cmpuint_Eq (Get_Size (Slice), 2);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 2);
      Assert_True (Get_Item_Type (Slice) = Get_Item_Type (Store));

      --  List and section interface conversions must both round-trip.
      List_View := +Slice;
      Section_View := +Slice;
      Round_Trip := -List_View;
      Assert_True (Round_Trip = Slice);
      Assert_True ((-Section_View) = Slice);

      --  Get_Item returns a full reference to the item at the slice position.
      Item := Get_Item (Slice, 0);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Gtk_String_Object (Item).Get_String, "two");
      Unref (Item);
      Assert_True (Get_Item (Slice, 2) = null);

      Unref (Slice);
      Unref (Store);
   end Test_Create_And_Access;

   ----------------------------------
   -- Test_Set_Model_And_Slice --
   ----------------------------------

   --  Setters update the visible range and reflect later source-model changes.
   procedure Test_Set_Model_And_Slice is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Slice : constant Gtk_Slice_List_Model :=
        Gtk_Slice_List_Model_New (Null_Glist_Model, 0, 2);
      Item : GObject;
   begin
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 0);

      --  Set_Model accepts a model and null, reporting the corresponding size.
      Set_Model (Slice, Source);
      Assert_True (Get_Model (Slice) = Source);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 2);
      Set_Model (Slice, Null_Glist_Model);
      Assert_True (Get_Model (Slice) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 0);
      Set_Model (Slice, Source);

      --  Offset and size jointly limit the visible items.
      Set_Offset (Slice, 1);
      Assert_Cmpuint_Eq (Get_Offset (Slice), 1);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 2);
      Set_Size (Slice, 1);
      Assert_Cmpuint_Eq (Get_Size (Slice), 1);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 1);
      Assert_Cmpstr_Eq (Get_String (Store, Get_Offset (Slice)), "two");
      Set_Size (Slice, 10);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 3);

      --  A source splice shifts the selected range and remains observable.
      Remove (Store, 0);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 2);
      Item := Get_Item (Slice, 0);
      Assert_Cmpstr_Eq (Gtk_String_Object (Item).Get_String, "three");
      Unref (Item);

      --  An offset beyond the source yields an empty slice, then can be reset.
      Set_Offset (Slice, 99);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 0);
      Set_Offset (Slice, 0);
      Assert_Cmpuint_Eq (Get_N_Items (Slice), 3);

      Unref (Slice);
      Unref (Store);
   end Test_Set_Model_And_Slice;

   ---------------------------------------------
   -- Test_Sections_And_Notifications --
   ---------------------------------------------

   --  Check section passthrough and exact arguments from both notify helpers.
   procedure Test_Sections_And_Notifications is
      Store : constant Gtk_String_List := New_Store;
      Source : constant Glist_Model := +Store;
      Slice : Gtk_Slice_List_Model;
      Start, Finish : Guint;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Items_Id, Sections_Id : Gulong;
   begin
      Ref (Store);
      Slice := Gtk_Slice_List_Model_New (Source, 0, 4);

      --  A plain string list presents one section spanning its current items.
      Get_Section (Slice, 1, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, 4);

      --  Observe notifications so the wrappers' arguments are verified.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Slice), Items_Name, On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Sections_Id := Signal_Connect_Sections
        (Get_Object (Slice), Sections_Name,
         On_Sections_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Interfaces.C.Strings.Free (Sections_Name);
      Assert_Cmpuint_Gt (Guint (Items_Id), 0);
      Assert_Cmpuint_Gt (Guint (Sections_Id), 0);

      Items_Changed (Slice, 1, 2, 3);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 2);
      Assert_Cmpuint_Eq (Items_Added, 3);
      Sections_Changed (Slice, 0, 4);
      Assert_Cmpuint_Eq (Sections_Position, 0);
      Assert_Cmpuint_Eq (Sections_Count, 4);

      Unref (Slice);
      Unref (Store);
   end Test_Sections_And_Notifications;

   -----------------------
   -- Signal callbacks --
   -----------------------

   --  Record the item-change signal's three unsigned arguments verbatim.
   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Items_Position := Position;
      Items_Removed := Removed;
      Items_Added := Added;
   end On_Items_Changed;

   --  Record the section-change signal's position and range length.
   procedure On_Sections_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Sections_Position := Position;
      Sections_Count := N_Items;
   end On_Sections_Changed;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/slicelistmodel/create_empty", Test_Create_Empty'Unrestricted_Access);
   Add_Func ("/slicelistmodel/create", Test_Create_And_Access'Unrestricted_Access);
   Add_Func ("/slicelistmodel/set_model_and_slice", Test_Set_Model_And_Slice'Unrestricted_Access);
   Add_Func ("/slicelistmodel/sections", Test_Sections_And_Notifications'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
