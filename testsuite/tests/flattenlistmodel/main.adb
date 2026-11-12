--  Tests Gtk.Flatten_List_Model. The model construction and mutation cases
--  port testsuite/c_tests/flattenlistmodel.c: test_create_empty, test_create,
--  test_model_add/remove, and test_submodel_add/add2/remove.

with Ada.Command_Line;
with GNAT.Strings;
with Interfaces.C.Strings;
with System;

with Glib;                      use Glib;
with Glib.List_Model;           use Glib.List_Model;
with Glib.List_Store;           use Glib.List_Store;
with Glib.Object;               use Glib.Object;
with Glib.Test;                 use Glib.Test;
with Gtk.Flatten_List_Model;    use Gtk.Flatten_List_Model;
with Gtk.Main;
with Gtk.Section_Model;         use Gtk.Section_Model;
with Gtk.String_Object;         use Gtk.String_Object;

procedure Main is

   use type Glist_Model;
   use type GObject;
   use type Gtk_Section_Model;
   use type Gtk_Flatten_List_Model;
   use type GType;

   --  The notification callbacks save arguments for checks after each call.
   Last_Items_Position : Guint := Guint'Last;
   Last_Items_Removed  : Guint := Guint'Last;
   Last_Items_Added    : Guint := Guint'Last;
   Last_Sections_Position : Guint := Guint'Last;
   Last_Sections_Count    : Guint := Guint'Last;

   procedure Test_Empty_Construction with Convention => C;
   procedure Test_Flatten_And_Access with Convention => C;
   procedure Test_Model_And_Submodel_Changes with Convention => C;
   procedure Test_Notification_Wrappers with Convention => C;

   --  C signal handlers match GObject's callback ABI and retain only values.
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

   --  Build GObject-typed children from GtkStringObjects for readable checks.
   function New_Child (First, Second : String := "") return Glist_Store;
   procedure Add_Child (Outer : Glist_Store; Child : Glist_Store);
   procedure Assert_Item
     (Model : Gtk_Flatten_List_Model; Position : Guint; Expected : String);

   ----------------
   -- New_Child --
   ----------------

   function New_Child (First, Second : String := "") return Glist_Store is
      Child : constant Glist_Store :=
        Glist_Store_New (GType_Object);
   begin
      --  Append retains each object; release the local reference immediately.
      if First /= "" then
         declare
            Item : constant Gtk_String_Object := Gtk_String_Object_New (First);
         begin
            Append (Child, Item);
            Unref (Item);
         end;
      end if;
      if Second /= "" then
         declare
            Item : constant Gtk_String_Object := Gtk_String_Object_New (Second);
         begin
            Append (Child, Item);
            Unref (Item);
         end;
      end if;
      return Child;
   end New_Child;

   ----------------
   -- Add_Child --
   ----------------

   procedure Add_Child (Outer : Glist_Store; Child : Glist_Store) is
   begin
      --  The outer store is typed to the GListModel interface.
      Append (Outer, Child);
   end Add_Child;

   -----------------
   -- Assert_Item --
   -----------------

   procedure Assert_Item
     (Model : Gtk_Flatten_List_Model; Position : Guint; Expected : String)
   is
      Item : constant GObject := Get_Item (Model, Position);
   begin
      --  Get_Item returns a full reference to the selected object.
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Get_String (Gtk_String_Object (Item)), Expected);
      Unref (Item);
   end Assert_Item;

   ------------------------------
   -- Test_Empty_Construction --
   ------------------------------

   procedure Test_Empty_Construction is
      Flat : Gtk_Flatten_List_Model;
      Child : constant Glist_Store := New_Child ("ignored");
   begin
      --  The procedure constructor accepts null and Initialize then stays inert.
      Gtk_New (Flat, Null_Glist_Model);
      Initialize (Flat, +Child);
      Assert_True (Get_Model (Flat) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 0);
      Assert_True (Get_Item_Type (Flat) = GType_Object);
      Assert_True (Get_Item (Flat, 0) = null);

      Unref (Flat);
      Unref (Child);
   end Test_Empty_Construction;

   ---------------------------
   -- Test_Flatten_And_Access --
   ---------------------------

   procedure Test_Flatten_And_Access is
      Outer : constant Glist_Store :=
        Glist_Store_New (Glib.List_Model.Get_Type);
      First : constant Glist_Store := New_Child ("one", "two");
      Empty : constant Glist_Store := New_Child;
      Last : constant Glist_Store := New_Child ("three");
      Source : Glist_Model;
      Flat : Gtk_Flatten_List_Model;
      List_View : Glist_Model;
      Section_View : Gtk_Section_Model;
      Round_Trip : Gtk_Flatten_List_Model;
      Child_View : Glist_Model;
      Start, Finish : Guint;
   begin
      --  Keep local child references while the outer store also owns them.
      Add_Child (Outer, First);
      Add_Child (Outer, Empty);
      Add_Child (Outer, Last);
      Source := +Outer;

      --  GTK consumes the passed model reference, so preserve our source ref.
      Ref (Outer);
      Flat := Gtk_Flatten_List_Model_New (Source);
      Assert_True (Get_Model (Flat) = Source);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 3);
      Assert_True (Get_Item_Type (Flat) = GType_Object);
      Assert_Item (Flat, 0, "one");
      Assert_Item (Flat, 1, "two");
      Assert_Item (Flat, 2, "three");
      Assert_True (Get_Item (Flat, 3) = null);

      --  Both implemented interfaces must convert back to the same object.
      List_View := +Flat;
      Section_View := +Flat;
      Round_Trip := -List_View;
      Assert_True (Round_Trip = Flat);
      Assert_True ((-Section_View) = Flat);

      --  Each nonempty child model forms a section in the flattened list.
      Get_Section (Flat, 1, Start, Finish);
      Assert_Cmpuint_Eq (Start, 0);
      Assert_Cmpuint_Eq (Finish, 2);
      Get_Section (Flat, 2, Start, Finish);
      Assert_Cmpuint_Eq (Start, 2);
      Assert_Cmpuint_Eq (Finish, 3);
      Child_View := Get_Model_For_Item (Flat, 0);
      Assert_True (Child_View = +First);
      Child_View := Get_Model_For_Item (Flat, 2);
      Assert_True (Child_View = +Last);

      --  The function constructor has initialized this object exactly once.
      Initialize (Flat, Null_Glist_Model);
      Assert_True (Get_Model (Flat) = Source);

      Unref (Flat);
      Unref (Outer);
      Unref (First);
      Unref (Empty);
      Unref (Last);
   end Test_Flatten_And_Access;

   ---------------------------------------
   -- Test_Model_And_Submodel_Changes --
   ---------------------------------------

   procedure Test_Model_And_Submodel_Changes is
      Outer : constant Glist_Store :=
        Glist_Store_New (Glib.List_Model.Get_Type);
      First : constant Glist_Store := New_Child ("one");
      Second : constant Glist_Store := New_Child ("two");
      Replacement : constant Glist_Store := New_Child ("three", "four");
      Source : Glist_Model := +Outer;
      Flat : constant Gtk_Flatten_List_Model :=
        Gtk_Flatten_List_Model_New (Null_Glist_Model);
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Items_Id : Gulong;
   begin
      --  Set_Model supports model replacement and clearing the model.
      Set_Model (Flat, Source);
      Assert_True (Get_Model (Flat) = Source);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 0);
      Items_Id := Signal_Connect_Items
        (Get_Object (Flat), Items_Name, On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Assert_Cmpuint_Gt (Guint (Items_Id), 0);

      --  Outer insertions report their flattened position and item count.
      Add_Child (Outer, First);
      Assert_Cmpuint_Eq (Last_Items_Position, 0);
      Assert_Cmpuint_Eq (Last_Items_Removed, 0);
      Assert_Cmpuint_Eq (Last_Items_Added, 1);
      Add_Child (Outer, Second);
      Assert_Cmpuint_Eq (Last_Items_Position, 1);
      Assert_Cmpuint_Eq (Last_Items_Added, 1);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 2);
      Assert_Item (Flat, 0, "one");
      Assert_Item (Flat, 1, "two");

      --  Editing child models updates the flattened positions immediately.
      declare
         Item : constant Gtk_String_Object := Gtk_String_Object_New ("one-a");
      begin
         Append (First, Item);
         Unref (Item);
      end;
      Assert_Cmpuint_Eq (Last_Items_Position, 1);
      Assert_Cmpuint_Eq (Last_Items_Added, 1);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 3);
      Assert_Item (Flat, 1, "one-a");
      Assert_Item (Flat, 2, "two");

      --  Removing a child and adding a two-item child covers outer changes.
      Remove (Outer, 0);
      Assert_Cmpuint_Eq (Last_Items_Position, 0);
      Assert_Cmpuint_Eq (Last_Items_Removed, 2);
      Assert_Cmpuint_Eq (Last_Items_Added, 0);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 1);
      Assert_Item (Flat, 0, "two");
      Add_Child (Outer, Replacement);
      Assert_Cmpuint_Eq (Last_Items_Position, 1);
      Assert_Cmpuint_Eq (Last_Items_Removed, 0);
      Assert_Cmpuint_Eq (Last_Items_Added, 2);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 3);
      Assert_Item (Flat, 1, "three");
      Assert_Item (Flat, 2, "four");

      Set_Model (Flat, Null_Glist_Model);
      Assert_True (Get_Model (Flat) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 0);
      Set_Model (Flat, Source);
      Assert_Cmpuint_Eq (Get_N_Items (Flat), 3);

      Unref (Flat);
      Unref (Outer);
      Unref (First);
      Unref (Second);
      Unref (Replacement);
   end Test_Model_And_Submodel_Changes;

   ----------------------------------
   -- Test_Notification_Wrappers --
   ----------------------------------

   procedure Test_Notification_Wrappers is
      Outer : constant Glist_Store :=
        Glist_Store_New (Glib.List_Model.Get_Type);
      Child : constant Glist_Store := New_Child ("one", "two");
      Flat : Gtk_Flatten_List_Model;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Items_Id, Sections_Id : Gulong;
   begin
      --  This two-item list gives the section notification a valid range.
      Add_Child (Outer, Child);
      Ref (Outer);
      Flat := Gtk_Flatten_List_Model_New (+Outer);

      --  Watch each interface signal so wrapper arguments are checked exactly.
      Items_Id := Signal_Connect_Items
        (Get_Object (Flat), Items_Name, On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Sections_Id := Signal_Connect_Sections
        (Get_Object (Flat), Sections_Name,
         On_Sections_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Interfaces.C.Strings.Free (Sections_Name);
      Assert_Cmpuint_Gt (Guint (Items_Id), 0);
      Assert_Cmpuint_Gt (Guint (Sections_Id), 0);

      --  These inherited helpers emit notifications; callbacks retain payloads.
      Items_Changed (Flat, 1, 1, 1);
      Assert_Cmpuint_Eq (Last_Items_Position, 1);
      Assert_Cmpuint_Eq (Last_Items_Removed, 1);
      Assert_Cmpuint_Eq (Last_Items_Added, 1);
      Sections_Changed (Flat, 0, 2);
      Assert_Cmpuint_Eq (Last_Sections_Position, 0);
      Assert_Cmpuint_Eq (Last_Sections_Count, 2);
      Unref (Flat);
      Unref (Outer);
      Unref (Child);
   end Test_Notification_Wrappers;

   -----------------------
   -- Signal callbacks --
   -----------------------

   --  Save item-change signal arguments so the test can assert each value.
   procedure On_Items_Changed
     (Instance : System.Address;
      Position, Removed, Added : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Last_Items_Position := Position;
      Last_Items_Removed := Removed;
      Last_Items_Added := Added;
   end On_Items_Changed;

   --  Save section-change arguments using the matching GObject callback ABI.
   procedure On_Sections_Changed
     (Instance : System.Address;
      Position, N_Items : Guint;
      User_Data : System.Address)
   is
      pragma Unreferenced (Instance, User_Data);
   begin
      Last_Sections_Position := Position;
      Last_Sections_Count := N_Items;
   end On_Sections_Changed;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/flattenlistmodel/create_empty", Test_Empty_Construction'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flattenlistmodel/create", Test_Flatten_And_Access'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flattenlistmodel/model_and_submodel_changes",
      Test_Model_And_Submodel_Changes'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/flattenlistmodel/notification_wrappers",
      Test_Notification_Wrappers'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
