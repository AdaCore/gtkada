--  Tests Gtk.Map_List_Model construction, callback mapping, model replacement,
--  list/section interfaces, and both callback forms.  Cases port the behavior
--  covered by testsuite/c_tests/maplistmodel.c, with Ada-side result checks.

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;

with Glib;                    use Glib;
with Glib.List_Model;         use Glib.List_Model;
with Glib.List_Store;         use Glib.List_Store;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Gtk.Main;
with Gtk.Map_List_Model;      use Gtk.Map_List_Model;
with Gtk.String_Object;       use Gtk.String_Object;

procedure Main is

   Factor : Guint := 2;
   Last_Mapped_Value : Guint := 0;
   Destroy_Count : Natural := 0;

   Items_Position : Guint := Guint'Last;
   Items_Removed  : Guint := Guint'Last;
   Items_Added    : Guint := Guint'Last;
   Sections_Position : Guint := Guint'Last;
   Sections_Count    : Guint := Guint'Last;

   procedure Add_String (Store : Glist_Store; Value : String);
   function Map_String (Item : GObject) return GObject;
   function Map_String_User
     (Item : GObject; User_Data : Gint) return GObject;
   procedure Destroy_User_Data (Data : in out Gint);

   procedure Test_Construct_And_Map with Convention => C;
   procedure Test_Set_Model_And_Function with Convention => C;
   procedure Test_User_Data_Callbacks with Convention => C;
   procedure Test_Change_Signals with Convention => C;

   --  These callbacks observe the interface signals emitted by the two
   --  inherited notification wrappers under test.
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
      Detailed_Signal : Interfaces.C.Strings.chars_ptr;
      Handler : Items_Handler;
      User_Data : System.Address;
      Destroy_Data : System.Address;
      Flags : Gint) return Gulong;
   pragma Import (C, Signal_Connect_Items, "g_signal_connect_data");

   function Signal_Connect_Sections
     (Instance : System.Address;
      Detailed_Signal : Interfaces.C.Strings.chars_ptr;
      Handler : Sections_Handler;
      User_Data : System.Address;
      Destroy_Data : System.Address;
      Flags : Gint) return Gulong;
   pragma Import (C, Signal_Connect_Sections, "g_signal_connect_data");

   package User_Mapper is new Map_List_Model_User_Data
     (User_Data_Type => Gint,
      Destroy => Destroy_User_Data);

   -----------------
   -- Add_String --
   -----------------

   procedure Add_String (Store : Glist_Store; Value : String) is
      Item : constant Gtk_String_Object := Gtk_String_Object_New (Value);
   begin
      --  The list store takes its own reference to each string object.
      Append (Store, Item);
      Unref (Item);
   end Add_String;

   -----------------
   -- Map_String --
   -----------------

   function Map_String (Item : GObject) return GObject is
      Value : constant String := Gtk_String_Object (Item).Get_String;
   begin
      --  Returning the input itself is valid because its full reference is
      --  transferred to the callback and back to the model.  Record the
      --  transformed value to check that the callback and factor were used.
      Last_Mapped_Value := Guint (Value'Length) * Factor;
      return Item;
   end Map_String;

   ----------------------
   -- Map_String_User --
   ----------------------

   function Map_String_User
     (Item : GObject; User_Data : Gint) return GObject
   is
      Value : constant String := Gtk_String_Object (Item).Get_String;
   begin
      --  The user-data callback applies its supplied factor and returns the
      --  transferred input reference unchanged.
      Last_Mapped_Value := Guint (Value'Length) * Guint (User_Data);
      return Item;
   end Map_String_User;

   -----------------------
   -- Destroy_User_Data --
   -----------------------

   procedure Destroy_User_Data (Data : in out Gint) is
   begin
      --  Count closure destruction so replacement and finalization are tested.
      Destroy_Count := Destroy_Count + 1;
      Data := 0;
   end Destroy_User_Data;

   --------------------------
   -- Test_Construct_And_Map --
   --------------------------

   procedure Test_Construct_And_Map is
      Store  : constant Glist_Store := Glist_Store_New (Gtk.String_Object.Get_Type);
      Source : constant Glist_Model := +Store;
      Map    : Gtk_Map_List_Model;
      Item   : GObject;
      Start  : Guint;
      Finish : Guint;
      List   : Glist_Model;
   begin
      Add_String (Store, "a");
      Add_String (Store, "bb");
      Add_String (Store, "ccc");

      --  The constructor consumes a model reference, so retain the test's
      --  store reference before passing the interface.
      Ref (Store);
      --  The function constructor covers the non-null callback path.  A
      --  repeated Initialize is a no-op and must preserve the original model.
      Map := Gtk_Map_List_Model_New
        (Source, Map_String'Unrestricted_Access);
      Initialize (Map, Null_Glist_Model, null);
      Assert_True (Has_Map (Map));
      Assert_True (Get_Model (Map) = Source);
      --  The map callback is declared to return a GObject, so GTK reports
      --  that general type for the mapped model.
      Assert_True (Get_Item_Type (Map) = GType_Object);
      Assert_Cmpuint_Eq (Get_N_Items (Map), 3);

      --  The list-model conversions exercise the generated interface adapters.
      List := +Map;
      Assert_True ((-List) = Map);

      --  Get_Item returns the callback's mapped GObject with a full reference.
      Item := Get_Item (Map, 1);
      Assert_True (Item /= null);
      Assert_True (Gtk_String_Object (Item).Get_String = "bb");
      Assert_Cmpuint_Eq (Last_Mapped_Value, 4);
      Unref (Item);

      --  GtkMapListModel implements GtkSectionModel even when the source is a
      --  plain list model; its reported range must contain the queried item.
      Get_Section (Map, 1, Start, Finish);
      Assert_Cmpuint_Le (Start, 1);
      Assert_Cmpuint_Gt (Finish, 1);
      Assert_Cmpuint_Le (Finish, Get_N_Items (Map));

      Unref (Map);
      Unref (Store);
   end Test_Construct_And_Map;

   -------------------------------
   -- Test_Set_Model_And_Function --
   -------------------------------

   procedure Test_Set_Model_And_Function is
      Store : constant Glist_Store := Glist_Store_New (Gtk.String_Object.Get_Type);
      Source : constant Glist_Model := +Store;
      Map : Gtk_Map_List_Model;
      Item : GObject;
   begin
      --  Gtk_New covers the null callback constructor path.
      Gtk_New (Map, Null_Glist_Model, null);
      Add_String (Store, "four");
      Add_String (Store, "five");

      --  A null model and callback produce an empty identity map.
      Assert_False (Has_Map (Map));
      Assert_True (Get_Model (Map) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Map), 0);

      Set_Model (Map, Source);
      Assert_True (Get_Model (Map) = Source);
      Assert_Cmpuint_Eq (Get_N_Items (Map), 2);
      Assert_False (Has_Map (Map));

      --  Switching callbacks replaces mapped values, and a null callback
      --  exposes source items directly as GTK's identity behavior specifies.
      --  With no mapper, the model returns source items unchanged.
      Item := Get_Item (Map, 1);
      Assert_True (Gtk_String_Object (Item).Get_String = "five");
      Unref (Item);

      --  Adding a mapper maps source items; replacing it runs the new callback.
      Factor := 3;
      Set_Map_Func (Map, Map_String'Unrestricted_Access);
      Assert_True (Has_Map (Map));
      Item := Get_Item (Map, 1);
      Assert_True (Gtk_String_Object (Item).Get_String = "five");
      Assert_Cmpuint_Eq (Last_Mapped_Value, 12);
      Unref (Item);

      --  Replacing one non-null mapper with another is supported by GTK and
      --  covers the binding's callback replacement path.
      Factor := 4;
      Set_Map_Func (Map, Map_String'Unrestricted_Access);
      Assert_True (Has_Map (Map));
      Item := Get_Item (Map, 1);
      Assert_True (Gtk_String_Object (Item).Get_String = "five");
      Assert_Cmpuint_Eq (Last_Mapped_Value, 16);
      Unref (Item);

      Set_Model (Map, Null_Glist_Model);
      Assert_True (Get_Model (Map) = Null_Glist_Model);
      Assert_Cmpuint_Eq (Get_N_Items (Map), 0);

      Unref (Map);
      Unref (Store);
   end Test_Set_Model_And_Function;

   ----------------------------
   -- Test_User_Data_Callbacks --
   ----------------------------

   procedure Test_User_Data_Callbacks is
      Store  : constant Glist_Store := Glist_Store_New (Gtk.String_Object.Get_Type);
      Source : constant Glist_Model := +Store;
      Map    : Gtk_Map_List_Model;
      Other  : Gtk_Map_List_Model;
      Null_Map : Gtk_Map_List_Model;
      Count_Before : Natural;
      Item : GObject;
   begin
      Add_String (Store, "two");
      Add_String (Store, "six");
      Destroy_Count := 0;

      --  Exercise the function-returning constructor and the closure callback.
      Ref (Store);
      Map := User_Mapper.Gtk_Map_List_Model_New_User
        (Source, Map_String_User'Unrestricted_Access, 4);
      Assert_True (Has_Map (Map));
      Item := Get_Item (Map, 1);
      Assert_True (Gtk_String_Object (Item).Get_String = "six");
      Assert_Cmpuint_Eq (Last_Mapped_Value, 12);
      Unref (Item);

      --  Replacing a user-data callback frees the previous closure.  GTK
      --  rejects a null callback with a non-null destroy function; that
      --  binding path is recorded below as a binding issue.
      User_Mapper.Set_Map_Func
        (Map, Map_String_User'Unrestricted_Access, 3);
      Assert_True (Has_Map (Map));
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 1);
      Item := Get_Item (Map, 0);
      Assert_True (Gtk_String_Object (Item).Get_String = "two");
      Assert_Cmpuint_Eq (Last_Mapped_Value, 9);
      Unref (Item);

      --  Gtk_New_User and redundant Initialize_User exercise the out
      --  constructor and its already-created guard.
      Ref (Store);
      User_Mapper.Gtk_New_User
        (Other, Source, Map_String_User'Unrestricted_Access, 5);
      Count_Before := Destroy_Count;
      User_Mapper.Initialize_User
        (Other, Null_Glist_Model, null, 0);
      Assert_True (Get_Model (Other) = Source);
      Assert_True (Has_Map (Other));
      Assert_Cmpuint_Eq (Guint (Destroy_Count), Guint (Count_Before));
      Item := Get_Item (Other, 1);
      Assert_True (Gtk_String_Object (Item).Get_String = "six");
      Assert_Cmpuint_Eq (Last_Mapped_Value, 15);
      Unref (Item);

      --  The generic function constructor also handles a null mapping
      --  function and frees its empty closure when the object is released.
      Ref (Store);
      Null_Map := User_Mapper.Gtk_Map_List_Model_New_User
        (Source, null, 0);
      Assert_False (Has_Map (Null_Map));
      Assert_True (Get_Model (Null_Map) = Source);
      Unref (Null_Map);
      User_Mapper.Initialize_User (Other, Source, null, 0);
      Unref (Other);
      Unref (Map);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), Guint (Count_Before + 2));
      Unref (Store);
   end Test_User_Data_Callbacks;

   -------------------------
   -- Test_Change_Signals --
   -------------------------

   procedure Test_Change_Signals is
      Store : constant Glist_Store :=
        Glist_Store_New (Gtk.String_Object.Get_Type);
      Source : constant Glist_Model := +Store;
      Map : Gtk_Map_List_Model;
      Items_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("items-changed");
      Sections_Name : Interfaces.C.Strings.chars_ptr :=
        Interfaces.C.Strings.New_String ("sections-changed");
      Items_Id, Sections_Id : Gulong;
   begin
      Add_String (Store, "one");
      Add_String (Store, "two");
      Add_String (Store, "three");
      --  As with the other constructors, GtkMapListModel takes a full model
      --  reference, so preserve the store's creator reference.
      Ref (Store);
      Map := Gtk_Map_List_Model_New (Source, null);

      --  Register C-convention observers so the notification wrapper results
      --  can be checked exactly, including each signal's supplied positions.
      Items_Position := Guint'Last;
      Items_Removed := Guint'Last;
      Items_Added := Guint'Last;
      Sections_Position := Guint'Last;
      Sections_Count := Guint'Last;
      Items_Id := Signal_Connect_Items
        (Get_Object (Map), Items_Name, On_Items_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Sections_Id := Signal_Connect_Sections
        (Get_Object (Map), Sections_Name, On_Sections_Changed'Unrestricted_Access,
         System.Null_Address, System.Null_Address, 0);
      Interfaces.C.Strings.Free (Items_Name);
      Interfaces.C.Strings.Free (Sections_Name);
      Assert_Cmpuint_Gt (Guint (Items_Id), 0);
      Assert_Cmpuint_Gt (Guint (Sections_Id), 0);

      Items_Changed (Map, 1, 1, 1);
      Assert_Cmpuint_Eq (Items_Position, 1);
      Assert_Cmpuint_Eq (Items_Removed, 1);
      Assert_Cmpuint_Eq (Items_Added, 1);
      Sections_Changed (Map, 0, 3);
      Assert_Cmpuint_Eq (Sections_Position, 0);
      Assert_Cmpuint_Eq (Sections_Count, 3);
      Unref (Map);
      Unref (Store);
   end Test_Change_Signals;

   -----------------------
   -- On_Items_Changed --
   -----------------------

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

   -------------------------
   -- On_Sections_Changed --
   -------------------------

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
   Add_Func ("/map-list-model/construct-and-map", Test_Construct_And_Map'Unrestricted_Access);
   Add_Func ("/map-list-model/set-model-and-function", Test_Set_Model_And_Function'Unrestricted_Access);
   Add_Func ("/map-list-model/user-data-callbacks", Test_User_Data_Callbacks'Unrestricted_Access);
   Add_Func ("/map-list-model/change-signals", Test_Change_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
