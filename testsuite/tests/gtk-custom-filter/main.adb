--  Focused Gtk.Custom_Filter tests, including the custom predicate behavior
--  from testsuite/c_tests/filterlistmodel.c (test_empty_set_filter).
--  Both GtkAda callback adapters are checked through Gtk.Filter.Match.

with Ada.Command_Line;
with Ada.Exceptions;
with System;

with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Test;             use Glib.Test;
with Gtk.Custom_Filter;     use Gtk.Custom_Filter;
with Gtk.Filter;            use Gtk.Filter;
with Gtk.Main;
with Gtk.String_Object;     use Gtk.String_Object;
with Gtkada.Bindings;

procedure Main is
   use type System.Address;

   Destroy_Count  : Natural := 0;
   Last_User_Data : Integer := 0;
   Signal_Filter  : Gtk_Custom_Filter;
   Signal_Slot    : GObject;
   Typed_Changed_Count : Natural := 0;
   Slot_Changed_Count  : Natural := 0;
   Exception_Count     : Natural := 0;
   Typed_Change        : Gtk_Filter_Change := Different;
   Slot_Change         : Gtk_Filter_Change := Different;
   Typed_Source_Matches : Boolean := False;
   Slot_Source_Matches  : Boolean := False;

   procedure Test_Standard_Callbacks with Convention => C;
   procedure Test_User_Data_Callbacks with Convention => C;
   procedure Test_Filter_Methods_And_Signals with Convention => C;

   procedure On_Filter_Changed
     (Self : access Gtk_Filter_Record'Class; Change : Gtk_Filter_Change);
   procedure On_Filter_Changed_Slot
     (Self : access GObject_Record'Class; Change : Gtk_Filter_Change);
   procedure Raise_From_Filter_Callback
     (Self : access Gtk_Filter_Record'Class; Change : Gtk_Filter_Change);
   procedure Raise_From_Filter_Slot
     (Self : access GObject_Record'Class; Change : Gtk_Filter_Change);
   procedure Record_Filter_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);

   --  The ordinary GtkAda callback is invoked by the binding's C adapter.
   function Keep_Four_Characters (Item : GObject) return Boolean;
   function Reject_Item (Item : GObject) return Boolean;
   function Raise_From_Callback (Item : GObject) return Boolean;

   --  The generic callback receives the stored threshold as user data.
   function Match_Length
     (Item : GObject; Threshold : Integer) return Boolean;
   procedure Destroy_Threshold (Data : in out Integer);

   package Filter_Data is new Custom_Filter_User_Data
     (Integer, Destroy_Threshold);

   -------------------------
   -- Keep_Four_Characters --
   -------------------------

   function Keep_Four_Characters (Item : GObject) return Boolean is
   begin
      return Get_String (Gtk_String_Object (Item))'Length = 4;
   end Keep_Four_Characters;

   -----------------
   -- Reject_Item --
   -----------------

   function Reject_Item (Item : GObject) return Boolean is
      pragma Unreferenced (Item);
   begin
      return False;
   end Reject_Item;

   -------------------------
   -- Raise_From_Callback --
   -------------------------

   function Raise_From_Callback (Item : GObject) return Boolean is
      pragma Unreferenced (Item);
   begin
      --  The wrapper converts a user exception into a non-matching result.
      raise Program_Error;
      return False;
   end Raise_From_Callback;

   ------------------
   -- Match_Length --
   ------------------

   function Match_Length
     (Item : GObject; Threshold : Integer) return Boolean
   is
   begin
      Last_User_Data := Threshold;
      if Threshold = Integer'Last then
         --  The generic wrapper also protects Gtk from Ada exceptions.
         raise Program_Error;
      end if;
      return Get_String (Gtk_String_Object (Item))'Length = Threshold;
   end Match_Length;

   ----------------------
   -- Destroy_Threshold --
   ----------------------

   procedure Destroy_Threshold (Data : in out Integer) is
   begin
      --  Replacing or releasing a callback frees its stored closure data.
      Destroy_Count := Destroy_Count + 1;
      Data := 0;
   end Destroy_Threshold;

   --  Keep expected objects globally because the signal marshallers call
   --  these package-level Ada callbacks from a C closure.
   procedure On_Filter_Changed
     (Self : access Gtk_Filter_Record'Class; Change : Gtk_Filter_Change)
   is
   begin
      Typed_Changed_Count := Typed_Changed_Count + 1;
      Typed_Change := Change;
      Typed_Source_Matches := Self.Get_Object = Signal_Filter.Get_Object;
   end On_Filter_Changed;

   --  The slot overload receives the explicitly supplied GObject as Self.
   procedure On_Filter_Changed_Slot
     (Self : access GObject_Record'Class; Change : Gtk_Filter_Change)
   is
   begin
      Slot_Changed_Count := Slot_Changed_Count + 1;
      Slot_Change := Change;
      Slot_Source_Matches := Self.Get_Object = Signal_Slot.Get_Object;
   end On_Filter_Changed_Slot;

   --  Raising from each callback shape exercises the generated exception
   --  containment paths without unwinding through GObject's C frames.
   procedure Raise_From_Filter_Callback
     (Self : access Gtk_Filter_Record'Class; Change : Gtk_Filter_Change)
   is
      pragma Unreferenced (Self, Change);
   begin
      raise Program_Error with "typed Gtk.Filter changed handler";
   end Raise_From_Filter_Callback;

   procedure Raise_From_Filter_Slot
     (Self : access GObject_Record'Class; Change : Gtk_Filter_Change)
   is
      pragma Unreferenced (Self, Change);
   begin
      raise Program_Error with "slot Gtk.Filter changed handler";
   end Raise_From_Filter_Slot;

   --  Count the exceptions reported by Process_Exception for the two
   --  intentionally raising changed-signal handlers.
   procedure Record_Filter_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Occurrence);
   begin
      Exception_Count := Exception_Count + 1;
   end Record_Filter_Exception;

   ------------------------------
   -- Test_Standard_Callbacks --
   ------------------------------

   procedure Test_Standard_Callbacks is
      Keep_Item : constant Gtk_String_Object := Gtk_String_Object_New ("five");
      Other_Item : constant Gtk_String_Object := Gtk_String_Object_New ("oak");
      Filter : Gtk_Custom_Filter;
      Procedure_Filter : Gtk_Custom_Filter;
   begin
      --  The function constructor forwards the callback to Gtk.Filter.Match.
      Filter := Gtk_Custom_Filter_New
        (Keep_Four_Characters'Unrestricted_Access);
      Assert_True (Match (Filter, Keep_Item));
      Assert_False (Match (Filter, Other_Item));

      --  Initialize leaves an already-created filter and callback untouched.
      Initialize (Filter, null);
      Assert_True (Match (Filter, Keep_Item));

      --  A replacement callback is active immediately for direct Match calls.
      Set_Filter_Func (Filter, Reject_Item'Unrestricted_Access);
      Assert_False (Match (Filter, Keep_Item));

      --  Callback exceptions are caught by the binding adapter and reject.
      Set_Filter_Func (Filter, Raise_From_Callback'Unrestricted_Access);
      Assert_False (Match (Filter, Keep_Item));

      --  The procedure constructor follows the same matching contract.
      Gtk_New (Procedure_Filter, Keep_Four_Characters'Unrestricted_Access);
      Assert_True (Match (Procedure_Filter, Keep_Item));
      Assert_False (Match (Procedure_Filter, Other_Item));

      Unref (Procedure_Filter);
      Unref (Filter);
      Unref (Keep_Item);
      Unref (Other_Item);
   end Test_Standard_Callbacks;

   -------------------------------
   -- Test_User_Data_Callbacks --
   -------------------------------

   procedure Test_User_Data_Callbacks is
      Keep_Item : constant Gtk_String_Object := Gtk_String_Object_New ("five");
      Other_Item : constant Gtk_String_Object := Gtk_String_Object_New ("oak");
      Filter : Gtk_Custom_Filter;
      Procedure_Filter : Gtk_Custom_Filter;
      Initialized_Filter : Gtk_Custom_Filter;
   begin
      Destroy_Count := 0;

      --  The function constructor stores and forwards the threshold value.
      Filter := Filter_Data.Gtk_Custom_Filter_New_User
        (Match_Length'Unrestricted_Access, 4);
      Assert_True (Match (Filter, Keep_Item));
      Assert_False (Match (Filter, Other_Item));
      Assert_Cmpint_Eq (Gint (Last_User_Data), 4);

      --  Initialize_User is a no-op once the function constructor has run.
      Filter_Data.Initialize_User (Filter, null, 3);
      Assert_True (Match (Filter, Keep_Item));
      Assert_Cmpint_Eq (Gint (Last_User_Data), 4);

      --  Gtk_New_User exercises the procedure constructor with user data.
      Filter_Data.Gtk_New_User
        (Procedure_Filter, Match_Length'Unrestricted_Access, 3);
      Assert_False (Match (Procedure_Filter, Keep_Item));
      Assert_True (Match (Procedure_Filter, Other_Item));
      Assert_Cmpint_Eq (Gint (Last_User_Data), 3);

      --  Replacing a callback destroys its old data before the next match.
      Filter_Data.Set_Filter_Func_User
        (Filter, Match_Length'Unrestricted_Access, 3);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 1);
      Assert_False (Match (Filter, Keep_Item));
      Assert_Cmpint_Eq (Gint (Last_User_Data), 3);

      --  The callback adapter catches exceptions and returns false.
      Filter_Data.Set_Filter_Func_User
        (Filter, Match_Length'Unrestricted_Access, Integer'Last);
      Assert_False (Match (Filter, Keep_Item));
      Assert_Cmpint_Eq (Gint (Last_User_Data), Gint (Integer'Last));

      --  Initialize_User also initializes an uncreated Ada record directly.
      Initialized_Filter := new Gtk_Custom_Filter_Record;
      Filter_Data.Initialize_User
        (Initialized_Filter, Match_Length'Unrestricted_Access, 4);
      Assert_True (Match (Initialized_Filter, Keep_Item));

      --  Releasing active closures invokes each stored-data destroy handler.
      Unref (Initialized_Filter);
      Unref (Procedure_Filter);
      Unref (Filter);
      Assert_Cmpuint_Eq (Guint (Destroy_Count), 5);
      Unref (Keep_Item);
      Unref (Other_Item);
   end Test_User_Data_Callbacks;

   -------------------------------
   -- Test_Filter_Methods_And_Signals --
   -------------------------------

   procedure Test_Filter_Methods_And_Signals is
      Filter : constant Gtk_Custom_Filter :=
        Gtk_Custom_Filter_New (Keep_Four_Characters'Unrestricted_Access);
      Item : constant Gtk_String_Object := Gtk_String_Object_New ("four");
      Slot : constant Gtk_String_Object := Gtk_String_Object_New ("odd");
   begin
      --  A custom filter leaves strictness unknown, and its predicate still
      --  answers direct match queries through the Gtk.Filter base API.
      Assert_True (Get_Strictness (Filter) = Match_Some);
      Assert_True (Match (Filter, Item));
      Assert_False (Match (Filter, Slot));

      Signal_Filter := Filter;
      Signal_Slot := GObject (Slot);
      Typed_Changed_Count := 0;
      Slot_Changed_Count := 0;
      Exception_Count := 0;
      Gtkada.Bindings.Set_On_Exception
        (Record_Filter_Exception'Unrestricted_Access);

      --  Connect both public overloads and emit through Changed. GtkAda's
      --  slot adapter passes Slot as the callback's Self value.
      On_Changed (Filter, On_Filter_Changed'Unrestricted_Access, After => True);
      On_Changed
        (Filter, On_Filter_Changed_Slot'Unrestricted_Access, Slot => Slot);
      On_Changed (Filter, Raise_From_Filter_Callback'Unrestricted_Access);
      On_Changed
        (Filter, Raise_From_Filter_Slot'Unrestricted_Access, Slot => Slot);
      Changed (Filter, More_Strict);

      Assert_Cmpuint_Eq (Guint (Typed_Changed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Changed_Count), 1);
      Assert_True (Typed_Source_Matches);
      Assert_True (Slot_Source_Matches);
      Assert_True (Typed_Change = More_Strict);
      Assert_True (Slot_Change = More_Strict);
      Assert_Cmpuint_Eq (Guint (Exception_Count), 2);

      Unref (Filter);
      Unref (Item);
      Unref (Slot);
   end Test_Filter_Methods_And_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gtk-custom-filter/standard-callbacks",
      Test_Standard_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-custom-filter/user-data-callbacks",
      Test_User_Data_Callbacks'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gtk-custom-filter/filter-methods-and-signals",
      Test_Filter_Methods_And_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
