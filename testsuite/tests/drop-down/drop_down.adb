--  Headless tests for Gtk.Drop_Down. GTK has no matching C test in this
--  checkout, so these cases check the constructors, properties, accessibility
--  interface, and activate signal directly through the documented API.

with Ada.Command_Line;
with Ada.Exceptions;
with GNAT.Strings;
with System;

with Glib;                    use Glib;
with Glib.List_Model;
with Glib.Object;             use Glib.Object;
with Glib.Test;               use Glib.Test;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Constraint_Target;
with Gtk.Drop_Down;            use Gtk.Drop_Down;
with Gtk.Expression;
with Gtk.List_Item_Factory;
with Gtk.Main;
with Gtk.Property_Expression;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.String_Filter;
with Gtk.String_List;          use Gtk.String_List;
with Gtk.String_Object;        use Gtk.String_Object;
with Gtkada.Bindings;
with Gtkada.Types;

procedure Drop_Down is

   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Drop_Down.Gtk_Drop_Down;
   use type Gtk.Expression.Gtk_Expression;
   use type Gtk.List_Item_Factory.Gtk_List_Item_Factory;
   use type Gtk.Signal_List_Item_Factory.Gtk_Signal_List_Item_Factory;
   use type Gtk.String_List.Gtk_String_List;
   use type Gtk.String_Filter.Gtk_String_Filter_Match_Mode;
   use type Glib.GType;
   use type Glib.Object.GObject;
   use type Glib.Signal_Name;
   use type System.Address;

   Typed_Count : Natural := 0;
   Slot_Count  : Natural := 0;
   Typed_Object : System.Address := System.Null_Address;
   Slot_Object  : System.Address := System.Null_Address;
   Raise_Callbacks : Boolean := False;
   Exception_Count : Natural := 0;
   Unexpected_Exception_Count : Natural := 0;

   --  This matches the fixed arguments of g_signal_emit_by_name and emits the
   --  action signal without requiring pointer or keyboard input.
   procedure Emit_Void_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Test_Constructors with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Accessibility with Convention => C;
   procedure Test_Activate with Convention => C;
   procedure On_Typed (Self : access Gtk_Drop_Down_Record'Class);
   procedure On_Slot (Self : access Glib.Object.GObject_Record'Class);
   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   function New_Model return Gtk_String_List;
   function New_Drop_Down return Gtk_Drop_Down;
   procedure Free_Strings (Items : in out GNAT.Strings.String_List);

   -----------------
   -- Free_Strings --
   -----------------

   procedure Free_Strings (Items : in out GNAT.Strings.String_List) is
   begin
      --  GtkAda copies these Ada strings into temporary C strings when it
      --  constructs the GTK model or dropdown, so the originals can be freed.
      for Item of Items loop
         GNAT.Strings.Free (Item);
      end loop;
   end Free_Strings;

   ---------------
   -- New_Model --
   ---------------

   function New_Model return Gtk_String_List is
      Items : GNAT.Strings.String_List :=
        (new String'("cedar"), new String'("birch"), new String'("maple"));
      Result : constant Gtk_String_List := Gtk_String_List_New (Items);
   begin
      Free_Strings (Items);
      return Result;
   end New_Model;

   ------------------
   -- New_Drop_Down --
   ------------------

   function New_Drop_Down return Gtk_Drop_Down is
      Items : GNAT.Strings.String_List :=
        (new String'("cedar"), new String'("birch"), new String'("maple"));
      Result : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New_From_Strings (Items);
   begin
      Free_Strings (Items);
      return Result;
   end New_Drop_Down;

   -----------------------
   -- Test_Constructors --
   -----------------------

   procedure Test_Constructors is
      Model : constant Gtk_String_List := New_Model;
      From_Function : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New (+Model, null);
      From_Procedure : Gtk_Drop_Down;
      From_Strings_Function : constant Gtk_Drop_Down := New_Drop_Down;
      From_Strings_Procedure : Gtk_Drop_Down;
      Strings : GNAT.Strings.String_List :=
        (new String'("one"), new String'("two"));
      Expression : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      With_Expression : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New
          (+Model, Gtk.Expression.Gtk_Expression (Expression));
   begin
      --  Both model constructors retain the supplied list model. Reinitializing
      --  an already-created widget is documented to leave it untouched.
      Gtk_New (From_Procedure, +Model, null);
      Assert_True (Gtk_String_List (-From_Function.Get_Model) = Model);
      Assert_True (Gtk_String_List (-From_Procedure.Get_Model) = Model);
      From_Function.Initialize (Glib.List_Model.Null_Glist_Model, null);
      Assert_True (Gtk_String_List (-From_Function.Get_Model) = Model);

      --  Both string-array constructor forms preserve the supplied options.
      Gtk_New_From_Strings (From_Strings_Procedure, Strings);
      Free_Strings (Strings);
      Assert_Cmpuint_Eq
        (Gtk_String_List (-From_Strings_Function.Get_Model).Get_N_Items, 3);
      Assert_Cmpuint_Eq
        (Gtk_String_List (-From_Strings_Procedure.Get_Model).Get_N_Items, 2);
      Assert_Cmpstr_Eq
        (Gtk_String_List (-From_Strings_Procedure.Get_Model).Get_String (1), "two");

      --  The model constructor accepts a string-valued expression for search.
      Assert_True (With_Expression.Get_Expression /= null);
      Assert_True (Gtk.Drop_Down.Get_Type /= GType_Invalid);

      --  Initialize_From_Strings also frees its converted temporary strings
      --  when the widget already exists, without replacing its original model.
      declare
         Replacement : GNAT.Strings.String_List :=
           (1 => new String'("replacement"));
      begin
         From_Strings_Procedure.Initialize_From_Strings (Replacement);
         Free_Strings (Replacement);
      end;
      Assert_Cmpuint_Eq
        (Gtk_String_List (-From_Strings_Procedure.Get_Model).Get_N_Items, 2);
      Assert_Cmpstr_Eq
        (Gtk_String_List (-From_Strings_Procedure.Get_Model).Get_String (0), "one");
   end Test_Constructors;

   ---------------------
   -- Test_Properties --
   ---------------------

   procedure Test_Properties is
      Drop : constant Gtk_Drop_Down := New_Drop_Down;
      Model : constant Gtk_String_List := New_Model;
      Other_Model : constant Gtk_String_List := New_Model;
      Expression : constant Gtk.Property_Expression.Gtk_Property_Expression :=
        Gtk.Property_Expression.Gtk_Property_Expression_New
          (Gtk.String_Object.Get_Type, null, "string");
      Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Header_Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      List_Factory : constant Gtk_Signal_List_Item_Factory :=
        Gtk_Signal_List_Item_Factory_New;
      Empty : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New (Glib.List_Model.Null_Glist_Model, null);
   begin
      --  The model, expression, and factories round-trip through their GTK
      --  getters; interface conversions preserve the original model object.
      Drop.Set_Model (+Model);
      Assert_True (Gtk_String_List (-Drop.Get_Model) = Model);
      Drop.Set_Model (+Other_Model);
      Assert_True (Gtk_String_List (-Drop.Get_Model) = Other_Model);
      Drop.Set_Expression (Gtk.Expression.Gtk_Expression (Expression));
      Assert_True (Drop.Get_Expression /= null);

      Drop.Set_Factory (Factory);
      Drop.Set_Header_Factory (Header_Factory);
      Drop.Set_List_Factory (List_Factory);
      Assert_True (Gtk_Signal_List_Item_Factory (Drop.Get_Factory) = Factory);
      Assert_True
        (Gtk_Signal_List_Item_Factory (Drop.Get_Header_Factory) = Header_Factory);
      Assert_True
        (Gtk_Signal_List_Item_Factory (Drop.Get_List_Factory) = List_Factory);
      Drop.Set_Header_Factory (null);
      Drop.Set_List_Factory (null);
      Assert_True (Drop.Get_Header_Factory = null);
      Assert_True (Drop.Get_List_Factory = null);

      --  Check both values of the boolean properties and the enum property.
      Drop.Set_Enable_Search (True);
      Assert_True (Drop.Get_Enable_Search);
      Drop.Set_Enable_Search (False);
      Assert_False (Drop.Get_Enable_Search);
      Drop.Set_Show_Arrow (False);
      Assert_False (Drop.Get_Show_Arrow);
      Drop.Set_Show_Arrow (True);
      Assert_True (Drop.Get_Show_Arrow);
      Drop.Set_Search_Match_Mode
        (Gtk.String_Filter.String_Filter_Match_Mode_Prefix);
      Assert_True
        (Drop.Get_Search_Match_Mode =
           Gtk.String_Filter.String_Filter_Match_Mode_Prefix);

      --  Selection is a zero-based model position. An empty model starts with
      --  no selected item and reports GTK_INVALID_LIST_POSITION.
      Assert_Cmpuint_Eq (Empty.Get_Selected, Guint'Last);
      Assert_True (Empty.Get_Selected_Item = null);
      Drop.Set_Selected (1);
      Assert_Cmpuint_Eq (Drop.Get_Selected, 1);
      Assert_Cmpstr_Eq
        (Gtk_String_Object (Drop.Get_Selected_Item).Get_String, "birch");
      Drop.Set_Selected (2);
      Assert_Cmpuint_Eq (Drop.Get_Selected, 2);
      Assert_Cmpstr_Eq
        (Gtk_String_Object (Drop.Get_Selected_Item).Get_String, "maple");
   end Test_Properties;

   ------------------------
   -- Test_Accessibility --
   ------------------------

   procedure Test_Accessibility is
      Drop : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New (Glib.List_Model.Null_Glist_Model, null);
      X, Y, Width, Height : Gint;
      Accessible : constant Gtk.Accessible.Gtk_Accessible :=
        Gtk.Drop_Down.Implements_Gtk_Accessible.To_Interface (Drop);
      Buildable : constant Gtk.Buildable.Gtk_Buildable :=
        Gtk.Drop_Down.Implements_Gtk_Buildable.To_Interface (Drop);
      Target : constant Gtk.Constraint_Target.Gtk_Constraint_Target :=
        Gtk.Drop_Down.Implements_Gtk_Constraint_Target.To_Interface (Drop);
   begin
      --  Round-trip each implemented interface, then check the widget's role
      --  and initial headless accessibility state.
      Assert_True
        (Gtk.Drop_Down.Implements_Gtk_Accessible.To_Object (Accessible) = Drop);
      Assert_True
        (Gtk.Drop_Down.Implements_Gtk_Buildable.To_Object (Buildable) = Drop);
      Assert_True
        (Gtk.Drop_Down.Implements_Gtk_Constraint_Target.To_Object (Target) = Drop);
      Assert_True (Drop.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Combo_Box);
      declare
         Accessible_Id : constant UTF8_String := Drop.Get_Accessible_Id;
      begin
         Assert_Cmpuint_Ge (Guint (Accessible_Id'Length), 0);
      end;
      Assert_True (Drop.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      --  The dropdown exposes its internal button as an accessible child even
      --  when the widget has not yet been realized.
      Assert_True
        (Drop.Get_First_Accessible_Child /= Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True
        (Drop.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Drop.Get_At_Context = null);
      Assert_False (Drop.Get_Bounds (X, Y, Width, Height));
      Assert_False
        (Drop.Get_Platform_State
           (Gtk.Accessible.Accessible_Platform_State_Focused));

      --  These updates are valid for an unrealized widget and leave its role
      --  and default parent/sibling relationships stable under the headless
      --  accessibility backend used by the test driver.
      Drop.Announce
        ("Drop down test", Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Drop.Set_Accessible_Parent
        (Gtk.Accessible.Null_Gtk_Accessible,
         Gtk.Accessible.Null_Gtk_Accessible);
      Drop.Update_Next_Accessible_Sibling (Gtk.Accessible.Null_Gtk_Accessible);
      Drop.Update_Platform_State
        (Gtk.Accessible.Accessible_Platform_State_Focusable);
      Drop.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Drop.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      Drop.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Assert_True (Drop.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Combo_Box);
      Assert_True (Drop.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
   end Test_Accessibility;

   -------------------
   -- Test_Activate --
   -------------------

   procedure Test_Activate is
      Drop : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New (Glib.List_Model.Null_Glist_Model, null);
      Slot : constant Gtk_Drop_Down :=
        Gtk_Drop_Down_New (Glib.List_Model.Null_Glist_Model, null);
      Name : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("activate");
   begin
      --  Connect both signal overloads and emit activate directly. The typed
      --  callback receives the dropdown; the slot overload receives its slot.
      Assert_True (Gtk.Drop_Down.Signal_Activate = "activate");
      Typed_Count := 0;
      Slot_Count := 0;
      Typed_Object := System.Null_Address;
      Slot_Object := System.Null_Address;
      Drop.On_Activate (On_Typed'Unrestricted_Access);
      Drop.On_Activate (On_Slot'Unrestricted_Access, Slot);
      Emit_Void_Signal (Drop.Get_Object, Name);
      Assert_Cmpuint_Eq (Guint (Typed_Count), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Count), 1);
      Assert_True (Typed_Object = Drop.Get_Object);
      Assert_True (Slot_Object = Slot.Get_Object);

      --  A callback exception must be contained by each generated marshaller
      --  and delivered to GtkAda's exception hook instead of crossing C.
      Gtkada.Bindings.Set_On_Exception (Record_Exception'Unrestricted_Access);
      Raise_Callbacks := True;
      Emit_Void_Signal (Drop.Get_Object, Name);
      Raise_Callbacks := False;
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Exception_Count), 2);
      Assert_Cmpuint_Eq (Guint (Unexpected_Exception_Count), 0);
   end Test_Activate;

   ----------------
   -- Callbacks --
   ----------------

   procedure On_Typed (Self : access Gtk_Drop_Down_Record'Class) is
   begin
      --  Save the emitted object so the typed closure can be verified.
      if Raise_Callbacks then
         raise Program_Error with "expected typed activate callback exception";
      end if;
      Typed_Count := Typed_Count + 1;
      Typed_Object := Self.Get_Object;
   end On_Typed;

   procedure On_Slot (Self : access Glib.Object.GObject_Record'Class) is
   begin
      --  A slot callback's Self is the supplied slot object, not the emitter.
      if Raise_Callbacks then
         raise Program_Error with "expected slot activate callback exception";
      end if;
      Slot_Count := Slot_Count + 1;
      Slot_Object := Self.Get_Object;
   end On_Slot;

   ---------------------
   -- Record_Exception --
   ---------------------

   procedure Record_Exception
     (Occurrence : Ada.Exceptions.Exception_Occurrence) is
   begin
      --  Count expected Program_Error exceptions separately so the test can
      --  detect an unrelated failure reaching GtkAda's callback hook.
      if Ada.Exceptions.Exception_Name (Occurrence) = "PROGRAM_ERROR" then
         Exception_Count := Exception_Count + 1;
      else
         Unexpected_Exception_Count := Unexpected_Exception_Count + 1;
      end if;
   end Record_Exception;

begin
   --  Widget creation requires GTK initialization after GLib's test setup.
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/drop-down/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/drop-down/properties", Test_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/drop-down/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/drop-down/activate", Test_Activate'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Drop_Down;
