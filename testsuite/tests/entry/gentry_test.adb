--  Unit tests for Gtk.GEntry and Gtk.Entry_Completion. The editable behavior
--  is ported from testsuite/c_tests/entry.c (test_insert, test_delete, and
--  test_editable); completion constructors are ported from
--  testsuite/c_tests/cellarea.c (test_completion_new and
--  test_completion_new_with_area). The tests run without user input.

with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                  use Glib;
with Glib.Bytes;            use Glib.Bytes;
with Glib.G_Icon;           use Glib.G_Icon;
with Glib.Menu_Model;       use Glib.Menu_Model;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Test;             use Glib.Test;
with Gdk.Content_Provider;  use Gdk.Content_Provider;
with Gdk.Drag;              use Gdk.Drag;
with Gdk.Paintable;
with Gdk.Rectangle;         use Gdk.Rectangle;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Button;            use Gtk.Button;
with Gtk.Buildable;
with Gtk.Cell_Area;         use Gtk.Cell_Area;
with Gtk.Cell_Area_Box;     use Gtk.Cell_Area_Box;
with Gtk.Cell_Editable;
with Gtk.Cell_Layout;       use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;     use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Constraint_Target;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Entry_Buffer;      use Gtk.Entry_Buffer;
with Gtk.Entry_Completion;  use Gtk.Entry_Completion;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.List_Store;        use Gtk.List_Store;
with Gtk.Main;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Pango.Attributes;      use Pango.Attributes;
with Pango.Tabs;            use Pango.Tabs;

procedure Gentry_Test is
   use type Interfaces.C.C_float;
   use type Gdk.Paintable.Gdk_Paintable;

   Insert_Calls  : Natural := 0;
   Delete_Calls  : Natural := 0;
   Changed_Calls : Natural := 0;
   Insert_Before : UTF8_String (1 .. 20) := (others => ' ');
   Delete_Before : UTF8_String (1 .. 20) := (others => ' ');
   Changed_After : UTF8_String (1 .. 20) := (others => ' ');
   Inserted_Text : UTF8_String (1 .. 20) := (others => ' ');
   Insert_Pos    : Gint := -1;
   Insert_Len    : Gint := -1;
   Delete_Start  : Gint := -1;
   Delete_End    : Gint := -1;

   Activated       : Natural := 0;
   Slot_Activated  : Natural := 0;
   Icon_Pressed    : Natural := 0;
   Icon_Released   : Natural := 0;
   Slot_Icon_Press : Natural := 0;
   Slot_Icon_Release : Natural := 0;
   Last_Icon       : Gtk_Entry_Icon_Position := Entry_Icon_Primary;

   Completion_Cursor_Calls         : Natural := 0;
   Completion_Cursor_Slot_Calls    : Natural := 0;
   Completion_Insert_Calls         : Natural := 0;
   Completion_Insert_Slot_Calls    : Natural := 0;
   Completion_Selected_Calls       : Natural := 0;
   Completion_Selected_Slot_Calls  : Natural := 0;
   Completion_No_Matches_Calls     : Natural := 0;
   Completion_No_Matches_Slot_Calls : Natural := 0;
   Completion_Match_Calls          : Natural := 0;
   Completion_Destroy_Calls        : Natural := 0;

   --  GLib's variadic emitter drives completion signal marshallers with
   --  explicit, deterministic arguments rather than synthesized GUI input.
   procedure Emit_Completion_Model_Iter_Bool
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Model    : System.Address;
      Iter     : access Gtk_Tree_Iter;
      Result   : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Completion_String_Bool
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr;
      Prefix   : Interfaces.C.Strings.chars_ptr;
      Result   : System.Address)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Completion_No_Args
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Activate
     (Instance : System.Address;
      Name     : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   function Lookup_Signal
     (Name : Interfaces.C.Strings.chars_ptr;
      Type_Id : GType) return Guint
   with Import, Convention => C,
        External_Name => "g_signal_lookup";

   function Has_Pending_Handler
     (Instance : System.Address;
      Signal_Id : Guint;
      Detail : Guint;
      May_Be_Blocked : Glib.Gboolean) return Glib.Gboolean
   with Import, Convention => C,
        External_Name => "g_signal_has_handler_pending";

   procedure On_Insert
     (Self     : Gtk_Editable;
      Text     : UTF8_String;
      Length   : Gint;
      Position : access Gint);
   procedure On_Delete
     (Self      : Gtk_Editable;
      Start_Pos : Gint;
      End_Pos   : Gint);
   procedure On_Changed_Editable (Self : Gtk_Editable);
   procedure On_Activate_Entry (Self : access Gtk_Entry_Record'Class);
   procedure On_Activate_Slot (Self : access GObject_Record'Class);
   procedure On_Icon_Press_Entry
     (Self : access Gtk_Entry_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position);
   procedure On_Icon_Release_Entry
     (Self : access Gtk_Entry_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position);
   procedure On_Icon_Press_Slot
     (Self : access GObject_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position);
   procedure On_Icon_Release_Slot
     (Self : access GObject_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position);

   --  Cursor-on-match callbacks share the model/iterator signature.
   function On_Completion_Cursor
     (Self : access Gtk_Entry_Completion_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean;
   function On_Completion_Cursor_Slot
     (Self : access GObject_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean;
   --  Insert-prefix callbacks receive the completion prefix as UTF-8 text.
   function On_Completion_Insert
     (Self : access Gtk_Entry_Completion_Record'Class;
      Prefix : UTF8_String) return Boolean;
   function On_Completion_Insert_Slot
     (Self : access GObject_Record'Class;
      Prefix : UTF8_String) return Boolean;
   --  Match-selected uses the same model/iterator signature as cursor match.
   function On_Completion_Selected
     (Self : access Gtk_Entry_Completion_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean;
   function On_Completion_Selected_Slot
     (Self : access GObject_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean;
   --  No-matches has a void signature, with typed and GObject slot forms.
   procedure On_Completion_No_Matches
     (Self : access Gtk_Entry_Completion_Record'Class);
   procedure On_Completion_No_Matches_Slot
     (Self : access GObject_Record'Class);
   --  Both generic adapters share a destroy callback so their closures can be
   --  observed when the callback is cleared.
   procedure On_Completion_Destroy (Data : in out Boolean);

   package Match_Callbacks is new Gtk.Entry_Completion.Set_Match_Func_User_Data
     (User_Data_Type => Boolean,
      Destroy        => On_Completion_Destroy);
   package Cell_Callbacks is new Gtk.Entry_Completion.Set_Cell_Data_Func_User_Data
     (User_Data_Type => Boolean,
      Destroy        => On_Completion_Destroy);

   --  The user-data callback accepts matching rows only for the current key.
   function Match_Completion
     (Completion : not null access Gtk_Entry_Completion_Record'Class;
      Key        : UTF8_String;
      Iter       : Gtk_Tree_Iter;
      User_Data  : Boolean) return Boolean;
   function Match_Completion_Plain
     (Completion : not null access Gtk_Entry_Completion_Record'Class;
      Key        : UTF8_String;
      Iter       : Gtk_Tree_Iter) return Boolean;
   --  Cell callbacks count invocations; renderers and rows are checked by GTK.
   procedure Set_Completion_Cell
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : not null access Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter;
      Data        : Boolean);
   procedure Set_Completion_Cell_Plain
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : not null access Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter);

   procedure Test_Constructors with Convention => C;
   procedure Test_Entry_Accessors with Convention => C;
   procedure Test_Icons_And_Menu with Convention => C;
   procedure Test_Editable_Port with Convention => C;
   procedure Test_Accessibility_And_Interfaces with Convention => C;
   procedure Test_Signals with Convention => C;
   procedure Test_Completion_Constructors with Convention => C;
   procedure Test_Completion_Accessors with Convention => C;
   procedure Test_Completion_Cell_Layout with Convention => C;
   procedure Test_Completion_Signals with Convention => C;

   --  Save a fixed-width snapshot so callbacks can verify before/after text.
   procedure Save_Text (Target : out UTF8_String; Value : UTF8_String) is
   begin
      Target := (others => ' ');
      Target (Target'First .. Target'First + Value'Length - 1) := Value;
   end Save_Text;

   ----------------
   -- On_Insert --
   ----------------

   procedure On_Insert
     (Self     : Gtk_Editable;
      Text     : UTF8_String;
      Length   : Gint;
      Position : access Gint)
   is
      E : constant Gtk_Entry := Gtk.GEntry."-" (Self);
   begin
      Insert_Calls := Insert_Calls + 1;
      Save_Text (Insert_Before, E.Get_Text);
      Save_Text (Inserted_Text, Text);
      Insert_Pos := Position.all;
      Insert_Len := Length;
   end On_Insert;

   ----------------
   -- On_Delete --
   ----------------

   procedure On_Delete
     (Self      : Gtk_Editable;
      Start_Pos : Gint;
      End_Pos   : Gint)
   is
      E : constant Gtk_Entry := Gtk.GEntry."-" (Self);
   begin
      Delete_Calls := Delete_Calls + 1;
      Save_Text (Delete_Before, E.Get_Text);
      Delete_Start := Start_Pos;
      Delete_End := End_Pos;
   end On_Delete;

   -------------------------
   -- On_Changed_Editable --
   -------------------------

   procedure On_Changed_Editable (Self : Gtk_Editable) is
      E : constant Gtk_Entry := Gtk.GEntry."-" (Self);
   begin
      Changed_Calls := Changed_Calls + 1;
      Save_Text (Changed_After, E.Get_Text);
   end On_Changed_Editable;

   -----------------------
   -- On_Activate_Entry --
   -----------------------

   procedure On_Activate_Entry (Self : access Gtk_Entry_Record'Class) is
   begin
      if Self /= null then
         Activated := Activated + 1;
      end if;
   end On_Activate_Entry;

   ---------------------
   -- On_Activate_Slot --
   ---------------------

   procedure On_Activate_Slot (Self : access GObject_Record'Class) is
   begin
      if Self /= null then
         Slot_Activated := Slot_Activated + 1;
      end if;
   end On_Activate_Slot;

   ------------------------
   -- On_Icon_Press_Entry --
   ------------------------

   procedure On_Icon_Press_Entry
     (Self : access Gtk_Entry_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position)
   is
   begin
      if Self /= null then
         Icon_Pressed := Icon_Pressed + 1;
         Last_Icon := Icon_Pos;
      end if;
   end On_Icon_Press_Entry;

   --------------------------
   -- On_Icon_Release_Entry --
   --------------------------

   procedure On_Icon_Release_Entry
     (Self : access Gtk_Entry_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position)
   is
   begin
      if Self /= null then
         Icon_Released := Icon_Released + 1;
         Last_Icon := Icon_Pos;
      end if;
   end On_Icon_Release_Entry;

   ----------------------
   -- On_Icon_Press_Slot --
   ----------------------

   procedure On_Icon_Press_Slot
     (Self : access GObject_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position)
   is
   begin
      if Self /= null then
         Slot_Icon_Press := Slot_Icon_Press + 1;
         Last_Icon := Icon_Pos;
      end if;
   end On_Icon_Press_Slot;

   ------------------------
   -- On_Icon_Release_Slot --
   ------------------------

   procedure On_Icon_Release_Slot
     (Self : access GObject_Record'Class;
      Icon_Pos : Gtk_Entry_Icon_Position)
   is
   begin
      if Self /= null then
         Slot_Icon_Release := Slot_Icon_Release + 1;
         Last_Icon := Icon_Pos;
      end if;
   end On_Icon_Release_Slot;

   --------------------------
   -- Completion callbacks --
   --------------------------

   function On_Completion_Cursor
     (Self : access Gtk_Entry_Completion_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Self, Model, Iter);
   begin
      Completion_Cursor_Calls := Completion_Cursor_Calls + 1;
      --  Returning False lets the later slot handler also observe the signal.
      return False;
   end On_Completion_Cursor;

   function On_Completion_Cursor_Slot
     (Self : access GObject_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Self, Model, Iter);
   begin
      Completion_Cursor_Slot_Calls := Completion_Cursor_Slot_Calls + 1;
      return True;
   end On_Completion_Cursor_Slot;

   function On_Completion_Insert
     (Self : access Gtk_Entry_Completion_Record'Class;
      Prefix : UTF8_String) return Boolean
   is
      pragma Unreferenced (Self, Prefix);
   begin
      Completion_Insert_Calls := Completion_Insert_Calls + 1;
      return False;
   end On_Completion_Insert;

   function On_Completion_Insert_Slot
     (Self : access GObject_Record'Class;
      Prefix : UTF8_String) return Boolean
   is
      pragma Unreferenced (Self, Prefix);
   begin
      Completion_Insert_Slot_Calls := Completion_Insert_Slot_Calls + 1;
      return True;
   end On_Completion_Insert_Slot;

   function On_Completion_Selected
     (Self : access Gtk_Entry_Completion_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Self, Model, Iter);
   begin
      Completion_Selected_Calls := Completion_Selected_Calls + 1;
      return False;
   end On_Completion_Selected;

   function On_Completion_Selected_Slot
     (Self : access GObject_Record'Class;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Self, Model, Iter);
   begin
      Completion_Selected_Slot_Calls := Completion_Selected_Slot_Calls + 1;
      return True;
   end On_Completion_Selected_Slot;

   procedure On_Completion_No_Matches
     (Self : access Gtk_Entry_Completion_Record'Class)
   is
   begin
      if Self /= null then
         Completion_No_Matches_Calls := Completion_No_Matches_Calls + 1;
      end if;
   end On_Completion_No_Matches;

   procedure On_Completion_No_Matches_Slot
     (Self : access GObject_Record'Class)
   is
   begin
      if Self /= null then
         Completion_No_Matches_Slot_Calls :=
           Completion_No_Matches_Slot_Calls + 1;
      end if;
   end On_Completion_No_Matches_Slot;

   procedure On_Completion_Destroy (Data : in out Boolean) is
      pragma Unreferenced (Data);
   begin
      Completion_Destroy_Calls := Completion_Destroy_Calls + 1;
   end On_Completion_Destroy;

   function Match_Completion
     (Completion : not null access Gtk_Entry_Completion_Record'Class;
      Key        : UTF8_String;
      Iter       : Gtk_Tree_Iter;
      User_Data  : Boolean) return Boolean
   is
      pragma Unreferenced (Completion, Iter);
   begin
      Completion_Match_Calls := Completion_Match_Calls + 1;
      return User_Data and then Key = "al";
   end Match_Completion;

   function Match_Completion_Plain
     (Completion : not null access Gtk_Entry_Completion_Record'Class;
      Key        : UTF8_String;
      Iter       : Gtk_Tree_Iter) return Boolean
   is
      pragma Unreferenced (Completion, Iter);
   begin
      Completion_Match_Calls := Completion_Match_Calls + 1;
      return Key = "al";
   end Match_Completion_Plain;

   procedure Set_Completion_Cell
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : not null access Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter;
      Data        : Boolean)
   is
      pragma Unreferenced (Cell_Layout, Cell, Tree_Model, Iter, Data);
   begin
      null;
   end Set_Completion_Cell;

   procedure Set_Completion_Cell_Plain
     (Cell_Layout : Gtk_Cell_Layout;
      Cell        : not null access Gtk_Cell_Renderer_Record'Class;
      Tree_Model  : Gtk_Tree_Model;
      Iter        : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Cell_Layout, Cell, Tree_Model, Iter);
   begin
      null;
   end Set_Completion_Cell_Plain;

   ----------------------
   -- Test_Constructors --
   ----------------------

   procedure Test_Constructors is
      Default_Entry : constant Gtk_Entry := Gtk_Entry_New;
      Out_Entry : Gtk_Entry;
      Buffer : constant Gtk_Entry_Buffer :=
        Gtk_Entry_Buffer_New ("from buffer", -1);
      Buffer_Entry : constant Gtk_Entry := Gtk_Entry_New_With_Buffer (Buffer);
      Out_Buffer_Entry : Gtk_Entry;
      Manual : constant Gtk_Entry := new Gtk_Entry_Record;
      Manual_Buffer : constant Gtk_Entry := new Gtk_Entry_Record;
   begin
      Ref_Sink (Default_Entry);
      Ref_Sink (Buffer_Entry);
      Gtk_New (Out_Entry);
      Gtk_New_With_Buffer (Out_Buffer_Entry, Buffer);
      Gtk.GEntry.Initialize (Manual);
      Gtk.GEntry.Initialize (Manual);
      Initialize_With_Buffer (Manual_Buffer, Buffer);
      Initialize_With_Buffer (Manual_Buffer, Buffer);
      Ref_Sink (Out_Entry);
      Ref_Sink (Out_Buffer_Entry);
      Ref_Sink (Manual);
      Ref_Sink (Manual_Buffer);

      --  All constructor paths produce a usable widget; the second initialize
      --  call checks that GtkAda's documented idempotence guard is taken.
      Assert_True (Gtk.GEntry.Get_Type /= 0);
      Assert_True (Default_Entry.Get_Buffer /= null);
      Assert_True (Out_Entry.Get_Buffer /= null);
      Assert_True (Buffer_Entry.Get_Buffer = Buffer);
      Assert_True (Out_Buffer_Entry.Get_Buffer = Buffer);
      Assert_True (Manual.Get_Buffer /= null);
      Assert_True (Manual_Buffer.Get_Buffer = Buffer);
      Assert_Cmpstr_Eq (Buffer_Entry.Get_Text, "from buffer");

      Unref (Default_Entry);
      Unref (Out_Entry);
      Unref (Buffer_Entry);
      Unref (Out_Buffer_Entry);
      Unref (Manual);
      Unref (Manual_Buffer);
      Unref (Buffer);
   end Test_Constructors;

   --------------------------
   -- Test_Entry_Accessors --
   --------------------------

   procedure Test_Entry_Accessors is
      E : constant Gtk_Entry := Gtk_Entry_New;
      Attrs : constant Pango_Attr_List := Pango_Attr_List_New;
      Tabs : constant Pango_Tab_Array := Pango_Tab_Array_New (2, False);
      Buffer : constant Gtk_Entry_Buffer :=
        Gtk_Entry_Buffer_New ("replacement", -1);
      Start_Pos, End_Pos : Gint;
      Has_Selection : Boolean;
      Insert_Pos_Local : Gint := 0;
      Fraction : Gdouble;
   begin
      Ref_Sink (E);

      --  These paired setters and getters verify the documented defaults and
      --  round trips on properties that do not require a realized widget.
      Assert_False (E.Get_Activates_Default);
      E.Set_Activates_Default (True);
      Assert_True (E.Get_Activates_Default);
      E.Set_Alignment (0.25);
      Assert_True
        (abs (E.Get_Alignment - Interfaces.C.C_float'(0.25)) < 0.001);
      E.Set_Attributes (Attrs);
      Assert_True (E.Get_Attributes /= Null_Pango_Attr_List);
      E.Set_Buffer (Buffer);
      Assert_True (E.Get_Buffer = Buffer);
      E.Set_Has_Frame (False);
      Assert_False (E.Get_Has_Frame);
      E.Set_Has_Frame (True);
      Assert_True (E.Get_Has_Frame);

      E.Set_Input_Hints (Input_Hint_No_Spellcheck);
      Assert_True (E.Get_Input_Hints = Input_Hint_No_Spellcheck);
      E.Set_Input_Purpose (Input_Purpose_Alpha);
      Assert_True (E.Get_Input_Purpose = Input_Purpose_Alpha);
      E.Set_Invisible_Char (Character'Pos ('#'));
      Assert_Cmpuint_Eq (Guint (E.Get_Invisible_Char), Character'Pos ('#'));
      E.Unset_Invisible_Char;
      Assert_True (E.Get_Invisible_Char /= Character'Pos ('#'));
      E.Set_Visibility (False);
      Assert_False (E.Get_Visibility);
      E.Set_Visibility (True);
      Assert_True (E.Get_Visibility);

      E.Set_Max_Length (4);
      E.Set_Text ("abcdef");
      Assert_Cmpint_Eq (E.Get_Max_Length, 4);
      Assert_Cmpstr_Eq (E.Get_Text, "abcd");
      Assert_Cmpuint_Eq (Guint (E.Get_Text_Length), 4);
      E.Set_Max_Length (0);
      Assert_Cmpint_Eq (E.Get_Max_Length, 0);

      E.Set_Menu_Entry_Icon_Text (Entry_Icon_Primary, "Search");
      E.Set_Menu_Entry_Icon_Text (Entry_Icon_Secondary, "Clear");
      Assert_Cmpstr_Eq
        (E.Get_Menu_Entry_Icon_Text (Entry_Icon_Primary), "Search");
      Assert_Cmpstr_Eq
        (E.Get_Menu_Entry_Icon_Text (Entry_Icon_Secondary), "Clear");
      E.Set_Overwrite_Mode (True);
      Assert_True (E.Get_Overwrite_Mode);
      E.Set_Overwrite_Mode (False);
      Assert_False (E.Get_Overwrite_Mode);
      E.Set_Placeholder_Text ("type here");
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "type here");
      E.Set_Placeholder_Text ("");
      Assert_Cmpstr_Eq (E.Get_Placeholder_Text, "");

      E.Set_Progress_Fraction (0.4);
      Assert_True (abs (E.Get_Progress_Fraction - 0.4) < 0.001);
      E.Set_Progress_Pulse_Step (0.2);
      Assert_True (abs (E.Get_Progress_Pulse_Step - 0.2) < 0.001);
      E.Progress_Pulse;
      Fraction := E.Get_Progress_Fraction;
      Assert_True (Fraction >= 0.0 and Fraction <= 1.0);
      E.Reset_Im_Context;

      E.Set_Tabs (Tabs);
      Assert_Cmpint_Eq (Get_Size (E.Get_Tabs), 2);
      Assert_True (E.Get_Extra_Menu = null);
      Assert_True (E.Get_Completion = null);
      E.Set_Completion (null);
      Assert_True (E.Get_Completion = null);

      --  Editable methods exposed directly by Gtk.GEntry mirror GtkEditable.
      E.Set_Text ("bar");
      Assert_Cmpstr_Eq (E.Get_Chars (0, 2), "ba");
      Assert_Cmpstr_Eq (E.Get_Chars (2), "r");
      E.Set_Position (-1);
      Assert_Cmpint_Eq (E.Get_Position, 3);
      E.Select_Region (1, 2);
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start_Pos, 1);
      Assert_Cmpint_Eq (End_Pos, 2);
      E.Insert_Text ("X", -1, Insert_Pos_Local);
      Assert_Cmpstr_Eq (E.Get_Text, "Xbar");
      E.Delete_Selection;
      Assert_Cmpstr_Eq (E.Get_Text, "Xbr");
      E.Delete_Text (0, 1);
      Assert_Cmpstr_Eq (E.Get_Text, "br");

      Assert_True (E.Get_Editable);
      E.Set_Editable (False);
      Assert_False (E.Get_Editable);
      E.Set_Editable (True);
      Assert_True (E.Get_Editable);
      Assert_True (E.Get_Enable_Undo);
      E.Set_Enable_Undo (False);
      Assert_False (E.Get_Enable_Undo);
      E.Set_Enable_Undo (True);
      Assert_True (E.Get_Enable_Undo);
      E.Set_Max_Width_Chars (13);
      Assert_Cmpint_Eq (E.Get_Max_Width_Chars, 13);
      E.Set_Width_Chars (9);
      Assert_Cmpint_Eq (E.Get_Width_Chars, 9);

      Unref (E);
      Unref (Buffer);
      Free (Tabs);
   end Test_Entry_Accessors;

   --------------------------
   -- Test_Icons_And_Menu --
   --------------------------

   procedure Test_Icons_And_Menu is
      E : constant Gtk_Entry := Gtk_Entry_New;
      Data : constant Gbytes := Gbytes_New ((65, 66), 2);
      Provider : constant Gdk_Content_Provider :=
        Gdk_Content_Provider_New_For_Bytes ("text/plain", Data);
      Area : Gdk_Rectangle;
   begin
      Ref_Sink (E);

      --  Icon-name accessors cover both a stored icon and clearing it with
      --  the empty string, which GTK represents as an empty storage type.
      E.Set_Icon_From_Icon_Name (Entry_Icon_Primary, "document-open");
      Assert_Cmpstr_Eq
        (E.Get_Icon_Name (Entry_Icon_Primary), "document-open");
      Assert_True (E.Get_Icon_Storage_Type (Entry_Icon_Primary) = Image_Icon_Name);
      E.Set_Icon_From_Icon_Name (Entry_Icon_Primary, "");
      Assert_Cmpstr_Eq (E.Get_Icon_Name (Entry_Icon_Primary), "");
      Assert_True (E.Get_Icon_Storage_Type (Entry_Icon_Primary) = Image_Empty);

      --  Null GIcon and paintable values clear either icon without needing
      --  an installed theme asset or a display-backed paintable.
      E.Set_Icon_From_Gicon
        (Entry_Icon_Secondary, Glib.G_Icon.G_Icon'(null));
      Assert_True
        (E.Get_Icon_Gicon (Entry_Icon_Secondary)
         = Glib.G_Icon.G_Icon'(null));
      E.Set_Icon_From_Paintable
        (Entry_Icon_Secondary, Gdk.Paintable.Null_Gdk_Paintable);
      Assert_True
        (E.Get_Icon_Paintable (Entry_Icon_Secondary)
         = Gdk.Paintable.Null_Gdk_Paintable);

      E.Set_Icon_Activatable (Entry_Icon_Primary, True);
      Assert_True (E.Get_Icon_Activatable (Entry_Icon_Primary));
      E.Set_Icon_Sensitive (Entry_Icon_Primary, False);
      Assert_False (E.Get_Icon_Sensitive (Entry_Icon_Primary));
      E.Set_Icon_Sensitive (Entry_Icon_Primary, True);
      Assert_True (E.Get_Icon_Sensitive (Entry_Icon_Primary));
      E.Set_Icon_Tooltip_Text (Entry_Icon_Primary, "Open a file");
      Assert_Cmpstr_Eq
        (E.Get_Icon_Tooltip_Text (Entry_Icon_Primary), "Open a file");
      E.Set_Icon_Tooltip_Markup (Entry_Icon_Secondary, "<b>Clear</b>");
      Assert_Cmpstr_Eq
        (E.Get_Icon_Tooltip_Markup (Entry_Icon_Secondary), "<b>Clear</b>");

      E.Set_Icon_Drag_Source (Entry_Icon_Primary, Provider, Gdk_Action_Copy);
      Assert_Cmpint_Eq (E.Get_Current_Icon_Drag_Source, -1);
      Assert_Cmpint_Eq (E.Get_Icon_At_Pos (0, 0), -1);
      E.Get_Icon_Area (Entry_Icon_Primary, Area);
      Assert_Cmpint_Eq (Area.Width, 0);
      Assert_Cmpint_Eq (Area.Height, 0);
      Assert_Cmpint_Eq (Area.X, 0);
      Assert_Cmpint_Eq (Area.Y, 0);

      Unref (Provider);
      Unref (Data);
      Unref (E);
   end Test_Icons_And_Menu;

   -----------------------
   -- Test_Editable_Port --
   -----------------------

   procedure Test_Editable_Port is
      E : constant Gtk_Entry := Gtk_Entry_New;
      Pos : Gint := 0;
      Start_Pos, End_Pos : Gint;
      Has_Selection : Boolean;
   begin
      Ref_Sink (E);
      E.Set_Text ("bar");
      E.Set_Position (-1);
      Insert_Calls := 0;
      Delete_Calls := 0;
      Changed_Calls := 0;
      Gtk.Editable.On_Insert_Text (+E, On_Insert'Unrestricted_Access);
      Gtk.Editable.On_Delete_Text (+E, On_Delete'Unrestricted_Access);
      Gtk.Editable.On_Changed (+E, On_Changed_Editable'Unrestricted_Access);

      --  Port of C test_insert: the insert signal observes the old text and
      --  insertion position; the changed signal observes the completed edit.
      Assert_Cmpint_Eq (E.Get_Position, 3);
      Pos := 0;
      E.Insert_Text ("foo", -1, Pos);
      Assert_Cmpint_Eq (Pos, 3);
      Assert_Cmpstr_Eq (E.Get_Text, "foobar");
      Assert_Cmpuint_Eq (Guint (Insert_Calls), 1);
      Assert_Cmpstr_Eq (Insert_Before (1 .. 3), "bar");
      Assert_Cmpint_Eq (Insert_Pos, 0);
      Assert_Cmpint_Eq (Insert_Len, 3);
      Assert_Cmpstr_Eq (Inserted_Text (1 .. 3), "foo");
      Assert_Cmpuint_Eq (Guint (Changed_Calls), 1);
      Assert_Cmpstr_Eq (Changed_After (1 .. 6), "foobar");

      --  Port of C test_delete and test_editable: deletion signals report the
      --  original range, selection bounds track the selected characters, and
      --  GtkEditable's text slicing uses character positions.
      E.Set_Text ("foobar");
      E.Set_Position (-1);
      Delete_Calls := 0;
      Changed_Calls := 0;
      E.Delete_Text (0, 3);
      Assert_Cmpstr_Eq (E.Get_Text, "bar");
      Assert_Cmpint_Eq (E.Get_Position, 3);
      Assert_Cmpuint_Eq (Guint (Delete_Calls), 1);
      Assert_Cmpstr_Eq (Delete_Before (1 .. 6), "foobar");
      Assert_Cmpint_Eq (Delete_Start, 0);
      Assert_Cmpint_Eq (Delete_End, 3);
      Assert_Cmpstr_Eq (Changed_After (1 .. 3), "bar");

      E.Set_Text ("ABC");
      E.Select_Region (1, 2);
      E.Get_Selection_Bounds (Start_Pos, End_Pos, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start_Pos, 1);
      Assert_Cmpint_Eq (End_Pos, 2);
      Assert_Cmpstr_Eq (E.Get_Chars (Start_Pos, End_Pos), "B");

      Unref (E);
   end Test_Editable_Port;

   -----------------------------------------
   -- Test_Accessibility_And_Interfaces --
   -----------------------------------------

   procedure Test_Accessibility_And_Interfaces is
      E : constant Gtk_Entry := Gtk_Entry_New;
      Accessible : Gtk_Accessible;
      Buildable : Gtk.Buildable.Gtk_Buildable;
      Cell : Gtk.Cell_Editable.Gtk_Cell_Editable;
      Target : Gtk.Constraint_Target.Gtk_Constraint_Target;
      Editable : Gtk_Editable;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible_Id : constant UTF8_String := E.Get_Accessible_Id;
   begin
      Ref_Sink (E);

      --  Round trips through each implemented interface validate the generic
      --  Ada interface conversions generated for Gtk.Entry.
      Accessible := Gtk.GEntry."+" (E);
      Buildable := Gtk.GEntry."+" (E);
      Cell := Gtk.GEntry."+" (E);
      Target := Gtk.GEntry."+" (E);
      Editable := Gtk.GEntry."+" (E);
      Assert_True (Gtk.GEntry."-" (Accessible) = E);
      Assert_True (Gtk.GEntry."-" (Buildable) = E);
      Assert_True (Gtk.GEntry."-" (Cell) = E);
      Assert_True (Gtk.GEntry."-" (Target) = E);
      Assert_True (Gtk.GEntry."-" (Editable) = E);

      --  GtkEntry has the text-box role and exposes an internal accessible
      --  child, while an unrooted entry has no parent or next sibling.
      Assert_True (E.Get_Accessible_Role = Accessible_Role_Text_Box);
      Assert_True (E.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (E.Get_First_Accessible_Child /= Null_Gtk_Accessible);
      Assert_True (E.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Has_Bounds := E.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_True (E.Get_At_Context = null);
      Assert_True (Accessible_Id'Length < 256);
      Assert_True
        (E.Delegate_Get_Accessible_Platform_State
           (Accessible_Platform_State_Focusable)
         = E.Get_Platform_State (Accessible_Platform_State_Focusable));

      --  The delegate query should agree with the entry's own platform-state
      --  query for this state; this also covers the GtkEditable helper wrapper.
      Assert_True
        (Gtk.Editable.Delegate_Get_Accessible_Platform_State
           (Editable, Accessible_Platform_State_Focusable)
         = E.Get_Platform_State (Accessible_Platform_State_Focusable));

      E.Announce ("Entry ready", Accessible_Announcement_Priority_Low);
      E.Reset_Property (Accessible_Property_Label);
      E.Reset_Relation (Accessible_Relation_Labelled_By);
      E.Reset_State (Accessible_State_Busy);
      E.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (E.Get_Accessible_Parent = Null_Gtk_Accessible);
      E.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      E.Update_Platform_State (Accessible_Platform_State_Focused);

      --  GtkEntry already owns its editing delegate, so initialize/finish are
      --  implementation lifecycle hooks and are not called on this widget.
      Assert_True (E.Get_Delegate /= Gtk.Editable.Null_Gtk_Editable);

      Unref (E);
   end Test_Accessibility_And_Interfaces;

   -------------------
   -- Test_Signals --
   -------------------

   procedure Test_Signals is
      E : constant Gtk_Entry := Gtk_Entry_New;
      Slot : constant Gtk_Button := Gtk_Button_New_With_Label ("slot");
      Name : Interfaces.C.Strings.chars_ptr;
      Icon_Press_Id, Icon_Release_Id : Guint;
   begin
      Ref_Sink (E);
      Ref_Sink (Slot);
      Activated := 0;
      Slot_Activated := 0;
      Icon_Pressed := 0;
      Icon_Released := 0;
      Slot_Icon_Press := 0;
      Slot_Icon_Release := 0;

      --  Emitting activate checks its direct and slot callbacks. The icon
      --  handlers are verified as pending below without synthesizing a mouse
      --  gesture, which requires a mapped entry.
      E.On_Activate (On_Activate_Entry'Unrestricted_Access);
      E.On_Activate (On_Activate_Slot'Unrestricted_Access, Slot);
      E.On_Icon_Press (On_Icon_Press_Entry'Unrestricted_Access);
      E.On_Icon_Press (On_Icon_Press_Slot'Unrestricted_Access, Slot);
      E.On_Icon_Release (On_Icon_Release_Entry'Unrestricted_Access);
      E.On_Icon_Release (On_Icon_Release_Slot'Unrestricted_Access, Slot);

      E.Set_Icon_From_Icon_Name (Entry_Icon_Primary, "document-open");
      E.Set_Icon_From_Icon_Name (Entry_Icon_Secondary, "edit-clear");
      E.Set_Icon_Activatable (Entry_Icon_Primary, True);
      E.Set_Icon_Activatable (Entry_Icon_Secondary, True);

      Name := Interfaces.C.Strings.New_String ("activate");
      Emit_Activate (Get_Object (E), Name);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpuint_Eq (Guint (Activated), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Activated), 1);

      Name := Interfaces.C.Strings.New_String ("icon-press");
      Icon_Press_Id := Lookup_Signal (Name, Gtk.GEntry.Get_Type);
      Interfaces.C.Strings.Free (Name);
      Name := Interfaces.C.Strings.New_String ("icon-release");
      Icon_Release_Id := Lookup_Signal (Name, Gtk.GEntry.Get_Type);
      Interfaces.C.Strings.Free (Name);
      Assert_True (Icon_Press_Id /= 0);
      Assert_True (Icon_Release_Id /= 0);
      Assert_True
        (Has_Pending_Handler (Get_Object (E), Icon_Press_Id, 0, 1) /= 0);
      Assert_True
        (Has_Pending_Handler (Get_Object (E), Icon_Release_Id, 0, 1) /= 0);

      Unref (Slot);
      Unref (E);
   end Test_Signals;

   -----------------------------------
   -- Test_Completion_Constructors --
   -----------------------------------

   --  Port of cellarea.c's default-area and caller-supplied-area completion
   --  constructor checks, including both GtkAda constructor styles.
   --  The no-area constructors create a usable default area for completion.
   procedure Test_Completion_Constructors is
      Default_Completion : constant Gtk_Entry_Completion :=
        Gtk_Entry_Completion_New;
      Area : constant Gtk_Cell_Area_Box := Gtk_Cell_Area_Box_New;
      Area_Completion : constant Gtk_Entry_Completion :=
        Gtk_Entry_Completion_New_With_Area (Area);
      Out_Completion : Gtk_Entry_Completion;
      Out_Area_Completion : Gtk_Entry_Completion;
      Manual : constant Gtk_Entry_Completion :=
        new Gtk_Entry_Completion_Record;
      Manual_With_Area : constant Gtk_Entry_Completion :=
        new Gtk_Entry_Completion_Record;
   begin
      Gtk_New (Out_Completion);
      Gtk_New_With_Area (Out_Area_Completion, Area);
      Gtk.Entry_Completion.Initialize (Manual);
      Gtk.Entry_Completion.Initialize (Manual);
      Gtk.Entry_Completion.Initialize_With_Area (Manual_With_Area, Area);
      Gtk.Entry_Completion.Initialize_With_Area (Manual_With_Area, Area);

      --  The default constructor supplies an area, while each area overload
      --  preserves the exact GtkCellArea object supplied by the caller.
      Assert_True (Gtk.Entry_Completion.Get_Type /= 0);
      Assert_True (Get_Area (+Default_Completion) /= null);
      Assert_True (Get_Area (+Area_Completion) = Gtk_Cell_Area (Area));
      Assert_True
        (Get_Area (+Out_Area_Completion) = Gtk_Cell_Area (Area));
      Assert_True
        (Get_Area (+Manual_With_Area) = Gtk_Cell_Area (Area));

      --  Sink each object before unref because GtkAda constructors return an
      --  owned GObject reference that this test releases explicitly.
      Ref_Sink (Default_Completion);
      Ref_Sink (Area_Completion);
      Ref_Sink (Out_Completion);
      Ref_Sink (Out_Area_Completion);
      Ref_Sink (Manual);
      Ref_Sink (Manual_With_Area);
      Unref (Default_Completion);
      Unref (Area_Completion);
      Unref (Out_Completion);
      Unref (Out_Area_Completion);
      Unref (Manual);
      Unref (Manual_With_Area);
      Unref (Area);
   end Test_Completion_Constructors;

   ---------------------------------
   -- Test_Completion_Accessors --
   ---------------------------------

   --  Verify property-like methods, text-column prefix computation, and the
   --  Ada callback adapters against a deterministic one-column string model.
   procedure Test_Completion_Accessors is
      Completion : constant Gtk_Entry_Completion :=
        Gtk_Entry_Completion_New;
      Entry_Widget : constant Gtk_Entry := Gtk_Entry_New;
      Store : Gtk_List_Store;
      Iter : Gtk_Tree_Iter;
      Match_Calls_Before : constant Natural := Completion_Match_Calls;
   begin
      Ref_Sink (Completion);
      Ref_Sink (Entry_Widget);
      Gtk_New (Store, (0 => GType_String));
      Store.Append (Iter);
      Store.Set (Iter, 0, "alpha");
      Store.Append (Iter);
      Store.Set (Iter, 0, "alpine");
      Store.Append (Iter);
      Store.Set (Iter, 0, "beta");

      --  Attaching through Gtk.Entry exercises the completion's entry getter
      --  and verifies the object relationship returned by GtkAda.
      Entry_Widget.Set_Completion (Completion);
      Assert_True
        (Gtk.GEntry.Gtk_Entry (Completion.Get_Entry) = Entry_Widget);
      Assert_Cmpint_Eq (Completion.Get_Minimum_Key_Length, 1);
      Completion.Set_Minimum_Key_Length (2);
      Assert_Cmpint_Eq (Completion.Get_Minimum_Key_Length, 2);

      Completion.Set_Model (+Store);
      Assert_True (Completion.Get_Model = +Store);
      Completion.Set_Text_Column (0);
      Assert_Cmpint_Eq (Completion.Get_Text_Column, 0);
      Entry_Widget.Set_Text ("al");
      Completion.Complete;
      Assert_Cmpstr_Eq (Completion.Compute_Prefix ("al"), "alp");
      Assert_Cmpstr_Eq (Completion.Compute_Prefix ("zz"), "");
      Assert_Cmpstr_Eq (Completion.Get_Completion_Prefix, "");
      Completion.Insert_Prefix;

      --  Each boolean setter is checked immediately through its getter.
      Completion.Set_Inline_Completion (True);
      Assert_True (Completion.Get_Inline_Completion);
      Completion.Set_Inline_Selection (True);
      Assert_True (Completion.Get_Inline_Selection);
      Completion.Set_Popup_Completion (False);
      Assert_False (Completion.Get_Popup_Completion);
      Completion.Set_Popup_Completion (True);
      Assert_True (Completion.Get_Popup_Completion);
      Completion.Set_Popup_Set_Width (False);
      Assert_False (Completion.Get_Popup_Set_Width);
      Completion.Set_Popup_Set_Width (True);
      Assert_True (Completion.Get_Popup_Set_Width);
      Completion.Set_Popup_Single_Match (False);
      Assert_False (Completion.Get_Popup_Single_Match);
      Completion.Set_Popup_Single_Match (True);
      Assert_True (Completion.Get_Popup_Single_Match);

      --  The custom match callback sees GTK's normalized key. Complete runs
      --  the filter without requiring a mapped popup or simulated input.
      Entry_Widget.Set_Text ("al");
      Completion.Set_Match_Func (Match_Completion_Plain'Unrestricted_Access);
      Completion.Complete;
      Assert_True (Completion_Match_Calls > Match_Calls_Before);
      Completion.Set_Match_Func (null);
      Match_Callbacks.Set_Match_Func
        (Completion, Match_Completion'Unrestricted_Access, True);
      Completion.Complete;
      Assert_True (Completion_Match_Calls > Match_Calls_Before);
      Match_Callbacks.Set_Match_Func (Completion, null, False);
      Assert_True (Completion_Destroy_Calls > 0);

      --  Setting a null model is the documented way to remove the data source.
      --  Null_Gtk_Tree_Model is the Ada null value for this interface type.
      Completion.Set_Model (Gtk.Tree_Model.Null_Gtk_Tree_Model);
      Assert_True
        (Completion.Get_Model = Gtk.Tree_Model.Null_Gtk_Tree_Model);

      Unref (Store);
      Unref (Entry_Widget);
      Unref (Completion);
   end Test_Completion_Accessors;

   -----------------------------------
   -- Test_Completion_Cell_Layout --
   -----------------------------------

   --  Exercise the CellLayout methods inherited by GtkEntryCompletion and
   --  both callback registration forms, checking the renderer list as it
   --  changes. The data functions are installed and then explicitly cleared.
   procedure Test_Completion_Cell_Layout is
      Completion : constant Gtk_Entry_Completion :=
        Gtk_Entry_Completion_New;
      Store : Gtk_List_Store;
      Iter : Gtk_Tree_Iter;
      First_Cell : constant Gtk_Cell_Renderer_Text :=
        Gtk_Cell_Renderer_Text_New;
      Last_Cell : constant Gtk_Cell_Renderer_Text :=
        Gtk_Cell_Renderer_Text_New;
   begin
      Ref_Sink (Completion);
      Gtk_New (Store, (0 => GType_String));
      Store.Append (Iter);
      Store.Set (Iter, 0, "alpha");

      --  The cell-layout calls verify packing, attribute changes, ordering,
      --  enumeration, and clearing through GtkEntryCompletion's interface.
      Completion.Pack_Start (First_Cell, False);
      Completion.Pack_End (Last_Cell, True);
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Completion.Get_Cells),
         2);
      Completion.Add_Attribute (First_Cell, "text", 0);
      Completion.Clear_Attributes (First_Cell);
      Completion.Add_Attribute (First_Cell, "text", 0);
      Completion.Reorder (First_Cell, 1);

      --  Install and remove both callback forms without opening the deprecated
      --  popup; removal is observable through the generic destroy callback.
      Completion.Set_Cell_Data_Func
        (First_Cell, Set_Completion_Cell_Plain'Unrestricted_Access);
      Completion.Set_Cell_Data_Func (First_Cell, null);
      Cell_Callbacks.Set_Cell_Data_Func
        (Completion, First_Cell, Set_Completion_Cell'Unrestricted_Access,
         True);
      Cell_Callbacks.Set_Cell_Data_Func
        (Completion, First_Cell, null, False);
      Assert_True (Completion_Destroy_Calls > 0);

      Completion.Clear;
      Assert_Cmpuint_Eq
        (Gtk.Cell_Renderer.Cell_Renderer_List.Length
           (Completion.Get_Cells),
         0);
      Unref (Store);
      Unref (Completion);
   end Test_Completion_Cell_Layout;

   --------------------------------
   -- Test_Completion_Signals --
   --------------------------------

   --  Emit the four public completion signals directly to verify both the
   --  widget-specific and GObject slot marshallers and their return values.
   procedure Test_Completion_Signals is
      Completion : constant Gtk_Entry_Completion :=
        Gtk_Entry_Completion_New;
      Slot : constant Gtk_Button := Gtk_Button_New_With_Label ("slot");
      Store : Gtk_List_Store;
      Iter : aliased Gtk_Tree_Iter;
      Result : aliased Gint := 0;
      Name : Interfaces.C.Strings.chars_ptr;
      Prefix : Interfaces.C.Strings.chars_ptr;
   begin
      Ref_Sink (Completion);
      Ref_Sink (Slot);
      Gtk_New (Store, (0 => GType_String));
      Store.Append (Iter);
      Store.Set (Iter, 0, "alpha");

      --  Reset global counters so every assertion belongs to this emission.
      Completion_Cursor_Calls := 0;
      Completion_Cursor_Slot_Calls := 0;
      Completion_Insert_Calls := 0;
      Completion_Insert_Slot_Calls := 0;
      Completion_Selected_Calls := 0;
      Completion_Selected_Slot_Calls := 0;
      Completion_No_Matches_Calls := 0;
      Completion_No_Matches_Slot_Calls := 0;
      Completion.On_Cursor_On_Match
        (On_Completion_Cursor'Unrestricted_Access);
      Completion.On_Cursor_On_Match
        (On_Completion_Cursor_Slot'Unrestricted_Access, Slot);
      Completion.On_Insert_Prefix
        (On_Completion_Insert'Unrestricted_Access);
      Completion.On_Insert_Prefix
        (On_Completion_Insert_Slot'Unrestricted_Access, Slot);
      Completion.On_Match_Selected
        (On_Completion_Selected'Unrestricted_Access);
      Completion.On_Match_Selected
        (On_Completion_Selected_Slot'Unrestricted_Access, Slot);
      Completion.On_No_Matches
        (On_Completion_No_Matches'Unrestricted_Access);
      Completion.On_No_Matches
        (On_Completion_No_Matches_Slot'Unrestricted_Access, Slot);

      --  The result pointer is the final vararg, as required by GLib's API.
      Name := Interfaces.C.Strings.New_String ("cursor-on-match");
      Emit_Completion_Model_Iter_Bool
        (Get_Object (Completion), Name, Get_Object (Store), Iter'Access,
         Result'Address);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpint_Eq (Result, 1);
      Assert_Cmpuint_Eq (Guint (Completion_Cursor_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Completion_Cursor_Slot_Calls), 1);

      --  A second model/iterator signal exercises the alternate completion
      --  handler pair while preserving the boolean return value.
      Result := 0;
      Name := Interfaces.C.Strings.New_String ("match-selected");
      Emit_Completion_Model_Iter_Bool
        (Get_Object (Completion), Name, Get_Object (Store), Iter'Access,
         Result'Address);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpint_Eq (Result, 1);
      Assert_Cmpuint_Eq (Guint (Completion_Selected_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Completion_Selected_Slot_Calls), 1);

      --  Prefix signals use a UTF-8 argument and the corresponding marshaller.
      Result := 0;
      Name := Interfaces.C.Strings.New_String ("insert-prefix");
      Prefix := Interfaces.C.Strings.New_String ("alpha");
      Emit_Completion_String_Bool
        (Get_Object (Completion), Name, Prefix, Result'Address);
      Interfaces.C.Strings.Free (Prefix);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpint_Eq (Result, 1);
      Assert_Cmpuint_Eq (Guint (Completion_Insert_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Completion_Insert_Slot_Calls), 1);

      --  The no-matches signal has no parameters and no return value.
      Name := Interfaces.C.Strings.New_String ("no-matches");
      Emit_Completion_No_Args (Get_Object (Completion), Name);
      Interfaces.C.Strings.Free (Name);
      Assert_Cmpuint_Eq (Guint (Completion_No_Matches_Calls), 1);
      Assert_Cmpuint_Eq (Guint (Completion_No_Matches_Slot_Calls), 1);

      Unref (Store);
      Unref (Slot);
      Unref (Completion);
   end Test_Completion_Signals;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func ("/entry/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func ("/entry/accessors", Test_Entry_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func ("/entry/icons-menu", Test_Icons_And_Menu'Unrestricted_Access);
   Glib.Test.Add_Func ("/entry/editable-port", Test_Editable_Port'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/entry/accessibility-interfaces",
      Test_Accessibility_And_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func ("/entry/signals", Test_Signals'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/entry/completion-constructors",
      Test_Completion_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/entry/completion-accessors",
      Test_Completion_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/entry/completion-cell-layout",
      Test_Completion_Cell_Layout'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/entry/completion-signals",
      Test_Completion_Signals'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Gentry_Test;
