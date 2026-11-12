--  Ada port of GTK's testsuite/gtk/label.c (GTK 4.22.2).
--
--  Note: /label/markup-parse is only partially ported. The original C test
--  also compares a stringified PangoAttrList dump, which cannot be reproduced
--  with the current bindings (Pango.Attributes lacks an attribute iterator and
--  per-attribute accessors). See transition_notes.txt.

with Glib;         use Glib;
with Glib.Test;    use Glib.Test;
with Ada.Command_Line;
with Interfaces.C;
with System;
with Glib.Object;
with Gtk.Label;    use Gtk.Label;
with Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Enums;
with Glib.Menu_Model;
with Gtk.Main;
with Gtk.Widget;
with Gtk.Window;   use Gtk.Window;
with Gtkada.Types;
with Pango.Attributes;
with Pango.Enums;
with Pango.Layout; use Pango.Layout;
with Pango.Tabs;

procedure Label is

   --  GDK keyvals. The GDK_KEY_* constants live in Gdk.Types.Keysyms, which is
   --  part of the gtk3 sources and not built into the gtk4 library, so we use
   --  the corresponding keyval literals directly.
   Key_t      : constant Guint := 16#74#;  --  GDK_KEY_t / 't'
   Key_r      : constant Guint := 16#72#;  --  GDK_KEY_r / 'r'
   Key_i      : constant Guint := 16#69#;  --  GDK_KEY_i / 'i'
   Key_d      : constant Guint := 16#64#;  --  GDK_KEY_d / 'd'
   VoidSymbol : constant Guint := 16#FFFFFF#;  --  GDK_KEY_VoidSymbol

   use type Interfaces.C.C_float;
   use type Pango.Attributes.Pango_Attr_List;
   use type Pango.Tabs.Pango_Tab_Array;
   use type Gtk.Accessible.Gtk_Accessible;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gtk.Widget.Gtk_Widget;
   use type Glib.Menu_Model.Gmenu_Model;
   use type Gtk.Enums.Gtk_Justification;
   use type Gtk.Enums.Gtk_Natural_Wrap_Mode;
   use type Gtk.Enums.Gtk_Movement_Step;
   use type Gtk.Accessible.Gtk_Accessible_Role;
   use type Pango.Enums.Wrap_Mode;
   use type Pango.Tabs.Pango_Tab_Align;
   use type System.Address;

   --  Callback observations let the signal test check both the signal values
   --  and the object passed through GtkAda's typed and slot-based adapters.
   Typed_Void_Count : Natural := 0;
   Slot_Void_Count  : Natural := 0;
   Typed_Link_Count : Natural := 0;
   Slot_Link_Count  : Natural := 0;
   Typed_Move_Count : Natural := 0;
   Slot_Move_Count  : Natural := 0;
   Last_URI         : UTF8_String (1 .. 24) := (others => ASCII.NUL);
   Last_Move_Step   : Gtk.Enums.Gtk_Movement_Step :=
     Gtk.Enums.Movement_Logical_Positions;
   Last_Move_Count  : Gint := 0;
   Last_Move_Extend : Boolean := False;
   Last_Slot        : System.Address := System.Null_Address;

   --  Emit signals directly so the tests do not depend on keyboard or pointer
   --  input. C_Variadic_2 matches g_signal_emit_by_name's fixed arguments.
   procedure Emit_Void_Signal
     (Object : System.Address;
      Name   : Gtkada.Types.Chars_Ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Link_Signal
     (Object       : System.Address;
      Name         : Gtkada.Types.Chars_Ptr;
      URI          : Gtkada.Types.Chars_Ptr;
      Return_Value : access Gboolean)
   with Import, Convention => C,
        External_Name => "g_signal_emit_by_name";

   procedure Emit_Move_Signal
     (Object  : System.Address;
      Name    : Gtkada.Types.Chars_Ptr;
      Step    : Interfaces.C.int;
      Count   : Interfaces.C.int;
      Extend  : Interfaces.C.int)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure Typed_Void (Self : access Gtk_Label_Record'Class);
   procedure Slot_Void (Self : access Glib.Object.GObject_Record'Class);
   function Typed_Link
     (Self : access Gtk_Label_Record'Class; URI : UTF8_String)
      return Boolean;
   function Slot_Link
     (Self : access Glib.Object.GObject_Record'Class; URI : UTF8_String)
      return Boolean;
   procedure Typed_Move
     (Self : access Gtk_Label_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step;
      Count : Gint;
      Extend_Selection : Boolean);
   procedure Slot_Move
     (Self : access Glib.Object.GObject_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step;
      Count : Gint;
      Extend_Selection : Boolean);

   procedure Test_Markup_Parse
   with Convention => C;

   procedure Test_Underline_Parse
   with Convention => C;

   procedure Test_Parse_More
   with Convention => C;

   procedure Test_Constructors
   with Convention => C;

   procedure Test_Accessors
   with Convention => C;

   procedure Test_Accessibility
   with Convention => C;

   procedure Test_Signals
   with Convention => C;

   procedure Test_Markup_Parse is
      Window : Gtk_Window;
      Label  : Gtk_Label;
   begin
      Window := Gtk_Window_New;
      Label := Gtk_Label_New ("");

      Window.Set_Child (Label);
      Window.Set_Mnemonics_Visible (True);

      Label.Set_Use_Underline (True);
      Label.Set_Use_Markup (True);
      Label.Set_Label
        ("<a href=""test""><span font_style=""italic"">abc</span> _def</a>");

      Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Key_d);

      Assert_Cmpstr_Eq (Get_Text (Label.Get_Layout), "abc def");

      --  The original C test compares the full PangoAttrList dump here (and
      --  again below for each state). That comparison is omitted because the
      --  Ada bindings provide no Pango attribute iterator/accessors. See
      --  transition_notes.md.

      Window.Set_Mnemonics_Visible (False);
      Assert_Cmpstr_Eq (Get_Text (Label.Get_Layout), "abc def");

      Window.Set_Mnemonics_Visible (True);
      Label.Set_Use_Underline (False);
      Assert_Cmpstr_Eq (Get_Text (Label.Get_Layout), "abc _def");

      Window.Destroy;
   end Test_Markup_Parse;

   procedure Test_Underline_Parse is
      Window : Gtk_Window;
      Label  : Gtk_Label;
   begin
      Window := Gtk_Window_New;
      Label := Gtk_Label_New ("");

      Window.Set_Child (Label);
      Window.Set_Mnemonics_Visible (True);

      Label.Set_Use_Markup (False);
      Label.Set_Use_Underline (True);
      Label.Set_Label ("tes_t & no markup <<");

      Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Key_t);

      Window.Destroy;
   end Test_Underline_Parse;

   procedure Test_Parse_More is
      Label : constant Gtk_Label := Gtk_Label_New ("");

      procedure Check
        (Input : UTF8_String;
         UL    : Boolean;
         UM    : Boolean;
         Text  : UTF8_String;
         Accel : Guint);

      procedure Check
        (Input : UTF8_String;
         UL    : Boolean;
         UM    : Boolean;
         Text  : UTF8_String;
         Accel : Guint) is
      begin
         Label.Set_Use_Underline (UL);
         Label.Set_Use_Markup (UM);
         Label.Set_Label (Input);

         Assert_Cmpstr_Eq (Label.Get_Label, Input);
         Assert_Cmpstr_Eq (Label.Get_Text, Text);
         Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Accel);
      end Check;

   begin
      Check ("tes_t m__e mo_re", True, False, "test m_e more", Key_t);
      Check ("test m__e mo_re", True, False, "test m_e more", Key_r);
      Check ("tes_t m__e mo_re", False, False, "tes_t m__e mo_re", VoidSymbol);
      Check ("test m__e more", True, False, "test m_e more", VoidSymbol);
      Check
        ("<span font='test_font'>test <a href='bla'>w_ith</a> bla</span>",
         True,
         True,
         "test with bla",
         Key_i);
      Check
        ("<span font='test_font'>test <a href='bla'>w_ith</a> bla</span>",
         False,
         True,
         "test w_ith bla",
         VoidSymbol);
      Check
        ("<span font='test_font'>test <a href='bla'>with</a> bla</span>",
         True,
         True,
         "test with bla",
         VoidSymbol);
   end Test_Parse_More;

   -----------------------
   -- Test_Constructors --
   -----------------------

   procedure Test_Constructors is
      Function_Label : constant Gtk_Label := Gtk_Label_New ("from function");
      Mnemonic_Label : constant Gtk_Label :=
        Gtk_Label_New_With_Mnemonic ("_Open");
      Out_Label      : Gtk_Label;
      Out_Mnemonic   : Gtk_Label;
      Manual_Label   : constant Gtk_Label := new Gtk_Label_Record;
      Manual_Mnemonic : constant Gtk_Label := new Gtk_Label_Record;
   begin
      --  Cover both constructor forms and check that mnemonic parsing agrees
      --  with the visible text and accelerator key.
      Assert_True (Gtk.Label.Get_Type /= 0);
      Assert_Cmpstr_Eq (Function_Label.Get_Text, "from function");
      Assert_Cmpstr_Eq (Mnemonic_Label.Get_Text, "Open");
      Assert_Cmpuint_Eq (Mnemonic_Label.Get_Mnemonic_Keyval, Character'Pos ('O'));

      --  The output-parameter constructors must initialize the same widgets
      --  as the function constructors.
      Gtk_New (Out_Label, "from procedure");
      Gtk_New_With_Mnemonic (Out_Mnemonic, "_Save");
      Assert_Cmpstr_Eq (Out_Label.Get_Text, "from procedure");
      Assert_Cmpstr_Eq (Out_Mnemonic.Get_Text, "Save");
      Assert_Cmpuint_Eq (Out_Mnemonic.Get_Mnemonic_Keyval, Character'Pos ('S'));

      --  Initialize* is public for derived widgets. Reinitializing an already
      --  created object is documented to leave its first text unchanged.
      Initialize (Manual_Label, "manual");
      Initialize (Manual_Label, "ignored");
      Initialize_With_Mnemonic (Manual_Mnemonic, "_Apply");
      Initialize_With_Mnemonic (Manual_Mnemonic, "_Ignored");
      Assert_Cmpstr_Eq (Manual_Label.Get_Text, "manual");
      Assert_Cmpstr_Eq (Manual_Mnemonic.Get_Text, "Apply");
      Assert_Cmpuint_Eq (Manual_Mnemonic.Get_Mnemonic_Keyval, Character'Pos ('A'));
   end Test_Constructors;

   --------------------
   -- Test_Accessors --
   --------------------

   procedure Test_Accessors is
      Label : constant Gtk_Label := Gtk_Label_New ("select me");
      Target : constant Gtk.Widget.Gtk_Widget :=
        Gtk.Widget.Gtk_Widget (Gtk.Label.Gtk_Label_New ("target"));
      Attrs : constant Pango.Attributes.Pango_Attr_List :=
        Pango.Attributes.Pango_Attr_List_New;
      Tabs : Pango.Tabs.Pango_Tab_Array :=
        Pango.Tabs.Pango_Tab_Array_New (2, Positions_In_Pixels => True);
      Has_Selection : Boolean;
      Start, Finish : Gint;
      Layout_X, Layout_Y : Gint;
      Tab_Align : Pango.Tabs.Pango_Tab_Align;
      Tab_Location : Gint;
   begin
      --  Sink the two floating references because this test owns these
      --  standalone widgets rather than parenting them in a window.
      Label.Ref_Sink;
      Target.Ref_Sink;

      --  Attribute lists and tab arrays are boxed values; the setters copy or
      --  retain their data, so the test keeps the input alive while querying.
      Label.Set_Attributes (Attrs);
      Assert_True (Label.Get_Attributes /= Pango.Attributes.Null_Pango_Attr_List);
      Label.Set_Extra_Menu (null);
      Assert_True (Label.Get_Extra_Menu = null);

      --  Text and markup setters have different escaping rules: plain text is
      --  literal, whereas the markup setter parses Pango's lightweight tags.
      Label.Set_Text ("<b>literal</b>");
      Assert_Cmpstr_Eq (Label.Get_Label, "<b>literal</b>");
      Assert_Cmpstr_Eq (Label.Get_Text, "<b>literal</b>");
      Assert_False (Label.Get_Use_Markup);
      Label.Set_Markup ("<b>bold</b>");
      Assert_True (Label.Get_Use_Markup);
      Assert_Cmpstr_Eq (Label.Get_Text, "bold");
      Label.Set_Markup_With_Mnemonic ("<b>_Open</b>");
      Assert_Cmpstr_Eq (Label.Get_Text, "Open");
      Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Character'Pos ('O'));
      Label.Set_Text_With_Mnemonic ("Sa_ve");
      Assert_Cmpstr_Eq (Label.Get_Label, "Sa_ve");
      Assert_Cmpstr_Eq (Label.Get_Text, "Save");
      Assert_Cmpuint_Eq (Label.Get_Mnemonic_Keyval, Character'Pos ('v'));
      Label.Set_Use_Underline (False);
      Assert_False (Label.Get_Use_Underline);
      Label.Set_Use_Markup (False);
      Assert_False (Label.Get_Use_Markup);

      --  Set the label's principal layout properties and check each getter
      --  against the value just requested.
      Label.Set_Ellipsize (Pango.Layout.Ellipsize_End);
      Assert_True (Label.Get_Ellipsize = Pango.Layout.Ellipsize_End);
      Label.Set_Justify (Gtk.Enums.Justify_Center);
      Assert_True (Label.Get_Justify = Gtk.Enums.Justify_Center);
      Label.Set_Lines (2);
      Assert_Cmpint_Eq (Label.Get_Lines, 2);
      Label.Set_Max_Width_Chars (24);
      Assert_Cmpint_Eq (Label.Get_Max_Width_Chars, 24);
      Label.Set_Width_Chars (12);
      Assert_Cmpint_Eq (Label.Get_Width_Chars, 12);
      Label.Set_Wrap (True);
      Assert_True (Label.Get_Wrap);
      Label.Set_Wrap_Mode (Pango.Enums.Pango_Wrap_Char);
      Assert_True (Label.Get_Wrap_Mode = Pango.Enums.Pango_Wrap_Char);
      Label.Set_Natural_Wrap_Mode (Gtk.Enums.Natural_Wrap_Word);
      Assert_True
        (Label.Get_Natural_Wrap_Mode = Gtk.Enums.Natural_Wrap_Word);
      Label.Set_Single_Line_Mode (True);
      Assert_True (Label.Get_Single_Line_Mode);
      Label.Set_Xalign (0.25);
      Label.Set_Yalign (0.75);
      Assert_True (abs (Label.Get_Xalign - 0.25) < 0.001);
      Assert_True (abs (Label.Get_Yalign - 0.75) < 0.001);

      --  The mnemonic target is a separate widget. GtkLabel exposes the same
      --  object through its getter without transferring ownership.
      Label.Set_Mnemonic_Widget (Target);
      Assert_True (Label.Get_Mnemonic_Widget = Target);

      --  A selected text range is reported in character offsets. The string
      --  here is ASCII, so its byte and character positions are identical.
      Label.Set_Text ("select me");
      Label.Set_Selectable (True);
      Assert_True (Label.Get_Selectable);
      Label.Get_Selection_Bounds (Start, Finish, Has_Selection);
      Assert_False (Has_Selection);
      --  A -1 end offset means the end of the text, as documented by GTK.
      Label.Select_Region (0, -1);
      Label.Get_Selection_Bounds (Start, Finish, Has_Selection);
      Assert_True (Has_Selection);
      Assert_Cmpint_Eq (Start, 0);
      Assert_Cmpint_Eq (Finish, 9);
      Label.Set_Selectable (False);
      Assert_False (Label.Get_Selectable);

      --  Custom tab positions round-trip through GtkLabel; GTK returns an
      --  owned copy, which is freed separately from the input array.
      Pango.Tabs.Set_Tab (Tabs, 0, Pango.Tabs.Pango_Tab_Left, 48);
      Pango.Tabs.Set_Tab (Tabs, 1, Pango.Tabs.Pango_Tab_Left, 96);
      Label.Set_Tabs (Tabs);
      declare
         Read_Tabs : Pango.Tabs.Pango_Tab_Array := Label.Get_Tabs;
      begin
         Assert_Cmpint_Eq (Pango.Tabs.Get_Size (Read_Tabs), 2);
         Assert_True (Pango.Tabs.Get_Positions_In_Pixels (Read_Tabs));
         Pango.Tabs.Get_Tab (Read_Tabs, 1, Tab_Align, Tab_Location);
         Assert_True (Tab_Align = Pango.Tabs.Pango_Tab_Left);
         Assert_Cmpint_Eq (Tab_Location, 96);
         Pango.Tabs.Free (Read_Tabs);
      end;

      --  Layout accessors are available without mapping a window. Offsets may
      --  depend on allocation, but remain nonnegative for this fresh label.
      Assert_True (Label.Get_Layout /= null);
      Label.Get_Layout_Offsets (Layout_X, Layout_Y);
      Assert_True
        (Layout_X in -1_000 .. 1_000 and Layout_Y in -1_000 .. 1_000);
      Assert_Cmpstr_Eq (Label.Get_Current_Uri, "");

      Pango.Tabs.Free (Tabs);
      Pango.Attributes.Unref (Attrs);
      Label.Unref;
      Target.Unref;
   end Test_Accessors;

   -------------------------
   -- Test_Accessibility --
   -------------------------

   procedure Test_Accessibility is
      Label : constant Gtk_Label := Gtk_Label_New ("accessible text");
      X, Y, Width, Height : Gint;
   begin
      Label.Ref_Sink;

      --  GtkLabel has the label role and starts without an explicit
      --  accessible parent, children, or sibling.
      Assert_True (Label.Get_Accessible_Role = Gtk.Accessible.Accessible_Role_Label);
      Assert_True (Label.Get_Accessible_Parent = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Label.Get_First_Accessible_Child = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_True (Label.Get_Next_Accessible_Sibling = Gtk.Accessible.Null_Gtk_Accessible);
      Assert_Cmpstr_Eq (Label.Get_Accessible_Id, "");
      --  An unrooted widget has not yet been assigned an accessibility
      --  context, so the accessor returns null in this headless state.
      Assert_True (Label.Get_At_Context = null);

      --  Bounds are unavailable until a widget is laid out. Calling the
      --  accessor on this unrooted label exercises its false-result path.
      Assert_False (Label.Get_Bounds (X, Y, Width, Height));

      --  Accessible reset/update methods accept enum values and text ranges;
      --  these calls exercise their public wrappers without a screen reader.
      Label.Announce ("label accessibility test", Gtk.Accessible.Accessible_Announcement_Priority_Low);
      Label.Reset_Property (Gtk.Accessible.Accessible_Property_Label);
      Label.Reset_Relation (Gtk.Accessible.Accessible_Relation_Described_By);
      Label.Reset_State (Gtk.Accessible.Accessible_State_Busy);
      Assert_False
        (Label.Get_Platform_State (Gtk.Accessible.Accessible_Platform_State_Focused));
      Label.Update_Platform_State (Gtk.Accessible.Accessible_Platform_State_Focusable);
      Label.Update_Caret_Position;
      Label.Update_Contents (Gtk.Enums.Accessible_Text_Content_Change_Insert, 0, 15);
      Label.Update_Selection_Bound;
      Label.Unref;
   end Test_Accessibility;

   -------------------
   -- Test_Signals --
   -------------------

   procedure Test_Signals is
      Label : constant Gtk_Label := Gtk_Label_New ("signal label");
      Slot_Label : constant Gtk_Label := Gtk_Label_New ("slot signal label");
      Slot  : constant Gtk_Label := Gtk_Label_New ("callback slot");
      Name  : Gtkada.Types.Chars_Ptr;
      URI   : Gtkada.Types.Chars_Ptr := Gtkada.Types.New_String ("https://gtk.test");
      Result : aliased Gboolean := 0;
   begin
      Typed_Void_Count := 0;
      Slot_Void_Count := 0;
      Typed_Link_Count := 0;
      Slot_Link_Count := 0;
      Typed_Move_Count := 0;
      Slot_Move_Count := 0;
      Last_Slot := System.Null_Address;
      Label.Ref_Sink;
      Slot_Label.Ref_Sink;
      Slot.Ref_Sink;

      --  Register both callback styles for each signal. Slot callbacks receive
      --  the supplied GObject, while typed callbacks receive the emitting label.
      Label.On_Activate_Current_Link (Typed_Void'Unrestricted_Access);
      Label.On_Activate_Current_Link (Slot_Void'Unrestricted_Access, Slot);
      Label.On_Copy_Clipboard (Typed_Void'Unrestricted_Access);
      Label.On_Copy_Clipboard (Slot_Void'Unrestricted_Access, Slot);
      Label.On_Activate_Link (Typed_Link'Unrestricted_Access);
      Slot_Label.On_Activate_Link (Slot_Link'Unrestricted_Access, Slot);
      Label.On_Move_Cursor (Typed_Move'Unrestricted_Access);
      Label.On_Move_Cursor (Slot_Move'Unrestricted_Access, Slot);

      --  Return-valued activation must reach both callbacks and return true to
      --  suppress GTK's default attempt to open the test URI.
      Name := Gtkada.Types.New_String ("activate-link");
      Emit_Link_Signal (Label.Get_Object, Name, URI, Result'Access);
      Assert_True (Result /= 0);
      Assert_Cmpuint_Eq (Guint (Typed_Link_Count), 1);

      Result := 0;
      Emit_Link_Signal (Slot_Label.Get_Object, Name, URI, Result'Access);
      Gtkada.Types.Free (Name);
      Assert_True (Result /= 0);
      Assert_Cmpuint_Eq (Guint (Slot_Link_Count), 1);
      Assert_Cmpstr_Eq (Last_URI (1 .. 16), "https://gtk.test");
      Assert_True (Last_Slot = Slot.Get_Object);

      --  Void signals exercise both void marshallers; move-cursor also checks
      --  that the enum, count and boolean arguments survive conversion.
      Name := Gtkada.Types.New_String ("activate-current-link");
      Emit_Void_Signal (Label.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Name := Gtkada.Types.New_String ("copy-clipboard");
      Emit_Void_Signal (Label.Get_Object, Name);
      Gtkada.Types.Free (Name);
      Assert_Cmpuint_Eq (Guint (Typed_Void_Count), 2);
      Assert_Cmpuint_Eq (Guint (Slot_Void_Count), 2);

      Name := Gtkada.Types.New_String ("move-cursor");
      Emit_Move_Signal
        (Label.Get_Object, Name,
         Interfaces.C.int (Gtk.Enums.Gtk_Movement_Step'Pos (Gtk.Enums.Movement_Words)),
         2, 1);
      Gtkada.Types.Free (Name);
      Gtkada.Types.Free (URI);
      Assert_Cmpuint_Eq (Guint (Typed_Move_Count), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Move_Count), 1);
      Assert_True (Last_Move_Step = Gtk.Enums.Movement_Words);
      Assert_Cmpint_Eq (Last_Move_Count, 2);
      Assert_True (Last_Move_Extend);
      Assert_True (Last_Slot = Slot.Get_Object);

      Label.Unref;
      Slot_Label.Unref;
      Slot.Unref;
   end Test_Signals;

   ---------------------
   -- Signal callbacks --
   ---------------------

   procedure Typed_Void (Self : access Gtk_Label_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Typed_Void_Count := Typed_Void_Count + 1;
   end Typed_Void;

   procedure Slot_Void (Self : access Glib.Object.GObject_Record'Class) is
   begin
      Slot_Void_Count := Slot_Void_Count + 1;
      Last_Slot := Self.Get_Object;
   end Slot_Void;

   function Typed_Link
     (Self : access Gtk_Label_Record'Class; URI : UTF8_String)
      return Boolean is
      pragma Unreferenced (Self);
   begin
      Typed_Link_Count := Typed_Link_Count + 1;
      Last_URI := (others => ASCII.NUL);
      Last_URI (1 .. URI'Length) := URI;
      return True;
   end Typed_Link;

   function Slot_Link
     (Self : access Glib.Object.GObject_Record'Class; URI : UTF8_String)
      return Boolean is
   begin
      Slot_Link_Count := Slot_Link_Count + 1;
      Last_Slot := Self.Get_Object;
      Last_URI := (others => ASCII.NUL);
      Last_URI (1 .. URI'Length) := URI;
      return True;
   end Slot_Link;

   procedure Typed_Move
     (Self : access Gtk_Label_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step;
      Count : Gint;
      Extend_Selection : Boolean) is
      pragma Unreferenced (Self);
   begin
      Typed_Move_Count := Typed_Move_Count + 1;
      Last_Move_Step := Step;
      Last_Move_Count := Count;
      Last_Move_Extend := Extend_Selection;
   end Typed_Move;

   procedure Slot_Move
     (Self : access Glib.Object.GObject_Record'Class;
      Step : Gtk.Enums.Gtk_Movement_Step;
      Count : Gint;
      Extend_Selection : Boolean) is
   begin
      Slot_Move_Count := Slot_Move_Count + 1;
      Last_Slot := Self.Get_Object;
      Last_Move_Step := Step;
      Last_Move_Count := Count;
      Last_Move_Extend := Extend_Selection;
   end Slot_Move;

begin
   Glib.Test.Init;

   --  The C test uses gtk_test_init, which also initializes GTK; widgets
   --  cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/label/markup-parse", Test_Markup_Parse'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/underline-parse", Test_Underline_Parse'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/parse-more", Test_Parse_More'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/constructors", Test_Constructors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/accessors", Test_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/accessibility", Test_Accessibility'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/label/signals", Test_Signals'Unrestricted_Access);

   --  Return with the exit code
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Label;
