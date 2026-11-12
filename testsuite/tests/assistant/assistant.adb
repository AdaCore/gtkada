--  Tests GtkAssistant page flow, inherited interfaces, and signal overloads.
--  Construction and showing port test_assistant_basic/show from templates.c;
--  the additional API checks exercise GtkAda behavior not covered by those C tests.
with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Types;
with Glib.Main;           use Glib.Main;
with Gdk;
with Gsk.Renderer;
with Glib.Test;           use Glib.Test;
with Gtk.Assistant;       use Gtk.Assistant;
with Gtk.Assistant_Page;  use Gtk.Assistant_Page;
with Gtk.Accessible;      use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Button;          use Gtk.Button;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Window;
with Gtk.Widget;          use Gtk.Widget;

procedure Assistant is
   use type System.Address;
   use type Gtk.Atcontext.Gtk_Atcontext;
   use type Gdk.Gdk_Display;
   use type Gdk.Gdk_Surface;
   use type Gsk.Renderer.Gsk_Renderer;

   Prepared : Gtk_Widget := null;
   Prepare_Count : Natural := 0;
   Destroy_Count : Natural := 0;

   Signal_Count : Natural := 0;
   Slot_Signal_Count : Natural := 0;
   Slot_Prepare_Count : Natural := 0;
   Slot_Prepared : Gtk_Widget := null;
   Slot_Object : System.Address := System.Null_Address;
   Show_Done : Boolean := False;
   pragma Volatile (Show_Done);

   --  The C template test waits briefly after showing its assistant so GTK
   --  can map it; this timeout gives the same bounded event-loop behavior.
   function Finish_Show return Boolean;

   function Finish_Show return Boolean is
   begin
      Show_Done := True;
      return False;
   end Finish_Show;

   procedure Emit_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Action (Self : access Gtk_Assistant_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Signal_Count := Signal_Count + 1;
   end On_Action;

   --  Slot handlers receive the closure's slot object instead of the signal
   --  emitter; using the assistant as both verifies the generated marshaller.
   procedure On_Slot_Action (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Object := Self.Get_Object;
      Slot_Signal_Count := Slot_Signal_Count + 1;
   end On_Slot_Action;

   procedure On_Slot_Prepare
     (Self : access GObject_Record'Class;
      Page : not null access Gtk_Widget_Record'Class) is
   begin
      Assert_True (Self /= null);
      Slot_Object := Self.Get_Object;
      Slot_Prepared := Gtk_Widget (Page);
      Slot_Prepare_Count := Slot_Prepare_Count + 1;
   end On_Slot_Prepare;

   procedure Test_Signals with Convention => C;
   procedure Test_Pages with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Navigation with Convention => C;
   procedure Test_Forward_Data with Convention => C;
   procedure Test_Inherited_Interfaces with Convention => C;
   procedure Test_Basic with Convention => C;
   procedure Test_Show with Convention => C;

   procedure On_Prepare
     (Self : access Gtk_Assistant_Record'Class;
      Page : not null access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Prepared := Gtk_Widget (Page);
      Prepare_Count := Prepare_Count + 1;
   end On_Prepare;

   function Skip_Page (Current_Page : Gint) return Gint is
   begin
      if Current_Page = 0 then
         return 2;
      end if;
      return Current_Page + 1;
   end Skip_Page;

   procedure Destroy_Data (Data : in out Gint) is
   begin
      Assert_True (Data = 2);
      Destroy_Count := Destroy_Count + 1;
   end Destroy_Data;

   package Forward_Data is new Set_Forward_Page_Func_User_Data
     (Gint, Destroy_Data);

   function Forward (Current_Page : Gint; Data : Gint) return Gint is
   begin
      if Current_Page = 0 then
         return Data;
      end if;
      return Current_Page + 1;
   end Forward;

   procedure Test_Signals is
      A : Gtk_Assistant;
      Name : Interfaces.C.Strings.chars_ptr;
      Expected_Object : System.Address;
   begin
      Gtk_New (A);
      Expected_Object := A.Get_Object;
      A.On_Apply (On_Action'Unrestricted_Access);
      A.On_Cancel (On_Action'Unrestricted_Access);
      A.On_Close (On_Action'Unrestricted_Access);
      A.On_Escape (On_Action'Unrestricted_Access);
      A.On_Apply (On_Slot_Action'Unrestricted_Access, Slot => A);
      A.On_Cancel (On_Slot_Action'Unrestricted_Access, Slot => A);
      A.On_Close (On_Slot_Action'Unrestricted_Access, Slot => A);
      A.On_Escape (On_Slot_Action'Unrestricted_Access, Slot => A);
      for I in 1 .. 3 loop
         Signal_Count := 0;
         Slot_Signal_Count := 0;
         case I is
            when 1 => Name := Interfaces.C.Strings.New_String ("apply");
            when 2 => Name := Interfaces.C.Strings.New_String ("cancel");
            when 3 => Name := Interfaces.C.Strings.New_String ("close");
            when others => Name := Interfaces.C.Strings.New_String ("close");
         end case;
         Emit_By_Name (A.Get_Object, Name);
         Interfaces.C.Strings.Free (Name);
         Assert_True (Signal_Count = 1);
         Assert_True (Slot_Signal_Count = 1);
         Assert_True (Slot_Object = Expected_Object);
      end loop;
      A.Destroy;
   end Test_Signals;

   procedure Test_Pages is
      A : Gtk_Assistant;
      First, Middle, Last : Gtk_Label;
      Model : Glist_Model;
      Item : GObject;
      Action : Gtk_Button;
   begin
      Gtk_New (A);
      Assert_True (A.Get_N_Pages = 0);
      Assert_True (A.Get_Current_Page = -1);
      Assert_True (A.Get_Nth_Page (0) = null);
      Model := A.Get_Pages;
      Assert_True (Get_N_Items (Model) = 0);
      Gtk_New (Last, "Last");
      Assert_True (A.Append_Page (Last) = 0);
      Gtk_New (First, "First");
      Assert_True (A.Prepend_Page (First) = 0);
      Gtk_New (Middle, "Middle");
      Assert_True (A.Insert_Page (Middle, 1) = 1);
      Assert_True (A.Get_N_Pages = 3);
      Assert_True (Get_N_Items (Model) = 3);
      Assert_True (A.Get_Nth_Page (0) = Gtk_Widget (First));
      Assert_True (A.Get_Nth_Page (1) = Gtk_Widget (Middle));
      Assert_True (A.Get_Nth_Page (-1) = Gtk_Widget (Last));
      Assert_True (A.Get_Nth_Page (3) = null);
      Item := Get_Item (Model, 1);
      Assert_True (Gtk_Assistant_Page (Item) = A.Get_Page (Middle));
      Assert_True (Gtk_Assistant_Page (Item).Get_Child = Gtk_Widget (Middle));
      Unref (Item);
      A.Remove_Page (1);
      Assert_True (Get_N_Items (Model) = 2);
      Assert_True (A.Get_Nth_Page (1) = Gtk_Widget (Last));
      A.Remove_Page (-1);
      Assert_True (A.Get_N_Pages = 1);
      Gtk_New (Action, "Extra action");
      Ref_Sink (Action);
      A.Add_Action_Widget (Action);
      Assert_True (Action.Get_Parent /= null);
      A.Remove_Action_Widget (Action);
      Assert_True (Action.Get_Parent = null);
      Unref (Action);
      --  Get_Pages returns a full reference to the live model.
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (Model)));
      A.Destroy;
   end Test_Pages;

   procedure Test_Properties is
      A : Gtk_Assistant;
      Child : Gtk_Label;
      Page : Gtk_Assistant_Page;
      Position : Gint;
   begin
      A := Gtk_Assistant_New;
      Gtk_New (Child, "Content");
      Position := A.Append_Page (Child);
      Assert_True (Position = 0);
      Page := A.Get_Page (Child);
      Assert_True (Page.Get_Child = Gtk_Widget (Child));
      Assert_False (A.Get_Page_Complete (Child));
      Assert_True (A.Get_Page_Type (Child) = Content);
      A.Set_Page_Title (Child, "Résumé — café");
      Assert_Cmpstr_Eq (Get_Property (Page, Title_Property), "Résumé — café");
      Set_Property (Page, Title_Property, "Changed title");
      Assert_Cmpstr_Eq (A.Get_Page_Title (Child), "Changed title");
      A.Set_Page_Complete (Child, True);
      Assert_True (Get_Property (Page, Complete_Property));
      Set_Property (Page, Complete_Property, False);
      Assert_False (A.Get_Page_Complete (Child));
      for Kind in Gtk_Assistant_Page_Type loop
         A.Set_Page_Type (Child, Kind);
         Assert_True (Get_Property (Page, Page_Type_Property) = Kind);
         Set_Property (Page, Page_Type_Property, Content);
         Assert_True (A.Get_Page_Type (Child) = Content);
      end loop;
      A.Destroy;
   end Test_Properties;

   procedure Fill (A : Gtk_Assistant) is
      Child : Gtk_Label;
      Position : Gint;
   begin
      for I in 0 .. 2 loop
         Gtk_New (Child, "Page" & Integer'Image (I));
         Position := A.Append_Page (Child);
         Assert_True (Position = Gint (I));
         A.Set_Page_Complete (Child, True);
         if I = 2 then
            A.Set_Page_Type (Child, Summary);
         end if;
      end loop;
   end Fill;

   procedure Test_Navigation is
      A : Gtk_Assistant;
   begin
      Gtk_New (A);
      Fill (A);
      Prepared := null;
      Prepare_Count := 0;
      Slot_Prepare_Count := 0;
      A.On_Prepare (On_Prepare'Unrestricted_Access);
      A.On_Prepare (On_Slot_Prepare'Unrestricted_Access, Slot => A);
      A.Set_Current_Page (0);
      Assert_True (Prepared = A.Get_Nth_Page (0));
      Assert_True (Prepare_Count = 1);
      Assert_True (Slot_Prepare_Count = 1);
      Assert_True (Slot_Prepared = A.Get_Nth_Page (0));
      Assert_True (Slot_Object = A.Get_Object);
      A.Next_Page;
      Assert_True (A.Get_Current_Page = 1);
      Assert_True (Prepared = A.Get_Nth_Page (1));
      A.Previous_Page;
      Assert_True (A.Get_Current_Page = 0);
      A.Set_Forward_Page_Func (Skip_Page'Unrestricted_Access);
      A.Update_Buttons_State;
      A.Next_Page;
      Assert_True (A.Get_Current_Page = 2);
      A.Previous_Page;
      Assert_True (A.Get_Current_Page = 0);
      A.Set_Forward_Page_Func (null);
      A.Next_Page;
      Assert_True (A.Get_Current_Page = 1);
      --  Use an explicit index: the GTK build used here dereferences
      --  past the page list for the documented negative-index shortcut.
      A.Set_Current_Page (2);
      Assert_True (A.Get_Current_Page = 2);
      A.Commit;
      A.Destroy;
   end Test_Navigation;

   procedure Test_Forward_Data is
      A : Gtk_Assistant;
   begin
      Gtk_New (A);
      Fill (A);
      A.Set_Current_Page (0);
      Destroy_Count := 0;
      Forward_Data.Set_Forward_Page_Func
        (A, Forward'Unrestricted_Access, 2);
      A.Next_Page;
      Assert_True (A.Get_Current_Page = 2);
      --  The generic overload accepts a null callback and releases its owned
      --  user data while restoring GTK's sequential default page flow.
      Forward_Data.Set_Forward_Page_Func (A, null, 2);
      Assert_True (Destroy_Count = 1);
      A.Set_Current_Page (0);
      A.Next_Page;
      Assert_True (A.Get_Current_Page = 1);
      Forward_Data.Set_Forward_Page_Func
        (A, Forward'Unrestricted_Access, 2);
      A.Destroy;
      Assert_True (Destroy_Count = 2);
   end Test_Forward_Data;

   --  GtkAssistant also implements Accessible, Root, and Native. Exercise
   --  their generated forwarding wrappers without depending on a screen
   --  reader or presenting a window to a user.
   procedure Test_Inherited_Interfaces is
      A : Gtk_Assistant;
      Child : Gtk_Label;
      First : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Surface_X, Surface_Y : Gdouble;
      Position : Gint;
   begin
      Gtk_New (A);
      Gtk_New (Child, "Assistant page");
      Position := A.Append_Page (Child);
      Assert_Cmpint_Eq (Position, 0);

      --  The assistant is a top-level accessible window with no parent;
      --  bounds stay unavailable until a platform accessibility bridge maps it.
      Assert_Cmpstr_Eq (A.Get_Accessible_Id, "GtkAssistant");
      Assert_True (A.Get_Accessible_Role = Accessible_Role_Window);
      Assert_True (A.Get_Accessible_Parent = Null_Gtk_Accessible);
      First := A.Get_First_Accessible_Child;
      Assert_True (First = A.Get_First_Accessible_Child);
      Assert_True (A.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      Assert_False (A.Get_Platform_State (Accessible_Platform_State_Focused));
      Context := A.Get_At_Context;
      Assert_True (Context = A.Get_At_Context);
      Assert_False (A.Get_Bounds (X, Y, Width, Height));

      --  The following Accessible operations are safe with a detached parent
      --  and exercise each update/reset wrapper while checking observable state.
      A.Announce ("Assistant test", Accessible_Announcement_Priority_Low);
      A.Reset_Property (Accessible_Property_Label);
      A.Reset_Relation (Accessible_Relation_Labelled_By);
      A.Reset_State (Accessible_State_Busy);
      A.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Assert_True (A.Get_Accessible_Parent = Null_Gtk_Accessible);
      A.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Assert_True (A.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);
      A.Update_Platform_State (Accessible_Platform_State_Focusable);

      --  Root methods work before mapping; setting focus on an assistant page
      --  records the requested child, and every window has a GDK display.
      Assert_True (A.Get_Display /= null);
      A.Set_Focus (Child);
      Assert_True (A.Get_Focus = Gtk_Widget (Child));

      --  GTK owns Native realization; before the assistant is presented, these
      --  getters return null and the surface transform is zero.
      Assert_True (A.Get_Surface = null);
      Assert_True (A.Get_Renderer = null);
      A.Get_Surface_Transform (Surface_X, Surface_Y);
      Assert_True (Surface_X = 0.0 and Surface_Y = 0.0);
      A.Destroy;
   end Test_Inherited_Interfaces;

   --  Port templates.c:test_assistant_basic's type and destruction checks.
   procedure Test_Basic is
      A : Gtk_Assistant;
   begin
      A := Gtk_Assistant_New;
      Assert_True (A.Get_Type = Gtk.Assistant.Get_Type);
      A.Destroy;
   end Test_Basic;

   --  Port templates.c:test_assistant_show by checking type, waiting briefly
   --  for mapping, then destroying the presented window.
   procedure Test_Show is
      A : Gtk_Assistant;
      Timeout_Id : G_Source_Id;
      pragma Unreferenced (Timeout_Id);
   begin
      A := Gtk_Assistant_New;
      Assert_True (A.Get_Type = Gtk.Assistant.Get_Type);
      Gtk.Window.Present (Gtk.Window.Gtk_Window (A));
      Show_Done := False;
      Timeout_Id := Timeout_Add (500, Finish_Show'Unrestricted_Access);
      while not Show_Done loop
         declare
            Dispatched : constant Boolean :=
              Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;
      Assert_True (A.Get_Visible);
      Assert_True (A.Get_Surface /= null);
      Assert_True (A.Get_Renderer /= null);
      A.Destroy;
   end Test_Show;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/assistant/signals", Test_Signals'Unrestricted_Access);
   Add_Func ("/assistant/pages", Test_Pages'Unrestricted_Access);
   Add_Func ("/assistant/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/assistant/navigation", Test_Navigation'Unrestricted_Access);
   Add_Func ("/assistant/forward-data", Test_Forward_Data'Unrestricted_Access);
   Add_Func
     ("/assistant/inherited-interfaces",
      Test_Inherited_Interfaces'Unrestricted_Access);
   Add_Func ("/assistant/basic", Test_Basic'Unrestricted_Access);
   Add_Func ("/assistant/show", Test_Show'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Assistant;
