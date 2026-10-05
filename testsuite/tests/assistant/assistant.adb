with Ada.Command_Line;
with Interfaces.C.Strings;
with System;
with Glib;                use Glib;
with Glib.List_Model;     use Glib.List_Model;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Glib.Types;
with Glib.Test;           use Glib.Test;
with Gtk.Assistant;       use Gtk.Assistant;
with Gtk.Assistant_Page;  use Gtk.Assistant_Page;
with Gtk.Button;          use Gtk.Button;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;
with Gtk.Widget;          use Gtk.Widget;

procedure Assistant is
   Prepared : Gtk_Widget := null;
   Prepare_Count : Natural := 0;
   Destroy_Count : Natural := 0;

   Signal_Count : Natural := 0;

   procedure Emit_By_Name
     (Instance : System.Address; Name : Interfaces.C.Strings.chars_ptr)
   with Import, Convention => C_Variadic_2,
        External_Name => "g_signal_emit_by_name";

   procedure On_Action (Self : access Gtk_Assistant_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Signal_Count := Signal_Count + 1;
   end On_Action;

   procedure Test_Signals with Convention => C;
   procedure Test_Pages with Convention => C;
   procedure Test_Properties with Convention => C;
   procedure Test_Navigation with Convention => C;
   procedure Test_Forward_Data with Convention => C;

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
   begin
      Gtk_New (A);
      A.On_Apply (On_Action'Unrestricted_Access);
      A.On_Cancel (On_Action'Unrestricted_Access);
      A.On_Close (On_Action'Unrestricted_Access);
      for I in 1 .. 3 loop
         Signal_Count := 0;
         case I is
            when 1 => Name := Interfaces.C.Strings.New_String ("apply");
            when 2 => Name := Interfaces.C.Strings.New_String ("cancel");
            when 3 => Name := Interfaces.C.Strings.New_String ("close");
         end case;
         Emit_By_Name (A.Get_Object, Name);
         Interfaces.C.Strings.Free (Name);
         Assert_True (Signal_Count = 1);
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
      A.On_Prepare (On_Prepare'Unrestricted_Access);
      A.Set_Current_Page (0);
      Assert_True (Prepared = A.Get_Nth_Page (0));
      Assert_True (Prepare_Count = 1);
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
      A.Set_Forward_Page_Func (null);
      Assert_True (Destroy_Count = 1);
      Forward_Data.Set_Forward_Page_Func
        (A, Forward'Unrestricted_Access, 2);
      A.Destroy;
      Assert_True (Destroy_Count = 2);
   end Test_Forward_Data;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/assistant/signals", Test_Signals'Unrestricted_Access);
   Add_Func ("/assistant/pages", Test_Pages'Unrestricted_Access);
   Add_Func ("/assistant/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/assistant/navigation", Test_Navigation'Unrestricted_Access);
   Add_Func ("/assistant/forward-data", Test_Forward_Data'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Assistant;
