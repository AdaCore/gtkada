--  Exercise the GFile/GListModel boundary and async finish/error marshalling.
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Gtk.Window; use Gtk.Window;
with Glib;             use Glib;
with Glib.Cancellable; use Glib.Cancellable;
with Glib.Error;
with Glib.GFile;
with Glib.List_Model;
with Glib.List_Store;  use Glib.List_Store;
with Glib.Main;
with Glib.Object;      use Glib.Object;
with Glib.Test;        use Glib.Test;
with Glib.Types;
with Gtk.File_Dialog;  use Gtk.File_Dialog;
with Gtk.File_Filter;  use Gtk.File_Filter;
with Gtk.Main;

procedure File_Dialog is
   use type Glib.GFile.Gfile;
   use type Glib.Error.GError;
   use type Glib.List_Model.Glist_Model;
   Done : Boolean := False;
   type Operation is
     (Open_One, Open_Many, Save_One, Folder_One, Folder_Many,
      Text_One, Text_Many, Text_Save);
   Current : Operation;

   procedure On_Open
     (Source : access GObject_Record'Class; Result : G_Async_Result);
   procedure Test_Properties with Convention => C;
   procedure Test_Cancel with Convention => C;

   procedure On_Open
     (Source : access GObject_Record'Class; Result : G_Async_Result)
   is
      Error : Glib.Error.GError;
      Dialog : constant Gtk_File_Dialog := Gtk_File_Dialog (Source);
      File : Glib.GFile.Gfile := Glib.GFile.Null_Gfile;
      Files : Glib.List_Model.Glist_Model := Glib.List_Model.Null_Glist_Model;
      Encoding, Ending : Unbounded_String;
   begin
      case Current is
         when Open_One => File := Dialog.Open_Finish (Result, Error);
         when Open_Many => Files := Dialog.Open_Multiple_Finish (Result, Error);
         when Save_One => File := Dialog.Save_Finish (Result, Error);
         when Folder_One => File := Dialog.Select_Folder_Finish (Result, Error);
         when Folder_Many =>
            Files := Dialog.Select_Multiple_Folders_Finish (Result, Error);
         when Text_One =>
            File := Dialog.Open_Text_File_Finish (Result, Encoding, Error);
         when Text_Many =>
            Files := Dialog.Open_Multiple_Text_Files_Finish
              (Result, Encoding, Error);
         when Text_Save =>
            File := Dialog.Save_Text_File_Finish
              (Result, Encoding, Ending, Error);
      end case;
      Assert_True (File = Glib.GFile.Null_Gfile);
      Assert_True (Files = Glib.List_Model.Null_Glist_Model);
      Assert_Cmpstr_Eq (To_String (Encoding), "");
      Assert_Cmpstr_Eq
        (To_String (Ending), (if Current = Text_Save then "" & ASCII.LF
                             else ""));
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Done := True;
   end On_Open;

   procedure Test_Properties is
      Dialog : Gtk_File_Dialog;
      File : constant Glib.GFile.Gfile :=
        Glib.GFile.New_For_Path ("/tmp/gtkada-picker.txt");
      Folder : constant Glib.GFile.Gfile := Glib.GFile.New_For_Path ("/tmp");
      Filters : Glist_Store;
      Filter : Gtk_File_Filter;
   begin
      Gtk_New (Dialog);
      Dialog.Set_Title ("Choose an Ada file");
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Choose an Ada file");
      Dialog.Set_Accept_Label ("_Choose");
      Assert_Cmpstr_Eq (Dialog.Get_Accept_Label, "_Choose");
      Dialog.Set_Modal (False);
      Assert_False (Dialog.Get_Modal);
      Dialog.Set_Modal (True);
      Assert_True (Dialog.Get_Modal);
      Dialog.Set_Initial_Folder (Folder);
      Dialog.Set_Initial_File (File);
      --  Initial_File is a shortcut for the folder and display name.
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (Folder)));
      Assert_Cmpstr_Eq (Dialog.Get_Initial_Name, "gtkada-picker.txt");
      Assert_Cmpstr_Eq
        (Glib.GFile.Get_Path (Dialog.Get_Initial_Folder), "/tmp");
      Dialog.Set_Initial_Name ("example.adb");
      Assert_Cmpstr_Eq (Dialog.Get_Initial_Name, "example.adb");
      Glib.List_Store.G_New (Filters, Gtk.File_Filter.Get_Type);
      Gtk_New (Filter);
      Filter.Set_Name ("Ada sources");
      Filter.Add_Pattern ("*.ad[bs]");
      Filters.Append (Filter);
      Dialog.Set_Filters (+Filters);
      Dialog.Set_Default_Filter (Filter);
      Filters.Unref;
      Filter.Unref;
      Assert_Cmpint_Eq
        (Gint (Glib.List_Model.Get_N_Items (Dialog.Get_Filters)), 1);
      Assert_Cmpstr_Eq (Dialog.Get_Default_Filter.Get_Name, "Ada sources");
      Dialog.Set_Initial_File (Glib.GFile.Null_Gfile);
      Dialog.Set_Initial_Folder (Glib.GFile.Null_Gfile);
      Dialog.Set_Filters (Glib.List_Model.Null_Glist_Model);
      Dialog.Set_Default_Filter (null);
      Assert_True (Dialog.Get_Initial_File = Glib.GFile.Null_Gfile);
      Assert_True (Dialog.Get_Filters = Glib.List_Model.Null_Glist_Model);
      Dialog.Unref;
   end Test_Properties;

   procedure Test_Cancel is
      Dialog : Gtk_File_Dialog;
      Cancel : Gcancellable;
      Dispatched : Boolean;
      Parent : Gtk_Window;
   begin
      Gtk_New (Dialog);
      G_New (Cancel);
      Gtk_New (Parent);
      for Op in Operation loop
         Current := Op;
         Done := False;
         Cancel.Reset;
         case Op is
            when Open_One =>
               Dialog.Open (Parent, Cancel, On_Open'Unrestricted_Access);
            when Open_Many =>
               Dialog.Open_Multiple (Parent, Cancel, On_Open'Unrestricted_Access);
            when Save_One =>
               Dialog.Save (Parent, Cancel, On_Open'Unrestricted_Access);
            when Folder_One =>
               Dialog.Select_Folder (Parent, Cancel, On_Open'Unrestricted_Access);
            when Folder_Many =>
               Dialog.Select_Multiple_Folders
                 (Parent, Cancel, On_Open'Unrestricted_Access);
            when Text_One =>
               Dialog.Open_Text_File (Parent, Cancel, On_Open'Unrestricted_Access);
            when Text_Many =>
               Dialog.Open_Multiple_Text_Files
                 (Parent, Cancel, On_Open'Unrestricted_Access);
            when Text_Save =>
               Dialog.Save_Text_File (Parent, Cancel, On_Open'Unrestricted_Access);
         end case;
         Cancel.Cancel;
         for I in 1 .. 5_000 loop
            exit when Done;
            Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
            delay 0.001;
         end loop;
         Assert_True (Done);
      end loop;
      Parent.Destroy;
      Cancel.Unref;
      Dialog.Unref;
   end Test_Cancel;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/file-dialog/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/file-dialog/cancel", Test_Cancel'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end File_Dialog;
