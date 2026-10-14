with Ada.Command_Line;
with Glib;               use Glib;
with Glib.Error;
with Glib.GFile;
with Glib.Main;
with Glib.Object;        use Glib.Object;
with Glib.Test;          use Glib.Test;
with Glib.Types;
with Gtk.File_Launcher;  use Gtk.File_Launcher;
with Gtk.Main;

procedure File_Launcher is
   use type Glib.GFile.Gfile;
   use type Glib.Error.GError;
   Done, Folder : Boolean := False;
   procedure Test_Properties with Convention => C;
   procedure Test_Errors with Convention => C;
   procedure On_Done (Source : access GObject_Record'Class; Res : G_Async_Result);

   procedure On_Done (Source : access GObject_Record'Class; Res : G_Async_Result)
   is
      Launcher : constant Gtk_File_Launcher := Gtk_File_Launcher (Source);
      Error : Glib.Error.GError;
      Success : Boolean;
   begin
      if Folder then
         Success := Launcher.Open_Containing_Folder_Finish (Res, Error);
      else
         Success := Launcher.Launch_Finish (Res, Error);
      end if;
      Assert_False (Success);
      Assert_True (Error /= null);
      Glib.Error.Error_Free (Error);
      Done := True;
   end On_Done;

   procedure Test_Properties is
      Launcher : Gtk_File_Launcher;
      File : constant Glib.GFile.Gfile :=
        Glib.GFile.New_For_Path ("/tmp/gtkada-launch.txt");
   begin
      Gtk_New (Launcher, File);
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      Assert_Cmpstr_Eq
        (Glib.GFile.Get_Path (Launcher.Get_File), "/tmp/gtkada-launch.txt");
      Launcher.Set_Always_Ask (True);
      Assert_True (Launcher.Get_Always_Ask);
      Launcher.Set_Always_Ask (False);
      Assert_False (Launcher.Get_Always_Ask);
      Launcher.Set_Writable (True);
      Assert_True (Launcher.Get_Writable);
      Launcher.Set_Writable (False);
      Assert_False (Launcher.Get_Writable);
      Launcher.Set_File (Glib.GFile.Null_Gfile);
      Assert_True (Launcher.Get_File = Glib.GFile.Null_Gfile);
      Launcher.Unref;
   end Test_Properties;

   procedure Test_Errors is
      Launcher : Gtk_File_Launcher;
      Dispatched : Boolean;
   begin
      --  No file means a deterministic async error without starting an app.
      Gtk_New (Launcher, Glib.GFile.Null_Gfile);
      for Is_Folder in Boolean loop
         Folder := Is_Folder;
         Done := False;
         if Folder then
            Launcher.Open_Containing_Folder
              (null, null, On_Done'Unrestricted_Access);
         else
            Launcher.Launch (null, null, On_Done'Unrestricted_Access);
         end if;
         for I in 1 .. 5_000 loop
            exit when Done;
            Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
            delay 0.001;
         end loop;
         Assert_True (Done);
      end loop;
      Launcher.Unref;
   end Test_Errors;
begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/file-launcher/properties", Test_Properties'Unrestricted_Access);
   Add_Func ("/file-launcher/errors", Test_Errors'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end File_Launcher;
