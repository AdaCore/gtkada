with GNAT.Strings;
with Glib.Object;            use Glib.Object;
with Gtk.Application_Window; use Gtk.Application_Window;
with Gtk.Button;             use Gtk.Button;

package body UG_Application is

   procedure On_Quit (Window : access GObject_Record'Class);
   --  Close Window, which is the last one, so that the application ends

   procedure Open_File (Name : String);
   --  Record that Name was opened

   -------------
   -- On_Quit --
   -------------

   --  START quit
   procedure On_Quit (Window : access GObject_Record'Class) is
   begin
      Gtk_Window (Window).Close;
   end On_Quit;
   --  END quit

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File (Name : String) is
   begin
      Append (Opened_Files, Name & " ");
   end Open_File;

   -----------------
   -- On_Activate --
   -----------------

   --  START activate
   procedure On_Activate (Self : access Gapplication_Record'Class) is
      App    : constant Gtk_Application := Gtk_Application (Self);
      Window : Gtk_Application_Window;
      Button : Gtk_Button;
   begin
      --  "activate" is emitted again when the user launches the application
      --  while it is already running: show the existing window instead of
      --  creating a new one.
      if App.Get_Active_Window /= null then
         App.Get_Active_Window.Present;
         return;
      end if;

      Gtk_New (Window, App);
      Window.Set_Title ("Hello");
      Window.Set_Default_Size (300, 200);
      Window.On_Close_Request (On_Close_Request'Access);

      Gtk_New (Button, "Quit");
      Button.On_Clicked (On_Quit'Access, Slot => Window);
      Window.Set_Child (Button);

      Window.Present;
   end On_Activate;
   --  END activate

   ----------------------
   -- On_Close_Request --
   ----------------------

   --  START close_request
   function On_Close_Request
     (Self : access Gtk_Window_Record'Class) return Boolean is
   begin
      --  Returning True stops the window from closing
      return Has_Unsaved_Changes;
   end On_Close_Request;
   --  END close_request

   -----------------------------
   -- Create_Command_Line_App --
   -----------------------------

   function Create_Command_Line_App return Gtk_Application is
      App : Gtk_Application;
   begin
      --  START create_command_line
      Gtk_New (App, "com.example.editor", G_Application_Handles_Command_Line);
      App.On_Activate (On_Activate'Access);
      App.On_Command_Line (On_Command_Line'Access);
      --  END create_command_line
      return App;
   end Create_Command_Line_App;

   ---------------------
   -- On_Command_Line --
   ---------------------

   --  START command_line
   function On_Command_Line
     (Self         : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Gint
   is
      Args : GNAT.Strings.String_List := Command_Line.Get_Arguments;
   begin
      --  Args (Args'First) is the name of the program
      for J in Args'First + 1 .. Args'Last loop
         Open_File (Args (J).all);
      end loop;

      for Arg of Args loop
         GNAT.Strings.Free (Arg);
      end loop;

      Self.Activate;
      return 0;  --  The exit status of the command line
   end On_Command_Line;
   --  END command_line

end UG_Application;
