--  Code samples quoted by the "Starting an application" chapter of the
--  User's Guide (docs/gtkada_ug/applications.rst). The text between the
--  START and END markers is included verbatim in the guide.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Glib;                  use Glib;
with Glib.Application;      use Glib.Application;
with Gtk.Application;       use Gtk.Application;
with Gtk.Window;            use Gtk.Window;

package UG_Application is

   Has_Unsaved_Changes : Boolean := False;
   --  Whether On_Close_Request should keep the window open

   Opened_Files : Unbounded_String;
   --  The files passed to Open_File, separated by spaces, so that the test
   --  can check what On_Command_Line did

   procedure On_Activate (Self : access Gapplication_Record'Class);
   --  Create and show the main window

   function On_Close_Request
     (Self : access Gtk_Window_Record'Class) return Boolean;
   --  Keep the window open while there are unsaved changes

   function On_Command_Line
     (Self         : access Gapplication_Record'Class;
      Command_Line : not null access Gapplication_Command_Line_Record'Class)
      return Gint;
   --  Open the files named on the command line, then activate

   function Create_Command_Line_App return Gtk_Application;
   --  Create an application that handles its own command line

end UG_Application;
