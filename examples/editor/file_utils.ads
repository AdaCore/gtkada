with GNAT.OS_Lib; use GNAT.OS_Lib;
with Unchecked_Deallocation;

package File_Utils is

   procedure Free is new Unchecked_Deallocation
     (Object => String, Name => String_Access);

   --  This is the filename of the file currently being edited.
   Current_Filename : String_Access;

   --  This flag is set to TRUE if the file has been changed. We don't actually
   --  use it here, but the program could be extended to prompt to save
   --  changes.
   File_Changed : Boolean := False;

   Buffer_Size : constant := 8192;
   --  The size of chunks to read in when opening files.

   procedure New_File;
   --  Creates a new document, by clearing the text widget and setting the
   --  current filename to null.

   procedure Open_File;
   --  Shows the file selection dialog to open a file

   procedure Save_As;
   --  Shows the file selection dialog to save a file

   procedure Real_Open_File (Filename : String);
   --  Loads the given file

   procedure Real_Save_File (Filename : String);
   --  Saves the given file

   procedure Set_Window_Title;
   --  Sets the window title according to the current filename.

end File_Utils;
