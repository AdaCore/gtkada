with Interfaces.C.Strings;

package body Gtk.File_Selection is

   package C renames Interfaces.C.Strings;

   --------------------
   --  Get_Filename  --
   --------------------

   function Get_Filename (File_Selection : in Gtk_File_Selection'Class)
                          return String is
      function Internal (File_Selection : in System.Address)
                         return C.Chars_Ptr;
      pragma Import (C, Internal, "gtk_file_selection_get_filename");
   begin
      return C.Value (Internal (Get_Object (File_Selection)));
   end Get_Filename;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (File_Selection : out Gtk_File_Selection) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_file_selection_new");
   begin
      Set_Object (File_Selection, Internal);
   end Gtk_New;


   ---------------------------
   --  Hide_Fileop_Buttons  --
   ---------------------------

   procedure Hide_Fileop_Buttons
     (File_Selection : in out Gtk_File_Selection'Class) is
      procedure Internal (File_Selection : in System.Address);
      pragma Import (C, Internal, "gtk_file_selection_hide_fileop_buttons");
   begin
      Internal (Get_Object (File_Selection));
   end Hide_Fileop_Buttons;


   --------------------
   --  Set_Filename  --
   --------------------

   procedure Set_Filename (File_Selection : in out Gtk_File_Selection'Class;
                           Filename       : in     String) is
      procedure Internal (File_Selection : in System.Address;
                          Filename : in String);
      pragma Import (C, Internal, "gtk_file_selection_set_filename");
   begin
      Internal (Get_Object (File_Selection), Filename & ASCII.NUL);
   end Set_Filename;


   ---------------------------
   --  Show_Fileop_Buttons  --
   ---------------------------

   procedure Show_Fileop_Buttons
     (File_Selection : in out Gtk_File_Selection'Class) is
      procedure Internal (File_Selection : in System.Address);
      pragma Import (C, Internal, "gtk_file_selection_show_fileop_buttons");
   begin
      Internal (Get_Object (File_Selection));
   end Show_Fileop_Buttons;


end Gtk.File_Selection;
