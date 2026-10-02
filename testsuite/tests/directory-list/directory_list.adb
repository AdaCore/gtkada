--  GtkDirectoryList tests, including the interface-valued file property.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Glib;               use Glib;
with Glib.Error;
with Glib.File_Info;     use Glib.File_Info;
with Glib.GFile;         use Glib.GFile;
with Glib.Main;
with Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Glib.Test;          use Glib.Test;
with Glib.Types;

with Gtk.Directory_List; use Gtk.Directory_List;
with Gtk.Main;

procedure Directory_List is

   subtype GType_Interface is Glib.Types.GType_Interface;

   use type Glib.Error.GError;
   use type Glib.Object.GObject;
   use type Glib.Types.GType_Interface;

   Base : constant String := Ada.Directories.Compose
     (Ada.Directories.Current_Directory, "scratch");
   Data_Path : constant String := Ada.Directories.Compose (Base, "data.txt");

   procedure Test_Basics with Convention => C;
   procedure Test_File_Property with Convention => C;
   procedure Test_Enumeration with Convention => C;

   procedure Test_Basics is
      List : Gtk_Directory_List;
   begin
      Gtk_New (List, "standard::name", Null_Gfile);
      Assert_Cmpstr_Eq (List.Get_Attributes, "standard::name");
      Assert_True (List.Get_File = Null_Gfile);
      Assert_True (List.Get_Error = null);
      Assert_False (List.Is_Loading);
      Assert_Cmpuint_Eq (List.Get_N_Items, 0);

      List.Set_Attributes ("standard::*");
      Assert_Cmpstr_Eq
        (Get_Property (List, Attributes_Property), "standard::*");
      Set_Property (List, Io_Priority_Property, Gint (123));
      Assert_Cmpint_Eq (List.Get_Io_Priority, 123);
      List.Set_Monitored (False);
      Assert_False (Get_Property (List, Monitored_Property));

      Glib.Object.Unref (Glib.Object.GObject (List));
   end Test_Basics;

   procedure Test_File_Property is
      List : constant Gtk_Directory_List :=
        Gtk_Directory_List_New ("standard::name", Null_Gfile);
      File : constant Glib.GFile.Gfile := New_For_Path (Base);
      Other : constant Glib.GFile.Gfile := New_For_Path (Base);
   begin
      Assert_True
        (Get_Property (List, File_Property) = GType_Interface (Null_Gfile));

      --  This call only compiles if File_Property is Property_Interface.
      --  Read through both APIs to check that they name the same file.
      Set_Property (List, File_Property, GType_Interface (File));
      Assert_True (List.Get_File = File);
      Assert_True
        (Get_Property (List, File_Property) = GType_Interface (File));

      --  The list keeps its own reference; the property getter is borrowed.
      Glib.Object.Unref (Glib.Types.To_Object (GType_Interface (File)));
      Assert_Cmpstr_Eq (Get_Path (List.Get_File), Base);

      Set_Property (List, File_Property, GType_Interface (Null_Gfile));
      Assert_True (List.Get_File = Null_Gfile);
      Assert_Cmpuint_Eq (List.Get_N_Items, 0);
      List.Set_File (Other);
      Glib.Object.Unref
        (Glib.Types.To_Object (GType_Interface (Other)));
      Assert_True
        (Get_Property (List, File_Property) = GType_Interface (Other));
      List.Set_File (Null_Gfile);
      Assert_True
        (Get_Property (List, File_Property) = GType_Interface (Null_Gfile));

      Glib.Object.Unref (Glib.Object.GObject (List));
   end Test_File_Property;

   procedure Test_Enumeration is
      File : constant Glib.GFile.Gfile := New_For_Path (Base);
      List : constant Gtk_Directory_List := Gtk_Directory_List_New
        ("standard::name,standard::type", File);
      Info : Glib.Object.GObject;
   begin
      List.Set_Monitored (False);
      Glib.Object.Unref (Glib.Types.To_Object (GType_Interface (File)));

      --  Enumeration is asynchronous. The driver bounds the entire test's
      --  runtime, including a main-context iteration that might block.
      while List.Is_Loading loop
         declare
            Dispatched : constant Boolean :=
              Glib.Main.Main_Context_Iteration (null, May_Block => True);
            pragma Unreferenced (Dispatched);
         begin
            null;
         end;
      end loop;

      Assert_True (List.Get_Error = null);
      Assert_Cmpuint_Eq (List.Get_N_Items, 1);
      Assert_Cmpuint_Eq (Get_Property (List, N_Items_Property), 1);
      Assert_False (Get_Property (List, Loading_Property));
      Assert_True (List.Get_Item_Type = Glib.File_Info.Get_Type);
      Info := List.Get_Item (0);
      Assert_True (Info /= null);
      Assert_Cmpstr_Eq (Gfile_Info (Info).Get_Name, "data.txt");
      Assert_True (Gfile_Info (Info).Get_File_Type = G_File_Type_Regular);
      Glib.Object.Unref (Info);

      List.Set_File (Null_Gfile);
      Assert_Cmpuint_Eq (List.Get_N_Items, 0);
      Glib.Object.Unref (Glib.Object.GObject (List));
   end Test_Enumeration;

   Output : Ada.Text_IO.File_Type;

begin
   Glib.Test.Init;
   Gtk.Main.Init;

   Ada.Directories.Create_Directory (Base);
   Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Data_Path);
   Ada.Text_IO.Put_Line (Output, "directory list payload");
   Ada.Text_IO.Close (Output);

   Glib.Test.Add_Func
     ("/directory-list/basics", Test_Basics'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/directory-list/file-property", Test_File_Property'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/directory-list/enumeration", Test_Enumeration'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);

   Ada.Directories.Delete_File (Data_Path);
   Ada.Directories.Delete_Directory (Base);
end Directory_List;
