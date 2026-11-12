--  Gtk.Bookmark_List tests: constructor, properties, attributes, and its
--  asynchronous GListModel contents, using a private XBEL fixture.

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with Glib;
with Glib.File_Info;
with Glib.List_Model;
with Glib.Main;
with Glib.Object;
with Glib.Properties;
with Glib.Test;
with Gtk.Bookmark_List;

procedure Bookmark_List is
   use Glib;
   use Glib.Test;
   use Gtk.Bookmark_List;

   use type Glib.Object.GObject;
   use type Gtk.Bookmark_List.Gtk_Bookmark_List;
   use type Glib.List_Model.Glist_Model;

   Base          : constant String :=
     Ada.Directories.Compose (Ada.Directories.Current_Directory, "scratch");
   Bookmark_Path : constant String :=
     Ada.Directories.Compose (Base, "bookmarks.xbel");
   Target_Path   : constant String :=
     Ada.Directories.Compose (Base, "target.txt");
   Target_URI    : constant String := "file://" & Target_Path;

   --  Pump the main context while GIO asynchronously resolves bookmark URIs.
   procedure Wait_Until_Loaded (List : Gtk_Bookmark_List);

   ----------------------
   -- Wait_Until_Loaded --
   ----------------------

   procedure Wait_Until_Loaded (List : Gtk_Bookmark_List) is
      Dispatched : Boolean;
      pragma Unreferenced (Dispatched);
   begin
      for I in 1 .. 5_000 loop
         exit when not Is_Loading (List);
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_False (Is_Loading (List));
   end Wait_Until_Loaded;

   -----------------------------
   -- Test_Constructors_And_Setters --
   -----------------------------

   procedure Test_Constructors_And_Setters
   with Convention => C;

   procedure Test_Constructors_And_Setters is
      List : Gtk_Bookmark_List;
   begin
      --  An empty attributes argument becomes NULL, while the scratch filename
      --  keeps this test independent of the user's recent-bookmarks file.
      Gtk_New (List, Bookmark_Path);
      Assert_True (List /= null);
      Assert_Cmpstr_Eq (Get_Filename (List), Bookmark_Path);
      Assert_Cmpstr_Eq (Get_Attributes (List), "");

      --  Setting a nonempty attribute string updates both getter and property.
      Set_Attributes (List, "standard::name");
      Assert_Cmpstr_Eq (Get_Attributes (List), "standard::name");
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property (List, Attributes_Property),
         "standard::name");

      --  The empty string maps back to a null C pointer and clears attributes.
      Set_Attributes (List, "");
      Assert_Cmpstr_Eq (Get_Attributes (List), "");

      --  Exercise the priority setter and verify its getter.
      Set_Io_Priority (List, 17);
      Assert_Cmpint_Eq (Get_Io_Priority (List), 17);

      --  Initialize must leave an already-created object unchanged.
      Initialize (List, Ada.Directories.Compose (Base, "other.xbel"),
                  "standard::type");
      Assert_Cmpstr_Eq (Get_Filename (List), Bookmark_Path);
      Glib.Object.Unref (Glib.Object.GObject (List));

      --  Gtk_Bookmark_List_New covers the function constructor and non-null
      --  filename/attributes conversions.
      List := Gtk_Bookmark_List_New
        (Bookmark_Path, "standard::name,standard::type");
      Assert_Cmpstr_Eq (Get_Filename (List), Bookmark_Path);
      Assert_Cmpstr_Eq (Get_Attributes (List), "standard::name,standard::type");
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property (List, Filename_Property), Bookmark_Path);
      Assert_Cmpstr_Eq
        (Glib.Properties.Get_Property (List, Attributes_Property),
         "standard::name,standard::type");
      Glib.Object.Unref (Glib.Object.GObject (List));
   end Test_Constructors_And_Setters;

   --------------------
   -- Test_List_Model --
   --------------------

   procedure Test_List_Model
   with Convention => C;

   procedure Test_List_Model is
      List  : constant Gtk_Bookmark_List := Gtk_Bookmark_List_New
        (Bookmark_Path, "standard::name,standard::type");
      Model : constant Glib.List_Model.Glist_Model := +List;
      Item  : Glib.Object.GObject;
   begin
      --  Wait for the bookmark URI to become a GFileInfo list item.
      Wait_Until_Loaded (List);
      Assert_False (Is_Loading (List));
      Assert_Cmpuint_Eq (Get_N_Items (List), 1);
      Assert_Cmpuint_Eq
        (Glib.Properties.Get_Property (List, N_Items_Property), 1);
      Assert_False (Glib.Properties.Get_Property (List, Loading_Property));
      Assert_True (Get_Item_Type (List) = Glib.File_Info.Get_Type);

      --  Check both directions of the GListModel interface conversion.
      Assert_True ((-Model) = List);
      Assert_Cmpuint_Eq (Glib.List_Model.Get_N_Items (Model), 1);

      --  GListModel.Get_Item returns a full reference, released after checking.
      Item := Get_Item (List, 0);
      Assert_True (Item /= null);
      Assert_Cmpstr_Eq (Glib.File_Info.Gfile_Info (Item).Get_Name, "target.txt");
      Glib.Object.Unref (Item);
      Glib.Object.Unref (Glib.Object.GObject (List));
   end Test_List_Model;

   Output : Ada.Text_IO.File_Type;

begin
   Glib.Test.Init;

   --  Recreate an isolated directory so repeated measurement runs are stable.
   if Ada.Directories.Exists (Base) then
      Ada.Directories.Delete_Tree (Base);
   end if;
   Ada.Directories.Create_Directory (Base);

   --  The target file is local, so bookmark enumeration needs no network.
   Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Target_Path);
   Ada.Text_IO.Put_Line (Output, "bookmark target");
   Ada.Text_IO.Close (Output);

   --  This minimal XBEL entry is sufficient for GtkBookmarkList to enumerate.
   Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Bookmark_Path);
   Ada.Text_IO.Put_Line (Output, "<?xml version='1.0' encoding='UTF-8'?>");
   Ada.Text_IO.Put_Line
     (Output,
      "<xbel version='1.0' " &
      "xmlns:bookmark='http://www.freedesktop.org/standards/desktop-bookmarks'>");
   Ada.Text_IO.Put_Line (Output, "  <bookmark href='" & Target_URI & "'>");
   Ada.Text_IO.Put_Line (Output, "    <title>Local target</title>");
   --  GTK includes this application list in the recent::applications attribute.
   Ada.Text_IO.Put_Line (Output, "    <info>");
   Ada.Text_IO.Put_Line
     (Output, "      <metadata owner='http://freedesktop.org'>");
   Ada.Text_IO.Put_Line (Output, "        <bookmark:applications>");
   Ada.Text_IO.Put_Line
     (Output,
      "          <bookmark:application name='gtkada-test' " &
      "exec='gtkada-test %u' modified='2026-11-10T00:00:00Z' count='1'/>");
   Ada.Text_IO.Put_Line (Output, "        </bookmark:applications>");
   Ada.Text_IO.Put_Line (Output, "      </metadata>");
   Ada.Text_IO.Put_Line (Output, "    </info>");
   Ada.Text_IO.Put_Line (Output, "  </bookmark>");
   Ada.Text_IO.Put_Line (Output, "</xbel>");
   Ada.Text_IO.Close (Output);

   Glib.Test.Add_Func
     ("/bookmark-list/constructors-and-setters",
      Test_Constructors_And_Setters'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/bookmark-list/list-model", Test_List_Model'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);

   --  Remove fixtures so the test directory contains no runtime artifacts.
   Ada.Directories.Delete_Tree (Base);
end Bookmark_List;
