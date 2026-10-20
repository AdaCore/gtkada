--  Based on GTK's recentmanager.c, with metadata and ownership checks.
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings; use GNAT.Strings;
with GNAT.OS_Lib;
with Glib;               use Glib;
with Glib.App_Info;      use Glib.App_Info;
with Glib.Date_Time;     use Glib.Date_Time;
with Glib.Error;         use Glib.Error;
with Glib.Values;
with Gtkada.Types;
with Glib.Main;          use Glib.Main;
with Glib.Properties;    use Glib.Properties;
with Glib.Object;        use Glib.Object;
with Glib.Types;
with Glib.Test;          use Glib.Test;
with Gtk.Main;
with Gtk.Recent_Info;    use Gtk.Recent_Info;
with Gtk.Recent_Manager; use Gtk.Recent_Manager;

procedure Recent_Manager is
   Uri : constant String := "file:///tmp/gtkada-recent-binding-test.txt";
   App : constant String := "gtkada-recent-test";
   Data_Dir : constant String := Ada.Directories.Current_Directory
     & "/recent-data-" & Integer'Image
       (GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id));

   --  Give independent managers separate files, including when their
   --  delayed changed signals flush data during another test.
   function New_Manager (Name : String) return Gtk_Recent_Manager is
      Manager : constant Gtk_Recent_Manager := new Gtk_Recent_Manager_Record;
      Params : Glib.Values.GParameter_Array (1 .. 1);
   begin
      Params (1).Name := Gtkada.Types.New_String ("filename");
      Glib.Values.Init (Params (1).Value, GType_String);
      Glib.Values.Set_String (Params (1).Value, Data_Dir & "/" & Name & ".xbel");
      Glib.Values.G_New (Manager, Gtk.Recent_Manager.Get_Type, Params);
      Glib.Values.Free (Params);
      return Manager;
   end New_Manager;

   procedure Test_Metadata with Convention => C;
   procedure Test_Mutations with Convention => C;
   procedure Test_Changed with Convention => C;
   Changed : Boolean := False;

   procedure On_Changed (Self : access Gtk_Recent_Manager_Record'Class) is
      pragma Unreferenced (Self);
   begin
      Changed := True;
   end On_Changed;

   function Add (Manager : Gtk_Recent_Manager; Resource : String)
      return Boolean is
   begin
      return Manager.Add_Full
        (Resource, Mime_Type => "text/plain", App_Name => App,
         App_Exec => "gtkada-recent-test %u", Groups => (1 .. 0 => null),
         Is_Private => False);
   end Add;


   procedure Test_Metadata is
      Manager : constant Gtk_Recent_Manager := Get_Default;
      Error : GError;
      Info, Retained : Gtk_Recent_Info;
      Added, Stamp, Visited : Gdate_Time;
      Count : Gint;
      Uses : Guint;
      Exec : Unbounded_String;
      App_Stamp : Gdate_Time;
      App_Info : Glib.App_Info.Gapp_Info;
      Groups : String_List :=
        (new String'("documents"), new String'("tests"));
   begin
      --  Check the default manager resolved the temporary data directory
      --  before performing any write through it.
      if GNAT.OS_Lib.Normalize_Pathname
           (Ada.Directories.Containing_Directory
              (Get_Property (Manager, Filename_Property))) /=
         GNAT.OS_Lib.Normalize_Pathname (Data_Dir)
      then
         raise Program_Error with "recent database is not isolated";
      end if;
      Assert_True (Manager = Get_Default);
      Assert_True
        (Manager.Add_Full
           (Uri, Display_Name => "Test document", Description => "Metadata",
            Mime_Type => "text/plain", App_Name => App,
            App_Exec => "gtkada-recent-test %u", Groups => Groups,
            Is_Private => True));
      for Group of Groups loop
         Free (Group);
      end loop;
      Assert_True (Manager.Has_Item (Uri));
      Info := Manager.Lookup_Item (Uri, Error);
      Assert_True (Error = null);
      Assert_True (Info /= Null_Gtk_Recent_Info);
      Assert_Cmpstr_Eq (Info.Get_Uri, Uri);
      Assert_Cmpstr_Eq (Info.Get_Display_Name, "Test document");
      Assert_Cmpstr_Eq (Info.Get_Description, "Metadata");
      Assert_Cmpstr_Eq (Info.Get_Mime_Type, "text/plain");
      Assert_True (Info.Get_Private_Hint);
      Assert_True (Info.Is_Local);
      Assert_True (Info.Has_Application (App));
      Assert_True (Info.Has_Group ("documents"));
      Assert_True (Info.Has_Group ("tests"));
      Assert_False (Info.Has_Group ("unknown"));
      Assert_Cmpstr_Eq (Info.Last_Application, App);
      declare
         Length : aliased Gsize;
         Apps : String_List := Info.Get_Applications (Length'Access);
         Names : String_List := Info.Get_Groups;
      begin
         Assert_True (Length = 1);
         Assert_Cmpuint_Eq (Guint (Apps'Length), 1);
         Assert_Cmpstr_Eq (Apps (Apps'First).all, App);
         Assert_Cmpuint_Eq (Guint (Names'Length), 2);
         for Name of Names loop
            Assert_True (Name.all = "documents" or else Name.all = "tests");
            Free (Name);
         end loop;
         for Name of Apps loop
            Free (Name);
         end loop;
      end;
      Assert_True (Info.Get_Application_Info (App, Exec, Uses, App_Stamp));
      Assert_Cmpuint_Eq (Uses, 1);
      Assert_True (App_Stamp /= Null_Gdate_Time);
      Assert_True (App_Stamp.To_Unix > 0);
      Assert_True (Ada.Strings.Fixed.Index (To_String (Exec), Uri) > 0);
      App_Info := Info.Create_App_Info (App, Error);
      Assert_True (Error = null);
      Assert_True (App_Info /= Null_Gapp_Info);
      Assert_Cmpstr_Eq (Get_Executable (App_Info), "gtkada-recent-test");
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (App_Info)));
      Added := Info.Get_Added;
      Stamp := Info.Get_Modified;
      Visited := Info.Get_Visited;
      Assert_True (Added /= Null_Gdate_Time);
      Assert_True (Stamp /= Null_Gdate_Time);
      Assert_True (Visited /= Null_Gdate_Time);
      Assert_True (Added.To_Unix > 0);
      --  Keep a timestamp across manager mutation by taking our own ref.
      Added := Added.Ref;
      Retained := Info.Ref;
      Unref (Info);
      Count := Manager.Purge_Items (Error);
      Assert_True (Error = null);
      Assert_Cmpint_Eq (Count, 1);
      --  Lookup gives the caller a reference independent of the manager.
      Assert_Cmpstr_Eq (Retained.Get_Display_Name, "Test document");
      Assert_True (Added.To_Unix > 0);
      Unref (Retained);
      Unref (Added);
      --  Stamp is borrowed and is not used after the manager changes.
   end Test_Metadata;

   procedure Test_Mutations is
      Manager : constant Gtk_Recent_Manager := New_Manager ("mutations");
      Moved : constant String := "file:///tmp/gtkada-recent-moved.txt";
      Error : GError;
      Info, Other : Gtk_Recent_Info;
      Items : Gtk_Recent_Info_List.Glist;
      use Gtk_Recent_Info_List;
   begin
      --  Each process has its own data directory, shared by its managers.
      Info := Manager.Lookup_Item (Uri, Error);
      Assert_True (Info = Null_Gtk_Recent_Info);
      Assert_True (Gtk_Recent_Manager_Error_Matches
        (Error, Recent_Manager_Error_Not_Found));
      Error_Free (Error);
      Assert_True (Add (Manager, Uri));
      Info := Manager.Lookup_Item (Uri, Error);
      Assert_True (Error = null);
      Other := Manager.Lookup_Item (Uri, Error);
      Assert_True (Info.Match (Other));
      declare
         Groups : constant String_List := Info.Get_Groups;
      begin
         Assert_Cmpuint_Eq (Guint (Groups'Length), 0);
      end;
      Unref (Other);
      Unref (Info);
      Items := Manager.Get_Items;
      Assert_Cmpuint_Eq (Length (Items), 1);
      Info := Get_Data (Items);
      Assert_Cmpstr_Eq (Info.Get_Uri, Uri);
      Unref (Info);
      Free (Items);
      Assert_True (Manager.Move_Item (Uri, Moved, Error));
      Assert_True (Error = null);
      Assert_False (Manager.Has_Item (Uri));
      Assert_True (Manager.Has_Item (Moved));
      Assert_False (Manager.Move_Item (Uri, Moved, Error));
      Assert_True (Gtk_Recent_Manager_Error_Matches
        (Error, Recent_Manager_Error_Not_Found));
      Error_Free (Error);
      Assert_True (Manager.Remove_Item (Moved, Error));
      Assert_True (Error = null);
      Assert_False (Manager.Remove_Item (Moved, Error));
      Assert_True (Gtk_Recent_Manager_Error_Matches
        (Error, Recent_Manager_Error_Not_Found));
      Error_Free (Error);
      --  A null destination requests deletion, rather than an empty URI.
      Assert_True (Add (Manager, Uri));
      Assert_True (Manager.Move_Item (Uri, Error => Error));
      Assert_True (Error = null);
      Assert_False (Manager.Has_Item (Uri));
      Unref (Manager);
   end Test_Mutations;

   procedure Test_Changed is
      Manager : constant Gtk_Recent_Manager := New_Manager ("changed");
      Dispatched : Boolean;
      Count : Gint;
      Error : GError;
   begin
      Manager.On_Changed (On_Changed'Unrestricted_Access);
      Assert_True (Add (Manager, Uri));
      Changed := False;
      for Attempt in 1 .. 300 loop
         Dispatched := Main_Context_Iteration (null, False);
         exit when Changed;
         delay 0.01;
      end loop;
      Assert_True (Changed);
      Count := Manager.Purge_Items (Error);
      Assert_True (Error = null);
      Assert_Cmpint_Eq (Count, 1);
      Unref (Manager);
   end Test_Changed;


begin
   Ada.Directories.Create_Directory (Data_Dir);
   Ada.Environment_Variables.Set ("XDG_DATA_HOME", Data_Dir);
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/recent-manager/metadata", Test_Metadata'Unrestricted_Access);
   Add_Func ("/recent-manager/mutations", Test_Mutations'Unrestricted_Access);
   Add_Func ("/recent-manager/changed", Test_Changed'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
   Ada.Directories.Delete_Tree (Data_Dir);
end Recent_Manager;
