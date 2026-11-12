--  Glib.App_Info tests for command-line-created application information,
--  application catalogue queries, launch errors, and async completion.
--  There is no matching C test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with Glib; use Glib;
with Glib.App_Info; use Glib.App_Info;
with Glib.Cancellable;
with Glib.Error;
with Glib.GFile;
with Glib.G_Icon;
with Glib.List_Store; use Glib.List_Store;
with Glib.Main;
with Glib.Object; use Glib.Object;
with Glib.Types;
with Glib.Test; use Glib.Test;
with Gtk.Enums;

procedure App_Info is
   use type Glib.Error.GError;
   use type Glib.App_Info.Gapp_Info;
   use type Glib.G_Icon.G_Icon;
   use type App_Info_List.Glist;

   Test_Content_Type : constant String :=
     "application/x-gtkada-coverage-unregistered";

   Pending_Async : Natural := 0;
   Launch_App    : Glib.App_Info.Gapp_Info := Null_Gapp_Info;

   --  Instantiate each generic callback interface. The null-callback paths
   --  below are safe; the generated callback path releases data too early.
   package Type_Async_With_Data is new
     Glib.App_Info.Get_Default_For_Type_Async_User_Data (String);
   package Scheme_Async_With_Data is new
     Glib.App_Info.Get_Default_For_Uri_Scheme_Async_User_Data (String);
   package Launch_Async_With_Data is new
     Glib.App_Info.Launch_Default_For_Uri_Async_User_Data (String);

   procedure Test_Model with Convention => C;
   procedure Test_All with Convention => C;
   procedure Test_Methods with Convention => C;
   procedure Test_Catalogue with Convention => C;
   procedure Test_Launch_Errors with Convention => C;
   procedure Test_Async with Convention => C;

   procedure Release (App : in out Glib.App_Info.Gapp_Info);
   procedure Release_List (Apps : in out App_Info_List.Glist);
   procedure Clear (Error : in out Glib.Error.GError);
   procedure Wait_For_Async;

   procedure On_Type_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure On_Scheme_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure On_Launch_Uri_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   procedure On_Default_Uri_Launch_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result);

   -----------
   -- Clear --
   -----------

   procedure Clear (Error : in out Glib.Error.GError) is
   begin
      if Error /= null then
         Glib.Error.Error_Free (Error);
         Error := null;
      end if;
   end Clear;

   -------------
   -- Release --
   -------------

   procedure Release (App : in out Glib.App_Info.Gapp_Info) is
   begin
      if App /= Null_Gapp_Info then
         Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (App)));
         App := Null_Gapp_Info;
      end if;
   end Release;

   ------------------
   -- Release_List --
   ------------------

   procedure Release_List (Apps : in out App_Info_List.Glist) is
      Cursor : App_Info_List.Glist := Apps;
   begin
      --  GIO catalogue lists own one GObject reference for each element.
      while Cursor /= App_Info_List.Null_List loop
         declare
            App : Glib.App_Info.Gapp_Info := App_Info_List.Get_Data (Cursor);
         begin
            Release (App);
         end;
         Cursor := App_Info_List.Next (Cursor);
      end loop;
      App_Info_List.Free (Apps);
   end Release_List;

   --------------------
   -- Wait_For_Async --
   --------------------

   procedure Wait_For_Async is
      Dispatched : Boolean;
   begin
      --  Iterate the default GLib context so pending GIO callbacks can run.
      for I in 1 .. 10_000 loop
         exit when Pending_Async = 0;
         Dispatched := Glib.Main.Main_Context_Iteration (May_Block => False);
         delay 0.001;
      end loop;
      Assert_Cmpuint_Eq (Guint (Pending_Async), 0);
   end Wait_For_Async;

   -------------------
   -- On_Type_Ready --
   -------------------

   procedure On_Type_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
      Error : Glib.Error.GError;
      App   : Glib.App_Info.Gapp_Info;
   begin
      --  The test MIME type has no default app; finish must return an error.
      App := Get_Default_For_Type_Finish (Res, Error);
      Assert_True (App = Null_Gapp_Info);
      Assert_True (Error /= null);
      Clear (Error);
      Release (App);
      Pending_Async := Pending_Async - 1;
   end On_Type_Ready;

   ---------------------
   -- On_Scheme_Ready --
   ---------------------

   procedure On_Scheme_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
      Error : Glib.Error.GError;
      App   : Glib.App_Info.Gapp_Info;
   begin
      --  The private URI scheme has no registered default application.
      App := Get_Default_For_Uri_Scheme_Finish (Res, Error);
      Assert_True (App = Null_Gapp_Info);
      Assert_True (Error /= null);
      Clear (Error);
      Release (App);
      Pending_Async := Pending_Async - 1;
   end On_Scheme_Ready;

   --------------------------
   -- On_Launch_Uri_Ready --
   --------------------------

   procedure On_Launch_Uri_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      --  A pre-cancelled request errors before GIO can spawn the app.
      Success := Launch_Uris_Finish (Launch_App, Res, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Clear (Error);
      Pending_Async := Pending_Async - 1;
   end On_Launch_Uri_Ready;

   ---------------------------------
   -- On_Default_Uri_Launch_Ready --
   ---------------------------------

   procedure On_Default_Uri_Launch_Ready
     (Source_Object : access Glib.Object.GObject_Record'Class;
      Res           : Glib.G_Async_Result)
   is
      pragma Unreferenced (Source_Object);
      Error   : Glib.Error.GError;
      Success : Boolean;
   begin
      --  Cancellation keeps the URI from reaching a desktop handler.
      Success := Launch_Default_For_Uri_Finish (Res, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Clear (Error);
      Pending_Async := Pending_Async - 1;
   end On_Default_Uri_Launch_Ready;

   ----------------
   -- Test_Model --
   ----------------

   procedure Test_Model is
      Error : Glib.Error.GError;
      App : constant Glib.App_Info.Gapp_Info := Create_From_Commandline
        ("gtkada-test-command %f", "GtkAda test application",
         G_App_Info_Create_None, Error);
      Copy : Glib.App_Info.Gapp_Info;
      Obj : GObject;
      Store : Glist_Store;
      Item : GObject;
   begin
      Assert_True (Error = null);
      Assert_True (App /= Null_Gapp_Info);
      Assert_Cmpstr_Eq (Get_Name (App), "GtkAda test application");
      Assert_Cmpstr_Eq (Get_Executable (App), "gtkada-test-command");
      Assert_True (Supports_Files (App));
      Copy := Dup (App);
      Assert_True (Equal (App, App));
      Assert_Cmpstr_Eq (Get_Commandline (Copy), Get_Commandline (App));
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (Copy)));
      Obj := Glib.Types.To_Object (Glib.Types.GType_Interface (App));
      G_New (Store);
      Store.Append (Obj);
      Unref (Obj);
      Assert_Cmpuint_Eq (Store.Get_N_Items, 1);
      Item := Store.Get_Item (0);
      Assert_Cmpstr_Eq (Get_Display_Name (Convert (Get_Object (Item))),
                       "GtkAda test application");
      Store.Remove_All;
      Assert_Cmpuint_Eq (Store.Get_N_Items, 0);
      --  The reference returned by Get_Item outlives removal from the model.
      Assert_Cmpstr_Eq (Get_Name (Convert (Get_Object (Item))),
                       "GtkAda test application");
      Unref (Item);
      Store.Unref;
   end Test_Model;

   ---------------
   -- Test_All --
   ---------------

   procedure Test_All is
      Apps : App_Info_List.Glist := Get_All;
      Cursor : App_Info_List.Glist := Apps;
      App : Glib.App_Info.Gapp_Info;
   begin
      --  An empty application catalogue is valid on a minimal test host.
      while Cursor /= App_Info_List.Null_List loop
         App := App_Info_List.Get_Data (Cursor);
         Assert_True (Get_Name (App)'Length > 0);
         Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (App)));
         Cursor := App_Info_List.Next (Cursor);
      end loop;
      App_Info_List.Free (Apps);
   end Test_All;

   ------------------
   -- Test_Methods --
   ------------------

   procedure Test_Methods is
      Error   : Glib.Error.GError;
      App     : Glib.App_Info.Gapp_Info := Create_From_Commandline
        ("gtkada-test-command %f", "GtkAda test application",
         G_App_Info_Create_None, Error);
      Uri_App : Glib.App_Info.Gapp_Info;
      Other   : Glib.App_Info.Gapp_Info;
      Copy    : Glib.App_Info.Gapp_Info;
      Icon    : Glib.G_Icon.G_Icon;
   begin
      --  A command-line AppInfo gives stable values without desktop-file
      --  dependencies on the host system.
      Assert_True (Error = null);
      Assert_True (App /= Null_Gapp_Info);
      Assert_Cmpstr_Eq (Get_Name (App), "GtkAda test application");
      Assert_Cmpstr_Eq (Get_Display_Name (App), Get_Name (App));
      Assert_Cmpstr_Eq (Get_Executable (App), "gtkada-test-command");
      declare
         Command : constant UTF8_String := Get_Commandline (App);
      begin
         --  GLib may append the file placeholder while normalizing a command.
         Assert_True
           (Command'Length >= 22
            and then Command (Command'First .. Command'First + 21) =
              "gtkada-test-command %f");
      end;
      Assert_Cmpstr_Eq
        (Get_Description (App), "Custom definition for GtkAda test application");
      Assert_False (Should_Show (App));
      Assert_True (Supports_Files (App));
      Assert_False (Supports_Uris (App));
      Assert_False (Can_Delete (App));
      Assert_True (Can_Remove_Supports_Type (App));
      Assert_False (Delete (App));

      --  This command-line app has no icon; Get_Icon returns a nullable icon.
      Icon := Get_Icon (App);
      Assert_True (Icon = null);

      --  Dup preserves app identity, while a different command line is unequal.
      Copy := Dup (App);
      --  GAppInfo equality may be identity-based, so a duplicate need not be
      --  equal even though its public fields match the original.
      Assert_False (Equal (App, Copy));
      Assert_Cmpstr_Eq (Get_Id (App), Get_Id (Copy));
      Assert_True (Equal (+App, App));
      Other := Create_From_Commandline
        ("gtkada-other-command %f", "Other test application",
         G_App_Info_Create_None, Error);
      Assert_True (Error = null);
      Assert_False (Equal (App, Other));

      --  The URI flag and %u placeholder exercise the URI capability result.
      Uri_App := Create_From_Commandline
        ("gtkada-test-command %u", "GtkAda URI test application",
         G_App_Info_Create_Supports_Uris, Error);
      Assert_True (Error = null);
      Assert_True (Supports_Uris (Uri_App));
      Assert_False (Supports_Files (Uri_App));

      --  The generic GList conversion stores interface addresses without
      --  taking ownership, so only the list nodes are freed here.
      declare
         Local_List : App_Info_List.Glist := App_Info_List.Null_List;
      begin
         App_Info_List.Append (Local_List, App);
         Assert_True (Equal (App, App_Info_List.Get_Data (Local_List)));
         App_Info_List.Free (Local_List);
      end;

      Release (Other);
      Release (Copy);
      Release (Uri_App);
      Release (App);
   end Test_Methods;

   --------------------
   -- Test_Catalogue --
   --------------------

   procedure Test_Catalogue is
      All_Apps  : App_Info_List.Glist := Get_All;
      For_Type  : App_Info_List.Glist := Get_All_For_Type (Test_Content_Type);
      Fallbacks : App_Info_List.Glist := Get_Fallback_For_Type
        (Test_Content_Type);
      Recommended : App_Info_List.Glist := Get_Recommended_For_Type
        (Test_Content_Type);
      Default : Glib.App_Info.Gapp_Info;
      Cursor  : App_Info_List.Glist := All_Apps;
   begin
      --  Installed applications vary, so check list elements without assuming
      --  how many are available on the test host.
      while Cursor /= App_Info_List.Null_List loop
         declare
            App : constant Glib.App_Info.Gapp_Info :=
              App_Info_List.Get_Data (Cursor);
         begin
            Assert_True (Get_Name (App)'Length > 0);
         end;
         Cursor := App_Info_List.Next (Cursor);
      end loop;
      Release_List (All_Apps);
      Release_List (For_Type);
      Release_List (Fallbacks);
      Release_List (Recommended);

      --  These private identifiers have no default app association.
      Default := Get_Default_For_Type (Test_Content_Type, False);
      Assert_True (Default = Null_Gapp_Info);
      Default := Get_Default_For_Uri_Scheme ("gtkada-coverage-no-handler");
      Assert_True (Default = Null_Gapp_Info);
   end Test_Catalogue;

   ------------------------
   -- Test_Launch_Errors --
   ------------------------

   procedure Test_Launch_Errors is
      Error   : Glib.Error.GError;
      App     : Glib.App_Info.Gapp_Info := Create_From_Commandline
        ("/no/such/gtkada-app-info-test %f", "Missing test executable",
         G_App_Info_Create_None, Error);
      Uri_App : Glib.App_Info.Gapp_Info := Create_From_Commandline
        ("/no/such/gtkada-app-info-test %u", "Missing URI executable",
         G_App_Info_Create_Supports_Uris, Error);
      Success : Boolean;
   begin
      --  A missing executable produces an error without starting another app.
      Assert_True (Error = null);
      Success := Launch
        (App, Glib.GFile.Gfile_List.Null_List, null, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Clear (Error);

      Success := Launch_Uris
        (Uri_App, Gtk.Enums.String_List.Null_List, null, Error);
      Assert_False (Success);
      Assert_True (Error /= null);
      Clear (Error);

      Release (App);
      Release (Uri_App);
   end Test_Launch_Errors;

   -----------------
   -- Test_Async --
   -----------------

   procedure Test_Async is
      Cancellable : Glib.Cancellable.Gcancellable;
      Error       : Glib.Error.GError;
   begin
      Glib.Cancellable.G_New (Cancellable);
      Glib.Cancellable.Cancel (Cancellable);

      --  Null-callback paths return immediately to Ada; cancellation ensures
      --  that any background launch request cannot contact a desktop handler.
      Get_Default_For_Type_Async
        (Test_Content_Type, False, Cancellable, null);
      Get_Default_For_Uri_Scheme_Async
        ("gtkada-coverage-no-handler", Cancellable, null);
      Launch_Default_For_Uri_Async
        ("gtkada-coverage-handler:test", null, Cancellable, null);
      Launch_App := Create_From_Commandline
        ("gtkada-test-command %u", "GtkAda async test application",
         G_App_Info_Create_Supports_Uris, Error);
      Assert_True (Error = null);
      Launch_Uris_Async
        (Launch_App, Gtk.Enums.String_List.Null_List, null,
         Cancellable, null);

      --  The user-data generic wrappers are also exercised with null callbacks.
      --  Their callback forms currently free the data record before it is used.
      Type_Async_With_Data.Get_Default_For_Type_Async
        (Test_Content_Type, False, Cancellable, null, "unused");
      Scheme_Async_With_Data.Get_Default_For_Uri_Scheme_Async
        ("gtkada-coverage-no-handler", Cancellable, null, "unused");
      Launch_Async_With_Data.Launch_Default_For_Uri_Async
        ("gtkada-coverage-handler:test", null, Cancellable, null, "unused");

      --  Each callback calls its matching finish function and checks the
      --  cancellation error before the cancellable and app references expire.
      Pending_Async := 4;
      Get_Default_For_Type_Async
        (Test_Content_Type, False, Cancellable,
         On_Type_Ready'Unrestricted_Access);
      Get_Default_For_Uri_Scheme_Async
        ("gtkada-coverage-no-handler", Cancellable,
         On_Scheme_Ready'Unrestricted_Access);
      Launch_Uris_Async
        (Launch_App, Gtk.Enums.String_List.Null_List, null,
         Cancellable, On_Launch_Uri_Ready'Unrestricted_Access);
      Launch_Default_For_Uri_Async
        ("gtkada-coverage-handler:test", null, Cancellable,
         On_Default_Uri_Launch_Ready'Unrestricted_Access);
      Wait_For_Async;

      Release (Launch_App);
      Unref (Glib.Object.GObject (Cancellable));
   end Test_Async;

begin
   Glib.Test.Init;
   Add_Func ("/app-info/model", Test_Model'Unrestricted_Access);
   Add_Func ("/app-info/all", Test_All'Unrestricted_Access);
   Add_Func ("/app-info/methods", Test_Methods'Unrestricted_Access);
   Add_Func ("/app-info/catalogue", Test_Catalogue'Unrestricted_Access);
   Add_Func ("/app-info/launch-errors", Test_Launch_Errors'Unrestricted_Access);
   Add_Func ("/app-info/async", Test_Async'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end App_Info;
