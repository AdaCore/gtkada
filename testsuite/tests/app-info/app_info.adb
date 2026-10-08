with Ada.Command_Line;
with Glib; use Glib;
with Glib.App_Info; use Glib.App_Info;
with Glib.Error;
with Glib.List_Store; use Glib.List_Store;
with Glib.Object; use Glib.Object;
with Glib.Test; use Glib.Test;
with Glib.Types;

procedure App_Info is
   use type Glib.Error.GError;
   use type App_Info_List.Glist;
   procedure Test_Model with Convention => C;
   procedure Test_All with Convention => C;

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
begin
   Glib.Test.Init;
   Add_Func ("/app-info/model", Test_Model'Unrestricted_Access);
   Add_Func ("/app-info/all", Test_All'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Run);
end App_Info;
