------------------------------------------------------------------------------
--               GtkAda - Ada binding for the Gimp Toolkit                  --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Directories;
with Glib; use Glib;
with Glib.App_Info; use Glib.App_Info;
with Glib.Error;
with Glib.File_Info; use Glib.File_Info;
with Glib.GFile; use Glib.GFile;
with Glib.List_Model;
with Glib.List_Store; use Glib.List_Store;
with Glib.Object; use Glib.Object;
with Glib.Types;
with Gtk.Box; use Gtk.Box;
with Gtk.Directory_List; use Gtk.Directory_List;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Label; use Gtk.Label;
with Gtk.List_Item; use Gtk.List_Item;
with Gtk.List_View; use Gtk.List_View;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection; use Gtk.Single_Selection;

package body Create_Data_Lists is
   use type App_Info_List.Glist;
   type Browser_Context is record
      Directory : Gtk_Directory_List;
      Path : Gtk_Entry;
      Status : Gtk_Label;
   end record;
   package Browser_Data is new Glib.Object.User_Data (Browser_Context);
   package Status_Data is new Glib.Object.User_Data (Gtk_Label);

   function As_App (Item : GObject) return Glib.App_Info.Gapp_Info is
     (Convert (Get_Object (Item)));

   function Help_Applications return String is
     ("Installed applications from GAppInfo.Get_All are kept in a GListStore."
      & " Double-click an application or press Enter to launch it."
      & " The factory renders each application's display name and description.");
   function Help_Files return String is
     ("GtkDirectoryList asynchronously supplies GFileInfo objects to a list view."
      & " Enter a directory path and press Enter, or activate a subdirectory."
      & " Directory enumeration and monitoring remain responsive to the main loop.");

   procedure Setup
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Label : Gtk_Label;
   begin
      Gtk_New (Label, "");
      Label.Set_Xalign (0.0);
      Label.Set_Margin_Top (6);
      Label.Set_Margin_Bottom (6);
      Label.Set_Margin_Start (12);
      Item.Set_Child (Label);
   end Setup;

   procedure Bind_App
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      App : constant Glib.App_Info.Gapp_Info := As_App (Item.Get_Item);
   begin
      Gtk_Label (Item.Get_Child).Set_Text
        (Get_Display_Name (App) & ASCII.LF & Get_Description (App));
   end Bind_App;

   procedure Bind_File
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Info : constant Gfile_Info := Gfile_Info (Item.Get_Item);
   begin
      Gtk_Label (Item.Get_Child).Set_Text
        (Info.Get_Display_Name
         & (if Info.Get_File_Type = G_File_Type_Directory then "/"
            else "  " & Gint64'Image (Info.Get_Size) & " bytes"));
   end Bind_File;

   procedure Launch_App
     (Self : access Gtk_List_View_Record'Class; Position : Guint)
   is
      Item : constant GObject := Glib.List_Model.Get_Item
        (Glib.List_Model.Glist_Model (Self.Get_Model), Position);
      Error : Glib.Error.GError;
      Ok : Boolean;
   begin
      Ok := Launch (As_App (Item), Gfile_List.Null_List, null, Error);
      Unref (Item);
      if Ok then
         Status_Data.Get (Self).Set_Text ("Application launched");
      else
         Status_Data.Get (Self).Set_Text (Glib.Error.Get_Message (Error));
         Glib.Error.Error_Free (Error);
      end if;
   end Launch_App;

   procedure Open_Directory (Context : Browser_Context; File : Glib.GFile.Gfile) is
   begin
      Context.Directory.Set_File (File);
      Context.Path.Set_Text (Get_Path (File));
      Context.Status.Set_Text ("Activate a directory to open it");
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
   end Open_Directory;

   procedure Activate_File
     (Self : access Gtk_List_View_Record'Class; Position : Guint)
   is
      Context : constant Browser_Context := Browser_Data.Get (Self);
      Item : constant GObject := Context.Directory.Get_Item (Position);
      Info : constant Gfile_Info := Gfile_Info (Item);
   begin
      if Info.Get_File_Type = G_File_Type_Directory then
         Open_Directory
           (Context, Resolve_Relative_Path
              (Context.Directory.Get_File, Info.Get_Name));
      else
         Context.Status.Set_Text (Info.Get_Display_Name);
      end if;
      Unref (Item);
   end Activate_File;

   procedure Enter_Path (Self : access GObject_Record'Class) is
      Context : constant Browser_Context := Browser_Data.Get (Self);
   begin
      Open_Directory (Context, New_For_Path (Context.Path.Get_Text));
   end Enter_Path;

   procedure Run_Applications (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Apps : App_Info_List.Glist := Get_All;
      Cursor : App_Info_List.Glist := Apps;
      Store : Glist_Store;
      Selection : Gtk_Single_Selection;
      Factory : Gtk_Signal_List_Item_Factory;
      View : Gtk_List_View;
      Scroll : Gtk_Scrolled_Window;
      Box : Gtk_Box;
      Status : Gtk_Label;
      App : Glib.App_Info.Gapp_Info;
      Obj : GObject;
   begin
      G_New (Store);
      while Cursor /= App_Info_List.Null_List loop
         App := App_Info_List.Get_Data (Cursor);
         Obj := Glib.Types.To_Object (Glib.Types.GType_Interface (App));
         if Should_Show (App) then
            Store.Append (Obj);
         end if;
         Unref (Obj);
         Cursor := App_Info_List.Next (Cursor);
      end loop;
      App_Info_List.Free (Apps);
      Gtk_New (Selection, +Store);
      Gtk_New (Factory);
      Factory.On_Setup (Setup'Access);
      Factory.On_Bind (Bind_App'Access);
      Gtk_New (View, +Selection, Factory);
      Gtk_New (Status, "Activate an application to launch it");
      Status_Data.Set (View, Status);
      View.On_Activate (Launch_App'Access);
      Gtk_New (Scroll);
      Scroll.Set_Vexpand (True);
      Scroll.Set_Child (View);
      Gtk_New (Box, Orientation_Vertical, 6);
      Box.Append (Scroll);
      Box.Append (Status);
      Frame.Set_Child (Box);
   end Run_Applications;

   procedure Run_Files (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      File : constant Glib.GFile.Gfile := New_For_Path (Ada.Directories.Current_Directory);
      Directory : Gtk_Directory_List;
      Selection : Gtk_Single_Selection;
      Factory : Gtk_Signal_List_Item_Factory;
      View : Gtk_List_View;
      Scroll : Gtk_Scrolled_Window;
      Box : Gtk_Box;
      Path : Gtk_Entry;
      Status : Gtk_Label;
   begin
      Gtk_New (Directory, "standard::name,standard::display-name,standard::type,standard::size", File);
      Unref (Glib.Types.To_Object (Glib.Types.GType_Interface (File)));
      Gtk_New (Selection, +Directory);
      Gtk_New (Factory);
      Factory.On_Setup (Setup'Access);
      Factory.On_Bind (Bind_File'Access);
      Gtk_New (View, +Selection, Factory);
      Gtk_New (Path);
      Path.Set_Text (Ada.Directories.Current_Directory);
      Gtk_New (Status, "Loading directory…");
      Browser_Data.Set (View, (Directory, Path, Status));
      Status_Data.Set (Directory, Status);
      View.On_Activate (Activate_File'Access);
      Path.On_Activate (Enter_Path'Access, View);
      --  The slot connection disconnects when the view is destroyed.
      Gtk_New (Scroll);
      Scroll.Set_Vexpand (True);
      Scroll.Set_Child (View);
      Gtk_New (Box, Orientation_Vertical, 6);
      Box.Append (Path);
      Box.Append (Scroll);
      Box.Append (Status);
      Frame.Set_Child (Box);
   end Run_Files;
end Create_Data_Lists;
