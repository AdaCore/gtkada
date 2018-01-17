------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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

with Glib;                   use Glib;
with Glib.Action_Map;        use Glib.Action_Map;
with Glib.Error;             use Glib.Error;
with Glib.Object;            use Glib.Object;
with Glib.Menu_Model;        use Glib.Menu_Model;
with Glib.Application;       use Glib.Application;
with Glib.Simple_Action;     use Glib.Simple_Action;
with Glib.Variant;           use Glib.Variant;
with Gtk.Builder;            use Gtk.Builder;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Application;        use Gtk.Application;
with Gtk.Application_Window; use Gtk.Application_Window;
with Gtk.Label;              use Gtk.Label;
with Gtk.Menu_Tool_Button;   use Gtk.Menu_Tool_Button;
with Gtk.Window;             use Gtk.Window;
with Gtk.Widget;             use Gtk.Widget;
with Ada.Text_IO;            use Ada.Text_IO;
with System;                 use System;

package body Create_Application is

   procedure Activate_Quit
      (Action : access Gsimple_Action;
       Parameter : Gvariant;
       Data      : System.Address);
   pragma Convention (C, Activate_Quit);
   --  Implements the "quit" action

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Application_Window@B and @bGtk_Application@B that"
         & " better integrates with the system";
   end Help;

   -------------
   -- Startup --
   -------------

   procedure Startup (Self : access Gapplication_Record'Class)
   is
      App : constant Gtk_Application := Gtk_Application (Self);
      Builder : Gtk_Builder;
      Success : Guint;
      Error   : aliased GError;
   begin
      --  Startup is when the application should create its menu bar. For
      --  this, we load an xml file and then extra parts of its to get the
      --  contents of the application menu and menu bar.

      Put_Line ("Application started");

      Builder := Gtk_Builder_New;
      Success := Builder.Add_From_File ("menus.ui", Error'Access);
      if Success = 0 then
         Put_Line ("Error parsing menus.ui: " & Get_Message (Error));
      else
         App.Set_App_Menu (Gmenu_Model (Builder.Get_Object ("appmenu")));
         App.Set_Menubar (Gmenu_Model (Builder.Get_Object ("menubar")));
      end if;
      Builder.Unref;  --  no longer needed
   end Startup;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Self : access Gapplication_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Put_Line ("Application shut down");
   end Shutdown;

   ------------------
   -- App_Activate --
   ------------------

   procedure App_Activate (Self : access Gapplication_Record'Class)
   is
      App : constant Gtk_Application := Gtk_Application (Self);
      Win : Gtk_Application_Window;
      Builder : Gtk_Builder;
      Menu_Tool : Gtk_Menu_Tool_Button;
      Tool_Menu : Gmenu_Model;
      Success : Guint;
      Error   : aliased GError;
      pragma Unreferenced (Success);
   begin
      --  Activation is when we should create the main window

      Put_Line ("Application activate");

      Win := Gtk_Application_Window_New (App);
      Win.Set_Title ("GtkApplication test");
      Win.Set_Icon_Name ("gtk-home");
      Win.Set_Default_Size (200, 200);

      Builder := Gtk_Builder_New;
      Success := Builder.Add_From_File ("application.ui", Error'Access);

      Win.Add (Gtk_Widget (Builder.Get_Object ("grid")));

      --  The "Open" button with its popup menu
      Menu_Tool := Gtk_Menu_Tool_Button (Builder.Get_Object ("menutool"));
      Tool_Menu := Gmenu_Model (Builder.Get_Object ("toolmenu"));
      Menu_Tool.Set_Menu (Gtk_Menu_New_From_Model (Tool_Menu));

      Win.Show_All;

      Builder.Unref; --  no longer needed
   end App_Activate;

   -------------------
   -- Activate_Quit --
   -------------------

   procedure Activate_Quit
      (Action : access Gsimple_Action;
       Parameter : Gvariant;
       Data      : System.Address)
   is
      pragma Unreferenced (Action, Parameter);
      use Widget_List;
      Stub : Gtk_Application_Record;
      App : constant Gtk_Application :=
         Gtk_Application (Get_User_Data (Data, Stub));
      List, N : Widget_List.Glist;
      Win  : Gtk_Window;
   begin
      List := App.Get_Windows;
      while List /= Null_List loop
         N := Next (List);
         Win := Gtk_Window (Get_Data (List));
         Win.Destroy;
         List := N;
      end loop;
   end Activate_Quit;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Label : Gtk_Label;
      App : Gtk_Application;
      Result : Gint;
   begin
      Gtk_New (Label, "This demo creates a new standalone toplevel window");
      Frame.Add (Label);
      Frame.Show_All;

      App := Gtk_Application_New
         (Application_Id => "com.adacore.testgtk",
          Flags          => G_Application_Flags_None);

      App.Add_Action_Entries
         ((1 => Build ("quit", Activate_Quit'Access)),
          User_Data => App.Get_Object);

      App.On_Startup (Startup'Access);
      App.On_Activate (App_Activate'Access);
      App.On_Shutdown (Shutdown'Access);

      Result := App.Run (0, (1 .. 0 => null));

      Put_Line ("Result of run is" & Gint'Image (Result));
   end Run;

end Create_Application;
