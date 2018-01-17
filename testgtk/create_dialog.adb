------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2018, AdaCore                     --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Strings;              use GNAT.Strings;

with Glib.Types;                use Glib.Types;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Recent_Chooser;        use Gtk.Recent_Chooser;
with Gtk.Recent_Chooser_Dialog; use Gtk.Recent_Chooser_Dialog;
with Gtk.Recent_Manager;        use Gtk.Recent_Manager;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;
with Gtk;                       use Gtk;

with Common;                    use Common;

package body Create_Dialog is

   type Gtk_Label_Access is access all Gtk_Label;
   package Label_Destroy is new Handlers.User_Callback
     (Gtk_Label_Record, Gtk_Label_Access);

   package Chooser_Interface_Implementation is new Glib.Types.Implements
     (Gtk_Recent_Chooser,
      Gtk_Recent_Chooser_Dialog_Record,
      Gtk_Recent_Chooser_Dialog);
   use Chooser_Interface_Implementation;

   procedure Destroyed
     (Lab : access Gtk_Label_Record'Class;
      Ptr : Gtk_Label_Access);

   Dialog       : aliased Gtk.Dialog.Gtk_Dialog;
   RDialog      : aliased Gtk.Recent_Chooser_Dialog.Gtk_Recent_Chooser_Dialog;
   Global_Label : aliased Gtk_Label;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Dialog@B is a separate window, usually used to print"
        & " a message for the user, or signal an error." & ASCII.LF
        & "A @bGtk_Dialog@B is split into two boxes, its @bVbox@B that will"
        & " contain the message, and an @bAction_Area@B that contains a"
        & " series of button, like OK, Cancel or Help.";
   end Help;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed
     (Lab : access Gtk_Label_Record'Class;
      Ptr : Gtk_Label_Access)
   is
      pragma Unreferenced (Lab);
   begin
      Ptr.all := null;
   end Destroyed;

   ------------------
   -- Label_Toggle --
   ------------------

   procedure Label_Toggle (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
   begin
      if Global_Label = null then
         Gtk_New (Global_Label, "Dialog Test");
         Label_Destroy.Connect
           (Global_Label, "destroy", Destroyed'Access, Global_Label'Access);
         Set_Padding (Global_Label, 10, 10);
         Pack_Start (Get_Content_Area (Dialog), Global_Label, True, True, 0);
         Show (Global_Label);
      else
         Destroy (Global_Label);
      end if;
   end Label_Toggle;

   ------------------
   -- Basic_Dialog --
   ------------------

   procedure Basic_Dialog (Widget : access Gtk_Widget_Record'Class) is
      Button : Gtk_Widget;
   begin
      if Dialog = null then
         Gtk_New (Dialog,
                  Title => "example dialog",
                  Parent => Gtk_Window (Get_Toplevel (Widget)),
                  Flags  => Use_Header_Bar_From_Settings (Widget));
         Destroy_Dialog_Handler.Connect
           (Dialog, "destroy",
            Destroy_Dialog_Handler.To_Marshaller (Destroy_Dialog'Access),
            Dialog'Access);
         Set_Title (Dialog, "Gtk_Dialog");
         Set_Border_Width (Dialog, 0);
         Set_Size_Request (Dialog, 200, 110);

         Button := Dialog.Add_Button ("OK", Response_Id => 0);
         Button.Set_Can_Default (True);
         Grab_Default (Button);
         Show (Button);

         Button := Dialog.Add_Button ("Toggle", Response_Id => 1);
         Label_Toggle (Button);
         Widget_Handler.Connect (Button, "clicked", Label_Toggle'Access);
         Show (Button);
         Show (Dialog);
      else
         Destroy (Dialog);
      end if;
   end Basic_Dialog;

   --------------------------
   -- Cancel_Recent_Dialog --
   --------------------------

   procedure Cancel_Recent_Dialog
      (Widget : access Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Widget);
   begin
      Put_Line ("Recent dialog cancelled.");
      Destroy (RDialog);
      --  Hack.  For some reason, Destroy does not set this to null.
      RDialog := null;
   end Cancel_Recent_Dialog;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File (Widget : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
      RChooser : constant Gtk_Recent_Chooser :=
        Gtk_Recent_Chooser (To_Interface (RDialog));
   begin
      Put_Line ("URI: " & Get_Current_Uri (RChooser));
      Destroy (RDialog);
      --  Hack.  For some reason, Destroy does not set this to null.
      RDialog := null;
   end Open_File;

   -------------------
   -- Recent_Dialog --
   -------------------

   procedure Recent_Dialog
      (Widget : access Gtk_Widget_Record'Class)
   is
      RManager : constant Gtk_Recent_Manager := Gtk.Recent_Manager.Get_Default;
      Button   : Gtk_Button;
      RChooser : Gtk_Recent_Chooser;
      Empty    : constant GNAT.Strings.String_List (1 .. 0) :=
                  (others => null);

      --  Content to add to the recent list.
      URL      : constant String := "http://www.adacore.com/";
   begin
      --  Before we do anything, in case the system's recent list is empty,
      --  let's make sure we have something to show.
      if Has_Item (RManager, URL) then
         Put_Line ("No need to add " & URL & " to recent list.");
      else
         if
           Add_Full
             (RManager,
              Uri          => URL,
              Display_Name => URL,
              Description  => "AdaCore's web site",
              Mime_Type    => "text/plain",
              App_Name     => "testgtk",
              App_Exec     => "testgtk",
              Groups       => Empty,
              Is_Private   => True)
         then
            Put_Line ("Successfuly added " & URL & " to recent list.");
         else
            Put_Line ("Problem adding " & URL & " to recent list.");
         end if;
      end if;

      if RDialog = null then
         Gtk_New
           (RDialog,
            "Recent Chooser Dialog",
            Gtk.Window.Gtk_Window (Get_Toplevel (Widget)));
         RChooser := Gtk_Recent_Chooser (To_Interface (RDialog));

         Button := Gtk_Button (Add_Button
           (RDialog, Stock_Cancel, Gtk_Response_Cancel));
         Widget_Handler.Connect
           (Button, "clicked",
            Widget_Handler.To_Marshaller (Cancel_Recent_Dialog'Access));

         Button := Gtk_Button (Add_Button
           (RDialog, Stock_Open, Gtk_Response_Accept));
         Widget_Handler.Connect
           (Button, "clicked",
            Widget_Handler.To_Marshaller (Open_File'Access));

         Set_Show_Private   (RChooser, True);
         Set_Show_Tips      (RChooser, True);
         Set_Show_Icons     (RChooser, True);
         Set_Show_Not_Found (RChooser, True);

         Show (RDialog);
      else
         Destroy (RDialog);
         --  Hack.  For some reason, Destroy does not set this to null.
         RDialog := null;
      end if;
   end Recent_Dialog;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1   : Gtk_Vbox;
      Button : Gtk_Button;
   begin
      Set_Label (Frame, "Dialog");

      Gtk_New_Vbox (Box1, False, 10);
      Add (Frame, Box1);

      Gtk_New (Button, "Simple Dialog");
      Widget_Handler.Connect (Button, "clicked", Basic_Dialog'Access);
      Pack_Start (Box1, Button, False, False, 10);

      Gtk_New (Button, "Recent Chooser Dialog");
      Widget_Handler.Connect (Button, "clicked", Recent_Dialog'Access);
      Pack_Start (Box1, Button, False, False, 10);

      Show_All (Frame);
   end Run;

end Create_Dialog;
