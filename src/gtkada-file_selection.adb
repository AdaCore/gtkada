------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2020, AdaCore                     --
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

with Gtk.Main;           use Gtk.Main;
with Gtk.Button;         use Gtk.Button;
with Gtk.Dialog;         use Gtk.Dialog;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.File_Chooser;   use Gtk.File_Chooser;
with Gtk.File_Chooser_Dialog;   use Gtk.File_Chooser_Dialog;
with Gtkada.Handlers;    use Gtkada.Handlers;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Gtk.Stock;          use Gtk.Stock;

package body Gtkada.File_Selection is

   type Gtkada_File_Selection_Record is new Gtk_File_Chooser_Dialog_Record with
   record
      File_Selected : Boolean := False;
   end record;
   type Gtkada_File_Selection is access all Gtkada_File_Selection_Record'Class;

   function Delete_Cb
     (Win : access Gtk_Widget_Record'Class) return Boolean;
   procedure Clicked_Ok_Cb
     (Button : access Gtk_Widget_Record'Class);
   procedure Clicked_Cancel_Cb
     (Button : access Gtk_Widget_Record'Class);

   ---------------
   -- Delete_Cb --
   ---------------

   function Delete_Cb
     (Win : access Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Win);
   begin
      Main_Quit;
      return True;
   end Delete_Cb;

   -------------------
   -- Clicked_Ok_Cb --
   -------------------

   procedure Clicked_Ok_Cb
     (Button : access Gtk_Widget_Record'Class)
   is
   begin
      Gtkada_File_Selection (Get_Toplevel (Button)).File_Selected := True;
      Main_Quit;
   end Clicked_Ok_Cb;

   -----------------------
   -- Clicked_Cancel_Cb --
   -----------------------

   procedure Clicked_Cancel_Cb
     (Button : access Gtk_Widget_Record'Class)
   is
   begin
      Gtkada_File_Selection (Get_Toplevel (Button)).File_Selected := False;
      Main_Quit;
   end Clicked_Cancel_Cb;

   --------------------
   -- Message_Dialog --
   --------------------

   function File_Selection_Dialog
     (Title       : Glib.UTF8_String := "Select File";
      Default_Dir : String := "";
      Dir_Only    : Boolean := False;
      Must_Exist  : Boolean := False) return String
   is
      Dialog : Gtkada_File_Selection;
      Button : Gtk_Button;
      Action : Gtk_File_Chooser_Action;
      Dummy  : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Dialog := new Gtkada_File_Selection_Record;
      if Dir_Only then
         Action := Action_Select_Folder;
      else
         if Must_Exist then
            Action := Action_Open;
         else
            Action := Action_Save;
         end if;
      end if;

      Initialize (Dialog, Title, null, Action);

      if Default_Dir /= "" then
         Dummy := Set_Current_Folder (+Dialog, Default_Dir);
      end if;

      Set_Modal (Dialog);
      Set_Position (Dialog, Win_Pos_Mouse);
      Return_Callback.Connect
        (Dialog, "delete_event",
         Delete_Cb'Access);
      Button := Gtk_Button (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Clicked_Ok_Cb'Access));
      Button := Gtk_Button
        (Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel));
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Clicked_Cancel_Cb'Access));
      Show_All (Dialog);

      Main;

      loop
         if Dialog.File_Selected then
            declare
               S    : constant String := Get_Filename (+Dialog);
               Last : Natural := S'Last;

            begin
               while Last > S'First
                 and then S (Last) = Directory_Separator
               loop
                  Last := Last - 1;
               end loop;

               if S = ""
                 or else not Must_Exist
                 or else
                   (Dir_Only and then Is_Directory (S (S'First .. Last)))
                 or else Is_Regular_File (S (S'First .. Last))
               then
                  Destroy (Dialog);
                  return S;
               else
                  --  A file/dir was entered, but Must_Exist was specified
                  --  and this file doesn't exist: try again.
                  Main;
               end if;
            end;
         else
            Destroy (Dialog);
            return "";
         end if;
      end loop;
   end File_Selection_Dialog;

end Gtkada.File_Selection;
