-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                        Copyright (C) 2000                         --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Main;           use Gtk.Main;
with Gtk.Button;         use Gtk.Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtkada.Handlers;    use Gtkada.Handlers;

package body Gtkada.File_Selection is

   type Gtkada_File_Selection_Record is new Gtk_File_Selection_Record with
   record
      File_Selected : Boolean := False;
   end record;
   type Gtkada_File_Selection is access all Gtkada_File_Selection_Record'Class;

   function Delete_Cb (Win : access Gtk_Widget_Record'Class) return Boolean;
   procedure Clicked_Ok_Cb (Button : access Gtk_Widget_Record'Class);
   procedure Clicked_Cancel_Cb (Button : access Gtk_Widget_Record'Class);

   ---------------
   -- Delete_Cb --
   ---------------

   function Delete_Cb (Win : access Gtk_Widget_Record'Class) return Boolean is
   begin
      Main_Quit;
      return True;
   end Delete_Cb;

   -------------------
   -- Clicked_Ok_Cb --
   -------------------

   procedure Clicked_Ok_Cb (Button : access Gtk_Widget_Record'Class) is
   begin
      Gtkada_File_Selection (Get_Toplevel (Button)).File_Selected := True;
      Main_Quit;
   end Clicked_Ok_Cb;

   -----------------------
   -- Clicked_Cancel_Cb --
   -----------------------

   procedure Clicked_Cancel_Cb (Button : access Gtk_Widget_Record'Class) is
   begin
      Main_Quit;
   end Clicked_Cancel_Cb;

   --------------------
   -- Message_Dialog --
   --------------------

   function File_Selection_Dialog
     (Title : String := "Select File") return String
   is
      Dialog : Gtkada_File_Selection;
      Button : Gtk_Button;

   begin
      Dialog := new Gtkada_File_Selection_Record;
      Initialize (Dialog, Title);
      Set_Modal (Dialog);
      Set_Position (Dialog, Win_Pos_Mouse);
      Return_Callback.Connect
        (Dialog, "delete_event",
         Return_Callback.To_Marshaller (Delete_Cb'Access));
      Button := Get_Ok_Button (Dialog);
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Clicked_Ok_Cb'Access));
      Button := Get_Cancel_Button (Dialog);
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Clicked_Cancel_Cb'Access));
      Show_All (Dialog);
      Main;

      if Dialog.File_Selected then
         declare
            S : String := Get_Filename (Dialog);
         begin
            Destroy (Dialog);
            return S;
         end;
      else
         Destroy (Dialog);
         return "";
      end if;
   end File_Selection_Dialog;

end Gtkada.File_Selection;
