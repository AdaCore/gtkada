-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gtk.Box;
with Gtk.Button;
with Gtk.Widget;
with Gtk.Window;

package Gtk.File_Selection is

   type Gtk_File_Selection_Record is new Gtk.Window.Gtk_Window_Record
     with private;
   type Gtk_File_Selection is access all Gtk_File_Selection_Record'Class;

   function Get_Action_Area (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Box.Gtk_Box;
   function Get_Button_Area (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Box.Gtk_Box;
   function Get_Cancel_Button
     (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button;
   function Get_Dir_List (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget;
   function Get_File_List (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Widget.Gtk_Widget;
   function Get_Filename (File_Selection : access Gtk_File_Selection_Record)
     return String;
   function Get_Help_Button (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button;
   function Get_History_Pulldown
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   function Get_Ok_Button (File_Selection : access Gtk_File_Selection_Record)
     return Gtk.Button.Gtk_Button;
   function Get_Selection_Entry
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   function Get_Selection_Text
     (File_Selection : access Gtk_File_Selection_Record)
      return Gtk.Widget.Gtk_Widget;
   procedure Gtk_New (File_Selection : out Gtk_File_Selection;
                      Title          : in String);
   procedure Initialize (File_Selection : access Gtk_File_Selection_Record;
                         Title          : in String);
   procedure Hide_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record);
   procedure Set_Filename
     (File_Selection  : access Gtk_File_Selection_Record;
      Filename        : in String);
   procedure Show_Fileop_Buttons
     (File_Selection : access Gtk_File_Selection_Record);

   procedure Generate (File_Selection : access Gtk_File_Selection_Record;
                       N              : in Node_Ptr;
                       File           : in File_Type);

   procedure Generate (File_Selection : access Gtk_File_Selection_Record;
                       N              : in Node_Ptr);

private
   type Gtk_File_Selection_Record is new Gtk.Window.Gtk_Window_Record
     with null record;

end Gtk.File_Selection;
