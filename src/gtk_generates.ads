-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
--                                                                   --
--                      Copyright (C) 2001                           --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- GATE is free software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides all the routines needed to output code from Glade
--  XML files for the Gtk+ hiearchy.

with Ada.Text_IO; use Ada.Text_IO;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;

package Gtk_Generates is

   procedure Accel_Label_Generate            (N : Node_Ptr; File : File_Type);
   procedure Alignment_Generate              (N : Node_Ptr; File : File_Type);
   procedure Arrow_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Aspect_Frame_Generate           (N : Node_Ptr; File : File_Type);
   procedure Box_Generate                    (N : Node_Ptr; File : File_Type);
   procedure Button_Generate                 (N : Node_Ptr; File : File_Type);
   procedure Button_Box_Generate             (N : Node_Ptr; File : File_Type);
   procedure Calendar_Generate               (N : Node_Ptr; File : File_Type);
   procedure Check_Button_Generate           (N : Node_Ptr; File : File_Type);
   procedure Check_Menu_Item_Generate        (N : Node_Ptr; File : File_Type);
   procedure Color_Selection_Generate        (N : Node_Ptr; File : File_Type);
   procedure Color_Selection_Dialog_Generate (N : Node_Ptr; File : File_Type);
   procedure Combo_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Container_Generate              (N : Node_Ptr; File : File_Type);
   procedure Curve_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Dialog_Generate                 (N : Node_Ptr; File : File_Type);
   procedure Drawing_Area_Generate           (N : Node_Ptr; File : File_Type);
   procedure Event_Box_Generate              (N : Node_Ptr; File : File_Type);
   procedure File_Selection_Generate         (N : Node_Ptr; File : File_Type);
   procedure Fixed_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Font_Selection_Generate         (N : Node_Ptr; File : File_Type);
   procedure Font_Selection_Dialog_Generate  (N : Node_Ptr; File : File_Type);
   procedure Frame_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Gamma_Curve_Generate            (N : Node_Ptr; File : File_Type);
   procedure GEntry_Generate                 (N : Node_Ptr; File : File_Type);
   procedure GRange_Generate                 (N : Node_Ptr; File : File_Type);
   procedure Handle_Box_Generate             (N : Node_Ptr; File : File_Type);
   procedure Hbutton_Box_Generate            (N : Node_Ptr; File : File_Type);
   procedure Image_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Image_Menu_Item_Generate        (N : Node_Ptr; File : File_Type);
   procedure Input_Dialog_Generate           (N : Node_Ptr; File : File_Type);
   procedure Label_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Layout_Generate                 (N : Node_Ptr; File : File_Type);
   procedure List_Generate                   (N : Node_Ptr; File : File_Type);
   procedure List_Item_Generate              (N : Node_Ptr; File : File_Type);
   procedure Menu_Generate                   (N : Node_Ptr; File : File_Type);
   procedure Menu_Bar_Generate               (N : Node_Ptr; File : File_Type);
   procedure Menu_Item_Generate              (N : Node_Ptr; File : File_Type);
   procedure Misc_Generate                   (N : Node_Ptr; File : File_Type);
   procedure Notebook_Generate               (N : Node_Ptr; File : File_Type);
   procedure Option_Menu_Generate            (N : Node_Ptr; File : File_Type);
   procedure Paned_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Pixmap_Generate                 (N : Node_Ptr; File : File_Type);
   procedure Preview_Generate                (N : Node_Ptr; File : File_Type);
   procedure Progress_Generate               (N : Node_Ptr; File : File_Type);
   procedure Progress_Bar_Generate           (N : Node_Ptr; File : File_Type);
   procedure Radio_Button_Generate           (N : Node_Ptr; File : File_Type);
   procedure Radio_Menu_Item_Generate        (N : Node_Ptr; File : File_Type);
   procedure Ruler_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Scale_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Scrollbar_Generate              (N : Node_Ptr; File : File_Type);
   procedure Scrolled_Window_Generate        (N : Node_Ptr; File : File_Type);
   procedure Separator_Generate              (N : Node_Ptr; File : File_Type);
   procedure Separator_Menu_Item_Generate    (N : Node_Ptr; File : File_Type);
   procedure Spin_Button_Generate            (N : Node_Ptr; File : File_Type);
   procedure Status_Bar_Generate             (N : Node_Ptr; File : File_Type);
   procedure Table_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Text_View_Generate              (N : Node_Ptr; File : File_Type);
   procedure Toggle_Button_Generate          (N : Node_Ptr; File : File_Type);
   procedure Toolbar_Generate                (N : Node_Ptr; File : File_Type);
   procedure Tree_View_Generate              (N : Node_Ptr; File : File_Type);
   procedure Vbutton_Box_Generate            (N : Node_Ptr; File : File_Type);
   procedure Viewport_Generate               (N : Node_Ptr; File : File_Type);
   procedure Widget_Generate                 (N : Node_Ptr; File : File_Type);
   procedure Window_Generate                 (N : Node_Ptr; File : File_Type);

   procedure Bin_Generate                    (N : Node_Ptr; File : File_Type)
     renames Container_Generate;
   procedure Editable_Generate               (N : Node_Ptr; File : File_Type)
     renames Widget_Generate;
   procedure Item_Generate                   (N : Node_Ptr; File : File_Type)
     renames Bin_Generate;
   procedure Menu_Shell_Generate             (N : Node_Ptr; File : File_Type)
     renames Container_Generate;

   procedure End_Generate
      (Project : Node_Ptr; N : Node_Ptr; File : File_Type);
   --  Common part needed for any widget.
   --  This should always be called *after* calling Generate for each widget.
   --  It will in particular set up the flags and accelerator, and put the
   --  widget in the right container using the right procedure call.

end Gtk_Generates;
