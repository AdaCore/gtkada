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
   procedure Clist_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Color_Selection_Generate        (N : Node_Ptr; File : File_Type);
   procedure Color_Selection_Dialog_Generate (N : Node_Ptr; File : File_Type);
   procedure Combo_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Container_Generate              (N : Node_Ptr; File : File_Type);
   procedure Ctree_Generate                  (N : Node_Ptr; File : File_Type);
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
   procedure Object_Generate                 (N : Node_Ptr; File : File_Type);
   procedure Option_Menu_Generate            (N : Node_Ptr; File : File_Type);
   procedure Packer_Generate                 (N : Node_Ptr; File : File_Type);
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
   procedure Spin_Button_Generate            (N : Node_Ptr; File : File_Type);
   procedure Status_Bar_Generate             (N : Node_Ptr; File : File_Type);
   procedure Table_Generate                  (N : Node_Ptr; File : File_Type);
   procedure Text_Generate                   (N : Node_Ptr; File : File_Type);
   procedure Toggle_Button_Generate          (N : Node_Ptr; File : File_Type);
   procedure Toolbar_Generate                (N : Node_Ptr; File : File_Type);
   procedure Tree_Generate                   (N : Node_Ptr; File : File_Type);
   procedure Tree_Item_Generate              (N : Node_Ptr; File : File_Type);
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
end Gtk_Generates;
