-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
--                                                                   --
--                   Copyright (C) 1999-2001                         --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;

package body Gtk_Generates is

   use Glib;

   procedure Accel_Label_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_accel_label_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "Accel_Label", Adjust (Get_Field (N, "label").all),
           File => File,
           Prefix => "-(""", Postfix => """)");
      else
         Gen_New (N, "Accel_Label", Adjust (Get_Field (N, "label").all),
           File => File, Prefix => """", Postfix => """");
      end if;

      Label_Generate (N, File);
   end Accel_Label_Generate;

   procedure Alignment_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_alignment_get_type");

   begin
      Build_Type;
      Gen_New
        (N, "Alignment",
         To_Float (Get_Field (N, "xalign").all),
         To_Float (Get_Field (N, "yalign").all),
         To_Float (Get_Field (N, "xscale").all),
         To_Float (Get_Field (N, "xscale").all), "",
         File => File);
      Bin_Generate (N, File);
   end Alignment_Generate;

   procedure Arrow_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_arrow_get_type");

   begin
      Build_Type;
      Gen_New (N, "Arrow", Get_Field (N, "arrow_type").all,
        Get_Field (N, "shadow_type").all, File => File);
      Misc_Generate (N, File);
   end Arrow_Generate;

   procedure Aspect_Frame_Generate (N : Node_Ptr; File : File_Type) is
      S  : String_Ptr;
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_aspect_frame_get_type");

   begin
      Build_Type;
      S := Get_Field (N, "label");

      if S /= null then
         if Gettext_Support (N) then
            Gen_New (N, "Aspect_Frame", S.all,
              To_Float (Get_Field (N, "xalign").all),
              To_Float (Get_Field (N, "yalign").all),
              To_Float (Get_Field (N, "ratio").all),
              Get_Field (N, "obey_child").all,
              File, "-""", """");
         else
            Gen_New (N, "Aspect_Frame", S.all,
              To_Float (Get_Field (N, "xalign").all),
              To_Float (Get_Field (N, "yalign").all),
              To_Float (Get_Field (N, "ratio").all),
              Get_Field (N, "obey_child").all,
              File, """", """");
         end if;
      else
         Gen_New (N, "Aspect_Frame", "",
           To_Float (Get_Field (N, "xalign").all),
           To_Float (Get_Field (N, "yalign").all),
           To_Float (Get_Field (N, "ratio").all),
           Get_Field (N, "obey_child").all,
           File, """", """");
      end if;

      Frame_Generate (N, File);
   end Aspect_Frame_Generate;

   procedure Box_Generate (N : Node_Ptr; File : File_Type) is
      Child_Name : constant Node_Ptr := Find_Tag (N.Child, "child_name");
      Class      : constant String_Ptr := Get_Field (N, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_box_get_type");

   begin
      Build_Type;
      if Child_Name = null then
         if not N.Specific_Data.Created then
            Gen_New (N, "Box", Get_Field (N, "homogeneous").all,
              Get_Field (N, "spacing").all,
              Class (Class'First + 3) & "box", File);
         end if;

      else
         Gen_Child (N, Child_Name, File);
      end if;

      Container_Generate (N, File);

      if Child_Name /= null then
         Gen_Set (N, "Box", "homogeneous", File);
         Gen_Set (N, "Box", "spacing", File);
      end if;
   end Box_Generate;

   procedure Button_Generate (N : Node_Ptr; File : File_Type) is
      Child_Name : constant Node_Ptr   := Find_Tag (N.Child, "child_name");
      Label      : constant String_Ptr := Get_Field (N, "label");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_button_get_type");

   begin
      Build_Type;
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         if Child_Name = null then
            if Label = null then
               Gen_New (N, "Button", File => File);
            else
               if Gettext_Support (N) then
                  Gen_New (N, "Button", Label.all, File => File,
                    Prefix => "-""", Postfix => """");
               else
                  Gen_New (N, "Button", Label.all, File => File,
                    Prefix => """", Postfix => """");
               end if;

            end if;
         else
            Gen_Child (N, Child_Name, File);
         end if;
      end if;

      Container_Generate (N, File);
      Gen_Set (N, "Button", "relief", File);
   end Button_Generate;

   procedure Button_Box_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_button_box_get_type");

   begin
      Build_Type;
      Box_Generate (N, File);
      Gen_Set (N, "Button_Box", "spacing", File);
      Gen_Set (N, "Button_Box", "Layout", "layout_style", "", "", "", File);
      Gen_Set (N, "Button_Box", "Child_Size",
        "child_min_width", "child_min_height", "", "", File);
      Gen_Set (N, "Button_Box", "Child_Ipadding",
        "child_ipad_x", "child_ipad_y", "", "", File);
   end Button_Box_Generate;

   procedure Calendar_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_calendar_get_type");

   begin
      Build_Type;
      Gen_New (N, "Calendar", File => File);
      Widget_Generate (N, File);
   end Calendar_Generate;

   procedure Check_Button_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String_Ptr := Get_Field (N, "label");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_check_button_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         if Label = null then
            Gen_New (N, "Check_Button", File => File);
         else
            if Gettext_Support (N) then
               Gen_New (N, "Check_Button", Label.all, File => File,
                 Prefix => "-""", Postfix => """");
            else
               Gen_New (N, "Check_Button", Label.all, File => File,
                 Prefix => """", Postfix => """");
            end if;
         end if;
      end if;

      Toggle_Button_Generate (N, File);
   end Check_Button_Generate;

   procedure Check_Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_check_menu_item_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "Check_Menu_Item", Get_Field (N, "label").all,
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Check_Menu_Item", Get_Field (N, "label").all,
           File => File, Prefix => """", Postfix => """");
      end if;

      Menu_Item_Generate (N, File);
      Gen_Set (N, "Check_Menu_Item", "active", File);
      Gen_Set (N, "Check_Menu_Item", "always_show_toggle", File => File);
   end Check_Menu_Item_Generate;

   procedure Clist_Generate (N : Node_Ptr; File : File_Type) is
      Columns, S : String_Ptr;
      Cur : constant String_Ptr := Get_Field (N, "name");
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_clist_get_type");

   begin
      Build_Type;
      Columns := Get_Field (N, "columns");

      if not N.Specific_Data.Created then
         if Get_Field (N, "class").all = "GtkCTree" then
            Gen_New (N, "Ctree", Columns.all, File => File);
         else
            Gen_New (N, "Clist", Columns.all, File => File);
         end if;
      end if;

      Container_Generate (N, File);
      Gen_Set (N, "Clist", "selection_mode", File => File);
      Gen_Set (N, "Clist", "shadow_type", File => File);
      Gen_Set (N, "Clist", "show_titles", File);

      S := Get_Field (N, "column_widths");

      if S /= null then
         for J in 0 .. Gint'Value (Columns.all) - 1 loop
            Put (File, "   Set_Column_Width (");

            if Top /= Cur then
               Put (File, To_Ada (Top.all) & ".");
            end if;

            Put_Line (File, To_Ada (Cur.all) & "," & Gint'Image (J) &
              ", " & Get_Part (S.all, Integer (J + 1), ',') & ");");
         end loop;
      end if;
   end Clist_Generate;

   procedure Color_Selection_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_color_selection_get_type");

   begin
      Build_Type;
      Gen_New (N, "Color_Selection", File => File);
      Gen_Set (N, "Color_Selection", "Update_Policy", "policy", File => File);
      Box_Generate (N, File);
   end Color_Selection_Generate;

   procedure Color_Selection_Dialog_Generate
     (N : Node_Ptr; File : File_Type)
   is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_color_selection_dialog_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "Color_Selection_Dialog", Get_Field (N, "title").all,
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Color_Selection_Dialog", Get_Field (N, "title").all,
           File => File, Prefix => """", Postfix => """");
      end if;

      Window_Generate (N, File);
   end Color_Selection_Dialog_Generate;

   procedure Combo_Generate (N : Node_Ptr; File : File_Type) is
      S     : String_Ptr;
      First, Last : Natural;
      Top_Widget  : Node_Ptr := Find_Top_Widget (N);
      Top   : constant String_Ptr := Get_Field (Top_Widget, "name");
      Child : Node_Ptr := Find_Tag (N.Child, "widget");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_combo_get_type");

   begin
      Build_Type;
      Gen_New (N, "Combo", File => File);

      --  The child is the entry field associated with the combo box. It only
      --  exists for Glade >= 0.5. Do not generate any "Add"

      if Child /= null then
         Child.Specific_Data.Has_Container := True;
      end if;

      Box_Generate (N, File);
      Gen_Set (N, "Combo", "case_sensitive", File);
      Gen_Set (N, "Combo", "use_arrows", File);
      Gen_Set (N, "Combo", "use_arrows_always", File);

      S := Get_Field (N, "items");

      if S /= null then
         First := S'First;

         loop
            Last := Index (S (First .. S'Last), ASCII.LF & "");

            if Last = 0 then
               Last := S'Last + 1;
            end if;

            Put (File, "   String_List.Append (" &
              To_Ada (Get_Field (N, "name").all) & "_Items, ");

            if Gettext_Support (Top_Widget) then
               Put (File, '-');
            end if;

            Put_Line (File, '"' &
              S (First .. Last - 1) & """);");

            exit when Last >= S'Last;

            First := Last + 1;
         end loop;

         Put_Line (File, "   Combo.Set_Popdown_Strings (" &
                   To_Ada (Top.all) & "." &
                   To_Ada (Get_Field (N, "name").all) & ", " &
                   To_Ada (Get_Field (N, "name").all) & "_Items);");
         Put_Line (File, "   Free_String_List (" &
           To_Ada (Get_Field (N, "name").all) & "_Items);");
      end if;
   end Combo_Generate;

   procedure Container_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_container_get_type");

   begin
      Build_Type;
      Widget_Generate (N, File);
      Gen_Set (N, "Container", "border_width", File);
      Gen_Set (N, "Container", "resize_mode", File);
   end Container_Generate;

   procedure Ctree_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_ctree_get_type");

   begin
      Build_Type;
      Clist_Generate (N, File);
   end Ctree_Generate;

   procedure Curve_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_curve_get_type");

   begin
      Build_Type;
      Gen_New (N, "Curve", File => File);
      Drawing_Area_Generate (N, File);
      Gen_Set (N, "Curve", "curve_type", File => File);
      Gen_Set (N, "Curve", "Range", "min_x", "max_x", "min_y", "max_y",
        File => File, Is_Float => True);
   end Curve_Generate;

   procedure Dialog_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_dialog_get_type");

   begin
      Build_Type;
      Gen_New (N, "Dialog", File => File);
      Window_Generate (N, File);
   end Dialog_Generate;

   procedure Drawing_Area_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_drawing_area_get_type");

   begin
      Build_Type;
      Gen_New (N, "Drawing_Area", File => File);
      Widget_Generate (N, File);
   end Drawing_Area_Generate;

   procedure Event_Box_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_event_box_get_type");

   begin
      Build_Type;
      Gen_New (N, "Event_Box", File => File);
      Bin_Generate (N, File);
   end Event_Box_Generate;

   procedure File_Selection_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_file_selection_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "File_Selection", Get_Field (N, "title").all,
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "File_Selection", Get_Field (N, "title").all,
           File => File, Prefix => """", Postfix => """");
      end if;

      Gen_Set (N, "File_Selection", "show_file_op_buttons", File);
      Window_Generate (N, File);
   end File_Selection_Generate;

   procedure Fixed_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_fixed_get_type");

   begin
      Build_Type;
      Gen_New (N, "Fixed", File => File);
      Container_Generate (N, File);
   end Fixed_Generate;

   procedure Font_Selection_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_font_selection_get_type");

   begin
      Build_Type;
      Gen_New (N, "Font_Selection", File => File);
      Notebook_Generate (N, File);
   end Font_Selection_Generate;

   procedure Font_Selection_Dialog_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_font_selection_dialog_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "Font_Selection_Dialog", Get_Field (N, "title").all,
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Font_Selection_Dialog", Get_Field (N, "title").all,
           File => File, Prefix => """", Postfix => """");
      end if;

      Window_Generate (N, File);
   end Font_Selection_Dialog_Generate;

   procedure Frame_Generate (N : Node_Ptr; File : File_Type) is
      S  : String_Ptr;
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_frame_get_type");

   begin
      Build_Type;
      S := Get_Field (N, "label");

      if S /= null then
         if Gettext_Support (N) then
            Gen_New (N, "Frame", S.all,
              File => File, Prefix => "-""", Postfix => """");
         else
            Gen_New (N, "Frame", S.all,
              File => File, Prefix => """", Postfix => """");
         end if;
      else
         Gen_New (N, "Frame", File => File);
      end if;

      Bin_Generate (N, File);
      Gen_Set
        (N, "Frame", "Label_Align",
         "label_xalign", "label_yalign", "", "", File,
         Is_Float => True);
      Gen_Set (N, "Frame", "shadow_type", File);
   end Frame_Generate;

   procedure Gamma_Curve_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_gamma_curve_get_type");

   begin
      Build_Type;
      Gen_New (N, "Gamma_Curve", File => File);
      Box_Generate (N, File);
      Add_Package ("Curve");
      Put_Line (File, "   Set_Range (Get_Curve (" &
        To_Ada (Get_Field (Find_Top_Widget (N), "name").all) & "." &
        To_Ada (Get_Field (N, "name").all) & "), " &
        To_Float (Get_Field (N, "min_x").all) & ", " &
        To_Float (Get_Field (N, "max_x").all) & ", " &
        To_Float (Get_Field (N, "min_y").all) & ", " &
        To_Float (Get_Field (N, "max_y").all) & ");");
   end Gamma_Curve_Generate;

   procedure GEntry_Generate (N : Node_Ptr; File : File_Type) is
      Child_Name : Node_Ptr := Find_Tag (N.Child, "child_name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_entry_get_type");

   begin
      Build_Type;
      if Child_Name = null then
         Gen_New (N, "GEntry", File => File);
      else
         Gen_Child (N, Child_Name, File);
      end if;

      Editable_Generate (N, File);
      Gen_Set (N, "GEntry", "editable", File);
      Gen_Set (N, "GEntry", "Max_Length", "text_max_length", "", "", "", File);
      Gen_Set (N, "GEntry", "position", File);

      if Gettext_Support (N) then
         Gen_Set (N, "GEntry", "text", File, "-""", """");
      else
         Gen_Set (N, "GEntry", "text", File, """", """");
      end if;

      Gen_Set (N, "GEntry", "Visibility", "text_visible", "", "", "", File);
   end GEntry_Generate;

   procedure GRange_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_range_get_type");

   begin
      Build_Type;
      Widget_Generate (N, File);
      Gen_Set (N, "GRange", "Update_Policy", "policy", File => File);
   end GRange_Generate;

   procedure Handle_Box_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_handle_box_get_type");

   begin
      Build_Type;
      Gen_New (N, "Handle_Box", File => File);
      Bin_Generate (N, File);
      Gen_Set (N, "Handle_Box", "shadow_type", File);
      Gen_Set (N, "Handle_Box", "handle_position", File);
      Gen_Set (N, "Handle_Box", "snap_edge", File);
   end Handle_Box_Generate;

   procedure Hbutton_Box_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_hbutton_box_get_type");

   begin
      Build_Type;
      Gen_New (N, "Hbutton_Box", File => File);
      Button_Box_Generate (N, File);
   end Hbutton_Box_Generate;

   procedure Image_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_image_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         Add_Package ("Gdk.Image");
         Add_Package ("Gdk.Bitmap");
         Add_Package ("Gdk.Visual");

         Put_Line (File, "   Get_System (The_Visual);");
         Put_Line (File, "   Gdk_New (The_Image, " &
           To_Ada (Get_Field (N, "image_type").all) & ", The_Visual, " &
           Get_Field (N, "image_width").all & ", " &
           Get_Field (N, "image_height").all & ");");
         Gen_New (N, "Image", "The_Image", "Null_Bitmap", File => File);
      end if;

      Misc_Generate (N, File);
   end Image_Generate;

   procedure Input_Dialog_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_input_dialog_get_type");

   begin
      Build_Type;
      Gen_New (N, "Input_Dialog", File => File);
      Dialog_Generate (N, File);
   end Input_Dialog_Generate;

   procedure Label_Generate (N : Node_Ptr; File : File_Type) is
      Child_Name : String_Ptr := Get_Field (N, "child_name");
      S          : String_Ptr;
      Top        : String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      P          : Node_Ptr;
      Num        : Gint;
      Is_Tab,
      Is_Title   : Boolean;
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_label_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "Label", Adjust (Get_Field (N, "label").all),
           File => File, Prefix => "-(""", Postfix => """)");
      else
         Gen_New (N, "Label", Adjust (Get_Field (N, "label").all),
           File => File, Prefix => """", Postfix => """");
      end if;

      Misc_Generate (N, File);
      Gen_Set (N, "Label", "justify", File);
      Gen_Set (N, "Label", "Line_Wrap", "wrap", File);

      if Child_Name /= null then
         Is_Tab := Get_Part (Child_Name.all, 2) = "tab";
         Is_Title := Get_Part (Child_Name.all, 2) = "title";

         if Is_Tab or else Is_Title then

            --  This label is part of a notebook (tab) or a clist (title)

            P   := N.Parent.Child;
            Num := 0;

            while P /= N loop
               S := Get_Field (P, "child_name");

               if S /= null and then S.all = Child_Name.all then
                  Num := Num + 1;
               end if;

               P := P.Next;
            end loop;

            if Is_Tab then
               Add_Package ("Notebook");
               Put (File, "   Set_Tab (");

            elsif Is_Title then
               Add_Package ("Clist");
               Put (File, "   Set_Column_Widget (");
            end if;

            Put_Line (File,
              To_Ada (Top.all) & "." &
              To_Ada (Find_Tag
                (Find_Parent (N.Parent, Get_Part (Child_Name.all, 1)),
                 "name").Value.all) & "," &
              Gint'Image (Num) & ", " &
              To_Ada (Top.all) & "." &
              To_Ada (Get_Field (N, "name").all) & ");");
         end if;
      end if;
   end Label_Generate;

   procedure Layout_Generate (N : Node_Ptr; File : File_Type) is
      Cur : constant String_Ptr := Get_Field (N, "name");
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_layout_get_type");

   begin
      Build_Type;
      Gen_New (N, "Layout", File => File);
      Container_Generate (N, File);

      Gen_Set (N, "Layout", "Size", "area_width", "area_height", "", "",
        File => File);
      Add_Package ("Adjustment");
      Put_Line (File, "   Set_Step_Increment (Get_Hadjustment (" &
        To_Ada (Top.all) & "." & To_Ada (Cur.all) & "), " &
        To_Float (Get_Field (N, "hstep").all) & ");");
      Put_Line (File, "   Set_Step_Increment (Get_Vadjustment (" &
        To_Ada (Top.all) & "." & To_Ada (Cur.all) & "), " &
        To_Float (Get_Field (N, "vstep").all) & ");");
   end Layout_Generate;

   procedure List_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_list_get_type");

   begin
      Build_Type;
      Gen_New (N, "List", File => File);
      Container_Generate (N, File);
      Gen_Set (N, "List", "selection_mode", File => File);
   end List_Generate;

   procedure List_Item_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_list_item_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "List_Item", Get_Field (N, "label").all,
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "List_Item", Get_Field (N, "label").all,
           File => File, Prefix => """", Postfix => """");
      end if;

      Item_Generate (N, File);
   end List_Item_Generate;

   procedure Menu_Generate (N : Node_Ptr; File : File_Type) is
      S  : String_Ptr := Get_Field (N.Parent, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_menu_get_type");

   begin
      Build_Type;
      Gen_New (N, "Menu", File => File);

      if S /= null and then S.all = "GtkMenuItem" then
         Gen_Call_Child (N, null, "Menu_Item", "Set_Submenu", File => File);
         N.Specific_Data.Has_Container := True;
      end if;

      Menu_Shell_Generate (N, File);
   end Menu_Generate;

   procedure Menu_Bar_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_menu_bar_get_type");

   begin
      Build_Type;
      Gen_New (N, "Menu_Bar", File => File);
      Menu_Shell_Generate (N, File);
      Gen_Set (N, "Menu_Bar", "shadow_type", File => File);
   end Menu_Bar_Generate;

   procedure Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      S  : constant String_Ptr := Get_Field (N, "label");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_menu_item_get_type");

   begin
      Build_Type;
      if S = null then
         Gen_New (N, "Menu_Item", File => File);
      else
         if Gettext_Support (N) then
            Gen_New (N, "Menu_Item", S.all,
              File => File, Prefix => "-""", Postfix => """");
         else
            Gen_New (N, "Menu_Item", S.all,
              File => File, Prefix => """", Postfix => """");
         end if;
      end if;

      Item_Generate (N, File);
      Gen_Set (N, "Menu_Item", "right_justify", File);
   end Menu_Item_Generate;

   procedure Misc_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_misc_get_type");

   begin
      Build_Type;
      Widget_Generate (N, File);
      Gen_Set (N, "Misc", "Alignment", "xalign", "yalign", "", "", File,
        Is_Float => True);
      Gen_Set (N, "Misc", "Padding", "xpad", "ypad", "", "", File);
   end Misc_Generate;

   procedure Notebook_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_notebook_get_type");

   begin
      Build_Type;
      Gen_New (N, "Notebook", File => File);
      Container_Generate (N, File);
      Gen_Set (N, "Notebook", "scrollable", File);
      Gen_Set (N, "Notebook", "show_border", File);
      Gen_Set (N, "Notebook", "show_tabs", File);
      Gen_Set (N, "Notebook", "tab_border", File);
      Gen_Set (N, "Notebook", "tab_hborder", File);
      Gen_Set (N, "Notebook", "tab_vborder", File);
      Gen_Set (N, "Notebook", "tab_pos", File);
   end Notebook_Generate;

   procedure Option_Menu_Generate (N : Node_Ptr; File : File_Type) is
      S  : String_Ptr;
      First, Last : Natural;
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_option_menu_get_type");

   begin
      Build_Type;
      Gen_New (N, "Option_Menu", File => File);
      Button_Generate (N, File);

      S := Get_Field (N, "items");

      if S /= null then
         First := S'First;

         Add_Package ("Menu");
         Add_Package ("Menu_Item");
         Put_Line (File, "   Menu.Gtk_New (" &
           To_Ada (Get_Field (N, "name").all) & "_Menu);");

         loop
            Last := Index (S (First .. S'Last), ASCII.LF & "");

            if Last = 0 then
               Last := S'Last + 1;
            end if;

            Put (File, "   Menu_Item.Gtk_New (The_Menu_Item, ");

            if Gettext_Support (N) then
               Put (File, '-');
            end if;

            Put_Line (File, '"' & S (First .. Last - 1) & """);");
            Put_Line (File, "   Menu.Append (" &
              To_Ada (Get_Field (N, "name").all) & "_Menu, The_Menu_Item);");
            Put_Line (File, "   Show (The_Menu_Item);");

            exit when Last >= S'Last;

            First := Last + 1;
         end loop;

         Put_Line (File, "   Option_Menu.Set_Menu");
         Put_Line (File, "     (Gtk_Option_Menu (" &
           To_Ada (Get_Field (Find_Top_Widget (N), "name").all) & "." &
           To_Ada (Get_Field (N, "name").all) & "),");
         Put_Line (File, "      " & To_Ada (Get_Field (N, "name").all) &
           "_Menu);");
      end if;
   end Option_Menu_Generate;

   procedure Paned_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String_Ptr := Get_Field (N, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_paned_get_type");

   begin
      Build_Type;
      Gen_New
        (N, "Paned",
         New_Name => Class (Class'First + 3) & "paned",
         File => File);

      Container_Generate (N, File);
      Gen_Set (N, "Paned", "handle_size", File);
      Gen_Set (N, "Paned", "gutter_size", File);
      Gen_Set (N, "Paned", "position", File);
   end Paned_Generate;

   procedure Pixmap_Generate (N : Node_Ptr; File : File_Type) is
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      Cur : constant String_Ptr := Get_Field (N, "name");
      S   : String_Ptr;
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_pixmap_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         Add_Package ("Pixmap");

         S := Get_Field (N, "filename");

         if S = null then
            S := new String' ("");
         end if;

         Put_Line (File, "   " & To_Ada (Top.all) & "." & To_Ada (Cur.all) &
           " := Create_Pixmap (""" & S.all & """, " & To_Ada (Top.all) &
           ");");
         N.Specific_Data.Created := True;
      end if;

      Misc_Generate (N, File);
   end Pixmap_Generate;

   procedure Preview_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_preview_get_type");

   begin
      Build_Type;
      if Get_Field (N, "type").all = "True" then
         Gen_New (N, "Preview", "Preview_Color", File => File);
         Gen_New (N, "Preview", "Preview_Grayscale", File => File);
      end if;

      Widget_Generate (N, File);
      Gen_Set (N, "Preview", "expand", File);
   end Preview_Generate;

   procedure Progress_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_progress_get_type");

   begin
      Build_Type;
      Widget_Generate (N, File);
      Gen_Set (N, "Progress", "activity_mode", File => File);
      Gen_Set (N, "Progress", "show_text", File => File);
   end Progress_Generate;

   procedure Progress_Bar_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_progress_bar_get_type");

   begin
      Build_Type;
      Gen_New (N, "Progress_Bar", File => File);
      Progress_Generate (N, File);
      Gen_Set (N, "Progress_Bar", "bar_style", File => File);
      Gen_Set (N, "Progress_Bar", "orientation", File => File);
   end Progress_Bar_Generate;

   procedure Radio_Button_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String_Ptr := Get_Field (N, "label");
      Name  : constant String_Ptr := Get_Field (N, "name");
      Top_Widget : Node_Ptr := Find_Top_Widget (N);
      Top   : constant String_Ptr := Get_Field (Top_Widget, "name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_radio_button_get_type");

   begin
      Build_Type;
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         Add_Package ("Radio_Button");
         Put (File, "   Gtk_New (" &
           To_Ada (Top.all) & "." & To_Ada (Name.all) & ", " &
           To_Ada (Get_Field (N.Parent, "name").all) & "_Group");

         if Label /= null then
            Put (File, ", ");

            if Gettext_Support (Top_Widget) then
               Put (File, '-');
            end if;

            Put (File, '"' & Label.all & '"');
         end if;

         Put_Line (File, ");");
         Put_Line (File, "   " & To_Ada (Get_Field (N.Parent, "name").all) &
           "_Group := Group (" & To_Ada (Top.all) & "." &
           To_Ada (Name.all) & ");");
         N.Specific_Data.Created := True;
      end if;

      Check_Button_Generate (N, File);
   end Radio_Button_Generate;

   --  ??? This code is very similar to what is done for Radio Buttons
   --  (see gtk-radio_button.adb), so it would be nice to share the code
   --  Also, this code only takes into account the default case of an unnamed
   --  radio group.

   procedure Radio_Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String_Ptr := Get_Field (N, "label");
      Name  : constant String_Ptr := Get_Field (N, "name");
      Top_Widget : Node_Ptr := Find_Top_Widget (N);
      Top   : constant String_Ptr := Get_Field (Top_Widget, "name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_radio_menu_item_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         Add_Package ("Radio_Menu_Item");
         Put (File, "   Gtk_New (" &
           To_Ada (Top.all) & "." & To_Ada (Name.all) & ", " &
           To_Ada (Get_Field (N.Parent, "name").all) & "_Group");

         if Label /= null then
            Put (File, ", ");

            if Gettext_Support (Top_Widget) then
               Put (File, '-');
            end if;

            Put (File, '"' & Label.all & '"');
         end if;

         Put_Line (File, ");");
         Put_Line (File, "   " & To_Ada (Get_Field (N.Parent, "name").all) &
           "_Group := Group (" & To_Ada (Top.all) & "." &
           To_Ada (Name.all) & ");");
         N.Specific_Data.Created := True;
      end if;

      Check_Menu_Item_Generate (N, File);
   end Radio_Menu_Item_Generate;

   procedure Ruler_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String_Ptr := Get_Field (N, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_ruler_get_type");

   begin
      Build_Type;
      Gen_New (N, "Ruler", "", "", Class (Class'First + 3) & "ruler", File);
      Widget_Generate (N, File);
      Gen_Set (N, "Ruler", "metric", File);
      Gen_Set
        (N, "Ruler", "Range", "lower", "upper", "position", "max_size", File,
         Is_Float => True);
   end Ruler_Generate;

   procedure Scale_Generate (N : Node_Ptr; File : File_Type) is
      S     : String_Ptr;
      Class : constant String_Ptr := Get_Field (N, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_scale_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         S := Get_Field (N, "name");
         Add_Package ("Adjustment");
         Put_Line
           (File, "   Adjustment.Gtk_New (" & To_Ada (S.all) & "_Adj, " &
            To_Float (Get_Field (N, "value").all) & ", " &
            To_Float (Get_Field (N, "lower").all) & ", " &
            To_Float (Get_Field (N, "upper").all) & ", " &
            To_Float (Get_Field (N, "step").all)  & ", " &
            To_Float (Get_Field (N, "page").all)  & ", " &
            To_Float (Get_Field (N, "page_size").all) & ");");

         Gen_New (N, "Scale", S.all & "_Adj", "",
           Class (Class'First + 3) & "scale", File => File);
      end if;

      GRange_Generate (N, File);
      Gen_Set (N, "Scale", "digits", File => File);
      Gen_Set (N, "Scale", "draw_value", File => File);
      Gen_Set (N, "Scale", "value_pos", File => File);
   end Scale_Generate;

   procedure Scrollbar_Generate (N : Node_Ptr; File : File_Type) is
      S     : String_Ptr;
      Class : constant String_Ptr := Get_Field (N, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_scrollbar_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         S := Get_Field (N, "name");
         Add_Package ("Adjustment");

         if Get_Field (N, "class").all = "GtkHScrollbar" then
            Put_Line
              (File, "   Adjustment.Gtk_New (" & To_Ada (S.all) & "_Adj, " &
               To_Float (Get_Field (N, "hvalue").all) & ", " &
               To_Float (Get_Field (N, "hlower").all) & ", " &
               To_Float (Get_Field (N, "hupper").all) & ", " &
               To_Float (Get_Field (N, "hstep").all)  & ", " &
               To_Float (Get_Field (N, "hpage").all)  & ", " &
               To_Float (Get_Field (N, "hpage_size").all) & ");");

            Gen_New (N, "Scrollbar", S.all & "Adj", "",
                     Class (Class'First + 3) & "scrollbar", File => File);
         else
            Put_Line
              (File, "   Adjustment.Gtk_New (" & To_Ada (S.all) & "_Adj, " &
               To_Float (Get_Field (N, "vvalue").all) & ", " &
               To_Float (Get_Field (N, "vlower").all) & ", " &
               To_Float (Get_Field (N, "vupper").all) & ", " &
               To_Float (Get_Field (N, "vstep").all)  & ", " &
               To_Float (Get_Field (N, "vpage").all)  & ", " &
               To_Float (Get_Field (N, "vpage_size").all) & ");");

            Gen_New (N, "Scrollbar", S.all & "Adj", "",
                     Class (Class'First + 3) & "scrollbar", File => File);
         end if;
      end if;

      GRange_Generate (N, File);
   end Scrollbar_Generate;

   procedure Scrolled_Window_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_scrolled_window_get_type");

   begin
      Build_Type;
      Gen_New (N, "Scrolled_Window", File => File);
      Container_Generate (N, File);
      Gen_Set (N, "Scrolled_Window", "Policy", "hscrollbar_policy",
        "vscrollbar_policy", "", "", File);
   end Scrolled_Window_Generate;

   procedure Separator_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String_Ptr := Get_Field (N, "class");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_separator_get_type");

   begin
      Build_Type;
      Gen_New (N, "Separator", "", "",
        Class (Class'First + 3) & "separator", File);
      Widget_Generate (N, File);
   end Separator_Generate;

   procedure Spin_Button_Generate (N : Node_Ptr; File : File_Type) is
      S   : String_Ptr;
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_spin_button_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         S := Get_Field (N, "name");
         Add_Package ("Adjustment");
         Put_Line
           (File, "   Gtk_New (" & To_Ada (S.all) & "_Adj, " &
            To_Float (Get_Field (N, "value").all) & ", " &
            To_Float (Get_Field (N, "lower").all) & ", " &
            To_Float (Get_Field (N, "upper").all) & ", " &
            To_Float (Get_Field (N, "step").all)  & ", " &
            To_Float (Get_Field (N, "page").all)  & ", " &
            To_Float (Get_Field (N, "page_size").all) & ");");
         Add_Package ("Spin_Button");
         Put_Line (File, "   Gtk_New (" & To_Ada (Top.all) & "." &
           To_Ada (S.all) & ", " & To_Ada (S.all) & "_Adj, " &
           To_Float (Get_Field (N, "climb_rate").all) & ", " &
           Get_Field (N, "digits").all & ");");
         N.Specific_Data.Created := True;
      end if;

      GEntry_Generate (N, File);

      Gen_Set (N, "Spin_Button", "numeric", File);
      Gen_Set (N, "Spin_Button", "Snap_To_Ticks", "snap", "", "", "", File);
      Gen_Set (N, "Spin_Button", "update_policy", File);
      Gen_Set (N, "Spin_Button", "value", File, Is_Float => True);
      Gen_Set (N, "Spin_Button", "wrap", File);
   end Spin_Button_Generate;

   procedure Status_Bar_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_statusbar_get_type");

   begin
      Build_Type;
      Gen_New (N, "Status_Bar", File => File);
      Box_Generate (N, File);
   end Status_Bar_Generate;

   procedure Table_Generate (N : Node_Ptr; File : File_Type) is
      P   : Node_Ptr;
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_table_get_type");

   begin
      Build_Type;
      if not N.Specific_Data.Created then
         P := Find_Tag (N.Child, "name");

         if P /= null then
            Add_Package ("Table");
            Put (File, "   Gtk_New (" & To_Ada (Top.all) & "." &
              To_Ada (P.Value.all) & ", " &
              To_Ada (Get_Field (N, "rows").all));
            Put (File, ", " & To_Ada (Get_Field (N, "columns").all));
            Put_Line
              (File, ", " & To_Ada (Get_Field (N, "homogeneous").all) & ");");
            N.Specific_Data.Created := True;
         end if;
      end if;

      Container_Generate (N, File);
      Gen_Set (N, "Table", "Row_Spacings", "row_spacing", File);
      Gen_Set (N, "Table", "Col_Spacings", "column_spacing", File);
   end Table_Generate;

   procedure Text_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_text_get_type");

   begin
      Build_Type;
      Gen_New (N, "Text", File => File);

      Editable_Generate (N, File);

      Gen_Set (N, "Text", "editable", File);
      Gen_Set (N, "Text", "point", File);
      Gen_Set (N, "Text", "word_wrap", File);
   end Text_Generate;

   procedure Toggle_Button_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String_Ptr := Get_Field (N, "label");
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_toggle_button_get_type");

   begin
      Build_Type;
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         if Label = null then
            Gen_New (N, "Toggle_Button", File => File);
         else
            if Gettext_Support (N) then
               Gen_New (N, "Toggle_Button", Label.all,
                 File => File, Prefix => "-""", Postfix => """");
            else
               Gen_New (N, "Toggle_Button", Label.all,
                 File => File, Prefix => """", Postfix => """");
            end if;
         end if;
      end if;

      Button_Generate (N, File);
      Gen_Set (N, "Toggle_Button", "mode", File);
      Gen_Set (N, "Toggle_Button", "active", File);
   end Toggle_Button_Generate;

   Widget_Class : aliased String := "GtkWidget";

   procedure Toolbar_Generate (N : Node_Ptr; File : File_Type) is
      P, Child   : Node_Ptr;
      Top_Widget : Node_Ptr := Find_Top_Widget (N);
      Top  : constant String_Ptr := Get_Field (Top_Widget, "name");
      Cur  : constant String_Ptr := Get_Field (N, "name");
      S, T : String_Ptr;
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_toolbar_get_type");

   begin
      Build_Type;
      Gen_New (N, "Toolbar", Get_Field (N, "orientation").all,
        Get_Field (N, "type").all, File => File);
      Container_Generate (N, File);
      Gen_Set (N, "Toolbar", "space_size", File);
      Gen_Set (N, "Toolbar", "space_style", File);
      Gen_Set (N, "Toolbar", "tooltips", File);
      Gen_Set (N, "Toolbar", "Button_Relief", "relief", File);

      --  Now look for widgets that should be added to this toolbar

      P := N.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            S := Get_Field (P, "class");

            if S.all = "GtkButton" or else S.all = "GtkToggleButton"
              or else S.all = "GtkRadioButton"
            then
               Child := Find_Child (P, "child");

               if Child /= null then
                  T := Get_Field (Child, "new_group");
               else
                  T := null;
               end if;

               if T /= null and then T.all = "True" then
                  Put_Line (File, "   Append_Space (" & To_Ada (Top.all) &
                    "." & To_Ada (Cur.all) & ");");
               end if;

               Put_Line (File, "   " & To_Ada (Top.all) & "." &
                 To_Ada (Get_Field (P, "name").all) &
                 " := Append_Element");
               Put (File, "     (Toolbar => ");

               if Top /= Cur then
                  Put (File, To_Ada (Top.all) & ".");
               end if;

               Put_Line (File, To_Ada (Cur.all) & ",");
               Put (File, "      The_Type => Toolbar_Child_" &
                 S (S'First + 3 .. S'Last));
               S := Get_Field (P, "label");

               if S /= null then
                  Put_Line (File, ",");

                  if Gettext_Support (Top_Widget) then
                     Put (File, "      Text => -""" & S.all & '"');
                  else
                     Put (File, "      Text => """ & S.all & '"');
                  end if;
               end if;

               S := Get_Field (P, "tooltip");

               if S /= null then
                  Put_Line (File, ",");

                  if Gettext_Support (Top_Widget) then
                     Put (File, "      Tooltip_Text => -""" & S.all & '"');
                  else
                     Put (File, "      Tooltip_Text => """ & S.all & '"');
                  end if;
               end if;

               T := Get_Field (P, "icon");

               if T /= null then
                  Add_Package ("Pixmap");
                  Put_Line (File, ",");
                  Put (File, "      Icon => Gtk_Widget (Create_Pixmap (");

                  if Index (T.all, ".") /= 0 then
                     Put (File, '"' & T.all & '"');
                  else
                     Put (File, T.all);
                  end if;

                  Put_Line (File, ", " & To_Ada (Top.all) & ")));");
               else
                  Put_Line (File, ");");
               end if;

               Add_Package ("Widget");
               Gen_Signal (P, File, Widget_Class'Access);

               P.Specific_Data.Created := True;
               P.Specific_Data.Initialized := True;
               P.Specific_Data.Has_Container := True;
            end if;
         end if;

         P := P.Next;
      end loop;
   end Toolbar_Generate;

   procedure Tree_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_tree_get_type");

   begin
      Build_Type;
      Gen_New (N, "Tree", File => File);
      Container_Generate (N, File);
      Gen_Set (N, "Tree", "selection_mode", File);
      Gen_Set (N, "Tree", "view_lines", File);
      Gen_Set (N, "Tree", "view_mode", File);
   end Tree_Generate;

   procedure Tree_Item_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_tree_item_get_type");

   begin
      Build_Type;
      if Gettext_Support (N) then
         Gen_New (N, "Tree_Item", Get_Field (N, "label").all,
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Tree_Item", Get_Field (N, "label").all,
           File => File, Prefix => """", Postfix => """");
      end if;

      Item_Generate (N, File);
   end Tree_Item_Generate;

   procedure Vbutton_Box_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_vbutton_box_get_type");

   begin
      Build_Type;
      Gen_New (N, "Vbutton_Box", File => File);
      Button_Box_Generate (N, File);
   end Vbutton_Box_Generate;

   procedure Viewport_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_viewport_get_type");

   begin
      Build_Type;
      Gen_New (N, "Viewport", File => File);
      Bin_Generate (N, File);
      Gen_Set (N, "Viewport", "shadow_type", File => File);
   end Viewport_Generate;

   procedure Widget_Generate (N : Node_Ptr; File : File_Type) is
   begin
      null;
   end Widget_Generate;

   procedure Window_Generate (N : Node_Ptr; File : File_Type) is
      procedure Build_Type;
      pragma Import (C, Build_Type, "gtk_window_get_type");

   begin
      Build_Type;
      Gen_New (N, "Window", Get_Field (N, "type").all, File => File);
      Bin_Generate (N, File);

      if Gettext_Support (N) then
         Gen_Set (N, "Window", "title", File, "-""", """");
      else
         Gen_Set (N, "Window", "title", File, """", """");
      end if;

      Gen_Set (N, "Window", "Policy", "allow_shrink", "allow_grow",
        "auto_shrink", "", File);
      Gen_Set (N, "Window", "position", File);
      Gen_Set (N, "Window", "modal", File);
      Gen_Set (N, "Window", "Default_Size", "default_width", "default_height",
        "", "", File);
   end Window_Generate;

   procedure End_Generate (N : Node_Ptr; File : File_Type) is
      Child       : Node_Ptr := Find_Tag (N.Child, "child");
      Q           : Node_Ptr;
      Top         : constant Node_Ptr   := Find_Top_Widget (N);
      Top_Name    : constant String_Ptr := Get_Field (Top, "name");
      Cur         : constant String_Ptr := Get_Field (N, "name");
      S           : String_Ptr;
      Flag_Set    : Boolean;
      Use_Default : Boolean;
      First       : Natural;
      Last        : Natural;
      The_First   : Natural;

   begin
      S := Get_Field (Find_Child (Top.Parent, "project"), "use_widget_names");

      if S /= null and then Boolean'Value (S.all) then
         if Gettext_Support (Top) then
            Gen_Set (N, "Widget", "name",
              File => File, Prefix => "-""", Postfix => """");
         else
            Gen_Set (N, "Widget", "name",
              File => File, Prefix => """", Postfix => """");
         end if;
      end if;

      Gen_Set (N, "Widget", "sensitive", File);
      Gen_Set (N, "Widget", "UPosition", "x", "y", "", "", File);
      Gen_Set (N, "Widget", "USize", "width", "height", "", "", File);
      Gen_Set (N, "Widget", "state", File);
      Gen_Set (N, "Widget", "extension_events", File);

      S := Get_Field (N, "can_default");

      if S /= null and then Boolean'Value (S.all) then
         Add_Package ("Object");
         Put (File, "   Set_Flags (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put_Line (File, To_Ada (Cur.all) & ", Can_Default);");
      end if;

      S := Get_Field (N, "has_focus");

      if S /= null and then Boolean'Value (S.all) then
         Put (File, "   Grab_Focus (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put_Line (File, To_Ada (Cur.all) & ");");
      end if;

      S := Get_Field (N, "has_default");

      if S /= null and then Boolean'Value (S.all) then
         Put (File, "   Grab_Default (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put_Line (File, To_Ada (Cur.all) & ");");
      end if;

      S := Get_Field (N, "events");

      if S /= null then
         Put (File, "   Set_Events (");

         if Top_Name /= Cur then
            Put (File, To_Ada (Top_Name.all) & ".");
         end if;

         Put (File, To_Ada (Cur.all) & ", ");

         Flag_Set := False;
         The_First := S'First;

         loop
            Find_Token (S (The_First .. S'Last), To_Set (" |"),
              Ada.Strings.Inside, First, Last);

            exit when Last = 0;

            if Flag_Set then
               Put_Line (File, " or");
            else
               New_Line (File);
               Flag_Set := True;
            end if;

            Put (File, "     " & To_Ada (S (The_First + 4 .. First - 1)));
            The_First := Last + 1;
         end loop;

         if The_First /= S'Last then
            if Flag_Set then
               Put_Line (File, " or");
            else
               New_Line (File);
            end if;

            Put (File, "     " & To_Ada (S (The_First + 4 .. S'Last)));

         elsif not Flag_Set then
            Put (File, "0");
         end if;

         Put_Line (File, ");");
      end if;

      --  ??? Need to find a better way to call Pack_Start

      if not N.Specific_Data.Has_Container and then Child /= null then
         Q := Find_Tag (Child.Child, "pack");

         if Q = null or else Q.Value.all = "GTK_PACK_START" then
            if Get_Field (Child, "fill") /= null then

               --  This widget is part of a Gtk_Box

               Gen_Call_Child (N, Child, "Box", "Pack_Start",
                 "expand", "fill", "padding", File);
               N.Specific_Data.Has_Container := True;

            elsif Get_Field (Child, "left_attach") /= null then

               --  This widget is part of a Gtk_Table

               Add_Package ("Table");
               Put_Line (File, "   Attach (" &
                 To_Ada (Top_Name.all) & "." &
                 To_Ada (Find_Tag
                   (Find_Parent (N.Parent, "Table"), "name").Value.all) &
                 ", " & To_Ada (Top_Name.all) & "." &
                 To_Ada (Cur.all) &
                 ", " & Get_Field (Child, "left_attach").all &
                 ", " & Get_Field (Child, "right_attach").all &
                 ", " & Get_Field (Child, "top_attach").all &
                 ", " & Get_Field (Child, "bottom_attach").all & ",");

               Put (File, "     ");

               Flag_Set := False;

               if Boolean'Value (Get_Field (Child, "xexpand").all) then
                  Put (File, "Expand");
                  Flag_Set := True;
               end if;

               if Boolean'Value (Get_Field (Child, "xshrink").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Shrink");
               end if;

               if Boolean'Value (Get_Field (Child, "xfill").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Fill");
               end if;

               if not Flag_Set then
                  Put (File, "0");
               end if;

               Put (File, ", ");

               Flag_Set := False;

               if Boolean'Value (Get_Field (Child, "yexpand").all) then
                  Put (File, "Expand");
                  Flag_Set := True;
               end if;

               if Boolean'Value (Get_Field (Child, "yshrink").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Shrink");
               end if;

               if Boolean'Value (Get_Field (Child, "yfill").all) then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Fill");
               end if;

               if not Flag_Set then
                  Put_Line (File, "0,");
               else
                  Put_Line (File, ",");
               end if;

               Put_Line (File, "     " & Get_Field (Child, "xpad").all & ", " &
                 Get_Field (Child, "ypad").all & ");");
               N.Specific_Data.Has_Container := True;

            elsif Get_Field (Child, "side") /= null then

               --  This widget is part of a packer

               Add_Package ("Packer");
               S := Get_Field (Child, "use_default");
               Use_Default := S /= null and then Boolean'Value (S.all);

               if Use_Default then
                  Put (File, "   Add_Defaults (");
               else
                  Put (File, "   Add (");
               end if;

               Put_Line (File,
                 To_Ada (Top_Name.all) & "." &
                 To_Ada (Find_Tag
                   (Find_Parent (N.Parent, "Packer"), "name").Value.all) &
                 ", " & To_Ada (Top_Name.all) & "." &
                 To_Ada (Cur.all) &
                 ", " & To_Ada (Get_Field (Child, "side").all) &
                 ", " & To_Ada (Get_Field (Child, "anchor").all) & ",");

               Put (File, "     ");

               Flag_Set := False;
               S := Get_Field (Child, "expand");

               if S /= null and then S.all = "True" then
                  Flag_Set := True;
                  Put (File, "Gtk_Pack_Expand");
               end if;

               S := Get_Field (Child, "xfill");

               if S /= null and then S.all = "True" then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Gtk_Fill_X");
               end if;

               S := Get_Field (Child, "yfill");

               if S /= null and then S.all = "True" then
                  if Flag_Set then
                     Put (File, " or ");
                  else
                     Flag_Set := True;
                  end if;

                  Put (File, "Gtk_Fill_Y");
               end if;

               if not Flag_Set then
                  Put (File, "0");
               end if;

               if not Use_Default then
                  Put_Line (File, ",");
                  Put (File,
                    "     " & Get_Field (Child, "border_width").all &
                    ", " & Get_Field (Child, "xpad").all &
                    ", " & Get_Field (Child, "ypad").all &
                    ", " & Get_Field (Child, "xipad").all &
                    ", " & Get_Field (Child, "yipad").all);
               end if;

               Put_Line (File, ");");
               N.Specific_Data.Has_Container := True;
            end if;

         elsif Q.Value.all = "GTK_PACK_END" then
            if Get_Field (Child, "fill") /= null
              and then Get_Field (N, "child_name").all /= "Dialog:action_area"
            then
               --  This widget is part of a Gtk_Box, but not one of the
               --  internal components of the box

               Gen_Call_Child (N, Child, "Box", "Pack_End",
                               "expand", "fill", "padding", File);
               N.Specific_Data.Has_Container := True;
            end if;
         end if;
      end if;

      Q := Find_Tag (N.Child, "accelerator");

      if Q /= null then
         if not Top.Specific_Data.Has_Accel_Group then
            Add_Package ("Accel_Group");
            Put_Line (File, "   Gtk_New (The_Accel_Group);");
            Put_Line (File, "   Add_Accel_Group (" &
              To_Ada (Top_Name.all) & ", The_Accel_Group);");
            Top.Specific_Data.Has_Accel_Group := True;
         end if;

         Put_Line (File, "   Add_Accelerator (" &
            To_Ada (Top_Name.all) & "." &
            To_Ada (Cur.all) & ", """ &
            Get_Field (Q, "signal").all & """,");
         Add_Package ("Gdk.Types.Keysyms");
         S := Get_Field (Q, "modifiers");
         Put (File, "     The_Accel_Group, " & Get_Field (Q, "key").all);

         if S'Length > 4 and then S (S'First .. S'First + 3) = "GDK_" then
            Put_Line (File, ", Gdk.Types." & To_Ada (S.all) &
              ", Accel_Visible);");
         else
            Put_Line (File, ", " & S.all & ", Accel_Visible);");
         end if;
      end if;

      S := Get_Field (N, "tooltip");

      if S /= null then
         if not Top.Specific_Data.Has_Tooltip then
            Add_Package ("Tooltips");
            Put_Line (File, "   Gtk_New (Tooltips);");
            Top.Specific_Data.Has_Tooltip := True;
         end if;

         if Gettext_Support (N) then
            Put_Line (File, "   Set_Tip (Tooltips, " &
               To_Ada (Top_Name.all) & "." &
               To_Ada (Cur.all) & ", -""" & S.all & """);");

         else
            Put_Line (File, "   Set_Tip (Tooltips, " &
               To_Ada (Top_Name.all) & "." &
               To_Ada (Cur.all) & ", """ & S.all & """);");
         end if;
      end if;

      if not N.Specific_Data.Initialized then
         Gen_Signal (N, File);
      end if;

      if Find_Tag (N.Child, "child_name") = null then
         if not N.Specific_Data.Has_Container then
            S := Get_Field (N.Parent, "class");

            if S /= null then
               if S.all = "GtkFixed" then
                  Gen_Call_Child (N, N, "Fixed",
                    "Put", "x", "y", File => File);

               elsif S.all = "GtkLayout" then
                  Gen_Call_Child (N, N, "Layout",
                    "Put", "x", "y", File => File);

               elsif S.all = "GtkToolbar" then
                  --  ??? Need to handle tooltip
                  Gen_Call_Child (N, null, "Toolbar",
                    "Append_Widget", File => File);

               else
                  Gen_Call_Child (N, null, "Container", "Add", File => File);
               end if;
            end if;

            N.Specific_Data.Has_Container := True;
         end if;
      end if;
   end End_Generate;

end Gtk_Generates;
