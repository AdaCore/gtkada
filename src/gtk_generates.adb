-----------------------------------------------------------------------
--                   Gate - GtkAda Components                        --
--                                                                   --
--   Copyright (C) 1999-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
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
with System;

package body Gtk_Generates is

   use Glib;

   Widget, Widget2 : System.Address;

   function Widget_New
     (T : Glib.GType; Addr : System.Address := System.Null_Address)
      return System.Address;
   pragma Import (C, Widget_New, "gtk_widget_new");

   procedure Widget_Destroy (Widget : System.Address);
   pragma Import (C, Widget_Destroy, "gtk_widget_destroy");

   --------------------------
   -- Accel_Label_Generate --
   --------------------------

   procedure Accel_Label_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_accel_label_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if Gettext_Support (N) then
         Gen_New (N, "Accel_Label", Adjust (Get_Property (N, "label", "")),
           File => File,
           Prefix => "-(""", Postfix => """)");
      else
         Gen_New (N, "Accel_Label", Adjust (Get_Property (N, "label", "")),
           File => File, Prefix => """", Postfix => """");
      end if;

      Widget_Destroy (Widget);
      Label_Generate (N, File);
   end Accel_Label_Generate;

   ------------------------
   -- Alignment_Generate --
   ------------------------

   procedure Alignment_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_alignment_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New
        (N, "Alignment",
         To_Float (Get_Property (N, "xalign", "0.1")),
         To_Float (Get_Property (N, "yalign", "0.1")),
         To_Float (Get_Property (N, "xscale", "0.1")),
         To_Float (Get_Property (N, "xscale", "0.1")), "",
         File => File);
      Widget_Destroy (Widget);
      Bin_Generate (N, File);
   end Alignment_Generate;

   --------------------
   -- Arrow_Generate --
   --------------------

   procedure Arrow_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_arrow_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Arrow",
               Get_Property (N, "arrow_type", "GTK_ARROW_RIGHT"),
               Get_Property (N, "shadow_type", "GTK_SHADOW_OUT"),
               File => File);
      Widget_Destroy (Widget);
      Misc_Generate (N, File);
   end Arrow_Generate;

   ---------------------------
   -- Aspect_Frame_Generate --
   ---------------------------

   procedure Aspect_Frame_Generate (N : Node_Ptr; File : File_Type) is
      S  : String_Ptr;
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_aspect_frame_get_type");

   begin
      Widget := Widget_New (Build_Type);
      S := Get_Property (N, "label");

      if S /= null then
         if Gettext_Support (N) then
            Gen_New (N, "Aspect_Frame", Adjust (S.all),
                     To_Float (Get_Property (N, "xalign", "0.0")),
                     To_Float (Get_Property (N, "yalign", "0.0")),
                     To_Float (Get_Property (N, "ratio", "1")),
                     Get_Property (N, "obey_child", "False"),
                     File, "-(""", """)");
         else
            Gen_New (N, "Aspect_Frame", Adjust (S.all),
                     To_Float (Get_Property (N, "xalign", "0.0")),
                     To_Float (Get_Property (N, "yalign", "0.0")),
                     To_Float (Get_Property (N, "ratio", "1")),
                     Get_Property (N, "obey_child", "False"),
                     File, """", """");
         end if;
      else
         Gen_New (N, "Aspect_Frame", "",
                  To_Float (Get_Property (N, "xalign", "0.0")),
                  To_Float (Get_Property (N, "yalign", "0.0")),
                  To_Float (Get_Property (N, "ratio", "1")),
                  Get_Property (N, "obey_child", "False"),
                  File, """", """");
      end if;

      Widget_Destroy (Widget);
      Frame_Generate (N, File);
   end Aspect_Frame_Generate;

   ------------------
   -- Box_Generate --
   ------------------

   procedure Box_Generate (N : Node_Ptr; File : File_Type) is
      Child_Name : constant Node_Ptr := Find_Tag (N.Child, "child_name");
      Class      : constant String := Get_Attribute (N, "class");
      P, Q       : Node_Ptr;
      function Build_HType return Glib.GType;
      pragma Import (C, Build_HType, "gtk_hbox_get_type");

      function Build_VType return Glib.GType;
      pragma Import (C, Build_VType, "gtk_vbox_get_type");

   begin
      Widget := Widget_New (Build_HType);
      Widget2 := Widget_New (Build_VType);

      if Child_Name = null then
         if not N.Specific_Data.Created then
            P := Find_Tag_With_Attribute (N.Child, "property", "name",
               "homogeneous");
            Q := Find_Tag_With_Attribute (N.Child, "property", "name",
               "spacing");
            if P /= null and Q /= null then
               Gen_New (N, "Box", P.Value.all, Q.Value.all,
                 Class (Class'First + 3) & "box", File);
            end if;
         end if;

      else
         Gen_Child (N, Child_Name, File);
      end if;

      Widget_Destroy (Widget);
      Widget_Destroy (Widget2);
      Container_Generate (N, File);

      if Child_Name /= null then
         Gen_Set (N, "homogeneous", File);
         Gen_Set (N, "spacing", File);
      end if;
   end Box_Generate;

   ---------------------
   -- Button_Generate --
   ---------------------

   procedure Button_Generate (N : Node_Ptr; File : File_Type) is
      Label      : constant Node_Ptr   := Find_Tag_With_Attribute
         (N.Child, "property", "name", "label");
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_button_get_type");

      Use_Stock : constant Boolean :=
        Get_Property (N, "use_stock", "False") = "True";
   begin
      Widget := Widget_New (Build_Type);
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         if Use_Stock then
            Gen_New (N, "Button",
                     Get_Property (N, "label", ""),
                     New_Name => "From_Stock",
                     File     => File,
                     Prefix => """", Postfix => """");

         else
            if Label = null then
               Gen_New (N, "Button", File => File);
            else
               if Gettext_Support (N) then
                  Gen_New (N, "Button", Label.Value.all,
                           File => File,
                           Prefix => "-""", Postfix => """");
               else
                  Gen_New (N, "Button", Label.Value.all,
                           File => File,
                           Prefix => """", Postfix => """");
               end if;
            end if;
         end if;
      end if;

      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "relief", File);
   end Button_Generate;

   -------------------------
   -- Button_Box_Generate --
   -------------------------

   procedure Button_Box_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String := Get_Class (N);
      function Build_Hbuttonbox return Glib.GType;
      pragma Import (C, Build_Hbuttonbox, "gtk_hbutton_box_get_type");
      function Build_Vbuttonbox return Glib.GType;
      pragma Import (C, Build_Vbuttonbox, "gtk_vbutton_box_get_type");

   begin
      if Class = "GtkHButtonBox" then
         Widget := Widget_New (Build_Hbuttonbox);
      else
         Widget := Widget_New (Build_Vbuttonbox);
      end if;

      Widget_Destroy (Widget);
      Box_Generate (N, File);
      Gen_Set (N, "spacing", File);
      Gen_Set (N, "Layout", "layout_style", "", "", "", File);
      Gen_Set (N, "Child_Size",
        "child_min_width", "child_min_height", "", "", File);
      Gen_Set (N, "Child_Ipadding",
        "child_ipad_x", "child_ipad_y", "", "", File);
   end Button_Box_Generate;

   -----------------------
   -- Calendar_Generate --
   -----------------------

   procedure Calendar_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_calendar_get_type");
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top        : constant String := Get_Name (Top_Widget);

      Added : Boolean := False;
   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Calendar", File => File);
      Widget_Destroy (Widget);

      if Get_Property (N, "display_options") /= "" then
         Put (File, "   Display_Options ("
              & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & ", ");
         declare
            S : constant String := Get_Property (N, "display_options");
            P : Natural := S'First;
            T : Natural := S'First;
         begin
            while T <= S'Last loop
               T := T + 1;

               if T > S'Last or else S (T) = '|' then
                  if P + 12 <= S'Last
                    and then S (P .. P + 12) = "GTK_CALENDAR_"
                  then
                     P := P + 13;
                  end if;

                  if T > P then
                     if not Added then
                        Put (File, To_Ada (S (P .. T - 1)));
                        Added := True;
                     else
                        Put (File, " and " & To_Ada (S (P .. T - 1)));
                     end if;
                  end if;

                  P := T + 1;
               end if;

            end loop;
         end;

         Put_Line (File, ");");
      end if;

      Widget_Generate (N, File);
   end Calendar_Generate;

   ---------------------------
   -- Check_Button_Generate --
   ---------------------------

   procedure Check_Button_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String := Get_Property (N, "label", "");
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_check_button_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if not N.Specific_Data.Created then
         if Label = "" then
            Gen_New (N, "Check_Button", File => File);
         else
            if Gettext_Support (N) then
               Gen_New (N, "Check_Button", Label, File => File,
                 Prefix => "-(""", Postfix => """)");
            else
               Gen_New (N, "Check_Button", Label, File => File,
                 Prefix => """", Postfix => """");
            end if;
         end if;
      end if;

      Widget_Destroy (Widget);
      Toggle_Button_Generate (N, File);
   end Check_Button_Generate;

   ------------------------------
   -- Check_Menu_Item_Generate --
   ------------------------------

   procedure Check_Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_check_menu_item_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if Gettext_Support (N) then
         Gen_New (N, "Check_Menu_Item", Get_Property (N, "label", ""),
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Check_Menu_Item", Get_Property (N, "label", ""),
           File => File, Prefix => """", Postfix => """");
      end if;

      Widget_Destroy (Widget);
      Menu_Item_Generate (N, File);
      Gen_Set (N, "active", File);
      Gen_Set (N, "always_show_toggle", File => File);
   end Check_Menu_Item_Generate;

   ------------------------------
   -- Color_Selection_Generate --
   ------------------------------

   procedure Color_Selection_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_color_selection_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Color_Selection", File => File);
      Gen_Set (N, "Update_Policy", "policy", File => File);
      Widget_Destroy (Widget);
      Box_Generate (N, File);
   end Color_Selection_Generate;

   -------------------------------------
   -- Color_Selection_Dialog_Generate --
   -------------------------------------

   procedure Color_Selection_Dialog_Generate
     (N : Node_Ptr; File : File_Type)
   is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_color_selection_dialog_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if Gettext_Support (N) then
         Gen_New (N, "Color_Selection_Dialog", Get_Property (N, "title", ""),
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Color_Selection_Dialog", Get_Property (N, "title", ""),
           File => File, Prefix => """", Postfix => """");
      end if;

      Widget_Destroy (Widget);
      Window_Generate (N, File);
   end Color_Selection_Dialog_Generate;

   --------------------
   -- Combo_Generate --
   --------------------

   procedure Combo_Generate (N : Node_Ptr; File : File_Type) is
      P          : Node_Ptr;
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top        : constant String := Get_Name (Top_Widget);
      Has_Items  : Boolean := False;
      Child : Node_Ptr := Find_Tag (N.Child, "widget");
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_combo_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Combo", File => File);

      --  The child is the entry field associated with the combo box. It only
      --  exists for Glade >= 0.5. Do not generate any "Add"

      if Child /= null then
         Child.Specific_Data.Has_Container := True;
      end if;

      Widget_Destroy (Widget);
      Box_Generate (N, File);

      Gen_Set (N, "value_in_list", File);
      Gen_Set (N, "use_arrows", File,
               Property_Name => "enable_arrow_keys");
      Gen_Set (N, "use_arrows_always", File,
               Property_Name => "enable_arrow_always");
      Gen_Set (N, "case_sensitive", File);

      --  Generate the properties of the entry.

      P := Find_Tag_With_Attribute
        (N.Child, "child", "internal-child", "entry");

      if P /= null and then P.Child /= null then
         Add_Package ("GEntry");
         Add_Package ("Glib.Unicode");

         P := P.Child;

         P.Specific_Data.Created := True;
         P.Specific_Data.Initialized := True;
         P.Specific_Data.Has_Container := True;

         Put_Line
           (File,
            "   Set_Editable (Get_Entry ("
            & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & "), "
            & Get_Property (P, "editable", "True") & ");");
         Put_Line
           (File,
            "   Set_Has_Frame (Get_Entry ("
            & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & "), "
            & Get_Property (P, "has_frame", "True") & ");");
         Put_Line
           (File,
            "   Set_Max_Length (Get_Entry ("
            & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & "), "
            & Get_Property (P, "max_length", "0") & ");");

         if Get_Property (P, "width_chars", "0") /= "0" then
            Put_Line
              (File,
               "   Set_Width_Chars (Get_Entry ("
               & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & "), "
               & Get_Property (P, "width_chars", "0") & ");");
         end if;

         Put_Line
           (File,
            "   Set_Text (Get_Entry ("
            & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & "), -("""
            & Adjust (Get_Property (P, "text", "")) & """));");
         Put_Line
           (File,
            "   Set_Invisible_Char (Get_Entry ("
            & To_Ada (Top) & "." & To_Ada (Get_Name (N))
            & "), UTF8_Get_Char ("""
            & Get_Property (P, "invisible_char", "") & """));");
         Put_Line
           (File,
            "   Set_Has_Frame (Get_Entry ("
            & To_Ada (Top) & "." & To_Ada (Get_Name (N)) & "), "
            & Get_Property (P, "has_frame", "False") & ");");
      end if;

      --  Generate the list of items.

      P := Find_Tag_With_Attribute
        (N.Child, "child", "internal-child", "list");

      if P /= null and then P.Child /= null and then P.Child.Child /= null then
         P := Find_Tag (P.Child.Child, "child");

         if P /= null then
            P := P.Child;
         end if;
      end if;

      while P /= null loop
         Has_Items := True;

         Put (File, "   String_List.Append (" &
              To_Ada (Get_Name (N)) & "_Items, ");

         if Gettext_Support (Top_Widget) then
            Put (File, "-(");
         end if;

         Put (File, '"' & Get_Property (P, "label", ""));

         if Gettext_Support (Top_Widget) then
            Put_Line (File, """));");
         else
            Put_Line (File, """);");
         end if;

         P.Specific_Data.Created := True;
         P.Specific_Data.Initialized := True;
         P.Specific_Data.Has_Container := True;

         P := P.Parent.Next;

         if P /= null then
            P := P.Child;
         end if;
      end loop;

      if Has_Items then
         Put_Line (File, "   Combo.Set_Popdown_Strings (" &
                   To_Ada (Top) & "." &
                   To_Ada (Get_Name (N)) & ", " &
                   To_Ada (Get_Name (N)) & "_Items);");

         Put_Line (File, "   Free_String_List (" &
                   To_Ada (Get_Name (N)) & "_Items);");
      end if;
   end Combo_Generate;

   ------------------------
   -- Container_Generate --
   ------------------------

   procedure Container_Generate (N : Node_Ptr; File : File_Type) is
   begin
      Widget_Generate (N, File);
      Gen_Set (N, "border_width", File);
      Gen_Set (N, "resize_mode", File);
   end Container_Generate;

   --------------------
   -- Curve_Generate --
   --------------------

   procedure Curve_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_curve_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Curve", File => File);
      Widget_Destroy (Widget);
      Drawing_Area_Generate (N, File);
      Gen_Set (N, "curve_type", File => File);
      Gen_Set (N, "Range", "min_x", "max_x", "min_y", "max_y",
        File => File, Is_Float => True);
   end Curve_Generate;

   ---------------------
   -- Dialog_Generate --
   ---------------------

   procedure Dialog_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_dialog_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Dialog", File => File);
      Widget_Destroy (Widget);
      Window_Generate (N, File);
   end Dialog_Generate;

   ---------------------------
   -- Drawing_Area_Generate --
   ---------------------------

   procedure Drawing_Area_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_drawing_area_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Drawing_Area", File => File);
      Widget_Destroy (Widget);
      Widget_Generate (N, File);
   end Drawing_Area_Generate;

   ------------------------
   -- Event_Box_Generate --
   ------------------------

   procedure Event_Box_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_event_box_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Event_Box", File => File);
      Widget_Destroy (Widget);
      Bin_Generate (N, File);
   end Event_Box_Generate;

   -----------------------------
   -- File_Selection_Generate --
   -----------------------------

   procedure File_Selection_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_file_selection_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if Gettext_Support (N) then
         Gen_New (N, "File_Selection", Get_Property (N, "title", ""),
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "File_Selection", Get_Property (N, "title", ""),
           File => File, Prefix => """", Postfix => """");
      end if;

      Gen_Set
        (N, "show_file_op_buttons", File, Property_Name => "show_fileops");
      Widget_Destroy (Widget);
      Window_Generate (N, File);
   end File_Selection_Generate;

   --------------------
   -- Fixed_Generate --
   --------------------

   procedure Fixed_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_fixed_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Fixed", File => File);
      Widget_Destroy (Widget);
      Container_Generate (N, File);
   end Fixed_Generate;

   -----------------------------
   -- Font_Selection_Generate --
   -----------------------------

   procedure Font_Selection_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_font_selection_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Font_Selection", File => File);
      Widget_Destroy (Widget);
      Notebook_Generate (N, File);
   end Font_Selection_Generate;

   ------------------------------------
   -- Font_Selection_Dialog_Generate --
   ------------------------------------

   procedure Font_Selection_Dialog_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_font_selection_dialog_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if Gettext_Support (N) then
         Gen_New (N, "Font_Selection_Dialog", Get_Property (N, "title", ""),
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "Font_Selection_Dialog", Get_Property (N, "title", ""),
           File => File, Prefix => """", Postfix => """");
      end if;

      Widget_Destroy (Widget);
      Window_Generate (N, File);
   end Font_Selection_Dialog_Generate;

   --------------------
   -- Frame_Generate --
   --------------------

   procedure Frame_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_frame_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Frame", File => File);

      Widget_Destroy (Widget);
      Bin_Generate (N, File);
      Gen_Set
        (N, "Label_Align",
         "label_xalign", "label_yalign", "", "", File,
         Is_Float => True);
      Gen_Set (N, "shadow_type", File);
   end Frame_Generate;

   --------------------------
   -- Gamma_Curve_Generate --
   --------------------------

   procedure Gamma_Curve_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_gamma_curve_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Gamma_Curve", File => File);
      Widget_Destroy (Widget);
      Box_Generate (N, File);
      Add_Package ("Curve");
      Put_Line (File, "   Set_Range (Get_Curve (" &
        To_Ada (Get_Name (Find_Top_Widget (N))) & "." &
        To_Ada (Get_Name (N) & "), " &
        To_Float (Get_Property (N, "min_x", "0")) & ", " &
        To_Float (Get_Property (N, "max_x", "1")) & ", " &
        To_Float (Get_Property (N, "min_y", "0")) & ", " &
        To_Float (Get_Property (N, "max_y", "1")) & ");"));
   end Gamma_Curve_Generate;

   ---------------------
   -- GEntry_Generate --
   ---------------------

   procedure GEntry_Generate (N : Node_Ptr; File : File_Type) is
      Child_Name : constant Node_Ptr := Find_Tag (N.Child, "child_name");
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top        : constant String := Get_Name (Top_Widget);
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_entry_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if Child_Name = null then
         Gen_New (N, "GEntry", File => File);
      else
         Gen_Child (N, Child_Name, File);
      end if;

      Widget_Destroy (Widget);
      Editable_Generate (N, File);
      Gen_Set (N, "editable", File);
      Gen_Set (N, "max_length", File);

      if Get_Property (N, "width_chars", "0") /= "0" then
         Gen_Set (N, "width_chars", File);
      end if;

      if Get_Property (N, "has_frame", "True") /= "True" then
         Gen_Set (N, "has_frame", File);
      end if;

      Gen_Set (N, "position", File);

      if Gettext_Support (N) then
         Gen_Set (N, "text", File, "-(""", """)");
      else
         Gen_Set (N, "text", File, """", """");
      end if;

      Gen_Set (N, "visibility", File);

      Add_Package ("Glib.Unicode");

      Put_Line (File, "   Set_Invisible_Char ("
                & To_Ada (Top) & "." & To_Ada (Get_Name (N))
                & ", UTF8_Get_Char ("""
                & Get_Property (N, "invisible_char", "") & """));");
   end GEntry_Generate;

   ---------------------
   -- GRange_Generate --
   ---------------------

   procedure GRange_Generate (N : Node_Ptr; File : File_Type) is begin
      Widget_Generate (N, File);
      Gen_Set (N, "Update_Policy", "policy", File => File);
   end GRange_Generate;

   -------------------------
   -- Handle_Box_Generate --
   -------------------------

   procedure Handle_Box_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_handle_box_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Handle_Box", File => File);
      Widget_Destroy (Widget);
      Bin_Generate (N, File);
      Gen_Set (N, "shadow_type", File);
      Gen_Set (N, "handle_position", File);
      Gen_Set (N, "snap_edge", File);
   end Handle_Box_Generate;

   --------------------------
   -- Hbutton_Box_Generate --
   --------------------------

   procedure Hbutton_Box_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_hbutton_box_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Hbutton_Box", File => File);
      Widget_Destroy (Widget);
      Button_Box_Generate (N, File);
   end Hbutton_Box_Generate;

   --------------------
   -- Image_Generate --
   --------------------

   procedure Image_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_image_get_type");
      Name : constant String := To_Ada (Get_Name (N));
      Top  : constant String := To_Ada (Get_Name (Find_Top_Widget (N)));
      Stock : constant String := Get_Property (N, "stock", "");

   begin
      Widget := Widget_New (Build_Type);
      if not N.Specific_Data.Created then
         Add_Package ("Image");

         if Stock = "" then
            Put_Line
              (File,
               "   Gtk_New (" & Top & "." & Name & " , Pixmaps_Dir & """
               & Get_Property (N, "pixbuf", "") & """);");
         else
            Put_Line
              (File,
               "   Gtk_New (" & Top & "." & Name & " , """
               & Stock & """, Gtk_Icon_Size'Val (" &
               Get_Property (N, "icon_size", "4") & "));");
         end if;
      end if;

      Widget_Destroy (Widget);
      Misc_Generate (N, File);
   end Image_Generate;

   ------------------------------
   -- Image_Menu_Item_Generate --
   ------------------------------

   procedure Image_Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      Label     : constant String := Get_Property (N, "label", "");
      Use_Stock : constant String := Get_Property (N, "use_stock", "False");
      Name : constant String := To_Ada (Get_Name (N));
      Top  : constant String := To_Ada (Get_Name (Find_Top_Widget (N)));
      Image_Node : Node_Ptr;
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_image_menu_item_get_type");

   begin
      Widget := Widget_New (Build_Type);

      if Use_Stock = "True" then
         if not Gettext_Support (N) then
            Gen_New
              (N, "Image_Menu_Item", Label,
               File => File, Prefix => """", Postfix => """",
               New_Name => "From_Stock");
         else
            Gen_New
              (N, "Image_Menu_Item", Label,
               File => File, Prefix => "-""", Postfix => """",
               New_Name => "From_Stock");
         end if;

      else
         if not Gettext_Support (N) then
            Gen_New
              (N, "Image_Menu_Item", Label,
               File => File, Prefix => """", Postfix => """");
         else
            Gen_New
              (N, "Image_Menu_Item", Label,
               File => File, Prefix => "-""", Postfix => """");
         end if;
      end if;

      if Use_Stock /= "True" then
         Image_Node := Find_Tag_With_Attribute
           (N.Child, "child", "internal-child", "image");

         if Image_Node /= null then
            Image_Generate (Image_Node.Child, File);
            Put_Line
              (File, "   Set_Image (" & Top & "." & Name & ", "
               & Top & "." & To_Ada (Get_Name (Image_Node.Child)) & ");");
         end if;
      end if;

      Widget_Destroy (Widget);
      Item_Generate (N, File);
   end Image_Menu_Item_Generate;

   ---------------------------
   -- Input_Dialog_Generate --
   ---------------------------

   procedure Input_Dialog_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_input_dialog_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Input_Dialog", File => File);
      Widget_Destroy (Widget);
      Dialog_Generate (N, File);
   end Input_Dialog_Generate;

   --------------------
   -- Label_Generate --
   --------------------

   procedure Label_Generate (N : Node_Ptr; File : File_Type) is
      Parent     : constant Node_Ptr := N.Parent.Parent;
      Name       : constant String := To_Ada (Get_Name (N));
      S          : String_Ptr;
      Top        : constant String := To_Ada (Get_Name (Find_Top_Widget (N)));
      P          : Node_Ptr;
      Packing    : Node_Ptr := null;
      Num        : Gint;

      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_label_get_type");

   begin
      Widget := Widget_New (Build_Type);

      P := Find_Tag_With_Attribute (N.Child, "property", "name", "label");

      if P /= null then
         if Gettext_Support (N) then
            Gen_New (N, "Label", P.Value.all,
               File => File, Prefix => "-(""", Postfix => """)");
         else
            Gen_New (N, "Label", P.Value.all,
               File => File, Prefix => """", Postfix => """");
         end if;
      end if;

      Widget_Destroy (Widget);
      Misc_Generate (N, File);

      Gen_Set (N, "justify", File);
      Gen_Set (N, "line_wrap", File, Property_Name => "wrap");
      Gen_Set (N, "selectable", File);
      Gen_Set (N, "use_markup", File);
      Gen_Set (N, "use_underline", File);

      Packing := Find_Tag (N, "packing");

      if Parent /= null then
         if Packing /= null then
            S := Get_Property (Packing, "type");

            if S /= null and then S.all = "tab" then
               --  This label is part of a notebook (tab) or a clist (title)

               Num := 0;

               P := Parent.Child;
               P := Find_Tag (P, "child");
               P := P.Child;

               while P /= null and then P /= N loop
                  --  Count the widgets that are not tabs.
                  Packing := Find_Tag (P, "packing");

                  if Packing /= null then
                     S := Get_Property (Packing, "type");

                     if S = null or else S.all /= "tab" then
                        Num := Num + 1;
                     end if;
                  end if;

                  if P.Parent.Next /= null then
                     P := P.Parent.Next.Child;
                  else
                     P := null;
                  end if;
               end loop;

               Num := Num - 1;

               Add_Package ("Notebook");
               Put (File, "   Set_Tab (");

               Put_Line
                 (File,
                  Top & "." & To_Ada (Get_Name (Parent)) & "," &
                  Gint'Image (Num) & ", " &
                  Top & "." & Name & ");");

            elsif S /= null and then S.all = "label_item" then
               --  This label is the title of a frame

               Put_Line
                 (File,
                  "   Set_Label_Widget (" &
                  Top & "." & To_Ada (Get_Name (Parent)) & "," &
                  Top & "." & Name & ");");

            end if;
         end if;
      end if;
   end Label_Generate;

   ---------------------
   -- Layout_Generate --
   ---------------------

   procedure Layout_Generate (N : Node_Ptr; File : File_Type) is
      Name       : constant String := To_Ada (Get_Name (N));
      Top        : constant String := To_Ada (Get_Name (Find_Top_Widget (N)));
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_layout_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Layout", File => File);
      Widget_Destroy (Widget);
      Container_Generate (N, File);

      Gen_Set (N, "Size", "area_width", "area_height", "", "",
               File => File);
      Add_Package ("Adjustment");
      Put_Line (File, "   Set_Step_Increment (Get_Hadjustment (" &
                Top & "." & Name & "), " &
                To_Float (Get_Property (N, "hstep", "0.1")) & ");");
      Put_Line (File, "   Set_Step_Increment (Get_Vadjustment (" &
                Top & "." & Name & "), " &
                To_Float (Get_Property (N, "vstep", "0.1")) & ");");
   end Layout_Generate;

   -------------------
   -- List_Generate --
   -------------------

   procedure List_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_list_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "List", File => File);
      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "selection_mode", File => File);
   end List_Generate;

   ------------------------
   -- List_Item_Generate --
   ------------------------

   procedure List_Item_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_list_item_get_type");

   begin
      Widget := Widget_New (Build_Type);

      Add_Package ("List_Item");

      if Gettext_Support (N) then
         Gen_New (N, "List_Item", Get_Property (N, "label"),
           File => File, Prefix => "-""", Postfix => """");
      else
         Gen_New (N, "List_Item", Get_Property (N, "label"),
           File => File, Prefix => """", Postfix => """");
      end if;

      Widget_Destroy (Widget);
      Item_Generate (N, File);
   end List_Item_Generate;

   -------------------
   -- Menu_Generate --
   -------------------

   procedure Menu_Generate (N : Node_Ptr; File : File_Type) is
      S  : constant String := Get_Class (N.Parent);
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_menu_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Menu", File => File);

      if S = "GtkMenuItem" then
         Gen_Call_Child
           (N, null, N.Parent, "Menu_Item", "Set_Submenu", File => File);
         N.Specific_Data.Has_Container := True;
      end if;

      Widget_Destroy (Widget);
      Menu_Shell_Generate (N, File);
   end Menu_Generate;

   -----------------------
   -- Menu_Bar_Generate --
   -----------------------

   procedure Menu_Bar_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_menu_bar_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Menu_Bar", File => File);
      Widget_Destroy (Widget);
      Menu_Shell_Generate (N, File);
      Gen_Set (N, "shadow_type", File => File);
   end Menu_Bar_Generate;

   ------------------------
   -- Menu_Item_Generate --
   ------------------------

   procedure Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      S  : constant String := Get_Property (N, "label", "");
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_menu_item_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if S = "" then
         Gen_New (N, "Menu_Item", File => File);
      else
         if Gettext_Support (N) then
            Gen_New (N, "Menu_Item", S,
                     File => File, Prefix => "-(""", Postfix => """)",
                     New_Name => "With_Mnemonic");
         else
            Gen_New (N, "Menu_Item", S,
                     File => File, Prefix => """", Postfix => """",
                     New_Name => "With_Mnemonic");
         end if;
      end if;

      Widget_Destroy (Widget);
      Item_Generate (N, File);
      Gen_Set (N, "right_justify", File);
   end Menu_Item_Generate;

   -------------------
   -- Misc_Generate --
   -------------------

   procedure Misc_Generate (N : Node_Ptr; File : File_Type) is
   begin
      Widget_Generate (N, File);
      Gen_Set (N, "Alignment", "xalign", "yalign", "", "", File,
        Is_Float => True);
      Gen_Set (N, "Padding", "xpad", "ypad", "", "", File);
   end Misc_Generate;

   -----------------------
   -- Notebook_Generate --
   -----------------------

   procedure Notebook_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_notebook_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Notebook", File => File);
      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "scrollable", File);
      Gen_Set (N, "show_border", File);
      Gen_Set (N, "show_tabs", File);
      Gen_Set (N, "tab_border", File);
      Gen_Set (N, "tab_hborder", File);
      Gen_Set (N, "tab_vborder", File);
      Gen_Set (N, "tab_pos", File);
   end Notebook_Generate;

   --------------------------
   -- Option_Menu_Generate --
   --------------------------

   procedure Option_Menu_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_option_menu_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Option_Menu", File => File);
      Widget_Destroy (Widget);
      Button_Generate (N, File);
   end Option_Menu_Generate;

   --------------------
   -- Paned_Generate --
   --------------------


   procedure Paned_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String := Get_Class (N);
      function Build_Vpaned return Glib.GType;
      pragma Import (C, Build_Vpaned, "gtk_vpaned_get_type");
      function Build_Hpaned return Glib.GType;
      pragma Import (C, Build_Hpaned, "gtk_hpaned_get_type");

   begin
      if Class = "GtkVPaned" then
         Widget := Widget_New (Build_Vpaned);
      else
         Widget := Widget_New (Build_Hpaned);
      end if;

      Gen_New
        (N, "Paned",
         New_Name => Class (Class'First + 3) & "paned",
         File => File);

      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "handle_size", File);
      Gen_Set (N, "gutter_size", File);
      Gen_Set (N, "position", File);
   end Paned_Generate;

   ---------------------
   -- Pixmap_Generate --
   ---------------------

   procedure Pixmap_Generate (N : Node_Ptr; File : File_Type) is
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");
      Cur : constant String_Ptr := Get_Field (N, "name");
      S   : String_Ptr;
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_pixmap_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if not N.Specific_Data.Created then
         Add_Package ("Pixmap");

         S := Get_Field (N, "filename");

         if S = null then
            S := new String'("");
         end if;

         Put_Line (File, "   " & To_Ada (Top.all) & "." & To_Ada (Cur.all) &
           " := Create_Pixmap (""" & S.all & """, " & To_Ada (Top.all) &
           ");");
         N.Specific_Data.Created := True;
      end if;

      Widget_Destroy (Widget);
      Misc_Generate (N, File);
   end Pixmap_Generate;

   -----------------------
   -- Progress_Generate --
   -----------------------

   procedure Progress_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_progress_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Widget_Destroy (Widget);
      Widget_Generate (N, File);
      Gen_Set (N, "activity_mode", File => File);
      Gen_Set (N, "show_text", File => File);
   end Progress_Generate;

   ---------------------------
   -- Progress_Bar_Generate --
   ---------------------------

   procedure Progress_Bar_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_progress_bar_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Progress_Bar", File => File);
      Gen_Set (N, "fraction", File => File, Is_Float => True);
      Gen_Set (N, "pulse_step", File => File, Is_Float => True);

      if Gettext_Support (N) then
         Gen_Set (N, "text", File, "-""", """");
      else
         Gen_Set (N, "text", File, """", """");
      end if;

      Widget_Destroy (Widget);
      Progress_Generate (N, File);
      Gen_Set (N, "bar_style", File => File);
      Gen_Set (N, "orientation", File => File);
   end Progress_Bar_Generate;

   ---------------------------
   -- Radio_Button_Generate --
   ---------------------------

   procedure Radio_Button_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String := Get_Property (N, "label", "");
      Name  : constant String := Get_Attribute (N, "id");
      Group : constant String := Get_Property (N, "group", "");
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top   : constant String := Get_Attribute (Top_Widget, "id");
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_radio_button_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         Add_Package ("Radio_Button");

         if Group = "" then
            Put (File, "   Gtk_New ("
                 & To_Ada (Top) & "." & To_Ada (Name)
                 & ", null");
         else
            Put (File, "   Gtk_New ("
                 & To_Ada (Top) & "." & To_Ada (Name)
                 & ", " & To_Ada (Top) & "." & To_Ada (Group));
         end if;

         if Label /= "" then
            Put (File, ", ");

            if Gettext_Support (Top_Widget) then
               Put (File, "-(");
            end if;

            Put (File, '"' & Adjust (Label) & '"');

            if Gettext_Support (Top_Widget) then
               Put (File, ')');
            end if;
         end if;

         Put_Line (File, ");");
         N.Specific_Data.Created := True;
      end if;

      Widget_Destroy (Widget);
      Check_Button_Generate (N, File);
   end Radio_Button_Generate;

   ------------------------------
   -- Radio_Menu_Item_Generate --
   ------------------------------

   procedure Radio_Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String := Get_Property (N, "label", "");
      Name  : constant String := Get_Name (N);
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top   : constant String := Get_Name (Top_Widget);
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_radio_menu_item_get_type");

      Group_Name : constant String := To_Ada (Get_Name (N.Parent.Parent));
   begin
      Widget := Widget_New (Build_Type);
      if not N.Specific_Data.Created then
         Add_Package ("Radio_Menu_Item");

         if Group_Name /= "" then
            Put (File, "   Gtk_New (" &
                 To_Ada (Top) & "." & To_Ada (Name) & ", " &
                 Group_Name & "_Group");
         else
            Put (File, "   Gtk_New (" &
                 To_Ada (Top) & "." & To_Ada (Name) & ", " &
                 "Default_Group");
         end if;


         if Label /= "" then
            Put (File, ", ");

            if Gettext_Support (Top_Widget) then
               Put (File, '-');
            end if;

            Put (File, '"' & Adjust (Label) & '"');
         end if;

         Put_Line (File, ");");

         if Group_Name /= "" then
            Put_Line (File, "   " & Group_Name &
                      "_Group := Group (" & To_Ada (Top) & "." &
                      To_Ada (Name) & ");");
         else
            Put_Line (File, "   " & "Default_Group := Group (" &
                      To_Ada (Top) & "." &
                      To_Ada (Name) & ");");
         end if;
         N.Specific_Data.Created := True;
      end if;

      Widget_Destroy (Widget);
      Check_Menu_Item_Generate (N, File);
   end Radio_Menu_Item_Generate;

   --------------------
   -- Ruler_Generate --
   --------------------

   procedure Ruler_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String := Get_Class (N);
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_ruler_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Ruler", "", "", Class (Class'First + 3) & "ruler", File);
      Widget_Destroy (Widget);
      Widget_Generate (N, File);
      Gen_Set (N, "metric", File);
      Gen_Set
        (N, "Range", "lower", "upper", "position", "max_size", File,
         Is_Float => True);
   end Ruler_Generate;

   --------------------
   -- Scale_Generate --
   --------------------

   procedure Scale_Generate (N : Node_Ptr; File : File_Type) is
      Name  : constant String := Get_Name (N);
      Class : constant String := Get_Class (N);
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top   : constant String := Get_Name (Top_Widget);
      function Build_Vscale return Glib.GType;
      pragma Import (C, Build_Vscale, "gtk_vscale_get_type");
      function Build_Hscale return Glib.GType;
      pragma Import (C, Build_Hscale, "gtk_hscale_get_type");

   begin
      if Class = "GtkVScale" then
         Widget := Widget_New (Build_Vscale);
      else
         Widget := Widget_New (Build_Hscale);
      end if;

      if not N.Specific_Data.Created then
         Add_Package ("Adjustment");
         declare
            A : constant String :=
              Get_Property (N, "adjustment", "1 0 100 1 10 10") & " ";
            K : Natural := A'First - 1;
            P : Natural := A'First;
         begin
            Put (File, "   Gtk_New (" & Name & "_Adj");
            for J in 1 .. 6 loop
               P := K + 1;
               K := Index (A (P .. A'Last), " ");
               Put (File, ", " & To_Float (A (P .. K - 1)));
            end loop;

            Put_Line (File, ");");
         end;

         Add_Package ("Scale");

         Put_Line (File, "   Gtk_New_" & Class (Class'First + 3) & "scale ("
                   & Top & "." & Name & ", " & Name & "_Adj);");
         N.Specific_Data.Created := True;
      end if;

      Widget_Destroy (Widget);
      GRange_Generate (N, File);
      Gen_Set (N, "digits", File => File);
      Gen_Set (N, "draw_value", File => File);
      Gen_Set (N, "value_pos", File => File);
   end Scale_Generate;

   ------------------------
   -- Scrollbar_Generate --
   ------------------------

   procedure Scrollbar_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String := Get_Class (N);
      Name  : constant String := To_Ada (Get_Name (N));

      function Build_Vscrollbar return Glib.GType;
      pragma Import (C, Build_Vscrollbar, "gtk_vscrollbar_get_type");
      function Build_Hscrollbar return Glib.GType;
      pragma Import (C, Build_Hscrollbar, "gtk_hscrollbar_get_type");

   begin
      if Class = "GtkVScrollbar" then
         Widget := Widget_New (Build_Vscrollbar);
      else
         Widget := Widget_New (Build_Hscrollbar);
      end if;

      if not N.Specific_Data.Created then
         Add_Package ("Adjustment");

         declare
            A : constant String :=
              Get_Property (N, "adjustment", "1 0 100 1 10 10") & " ";
            K : Natural := A'First - 1;
            P : Natural := A'First;
         begin
            Put (File, "   Adjustment.Gtk_New (" & Name & "_Adj");
            for J in 1 .. 6 loop
               P := K + 1;
               K := Index (A (P .. A'Last), " ");
               Put (File, ", " & To_Float (A (P .. K - 1)));
            end loop;

            Put_Line (File, ");");
         end;

         Gen_New (N, "Scrollbar", Name & "Adj", "",
                     Class (Class'First + 3) & "scrollbar", File => File);
      end if;

      Widget_Destroy (Widget);
      GRange_Generate (N, File);
   end Scrollbar_Generate;

   ------------------------------
   -- Scrolled_Window_Generate --
   ------------------------------

   procedure Scrolled_Window_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_scrolled_window_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Scrolled_Window", File => File);
      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "Policy", "hscrollbar_policy",
               "vscrollbar_policy", "", "", File);
      Gen_Set (N, "Shadow_Type", "shadow_type", File);
   end Scrolled_Window_Generate;

   ------------------------
   -- Separator_Generate --
   ------------------------

   procedure Separator_Generate (N : Node_Ptr; File : File_Type) is
      Class : constant String := Get_Class (N);
      function Build_Hseparator return Glib.GType;
      pragma Import (C, Build_Hseparator, "gtk_hseparator_get_type");
      function Build_Vseparator return Glib.GType;
      pragma Import (C, Build_Vseparator, "gtk_vseparator_get_type");

   begin
      if Class = "GtkHSeparator" then
         Widget := Widget_New (Build_Hseparator);
      else
         Widget := Widget_New (Build_Vseparator);
      end if;

      Gen_New (N, "Separator", "", "",
        Class (Class'First + 3) & "separator", File);
      Widget_Destroy (Widget);
      Widget_Generate (N, File);
   end Separator_Generate;

   ----------------------------------
   -- Separator_Menu_Item_Generate --
   ----------------------------------

   procedure Separator_Menu_Item_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_separator_menu_item_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Separator_Menu_Item", File => File);
      Widget_Destroy (Widget);
   end Separator_Menu_Item_Generate;

   --------------------------
   -- Spin_Button_Generate --
   --------------------------

   procedure Spin_Button_Generate (N : Node_Ptr; File : File_Type) is
      Top : constant String := To_Ada (Get_Name (Find_Top_Widget (N)));
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_spin_button_get_type");
      Name : constant String := To_Ada (Get_Name (N));
   begin
      Widget := Widget_New (Build_Type);
      if not N.Specific_Data.Created then
         Add_Package ("Adjustment");
         declare
            A : constant String :=
              Get_Property (N, "adjustment", "1 0 100 1 10 10") & " ";
            K : Natural := A'First - 1;
            P : Natural := A'First;
         begin
            Put (File, "   Gtk_New (" & Name & "_Adj");
            for J in 1 .. 6 loop
               P := K + 1;
               K := Index (A (P .. A'Last), " ");
               Put (File, ", " & To_Float (A (P .. K - 1)));
            end loop;

            Put_Line (File, ");");
         end;

         Add_Package ("Spin_Button");

         Put_Line (File, "   Gtk_New (" & Top & "." &
                   Name & ", " & Name & "_Adj, " &
                   To_Float (Get_Property (N, "climb_rate", "1.0")) & ", " &
                   Get_Property (N, "digits", "0") & ");");
         N.Specific_Data.Created := True;
      end if;

      Widget_Destroy (Widget);
      GEntry_Generate (N, File);

      Gen_Set (N, "numeric", File);
      Gen_Set (N, "Snap_To_Ticks", "snap", "", "", "", File);
      Gen_Set (N, "update_policy", File);
      Gen_Set (N, "value", File, Is_Float => True);
      Gen_Set (N, "wrap", File);
   end Spin_Button_Generate;

   -------------------------
   -- Status_Bar_Generate --
   -------------------------

   procedure Status_Bar_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_statusbar_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Status_Bar", File => File);
      Widget_Destroy (Widget);
      Box_Generate (N, File);
   end Status_Bar_Generate;

   --------------------
   -- Table_Generate --
   --------------------

   procedure Table_Generate (N : Node_Ptr; File : File_Type) is
      Name     : constant String := To_Ada (Get_Attribute (N, "id"));
      Top      : constant Node_Ptr := Find_Top_Widget (N);
      Top_Name : constant String := To_Ada (Get_Attribute (Top, "id"));
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_table_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if not N.Specific_Data.Created
        and then Name /= ""
      then
         Add_Package ("Table");
         Put (File, "   Gtk_New (" & Top_Name & "." &
                 Name & ", " &
                 To_Ada (Get_Property (N, "n_rows").all));
         Put (File, ", " & To_Ada (Get_Property (N, "n_columns").all));
         Put_Line
           (File,
            ", " & To_Ada (Get_Property (N, "homogeneous").all) & ");");
         N.Specific_Data.Created := True;
      end if;

      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "Row_Spacings", "row_spacing", File);
      Gen_Set (N, "Col_Spacings", "column_spacing", File);
   end Table_Generate;

   ------------------------
   -- Text_View_Generate --
   ------------------------

   procedure Text_View_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_text_view_get_type");

      --  Make sure the buffer is also initialized, since some signals are
      --  redirected to it.
      function Text_Buffer_New (Table : System.Address) return System.Address;
      pragma Import (C, Text_Buffer_New, "gtk_text_buffer_new");
      Buffer : constant System.Address :=
        Text_Buffer_New (System.Null_Address);
      pragma Unreferenced (Buffer);

      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top  : constant String := Get_Name (Top_Widget);
      Cur  : constant String := Get_Name (N);

   begin

      Widget := Widget_New (Build_Type);
      Gen_New (N, "Text_View", File => File);
      Widget_Destroy (Widget);

      Gen_Set (N, "editable", File);
      Gen_Set (N, "justification", File);
      Gen_Set (N, "wrap_mode", File);
      Gen_Set (N, "cursor_visible", File);
      Gen_Set (N, "pixels_above_lines", File);
      Gen_Set (N, "pixels_below_lines", File);
      Gen_Set (N, "pixels_inside_wrap", File);
      Gen_Set (N, "left_margin", File);
      Gen_Set (N, "right_margin", File);
      Gen_Set (N, "indent", File);

      Add_Package ("Text_Buffer");
      Add_Package ("Text_Iter");

      Put_Line (File, "   declare");
      Put_Line (File, "      Iter : Gtk_Text_Iter;");
      Put_Line (File, "   begin");
      Put      (File, "      Get_Iter_At_Line (Get_Buffer (");
      Put_Line (File, To_Ada (Top) & "." & To_Ada (Cur) & "), Iter, 0);");
      Put      (File, "      Insert (Get_Buffer (");
      Put_Line (File, To_Ada (Top) & "." & To_Ada (Cur) & "), Iter,");
      Put      (File, "         -(""");
      Put_Line (File, Adjust (Get_Property (N, "text", "")) & """));");
      Put_Line (File, "   end;");
   end Text_View_Generate;

   ----------------------------
   -- Toggle_Button_Generate --
   ----------------------------

   procedure Toggle_Button_Generate (N : Node_Ptr; File : File_Type) is
      Label : constant String := Get_Property (N, "label", "");
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_toggle_button_get_type");

   begin
      Widget := Widget_New (Build_Type);
      if N.Specific_Data.Initialized then
         return;
      end if;

      if not N.Specific_Data.Created then
         if Label = "" then
            Gen_New (N, "Toggle_Button", File => File);
         else
            if Gettext_Support (N) then
               Gen_New (N, "Toggle_Button", Label,
                 File => File, Prefix => "-""", Postfix => """");
            else
               Gen_New (N, "Toggle_Button", Label,
                 File => File, Prefix => """", Postfix => """");
            end if;
         end if;
      end if;

      Widget_Destroy (Widget);
      Button_Generate (N, File);
      Gen_Set (N, "mode", File);
      Gen_Set (N, "active", File);
      Gen_Set (N, "inconsistent", File);
      Gen_Set (N, "relief", File);
      Gen_Set (N, "use_underline", File);
   end Toggle_Button_Generate;

   ----------------------
   -- Toolbar_Generate --
   ----------------------

   procedure Toolbar_Generate (N : Node_Ptr; File : File_Type) is
      P, Child   : Node_Ptr;
      Top_Widget : constant Node_Ptr := Find_Top_Widget (N);
      Top  : constant String := Get_Name (Top_Widget);
      Cur  : constant String := Get_Name (N);
      S, T : String_Ptr;
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_toolbar_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Toolbar",
               To_Ada (Get_Property
                         (N, "orientation", "GTK_ORIENTATION_HORIZONTAL")),
               To_Ada (Get_Property (N, "toolbar_style", "GTK_TOOLBAR_BOTH")),
               File => File);
      Widget_Destroy (Widget);
      Container_Generate (N, File);
      Gen_Set (N, "space_size", File);
      Gen_Set (N, "space_style", File);
      Gen_Set (N, "tooltips", File);
      Gen_Set (N, "Button_Relief", "relief", File);

      --  Now look for widgets that should be added to this toolbar


      P := Find_Child (N, "child");
      P := P.Child;

      while P /= null loop
         if P.Tag.all = "widget" then
            declare
               Child_Class : constant String := Get_Class (P);
            begin
               if Child_Class = "GtkButton"
                 or else Child_Class = "GtkToggleButton"
                 or else Child_Class = "GtkRadioButton"
               then
                  Child := Find_Child (P, "child");

                  if Get_Property (Child, "new_group") = "True" then
                     Put_Line (File, "   Append_Space (" & To_Ada (Top) &
                               "." & To_Ada (Cur) & ");");
                  end if;

                  if Get_Property (P, "use_stock", "") = "True" then
                     Put_Line (File, "   " & To_Ada (Top) & "." &
                               To_Ada (Get_Name (P)) &
                               " := Insert_Stock");

                     Put (File, "     (Toolbar  => ");

                     if Top /= Cur then
                        Put (File, To_Ada (Top) & ".");
                     end if;

                     Put_Line (File, To_Ada (Cur) & ",");

                     Put (File, "      Stock_Id => """);
                     Put_Line (File, Get_Property (P, "label", "") & """,");

                     Put (File, "      Tooltip_Text => """);
                     Put_Line
                       (File, Get_Property (P, "tooltip", "") & """,");

                     Put (File, "      Tooltip_Private_Text => """);
                     Put_Line
                       (File, Get_Property (P, "tooltip", "") & """);");

                  else
                     Put_Line (File, "   " & To_Ada (Top) & "." &
                               To_Ada (Get_Name (P)) &
                               " := Gtk_Button (Append_Element");
                     Put (File, "     (Toolbar => ");

                     if Top /= Cur then
                        Put (File, To_Ada (Top) & ".");
                     end if;

                     Put_Line (File, To_Ada (Cur) & ",");

                     Put (File, "      The_Type => Toolbar_Child_" &
                       Child_Class
                         (Child_Class'First + 3 .. Child_Class'Last));
                     S := Get_Property (P, "label");

                     if S /= null then
                        Put_Line (File, ",");

                        if Gettext_Support (Top_Widget) then
                           Put (File,
                                "      Text => -(""" & Adjust (S.all) & """)");
                        else
                           Put (File,
                                "      Text => """ & Adjust (S.all) & """");
                        end if;
                     end if;

                     S := Get_Property (P, "tooltip");

                     if S /= null then
                        Put_Line (File, ",");

                        if Gettext_Support (Top_Widget) then
                           Put (File,
                                "      Tooltip_Text => -("""
                                & Adjust (S.all) & """)");
                        else
                           Put (File,
                                "      Tooltip_Text => """
                                & Adjust (S.all) & '"');
                        end if;
                     end if;

                     T := Get_Property (P, "icon");

                     if T /= null then
                        Add_Package ("Pixmap");
                        Put_Line (File, ",");
                        Put
                          (File, "      Icon => Gtk_Widget (Create_Pixmap (");

                        if Index (T.all, ".") /= 0 then
                           Put (File, '"' & T.all & '"');
                        else
                           Put (File, T.all);
                        end if;

                        Put_Line (File, ", " & To_Ada (Top) & ")));");
                     else
                        Put_Line (File, "));");
                     end if;
                  end if;

                  Add_Package ("Widget");
                  Gen_Signal (P, File, "GtkWidget");

                  P.Specific_Data.Created := True;
                  P.Specific_Data.Initialized := True;
                  P.Specific_Data.Has_Container := True;
               end if;
            end;
         end if;

         P := P.Parent.Next;

         if P /= null then
            P := P.Child;
         end if;
      end loop;
   end Toolbar_Generate;

   ------------------------
   -- Tree_View_Generate --
   ------------------------

   procedure Tree_View_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_tree_view_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Tree_View", File => File);
      Widget_Destroy (Widget);

      Gen_Set (N, "headers_visible", File);
      Gen_Set (N, "rules_hint", File);
      Gen_Set (N, "reorderable", File);
      Gen_Set (N, "enable_search", File);
   end Tree_View_Generate;

   --------------------------
   -- Vbutton_Box_Generate --
   --------------------------

   procedure Vbutton_Box_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_vbutton_box_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Vbutton_Box", File => File);
      Widget_Destroy (Widget);
      Button_Box_Generate (N, File);
   end Vbutton_Box_Generate;

   -----------------------
   -- Viewport_Generate --
   -----------------------

   procedure Viewport_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_viewport_get_type");

   begin
      Widget := Widget_New (Build_Type);
      Gen_New (N, "Viewport", File => File);
      Widget_Destroy (Widget);
      Bin_Generate (N, File);
      Gen_Set (N, "shadow_type", File => File);
   end Viewport_Generate;

   ---------------------
   -- Widget_Generate --
   ---------------------

   procedure Widget_Generate (N : Node_Ptr; File : File_Type) is
      pragma Unreferenced (N);
      pragma Unreferenced (File);
   begin
      null;
   end Widget_Generate;

   ---------------------
   -- Window_Generate --
   ---------------------

   procedure Window_Generate (N : Node_Ptr; File : File_Type) is
      function Build_Type return Glib.GType;
      pragma Import (C, Build_Type, "gtk_window_get_type");

      P : Node_Ptr;
   begin
      Widget := Widget_New (Build_Type);

      P := Find_Tag_With_Attribute (N.Child, "property", "name", "type");
      Gen_New (N, "Window", P.Value.all, File => File);
      Widget_Destroy (Widget);
      Bin_Generate (N, File);

      if Gettext_Support (N) then
         Gen_Set (N, "title", File, "-""", """");
      else
         Gen_Set (N, "title", File, """", """");
      end if;

      Gen_Set (N, "Policy", "allow_shrink", "allow_grow",
        "auto_shrink", "", File);
      Gen_Set (N, "position", File, Property_Name => "window_position");
      Gen_Set (N, "modal", File);
      Gen_Set (N, "Default_Size", "default_width", "default_height",
        "", "", File);
   end Window_Generate;

   ------------------
   -- End_Generate --
   ------------------

   procedure End_Generate
      (Project : Node_Ptr; N : Node_Ptr; File : File_Type)
   is
      Packing      : constant Node_Ptr := Find_Tag (N, "packing");
      Q            : Node_Ptr;
      Top          : constant Node_Ptr := Find_Top_Widget (N);
      Top_Name     : constant String := To_Ada (Get_Attribute (Top, "id"));
      Cur          : constant String := To_Ada (Get_Attribute (N, "id"));
      Parent       : constant Node_Ptr := N.Parent.Parent;
      Parent_Name  : constant String := To_Ada (Get_Attribute (Parent, "id"));
      S            : String_Ptr;
      Flag_Set     : Boolean;
      First        : Natural;
      Last         : Natural;
      The_First    : Natural;
      Parent_Class : constant String := Get_Class (Parent);

   begin
      S := Get_Field (Find_Child (Project, "glade-project"),
         "use_widget_names");

      if S /= null and then Boolean'Value (S.all) then
         if Gettext_Support (N) then
            Gen_Set (N, "name",
              File => File, Prefix => "-""", Postfix => """");
         else
            Gen_Set (N, "name",
              File => File, Prefix => """", Postfix => """");
         end if;
      end if;

      Gen_Set (N, "sensitive", File);
      Gen_Set (N, "UPosition", "x", "y", "", "", File);
      Gen_Set (N, "USize", "width", "height", "", "", File);
      Gen_Set (N, "state", File);
      Gen_Set (N, "extension_events", File);

      S := Get_Property (N, "can_default");

      if S /= null and then Boolean'Value (S.all) then
         Add_Package ("Object");
         Put (File, "   Set_Flags (");

         if Top_Name /= Cur then
            Put (File, Top_Name & ".");
         end if;

         Put_Line (File, Cur & ", Can_Default);");
      end if;

      S := Get_Property (N, "has_focus");

      if S /= null and then Boolean'Value (S.all) then
         Put (File, "   Grab_Focus (");

         if Top_Name /= Cur then
            Put (File, Top_Name & ".");
         end if;

         Put_Line (File, Cur & ");");
      end if;

      S := Get_Property (N, "has_default");

      if S /= null and then Boolean'Value (S.all) then
         Put (File, "   Grab_Default (");

         if Top_Name /= Cur then
            Put (File, Top_Name & ".");
         end if;

         Put_Line (File, Cur & ");");
      end if;

      S := Get_Field (N, "events");

      if S /= null then
         Put (File, "   Set_Events (");

         if Top_Name /= Cur then
            Put (File, Top_Name & ".");
         end if;

         Put (File, Cur & ", ");

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

      --  Look for a "packing" sibling node.

      if not N.Specific_Data.Has_Container and then Packing /= null then
         --  The following depends on the class of the parent.

         if Parent_Class = "GtkTable" then
            Put (File,
                 "   Attach" & ASCII.LF &
                 "     (" & Top_Name & "." & Parent_Name & "," & ASCII.LF &
                 "       " & Top_Name & "." & Cur & ",");

            Put (File, "      Left_Attach  => ");
            S := Get_Property (Packing, "left_attach");

            if S = null then
               Put_Line (File, "0,");
            else
               Put_Line (File, S.all & ",");
            end if;

            Put (File, "      Right_Attach  => ");
            S := Get_Property (Packing, "right_attach");

            if S = null then
               Put_Line (File, "0,");
            else
               Put_Line (File, S.all & ",");
            end if;

            Put (File, "      Top_Attach  => ");
            S := Get_Property (Packing, "top_attach");

            if S = null then
               Put_Line (File, "0,");
            else
               Put_Line (File, S.all & ",");
            end if;

            Put (File, "      Bottom_Attach  => ");
            S := Get_Property (Packing, "bottom_attach");

            if S = null then
               Put_Line (File, "0");
            else
               Put_Line (File, S.all & ",");
            end if;

            S := Get_Property (Packing, "x_options");

            if S /= null and then S.all /= "" then
               Put (File, "      Xoptions  => ");

               if S.all = "expand|shrink" then
                  Put_Line (File, "Expand,");
               else
                  Put_Line (File, To_Ada (S.all) & ",");
               end if;
            end if;

            S := Get_Property (Packing, "y_options");

            if S /= null and then S.all /= "" then
               Put (File, "      Yoptions  => ");

               if S.all = "expand|shrink" then
                  Put_Line (File, "Expand,");
               else
                  Put_Line (File, To_Ada (S.all) & ",");
               end if;
            end if;

            Put (File, "      Xpadding  => ");
            S := Get_Property (Packing, "x_padding");

            if S = null then
               Put_Line (File, "0,");
            else
               Put_Line (File, S.all & ",");
            end if;

            Put (File, "      Ypadding  => ");
            S := Get_Property (Packing, "y_padding");

            if S = null then
               Put_Line (File, "0);");
            else
               Put_Line (File, S.all & ");");
            end if;

         elsif Parent_Class = "GtkVBox"
           or else Parent_Class = "GtkHBox"
           or else Parent_Class = "GtkHButtonBox"
           or else Parent_Class = "GtkVButtonBox"
         then
            S := Get_Property (Packing, "pack_type");

            --  Put the procedure call.

            if S = null or else S.all = "GTK_PACK_START" then
               Put
                 (File, "   Pack_Start" & ASCII.LF &
                  "     (");
            elsif S.all = "GTK_PACK_END" then
               Put
                 (File, "   Pack_End" & ASCII.LF &
                  "     (");
            end if;

            --  Put the name of the parent.

            if Get_Attribute (Parent.Parent, "internal-child") = "" then
               Put_Line (File, Top_Name & "." & Parent_Name & ",");
            else
               Put (File, "Get_"
                    & To_Ada (Get_Attribute (Parent.Parent, "internal-child"))
                    & " (");

               if Parent.Parent.Parent = Top then
                  Put_Line (File, Top_Name & "),");
               else
                  Put_Line (File, Top_Name & "."
                            & To_Ada
                              (Get_Name (Parent.Parent.Parent))
                            & "),");
               end if;
            end if;

            --  Put the name of the child.

            Put (File, "      " & Top_Name & "." & Cur);
            Put_Line (File, ",");

            --  Put the Fill, Expand, Padding attributes.

            Put (File, "      Expand  => ");
            S := Get_Property (Packing, "expand");

            if S = null then
               Put_Line (File, "False,");
            else
               Put_Line (File, S.all & ",");
            end if;

            Put (File, "      Fill    => ");
            S := Get_Property (Packing, "fill");

            if S = null then
               Put_Line (File, "False,");
            else
               Put_Line (File, S.all & ",");
            end if;

            Put (File, "      Padding => ");
            S := Get_Property (Packing, "padding");

            if S = null then
               Put_Line (File, "0);");
            else
               Put_Line (File, S.all & ");");
            end if;

         elsif Parent_Class = "GtkNotebook" then
            --  If the item is not a Tab, add it as a page.

            S := Get_Property (Packing, "type");

            if S = null or else S.all /= "tab" then
               Put (File, "   Append_Page (");
               Put (File, Top_Name & "." & Parent_Name & ", ");
               Put_Line (File, Top_Name & "." & Cur & ");");

               Put (File, "   Set_Tab_Label_Packing (");
               Put (File, Top_Name & "." & Parent_Name & ", ");
               Put (File, Top_Name & "." & Cur & ", ");
               Put (File, To_Ada
                      (Get_Property (Packing, "tab_expand", "False")) & ", ");
               Put (File, To_Ada
                      (Get_Property (Packing, "tab_fill", "False")) & ", ");
               Put_Line (File, To_Ada
                           (Get_Property
                              (Packing, "tab_pack", "Pack_Start")) & ");");
            end if;


         elsif Parent_Class = "GtkVPaned"
           or else Parent_Class = "GtkHPaned"
         then
            --  Check if we are adding the first or the second child.

            Q := Find_Tag (Parent.Child, "child");

            if Q.Child /= null and then Q.Child = N then
               Put (File, "   Pack1 (");
            else
               Put (File, "   Pack2 (");
            end if;

            Put (File, Top_Name & "." & Parent_Name & ", ");
            Put (File, Top_Name & "." & Cur & ", ");

            Put (File, Get_Property (Packing, "resize", "True") & ", ");
            Put_Line (File, Get_Property (Packing, "shrink", "True") & ");");


         elsif Parent_Class = "GtkFixed"
           or else Parent_Class = "GtkLayout"
         then
            Put_Line (File, "   Put (" & Top_Name & "." & Parent_Name & ", "
                      & Top_Name & "." & Cur & ", "
                      & Get_Property (Packing, "x", "0") & ", "
                      & Get_Property (Packing, "y", "0") & ");");
         end if;

         N.Specific_Data.Has_Container := True;
      end if;

      Q := Find_Tag (N.Child, "accelerator");

      if Q /= null then
         if not Top.Specific_Data.Has_Accel_Group then
            Add_Package ("Accel_Group");
            Put_Line (File, "   Gtk_New (The_Accel_Group);");
            Put_Line (File, "   Add_Accel_Group (" &
               Top_Name & ", The_Accel_Group);");
            Top.Specific_Data.Has_Accel_Group := True;
         end if;

         Put_Line (File, "   Add_Accelerator (" &
                   Top_Name & "." &
                   Cur & ", """ &
                   Get_Attribute (Q, "signal") & """,");
         Add_Package ("Gdk.Types.Keysyms");
         declare
            M : constant String := Get_Attribute (Q, "modifiers");
         begin
            Put (File, "     The_Accel_Group, "
                 & "Gdk.Types.keysyms.GDK_" & Get_Attribute (Q, "key"));

            if M'Length > 4 and then M (M'First .. M'First + 3) = "GDK_" then
               Put_Line (File, ", Gdk.Types." & To_Ada (M) &
                         ", Accel_Visible);");

            else
               Put_Line (File, ", " & M & ", Accel_Visible);");
            end if;
         end;
      end if;

      S := Get_Property (N, "tooltip");

      if S /= null then
         if not Top.Specific_Data.Has_Tooltip then
            Add_Package ("Tooltips");
            Put_Line (File, "   Gtk_New (Tooltips);");
            Top.Specific_Data.Has_Tooltip := True;
         end if;

         if Gettext_Support (N) then
            Put_Line (File, "   Set_Tip (Tooltips, " &
               Top_Name & "." &
               Cur & ", -""" & S.all & """);");

         else
            Put_Line (File, "   Set_Tip (Tooltips, " &
                Top_Name & "." &
                Cur & ", """ & S.all & """);");
         end if;
      end if;

      if not N.Specific_Data.Initialized then
         Gen_Signal (N, File);
      end if;

      if Find_Tag (N.Child, "child_name") = null then
         if not N.Specific_Data.Has_Container then
            declare
               S : constant String := Get_Class (Parent);
            begin

               if S /= "" then
                  if S = "GtkFixed" then
                     Gen_Call_Child (N, N, Parent, "Fixed",
                                     "Put", "x", "y", File => File);

                  elsif S = "GtkLayout" then
                     Gen_Call_Child (N, N, Parent, "Layout",
                                  "Put", "x", "y", File => File);

                  elsif S = "GtkToolbar" then
                     --  ??? Need to handle tooltip
                     Gen_Call_Child (N, null, Parent, "Toolbar",
                                  "Append_Widget", File => File);

                  elsif S = "GtkMenu"
                    or else S = "GtkMenuBar"
                    or else S = "GtkMenuShell"
                  then
                     Gen_Call_Child
                       (N, null, Parent, "Menu_Shell", "Append", File => File);

                  elsif S = "GtkMenuItem"
                    or else S = "GtkSeparatorMenuItem"
                    or else S = "GtkImageMenuItem"
                  then
                     Gen_Call_Child
                       (N, null, Parent,
                        "Menu_Item", "Set_Submenu", File => File);

                  elsif S = "GtkOptionMenu" then
                     Gen_Call_Child
                       (N, null, Parent,
                        "Option_Menu", "Set_Menu", File => File);

                  elsif S = "GtkHButtonBox" or else S = "GtkVButtonBox"
                    or else S = "GtkVBox" or else S = "GtkHBox"
                  then
                     if Get_Attribute (Parent.Parent, "internal-child")
                       = ""
                     then
                        Gen_Call_Child
                          (N, null, Parent,
                           "Container", "Pack_Start", File => File);
                     else
                        Add_Package ("Box");

                        Put (File, "   Pack_Start ("
                             & "Get_" & To_Ada
                               (Get_Attribute
                                  (Parent.Parent, "internal-child"))
                             & " (");

                        if Parent.Parent.Parent.Parent.Parent = Top then
                           Put (File, Top_Name & "), ");

                        else
                           Put (File, Top_Name & "."
                                & To_Ada
                                  (Get_Name
                                     (Parent.Parent.Parent.Parent.Parent))
                                & "), ");
                        end if;

                        Put_Line (File, Top_Name & "." & Cur & ");");
                     end if;

                  elsif S = "GtkList" then
                     null;

                  else
                     Gen_Call_Child
                       (N, null, Parent, "Container", "Add", File => File);
                  end if;
               end if;

               N.Specific_Data.Has_Container := True;
            end;
         end if;
      end if;
   end End_Generate;

end Gtk_Generates;
