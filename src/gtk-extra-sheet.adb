-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Gtk.Enums; use Gtk.Enums;
with System;
with Interfaces.C.Strings;
with Unchecked_Conversion;

package body Gtk.Extra.Sheet is

   -----------------------
   -- Sheet_Flag_Is_Set --
   -----------------------

   function Sheet_Flag_Is_Set (Sheet : access Gtk_Sheet_Record;
                               Flag  : Guint16)
                              return Boolean
   is
      function Internal (Sheet : System.Address; Flag : Guint16)
                        return Gint;
      pragma Import (C, Internal, "ada_gtk_extra_sheet_flag_is_set");
   begin
      return Internal (Get_Object (Sheet), Flag) /= 0;
   end Sheet_Flag_Is_Set;

   ---------------------
   -- Sheet_Set_Flags --
   ---------------------

   procedure Sheet_Set_Flags  (Sheet : access Gtk_Sheet_Record;
                               Flags : Guint16)
   is
      procedure Internal (Sheet : System.Address; Flags : Guint16);
      pragma Import (C, Internal, "ada_gtk_extra_sheet_set_flags");
   begin
      Internal (Get_Object (Sheet), Flags);
   end Sheet_Set_Flags;

   -----------------------
   -- Sheet_Unset_Flags --
   -----------------------

   procedure Sheet_Unset_Flags  (Sheet  : access Gtk_Sheet_Record;
                                 Flags : Guint16)
   is
      procedure Internal (Sheet : System.Address; Flags : Guint16);
      pragma Import (C, Internal, "ada_gtk_extra_sheet_unset_flags");
   begin
      Internal (Get_Object (Sheet), Flags);
   end Sheet_Unset_Flags;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Sheet   : out Gtk_Sheet;
                      Rows    : in Guint;
                      Columns : in Guint;
                      Title   : in String := "";
                      Entry_Type : in Gtk_Type := GType_Invalid)
   is
   begin
      Sheet := new Gtk_Sheet_Record;
      Initialize (Sheet, Rows, Columns, Title, Entry_Type);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Sheet   : access Gtk_Sheet_Record'Class;
      Rows    : in Guint;
      Columns : in Guint;
      Title   : in String := "";
      Entry_Type : in Gtk_Type := GType_Invalid)
   is
      function Internal
        (Rows    : in Guint;
         Columns : in Guint;
         Title   : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_sheet_new");

      function Internal2
        (Rows      : in Guint;
         Columns    : in Guint;
         Title      : in System.Address;
         Entry_Type : in Gtk_Type) return System.Address;
      pragma Import (C, Internal2, "gtk_sheet_new_with_custom_entry");

      S : aliased String := Title & ASCII.NUL;
      Sa : System.Address := S'Address;

   begin
      if Title = "" then
         Sa := System.Null_Address;
      end if;

      if Entry_Type /= GType_Invalid then
         Set_Object (Sheet, Internal2 (Rows, Columns, Sa, Entry_Type));
      else
         Set_Object (Sheet, Internal (Rows, Columns, Sa));
      end if;
   end Initialize;

   ---------------------
   -- Gtk_New_Browser --
   ---------------------

   procedure Gtk_New_Browser
     (Sheet   : out Gtk_Sheet;
      Rows    : in Guint;
      Columns : in Guint;
      Title   : in String := "") is
   begin
      Sheet := new Gtk_Sheet_Record;
      Initialize_Browser (Sheet, Rows, Columns, Title);
   end Gtk_New_Browser;

   ------------------------
   -- Initialize_Browser --
   ------------------------

   procedure Initialize_Browser (Sheet   : access Gtk_Sheet_Record'Class;
                                 Rows    : in Guint;
                                 Columns : in Guint;
                                 Title   : in String := "")
   is
      function Internal
        (Rows    : in Guint;
         Columns : in Guint;
         Title   : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_sheet_new_browser");

      S : aliased String := Title & ASCII.NUL;
      Sa : System.Address := S'Address;

   begin
      if Title = "" then
         Sa := System.Null_Address;
      end if;

      Set_Object (Sheet, Internal (Rows, Columns, Sa));
   end Initialize_Browser;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
     (Sheet      : access Gtk_Sheet_Record;
      Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
        (Sheet : in System.Address; Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_set_hadjustment");

   begin
      Internal (Get_Object (Sheet), Get_Object (Adjustment));
   end Set_Hadjustment;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Sheet      : access Gtk_Sheet_Record;
       Adjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal (Sheet      : in System.Address;
                          Adjustment : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_set_vadjustment");
   begin
      Internal (Get_Object (Sheet), Get_Object (Adjustment));
   end Set_Vadjustment;

   ------------------
   -- Change_Entry --
   ------------------

   procedure Change_Entry (Sheet      : access Gtk_Sheet_Record;
                           Entry_Type : in Gtk_Type)
   is
      procedure Internal (Sheet      : in System.Address;
                          Entry_Type : in Gtk_Type);
      pragma Import (C, Internal, "gtk_sheet_change_entry");
   begin
      Internal (Get_Object (Sheet), Entry_Type);
   end Change_Entry;

   ---------------
   -- Get_Entry --
   ---------------

   function Get_Entry (Sheet  : access Gtk_Sheet_Record)
                      return Gtk.Widget.Gtk_Widget
   is
      function Internal (Sheet  : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_sheet_get_entry");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Sheet)), Stub));
   end Get_Entry;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Sheet  : access Gtk_Sheet_Record) return Sheet_State is
      function Internal (Sheet  : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_sheet_get_state");
   begin
      return Sheet_State'Val (Internal (Get_Object (Sheet)));
   end Get_State;

   -----------------------
   -- Get_Visible_Range --
   -----------------------

   procedure Get_Visible_Range
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : out Gtk_Sheet_Range)
   is
      procedure Internal
        (Sheet : in System.Address; The_Range : access Gtk_Sheet_Range);
      pragma Import (C, Internal, "gtk_sheet_get_visible_range");

      R : aliased Gtk_Sheet_Range;
   begin
      Internal (Get_Object (Sheet), R'Access);
      The_Range := R;
   end Get_Visible_Range;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode (Sheet : access Gtk_Sheet_Record;
                                 Mode  : in Gtk.Enums.Gtk_Selection_Mode)
   is
      procedure Internal
        (Sheet : System.Address;
         Mode  : Gtk.Enums.Gtk_Selection_Mode);
      pragma Import (C, Internal, "gtk_sheet_set_selection_mode");

   begin
      Internal (Get_Object (Sheet), Mode);
   end Set_Selection_Mode;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title (Sheet : access Gtk_Sheet_Record;
                        Title : in String)
   is
      procedure Internal (Sheet : in System.Address;
                          Title : in String);
      pragma Import (C, Internal, "gtk_sheet_set_title");
   begin
      Internal (Get_Object (Sheet), Title & ASCII.NUL);
   end Set_Title;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_freeze");
   begin
      Internal (Get_Object (Sheet));
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_thaw");
   begin
      Internal (Get_Object (Sheet));
   end Thaw;

   ----------------------
   -- Set_Column_Title --
   ----------------------

   procedure Set_Column_Title (Sheet  : access Gtk_Sheet_Record;
                               Column : in Gint;
                               Title  : in String)
   is
      procedure Internal (Sheet  : in System.Address;
                          Column : in Gint;
                          Title  : in String);
      pragma Import (C, Internal, "gtk_sheet_set_column_title");
   begin
      Internal (Get_Object (Sheet), Column, Title & ASCII.NUL);
   end Set_Column_Title;

   -------------------
   -- Set_Row_Title --
   -------------------

   procedure Set_Row_Title (Sheet : access Gtk_Sheet_Record;
                            Row   : in Gint;
                            Title : in String)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint;
                          Title : in String);
      pragma Import (C, Internal, "gtk_sheet_set_row_title");
   begin
      Internal (Get_Object (Sheet), Row, Title & ASCII.NUL);
   end Set_Row_Title;

   --------------------------
   -- Row_Button_Add_Label --
   --------------------------

   procedure Row_Button_Add_Label (Sheet : access Gtk_Sheet_Record;
                                   Row   : in Gint;
                                   Label : in String)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint;
                          Label : in String);
      pragma Import (C, Internal, "gtk_sheet_row_button_add_label");
   begin
      Internal (Get_Object (Sheet), Row, Label & ASCII.NUL);
   end Row_Button_Add_Label;

   -----------------------------
   -- Column_Button_Add_Label --
   -----------------------------

   procedure Column_Button_Add_Label (Sheet  : access Gtk_Sheet_Record;
                                      Column : in Gint;
                                      Label  : in String)
   is
      procedure Internal (Sheet  : in System.Address;
                          Column : in Gint;
                          Label  : in String);
      pragma Import (C, Internal, "gtk_sheet_column_button_add_label");
   begin
      Internal (Get_Object (Sheet), Column, Label & ASCII.NUL);
   end Column_Button_Add_Label;

   ------------------------
   -- Row_Button_Justify --
   ------------------------

   procedure Row_Button_Justify
      (Sheet         : access Gtk_Sheet_Record;
       Row           : in Gint;
       Justification : in Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Sheet         : System.Address;
         Row           : Gint;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_sheet_row_button_justify");

   begin
      Internal (Get_Object (Sheet), Row, Justification);
   end Row_Button_Justify;

   ---------------------------
   -- Column_Button_Justify --
   ---------------------------

   procedure Column_Button_Justify
      (Sheet         : access Gtk_Sheet_Record;
       Column        : in Gint;
       Justification : in Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
         (Sheet         : System.Address;
          Column        : Gint;
          Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_sheet_column_button_justify");

   begin
      Internal (Get_Object (Sheet), Column, Justification);
   end Column_Button_Justify;

   ------------
   -- Moveto --
   ------------

   procedure Moveto (Sheet     : access Gtk_Sheet_Record;
                     Row       : in Gint;
                     Column    : in Gint;
                     Row_Align : in Gfloat;
                     Col_Align : in Gfloat)
   is
      procedure Internal (Sheet     : in System.Address;
                          Row       : in Gint;
                          Column    : in Gint;
                          Row_Align : in Gfloat;
                          Col_Align : in Gfloat);
      pragma Import (C, Internal, "gtk_sheet_moveto");
   begin
      Internal (Get_Object (Sheet), Row, Column, Row_Align, Col_Align);
   end Moveto;

   --------------------------
   -- Set_Row_Titles_Width --
   --------------------------

   procedure Set_Row_Titles_Width (Sheet : access Gtk_Sheet_Record;
                                   Width : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Width : in Guint);
      pragma Import (C, Internal, "gtk_sheet_set_row_titles_width");
   begin
      Internal (Get_Object (Sheet), Width);
   end Set_Row_Titles_Width;

   ------------------------------
   -- Set_Column_Titles_Height --
   ------------------------------

   procedure Set_Column_Titles_Height (Sheet  : access Gtk_Sheet_Record;
                                       Height : in Guint)
   is
      procedure Internal (Sheet  : in System.Address;
                          Height : in Guint);
      pragma Import (C, Internal, "gtk_sheet_set_column_titles_height");
   begin
      Internal (Get_Object (Sheet), Height);
   end Set_Column_Titles_Height;

   ------------------------
   -- Show_Column_Titles --
   ------------------------

   procedure Show_Column_Titles (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_show_column_titles");
   begin
      Internal (Get_Object (Sheet));
   end Show_Column_Titles;

   ---------------------
   -- Show_Row_Titles --
   ---------------------

   procedure Show_Row_Titles (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_show_row_titles");
   begin
      Internal (Get_Object (Sheet));
   end Show_Row_Titles;

   ------------------------
   -- Hide_Column_Titles --
   ------------------------

   procedure Hide_Column_Titles (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_hide_column_titles");
   begin
      Internal (Get_Object (Sheet));
   end Hide_Column_Titles;

   ---------------------
   -- Hide_Row_Titles --
   ---------------------

   procedure Hide_Row_Titles (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_hide_row_titles");
   begin
      Internal (Get_Object (Sheet));
   end Hide_Row_Titles;

   -----------------------------
   -- Columns_Set_Sensitivity --
   -----------------------------

   procedure Columns_Set_Sensitivity (Sheet     : access Gtk_Sheet_Record;
                                      Sensitive : in Boolean)
   is
      procedure Internal
         (Sheet     : in System.Address;
          Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_sheet_columns_set_sensitivity");
   begin
      Internal (Get_Object (Sheet), Boolean'Pos (Sensitive));
   end Columns_Set_Sensitivity;

   ----------------------------
   -- Column_Set_Sensitivity --
   ----------------------------

   procedure Column_Set_Sensitivity (Sheet     : access Gtk_Sheet_Record;
                                     Column    : in Gint;
                                     Sensitive : in Boolean)
   is
      procedure Internal (Sheet     : in System.Address;
                          Column    : in Gint;
                          Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_sheet_column_set_sensitivity");
   begin
      Internal (Get_Object (Sheet), Column, Boolean'Pos (Sensitive));
   end Column_Set_Sensitivity;

   --------------------------
   -- Rows_Set_Sensitivity --
   --------------------------

   procedure Rows_Set_Sensitivity (Sheet     : access Gtk_Sheet_Record;
                                   Sensitive : in Boolean)
   is
      procedure Internal (Sheet     : in System.Address;
                          Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_sheet_rows_set_sensitivity");
   begin
      Internal (Get_Object (Sheet), Boolean'Pos (Sensitive));
   end Rows_Set_Sensitivity;

   -------------------------
   -- Row_Set_Sensitivity --
   -------------------------

   procedure Row_Set_Sensitivity (Sheet     : access Gtk_Sheet_Record;
                                  Row       : in Gint;
                                  Sensitive : in Boolean)
   is
      procedure Internal (Sheet     : in System.Address;
                          Row       : in Gint;
                          Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_sheet_row_set_sensitivity");
   begin
      Internal (Get_Object (Sheet), Row, Boolean'Pos (Sensitive));
   end Row_Set_Sensitivity;

   ---------------------------
   -- Column_Set_Visibility --
   ---------------------------

   procedure Column_Set_Visibility (Sheet   : access Gtk_Sheet_Record;
                                    Column  : in Gint;
                                    Visible : in Boolean)
   is
      procedure Internal (Sheet   : in System.Address;
                          Column  : in Gint;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_sheet_column_set_visibility");
   begin
      Internal (Get_Object (Sheet), Column, Boolean'Pos (Visible));
   end Column_Set_Visibility;

   ---------------------------------
   -- Column_Label_Set_Visibility --
   ---------------------------------

   procedure Column_Label_Set_Visibility (Sheet   : access Gtk_Sheet_Record;
                                          Column  : in Gint;
                                          Visible : in Boolean := True)
   is
      procedure Internal (Sheet   : in System.Address;
                          Column  : in Gint;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_sheet_column_label_set_visibility");
   begin
      Internal (Get_Object (Sheet), Column, Boolean'Pos (Visible));
   end Column_Label_Set_Visibility;

   -----------------------------------
   -- Columns_Labels_Set_Visibility --
   -----------------------------------

   procedure Columns_Labels_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Visible : Boolean := True)
   is
      procedure Internal (Sheet   : in System.Address;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_sheet_columns_labels_set_visibility");
   begin
      Internal (Get_Object (Sheet), Boolean'Pos (Visible));
   end Columns_Labels_Set_Visibility;

   ------------------------
   -- Row_Set_Visibility --
   ------------------------

   procedure Row_Set_Visibility (Sheet   : access Gtk_Sheet_Record;
                                 Row     : in Gint;
                                 Visible : in Boolean)
   is
      procedure Internal (Sheet   : in System.Address;
                          Row     : in Gint;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_sheet_row_set_visibility");
   begin
      Internal (Get_Object (Sheet), Row, Boolean'Pos (Visible));
   end Row_Set_Visibility;

   ------------------------------
   -- Row_Label_Set_Visibility --
   ------------------------------

   procedure Row_Label_Set_Visibility (Sheet   : access Gtk_Sheet_Record;
                                       Row     : in Gint;
                                       Visible : in Boolean := True)
   is
      procedure Internal (Sheet   : in System.Address;
                          Row  : in Gint;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_sheet_row_label_set_visibility");
   begin
      Internal (Get_Object (Sheet), Row, Boolean'Pos (Visible));
   end Row_Label_Set_Visibility;

   --------------------------------
   -- Rows_Labels_Set_Visibility --
   --------------------------------

   procedure Rows_Labels_Set_Visibility
     (Sheet   : access Gtk_Sheet_Record;
      Visible : Boolean := True)
   is
      procedure Internal (Sheet   : in System.Address;
                          Visible : in Gint);
      pragma Import (C, Internal, "gtk_sheet_rows_labels_set_visibility");
   begin
      Internal (Get_Object (Sheet), Boolean'Pos (Visible));
   end Rows_Labels_Set_Visibility;

   -------------------
   -- Select_Column --
   -------------------

   procedure Select_Column (Sheet  : access Gtk_Sheet_Record;
                            Column : in Gint)
   is
      procedure Internal (Sheet  : in System.Address;
                          Column : in Gint);
      pragma Import (C, Internal, "gtk_sheet_select_column");
   begin
      Internal (Get_Object (Sheet), Column);
   end Select_Column;

   ----------------
   -- Select_Row --
   ----------------

   procedure Select_Row (Sheet : access Gtk_Sheet_Record;
                         Row   : in Gint)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint);
      pragma Import (C, Internal, "gtk_sheet_select_row");
   begin
      Internal (Get_Object (Sheet), Row);
   end Select_Row;

   ----------------
   -- Clip_Range --
   ----------------

   procedure Clip_Range (Sheet     : access Gtk_Sheet_Record;
                         The_Range : in Gtk_Sheet_Range)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range);
      pragma Import (C, Internal, "gtk_sheet_clip_range");
   begin
      Internal (Get_Object (Sheet), The_Range);
   end Clip_Range;

   ------------------
   -- Unclip_Range --
   ------------------

   procedure Unclip_Range (Sheet : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_unclip_range");
   begin
      Internal (Get_Object (Sheet));
   end Unclip_Range;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment (Sheet  : access Gtk_Sheet_Record)
                            return      Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Sheet  : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_sheet_get_vadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Sheet)), Stub));
   end Get_Vadjustment;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment (Sheet  : access Gtk_Sheet_Record)
                            return      Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Sheet  : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_sheet_get_hadjustment");
      Stub : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment
        (Get_User_Data (Internal (Get_Object (Sheet)), Stub));
   end Get_Hadjustment;

   ------------------
   -- Select_Range --
   ------------------

   procedure Select_Range
     (Sheet     : access Gtk_Sheet_Record;
      The_Range : in Gtk_Sheet_Range)
   is
      procedure Internal
        (Sheet : in System.Address; The_Range : access Gtk_Sheet_Range);
      pragma Import (C, Internal, "gtk_sheet_select_range");

      R : aliased Gtk_Sheet_Range := The_Range;

   begin
      Internal (Get_Object (Sheet), R'Access);
   end Select_Range;

   --------------------
   -- Unselect_Range --
   --------------------

   procedure Unselect_Range (Sheet     : access Gtk_Sheet_Record)
   is
      procedure Internal (Sheet     : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_unselect_range");
   begin
      Internal (Get_Object (Sheet));
   end Unselect_Range;

   ---------------------
   -- Set_Active_Cell --
   ---------------------

   function Set_Active_Cell (Sheet  : access Gtk_Sheet_Record;
                             Row    : in Gint;
                             Column : in Gint)
                            return      Boolean
   is
      function Internal (Sheet  : in System.Address;
                         Row    : in Gint;
                         Column : in Gint)
                        return      Gint;
      pragma Import (C, Internal, "gtk_sheet_set_active_cell");
   begin
      return Boolean'Val (Internal (Get_Object (Sheet), Row, Column));
   end Set_Active_Cell;

   ---------------------
   -- Get_Active_Cell --
   ---------------------

   procedure Get_Active_Cell (Sheet  : access Gtk_Sheet_Record;
                              Row    : out Gint;
                              Column : out Gint)
   is
      procedure Internal (Sheet  : in System.Address;
                          Row    : access Gint;
                          Column : access Gint);
      pragma Import (C, Internal, "gtk_sheet_get_active_cell");
      R, C : aliased Gint;
   begin
      Internal (Get_Object (Sheet), R'Access, C'Access);
      Row := R;
      Column := C;
   end Get_Active_Cell;

   --------------
   -- Set_Cell --
   -------------

   procedure Set_Cell (Sheet         : access Gtk_Sheet_Record;
                       Row           : in Gint;
                       Col           : in Gint;
                       Justification : in Gtk.Enums.Gtk_Justification;
                       Text          : in String)
   is
      procedure Internal
        (Sheet         : System.Address;
         Row           : Gint;
         Col           : Gint;
         Justification : Gtk.Enums.Gtk_Justification;
         Text          : System.Address);
      pragma Import (C, Internal, "gtk_sheet_set_cell");

      T : aliased String := Text & ASCII.NUL;
      Ta : System.Address := T'Address;

   begin
      if Text = "" then
         Ta := System.Null_Address;
      end if;

      Internal (Get_Object (Sheet), Row, Col, Justification, Ta);
   end Set_Cell;

   -------------------
   -- Set_Cell_Text --
   -------------------

   procedure Set_Cell_Text (Sheet : access Gtk_Sheet_Record;
                            Row   : in Gint;
                            Col   : in Gint;
                            Text  : in String)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint;
                          Col   : in Gint;
                          Text  : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_set_cell_text");

      T : aliased String := Text & ASCII.NUL;
      Ta : System.Address := T'Address;

   begin
      if Text = "" then
         Ta := System.Null_Address;
      end if;

      Internal (Get_Object (Sheet), Row, Col, Ta);
   end Set_Cell_Text;

   -------------------
   -- Cell_Get_Text --
   -------------------

   function Cell_Get_Text (Sheet  : access Gtk_Sheet_Record;
                           Row    : in Gint;
                           Col    : in Gint)
                          return      String
   is
      function Internal (Sheet  : in System.Address;
                         Row    : in Gint;
                         Col    : in Gint)
                        return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_sheet_cell_get_text");
      use type Interfaces.C.Strings.chars_ptr;
      C : Interfaces.C.Strings.chars_ptr;
   begin
      C := Internal (Get_Object (Sheet), Row, Col);
      if C = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (C);
      end if;
   end Cell_Get_Text;

   ----------------
   -- Cell_Clear --
   ----------------

   procedure Cell_Clear (Sheet : access Gtk_Sheet_Record;
                         Row   : in Gint;
                         Col   : in Gint)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint;
                          Col   : in Gint);
      pragma Import (C, Internal, "gtk_sheet_cell_clear");
   begin
      Internal (Get_Object (Sheet), Row, Col);
   end Cell_Clear;

   -----------------
   -- Cell_Delete --
   -----------------

   procedure Cell_Delete (Sheet : access Gtk_Sheet_Record;
                          Row   : in Gint;
                          Col   : in Gint)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint;
                          Col   : in Gint);
      pragma Import (C, Internal, "gtk_sheet_cell_delete");
   begin
      Internal (Get_Object (Sheet), Row, Col);
   end Cell_Delete;

   -----------------
   -- Range_Clear --
   -----------------

   procedure Range_Clear (Sheet     : access Gtk_Sheet_Record;
                          The_Range : in Gtk_Sheet_Range)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : access Gtk_Sheet_Range);
      pragma Import (C, Internal, "gtk_sheet_range_clear");

      R : aliased Gtk_Sheet_Range := The_Range;

   begin
      Internal (Get_Object (Sheet), R'Access);
   end Range_Clear;

   ------------------
   -- Range_Delete --
   ------------------

   procedure Range_Delete (Sheet     : access Gtk_Sheet_Record;
                           The_Range : in Gtk_Sheet_Range)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range);
      pragma Import (C, Internal, "gtk_sheet_range_delete");
   begin
      Internal (Get_Object (Sheet), The_Range);
   end Range_Delete;

   --------------------
   -- Cell_Get_State --
   --------------------

   function Cell_Get_State (Sheet  : access Gtk_Sheet_Record;
                            Row    : in Gint;
                            Col    : in Gint)
                           return      Gtk.Enums.Gtk_State_Type
   is
      function Internal (Sheet  : in System.Address;
                         Row    : in Gint;
                         Col    : in Gint)
                        return      Gint;
      pragma Import (C, Internal, "gtk_sheet_cell_get_state");
   begin
      return Gtk.Enums.Gtk_State_Type'Val
        (Internal (Get_Object (Sheet), Row, Col));
   end Cell_Get_State;

   -----------
   -- Links --
   -----------

   package body Links is

      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);
      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);

      ---------------
      -- Link_Cell --
      ---------------

      procedure Link_Cell (Sheet : access Gtk_Sheet_Record;
                           Row   : in Gint;
                           Col   : in Gint;
                           Link  : in Data_Type)
      is
         procedure Internal (Sheet : in System.Address;
                             Row   : in Gint;
                             Col   : in Gint;
                             Link  : in System.Address);
         pragma Import (C, Internal, "gtk_sheet_link_cell");
         D : Data_Type_Access := new Data_Type'(Link);
      begin
         Internal (Get_Object (Sheet),
                   Row,
                   Col,
                   Convert (D));
      end Link_Cell;

      --------------
      -- Get_Link --
      --------------

      function Get_Link (Sheet  : access Gtk_Sheet_Record;
                         Row    : in Gint;
                         Col    : in Gint)
                        return      Data_Type_Access
      is
         function Internal (Sheet  : in System.Address;
                            Row    : in Gint;
                            Col    : in Gint)
                           return      System.Address;
         pragma Import (C, Internal, "gtk_sheet_get_link");
      begin
         return Convert (Internal (Get_Object (Sheet), Row, Col));
      end Get_Link;

   end Links;

   -----------------
   -- Remove_Link --
   -----------------

   procedure Remove_Link (Sheet : access Gtk_Sheet_Record;
                          Row   : in Gint;
                          Col   : in Gint)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Gint;
                          Col   : in Gint);
      pragma Import (C, Internal, "gtk_sheet_remove_link");
   begin
      Internal (Get_Object (Sheet), Row, Col);
   end Remove_Link;

   --------------------
   -- Get_Pixel_Info --
   --------------------

   procedure Get_Pixel_Info
     (Sheet  : access Gtk_Sheet_Record;
      X      : in Gint;
      Y      : in Gint;
      Row    : out Gint;
      Column : out Gint)
   is
      function Internal
        (Sheet  : in System.Address;
         X      : in Gint;
         Y      : in Gint;
         Row    : access Gint;
         Column : access Gint) return Gint;
      pragma Import (C, Internal, "gtk_sheet_get_pixel_info");

      R, C : aliased Gint;
   begin
      if Internal (Get_Object (Sheet), X, Y, R'Access, C'Access) = 0 then
         raise Constraint_Error;
      end if;

      Row := R;
      Column := C;
   end Get_Pixel_Info;

   -------------------
   -- Get_Cell_Area --
   -------------------

   procedure Get_Cell_Area
     (Sheet  : access Gtk_Sheet_Record;
      Row    : in Gint;
      Column : in Gint;
      Area   : out Gdk.Rectangle.Gdk_Rectangle)
   is
      function Internal
        (Sheet  : in System.Address;
         Row    : in Gint;
         Column : in Gint;
         Area   : access Gdk.Rectangle.Gdk_Rectangle) return Gint;
      pragma Import (C, Internal, "gtk_sheet_get_cell_area");

      A : aliased Gdk.Rectangle.Gdk_Rectangle;
   begin
      if Internal (Get_Object (Sheet), Row, Column, A'Access) = 0 then
         raise Constraint_Error;
      end if;
      Area := A;
   end Get_Cell_Area;

   ----------------------
   -- Set_Column_Width --
   ----------------------

   procedure Set_Column_Width (Sheet  : access Gtk_Sheet_Record;
                               Column : in Gint;
                               Width  : in Guint)
   is
      procedure Internal (Sheet  : in System.Address;
                          Column : in Gint;
                          Width  : in Guint);
      pragma Import (C, Internal, "gtk_sheet_set_column_width");
   begin
      Internal (Get_Object (Sheet), Column, Width);
   end Set_Column_Width;

   --------------------
   -- Set_Row_Height --
   --------------------

   procedure Set_Row_Height (Sheet  : access Gtk_Sheet_Record;
                             Row    : in Gint;
                             Height : in Guint)
   is
      procedure Internal (Sheet  : in System.Address;
                          Row    : in Gint;
                          Height : in Guint);
      pragma Import (C, Internal, "gtk_sheet_set_row_height");
   begin
      Internal (Get_Object (Sheet), Row, Height);
   end Set_Row_Height;

   ----------------
   -- Add_Column --
   ----------------

   procedure Add_Column (Sheet : access Gtk_Sheet_Record;
                         Ncols : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Ncols : in Guint);
      pragma Import (C, Internal, "gtk_sheet_add_column");
   begin
      Internal (Get_Object (Sheet), Ncols);
   end Add_Column;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row (Sheet : access Gtk_Sheet_Record;
                      Nrows : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Nrows : in Guint);
      pragma Import (C, Internal, "gtk_sheet_add_row");
   begin
      Internal (Get_Object (Sheet), Nrows);
   end Add_Row;

   -----------------
   -- Insert_Rows --
   -----------------

   procedure Insert_Rows (Sheet : access Gtk_Sheet_Record;
                          Row   : in Guint;
                          Nrows : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Guint;
                          Nrows : in Guint);
      pragma Import (C, Internal, "gtk_sheet_insert_rows");
   begin
      Internal (Get_Object (Sheet), Row, Nrows);
   end Insert_Rows;

   --------------------
   -- Insert_Columns --
   --------------------

   procedure Insert_Columns (Sheet : access Gtk_Sheet_Record;
                             Col   : in Guint;
                             Ncols : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Col   : in Guint;
                          Ncols : in Guint);
      pragma Import (C, Internal, "gtk_sheet_insert_columns");
   begin
      Internal (Get_Object (Sheet), Col, Ncols);
   end Insert_Columns;

   -----------------
   -- Delete_Rows --
   -----------------

   procedure Delete_Rows (Sheet : access Gtk_Sheet_Record;
                          Row   : in Guint;
                          Nrows : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Row   : in Guint;
                          Nrows : in Guint);
      pragma Import (C, Internal, "gtk_sheet_delete_rows");
   begin
      Internal (Get_Object (Sheet), Row, Nrows);
   end Delete_Rows;

   --------------------
   -- Delete_Columns --
   --------------------

   procedure Delete_Columns (Sheet : access Gtk_Sheet_Record;
                             Col   : in Guint;
                             Ncols : in Guint)
   is
      procedure Internal (Sheet : in System.Address;
                          Col   : in Guint;
                          Ncols : in Guint);
      pragma Import (C, Internal, "gtk_sheet_delete_columns");
   begin
      Internal (Get_Object (Sheet), Col, Ncols);
   end Delete_Columns;

   --------------------------
   -- Range_Set_Background --
   --------------------------

   procedure Range_Set_Background (Sheet     : access Gtk_Sheet_Record;
                                   The_Range : in Gtk_Sheet_Range;
                                   Color     : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range;
                          Color     : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_range_set_background");
      use type Gdk.Color.Gdk_Color;

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Sheet), The_Range, Ca);
   end Range_Set_Background;

   --------------------------
   -- Range_Set_Foreground --
   --------------------------

   procedure Range_Set_Foreground (Sheet     : access Gtk_Sheet_Record;
                                   The_Range : in Gtk_Sheet_Range;
                                   Color     : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range;
                          Color     : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_range_set_foreground");
      use type Gdk.Color.Gdk_Color;

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Sheet), The_Range, Ca);
   end Range_Set_Foreground;

   -----------------------------
   -- Range_Set_Justification --
   -----------------------------

   procedure Range_Set_Justification
      (Sheet         : access Gtk_Sheet_Record;
       The_Range     : in Gtk_Sheet_Range;
       Justification : in Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Sheet         : System.Address;
         The_Range     : Gtk_Sheet_Range;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_sheet_range_set_justification");

   begin
      Internal (Get_Object (Sheet), The_Range, Justification);
   end Range_Set_Justification;

   ------------------------------
   -- Column_Set_Justification --
   ------------------------------

   procedure Column_Set_Justification
      (Sheet         : access Gtk_Sheet_Record;
       Column        : in Gint;
       Justification : in Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Sheet         : System.Address;
         Column        : Gint;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_sheet_column_set_justification");

   begin
      Internal (Get_Object (Sheet), Column, Justification);
   end Column_Set_Justification;

   ------------------------
   -- Range_Set_Editable --
   ------------------------

   procedure Range_Set_Editable (Sheet     : access Gtk_Sheet_Record;
                                 The_Range : in Gtk_Sheet_Range;
                                 Editable  : in Boolean)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range;
                          Editable  : in Gint);
      pragma Import (C, Internal, "gtk_sheet_range_set_editable");
   begin
      Internal (Get_Object (Sheet), The_Range, Boolean'Pos (Editable));
   end Range_Set_Editable;

   -----------------------
   -- Range_Set_Visible --
   -----------------------

   procedure Range_Set_Visible (Sheet     : access Gtk_Sheet_Record;
                                The_Range : in Gtk_Sheet_Range;
                                Visible   : in Boolean)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range;
                          Visible   : in Gint);
      pragma Import (C, Internal, "gtk_sheet_range_set_visible");
   begin
      Internal (Get_Object (Sheet), The_Range, Boolean'Pos (Visible));
   end Range_Set_Visible;

   ----------------------
   -- Range_Set_Border --
   ----------------------

   procedure Range_Set_Border (Sheet      : access Gtk_Sheet_Record;
                               The_Range  : in Gtk_Sheet_Range;
                               Mask       : in Gtk_Sheet_Border;
                               Width      : in Guint;
                               Line_Style : in Gdk.GC.Gdk_Line_Style)
   is
      procedure Internal
        (Sheet      : System.Address;
         The_Range  : Gtk_Sheet_Range;
         Mask       : Gtk_Sheet_Border;
         Width      : Guint;
         Line_Style : Gdk.GC.Gdk_Line_Style);
      pragma Import (C, Internal, "gtk_sheet_range_set_border");

   begin
      Internal (Get_Object (Sheet), The_Range, Mask, Width, Line_Style);
   end Range_Set_Border;

   ----------------------------
   -- Range_Set_Border_Color --
   ----------------------------

   procedure Range_Set_Border_Color (Sheet     : access Gtk_Sheet_Record;
                                     The_Range : in Gtk_Sheet_Range;
                                     Color     : in Gdk.Color.Gdk_Color)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range;
                          Color     : in System.Address);
      pragma Import (C, Internal, "gtk_sheet_range_set_border_color");
      use type Gdk.Color.Gdk_Color;

      C : aliased Gdk.Color.Gdk_Color := Color;
      Ca : System.Address := C'Address;

   begin
      if Color = Gdk.Color.Null_Color then
         Ca := System.Null_Address;
      end if;

      Internal (Get_Object (Sheet), The_Range, Ca);
   end Range_Set_Border_Color;

   --------------------
   -- Range_Set_Font --
   --------------------

   procedure Range_Set_Font (Sheet     : access Gtk_Sheet_Record;
                             The_Range : in Gtk_Sheet_Range;
                             Font      : in Gdk.Font.Gdk_Font)
   is
      procedure Internal (Sheet     : in System.Address;
                          The_Range : in Gtk_Sheet_Range;
                          Font      : in Gdk.Font.Gdk_Font);
      pragma Import (C, Internal, "gtk_sheet_range_set_font");
   begin
      Internal (Get_Object (Sheet), The_Range, Font);
   end Range_Set_Font;

   ---------
   -- Put --
   ---------

   procedure Put (Sheet  : access Gtk_Sheet_Record;
                  Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                  X      : in Gint;
                  Y      : in Gint)
   is
      function Internal (Sheet  : in System.Address;
                         Widget : in System.Address;
                         X      : in Gint;
                         Y      : in Gint)
                        return      System.Address;
      pragma Import (C, Internal, "gtk_sheet_put");
      S : System.Address;
   begin
      S := Internal (Get_Object (Sheet), Get_Object (Widget), X, Y);
   end Put;

   ------------
   -- Attach --
   ------------

   procedure Attach (Sheet   : access Gtk_Sheet_Record;
                     Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
                     Row     : in Gint;
                     Col     : in Gint;
                     X_Align : in Gfloat;
                     Y_Align : in Gfloat)
   is
      procedure Internal (Sheet   : in System.Address;
                          Widget  : in System.Address;
                          Row     : in Gint;
                          Col     : in Gint;
                          X_Align : in Gfloat;
                          Y_Align : in Gfloat);
      pragma Import (C, Internal, "gtk_sheet_attach");
   begin
      Internal (Get_Object (Sheet), Get_Object (Widget), Row,  Col,
                X_Align, Y_Align);
   end Attach;

   ----------------
   -- Move_Child --
   ----------------

   procedure Move_Child (Sheet  : access Gtk_Sheet_Record;
                         Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
                         X      : in Gint;
                         Y      : in Gint)
   is
      procedure Internal (Sheet  : in System.Address;
                          Widget : in System.Address;
                          X      : in Gint;
                          Y      : in Gint);
      pragma Import (C, Internal, "gtk_sheet_move_child");
   begin
      Internal (Get_Object (Sheet), Get_Object (Widget), X, Y);
   end Move_Child;

   ------------------
   -- Get_Child_At --
   ------------------

   function Get_Child_At (Sheet  : access Gtk_Sheet_Record;
                          Row    : in Gint;
                          Col    : in Gint)
                         return      Gtk_Sheet_Child
   is
      function Internal (Sheet  : in System.Address;
                         Row    : in Gint;
                         Col    : in Gint)
                        return      Gtk_Sheet_Child;
      pragma Import (C, Internal, "gtk_sheet_get_child_at");
   begin
      return Internal (Get_Object (Sheet), Row, Col);
   end Get_Child_At;

   ----------------
   -- Get_Widget --
   ----------------

   function Get_Widget (Child : Gtk_Sheet_Child)
                       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Child : Gtk_Sheet_Child) return System.Address;
      pragma Import (C, Internal, "ada_gtk_sheet_get_widget");
      Stub : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget
        (Get_User_Data (Internal (Child), Stub));
   end Get_Widget;

   ---------------
   -- Get_Range --
   ---------------

   function Get_Range
     (Sheet : access Gtk_Sheet_Record) return Gtk_Sheet_Range
   is
      function Internal (Sheet : System.Address) return Gtk_Sheet_Range;
      pragma Import (C, Internal, "ada_gtk_sheet_get_range");
   begin
      return Internal (Get_Object (Sheet));
   end Get_Range;

   -----------------------
   -- Get_Columns_Count --
   -----------------------

   function Get_Columns_Count (Sheet : access Gtk_Sheet_Record) return Guint is
      function Internal (Sheet : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_sheet_get_columns_count");
   begin
      return Internal (Get_Object (Sheet));
   end Get_Columns_Count;

   --------------------
   -- Get_Rows_Count --
   --------------------

   function Get_Rows_Count (Sheet : access Gtk_Sheet_Record) return Guint is
      function Internal (Sheet : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_sheet_get_rows_count");
   begin
      return Internal (Get_Object (Sheet));
   end Get_Rows_Count;

   ----------------------
   -- Get_Column_Title --
   ----------------------

   function Get_Column_Title (Sheet  : access Gtk_Sheet_Record;
                              Column : Gint)
                             return String
   is
      function Internal (Sheet : System.Address; Column : Gint)
                        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_sheet_get_column_title");
      use type Interfaces.C.Strings.chars_ptr;
      C : Interfaces.C.Strings.chars_ptr;
   begin
      C := Internal (Get_Object (Sheet), Column);
      if C = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (C);
      end if;
   end Get_Column_Title;

   -------------------
   -- Get_Row_Title --
   -------------------

   function Get_Row_Title (Sheet  : access Gtk_Sheet_Record;
                           Row : Gint)
                          return String
   is
      function Internal (Sheet : System.Address; Row : Gint)
                        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_gtk_sheet_get_row_title");
      use type Interfaces.C.Strings.chars_ptr;
      C : Interfaces.C.Strings.chars_ptr;
   begin
      C := Internal (Get_Object (Sheet), Row);
      if C = Interfaces.C.Strings.Null_Ptr then
         return "";
      else
         return Interfaces.C.Strings.Value (C);
      end if;
   end Get_Row_Title;

   ----------------------
   -- Get_Column_Width --
   ----------------------

   function Get_Column_Width (Sheet  : access Gtk_Sheet_Record;
                              Column : in Gint)
                             return Gint
   is
      function Internal (Sheet  : System.Address;
                         Column : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gtk_sheet_get_column_width");
   begin
      return Internal (Get_Object (Sheet), Column);
   end Get_Column_Width;

   --------------------
   -- Get_Row_Height --
   --------------------

   function Get_Row_Height (Sheet  : access Gtk_Sheet_Record;
                            Row : in Gint)
                           return Gint
   is
      function Internal (Sheet  : System.Address;
                         Row : Gint)
                        return Gint;
      pragma Import (C, Internal, "ada_gtk_sheet_get_row_height");
   begin
      return Internal (Get_Object (Sheet), Row);
   end Get_Row_Height;

   -------------------
   -- Button_Attach --
   -------------------

   procedure Button_Attach
     (Sheet   : access Gtk_Sheet_Record;
      Widget  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Row     : in Gint;
      Col     : in Gint;
      X_Align : in Gfloat;
      Y_Align : in Gfloat)
   is
      procedure Internal (Sheet   : System.Address;
                          Widget  : System.Address;
                          Row     : Gint;
                          Col     : Gint;
                          X_Align : Gfloat;
                          Y_Align : Gfloat);
      pragma Import (C, Internal, "gtk_sheet_button_attach");
   begin
      Internal (Get_Object (Sheet), Get_Object (Widget), Row, Col,
                X_Align, Y_Align);
   end Button_Attach;

   --------------------
   -- Get_Attributes --
   --------------------

--     function Get_Attributes (Sheet      : access Gtk_Sheet_Record;
--                              Row        : in Gint;
--                              Col        : in Gint;
--                              Attributes : access Gtk_Sheet_Cell_Attr)
--                             return      Boolean
--     is
--        function Internal (Sheet      : in System.Address;
--                           Row        : in Gint;
--                           Col        : in Gint;
--                           Attributes : access Gtk_Sheet_Cell_Attr)
--                          return          Gint;
--        pragma Import (C, Internal, "gtk_sheet_get_attributes");
--     begin
--        return Boolean'Val (Internal (Get_Object (Sheet),
--                                      Row,
--                                      Col,
--                                      Attributes));
--     end Get_Attributes;

end Gtk.Extra.Sheet;
