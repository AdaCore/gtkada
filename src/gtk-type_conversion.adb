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

pragma Warnings (Off);
with Gtk.Accel_Label;
with Gtk.Adjustment;
with Gtk.Alignment;
with Gtk.Arrow;
with Gtk.Aspect_Frame;
with Gtk.Bin;
with Gtk.Box;
with Gtk.Button;
with Gtk.Button_Box;
with Gtk.Calendar;
with Gtk.Check_Button;
with Gtk.Check_Menu_Item;
with Gtk.Clist;
with Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;
with Gtk.Combo;
with Gtk.Container;
with Gtk.Ctree;
with Gtk.Curve;
with Gtk.Data;
with Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.Editable;
with Gtk.Event_Box;
with Gtk.Extra.Plot;
with Gtk.Extra.Plot_Canvas;
with Gtk.Extra.Sheet;
with Gtk.Extra.Combo_Box;
with Gtk.Extra.Color_Combo;
with Gtk.Extra.Border_Combo;
with Gtk.Extra.Font_Combo;
with Gtk.Extra.Item_Entry;
with Gtk.File_Selection;
with Gtk.Fixed;
with Gtk.Font_Selection;
with Gtk.Font_Selection_Dialog;
with Gtk.Frame;
with Gtk.Gamma_Curve;
with Gtk.GEntry;
with Gtk.GRange;
with Gtk.Handle_Box;
with Gtk.Hbutton_Box;
with Gtk.Image;
with Gtk.Input_Dialog;
with Gtk.Invisible;
with Gtk.Item;
with Gtk.Item_Factory;
with Gtk.Layout;
with Gtk.List;
with Gtk.List_Item;
with Gtk.Menu;
with Gtk.Menu_Bar;
with Gtk.Menu_Item;
with Gtk.Menu_Shell;
with Gtk.Misc;
with Gtk.Notebook;
with Gtk.Object;
with Gtk.Option_Menu;
with Gtk.Packer;
with Gtk.Paned;
with Gtk.Pixmap;
with Gtk.Plug;
with Gtk.Preview;
with Gtk.Progress;
with Gtk.Progress_Bar;
with Gtk.Radio_Button;
with Gtk.Radio_Menu_Item;
with Gtk.Rc;
with Gtk.Ruler;
with Gtk.Scale;
with Gtk.Scrollbar;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Socket;
with Gtk.Spin_Button;
with Gtk.Status_Bar;
with Gtk.Table;
with Gtk.Tearoff_Menu_Item;
with Gtk.Text;
with Gtk.Tips_Query;
with Gtk.Toggle_Button;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Tree;
with Gtk.Tree_Item;
with Gtk.Vbutton_Box;
with Gtk.Viewport;
with Gtk.Widget;
with Gtk.Window;
pragma Warnings (On);

with Gtk.Type_Conversion_Hooks;
pragma Elaborate_All (Gtk.Type_Conversion_Hooks);

package body Gtk.Type_Conversion is

   function Full_Conversion (Type_Name : String) return Root_Type_Access;
   --  This function knows about all base widgets present in GtkAda.
   --  One noticeable difference is Gtk_Label that is recognized by default,
   --  to avoid the need of this package for the common usage.

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      null;
      --  This function is only used to force the 'with' of this unit. All the
      --  actual work is done in the elaboration part of this package.
   end Init;

   ---------------------
   -- Full_Conversion --
   ---------------------

   function Full_Conversion (Type_Name : String) return Root_Type_Access is
   begin
      case Type_Name (Type_Name'First + 3) is
         when 'A' =>
            if Type_Name = "GtkAccelLabel" then
               return new Gtk.Accel_Label.Gtk_Accel_Label_Record;
            elsif Type_Name = "GtkAdjustment" then
               return new Gtk.Adjustment.Gtk_Adjustment_Record;
            elsif Type_Name = "GtkAlignment" then
               return new Gtk.Alignment.Gtk_Alignment_Record;
            elsif Type_Name = "GtkArrow" then
               return new Gtk.Arrow.Gtk_Arrow_Record;
            elsif Type_Name = "GtkAspectFrame" then
               return new Gtk.Aspect_Frame.Gtk_Aspect_Frame_Record;
            end if;
         when 'B' =>
            if Type_Name = "GtkBin" then
               return new Gtk.Bin.Gtk_Bin_Record;
            elsif Type_Name = "GtkBorderCombo" then
               return new Gtk.Extra.Border_Combo.Gtk_Border_Combo_Record;
            elsif Type_Name = "GtkBox" then
               return new Gtk.Box.Gtk_Box_Record;
            elsif Type_Name = "GtkButton" then
               return new Gtk.Button.Gtk_Button_Record;
            elsif Type_Name = "GtkButtonBox" then
               return new Gtk.Button_Box.Gtk_Button_Box_Record;
            end if;
         when 'C' =>
            if Type_Name = "GtkCalendar" then
               return new Gtk.Calendar.Gtk_Calendar_Record;
            elsif Type_Name = "GtkCheckButton" then
               return new Gtk.Check_Button.Gtk_Check_Button_Record;
            elsif Type_Name = "GtkCheckMenuItem" then
               return new Gtk.Check_Menu_Item.Gtk_Check_Menu_Item_Record;
            elsif Type_Name = "GtkCList" then
               return new Gtk.Clist.Gtk_Clist_Record;
            elsif Type_Name = "GtkColorCombo" then
               return new Gtk.Extra.Color_Combo.Gtk_Color_Combo_Record;
            elsif Type_Name = "GtkColorSelection" then
               return new Gtk.Color_Selection.Gtk_Color_Selection_Record;
            elsif Type_Name = "GtkColorSelectionDialog" then
               return new
                 Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog_Record;
            elsif Type_Name = "GtkCombo" then
               return new Gtk.Combo.Gtk_Combo_Record;
            elsif Type_Name = "GtkComboBox" then
               return new Gtk.Extra.Combo_Box.Gtk_Combo_Box_Record;
            elsif Type_Name = "GtkContainer" then
               return new Gtk.Container.Gtk_Container_Record;
            elsif Type_Name = "GtkCtree" then
               return new Gtk.Ctree.Gtk_Ctree_Record;
            elsif Type_Name = "GtkCurve" then
               return new Gtk.Curve.Gtk_Curve_Record;
            end if;
         when 'D' =>
            if Type_Name = "GtkData" then
               return new Gtk.Data.Gtk_Data_Record;
            elsif Type_Name = "GtkDialog" then
               return new Gtk.Dialog.Gtk_Dialog_Record;
            elsif Type_Name = "GtkDrawingArea" then
               return new Gtk.Drawing_Area.Gtk_Drawing_Area_Record;
            end if;
         when 'E' =>
            if Type_Name = "GtkEditable" then
               return new Gtk.Editable.Gtk_Editable_Record;
            elsif Type_Name = "GtkEventBox" then
               return new Gtk.Event_Box.Gtk_Event_Box_Record;
            elsif Type_Name = "GtkEntry" then
               return new Gtk.GEntry.Gtk_Entry_Record;
            end if;
         when 'F' =>
            if Type_Name = "GtkFileSelection" then
               return new Gtk.File_Selection.Gtk_File_Selection_Record;
            elsif Type_Name = "GtkFixed" then
               return new Gtk.Fixed.Gtk_Fixed_Record;
            elsif Type_Name = "GtkFontCombo" then
               return new Gtk.Extra.Font_Combo.Gtk_Font_Combo_Record;
            elsif Type_Name = "GtkFontSelection" then
               return new Gtk.Font_Selection.Gtk_Font_Selection_Record;
            elsif Type_Name = "GtkFontSelectionDialog" then
               return new
                 Gtk.Font_Selection_Dialog.Gtk_Font_Selection_Dialog_Record;
            elsif Type_Name = "GtkFrame" then
               return new Gtk.Frame.Gtk_Frame_Record;
            end if;
         when 'G' =>
            if Type_Name = "GtkGammaCurve" then
               return new Gtk.Gamma_Curve.Gtk_Gamma_Curve_Record;
            end if;
         when 'H' =>
            if Type_Name = "GtkHBox" then
               return new Gtk.Box.Gtk_Box_Record;
            elsif Type_Name = "GtkHandleBox" then
               return new Gtk.Handle_Box.Gtk_Handle_Box_Record;
            elsif Type_Name = "GtkHButtonBox" then
               return new Gtk.Hbutton_Box.Gtk_Hbutton_Box_Record;
            elsif Type_Name = "GtkHPaned" then
               return new Gtk.Paned.Gtk_Paned_Record;
            end if;
         when 'I' =>
            if Type_Name = "GtkItemEntry" then
               return new Gtk.Extra.Item_Entry.Gtk_IEntry_Record;
            elsif Type_Name = "GtkImage" then
               return new Gtk.Image.Gtk_Image_Record;
            elsif Type_Name = "GtkInputDialog" then
               return new Gtk.Input_Dialog.Gtk_Input_Dialog_Record;
            elsif Type_Name = "GtkInvisible" then
               return new Gtk.Invisible.Gtk_Invisible_Record;
            elsif Type_Name = "GtkItem" then
               return new Gtk.Item.Gtk_Item_Record;
            elsif Type_Name = "GtkItemFactory" then
               return new Gtk.Item_Factory.Gtk_Item_Factory_Record;
            end if;
         when 'L' =>
            if Type_Name = "GtkLayout" then
               return new Gtk.Layout.Gtk_Layout_Record;
            elsif Type_Name = "GtkList" then
               return new Gtk.List.Gtk_List_Record;
            elsif Type_Name = "GtkListItem" then
               return new Gtk.List_Item.Gtk_List_Item_Record;
            end if;
         when 'M' =>
            if Type_Name = "GtkMenu" then
               return new Gtk.Menu.Gtk_Menu_Record;
            elsif Type_Name = "GtkMenuBar" then
               return new Gtk.Menu_Bar.Gtk_Menu_Bar_Record;
            elsif Type_Name = "GtkMenuItem" then
               return new Gtk.Menu_Item.Gtk_Menu_Item_Record;
            elsif Type_Name = "GtkMenuShell" then
               return new Gtk.Menu_Shell.Gtk_Menu_Shell_Record;
            elsif Type_Name = "GtkMisc" then
               return new Gtk.Misc.Gtk_Misc_Record;
            end if;
         when 'N' =>
            if Type_Name = "GtkNotebook" then
               return new Gtk.Notebook.Gtk_Notebook_Record;
            end if;
         when 'O' =>
            if Type_Name = "GtkObject" then
               return new Gtk.Object.Gtk_Object_Record;
            elsif Type_Name = "GtkOptionMenu" then
               return new Gtk.Option_Menu.Gtk_Option_Menu_Record;
            end if;
         when 'P' =>
            if Type_Name = "GtkPacker" then
               return new Gtk.Packer.Gtk_Packer_Record;
            elsif Type_Name = "GtkPixmap" then
               return new Gtk.Pixmap.Gtk_Pixmap_Record;
            elsif Type_Name = "GtkPlug" then
               return new Gtk.Plug.Gtk_Plug_Record;
            elsif Type_Name = "GtkPlot" then
               return new Gtk.Extra.Plot.Gtk_Plot_Record;
            elsif Type_Name = "GtkPlotCanvas" then
               return new Gtk.Extra.Plot_Canvas.Gtk_Plot_Canvas_Record;
            elsif Type_Name = "GtkPreview" then
               return new Gtk.Preview.Gtk_Preview_Record;
            elsif Type_Name = "GtkProgress" then
               return new Gtk.Progress.Gtk_Progress_Record;
            elsif Type_Name = "GtkProgressBar" then
               return new Gtk.Progress_Bar.Gtk_Progress_Bar_Record;
            end if;
         when 'R' =>
            if Type_Name = "GtkRadioButton" then
               return new Gtk.Radio_Button.Gtk_Radio_Button_Record;
            elsif Type_Name = "GtkRadioMenuItem" then
               return new Gtk.Radio_Menu_Item.Gtk_Radio_Menu_Item_Record;
            elsif Type_Name = "GtkRange" then
               return new Gtk.GRange.Gtk_Range_Record;
            elsif Type_Name = "GtkRuler" then
               return new Gtk.Ruler.Gtk_Ruler_Record;
            end if;
         when 'S' =>
            if Type_Name = "GtkScale" then
               return new Gtk.Scale.Gtk_Scale_Record;
            elsif Type_Name = "GtkScrollbar" then
               return new Gtk.Scrollbar.Gtk_Scrollbar_Record;
            elsif Type_Name = "GtkScrolledWindow" then
               return new Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record;
            elsif Type_Name = "GtkSeparator" then
               return new Gtk.Separator.Gtk_Separator_Record;
            elsif Type_Name = "GtkSheet" then
               return new Gtk.Extra.Sheet.Gtk_Sheet_Record;
            elsif Type_Name = "GtkSocket" then
               return new Gtk.Socket.Gtk_Socket_Record;
            elsif Type_Name = "GtkSpinButton" then
               return new Gtk.Spin_Button.Gtk_Spin_Button_Record;
            elsif Type_Name = "GtkStatusBar" then
               return new Gtk.Status_Bar.Gtk_Status_Bar_Record;
            end if;
         when 'T' =>
            if Type_Name = "GtkTable" then
               return new Gtk.Table.Gtk_Table_Record;
            elsif Type_Name = "GtkTearoffMenuItem" then
               return new Gtk.Tearoff_Menu_Item.Gtk_Tearoff_Menu_Item_Record;
            elsif Type_Name = "GtkText" then
               return new Gtk.Text.Gtk_Text_Record;
            elsif Type_Name = "GtkTipsQuery" then
               return new Gtk.Tips_Query.Gtk_Tips_Query_Record;
            elsif Type_Name = "GtkToggleButton" then
               return new Gtk.Toggle_Button.Gtk_Toggle_Button_Record;
            elsif Type_Name = "GtkToolbar" then
               return new Gtk.Toolbar.Gtk_Toolbar_Record;
            elsif Type_Name = "GtkTooltips" then
               return new Gtk.Tooltips.Gtk_Tooltips_Record;
            elsif Type_Name = "GtkTree" then
               return new Gtk.Tree.Gtk_Tree_Record;
            elsif Type_Name = "GtkTreeItem" then
               return new Gtk.Tree_Item.Gtk_Tree_Item_Record;
            end if;
         when 'V' =>
            if Type_Name = "GtkVBox" then
               return new Gtk.Box.Gtk_Box_Record;
            elsif Type_Name = "GtkVButtonBox" then
               return new Gtk.Vbutton_Box.Gtk_Vbutton_Box_Record;
            elsif Type_Name = "GtkViewport" then
               return new Gtk.Viewport.Gtk_Viewport_Record;
            elsif Type_Name = "GtkVPaned" then
               return new Gtk.Paned.Gtk_Paned_Record;
            elsif Type_Name = "GtkVScrollbar" then
               return new Gtk.Scrollbar.Gtk_Scrollbar_Record;
            end if;
         when 'W' =>
            if Type_Name = "GtkWidget" then
               return new Gtk.Widget.Gtk_Widget_Record;
            elsif Type_Name = "GtkWindow" then
               return new Gtk.Window.Gtk_Window_Record;
            end if;
         when others => null;
      end case;

      return null;
   end Full_Conversion;

begin
   Gtk.Type_Conversion_Hooks.Add_Hook (Full_Conversion'Access);
end Gtk.Type_Conversion;
