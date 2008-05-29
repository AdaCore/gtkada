-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2008, AdaCore                   --
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

with Gtk.Adjustment;
with Gtk.Alignment;
with Gtk.Arrow;
with Gtk.Aspect_Frame;
with Gtk.Bin;
with Gtk.Box;
with Gtk.Button_Box;
with Gtk.Calendar;
with Gtk.Check_Button;
with Gtk.Color_Selection;
with Gtk.Color_Selection_Dialog;
with Gtk.Container;
with Gtk.Curve;
with Gtk.Dialog;
with Gtk.Drawing_Area;
with Gtk.Editable;
with Gtk.Event_Box;
--  XXX ???
--  with Gtk.Extra.Plot;
--  with Gtk.Extra.Plot_Canvas;
--  with Gtk.Extra.Sheet;
--  with Gtk.Extra.Combo_Box;
--  with Gtk.Extra.Color_Combo;
--  with Gtk.Extra.Border_Combo;
--  with Gtk.Extra.Font_Combo;
--  with Gtk.Extra.Item_Entry;
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
--  with Gtk.Item;
with Gtk.Layout;
with Gtk.Menu_Shell;
with Gtk.Misc;
with Gtk.Notebook;
--  with Gtk.Object;
with Gtk.Paned;
with Gtk.Plug;
pragma Warnings (Off);  --  These packages are obsolescent
with Gtk.Combo;
with Gtk.Item_Factory;
with Gtk.List;
with Gtk.Option_Menu;
with Gtk.Pixmap;
with Gtk.Preview;
with Gtk.Progress;
with Gtk.Ctree;
with Gtk.Clist;
with Gtk.Text;
with Gtk.Tips_Query;
pragma Warnings (On);
with Gtk.Progress_Bar;
with Gtk.Radio_Button;
--  with Gtk.Rc;
with Gtk.Ruler;
with Gtk.Scale;
with Gtk.Scrollbar;
with Gtk.Scrolled_Window;
with Gtk.Separator;
with Gtk.Socket;
with Gtk.Spin_Button;
with Gtk.Status_Bar;
with Gtk.Table;
with Gtk.Text_View;
with Gtk.Toggle_Button;
with Gtk.Toolbar;
with Gtk.Tooltips;
with Gtk.Tree_View;
with Gtk.Vbutton_Box;
with Gtk.Viewport;
with Gtk.Widget;
with Gtk.Window;

with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Type_Conversion is

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      null;
      --  This function is only used to force the 'with' of this unit. All the
      --  actual work is done in the elaboration part of the Type_Conversion
      --  packages
   end Init;

   package Gtk_Adjustment_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Adjustment.Get_Type'Access, Gtk.Adjustment.Gtk_Adjustment_Record);
   pragma Warnings (Off, Gtk_Adjustment_Conversion);

   package Gtk_Alignment_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Alignment.Get_Type'Access, Gtk.Alignment.Gtk_Alignment_Record);
   pragma Warnings (Off, Gtk_Alignment_Conversion);

   package Gtk_Arrow_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Arrow.Get_Type'Access, Gtk.Arrow.Gtk_Arrow_Record);
   pragma Warnings (Off, Gtk_Arrow_Conversion);

   package Gtk_Aspect_Frame_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Aspect_Frame.Get_Type'Access,
        Gtk.Aspect_Frame.Gtk_Aspect_Frame_Record);
   pragma Warnings (Off, Gtk_Aspect_Frame_Conversion);

   package Gtk_Bin_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Bin.Get_Type'Access, Gtk.Bin.Gtk_Bin_Record);
   pragma Warnings (Off, Gtk_Bin_Conversion);

   package Gtk_Box_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Box.Get_Type'Access, Gtk.Box.Gtk_Box_Record);
   pragma Warnings (Off, Gtk_Box_Conversion);

   package Gtk_Button_Box_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Button_Box.Get_Type'Access, Gtk.Button_Box.Gtk_Button_Box_Record);
   pragma Warnings (Off, Gtk_Button_Box_Conversion);

   package Gtk_Calendar_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Calendar.Get_Type'Access, Gtk.Calendar.Gtk_Calendar_Record);
   pragma Warnings (Off, Gtk_Calendar_Conversion);

   package Gtk_Check_Button_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Check_Button.Get_Type'Access,
        Gtk.Check_Button.Gtk_Check_Button_Record);
   pragma Warnings (Off, Gtk_Check_Button_Conversion);

   package Gtk_Clist_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Clist.Get_Type'Access, Gtk.Clist.Gtk_Clist_Record);
   pragma Warnings (Off, Gtk_Clist_Conversion);

   package Gtk_Color_Selection_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Color_Selection.Get_Type'Access,
        Gtk.Color_Selection.Gtk_Color_Selection_Record);
   pragma Warnings (Off, Gtk_Color_Selection_Conversion);

   package Gtk_Color_Selection_Dialog_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Color_Selection_Dialog.Get_Type'Access,
        Gtk.Color_Selection_Dialog.Gtk_Color_Selection_Dialog_Record);
   pragma Warnings (Off, Gtk_Color_Selection_Dialog_Conversion);

   package Gtk_Combo_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Combo.Get_Type'Access, Gtk.Combo.Gtk_Combo_Record);
   pragma Warnings (Off, Gtk_Combo_Conversion);

   package Gtk_Container_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Container.Get_Type'Access, Gtk.Container.Gtk_Container_Record);
   pragma Warnings (Off, Gtk_Container_Conversion);

   package Gtk_Ctree_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Ctree.Get_Type'Access, Gtk.Ctree.Gtk_Ctree_Record);
   pragma Warnings (Off, Gtk_Ctree_Conversion);

   package Gtk_Curve_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Curve.Get_Type'Access, Gtk.Curve.Gtk_Curve_Record);
   pragma Warnings (Off, Gtk_Curve_Conversion);

   package Gtk_Dialog_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Dialog.Get_Type'Access, Gtk.Dialog.Gtk_Dialog_Record);
   pragma Warnings (Off, Gtk_Dialog_Conversion);

   package Gtk_Drawing_Area_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Drawing_Area.Get_Type'Access,
        Gtk.Drawing_Area.Gtk_Drawing_Area_Record);
   pragma Warnings (Off, Gtk_Drawing_Area_Conversion);

   package Gtk_Editable_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Editable.Get_Type'Access, Gtk.Editable.Gtk_Editable_Record);
   pragma Warnings (Off, Gtk_Editable_Conversion);

   package Gtk_Event_Box_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Event_Box.Get_Type'Access, Gtk.Event_Box.Gtk_Event_Box_Record);
   pragma Warnings (Off, Gtk_Event_Box_Conversion);

   package Gtk_GEntry_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.GEntry.Get_Type'Access, Gtk.GEntry.Gtk_Entry_Record);
   pragma Warnings (Off, Gtk_GEntry_Conversion);

   package Gtk_File_Selection_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.File_Selection.Get_Type'Access,
        Gtk.File_Selection.Gtk_File_Selection_Record);
   pragma Warnings (Off, Gtk_File_Selection_Conversion);

   package Gtk_Fixed_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Fixed.Get_Type'Access, Gtk.Fixed.Gtk_Fixed_Record);
   pragma Warnings (Off, Gtk_Fixed_Conversion);

   package Gtk_Font_Selection_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Font_Selection.Get_Type'Access,
        Gtk.Font_Selection.Gtk_Font_Selection_Record);
   pragma Warnings (Off, Gtk_Font_Selection_Conversion);

   package Gtk_Font_Selection_Dialog_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Font_Selection_Dialog.Get_Type'Access,
        Gtk.Font_Selection_Dialog.Gtk_Font_Selection_Dialog_Record);
   pragma Warnings (Off, Gtk_Font_Selection_Dialog_Conversion);

   package Gtk_Frame_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Frame.Get_Type'Access, Gtk.Frame.Gtk_Frame_Record);
   pragma Warnings (Off, Gtk_Frame_Conversion);

   package Gtk_Gamma_Curve_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Gamma_Curve.Get_Type'Access,
        Gtk.Gamma_Curve.Gtk_Gamma_Curve_Record);
   pragma Warnings (Off, Gtk_Gamma_Curve_Conversion);

   package Gtk_Handle_Box_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Handle_Box.Get_Type'Access, Gtk.Handle_Box.Gtk_Handle_Box_Record);
   pragma Warnings (Off, Gtk_Handle_Box_Conversion);

   package Gtk_HButton_Box_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Hbutton_Box.Get_Type'Access,
        Gtk.Hbutton_Box.Gtk_Hbutton_Box_Record);
   pragma Warnings (Off, Gtk_HButton_Box_Conversion);

   package Gtk_Paned_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Paned.Get_Type'Access, Gtk.Paned.Gtk_Paned_Record);
   pragma Warnings (Off, Gtk_Paned_Conversion);

   package Gtk_Image_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Image.Get_Type'Access, Gtk.Image.Gtk_Image_Record);
   pragma Warnings (Off, Gtk_Image_Conversion);

   package Gtk_Input_Dialog_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Input_Dialog.Get_Type'Access,
        Gtk.Input_Dialog.Gtk_Input_Dialog_Record);
   pragma Warnings (Off, Gtk_Input_Dialog_Conversion);

   package Gtk_Invisible_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Invisible.Get_Type'Access, Gtk.Invisible.Gtk_Invisible_Record);
   pragma Warnings (Off, Gtk_Invisible_Conversion);

   package Gtk_Item_Factory_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Item_Factory.Get_Type'Access,
        Gtk.Item_Factory.Gtk_Item_Factory_Record);
   pragma Warnings (Off, Gtk_Item_Factory_Conversion);

   package Gtk_Layout_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Layout.Get_Type'Access, Gtk.Layout.Gtk_Layout_Record);
   pragma Warnings (Off, Gtk_Layout_Conversion);

   package Gtk_List_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.List.Get_Type'Access, Gtk.List.Gtk_List_Record);
   pragma Warnings (Off, Gtk_List_Conversion);

   package Gtk_Menu_Shell_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Menu_Shell.Get_Type'Access, Gtk.Menu_Shell.Gtk_Menu_Shell_Record);
   pragma Warnings (Off, Gtk_Menu_Shell_Conversion);

   package Gtk_Misc_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Misc.Get_Type'Access, Gtk.Misc.Gtk_Misc_Record);
   pragma Warnings (Off, Gtk_Misc_Conversion);

   package Gtk_Notebook_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Notebook.Get_Type'Access, Gtk.Notebook.Gtk_Notebook_Record);
   pragma Warnings (Off, Gtk_Notebook_Conversion);

--     package Gtk_Object_Conversion is new
--       Glib.Type_Conversion_Hooks.Hook_Registrator
--         (Gtk.Object.Get_Type'Access, Gtk.Object.Gtk_Object_Record);
--     pragma Warnings (Off, Gtk_Object_Conversion);

   package Gtk_Option_Menu_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Option_Menu.Get_Type'Access,
        Gtk.Option_Menu.Gtk_Option_Menu_Record);
   pragma Warnings (Off, Gtk_Option_Menu_Conversion);

   package Gtk_Pixmap_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Pixmap.Get_Type'Access, Gtk.Pixmap.Gtk_Pixmap_Record);
   pragma Warnings (Off, Gtk_Pixmap_Conversion);

   package Gtk_Plug_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Plug.Get_Type'Access, Gtk.Plug.Gtk_Plug_Record);
   pragma Warnings (Off, Gtk_Plug_Conversion);

   package Gtk_Preview_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Preview.Get_Type'Access, Gtk.Preview.Gtk_Preview_Record);
   pragma Warnings (Off, Gtk_Preview_Conversion);

   package Gtk_Progress_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Progress.Get_Type'Access, Gtk.Progress.Gtk_Progress_Record);
   pragma Warnings (Off, Gtk_Progress_Conversion);

   package Gtk_Progress_Bar_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Progress_Bar.Get_Type'Access,
        Gtk.Progress_Bar.Gtk_Progress_Bar_Record);
   pragma Warnings (Off, Gtk_Progress_Bar_Conversion);

   package Gtk_Radio_Button_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Radio_Button.Get_Type'Access,
        Gtk.Radio_Button.Gtk_Radio_Button_Record);
   pragma Warnings (Off, Gtk_Radio_Button_Conversion);

   package Gtk_Range_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.GRange.Get_Type'Access, Gtk.GRange.Gtk_Range_Record);
   pragma Warnings (Off, Gtk_Range_Conversion);

   package Gtk_Ruler_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Ruler.Get_Type'Access, Gtk.Ruler.Gtk_Ruler_Record);
   pragma Warnings (Off, Gtk_Ruler_Conversion);

   package Gtk_Scale_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Scale.Get_Type'Access, Gtk.Scale.Gtk_Scale_Record);
   pragma Warnings (Off, Gtk_Scale_Conversion);

   package Gtk_Scrollbar_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Scrollbar.Get_Type'Access, Gtk.Scrollbar.Gtk_Scrollbar_Record);
   pragma Warnings (Off, Gtk_Scrollbar_Conversion);

   package Gtk_Scrolled_Window_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Scrolled_Window.Get_Type'Access,
        Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record);
   pragma Warnings (Off, Gtk_Scrolled_Window_Conversion);

   package Gtk_Separator_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Separator.Get_Type'Access, Gtk.Separator.Gtk_Separator_Record);
   pragma Warnings (Off, Gtk_Separator_Conversion);

   package Gtk_Socket_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Socket.Get_Type'Access, Gtk.Socket.Gtk_Socket_Record);
   pragma Warnings (Off, Gtk_Socket_Conversion);

   package Gtk_Spin_Button_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Spin_Button.Get_Type'Access,
        Gtk.Spin_Button.Gtk_Spin_Button_Record);
   pragma Warnings (Off, Gtk_Spin_Button_Conversion);

   package Gtk_Status_Bar_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Status_Bar.Get_Type'Access, Gtk.Status_Bar.Gtk_Status_Bar_Record);
   pragma Warnings (Off, Gtk_Status_Bar_Conversion);

   package Gtk_Table_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Table.Get_Type'Access, Gtk.Table.Gtk_Table_Record);
   pragma Warnings (Off, Gtk_Table_Conversion);

   package Gtk_Text_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Text.Get_Type'Access, Gtk.Text.Gtk_Text_Record);
   pragma Warnings (Off, Gtk_Text_Conversion);

   package Gtk_Toggle_Button_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Toggle_Button.Get_Type'Access,
        Gtk.Toggle_Button.Gtk_Toggle_Button_Record);
   pragma Warnings (Off, Gtk_Toggle_Button_Conversion);

   package Gtk_Toolbar_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Toolbar.Get_Type'Access, Gtk.Toolbar.Gtk_Toolbar_Record);
   pragma Warnings (Off, Gtk_Toolbar_Conversion);

   package Gtk_Tooltips_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Tooltips.Get_Type'Access, Gtk.Tooltips.Gtk_Tooltips_Record);
   pragma Warnings (Off, Gtk_Tooltips_Conversion);

   package Gtk_Text_View_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Text_View.Get_Type'Access, Gtk.Text_View.Gtk_Text_View_Record);
   pragma Warnings (Off, Gtk_Text_View_Conversion);

   package Gtk_Tree_View_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Tree_View.Get_Type'Access, Gtk.Tree_View.Gtk_Tree_View_Record);
   pragma Warnings (Off, Gtk_Tree_View_Conversion);

   package Gtk_Vbutton_Box_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Vbutton_Box.Get_Type'Access,
        Gtk.Vbutton_Box.Gtk_Vbutton_Box_Record);
   pragma Warnings (Off, Gtk_Vbutton_Box_Conversion);

   package Gtk_Viewport_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Viewport.Get_Type'Access, Gtk.Viewport.Gtk_Viewport_Record);
   pragma Warnings (Off, Gtk_Viewport_Conversion);

   package Gtk_Widget_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Widget.Get_Type'Access, Gtk.Widget.Gtk_Widget_Record);
   pragma Warnings (Off, Gtk_Widget_Conversion);

   package Gtk_Window_Conversion is new
     Glib.Type_Conversion_Hooks.Hook_Registrator
       (Gtk.Window.Get_Type'Access, Gtk.Window.Gtk_Window_Record);
   pragma Warnings (Off, Gtk_Window_Conversion);

end Gtk.Type_Conversion;
