------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                    Copyright (C) 1998-2026, AdaCore                      --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib;                   use Glib;
with Gtk.Application;        use Gtk.Application;
with Gtk.Application_Window; use Gtk.Application_Window;
with Gtk.Box;                use Gtk.Box;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Label;              use Gtk.Label;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Paned;              use Gtk.Paned;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;

--  TRANSITION: the original `with` clauses are preserved below as
--  comments. Uncomment each one as the corresponding binding becomes
--  available again.
--
--  with Glib.Properties;     use Glib.Properties;
--  with Gtk;                 use Gtk;
--  with Gdk;                 use Gdk;
--  with Gdk.Color;           use Gdk.Color;
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
--  with Gtk.Dialog;          use Gtk.Dialog;
--  with Gtk.Handlers;        use Gtk.Handlers;
--  with Gtkada.Handlers;     use Gtkada.Handlers;
--  with Gtk.Label;           use Gtk.Label;
--  with Gtk.Main;            use Gtk.Main;
--  with Gtk.Notebook;        use Gtk.Notebook;
--  with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
--  with Gtk.Text_Iter;       use Gtk.Text_Iter;
--  with Gtk.Text_Mark;       use Gtk.Text_Mark;
--  with Gtk.Text_Tag;        use Gtk.Text_Tag;
--  with Gtk.Text_Tag_Table;  use Gtk.Text_Tag_Table;
--  with Gtk.Text_View;       use Gtk.Text_View;
--  with Gtk.Window;          use Gtk.Window;
--  with Pango.Font;          use Pango.Font;

with Ada.Strings.Unbounded;

--  with Create_About;
--  with Create_Alignment;
--  with Create_Application;
--  with Create_Arrow;
--  with Create_Assistant;
--  with Create_Builder;
--  with Create_GL;
--  with Create_Gtkada_Builder;
--  with Create_Button_Box;
with Create_Buttons;
--  with Create_Cairo;
--  with Create_Calendar;
--  with Create_Canvas;
--  with Create_Canvas_View_Animate;
--  with Create_Canvas_View_Composite;
--  with Create_Canvas_View_Edit;
--  with Create_Canvas_View_Events;
--  with Create_Canvas_View_Items;
--  with Create_Canvas_View_Links;
--  with Create_Canvas_View_Minimap;
--  with Create_Canvas_View_Routes;
--  with Create_Canvas_View_Rtrees;
--  with Create_Cell_View;
--  with Create_Check_Buttons;
--  with Create_Clipboard;
--  with Create_Color_Chooser;
--  with Create_Color_Selection;
--  with Create_Combo_Box;
--  with Create_Cursors;
--  with Create_Dialog;
--  with Create_Dnd;
--  with Create_Entry;
--  with Create_File_Chooser;
--  with Create_File_Selection;
--  with Create_Fixed;
--  with Create_Flow_Box;
--  with Create_Font_Chooser;
--  with Create_Font_Selection;
--  with Create_Gestures;
--  with Create_Gtkada_Dialog;
--  with Create_Icon_View;
with Create_Label;
--  with Create_Layout;
--  with Create_Link_Buttons;
--  with Create_Main_Loop;
--  with Create_Menu;
--  with Create_MDI;
--  with Create_Notebook;
--  with Create_Opacity;
--  with Create_Pixbuf;
--  with Create_Print;
--  with Create_Progress;
--  with Create_Radio_Button;
--  with Create_Range;
--  with Create_Reparent;
--  with Create_Revealer;
--  with Create_Selection;
--  with Create_Size_Groups;
--  with Create_Stack;
--  with Create_Sources;
--  with Create_Spin;
--  with Create_Spinners;
--  with Create_Splittable;
--  with Create_Status;
--  with Create_Status_Icons;
--  with Create_Task_Monitor;
--  with Create_Test_Idle;
--  with Create_Test_Timeout;
--  with Create_Text_View;
--  with Create_Toggle_Buttons;
--  with Create_Toolbar;
--  with Create_Tooltips;
with Create_Tree_Filter;
with Create_Tree_View;
--  with Create_UI_Manager;
--  with Common; use Common;
--  with Create_Css_Accordion;
--  with Create_Css_Editor;
--
--  with Libart_Demo;  use Libart_Demo;

package body Main_Windows is

   Label_Column : constant := 0;
   Demo_Column  : constant := 1;
   --  Columns in the demo selector list store. Label_Column holds the text
   --  shown in the tree, Demo_Column holds the index into Demos below.

   type Demo_Function is
     access procedure (Frame : access Gtk.Frame.Gtk_Frame_Record'Class);

   type Help_Function is access function return String;

   type Demo_Info is record
      Label : Ada.Strings.Unbounded.Unbounded_String;
      Run   : Demo_Function;
      Help  : Help_Function;
   end record;

   function To_Demo (Name : String; Runner : Demo_Function) return Demo_Info
   is (Label => Ada.Strings.Unbounded.To_Unbounded_String (Name),
       Run   => Runner);

   Demos : constant array (Positive range <>) of Demo_Info :=
     (To_Demo ("Labels", Create_Label.Run'Access),
      To_Demo ("Tree Filter", Create_Tree_Filter.Run'Access),
      To_Demo ("Tree View", Create_Tree_View.Run'Access),
      To_Demo ("Buttons", Create_Buttons.Run'Access));
   --  The set of demos exposed in the selector. New entries can be added
   --  here as the corresponding bindings are reintroduced.

   Demo_Frame : Gtk_Frame;
   --  The frame on the right-hand side of the paned, in which the currently
   --  selected demo is displayed.

   Help_Label : Gtk_Label;
   --  The label, below Demo_Frame, showing the help text of the currently
   --  selected demo.

   function To_Markup (Help : String) return String;
   --  Translate the legacy "@b...@B" emphasis markers used by the demo Help
   --  strings into Pango markup ("<b>...</b>"), escaping the XML-significant
   --  characters along the way so the result is always well-formed markup.

   procedure On_Selection_Changed
     (Selection : access Gtk_Tree_Selection_Record'Class);
   --  Replace the contents of Demo_Frame with the demo corresponding to the
   --  currently selected row, and refresh Help_Label with its help text.

   ---------------
   -- To_Markup --
   ---------------

   function To_Markup (Help : String) return String is
      Result : Unbounded_String;
      I      : Positive := Help'First;
   begin
      while I <= Help'Last loop
         if I < Help'Last and then Help (I) = '@' and then Help (I + 1) = 'b'
         then
            Append (Result, "<b>");
            I := I + 2;
         elsif I < Help'Last
           and then Help (I) = '@'
           and then Help (I + 1) = 'B'
         then
            Append (Result, "</b>");
            I := I + 2;
         else
            case Help (I) is
               when '&'    =>
                  Append (Result, "&amp;");

               when '<'    =>
                  Append (Result, "&lt;");

               when '>'    =>
                  Append (Result, "&gt;");

               when others =>
                  Append (Result, Help (I));
            end case;
            I := I + 1;
         end if;
      end loop;
      return To_String (Result);
   end To_Markup;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Selection : access Gtk_Tree_Selection_Record'Class)
   is
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter = Null_Iter then
         return;
      end if;

      declare
         Index : constant Integer :=
           Integer (Get_Int (Model, Iter, Demo_Column));
      begin
         Demo_Frame.Set_Child (null);
         if Index in Demos'Range and then Demos (Index).Run /= null then
            Demos (Index).Run (Demo_Frame);
         end if;

         if Index in Demos'Range and then Demos (Index).Help /= null then
            Help_Label.Set_Markup (To_Markup (Demos (Index).Help.all));
         else
            Help_Label.Set_Text ("");
         end if;
      end;
   end On_Selection_Changed;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate (Self : access GApplication_Record'Class) is
      App_Win  : Gtk_Application_Window;
      Paned    : Gtk_Paned;
      Scrolled : Gtk_Scrolled_Window;
      Tree     : Gtk_Tree_View;
      Store    : Gtk_List_Store;
      Render   : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      Iter     : Gtk_Tree_Iter;
      Dummy    : Gint;
   begin
      Gtk_New (App_Win, Gtk_Application (Self));
      App_Win.Set_Title ("GtkAda Demo");
      App_Win.Set_Default_Size (800, 600);

      Gtk_New (Paned, Orientation_Horizontal);
      App_Win.Set_Child (Paned);

      Gtk_New (Scrolled);
      Scrolled.Set_Has_Frame (True);
      Paned.Set_Start_Child (Scrolled);
      Paned.Set_Resize_Start_Child (False);

      Gtk_New
        (Store, (Label_Column => GType_String, Demo_Column => GType_Int));
      for Index in Demos'Range loop
         Store.Append (Iter);
         Store.Set
           (Iter,
            Label_Column,
            Ada.Strings.Unbounded.To_String (Demos (Index).Label));
         Store.Set (Iter, Demo_Column, Gint (Index));
      end loop;

      Gtk_New (Tree, +Store);
      Tree.Set_Headers_Visible (False);
      Scrolled.Set_Child (Tree);

      Gtk_New (Col);
      Gtk_New (Render);
      Col.Pack_Start (Render, Expand => True);
      Col.Add_Attribute (Render, "text", Label_Column);
      Dummy := Tree.Append_Column (Col);

      declare
         Right_Box  : Gtk_Box;
         Help_Frame : Gtk_Frame;
      begin
         Gtk_New (Right_Box, Orientation_Vertical, Spacing => 0);
         Right_Box.Set_Homogeneous (False);
         Paned.Set_End_Child (Right_Box);

         Gtk_New (Demo_Frame);
         Demo_Frame.Set_Vexpand (True);
         Right_Box.Append (Demo_Frame);

         Gtk_New (Help_Frame, "Help");
         Gtk_New (Help_Label);
         Help_Label.Set_Wrap (True);
         Help_Label.Set_Xalign (0.0);
         Help_Label.Set_Margin_Start (5);
         Help_Label.Set_Margin_End (5);
         Help_Label.Set_Margin_Top (5);
         Help_Label.Set_Margin_Bottom (5);
         Help_Frame.Set_Child (Help_Label);
         Right_Box.Append (Help_Frame);
      end;
      Paned.Set_Position (170);

      Get_Selection (Tree).Set_Mode (Selection_Single);
      Get_Selection (Tree).On_Changed (On_Selection_Changed'Access);

      --  Select the first row so the demo frame is populated on startup.
      Iter := Get_Iter_First (+Store);
      if Iter /= Null_Iter then
         Get_Selection (Tree).Select_Iter (Iter);
      end if;

      App_Win.Present;
   end On_Activate;

end Main_Windows;
