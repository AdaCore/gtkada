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

with Glib;                use Glib;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Color;           use Gdk.Color;
with Gdk.Font;            use Gdk.Font;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Drawing_Area;    use Gtk.Drawing_Area;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;            use Gtk.Main;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Radio_Button;    use Gtk.Radio_Button;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Signal;          use Gtk.Signal;
with Gtk.Style;           use Gtk.Style;
with Gtk.Text;            use Gtk.Text;
with Gtk.Toolbar;         use Gtk.Toolbar;
with Gtk.Tree;            use Gtk.Tree;
with Gtk.Tree_Item;       use Gtk.Tree_Item;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;

with Interfaces.C.Strings;
with Ada.Strings.Fixed;

with Create_Arrow;
with Create_Button_Box;
with Create_Buttons;
with Create_Calendar;
with Create_Check_Buttons;
with Create_Clist;
with Create_Ctree;
with Create_Color_Selection;
with Create_Cursors;
with Create_Dialog;
with Create_Entry;
with Create_File_Selection;
with Create_Font_Selection;
with Create_Gamma_Curve;
with Create_Handle_Box;
with Create_List;
with Create_Main_Loop;
with Create_Menu;
with Create_Notebook;
with Create_Paned;
with Create_Pixmap;
with Create_Preview_Color;
with Create_Preview_Gray;
with Create_Progress;
with Create_Radio_Button;
with Create_Range;
with Create_Reparent;
with Create_Rulers;
with Create_Scrolled;
with Create_Scroll_Test;
with Create_Spin;
with Create_Status;
with Create_Test_Idle;
with Create_Test_Timeout;
with Create_Text;
with Create_Toggle_Buttons;
with Create_Toolbar;
with Create_Tooltips;
with Create_Tree;
with Xpm;
with Common; use Common;
with View_GL; use View_GL;

package body Main_Windows is

   procedure Fill_Gtk_Tree (Tree : in out Gtk.Tree.Gtk_Tree);
   --  Creates the tree that contains the list of gtk demos available

   function Create_Gdk_Toolbar (Frame : Gtk.Frame.Gtk_Frame)
                               return Gtk_Toolbar;
   --  Create the toolbar used for the gdk demo

   function New_Pixmap (Icon   : Interfaces.C.Strings.chars_ptr_array;
                        Window : access Gtk_Widget_Record'Class)
                       return Gtk_Pixmap;
   --  Create a new icon from a file

   procedure Display_Help (Button : access Gtk_Widget_Record);
   --  Display an Help window for the current demo

   Help_Dialog : Gtk.Dialog.Gtk_Dialog;
   Help_Text   : Gtk.Text.Gtk_Text;
   --  The dialog used to display the help window

   type Demo_Function is
     access procedure (Frame : access Gtk_Frame_Record'Class);
   --  The type of function to call when an item in the tree is selected.
   --  The parameter is the frame in which the demo should be displayed

   type Help_Function is
     access function return String;
   Current_Help : Help_Function := null;
   --  Returns the help string to display,
   --  Symbols between @b and @B are displayed in bold
   --  New lines should be represented by ASCII.LF

   type String_Access is access String;
   function NS (S : String) return String_Access is
   begin
      return new String'(S);
   end NS;
   --  Access to strings

   type Demo_Tree_Item_Record is new Gtk_Tree_Item_Record with
      record
         Demo_Num  : Natural;
      end record;
   type Demo_Tree_Item is access all Demo_Tree_Item_Record'Class;
   procedure Gtk_New (Item  : out Demo_Tree_Item;
                      Label : String;
                      Num   : Natural);
   procedure Initialize (Item  : access Demo_Tree_Item_Record'Class;
                         Label : String;
                         Num   : Natural);
   --  New definition for tree items, so that they know which demo function
   --  to call.

   package Tree_Cb is new Gtk.Signal.Two_Callback_Gtk
     (Base_Type => Gtk.Tree.Gtk_Tree_Record,
      Data_Type => Integer,
      Cb_Type   => Demo_Tree_Item_Record);
   procedure Tree_Select_Child (Tree : access Gtk_Tree_Record;
                                Item : access Demo_Tree_Item_Record;
                                Data : Integer);
   --  Callbacks when a different item in the tree is selected.

   package Window_Callback is new Gtk.Signal.Void_Callback
     (Base_Type => Main_Window_Record);
   procedure Exit_Main (Object : access Main_Window_Record);
   --  Callbacks when the main window is killed

   type Demo_Type is (Box, Base, Complex, Gimp, Misc);
   --  The available types for demos. Each of them is a tree item, whose subitems
   --  are the matching demos.
   --  Box:     Containers
   --  Base:    Basic widgets, found in all GUI toolkits
   --  Complex: More interesting widgets
   --  Gimp:    Widgets developped for gimp, that could be reused
   --  Misc:    Demonstrates some features that are not widgets

   type Tree_Item_Information is
      record
         Label  : String_Access;
         Typ    : Demo_Type;
         Func   : Demo_Function;
         Help   : Help_Function;
      end record;
   type Tree_Item_Array is array (Positive range <>) of Tree_Item_Information;
   --  The list of items to put in the tree for the gtk demos


   Gtk_Demos : constant Tree_Item_Array :=
     ((NS ("arrow"),            Base,    Create_Arrow.Run'Access,
                                         Create_Arrow.Help'Access),
      (NS ("button box"),       Box,     Create_Button_Box.Run'Access,
                                         Create_Button_Box.Help'Access),
      (NS ("buttons"),          Base,    Create_Buttons.Run'Access, null),
      (NS ("calendar"),         Base,    Create_Calendar.Run'Access, null),
      (NS ("check buttons"),    Base,    Create_Check_Buttons.Run'Access, null),
      (NS ("clist"),            Complex, Create_Clist.Run'Access, null),
      (NS ("ctree"),            Complex, Create_Ctree.Run'Access, null),
      (NS ("color selection"),  Gimp,    Create_Color_Selection.Run'Access, null),
      (NS ("cursors"),          Misc,    Create_Cursors.Run'Access, null),
      (NS ("dialog"),           Base,    Create_Dialog.Run'Access, null),
      (NS ("dnd"),              Complex, null, null),
      (NS ("entry"),            Base,    Create_Entry.Run'Access, null),
      (NS ("event watcher"),    Misc,    null, null),
      (NS ("file selection"),   Complex, Create_File_Selection.Run'Access, null),
      (NS ("font selection"),   Gimp,    Create_Font_Selection.Run'Access, null),
      (NS ("gamma curve"),      Gimp,    Create_Gamma_Curve.Run'Access, null),
      (NS ("handle box"),       Box,     Create_Handle_Box.Run'Access, null),
      (NS ("item factory"),     Complex, null, null),
      (NS ("labels"),           Base,    null, null),
      (NS ("layout"),           Box,     null, null),
      (NS ("list"),             Base,    Create_List.Run'Access, null),
      (NS ("menus"),            Base,    Create_Menu.Run'Access, null),
      (NS ("modal window"),     Base,    null, null),
      (NS ("notebook"),         Box,     Create_Notebook.Run'Access, null),
      (NS ("panes"),            Box,     Create_Paned.Run'Access, null),
      (NS ("pixmap"),           Base,    Create_Pixmap.Run'Access, null),
      (NS ("preview color"),    Gimp,    Create_Preview_Color.Run'Access, null),
      (NS ("preview gray"),     Gimp,    Create_Preview_Gray.Run'Access, null),
      (NS ("progress bar"),     Complex, Create_Progress.Run'Access, null),
      (NS ("radio buttons"),    Base,    Create_Radio_Button.Run'Access, null),
      (NS ("range controls"),   Base,    Create_Range.Run'Access, null),
      (NS ("rc file"),          Misc,    null, null),
      (NS ("reparent"),         Complex, Create_Reparent.Run'Access, null),
      (NS ("rulers"),           Gimp,    Create_Rulers.Run'Access, null),
      (NS ("saved position"),   Misc,    null, null),
      (NS ("scrolled windows"), Base,    Create_Scrolled.Run'Access, null),
      (NS ("shapes"),           Misc,    null, null),
      (NS ("spinbutton"),       Base,    Create_Spin.Run'Access, null),
      (NS ("statusbar"),        Base,    Create_Status.Run'Access, null),
      (NS ("test idle"),        Misc,    Create_Test_Idle.Run'Access, null),
      (NS ("test mainloop"),    Misc,    Create_Main_Loop.Run'Access, null),
      (NS ("test scrolling"),   Misc,    Create_Scroll_Test.Run'Access, null),
      (NS ("test selection"),   Misc,    null, null),
      (NS ("test timeout"),     Misc,    Create_Test_Timeout.Run'Access, null),
      (NS ("text"),             Complex, Create_Text.Run'Access, null),
      (NS ("toggle buttons"),   Base,    Create_Toggle_Buttons.Run'Access, null),
      (NS ("toolbar"),          Box,     Create_Toolbar.Run'Access, null),
      (NS ("tooltips"),         Complex, Create_Tooltips.Run'Access, null),
      (NS ("tree"),             Complex, Create_Tree.Run'Access, null),
      (NS ("WM hints"),         Misc,    null, null)
      );

   -------------------
   -- Fill_Gtk_Tree --
   -------------------

   procedure Fill_Gtk_Tree (Tree : in out Gtk.Tree.Gtk_Tree) is
      Item_Subtree : Gtk_Tree;
      Item_New     : Demo_Tree_Item;
      Item         : Gtk_Tree_Item;
      Id           : Guint;

   begin
      for Typ in Demo_Type'Range loop
         case Typ is
            when Box     => Gtk_New (Item, "Containers");
            when Base    => Gtk_New (Item, "Basic Widgets");
            when Complex => Gtk_New (Item, "Composite Widgets");
            when Gimp    => Gtk_New (Item, "Gimp Widgets");
            when Misc    => Gtk_New (Item, "Misc. Demos");
            when others  => Gtk_New (Item, Demo_Type'Image (Typ));
         end case;
         Append (Tree, Item);

         Gtk_New (Item_Subtree);

         for Item_Num in Gtk_Demos'Range loop
            if Gtk_Demos (Item_Num).Typ = Typ
              and then Gtk_Demos (Item_Num).Func /= null
            then
               Gtk_New (Item_New,
                        Label => Gtk_Demos (Item_Num).Label.all,
                        Num   => Item_Num);
               Append (Item_Subtree, Item_New);
               Show (Item_New);
            end if;
         end loop;

         Set_Subtree (Item, Item_Subtree);
         Id := Tree_Cb.Connect (Item_Subtree, "select_child",
                                Tree_Select_Child'Access, 0);
      end loop;
   end Fill_Gtk_Tree;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help (Button : access Gtk_Widget_Record) is
      Close     : Gtk.Button.Gtk_Button;
      Id        : Guint;
      Scrolled  : Gtk_Scrolled_Window;
      Label     : Gtk.Label.Gtk_Label;

   begin
      if Current_Help = null then
         return;
      end if;

      if Help_Dialog = null then
         Gtk_New (Help_Dialog);
         Set_Policy (Help_Dialog, Allow_Shrink => True, Allow_Grow => True,
                     Auto_Shrink => True);
         Set_Title (Help_Dialog, "testgtk help");
         Set_Usize (Help_Dialog, 400, 250);

         Set_Spacing (Get_Vbox (Help_Dialog), 3);

         Gtk_New (Label, "Information on this demo");
         Pack_Start (Get_Vbox (Help_Dialog), Label, False, True, 0);

         Gtk_New (Scrolled);
         Pack_Start (Get_Vbox (Help_Dialog), Scrolled, True, True, 0);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

         Gtk_New (Help_Text);
         Add (Scrolled, Help_Text);
         Set_Editable (Help_Text, False);
         Set_Style (Help_Text, Get_Style (Help_Dialog));

         Gtk_New (Close, "Close");
         Pack_Start (Get_Action_Area (Help_Dialog), Close, False, False);
         Id := Widget_Cb.Connect (Close, "clicked", Hide'Access, Help_Dialog);

      else
         Delete_Text (Help_Text, 0, -1);
      end if;

      Freeze (Help_Text);
      declare
         Help  : constant String := Current_Help.all;
         Pos   : Natural := Help'First;
         First : Natural;
         Blue  : Gdk_Color;
         Black : constant Gdk_Color
           := Gdk.Color.Black (Get_Colormap (Help_Text));
         Current_Color : Gdk_Color := Black;
         Newline : constant String := (1 => ASCII.LF);
         Max_Length : constant := 70;

         Line_End : Natural;
         --  Points to the first character of the next line

      begin
         Set_Rgb (Blue, 16#0#, 16#0#, 16#FFFF#);

         loop

            --  The end of the line can be at most Max_Length character,
            --  finishing at the first previous white space. Stops at the
            --  first Newline encountered if any

            if Pos + Max_Length < Help'Last then
               Line_End := Ada.Strings.Fixed.Index
                 (Help (Pos .. Pos + Max_Length), " ", Ada.Strings.Backward);
            else
               Line_End := Help'Last + 1;
            end if;

            First := Ada.Strings.Fixed.Index
              (Help (Pos .. Line_End - 1), Newline);
            if First /= 0 then
               Line_End := First;
            end if;

            --  Scan and print the line

            while Pos < Line_End loop

               --  Any special sections to highlight ?

               First := Ada.Strings.Fixed.Index
                 (Help (Pos .. Line_End - 1), "@");

               if First = 0 or First = Line_End - 1 then
                  Insert (Help_Text, Null_Font, Current_Color, Null_Color,
                          Help (Pos .. Line_End - 1), -1);
                  Pos := Line_End;

               else
                  Insert (Help_Text, Null_Font, Current_Color, Null_Color,
                          Help (Pos .. First - 1), -1);

                  case Help (First + 1) is
                     when 'b' =>
                        Current_Color := Blue;
                        Pos := First + 2;
                     when 'B' =>
                        Current_Color := Black;
                        Pos := First + 2;
                     when others =>
                        Insert (Help_Text, Null_Font, Current_Color, Null_Color,
                                "@", 1);
                        Pos := First + 1;
                  end case;
               end if;
            end loop;

            Pos := Pos + 1;
            exit when Pos > Help'Last;
            Insert (Help_Text, Null_Font, Null_Color, Null_Color, Newline, 1);
         end loop;
      end;

      Thaw (Help_Text);
      Show_All (Help_Dialog);
   end Display_Help;

   ----------------
   -- New_Pixmap --
   ----------------

   function New_Pixmap (Icon   : Interfaces.C.Strings.chars_ptr_array;
                        Window : access Gtk_Widget_Record'Class)
                       return Gtk_Pixmap
   is
      Pixmap    : Gdk_Pixmap;
      Mask      : Gdk_Bitmap;
      GtkPixmap : Gtk_Pixmap;
   begin
      Create_From_Xpm_D (Pixmap, Get_Window (Window), Mask, Null_Color, Icon);
      Gtk_New (GtkPixmap, Pixmap, Mask);
      return GtkPixmap;
   end New_Pixmap;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Item  : out Demo_Tree_Item;
                      Label : String;
                      Num   : Natural)
   is
   begin
      Item := new Demo_Tree_Item_Record;
      Initialize (Item, Label, Num);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item  : access Demo_Tree_Item_Record'Class;
                         Label : String;
                         Num   : Natural)
   is
   begin
      Gtk.Tree_Item.Initialize (Item, Label);
      Item.Demo_Num := Num;
   end Initialize;

   -----------------
   --  Exit_Main  --
   -----------------

   procedure Exit_Main (Object : access Main_Window_Record) is
   begin
      Gtk.Main.Main_Quit;
   end Exit_Main;

   -----------------------
   -- Tree_Select_Child --
   -----------------------

   procedure Tree_Select_Child (Tree : access Gtk_Tree_Record;
                                Item : access Demo_Tree_Item_Record;
                                Data : Integer)
   is
      use Gtk.Widget.Widget_List;
      List : Gtk.Widget.Widget_List.Glist;
   begin
      if Gtk_Demos (Item.Demo_Num).Func /= null then

         --  Remove the current demo from the frame

         List := Gtk.Frame.Children (Gtk_Demo_Frame);
         if Length (List) /= 0 then
            Gtk.Frame.Remove (Container => Gtk_Demo_Frame,
                              Widget    => Get_Data (List));
         end if;

         --  And then insert our own new demo

         Gtk_Demos (Item.Demo_Num).Func (Gtk_Demo_Frame);
         Current_Help := Gtk_Demos (Item.Demo_Num).Help;
         if Help_Dialog /= null then
            declare
               W : aliased Gtk_Widget_Record;
            begin
               Display_Help (W'Access);
            end;
         end if;
      end if;
   end Tree_Select_Child;

   ------------------------
   -- Create_Gdk_Toolbar --
   ------------------------

   function Create_Gdk_Toolbar (Frame : Gtk.Frame.Gtk_Frame)
                               return Gtk_Toolbar
   is

      Tool_Group : Widget_SList.GSlist;
      Toolbox    : Gtk_Toolbar;

      procedure Add_Tool (Toolbar   : access Gtk_Toolbar_Record'Class;
                          Tooltips  : String;
                          Icon_File : Interfaces.C.Strings.chars_ptr_array)
      is
         Button : Gtk_Radio_Button;
         Widget : Gtk_Widget;
         --  Id     : Guint;
         Pixmap : Gtk_Pixmap;

      begin
         Gtk_New (Button, Tool_Group);
         Tool_Group := Gtk.Radio_Button.Group (Button);
         Set_Mode (Button, Draw_Indicator => False);
         Set_Active (Button, Is_Active => False);
         Set_Border_Width (Button, 0);

         Pixmap := New_Pixmap (Icon_File, Frame);
         Show (Pixmap);

         Add (Button, Pixmap);
         Show (Button);

         Widget := Append_Element (Toolbar, Toolbar_Child_Widget, Button,
                                   "", Tooltips, "", Pixmap);

         --  Id := Tool_Cb.Connect (Button, "clicked", Execute_Tool_Cb'Access, Cmd);
      end Add_Tool;

   begin
      Realize (Frame);
      Gtk_New (Toolbox,
               Orientation => Gtk.Enums.Orientation_Horizontal,
               Style       => Gtk.Enums.Toolbar_Icons);
      Add_Tool (Toolbox, "Select object", Xpm.Select_Xpm);
      Add_Tool (Toolbox, "Rotate object", Xpm.Rotate_Xpm);
      Add_Tool (Toolbox, "Insert text", Xpm.Text_Xpm);
      Add_Tool (Toolbox, "insert line", Xpm.Line_Xpm);
      Add_Tool (Toolbox, "Insert ellispe", Xpm.Ellipse_Xpm);
      Add_Tool (Toolbox, "Insert circle", Xpm.Circle_Xpm);
      Add_Tool (Toolbox, "Insert rectangle", Xpm.Rectangle_Xpm);
      return Toolbox;
   end Create_Gdk_Toolbar;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Win : out Main_Window) is
   begin
      Win := new Main_Window_Record;
      Initialize (Win);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Win : access Main_Window_Record'Class) is
      Frame    : Gtk.Frame.Gtk_Frame;
      Label    : Gtk.Label.Gtk_Label;
      Box      : Gtk.Box.Gtk_Box;
      Vbox     : Gtk.Box.Gtk_Box;
      Tree     : Gtk.Tree.Gtk_Tree;
      Scrolled : Gtk_Scrolled_Window;
      Cb_Id    : Guint;
      Font     : Gdk.Font.Gdk_Font;
      Style    : Gtk_Style;
      Drawing_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Button   : Gtk.Button.Gtk_Button;

   begin
      Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Cb_Id := Window_Callback.Connect (Obj  => Win,
                                        Name => "destroy",
                                        Func => Exit_Main'Access);
      Cb_Id := Window_Callback.Connect (Obj  => Win,
                                        Name => "delete_event",
                                        Func => Exit_Main'Access);

      --  The global box
      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Win, Vbox);

      --  Label
      Style := Copy (Get_Style (Win));
      Load (Font, "-adobe-helvetica-bold-*-*-*-*-190-*-*-*-*-*-*");
      Set_Font (Style, Font);

      Gtk_New (Label, "GtkAda, the portable Ada95 GUI");
      Set_Style (Label, Style);
      Pack_Start (Vbox, Label, Expand => True, Fill => True, Padding => 10);
      Show (Label);

      --  Notebook creation
      Gtk_New (Win.Notebook);
      Pack_Start (Vbox, Win.Notebook, Expand => False, Fill => False);

      --  First page: Gtk demos
      Gtk_New (Frame);
      Gtk_New (Label, "Gtk demo");
      Append_Page (Win.Notebook, Child => Frame, Tab_Label => Label);

      Gtk.Box.Gtk_New_Hbox (Box, Homogeneous => False, Spacing => 0);
      Gtk.Frame.Add (Frame, Widget => Box);
      Set_Usize (Box, 700, 600);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Pack_Start (In_Box  => Box,
                  Child   => Vbox,
                  Expand  => True,
                  Fill    => True,
                  Padding => 0);

      Gtk_New (Button, "Help on current demo");
      Pack_Start (In_Box => Vbox,
                  Child  => Button,
                  Expand => False,
                  Fill   => False);
      Cb_Id := Widget3_Cb.Connect (Button, "clicked", Display_Help'Access);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Always);
      Pack_Start (In_Box  => VBox,
                  Child   => Scrolled,
                  Expand  => True,
                  Fill    => True,
                  Padding => 0);
      Set_Usize (Scrolled, 150, 600);

      Gtk_New (Tree);
      Set_Selection_Mode (Tree, Gtk.Enums.Selection_Single);
      Set_View_Lines (Tree, True);
      Add_With_Viewport (Scrolled, Tree);
      Fill_Gtk_Tree (Tree);

      Gtk_New (Gtk_Demo_Frame);
      Set_Shadow_Type (Gtk_Demo_Frame, The_Type => Gtk.Enums.Shadow_None);
      Pack_End (In_Box  => Box,
                Child   => Gtk_Demo_Frame,
                Expand  => True,
                Fill    => True,
                Padding => 0);
      Set_Usize (Gtk_Demo_Frame, 550, 600);

      --  Second page: Gdk demos
      Gtk_New (Frame);
      Gtk_New (Label, "Gdk demo");
      Append_Page (Win.Notebook, Child => Frame, Tab_Label => Label);

      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 0);
      Add (Frame, Box);

      Pack_Start (Box, Create_Gdk_Toolbar (Frame), Expand => False, Fill => False);

      Gtk_New (Frame, "Drawing Demo");
      Pack_Start (Box, Frame, Expand => True, Fill => True);

      Gtk_New (Drawing_Area);
      Add (Frame, Drawing_Area);

      Show_All (Box);

      --  Third page: OpenGL demos
      Gtk_New (Frame);
      Gtk_New (Label, "OpenGL demo");
      Append_Page (Win.Notebook, Frame, Label);

      View_GL.Run (Frame);

   end Initialize;
end Main_Windows;
