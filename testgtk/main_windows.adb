-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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
with Gtk;                 use Gtk;
with Gdk;                 use Gdk;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Color;           use Gdk.Color;
with Gdk.Font;            use Gdk.Font;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.HButton_Box;     use Gtk.HButton_Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;            use Gtk.Main;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Style;           use Gtk.Style;
with Gtk.Text;            use Gtk.Text;
with Gtk.Clist;           use Gtk.Clist;
with Gtk.Ctree;           use Gtk.Ctree;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;
with Pango.Font;          use Pango.Font;

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Strings.Fixed;

with Create_Alignment;
with Create_Arrow;
with Create_Box;
with Create_Button_Box;
with Create_Buttons;
with Create_Calendar;
with Create_Canvas;
with Create_Check_Buttons;
with Create_Clist;
with Create_Ctree;
with Create_Color_Selection;
with Create_Cursors;
with Create_Dialog;
with Create_Dnd;
with Create_Entry;
with Create_Frame;
with Create_File_Selection;
with Create_Fixed;
with Create_Font_Selection;
with Create_Gamma_Curve;
with Create_Gc;
with Create_Handle_Box;
with Create_Item_Factory;
with Create_Label;
with Create_Layout;
with Create_List;
with Create_Main_Loop;
with Create_Menu;
with Create_Notebook;
with Create_Paned;
with Create_Pixbuf;
with Create_Pixmap;
--  XXX ???
--  with Create_Plot;
--  with Create_Plot_3D;
--  with Create_Plot_Realtime;
with Create_Preview_Color;
with Create_Preview_Gray;
with Create_Progress;
with Create_Radio_Button;
with Create_Range;
with Create_Reparent;
with Create_Rulers;
with Create_Scrolled;
with Create_Scroll_Test;
with Create_Selection;
with Create_Size_Groups;
--  with Create_Sheet;
with Create_Spin;
with Create_Status;
with Create_Test_Idle;
with Create_Test_Timeout;
with Create_Text;
with Create_Toggle_Buttons;
with Create_Toolbar;
with Create_Tooltips;
--  with Create_Tree;
with Common; use Common;
with View_GL; use View_GL;

with Libart_Demo;  use Libart_Demo;

with Ada.Text_IO; use Ada.Text_IO;

package body Main_Windows is

   procedure Fill_Gtk_Tree
     (Tree         : in out Gtk.Ctree.Gtk_Ctree;
      Gtkada_Demos : Boolean := False;
      Pixbuf_Demos : Boolean := False);
   --  Creates the tree that contains the list of gtk demos available

   function New_Pixmap
     (Icon   : Interfaces.C.Strings.chars_ptr_array;
      Window : access Gtk_Widget_Record'Class) return Gtk_Pixmap;
   --  Create a new icon from a file

   procedure Display_Help (Button : access Gtk_Widget_Record'Class);
   --  Display an Help window for the current demo

   package Notebook_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Notebook_Record, Gtk_Notebook);

   Help_Dialog : Gtk.Dialog.Gtk_Dialog;
   Help_Text   : Gtk.Text.Gtk_Text;
   --  The dialog used to display the help window

   Gtk_Demo_Frames  : array (1 .. 3) of Gtk.Frame.Gtk_Frame;
   --  Frames where the gtk demos should be displayed.

   type Demo_Function is
     access procedure (Frame : access Gtk_Frame_Record'Class);
   --  The type of function to call when an item in the tree is selected.
   --  The parameter is the frame in which the demo should be displayed

   Current_Help : Help_Function := null;
   --  Returns the help string to display,
   --  Symbols between @b and @B are displayed in bold
   --  New lines should be represented by ASCII.LF

   function NS (S : String) return chars_ptr renames New_String;

   type Demo_Tree_Item is record
      Demo_Num : Natural;
      Frame    : Integer;
   end record;

   package Tree_Data is new Gtk.Clist.Row_Data (Demo_Tree_Item);

   package Tree_Cb is new Gtk.Handlers.Callback
     (Widget_Type => Gtk.Ctree.Gtk_Ctree_Record);

   procedure Tree_Select_Child (Tree : access Gtk_Ctree_Record'Class);
   --  Callbacks when a different item in the tree is selected.

   package Window_Cb is new Handlers.Callback (Gtk_Widget_Record);
   package Return_Window_Cb is new Handlers.Return_Callback
     (Gtk_Widget_Record, Boolean);
   procedure Exit_Main (Object : access Gtk_Widget_Record'Class);
   --  Callbacks when the main window is killed
   function Delete_Event
     (Object : access Gtk_Widget_Record'Class) return Boolean;

   type Demo_Type is (Box, Base, Complex, Gimp, GdkD, Gtkada, Misc, Pixbuf);
   --  The available types for demos.
   --  Each of them is a tree item, whose subitems are the matching demos.
   --  Box:     Containers
   --  Base:    Basic widgets, found in all GUI toolkits
   --  Complex: More interesting widgets
   --  Gimp:    Widgets developped for gimp, that could be reused
   --  Misc:    Demonstrates some features that are not widgets
   --  Gtkada:  Widgets specific to GtkAda
   --  Pixbuf:  Demonstrate the use of images

   type Tree_Item_Information is record
      Label  : chars_ptr;
      Typ    : Demo_Type;
      Func   : Demo_Function;
      Help   : Help_Function;
   end record;

   type Tree_Item_Array is array (Positive range <>) of Tree_Item_Information;
   --  The list of items to put in the tree for the gtk demos

   Gtk_Demos : constant Tree_Item_Array :=
     ((NS ("alignment"),        Box,     Create_Alignment.Run'Access,
                                         Create_Alignment.Help'Access),
      (NS ("animation"),        Pixbuf,  Create_Pixbuf.Run'Access,
                                         Create_Pixbuf.Help'Access),
      (NS ("animated gif"),     Pixbuf,  Create_Pixbuf.Run_Gif'Access,
                                         Create_Pixbuf.Help_Gif'Access),
      (NS ("arrow"),            Base,    Create_Arrow.Run'Access,
                                         Create_Arrow.Help'Access),
      (NS ("box"),              Box,     Create_Box.Run'Access,
                                         Create_Box.Help'Access),
      (NS ("button box"),       Box,     Create_Button_Box.Run'Access,
                                         Create_Button_Box.Help'Access),
      (NS ("buttons"),          Base,    Create_Buttons.Run'Access,
                                         Create_Buttons.Help'Access),
      (NS ("calendar"),         Base,    Create_Calendar.Run'Access,
                                         Create_Calendar.Help'Access),
      (Ns ("canvas"),           Gtkada,  Create_Canvas.Run'Access,
                                         Create_Canvas.Help'Access),
      (NS ("check buttons"),    Base,    Create_Check_Buttons.Run'Access,
                                         Create_Check_Buttons.Help'Access),
      (NS ("clist"),            Complex, Create_Clist.Run'Access,
                                         Create_Clist.Help'Access),
      (NS ("ctree"),            Complex, Create_Ctree.Run'Access,
                                         Create_Ctree.Help'Access),
      (NS ("color selection"),  Gimp,    Create_Color_Selection.Run'Access,
                                         Create_Color_Selection.Help'Access),
      (NS ("cursors"),          Misc,    Create_Cursors.Run'Access,
                                         Create_Cursors.Help'Access),
      (NS ("dialog"),           Base,    Create_Dialog.Run'Access,
                                         Create_Dialog.Help'Access),
      (NS ("drag-and-drop"),    Complex, Create_Dnd.Run'Access,
                                         Create_Dnd.Help'Access),
      (NS ("entry"),            Base,    Create_Entry.Run'Access,
                                         Create_Entry.Help'Access),
      (NS ("event watcher"),    Misc,    null, null),
      (NS ("file selection"),   Complex, Create_File_Selection.Run'Access,
                                         Create_File_Selection.Help'Access),
      (NS ("fixed"),            Box,     Create_Fixed.Run'Access,
                                         Create_Fixed.Help'Access),
      (NS ("font selection"),   Gimp,    Create_Font_Selection.Run'Access,
                                         Create_Font_Selection.Help'Access),
      (NS ("frame/aspect frame"), Box,   Create_Frame.Run'Access,
                                         Create_Frame.Help'Access),
      (NS ("gamma curve"),      Gimp,    Create_Gamma_Curve.Run'Access,
                                         Create_Gamma_Curve.Help'Access),
      (Ns ("graphic contexts"), GdkD,    Create_Gc.Run'Access,
                                         Create_Gc.Help'Access),
      (NS ("handle box"),       Box,     Create_Handle_Box.Run'Access,
                                         Create_Handle_Box.Help'Access),
      (NS ("item factory"),     Complex, Create_Item_Factory.Run'Access,
                                         Create_Item_Factory.Help'Access),
      (NS ("labels"),           Base,    Create_Label.Run'Access,
                                         Create_Label.Help'Access),
      (NS ("layout"),           Box,     Create_Layout.Run'Access,
                                         Create_Layout.Help'Access),
      (NS ("list"),             Base,    Create_List.Run'Access,
                                         Create_List.Help'Access),
      (NS ("menus"),            Base,    Create_Menu.Run'Access,
                                         Create_Menu.Help'Access),
      (NS ("modal window"),     Base,    null, null),
      (NS ("notebook"),         Box,     Create_Notebook.Run'Access,
                                         Create_Notebook.Help'Access),
      (NS ("panes"),            Box,     Create_Paned.Run'Access,
                                         Create_Paned.Help'Access),
      (NS ("pixmap"),           Base,    Create_Pixmap.Run'Access,
                                         Create_Pixmap.Help'Access),
      --  (NS ("plot"),             Complex, Create_Plot.Run'Access,
      --                                     Create_Plot.Help'Access),
      --  (NS ("plot 3D"),          Complex, Create_Plot_3D.Run'Access,
      --                                     Create_Plot_3D.Help'Access),
      --  (NS ("plot realtime"),    Complex, Create_Plot_Realtime.Run'Access,
      --                                     Create_Plot_Realtime.Help'Access),
      (NS ("properties"),       Misc,    null, null),
      (NS ("preview color"),    Gimp,    Create_Preview_Color.Run'Access,
                                         Create_Preview_Color.Help'Access),
      (NS ("preview gray"),     Gimp,    Create_Preview_Gray.Run'Access,
                                         Create_Preview_Gray.Help'Access),
      (NS ("progress bar"),     Complex, Create_Progress.Run'Access,
                                         Create_Progress.Help'Access),
      (NS ("progressive loading"), Pixbuf, null, null),
      (NS ("radio buttons"),    Base,    Create_Radio_Button.Run'Access,
                                         Create_Radio_Button.Help'Access),
      (NS ("range controls"),   Base,    Create_Range.Run'Access,
                                         Create_Range.Help'Access),
      (NS ("rc file"),          Misc,    null, null),
      (NS ("reparent"),         Complex, Create_Reparent.Run'Access,
                                         Create_Reparent.Help'Access),
      (NS ("rulers"),           Gimp,    Create_Rulers.Run'Access,
                                         Create_Rulers.Help'Access),
      (NS ("saved position"),   Misc,    null, null),
      (NS ("scaling/composing"), Pixbuf,  Libart_Demo.Run'Access,
                                         Libart_Demo.Help'Access),
      (NS ("scrolled windows"), Base,    Create_Scrolled.Run'Access,
                                         Create_Scrolled.Help'Access),
      (NS ("selection"),        Complex, Create_Selection.Run'Access,
                                         Create_Selection.Help'Access),
      (NS ("shapes"),           Misc,    null, null),
      --  (NS ("sheet"),            Complex, Create_Sheet.Run'Access,
      --                                     Create_Sheet.Help'Access),
      (NS ("size groups"),      Box,     Create_Size_Groups.Run'Access,
                                         Create_Size_Groups.Help'Access),
      (NS ("spinbutton"),       Base,    Create_Spin.Run'Access,
                                         Create_Spin.Help'Access),
      (NS ("statusbar"),        Base,    Create_Status.Run'Access,
                                         Create_Status.Help'Access),
      (NS ("stock icons"),      Pixbuf,  null, null),
      (NS ("test idle"),        Misc,    Create_Test_Idle.Run'Access,
                                         Create_Test_Idle.Help'Access),
      (NS ("test mainloop"),    Misc,    Create_Main_Loop.Run'Access,
                                         Create_Main_Loop.Help'Access),
      (NS ("test scrolling"),   Misc,    Create_Scroll_Test.Run'Access,
                                         Create_Scroll_Test.Help'Access),
      (NS ("test selection"),   Misc,    null, null),
      (NS ("test timeout"),     Misc,    Create_Test_Timeout.Run'Access,
                                         Create_Test_Timeout.Help'Access),
      (NS ("text"),             Complex, Create_Text.Run'Access,
                                         Create_Text.Help'Access),
      (NS ("text view"),        Complex, null, null),
      (NS ("toggle buttons"),   Base,    Create_Toggle_Buttons.Run'Access,
                                         Create_Toggle_Buttons.Help'Access),
      (NS ("toolbar"),          Box,     Create_Toolbar.Run'Access,
                                         Create_Toolbar.Help'Access),
      (NS ("tooltips"),         Complex, Create_Tooltips.Run'Access,
                                         Create_Tooltips.Help'Access),
      --  (NS ("tree"),             Complex, Create_Tree.Run'Access,
      --                                     Create_Tree.Help'Access),
      (NS ("WM hints"),         Misc,    null, null)
      );

   -------------------
   -- Fill_Gtk_Tree --
   -------------------

   procedure Fill_Gtk_Tree
     (Tree         : in out Gtk.Ctree.Gtk_Ctree;
      Gtkada_Demos : Boolean := False;
      Pixbuf_Demos : Boolean := False)
   is
      Sibling   : Gtk.Ctree.Gtk_Ctree_Node;
      Subtree   : Gtk.Ctree.Gtk_Ctree_Node;
      Text      : chars_ptr;
      Frame_Num : Integer := 1;
   begin
      for Typ in Demo_Type'Range loop
         if ((not Gtkada_Demos)
             and then not Pixbuf_Demos
             and then Typ /= Gtkada
             and then Typ /= Pixbuf)
           or else (Gtkada_Demos and then Typ = Gtkada)
           or else (Pixbuf_Demos and then Typ = Pixbuf)
         then
            case Typ is
               when Box     => Text := New_String ("Containers");
               when Base    => Text := New_String ("Basic Widgets");
               when Complex => Text := New_String ("Composite Widgets");
               when Gimp    => Text := New_String ("Gimp Widgets");
               when Misc    => Text := New_String ("Misc. Demos");
               when GdkD    => Text := New_String ("Gdk demos");
               when Gtkada  =>
                  Text := New_String ("GtkAda Widgets");
                  Frame_Num := 2;
               when Pixbuf  =>
                  Text := New_String ("Images");
                  Frame_Num := 3;
               when others  =>
                  Text := New_String (Demo_Type'Image (Typ));
            end case;

            Sibling := Gtk.Ctree.Insert_Node
              (Tree,
               Parent => null,
               Sibling => null,
               Text => (1 => Text),
               Spacing => 5,
               Pixmap_Closed => Gdk.Pixmap.Null_Pixmap,
               Mask_Closed => Gdk.Bitmap.Null_Bitmap,
               Pixmap_Opened => Gdk.Pixmap.Null_Pixmap,
               Mask_Opened => Gdk.Bitmap.Null_Bitmap,
               Is_Leaf => False,
               Expanded => False);
            Free (Text);

            for Item_Num in Gtk_Demos'Range loop
               if Gtk_Demos (Item_Num).Typ = Typ
                 and then Gtk_Demos (Item_Num).Func /= null
               then
                  Subtree := Gtk.Ctree.Insert_Node
                    (Tree,
                     Parent => Sibling,
                     Sibling => null,
                     Text => (1 => Gtk_Demos (Item_Num).Label),
                     Spacing => 5,
                     Pixmap_Closed => Gdk.Pixmap.Null_Pixmap,
                     Mask_Closed => Gdk.Bitmap.Null_Bitmap,
                     Pixmap_Opened => Gdk.Pixmap.Null_Pixmap,
                     Mask_Opened => Gdk.Bitmap.Null_Bitmap,
                     Is_Leaf => True,
                     Expanded => False);
                  Tree_Data.Set
                    (Tree, Gtk_Clist_Row (Node_Get_Row (Subtree)),
                     (Demo_Num => Item_Num,
                      Frame    => Frame_Num));
               end if;
            end loop;
         end if;
      end loop;

      Tree_Cb.Connect
        (Tree, "tree_select_row",
         Tree_Cb.To_Marshaller (Tree_Select_Child'Access));
   end Fill_Gtk_Tree;

   ------------------
   -- Destroy_Help --
   ------------------

   procedure Destroy_Help (Button : access Gtk_Widget_Record'Class) is
      pragma Warnings (Off, Button);
   begin
      Destroy (Help_Dialog);
      Help_Dialog := null;
   end Destroy_Help;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help (Button : access Gtk_Widget_Record'Class) is
      Close     : Gtk.Button.Gtk_Button;
      Scrolled  : Gtk_Scrolled_Window;
      Label     : Gtk.Label.Gtk_Label;

   begin
      if Help_Dialog = null then
         Gtk_New (Help_Dialog);
         Set_Policy (Help_Dialog, Allow_Shrink => True, Allow_Grow => True,
                     Auto_Shrink => True);
         Set_Title (Help_Dialog, "testgtk help");
         Set_Default_Size (Help_Dialog, 400, 250);
         --  Set_Usize (Help_Dialog, 400, 250);

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
         Set_Word_Wrap (Help_Text, Word_Wrap => True);

         Gtk_New (Close, "Close");
         Pack_Start (Get_Action_Area (Help_Dialog), Close, False, False);
         Widget_Handler.Object_Connect
           (Close, "clicked",
            Widget_Handler.To_Marshaller (Destroy_Help'Access),
            Slot_Object => Help_Dialog);
         Set_Flags (Close, Can_Default);
         Grab_Default (Close);

      else
         Delete_Text (Help_Text, 0, -1);
      end if;

      Freeze (Help_Text);

      if Current_Help = null then
         Insert (Help_Text, Null_Font,
                 Null_Color, Null_Color,
                 "No help available", -1);
      else

         declare
            Help  : constant String := Current_Help.all;
            Pos   : Natural := Help'First;
            First : Natural;
            Blue  : Gdk_Color;
            Current_Color : Gdk_Color := Null_Color;
            Newline : constant String := (1 => ASCII.LF);

            Line_End : Natural;
            --  Points to the first character of the next line

         begin
            Set_Rgb (Blue, 16#0#, 16#0#, 16#FFFF#);
            Alloc (Get_Default_Colormap, Blue);

            loop

               --  The end of the line can be at most Max_Length character,
               --  finishing at the first previous white space. Stops at the
               --  first Newline encountered if any

               Line_End := Help'Last + 1;

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
                           Current_Color := Gdk.Color.Null_Color;
                           Pos := First + 2;
                        when others =>
                           Insert
                             (Help_Text, Null_Font, Current_Color, Null_Color,
                              "@", 1);
                           Pos := First + 1;
                     end case;
                  end if;
               end loop;

               Pos := Pos + 1;
               exit when Pos > Help'Last;
               Insert
                 (Help_Text, Null_Font, Null_Color, Null_Color, Newline, 1);
            end loop;
         end;
      end if;

      Thaw (Help_Text);
      Show_All (Help_Dialog);
   end Display_Help;

   ----------------
   -- New_Pixmap --
   ----------------

   function New_Pixmap
     (Icon   : Interfaces.C.Strings.chars_ptr_array;
      Window : access Gtk_Widget_Record'Class) return Gtk_Pixmap
   is
      Pixmap    : Gdk.Gdk_Pixmap;
      Mask      : Gdk.Gdk_Bitmap;
      GtkPixmap : Gtk_Pixmap;
   begin
      Create_From_Xpm_D (Pixmap, Get_Window (Window), Mask, Null_Color, Icon);
      Gtk_New (GtkPixmap, Pixmap, Mask);
      return GtkPixmap;
   end New_Pixmap;

   -----------------
   --  Exit_Main  --
   -----------------

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end Exit_Main;

   ------------------
   -- Delete_Event --
   ------------------

   function Delete_Event (Object : access Gtk_Widget_Record'Class)
                         return Boolean
   is
   begin
      --  Do not allow the user to kill the window by clicking on the icon,
      --  he has to press explicitly "Quit"
      return True;
   end Delete_Event;

   --------------
   -- Set_Help --
   --------------

   procedure Set_Help (Func : Help_Function) is
   begin
      Current_Help := Func;
      if Help_Dialog /= null then
         declare
            W : aliased Gtk_Widget_Record;
         begin
            Display_Help (W'Access);
         end;
      end if;
   end Set_Help;

   -----------------------
   -- Tree_Select_Child --
   -----------------------

   procedure Tree_Select_Child (Tree : access Gtk_Ctree_Record'Class) is
      use Gtk.Widget.Widget_List;
      List : Gtk.Widget.Widget_List.Glist;
      Node : constant Gtk_Ctree_Node := Node_List.Get_Data
        (Node_List.First (Get_Selection (Tree)));
      Item : Demo_Tree_Item;
   begin
      if Row_Get_Is_Leaf (Node_Get_Row (Node)) then
         Item := Tree_Data.Get (Tree, Gtk_Clist_Row (Node_Get_Row (Node)));

         if Gtk_Demos (Item.Demo_Num).Func /= null then

            --  Remove the current demo from the frame

            List := Gtk.Frame.Get_Children (Gtk_Demo_Frames (Item.Frame));

            if Length (List) /= 0 then
               Gtk.Frame.Remove
                 (Container => Gtk_Demo_Frames (Item.Frame),
                  Widget    => Get_Data (List));
            end if;

            --  And then insert our own new demo

            Gtk_Demos (Item.Demo_Num).Func (Gtk_Demo_Frames (Item.Frame));
            Set_Help (Gtk_Demos (Item.Demo_Num).Help);
         end if;
      end if;
   end Tree_Select_Child;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Win : out Main_Window) is
   begin
      Win := new Main_Window_Record;
      Initialize (Win);
   end Gtk_New;

   -----------------
   -- OpenGL_Help --
   -----------------

   function Opengl_Help return String is
   begin
      return "This demo shows how you can use GtkAda to display an @bOpenGL@B"
        & " widget. GtkAda provides a special window in which you can display"
        & " any kind of OpenGL drawing." & ASCII.LF
        & "GtkAda comes with a very basic binding to OpenGL (@bMesa@B), but"
        & " you can use any binding you want, including win32 on Windows."
        & ASCII.LF & ASCII.LF
        & "To use this demo, try moving the demo with: " & ASCII.LF
        & "   - Left mouse button: rotate the drawing." & ASCII.LF
        & "   - Middle mouse button: zoom the drawing.";
   end Opengl_Help;

   -----------------
   -- Switch_Page --
   -----------------

   procedure Switch_Page (Notebook : access Gtk_Notebook_Record'Class;
                          Page     : in Gtk.Gtk_Notebook_Page;
                          User     : in Gtk_Notebook)
   is
      pragma Warnings (Off, Page);
      pragma Warnings (Off, User);
   begin
      if Get_Current_Page (Notebook) = 3 then
         Set_Help (Opengl_Help'Access);
      else
         Set_Help (null);
      end if;
   end Switch_Page;

   -----------------------
   -- Create_Demo_Frame --
   -----------------------

   procedure Create_Demo_Frame
     (Win   : access Main_Window_Record'Class;
      Page  : Integer;
      Title : String;
      Gtkada_Demo, Pixbuf_Demo : Boolean)
   is
      Frame    : Gtk.Frame.Gtk_Frame;
      Label    : Gtk.Label.Gtk_Label;
      Box      : Gtk.Box.Gtk_Box;
      Vbox2    : Gtk.Box.Gtk_Box;
      Tree     : Gtk.Ctree.Gtk_Ctree;
      Scrolled : Gtk_Scrolled_Window;

   begin
      Gtk_New (Frame);
      Gtk_New (Label, Title);
      Append_Page (Win.Notebook, Child => Frame, Tab_Label => Label);

      Gtk.Box.Gtk_New_Hbox (Box, Homogeneous => False, Spacing => 0);
      Gtk.Frame.Add (Frame, Widget => Box);

      Gtk_New_Vbox (Vbox2, Homogeneous => False, Spacing => 0);
      Pack_Start (In_Box  => Box,
                  Child   => Vbox2,
                  Expand  => True,
                  Fill    => True,
                  Padding => 0);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Always);
      Pack_Start (In_Box  => VBox2,
                  Child   => Scrolled,
                  Expand  => True,
                  Fill    => True,
                  Padding => 0);
      Set_Usize (Scrolled, 170, 500);

      Gtk_New (Tree, 1);
      Set_Selection_Mode (Tree, Gtk.Enums.Selection_Single);
      Add_With_Viewport (Scrolled, Tree);
      Fill_Gtk_Tree (Tree, Gtkada_Demo, Pixbuf_Demo);

      Gtk_New (Gtk_Demo_Frames (Page));
      Set_Shadow_Type (Gtk_Demo_Frames (Page), The_Type => Gtk.Enums.Shadow_None);
      Pack_End (In_Box  => Box,
                Child   => Gtk_Demo_Frames (Page),
                Expand  => True,
                Fill    => True,
                Padding => 0);
      Set_Usize (Gtk_Demo_Frames (Page), 550, 500);
   end Create_Demo_Frame;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Win : access Main_Window_Record'Class) is
      Frame    : Gtk.Frame.Gtk_Frame;
      Label    : Gtk.Label.Gtk_Label;
      Vbox     : Gtk.Box.Gtk_Box;
      Style    : Gtk_Style;
      Button   : Gtk.Button.Gtk_Button;
      Bbox     : Gtk.Hbutton_Box.Gtk_Hbutton_Box;

   begin
      Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Window_Cb.Connect (Win, "destroy",
                         Window_Cb.To_Marshaller (Exit_Main'Access));
      Return_Window_Cb.Connect
        (Win, "delete_event",
         Return_Window_Cb.To_Marshaller (Delete_Event'Access));

      --  The global box
      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Win, Vbox);

      --  Label
      Style := Copy (Get_Style (Win));
      Set_Font_Description (Style, From_String ("Helvetica Bold 18"));

      Gtk_New (Label, "GtkAda, the portable Ada95 GUI");
      Set_Style (Label, Style);
      Pack_Start (Vbox, Label, Expand => False, Fill => False, Padding => 10);

      --  Notebook creation
      Gtk_New (Win.Notebook);
      Pack_Start (Vbox, Win.Notebook, Expand => True, Fill => True);
      Notebook_Cb.Connect
        (Win.Notebook, "switch_page",
         Notebook_Cb.To_Marshaller (Switch_Page'Access),
         Win.Notebook,
         After => True);

      --  First page: Gtk demos
      Create_Demo_Frame (Win, 1, "Gtk demo", False, False);
      Create_Demo_Frame (Win, 2, "GtkAda demo", True, False);
      Create_Demo_Frame (Win, 3, "Image manipulation", False, True);

      --  Fourth page: OpenGL demos
      Gtk_New (Frame);
      Gtk_New (Label, "OpenGL demo");
      Append_Page (Win.Notebook, Frame, Label);

      View_GL.Run (Frame);

      --  Button box for the buttons at the bottom
      -- Gtk_New_Hbox (Bbox, Homogeneous => True, Spacing => 10);
      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Spread);
      Set_Spacing (Bbox, 40);

      Gtk_New (Button, "Help on current demo");
      Pack_Start (Bbox, Button, Expand => True, Fill => False);
      Widget_Handler.Connect
        (Button, "clicked",
         Widget_Handler.To_Marshaller (Display_Help'Access));

      Gtk_New (Button, "Quit");
      Pack_Start (Bbox, Button, Expand => True, Fill => False);
      Window_Cb.Connect (Button, "clicked",
                         Window_Cb.To_Marshaller (Exit_Main'Access));

      Pack_End (Vbox, Bbox, Expand => False, Padding => 5);

      --  Display everything
      Show_All (Vbox);

   end Initialize;
end Main_Windows;
