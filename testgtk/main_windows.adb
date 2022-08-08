------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 1998-2022, AdaCore                     --
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

with Glib;                use Glib;
with Glib.Properties;     use Glib.Properties;
with Gtk;                 use Gtk;
with Gdk;                 use Gdk;
with Gdk.Color;           use Gdk.Color;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtk.Hbutton_Box;     use Gtk.Hbutton_Box;
with Gtk.Label;           use Gtk.Label;
with Gtk.Main;            use Gtk.Main;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Text_Tag;        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;  use Gtk.Text_Tag_Table;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_Store;         use Gtk.Tree_Store;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Window;          use Gtk.Window;
with Pango.Font;          use Pango.Font;

with Ada.Strings.Fixed;

with Create_About;
with Create_Alignment;
with Create_Application;
with Create_Arrow;
with Create_Assistant;
with Create_Box;
with Create_Builder;
with Create_Gtkada_Builder;
with Create_Button_Box;
with Create_Buttons;
with Create_Cairo;
with Create_Calendar;
with Create_Canvas;
with Create_Canvas_View_Animate;
with Create_Canvas_View_Composite;
with Create_Canvas_View_Edit;
with Create_Canvas_View_Events;
with Create_Canvas_View_Items;
with Create_Canvas_View_Links;
with Create_Canvas_View_Minimap;
with Create_Canvas_View_Routes;
with Create_Canvas_View_Rtrees;
with Create_Cell_View;
with Create_Check_Buttons;
with Create_Clipboard;
with Create_Color_Chooser;
with Create_Color_Selection;
with Create_Combo_Box;
with Create_Cursors;
with Create_Dialog;
with Create_Dnd;
with Create_Entry;
with Create_Frame;
with Create_File_Chooser;
with Create_File_Selection;
with Create_Fixed;
with Create_Flow_Box;
with Create_Font_Chooser;
with Create_Font_Selection;
with Create_Gestures;
with Create_Gtkada_Dialog;
with Create_Icon_View;
with Create_Label;
with Create_Layout;
with Create_Link_Buttons;
with Create_Main_Loop;
with Create_Menu;
with Create_MDI;
with Create_Notebook;
with Create_Opacity;
with Create_Paned;
with Create_Pixbuf;
with Create_Print;
with Create_Progress;
with Create_Radio_Button;
with Create_Range;
with Create_Reparent;
with Create_Revealer;
with Create_Scrolled;
with Create_Selection;
with Create_Size_Groups;
with Create_Stack;
with Create_Sources;
with Create_Spin;
with Create_Spinners;
with Create_Splittable;
with Create_Status;
with Create_Status_Icons;
with Create_Task_Monitor;
with Create_Test_Idle;
with Create_Test_Timeout;
with Create_Text_View;
with Create_Toggle_Buttons;
with Create_Toolbar;
with Create_Tooltips;
with Create_Tree_Filter;
with Create_Tree_View;
with Create_UI_Manager;
with Common; use Common;
with Create_Css_Accordion;
with Create_Css_Editor;

with Libart_Demo;  use Libart_Demo;

package body Main_Windows is

   procedure Fill_Gtk_Tree
     (Tree         : Gtk.Tree_Store.Gtk_Tree_Store;
      Gtkada_Demos : Boolean := False;
      Pixbuf_Demos : Boolean := False);
   --  Creates the tree that contains the list of gtk demos available

   procedure Display_Help (Window : access Gtk_Widget_Record'Class);
   --  Display an Help window for the current demo

   package Notebook_Cb is new Gtk.Handlers.Callback (Gtk_Notebook_Record);

   Help_Dialog : aliased Gtk.Dialog.Gtk_Dialog;
   Help_Text   : Gtk.Text_Buffer.Gtk_Text_Buffer;
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

   type String_Access is access String;
   function NS (S : String) return String_Access;

   procedure Tree_Select_Child
      (View : access Gtk_Widget_Record'Class);
   --  Callbacks when a different item in the tree is selected.

   package Window_Cb is new Handlers.Callback (Gtk_Widget_Record);
   package Return_Window_Cb is new Handlers.Return_Callback
     (Gtk_Widget_Record, Boolean);
   procedure Exit_Main (Object : access Gtk_Widget_Record'Class);
   --  Callbacks when the main window is killed
   function Delete_Event
     (Object : access Gtk_Widget_Record'Class) return Boolean;

   procedure Destroy_Help (Button : access Gtk_Widget_Record'Class);
   function Opengl_Help return String;

   procedure Switch_Page
      (Notebook : access Gtk_Notebook_Record'Class);
   --  Called when a new notebook page is selected

   procedure Create_Demo_Frame
     (Win   : access Main_Window_Record'Class;
      Page  : Integer;
      Title : String;
      Gtkada_Demo, Pixbuf_Demo : Boolean);
   --  Create the main demo frame

   type Demo_Type is
     (Box, Base, Complex, Colors_And_Fonts, GdkD, Gtkada, Misc, CSS, Pixbuf,
      Cairo);
   --  The available types for demos.
   --  Each of them is a tree item, whose subitems are the matching demos.
   --  Box:     Containers
   --  Base:    Basic widgets, found in all GUI toolkits
   --  Complex: More interesting widgets
   --  Colors_And_Fonts:    Widgets developped for gimp, that could be reused
   --  Misc:    Demonstrates some features that are not widgets
   --  Misc:    Demonstrates features about CSS styling
   --  Gtkada:  Widgets specific to GtkAda
   --  Pixbuf:  Demonstrate the use of images
   --  Cairo:   Low-level manipulation of pixels.

   type Tree_Item_Information is record
      Label  : String_Access;
      Typ    : Demo_Type;
      Func   : Demo_Function;
      Help   : Help_Function;
   end record;

   function NS (S : String) return String_Access is
   begin
      return new String'(S);
   end NS;

   type Tree_Item_Array is array (Positive range <>) of Tree_Item_Information;
   --  The list of items to put in the tree for the gtk demos

   Gtk_Demos : constant Tree_Item_Array :=
     ((NS ("about dialog"),     Base,    Create_About.Run'Access,
                                         Create_About.Help'Access),
      (NS ("alignment (obsolescent)"),
                                Box,     Create_Alignment.Run'Access,
                                         Create_Alignment.Help'Access),
      (NS ("animation"),        Pixbuf,  Create_Pixbuf.Run'Access,
                                         Create_Pixbuf.Help'Access),
      (NS ("animated gif"),     Pixbuf,  Create_Pixbuf.Run_Gif'Access,
                                         Create_Pixbuf.Help_Gif'Access),
      (NS ("application"),      Complex, Create_Application.Run'Access,
                                         Create_Application.Help'Access),
      (NS ("arrow (obsolescent)"),
                                Base,    Create_Arrow.Run'Access,
                                         Create_Arrow.Help'Access),
      (NS ("assistant"),        Complex, Create_Assistant.Run'Access,
                                         Create_Assistant.Help'Access),
      (NS ("box"),              Box,     Create_Box.Run'Access,
                                         Create_Box.Help'Access),
      (NS ("builder"),          Complex, Create_Builder.Run'Access,
                                         Create_Builder.Help'Access),
      (NS ("gtkada builder"),   Complex, Create_Gtkada_Builder.Run'Access,
                                         Create_Gtkada_Builder.Help'Access),
      (NS ("button box"),       Box,     Create_Button_Box.Run'Access,
                                         Create_Button_Box.Help'Access),
      (NS ("buttons"),          Base,    Create_Buttons.Run'Access,
                                         Create_Buttons.Help'Access),
      (NS ("calendar"),         Base,    Create_Calendar.Run'Access,
                                         Create_Calendar.Help'Access),

      (NS ("Simple rectangles"), Cairo,
       Create_Cairo.Run_Rectangles'Access, Create_Cairo.Help'Access),
      (NS ("Transparency"), Cairo,
       Create_Cairo.Run_Transparency'Access, Create_Cairo.Help'Access),
      (NS ("Compositing operators"), Cairo,
       Create_Cairo.Run_Operators'Access, Create_Cairo.Help'Access),
      (NS ("Translating, rotation and scaling"), Cairo,
       Create_Cairo.Run_Matrix'Access, Create_Cairo.Help'Access),
      (NS ("Direct transformations"), Cairo,
       Create_Cairo.Run_Transformations'Access, Create_Cairo.Help'Access),
      (NS ("Paths"), Cairo,
       Create_Cairo.Run_Paths'Access, Create_Cairo.Help'Access),
      (NS ("Patterns"), Cairo,
       Create_Cairo.Run_Patterns'Access, Create_Cairo.Help'Access),
      (NS ("The 'toy' text API"), Cairo,
       Create_Cairo.Run_Toy_Text'Access, Create_Cairo.Help'Access),
      (NS ("Rendering text with pango"), Cairo,
       Create_Cairo.Run_Pango_Text'Access, Create_Cairo.Help'Access),
      (NS ("Painting and clipping"), Cairo,
       Create_Cairo.Run_Clip_And_Paint'Access, Create_Cairo.Help'Access),
      (NS ("Using surfaces and saving to PNG"), Cairo,
       Create_Cairo.Run_Surface_And_Png'Access, Create_Cairo.Help'Access),
      (NS ("Image as background"), Cairo,
       Create_Cairo.Run_Image'Access, Create_Cairo.Help'Access),

      (NS ("canvas (obsolescent)"),
                                Gtkada,  Create_Canvas.Run'Access,
                                         Create_Canvas.Help'Access),
      (NS ("canvas view (items)"),     Gtkada,
       Create_Canvas_View_Items.Run'Access,
       Create_Canvas_View_Items.Help'Access),
      (NS ("canvas view (composite)"),     Gtkada,
       Create_Canvas_View_Composite.Run'Access,
       Create_Canvas_View_Composite.Help'Access),
      (NS ("canvas view (links)"),     Gtkada,
       Create_Canvas_View_Links.Run'Access,
       Create_Canvas_View_Links.Help'Access),
      (NS ("canvas view (routes)"),     Gtkada,
       Create_Canvas_View_Routes.Run'Access,
       Create_Canvas_View_Routes.Help'Access),
      (NS ("canvas view (events)"),     Gtkada,
       Create_Canvas_View_Events.Run'Access,
       Create_Canvas_View_Events.Help'Access),
      (NS ("canvas view (minimap)"),     Gtkada,
       Create_Canvas_View_Minimap.Run'Access,
       Create_Canvas_View_Minimap.Help'Access),
      (NS ("canvas view (editing)"),     Gtkada,
       Create_Canvas_View_Edit.Run'Access,
       Create_Canvas_View_Edit.Help'Access),
      (NS ("canvas view (large)"),     Gtkada,
       Create_Canvas_View_Rtrees.Run'Access,
       Create_Canvas_View_Rtrees.Help'Access),
      (NS ("canvas view (animation)"),     Gtkada,
       Create_Canvas_View_Animate.Run'Access,
       Create_Canvas_View_Animate.Help'Access),
      (NS ("cell view"),        Complex, Create_Cell_View.Run'Access,
                                         Create_Cell_View.Help'Access),
      (NS ("check buttons"),    Base,    Create_Check_Buttons.Run'Access,
                                         Create_Check_Buttons.Help'Access),
      (NS ("clipboard"),        Misc,    Create_Clipboard.Run'Access,
                                         Create_Clipboard.Help'Access),
      (NS ("color selection"),  Colors_And_Fonts,
                                         Create_Color_Selection.Run'Access,
                                         Create_Color_Selection.Help'Access),
      (NS ("color chooser"),    Colors_And_Fonts,
                                         Create_Color_Chooser.Run'Access,
                                         Create_Color_Chooser.Help'Access),
      (NS ("combo boxes"),      Complex, Create_Combo_Box.Run'Access,
                                         Create_Combo_Box.Help'Access),
      (NS ("cursors"),          Misc,    Create_Cursors.Run'Access,
       Create_Cursors.Help'Access),
      (NS ("CSS accordion"),    CSS,    Create_Css_Accordion.Run'Access,
       Create_Css_Accordion.Help'Access),
      (NS ("CSS editor"),       CSS,    Create_Css_Editor.Run'Access,
       Create_Css_Editor.Help'Access),
      (NS ("dialog"),           Base,    Create_Dialog.Run'Access,
                                         Create_Dialog.Help'Access),
      (NS ("drag-and-drop"),    Complex, Create_Dnd.Run'Access,
                                         Create_Dnd.Help'Access),
      (NS ("entry"),            Base,    Create_Entry.Run'Access,
                                         Create_Entry.Help'Access),
      (NS ("event watcher"),    Misc,    null, null),
      (NS ("file chooser button"), Complex,
                                   Create_File_Chooser.Run_Button'Access,
                                   Create_File_Chooser.Help_Button'Access),
      (NS ("file selection"),   Complex, Create_File_Selection.Run'Access,
                                         Create_File_Selection.Help'Access),
      (NS ("fixed"),            Box,     Create_Fixed.Run'Access,
                                         Create_Fixed.Help'Access),
      (NS ("flow box"), Box,
       Create_Flow_Box.Run'Access, Create_Flow_Box.Help'Access),
      (NS ("font chooser"),
                                         Colors_And_Fonts,
                                         Create_Font_Chooser.Run'Access,
                                         Create_Font_Chooser.Help'Access),
      (NS ("font selection (obsolescent)"),
                                         Colors_And_Fonts,
                                         Create_Font_Selection.Run'Access,
                                         Create_Font_Selection.Help'Access),
      (NS ("frame/aspect frame"), Box,   Create_Frame.Run'Access,
                                         Create_Frame.Help'Access),
      (NS ("gestures"),         Misc,    Create_Gestures.Run'Access,
                                         Create_Gestures.Help'Access),
      (NS ("gtkada dialog"),    Gtkada,  Create_Gtkada_Dialog.Run'Access,
                                         Create_Gtkada_Dialog.Help'Access),
      (NS ("icon view"),        Complex, Create_Icon_View.Run'Access,
                                         Create_Icon_View.Help'Access),
      (NS ("labels"),           Base,    Create_Label.Run'Access,
                                         Create_Label.Help'Access),
      (NS ("layout"),           Box,     Create_Layout.Run'Access,
                                         Create_Layout.Help'Access),
      (NS ("link buttons"),     Base,    Create_Link_Buttons.Run'Access,
                                         Create_Link_Buttons.Help'Access),
      (NS ("menus"),            Base,    Create_Menu.Run'Access,
                                         Create_Menu.Help'Access),
      (NS ("mdi"),              Gtkada,  Create_MDI.Run'Access,
                                         Create_MDI.Help'Access),
      (NS ("mdi (independent perspectives)"),
                                Gtkada,  Create_MDI.Run_Independent'Access,
                                         Create_MDI.Help'Access),
      (NS ("modal window"),     Base,    null, null),
      (NS ("multi paned"),      Gtkada,  Create_Splittable.Run'Access,
                                         Create_Splittable.Help'Access),
      (NS ("notebook"),         Box,     Create_Notebook.Run'Access,
                                         Create_Notebook.Help'Access),
      (NS ("opacity"),          Misc,    Create_Opacity.Run'Access,
                                         Create_Opacity.Help'Access),
      (NS ("panes"),            Box,     Create_Paned.Run'Access,
                                         Create_Paned.Help'Access),
      (NS ("properties"),       Misc,    null, null),
      (NS ("printing"),         Base,    Create_Print.Run'Access,
                                         Create_Print.Help'Access),
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
      (NS ("saved position"),   Misc,    null, null),
      (NS ("scaling/composing"), Pixbuf,  Libart_Demo.Run'Access,
                                         Libart_Demo.Help'Access),
      (NS ("scrolled windows"), Base,    Create_Scrolled.Run'Access,
                                         Create_Scrolled.Help'Access),
      (NS ("selection"),        Complex, Create_Selection.Run'Access,
                                         Create_Selection.Help'Access),
      (NS ("shapes"),           Misc,    null, null),
      (NS ("size groups"),      Box,     Create_Size_Groups.Run'Access,
                                         Create_Size_Groups.Help'Access),
      (NS ("event sources"),    Misc,    Create_Sources.Run'Access,
                                         Create_Sources.Help'Access),
      (NS ("revealer"), Box,
       Create_Revealer.Run'Access, Create_Revealer.Help'Access),
      (NS ("spinbutton"),       Base,    Create_Spin.Run'Access,
                                         Create_Spin.Help'Access),
      (NS ("spinner"),          Base,    Create_Spinners.Run'Access,
                                         Create_Spinners.Help'Access),
      (NS ("stack"), Box, Create_Stack.Run'Access, Create_Stack.Help'Access),
      (NS ("statusbar"),        Base,    Create_Status.Run'Access,
                                         Create_Status.Help'Access),
      (NS ("status icons"),     Base,    Create_Status_Icons.Run'Access,
                                         Create_Status_Icons.Help'Access),
      (NS ("stock icons"),      Pixbuf,  null, null),
      (NS ("test idle"),        Misc,    Create_Test_Idle.Run'Access,
                                         Create_Test_Idle.Help'Access),
      (NS ("test mainloop"),    Misc,    Create_Main_Loop.Run'Access,
                                         Create_Main_Loop.Help'Access),
      (NS ("test selection"),   Misc,    null, null),
      (NS ("test timeout"),     Misc,    Create_Test_Timeout.Run'Access,
                                         Create_Test_Timeout.Help'Access),
      (NS ("test tasking"),     Misc,    Create_Task_Monitor.Run'Access,
                                         Create_Task_Monitor.Help'Access),
      (NS ("text view"),        Complex, Create_Text_View.Run'Access,
                                         Create_Text_View.Help'Access),
      (NS ("toggle buttons"),   Base,    Create_Toggle_Buttons.Run'Access,
                                         Create_Toggle_Buttons.Help'Access),
      (NS ("toolbar"),          Box,     Create_Toolbar.Run'Access,
                                         Create_Toolbar.Help'Access),
      (NS ("tooltips"),         Complex, Create_Tooltips.Run'Access,
                                         Create_Tooltips.Help'Access),
      (NS ("tree filter"),      Complex, Create_Tree_Filter.Run'Access,
                                         Create_Tree_Filter.Help'Access),
      (NS ("tree view"),        Complex, Create_Tree_View.Run'Access,
                                         Create_Tree_View.Help'Access),
      (NS ("UI manager"),       Complex, Create_UI_Manager.Run'Access,
                                         Create_UI_Manager.Help'Access),
      (NS ("WM hints"),         Misc,    null, null)
      );

   -------------------
   -- Fill_Gtk_Tree --
   -------------------

   procedure Fill_Gtk_Tree
     (Tree         : Gtk.Tree_Store.Gtk_Tree_Store;
      Gtkada_Demos : Boolean := False;
      Pixbuf_Demos : Boolean := False)
   is
      Sibling   : Gtk_Tree_Iter := Null_Iter;
      Subtree   : Gtk_Tree_Iter := Null_Iter;
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
            Append
              (Tree_Store => Tree,
               Iter       => Sibling,
               Parent     => Null_Iter);

            case Typ is
               when Box     =>
                  Set (Tree, Sibling, 0, "Containers");
               when Base    =>
                  Set (Tree, Sibling, 0, "Base widgets");
               when Complex =>
                  Set (Tree, Sibling, 0, "Composite widgets");
               when Colors_And_Fonts    =>
                  Set (Tree, Sibling, 0, "Colors and fonts");
               when Misc    =>
                  Set (Tree, Sibling, 0, "Misc. demos");
               when CSS     =>
                  Set (Tree, Sibling, 0, "CSS demos");
               when GdkD    =>
                  Set (Tree, Sibling, 0, "Gdk demos");
               when Cairo   =>
                  Set (Tree, Sibling, 0, "Cairo");
               when Gtkada  =>
                  Set (Tree, Sibling, 0, "GtkAda widgets");
                  Frame_Num := 2;
               when Pixbuf  =>
                  Set (Tree, Sibling, 0, "Images");
                  Frame_Num := 3;
            end case;

            Set (Tree, Sibling, 1, 0);
            Set (Tree, Sibling, 2, 0);

            for Item_Num in Gtk_Demos'Range loop
               if Gtk_Demos (Item_Num).Typ = Typ
                 and then Gtk_Demos (Item_Num).Func /= null
               then
                  Append
                    (Tree_Store => Tree,
                     Iter       => Subtree,
                     Parent     => Sibling);
                  Set (Tree, Subtree, 0, Gtk_Demos (Item_Num).Label.all);
                  Set (Tree, Subtree, 1, Gint (Item_Num));
                  Set (Tree, Subtree, 2, Gint (Frame_Num));
               end if;
            end loop;
         end if;
      end loop;
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

   procedure Display_Help (Window : access Gtk_Widget_Record'Class) is
      Close     : Gtk_Widget;
      Scrolled  : Gtk_Scrolled_Window;
      Label     : Gtk.Label.Gtk_Label;
      View      : Gtk_Text_View;
      Iter, Last : Gtk_Text_Iter;
      Blue_Tag, Tag  : Gtk_Text_Tag;
      Mark       : Gtk_Text_Mark;

      procedure Show_Text_With_Tag
        (Iter : in out Gtk_Text_Iter; Text : String);
      --  Insert Text in the help dialog, using Tag to set the color

      procedure Show_Text_With_Tag
        (Iter : in out Gtk_Text_Iter; Text : String)
      is
         Last : Gtk_Text_Iter;
      begin
         if Mark /= null then
            Move_Mark (Help_Text, Mark, Iter);
         else
            Mark := Create_Mark (Help_Text, "", Iter);
         end if;

         Insert (Help_Text, Iter, Text);
         if Tag /= null then
            Get_Iter_At_Mark (Help_Text, Last, Mark);
            Apply_Tag (Help_Text, Tag, Last, Iter);
         end if;
      end Show_Text_With_Tag;

   begin
      if Help_Dialog = null then
         Gtk_New (Help_Dialog,
                  Title  => "Help on current demo",
                  Parent => Gtk_Window (Window),
                  Flags  => Destroy_With_Parent);
         Help_Dialog.Set_Resizable (True);
         Help_Dialog.Set_Default_Size (640, 450);
         Destroy_Dialog_Handler.Connect
           (Help_Dialog, "destroy",
            Destroy_Dialog_Handler.To_Marshaller (Destroy_Dialog'Access),
            Help_Dialog'Access);

         Set_Spacing (Get_Content_Area (Help_Dialog), 3);

         Gtk_New (Label, "Information on this demo");
         Pack_Start (Get_Content_Area (Help_Dialog), Label, False, True, 0);

         Gtk_New (Scrolled);
         Pack_Start (Get_Content_Area (Help_Dialog), Scrolled, True, True, 0);
         Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

         Gtk_New (Help_Text);
         Gtk_New (View, Help_Text);
         Add (Scrolled, View);
         Set_Editable (View, False);
         Set_Wrap_Mode (View, Wrap_Mode => Wrap_Word);

         Close := Help_Dialog.Add_Button (Text => "Close", Response_Id => 0);
         Widget_Handler.Object_Connect
           (Close, "clicked",
            Widget_Handler.To_Marshaller (Destroy_Help'Access),
            Slot_Object => Help_Dialog);
         Close.Set_Can_Default (True);
         Grab_Default (Close);

         Blue_Tag := Create_Tag (Help_Text, "blue");
         Set_Property (Blue_Tag, Gtk.Text_Tag.Foreground_Property, "blue");

      else
         Get_Start_Iter (Help_Text, Iter);
         Get_End_Iter   (Help_Text, Last);
         Delete (Help_Text, Iter, Last);

         Blue_Tag := Lookup (Get_Tag_Table (Help_Text), "blue");
      end if;

      Get_Start_Iter (Help_Text, Iter);
      Tag := null;

      if Current_Help = null then
         Insert (Help_Text, Iter, "No help available");
      else

         declare
            Help  : constant String := Current_Help.all;
            Pos   : Natural := Help'First;
            First : Natural;
            Blue  : Gdk_Color;
            Newline : constant String := (1 => ASCII.LF);

            Line_End : Natural;
            --  Points to the first character of the next line

         begin
            --  In gtk3, the colors no longer need to be allocated.
            Set_Rgb (Blue, 16#0#, 16#0#, 16#FFFF#);

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
                     Show_Text_With_Tag (Iter, Help (Pos .. Line_End - 1));
                     Pos := Line_End;

                  else
                     Show_Text_With_Tag (Iter, Help (Pos .. First - 1));

                     case Help (First + 1) is
                        when 'b' =>
                           Tag := Blue_Tag;
                           Pos := First + 2;
                        when 'B' =>
                           Tag := null;
                           Pos := First + 2;
                        when others =>
                           Show_Text_With_Tag (Iter, "@");
                           Pos := First + 1;
                     end case;
                  end if;
               end loop;

               Pos := Pos + 1;
               exit when Pos > Help'Last;

               Insert (Help_Text, Iter, Newline);
            end loop;
         end;
      end if;

      Show_All (Help_Dialog);
   end Display_Help;

   -----------------
   --  Exit_Main  --
   -----------------

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Object);
      Gtk.Main.Main_Quit;
   end Exit_Main;

   ------------------
   -- Delete_Event --
   ------------------

   function Delete_Event
     (Object : access Gtk_Widget_Record'Class) return Boolean
   is
      pragma Unreferenced (Object);
   begin
      --  We could return True to force the user to kill the window through the
      --  "Quit" button, as opposed to the icon in the title bar.
      return False;
   end Delete_Event;

   --------------
   -- Set_Help --
   --------------

   procedure Set_Help (Func : Help_Function) is
   begin
      Current_Help := Func;
      if Help_Dialog /= null then
         Display_Help (null);
      end if;
   end Set_Help;

   -----------------------
   -- Tree_Select_Child --
   -----------------------

   procedure Tree_Select_Child
      (View : access Gtk_Widget_Record'Class)
   is
      Model     : Gtk_Tree_Model;
      Iter      : Gtk_Tree_Iter;
      Demo_Num  : Integer;
      Frame_Num : Integer;
      Child     : Gtk_Widget;
   begin
      Get_Selected (Get_Selection (Gtk_Tree_View (View)), Model, Iter);
      if Iter = Null_Iter then
         return;
      end if;

      Demo_Num  := Integer (Get_Int (Model, Iter, 1));
      Frame_Num := Integer (Get_Int (Model, Iter, 2));

      if Demo_Num /= 0 and then Gtk_Demos (Demo_Num).Func /= null then
         --  Remove the current demo from the frame
         Child := Get_Child (Gtk_Demo_Frames (Frame_Num));
         if Child /= null then
            Remove (Gtk_Demo_Frames (Frame_Num), Child);
         end if;

         --  And then insert our own new demo

         Gtk_Demos (Demo_Num).Func (Gtk_Demo_Frames (Frame_Num));
         Set_Help (Gtk_Demos (Demo_Num).Help);
      end if;
   end Tree_Select_Child;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Win : out Main_Window) is
   begin
      Win := new Main_Window_Record;
      Main_Windows.Initialize (Win);
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

   procedure Switch_Page
      (Notebook : access Gtk_Notebook_Record'Class) is
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
      Tree     : Gtk_Tree_View;
      Model    : Gtk_Tree_Store;
      Scrolled : Gtk_Scrolled_Window;
      Paned    : Gtk_Paned;
      Render   : Gtk_Cell_Renderer_Text;
      Col      : Gtk_Tree_View_Column;
      C        : Gint;
      pragma Unreferenced (C);

   begin
      Gtk_New (Frame);
      Gtk_New (Label, Title);
      Append_Page (Win.Notebook, Child => Frame, Tab_Label => Label);

      Gtk_New_Hpaned (Paned);
      Add (Frame, Paned);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Always);
      Pack1 (Paned, Scrolled);

      Gtk_New (Model, (1 => GType_String,
                       2 => GType_Int,
                       3 => GType_Int));
      Gtk_New (Tree, +Model);
      Set_Headers_Visible (Tree, False);

      Modify_Font (Tree, From_String ("Sans 8"));

      Gtk_New (Render);
      Gtk_New (Col);
      C := Append_Column (Tree, Col);

      Pack_Start (Col, Render, Expand => True);
      Add_Attribute (Col, Render, "text", 0);

      Set_Mode (Get_Selection (Tree), Gtk.Enums.Selection_Single);
      Scrolled.Add (Tree);

      Fill_Gtk_Tree (Model, Gtkada_Demo, Pixbuf_Demo);

      Widget_Callback.Object_Connect
        (Get_Selection (Tree), Gtk.Tree_Selection.Signal_Changed,
         Tree_Select_Child'Access, Slot_Object => Tree);

      Gtk_New (Gtk_Demo_Frames (Page));
      Set_Shadow_Type
        (Gtk_Demo_Frames (Page), The_Type => Gtk.Enums.Shadow_None);
      Pack2 (Paned, Gtk_Demo_Frames (Page));

      Set_Position (Paned, 170);
   end Create_Demo_Frame;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Win : access Main_Window_Record'Class) is
      Frame    : Gtk.Frame.Gtk_Frame;
      Label    : Gtk.Label.Gtk_Label;
      Vbox     : Gtk.Box.Gtk_Box;
      Button   : Gtk.Button.Gtk_Button;
      Bbox     : Gtk.Hbutton_Box.Gtk_Hbutton_Box;

   begin
      Gtk.Window.Initialize (Win, Gtk.Enums.Window_Toplevel);
      Set_Default_Size (Win, 800, 600);
      Window_Cb.Connect (Win, "destroy",
                         Window_Cb.To_Marshaller (Exit_Main'Access));
      Return_Window_Cb.Connect
        (Win, "delete_event",
         Return_Window_Cb.To_Marshaller (Delete_Event'Access));

      --  The global box
      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 0);
      Add (Win, Vbox);

      --  Label
      Gtk_New (Label, "GtkAda, the portable Ada GUI");
      Override_Font (Label, From_String ("Helvetica Bold 18"));
      Pack_Start (Vbox, Label, Expand => False, Fill => False, Padding => 10);

      --  Notebook creation
      Gtk_New (Win.Notebook);
      Pack_Start (Vbox, Win.Notebook, Expand => True, Fill => True);
      Notebook_Cb.Connect
        (Win.Notebook, "switch_page", Switch_Page'Access, After => True);

      --  First page: Gtk demos
      Create_Demo_Frame (Win, 1, "Gtk demo", False, False);
      Create_Demo_Frame (Win, 2, "GtkAda demo", True, False);
      Create_Demo_Frame (Win, 3, "Image manipulation", False, True);

      --  Button box for the buttons at the bottom
      --  Gtk_New_Hbox (Bbox, Homogeneous => True, Spacing => 10);
      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Spread);
      Set_Spacing (Bbox, 40);

      Gtk_New (Button, "Help on current demo");
      Pack_Start (Bbox, Button, Expand => True, Fill => False);
      Widget_Handler.Object_Connect
        (Button, "clicked",
         Widget_Handler.To_Marshaller (Display_Help'Access), Win);

      Gtk_New (Button, "Quit");
      Pack_Start (Bbox, Button, Expand => True, Fill => False);
      Window_Cb.Object_Connect
         (Button, "clicked",
          Window_Cb.To_Marshaller (Exit_Main'Access), Win);

      Pack_End (Vbox, Bbox, Expand => False, Padding => 5);

      --  Display everything
      Show_All (Vbox);

   end Initialize;
end Main_Windows;
