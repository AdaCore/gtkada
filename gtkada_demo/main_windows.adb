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

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Gtk.Application;             use Gtk.Application;
with Gtk.Application_Window;      use Gtk.Application_Window;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Css_Provider;            use Gtk.Css_Provider;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Frame;                   use Gtk.Frame;
with Gtk.Label;                   use Gtk.Label;
with Gtk.List_Item;               use Gtk.List_Item;
with Gtk.List_View;               use Gtk.List_View;
with Gtk.Paned;                   use Gtk.Paned;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Selection_Model;
with Gtk.Signal_List_Item_Factory; use Gtk.Signal_List_Item_Factory;
with Gtk.Single_Selection;        use Gtk.Single_Selection;
with Gtk.Style_Context;           use Gtk.Style_Context;
with Gtk.Style_Provider;          use Gtk.Style_Provider;
with Gtk.Tree_Expander;           use Gtk.Tree_Expander;
with Gtk.Tree_List_Model;         use Gtk.Tree_List_Model;
with Gtk.Tree_List_Row;           use Gtk.Tree_List_Row;
with Gtk.Widget;                  use Gtk.Widget;

with Demo_Items;                  use Demo_Items;
with Demo_Registry;

--  TRANSITION: the original `with` clauses are preserved below as
--  comments. Uncomment each one as the corresponding binding becomes
--  available again.
--
--  with Glib.Properties;     use Glib.Properties;
--  with Gtk;                 use Gtk;
--  with Gdk;                 use Gdk;
--  with Gdk.Color;           use Gdk.Color;
--  with Gtk.Handlers;        use Gtk.Handlers;
--  with Gtkada.Handlers;     use Gtkada.Handlers;
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

--  TRANSITION: the demos themselves now live in Demo_Registry, which is the
--  single place to edit when a binding comes back. The ones still waiting
--  for theirs are:
--
--  Create_Application, Create_Builder, Create_GL, Create_Gtkada_Builder,
--  Create_Cairo, Create_Calendar, Create_Canvas, Create_Canvas_View_Animate,
--  Create_Canvas_View_Composite, Create_Canvas_View_Edit,
--  Create_Canvas_View_Events, Create_Canvas_View_Items,
--  Create_Canvas_View_Links, Create_Canvas_View_Minimap,
--  Create_Canvas_View_Routes, Create_Canvas_View_Rtrees, Create_Clipboard,
--  Create_Cursors, Create_Dnd, Create_File_Chooser, Create_File_Selection,
--  Create_Fixed, Create_Flow_Box, Create_Font_Chooser, Create_Gestures,
--  Create_Gtkada_Dialog, Create_Link_Buttons, Create_Main_Loop, Create_MDI,
--  Create_Notebook, Create_Opacity, Create_Pixbuf, Create_Print,
--  Create_Progress, Create_Range, Create_Revealer, Create_Selection,
--  Create_Size_Groups, Create_Stack, Create_Sources, Create_Spinners,
--  Create_Splittable, Create_Task_Monitor, Create_Test_Idle,
--  Create_Css_Editor, Libart_Demo.

package body Main_Windows is

   Css_Filename : constant String := "gtkada_demo.css";

   procedure Load_Css (Window : Gtk_Application_Window);
   --  Load the demo stylesheet and install it for every widget on Window's
   --  display. A missing stylesheet is non-fatal so that the demo can still
   --  be run from a directory where its data files are unavailable.

   --------------
   -- Load_Css --
   --------------

   procedure Load_Css (Window : Gtk_Application_Window) is
      Provider : Gtk_Css_Provider;
   begin
      if not Ada.Directories.Exists (Css_Filename) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "warning: cannot find "
            & Css_Filename
            & "; continuing without custom CSS");
         return;
      end if;

      Gtk_New (Provider);
      Provider.Load_From_Path (Css_Filename);

      Add_Provider_For_Display
        (Window.Get_Display, +Provider, Priority_Application);
      Unref (Provider);
   end Load_Css;

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

   procedure Show_Demo (Item : Demo_Item);
   --  Replace the contents of Demo_Frame with Item's demo, and refresh
   --  Help_Label with its help text.

   procedure Setup_Row
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   --  Build the widget shown in a selector row: a Gtk_Tree_Expander, which
   --  draws the arrow and the indentation, around a label.

   procedure Bind_Row
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class);
   --  Point a recycled selector row at the item it is now to show.

   procedure On_Selection
     (Self     : Gtk.Selection_Model.Gtk_Selection_Model;
      Position : Guint;
      N_Items  : Guint);
   --  Run the newly selected demo, if the selected row is one.

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

   ---------------
   -- Show_Demo --
   ---------------

   procedure Show_Demo (Item : Demo_Item) is
   begin
      Demo_Frame.Set_Child (null);
      Demo_Frame.Set_Label ("");
      Item.Run (Demo_Frame);

      if Item.Help /= null then
         Help_Label.Set_Markup (To_Markup (Item.Help.all));
      else
         Help_Label.Set_Markup ("No help available");
      end if;
   end Show_Demo;

   ---------------
   -- Setup_Row --
   ---------------

   procedure Setup_Row
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Expander : Gtk_Tree_Expander;
      Label    : Gtk_Label;
   begin
      Gtk.Label.Gtk_New (Label, "");
      Label.Set_Xalign (0.0);

      Gtk.Tree_Expander.Gtk_New (Expander);
      Expander.Set_Child (Label);
      Item.Set_Child (Expander);

      --  The expander carries the +, - and arrow key bindings, so the
      --  keyboard focus must reach it rather than stop at the row.
      Item.Set_Focusable (False);
   end Setup_Row;

   --------------
   -- Bind_Row --
   --------------

   procedure Bind_Row
     (Self : access Gtk_Signal_List_Item_Factory_Record'Class;
      Item : not null access Gtk_List_Item_Record'Class)
   is
      pragma Unreferenced (Self);
      Expander : constant Gtk_Tree_Expander :=
        Gtk_Tree_Expander (Item.Get_Child);
      Row      : constant GObject := Item.Get_Item;
   begin
      --  The tree model is not a passthrough one, so what the list item
      --  holds is the Gtk_Tree_List_Row wrapping the demo item -- which is
      --  also the only thing Set_List_Row accepts. The row is null while
      --  the cell is unbound.
      if Row = null then
         return;
      end if;

      --  Handing the row to the expander is all that is needed for the
      --  arrow, the indentation and the expand/collapse gestures: the
      --  expander watches the row from here on.
      Expander.Set_List_Row (Gtk_Tree_List_Row (Row));

      declare
         Object : constant GObject := Gtk_Tree_List_Row (Row).Get_Item;
         Demo   : constant Demo_Item := To_Demo_Item (Object);
      begin
         Gtk_Label (Expander.Get_Child).Set_Text (Title_Of (Object));

         --  A category has nothing to run, so take the click affordance
         --  away. This is cosmetic only: a non-selectable item may still be
         --  selected, and GTK resets the property on every rebind, so the
         --  guard that matters is the one in On_Selection below.
         Item.Set_Selectable (Demo /= null and then Demo.Run /= null);
      end;
   end Bind_Row;

   ------------------
   -- On_Selection --
   ------------------

   procedure On_Selection
     (Self     : Gtk.Selection_Model.Gtk_Selection_Model;
      Position : Guint;
      N_Items  : Guint)
   is
      pragma Unreferenced (Position, N_Items);
      Selection : constant Gtk_Single_Selection := -Self;
      Row       : constant GObject := Selection.Get_Selected_Item;
      Demo      : Demo_Item;
   begin
      --  Null when nothing is selected: this replaces the old Null_Iter
      --  guard.
      if Row = null then
         return;
      end if;

      Demo := To_Demo_Item (Gtk_Tree_List_Row (Row).Get_Item);

      --  THE guard against running a category row. Gtk.List_Item.
      --  Set_Selectable (False) does not stop the selection model selecting
      --  one -- gtk-list_item.ads says so outright, and GTK resets the
      --  property on every rebind -- so the test has to happen here.
      --  Upstream gates the same way, on demo->func /= NULL.
      if Demo = null or else Demo.Run = null then
         return;
      end if;

      Show_Demo (Demo);
   end On_Selection;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate (Self : access GApplication_Record'Class) is
      App_Win   : Gtk_Application_Window;
      Paned     : Gtk_Paned;
      Scrolled  : Gtk_Scrolled_Window;
      Tree      : Gtk_Tree_List_Model;
      Selection : Gtk_Single_Selection;
      Factory   : Gtk_Signal_List_Item_Factory;
      List      : Gtk_List_View;
   begin
      Gtk_New (App_Win, Gtk_Application (Self));
      App_Win.Set_Title ("GtkAda Demo");
      App_Win.Set_Default_Size (800, 600);
      Load_Css (App_Win);

      Gtk_New (Paned, Orientation_Horizontal);
      App_Win.Set_Child (Paned);

      Gtk_New (Scrolled);
      Scrolled.Set_Has_Frame (True);
      Paned.Set_Start_Child (Scrolled);
      Paned.Set_Resize_Start_Child (False);

      --  Passthrough => False is load-bearing: it is what makes the items of
      --  the model Gtk_Tree_List_Rows, which is the only thing the expanders
      --  built by Setup_Row accept. Autoexpand => True opens the tree on
      --  startup, so that the first demo can be shown without a category
      --  having to be expanded first.
      --
      --  Each of the three constructors below is transfer-full on what it
      --  wraps, and each is handed something freshly made, so -- unlike
      --  Demo_Registry.Children_Of -- no reference is taken here.
      Gtk.Tree_List_Model.Gtk_New
        (Tree,
         Root        => Demo_Registry.Root_Model,
         Passthrough => False,
         Autoexpand  => True,
         Create_Func => Demo_Registry.Children_Of'Access);

      Selection := Gtk_Single_Selection_New (+Tree);

      Gtk.Signal_List_Item_Factory.Gtk_New (Factory);
      Factory.On_Setup (Setup_Row'Access);
      Factory.On_Bind (Bind_Row'Access);

      Gtk.List_View.Gtk_New (List, +Selection, Factory);
      Scrolled.Set_Child (List);

      declare
         Right_Box  : Gtk_Box;
         Help_Frame : Gtk_Frame;
      begin
         Gtk_New (Right_Box, Orientation_Vertical, Spacing => 0);
         Right_Box.Set_Homogeneous (False);
         Paned.Set_End_Child (Right_Box);

         Gtk_New (Demo_Frame);
         Demo_Frame.Set_Name ("demo-frame");
         Demo_Frame.Set_Vexpand (True);
         Right_Box.Append (Demo_Frame);

         --  A titled Gtk_Frame reports inconsistent cross-axis sizes when its
         --  child wraps. Keep the visible heading in Help_Label's markup.
         Gtk_New (Help_Frame);
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

      --  Select the first demo, so that the demo frame is populated on
      --  startup. Row 0 is a category now, hence the walk; Autoexpand above
      --  is what guarantees its children are already in the model. The
      --  selection is made -- and shown by hand -- before the handler is
      --  connected, so that the demo is built exactly once whether or not
      --  Set_Selected emits selection-changed.
      declare
         Position : Guint := 0;
         Row      : Gtk_Tree_List_Row;
         Demo     : Demo_Item;
      begin
         while Position < Tree.Get_N_Items loop
            Row := Tree.Get_Row (Position);
            Demo :=
              (if Row = null then null else To_Demo_Item (Row.Get_Item));

            if Demo /= null and then Demo.Run /= null then
               Selection.Set_Selected (Position);
               Show_Demo (Demo);
               exit;
            end if;

            Position := Position + 1;
         end loop;
      end;

      Gtk.Selection_Model.On_Selection_Changed
        (+Selection, On_Selection'Access);

      App_Win.Present;
   end On_Activate;

end Main_Windows;
