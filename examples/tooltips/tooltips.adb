-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                   Copyright (C) 2009, AdaCore                     --
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

--  This program demonstraites how to use tooltips for different widgets.

with Glib.Object;
with Glib.Properties;
with Glib.Values;
with Gdk.Color;
with Gtk.Box;
with Gtk.Cell_Renderer_Text;
with Gtk.Check_Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Main;
with Gtk.Stock;
with Gtk.Tooltips;
with Gtk.Tree_Model;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;
with Gtk.Window;

procedure Tooltips is

   function On_Delete_Event
     (Sender : access Gtk.Window.Gtk_Window_Record'Class)
      return Boolean;

   function On_Check_Button_Query_Tooltip
     (Sender : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Params : Glib.Values.GValues)
      return Boolean;
   --  Application defined "query-tooltip" signal handler. Shows how to setup
   --  application defined text and icon.

   function On_Label_Query_Tooltip_Custom
     (Sender : access Gtk.Label.Gtk_Label_Record'Class;
      Params : Glib.Values.GValues)
      return Boolean;
   --  Application defined "query-tooltip" signal handler. Shows how to use
   --  custom tooltip window.

   function On_Tree_View_Query_Tooltip
     (Sender : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Params : Glib.Values.GValues)
      return Boolean;
   --  Application defined "query-tooltip" signal handler. Shows how to setup
   --  tooltips for items in tree view.

   package Gtk_Window_Callbacks is
     new Gtk.Handlers.Return_Callback (Gtk.Window.Gtk_Window_Record, Boolean);

   package Gtk_Check_Button_Callbacks is
     new Gtk.Handlers.Return_Callback
           (Gtk.Check_Button.Gtk_Check_Button_Record, Boolean);

   package Gtk_Label_Callbacks is
     new Gtk.Handlers.Return_Callback (Gtk.Label.Gtk_Label_Record, Boolean);

   package Gtk_Tree_View_Callbacks is
     new Gtk.Handlers.Return_Callback
           (Gtk.Tree_View.Gtk_Tree_View_Record, Boolean);

   -----------------------------------
   -- On_Check_Button_Query_Tooltip --
   -----------------------------------

   function On_Check_Button_Query_Tooltip
     (Sender : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Params : Glib.Values.GValues)
      return Boolean
   is
      Stub          : Gtk.Tooltips.Gtk_Tooltips_Record;
      Tooltip       : constant Gtk.Tooltips.Gtk_Tooltips :=
                        Gtk.Tooltips.Gtk_Tooltips
                          (Glib.Object.Get_User_Data
                             (Glib.Values.Get_Address
                                (Glib.Values.Nth (Params, 4)), Stub));

   begin
      Tooltip.Set_Markup (Sender.Get_Label);
      Tooltip.Set_Icon_From_Stock
        (Gtk.Stock.Stock_Delete, Gtk.Enums.Icon_Size_Menu);

      return True;
   end On_Check_Button_Query_Tooltip;

   ---------------------
   -- On_Delete_Event --
   ---------------------

   function On_Delete_Event
     (Sender : access Gtk.Window.Gtk_Window_Record'Class)
      return Boolean
   is
   begin
      Gtk.Main.Main_Quit;

      return True;
   end On_Delete_Event;

   -----------------------------------
   -- On_Label_Query_Tooltip_Custom --
   -----------------------------------

   function On_Label_Query_Tooltip_Custom
     (Sender : access Gtk.Label.Gtk_Label_Record'Class;
      Params : Glib.Values.GValues)
      return Boolean
   is
      Window : constant Gtk.Window.Gtk_Window :=
                 Gtk.Window.Gtk_Window (Sender.Get_Tooltip_Window);
      Color  : Gdk.Color.Gdk_Color;

   begin
      Gdk.Color.Set_Rgb (Color, 0, 0, 65535);
      Window.Modify_Bg (Gtk.Enums.State_Normal, Color);

      return True;
   end On_Label_Query_Tooltip_Custom;

   --------------------------------
   -- On_Tree_View_Query_Tooltip --
   --------------------------------

   function On_Tree_View_Query_Tooltip
     (Sender : access Gtk.Tree_View.Gtk_Tree_View_Record'Class;
      Params : Glib.Values.GValues)
      return Boolean
   is
      X       : Glib.Gint := Glib.Values.Get_Int (Glib.Values.Nth (Params, 1));
      Y       : Glib.Gint := Glib.Values.Get_Int (Glib.Values.Nth (Params, 2));
      Mode    : constant Boolean :=
                  Glib.Values.Get_Boolean (Glib.Values.Nth (Params, 3));
      Stub    : Gtk.Tooltips.Gtk_Tooltips_Record;
      Tooltip : constant Gtk.Tooltips.Gtk_Tooltips :=
                  Gtk.Tooltips.Gtk_Tooltips
                    (Glib.Object.Get_User_Data
                       (Glib.Values.Get_Address
                          (Glib.Values.Nth (Params, 4)), Stub));

      Model   : Gtk.Tree_Model.Gtk_Tree_Model;
      Path    : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter    : Gtk.Tree_Model.Gtk_Tree_Iter;
      Success : Boolean;

   begin
      Sender.Get_Tooltip_Context (X, Y, Mode, Model, Path, Iter, Success);

      if not Success then
         return False;
      end if;

      Tooltip.Set_Markup
        ("<b>Path "
         & Gtk.Tree_Model.To_String (Path)
         & "</b>: "
         & Model.Get_String (Iter, 0));
      Sender.Set_Tooltip_Row (Tooltip, Path);

      Gtk.Tree_Model.Path_Free (Path);

      return True;
   end On_Tree_View_Query_Tooltip;

   Tree_Types : constant Glib.GType_Array := (1 => Glib.GType_String);

   Window         : Gtk.Window.Gtk_Window;
   Box            : Gtk.Box.Gtk_Vbox;
   Button         : Gtk.Check_Button.Gtk_Check_Button;
   Label          : Gtk.Label.Gtk_Label;
   Tooltip_Window : Gtk.Window.Gtk_Window;
   Tooltip_Label  : Gtk.Label.Gtk_Label;
   Tree_Store     : Gtk.Tree_Store.Gtk_Tree_Store;
   Tree_View      : Gtk.Tree_View.Gtk_Tree_View;
   Column         : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
   Renderer       : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
   Iter           : Gtk.Tree_Model.Gtk_Tree_Iter;
   Dummy          : Glib.Gint;

begin
   Gtk.Main.Init;

   Gtk.Window.Gtk_New (Window, Gtk.Enums.Window_Toplevel);
   Window.Set_Title ("Tooltip example");
   Window.Set_Border_Width (10);
   Gtk_Window_Callbacks.Connect
     (Window,
      Gtk.Widget.Signal_Delete_Event,
      Gtk_Window_Callbacks.To_Marshaller (On_Delete_Event'Access));

   Gtk.Box.Gtk_New_Vbox (Box, False, 3);
   Window.Add (Box);

   --  A check button using the tooltip-markup property

   Gtk.Check_Button.Gtk_New
     (Button, "This one uses the tooltip-markup property");
   Button.Set_Tooltip_Text ("Hello, I am a static tooltip.");
   Box.Pack_Start (Button, False, False, 0);

   --  A check button using the query-tooltip signal

   Gtk.Check_Button.Gtk_New (Button, "I use the query-tooltip signal");
   Glib.Properties.Set_Property
     (Button, Gtk.Widget.Has_Tooltip_Property, True);
   Gtk_Check_Button_Callbacks.Connect
     (Button,
      Gtk.Widget.Signal_Query_Tooltip,
      On_Check_Button_Query_Tooltip'Access);
   Box.Pack_Start (Button, False, False, 0);

   --  A selectable label

   Gtk.Label.Gtk_New (Label, "I am a selectable label");
   Label.Set_Selectable (True);
   Label.Set_Tooltip_Markup ("<b>Another</b> Label tooltip");
   Box.Pack_Start (Label, False, False, 0);

   --  Another one, with a custom tooltip window

   Gtk.Label.Gtk_New (Label, "This one has a custom tooltip window!");
   Box.Pack_Start (Label, False, False, 0);

   Gtk.Window.Gtk_New (Tooltip_Window, Gtk.Enums.Window_Popup);
   Gtk.Label.Gtk_New (Tooltip_Label, "blaat!");
   Tooltip_Window.Add (Tooltip_Label);
   Tooltip_Label.Show;

   Label.Set_Tooltip_Window (Tooltip_Window);
   Glib.Properties.Set_Property
     (Label, Gtk.Widget.Has_Tooltip_Property, True);
   Gtk_Label_Callbacks.Connect
     (Label,
      Gtk.Widget.Signal_Query_Tooltip,
      On_Label_Query_Tooltip_Custom'Access);

   --  Tree view

   Gtk.Tree_Store.Gtk_New (Tree_Store, Tree_Types);
   Tree_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
   Tree_Store.Set (Iter, 0, "File Manager");
   Tree_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
   Tree_Store.Set (Iter, 0, "Gossip");
   Tree_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
   Tree_Store.Set (Iter, 0, "System Settings");
   Tree_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
   Tree_Store.Set (Iter, 0, "The GIMP");
   Tree_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
   Tree_Store.Set (Iter, 0, "Terminal");
   Tree_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
   Tree_Store.Set (Iter, 0, "Word Processor");

   Gtk.Tree_View.Gtk_New (Tree_View, Tree_Store);
   Tree_View.Set_Size_Request (200, 240);
   Glib.Properties.Set_Property
     (Tree_View, Gtk.Widget.Has_Tooltip_Property, True);
   Gtk_Tree_View_Callbacks.Connect
     (Tree_View,
      Gtk.Widget.Signal_Query_Tooltip,
      On_Tree_View_Query_Tooltip'Access);
   Box.Pack_Start (Tree_View, False, False, 2);

   Gtk.Tree_View_Column.Gtk_New (Column);
   Column.Set_Title ("Test");
   Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
   Column.Pack_Start (Renderer, False);
   Column.Add_Attribute (Renderer, "text", 0);
   Dummy := Tree_View.Append_Column (Column);

   --  Set a tooltip on column's header
   Window.Realize;

   Column.Set_Clickable (True);
   Column.Get_Widget.Set_Tooltip_Text ("Header");

   Gtk.Window.Show_All (Window);
   Gtk.Main.Main;
end Tooltips;
