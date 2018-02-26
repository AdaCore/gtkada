------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Gdk.Dnd;                  use Gdk.Dnd;
with Gdk.Drag_Contexts;        use Gdk.Drag_Contexts;
with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.Types;                use Gdk.Types;
with Glib;                     use Glib;
with Glib.Error;               use Glib.Error;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtk.Cell_Layout;          use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;        use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Icon_View;            use Gtk.Icon_View;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtkada.Types;             use Gtkada.Types;
with Interfaces.C.Strings;     use Interfaces.C.Strings;
with Gtk.Target_List;          use Gtk.Target_List;

package body Create_Icon_View is

   Item_Targets : constant Target_Entry_Array :=
     (1 => (Target => New_String ("GTK_TREE_MODEL_ROW"),
            Flags  => Gtk_Target_Same_App,
            Info   => 0));

   procedure Fill_Model (List : Gtk_List_Store);
   --  Fill the contents of List

   procedure Toggled
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when one of the toggle buttons is clicked on. This performs the
   --  actual toggling.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Icon_View@B is a widget which displays a list of icons"
        & " in a grid. Its data is stored in a @bGtk_Tree_Model@B."
        & ASCII.LF
        & "In this demo, @bdrag-and-drop@B is activated so that icons can be"
        & " moved from one location to another."
        & ASCII.LF
        & "This example also shows how to make a toggle button editable within"
        & " an icon view.";
   end Help;

   -------------
   -- Toggled --
   -------------

   procedure Toggled
     (Model  : access GObject_Record'Class;
      Params : Glib.Values.GValues)
   is
      M    : constant Gtk_List_Store := Gtk_List_Store (Model);
      Path_String : constant String := To_String (Params, 1);
      Path        : Gtk_Tree_Path;
      Iter        : Gtk_Tree_Iter;
      Value       : Boolean;
   begin
      Gtk_New (Path, Path_String);
      Iter := Get_Iter (M, Path);
      Value := M.Get_Boolean (Iter, 2);
      Set         (M, Iter, 2, not Value);
      Path_Free (Path);
   end Toggled;

   ----------------
   -- Fill_Model --
   ----------------

   procedure Fill_Model (List : Gtk_List_Store) is
      Pixbuf : Gdk_Pixbuf;
      Error  : GError;
      Iter   : Gtk_Tree_Iter;
   begin
      Gdk_New_From_File
        (Pixbuf, Filename => "gnome-textfile.png", Error => Error);

      Prepend (List, Iter);
      Set (List, Iter, 0, Pixbuf);
      Set (List, Iter, 1, "Really Really" & ASCII.LF
           & " Really really looooooooong item name");
      Set (List, Iter, 2, True);
      Set (List, Iter, 3, "This is the tooltip for the really long item.");

      for J in 0 .. 9 loop
         Prepend (List, Iter);
         Set (List, Iter, 0, Pixbuf);
         Set (List, Iter, 1, "Icon" & J'Img);
         Set (List, Iter, 2, True);
         Set (List, Iter, 3, "This is the tooltip for item" & J'Img & ".");
      end loop;
   end Fill_Model;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Scrolled : Gtk_Scrolled_Window;
      View     : Gtk_Icon_View;
      List     : Gtk_List_Store;
      Text     : Gtk_Cell_Renderer_Text;
      Toggle   : Gtk_Cell_Renderer_Toggle;
      Pixbuf   : Gtk_Cell_Renderer_Pixbuf;
   begin
      Set_Label (Frame, "Icon View");

      Gtk_New (Scrolled);
      Add (Frame, Scrolled);

      Gtk_New (View);
      Add (Scrolled, View);
      Set_Selection_Mode (View, Selection_Multiple);

      Gtk_New
        (List,
         (0 => Gdk.Pixbuf.Get_Type,  --  Associated pixbuf
          1 => GType_String,         --  Text for the icon
          2 => GType_Boolean,        --  Toggle activated ?
          3 => GType_String));       --  Tooltip
      Set_Model (View, +List);
      Fill_Model (List);

      --  Position 3 in our model is the column that we use for tooltips.
      Set_Tooltip_Column (View, 3);

      Gtk_New (Toggle);
      Pack_Start (+View, Toggle, Expand => False);
      Set_Property (Toggle, Activatable_Property, True);
      Add_Attribute (+View, Toggle, "active", 2);
      Object_Callback.Object_Connect
        (Toggle, "toggled", Toggled'Access, List);

      --  Connect "toggled" signal

      Gtk_New (Pixbuf);
      Pack_Start (+View, Pixbuf, Expand => False);
      Set_Property (Pixbuf, Follow_State_Property, True);
      Add_Attribute (+View, Pixbuf, "pixbuf", 0);

      Gtk_New (Text);
      Pack_Start (+View, Text, Expand => False);
      Set_Property (Text, Editable_Property, True);
      Set_Property (Text, Xalign_Property, 0.5);
      --  Set_Property (Text, Wrap_Mode_Property, Wrap_Word_Char);
      Set_Property (Text, Wrap_Width_Property, 100);
      Add_Attribute (+View, Text, "text", 1);
      --  Connect "edited" signal

      --  Allow drag-and-drop of icons
      Enable_Model_Drag_Source
        (View,
         Start_Button_Mask => Button1_Mask,
         Targets           => Item_Targets,
         Actions           => Action_Move);
      Enable_Model_Drag_Dest
        (View,
         Targets           => Item_Targets,
         Actions           => Action_Move);

      Show_All (Frame);
   end Run;

end Create_Icon_View;
