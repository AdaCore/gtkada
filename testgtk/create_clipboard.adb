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

with Glib;                   use Glib;
with Gdk.Pixbuf;             use Gdk.Pixbuf;
with Gdk.Property;           use Gdk.Property;
with Gdk.Types;              use Gdk.Types;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtk;                    use Gtk;
with Gtk.Box;                use Gtk.Box;
with Gtk.Button;             use Gtk.Button;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;
with Gtk.Clipboard;          use Gtk.Clipboard;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Hbutton_Box;        use Gtk.Hbutton_Box;
with Gtk.Image;              use Gtk.Image;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Scrolled_Window;    use Gtk.Scrolled_Window;
with Gtk.Selection_Data;     use Gtk.Selection_Data;
with Gtk.Text_Buffer;        use Gtk.Text_Buffer;
with Gtk.Text_Iter;          use Gtk.Text_Iter;
with Gtk.Text_View;          use Gtk.Text_View;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Selection;     use Gtk.Tree_Selection;
with Gtk.Tree_View;          use Gtk.Tree_View;
with Gtk.Tree_View_Column;   use Gtk.Tree_View_Column;
with Gtk.Widget;             use Gtk.Widget;

package body Create_Clipboard is

   procedure Refresh (Button : access Gtk_Widget_Record'Class);
   --  Refresh the window to show the current clipboard contents

   procedure On_Select_Format (View : access Gtk_Widget_Record'Class);
   --  Called when a new format is selected

   procedure On_Image_Retrieved
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class);
   --  Called when the image has been retrieved from the clipboard

   List     : Gtk_List_Store;
   Contents : Gtk_Text_Buffer;
   Image    : Gtk_Image;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This demo shows how to interface to the system clipboard. You"
        & " should select text or images in some other application, copy them"
        & " to the clipboard (generally through Edit->Copy), and then click on"
        & " Refresh in this demo." & ASCII.LF
        & "The data contained in a clipboard can be retrieved in multiple"
        & " formats (ASCII text, UTF8 text, image, application-dependent types"
        & ",...). This demo shows the format that the current clipboard"
        & " can be retrieved in. Double-clicking on any of the format will"
        & " retrieve the selection in that format, and display it if possible"
        & ASCII.LF
        & "Try also copying an image into the clipboard, it will be displayed."
        & " Big images will take time to be retrieved asynchronously however.";
   end Help;

   ------------------------
   -- On_Image_Retrieved --
   ------------------------

   procedure On_Image_Retrieved
     (Clipboard : not null access Gtk_Clipboard_Record'Class;
      Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      pragma Unreferenced (Clipboard);

   begin
      Set (Image, Pixbuf);
   end On_Image_Retrieved;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Button : access Gtk_Widget_Record'Class) is
      pragma Unreferenced (Button);
      Clipboard : constant Gtk_Clipboard := Get;
      Targets   : constant Gdk_Atom_Array := Wait_For_Targets (Clipboard);
      Iter      : Gtk_Tree_Iter;
   begin
      Clear (List);

      for T in reverse Targets'Range loop
         Insert (List, Iter, 0);
         Set (List, Iter, 0, Atom_Name (Targets (T)));
      end loop;
   end Refresh;

   ----------------------
   -- On_Select_Format --
   ----------------------

   procedure On_Select_Format
      (View : access Gtk_Widget_Record'Class)
   is
      Clipboard : constant Gtk_Clipboard := Get;
      Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : Gtk_Selection_Data;
      First, Last : Gtk_Text_Iter;
      As_String : Boolean;
      As_Image  : Boolean;
   begin
      Clear (Image);

      Get_Start_Iter (Contents, First);
      Get_End_Iter   (Contents, Last);
      Delete (Contents, First, Last);

      Get_Start_Iter (Contents, First);
      As_String := Wait_Is_Text_Available (Clipboard);
      As_Image  := Wait_Is_Image_Available (Clipboard);
      Insert
        (Contents,
         First,
         "Can be retrieved as a string: "
         & Boolean'Image (As_String)
         & ASCII.LF);
      Insert
        (Contents,
         First,
         "Can be retrieved as an image: "
         & Boolean'Image (As_Image)
         & ASCII.LF);

      if As_Image then
         Request_Image (Clipboard, On_Image_Retrieved'Access);
      end if;

      Get_Selected (Get_Selection (Gtk_Tree_View (View)), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         Format : constant String := Get_String (Model, Iter, 0);
      begin
         Data := Wait_For_Contents (Clipboard, Atom_Intern (Format));

         if not Data.Is_Null then
            Insert
              (Contents,
               First,
               "Target=   "
               & Atom_Name (Get_Target (Data))
               & ASCII.LF);
            Insert
              (Contents,
               First,
               "Type=     "
               & Atom_Name (Get_Data_Type (Data))
               & ASCII.LF);
            Insert
              (Contents,
               First,
               "Format=  "
               & Gint'Image (Get_Format (Data))
               & " (bits/char for strings for instance)"
               & ASCII.LF);
            Insert
              (Contents,
               First,
               "Length=  "
               & Gint'Image (Get_Length (Data))
               & ASCII.LF);

            if As_String then
               Insert
                 (Contents,
                  First,
                  "As_String=" & Get_Data_As_String (Data));
            end if;

            Free (Data);
         end if;
      end;
   end On_Select_Format;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box1, Box2 : Gtk_Box;
      Bbox       : Gtk_Hbutton_Box;
      View       : Gtk_Tree_View;
      Col_Pos    : Gint;
      pragma Unreferenced (Col_Pos);
      Col        : Gtk_Tree_View_Column;
      Render     : Gtk_Cell_Renderer_Text;
      Button     : Gtk_Button;
      Scrolled   : Gtk_Scrolled_Window;
      Text       : Gtk_Text_View;
   begin
      Gtk.Frame.Set_Label (Frame, "Clipboard");

      Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
      Add (Frame, Box1);

      Gtk_New_Hbox (Box2, Homogeneous => False);
      Pack_Start (Box1, Box2, Expand => True);

      Gtk_New (Bbox);
      Pack_Start (Box2, Bbox, Expand => False);

      Gtk_New (Button, "Refresh");
      Widget_Callback.Connect
        (Button, Gtk.Button.Signal_Clicked, Refresh'Access);
      Pack_Start (Bbox, Button, Expand => False);

      Gtk_New (List, (0 => GType_String));
      Gtk_New (Render);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Box2, Scrolled, Expand => True);

      Gtk_New (View, +List);
      Add (Scrolled, View);
      Set_Headers_Visible (View, False);

      Widget_Callback.Object_Connect
        (Get_Selection (View), "changed", On_Select_Format'Access, View);

      Gtk_New (Col);
      Col_Pos := Insert_Column (View, Col);
      Pack_Start (Col, Render, Expand => True);
      Add_Attribute (Col, Render, "text", 0);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Box1, Scrolled, Expand => True);

      Gtk_New (Contents);
      Gtk_New (Text, Contents);
      Add (Scrolled, Text);

      Gtk_New (Image);
      Pack_Start (Box1, Image);

      Refresh (Button);

      Show_All (Frame);
   end Run;

end Create_Clipboard;
