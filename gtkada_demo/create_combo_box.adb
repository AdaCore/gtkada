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

with Gdk.Color;              use Gdk.Color;
with Gdk.Pixbuf;             use Gdk.Pixbuf;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Gtk.Box;                use Gtk.Box;
with Gtk.Frame;              use Gtk.Frame;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Combo_Box_Text;     use Gtk.Combo_Box_Text;
with Gtk.Combo_Box;          use Gtk.Combo_Box;
with Gtk.Cell_Layout;        use Gtk.Cell_Layout;
with Gtk.List_Store;         use Gtk.List_Store;
with Gtk.Tooltip;            use Gtk.Tooltip;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Widget;             use Gtk.Widget;

package body Create_Combo_Box is

   Column_0 : constant := 0;
   Column_1 : constant := 1;
   Column_2 : constant := 2;
   --  The columns in the model

   procedure Append_Color_Pixbuf
     (Model : Gtk_List_Store;
      Color : String);
   --  Append a new pixbuf with Color as its background

   procedure Fill_Pixbuf (Pix : Gdk_Pixbuf; Color : String);
   --  Fill the background of Pix. This is probably not some code you should
   --  copy in your own application, since not very clean.

   procedure Set_Color_Pixbuf
     (Model : Gtk_List_Store; Iter : Gtk_Tree_Iter; Color : String);
   --  Add a pixbuf to the second column of Model

   function On_Query_Tooltip
      (Widget        : access Gtk_Widget_Record'Class;
       X, Y          : Gint;
       Keyboard_Mode : Boolean;
       Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean;
   --  Compute an item specific tooltip for the first combo box

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Combo_Box@B is a widget that allows the user to choose"
        & " from a list of valid choices.";
   end Help;

   -----------------
   -- Fill_Pixbuf --
   -----------------

   procedure Fill_Pixbuf (Pix : Gdk_Pixbuf; Color : String) is
      GColor : Gdk_Color;
      Num     : Guint;
      Pixels  : Rgb_Buffer_Access;
   begin
      GColor := Parse (Color);

      --  This code is not clean. It would be better to use cairo, but GtkAda
      --  has no binding for it at the time of this writing. You could also
      --  load the images from XPM data instead.
      Num    := Guint (Get_Width (Pix) * Get_Height (Pix));
      Pixels := Get_Pixels (Pix);

      for N in 0 .. Num - 1 loop
         --  By default, each color occupies 8bits, thus is it easier to
         --  manipulate colors
         Pixels (N).Red   := Guchar (Red   (GColor) / 65535 * 255);
         Pixels (N).Green := Guchar (Green (GColor) / 65535 * 255);
         Pixels (N).Blue  := Guchar (Blue  (GColor) / 65535 * 255);
      end loop;
   end Fill_Pixbuf;

   -------------------------
   -- Append_Color_Pixbuf --
   -------------------------

   procedure Append_Color_Pixbuf
     (Model : Gtk_List_Store;
      Color : String)
   is
      Pix  : Gdk_Pixbuf;
      Iter : Gtk_Tree_Iter;
   begin
      Pix := Gdk_New (Bits_Per_Sample => 8, Width => 16, Height => 16);
      Fill_Pixbuf (Pix, Color);
      Append (Model, Iter);
      Set (Model, Iter, Column_0, Pix);
      Unref (Pix);
   end Append_Color_Pixbuf;

   ----------------------
   -- Set_Color_Pixbuf --
   ----------------------

   procedure Set_Color_Pixbuf
     (Model : Gtk_List_Store; Iter : Gtk_Tree_Iter; Color : String)
   is
      Pix : Gdk_Pixbuf;
   begin
      Pix := Gdk_New (Bits_Per_Sample => 8, Width => 16, Height => 16);
      Fill_Pixbuf (Pix, Color);
      Set (Model, Iter, Column_1, Pix);
      Unref (Pix);
   end Set_Color_Pixbuf;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   function On_Query_Tooltip
      (Widget        : access Gtk_Widget_Record'Class;
       X, Y          : Gint;
       Keyboard_Mode : Boolean;
       Tooltip       : not null access Glib.Object.GObject_Record'Class)
      return Boolean
   is
      pragma Unreferenced (X, Y, Keyboard_Mode);
      Combo : constant Gtk_Combo_Box := Gtk_Combo_Box (Widget);
      Tip   : constant Gtk_Tooltip := Gtk_Tooltip (Tooltip);
   begin
      Tip.Set_Text ("This is the tooltip for the active item "
                    & Gint'Image (Combo.Get_Active));
      return True;  --  display the tooltip
   end On_Query_Tooltip;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box        : Gtk_Box;
      Model      : Gtk_List_Store;
      Iter       : Gtk_Tree_Iter;
      Combo      : Gtk_Combo_Box;
      TCombo      : Gtk_Combo_Box_Text;
      Render     : Gtk_Cell_Renderer_Text;
      Pix        : Gtk_Cell_Renderer_Pixbuf;
   begin
      Set_Label (Frame, "Combo box");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      --  A simple text combo

      Gtk_New (TCombo);
      Pack_Start (Box, TCombo, Expand => False);
      Append_Text (TCombo, "Simple Text Combo 1");
      Append_Text (TCombo, "Simple Text Combo 2");
      Append_Text (TCombo, "Simple Text Combo 3");
      Set_Active (TCombo, 0);

      --  Let's make the text of the combo box tooltip depend on which item
      --  is selected. Unfortunately, there doesn't seem to be a way to set
      --  a tooltip on the popup window itself, since we do not have access
      --  to that window.

      TCombo.Set_Tooltip_Text ("A general tooltip");
      TCombo.On_Query_Tooltip (On_Query_Tooltip'Access);

      --  A combo box with an entry, and some additional columns in the
      --  popup

      Gtk_New (Model, (Column_0 => GType_String,   --  text for the entry
                       Column_1 => GType_String)); --  text for the popup

      for Choice in 1 .. 10 loop
         Model.Append (Iter);
         Set (Model, Iter, Column_0, "Choice" & Integer'Image (Choice));
         Set (Model, Iter, Column_1,
              "Some explanation on choice" & Integer'Image (Choice));
      end loop;

      Gtk_New_With_Model_And_Entry (Combo, +Model);
      Box.Pack_Start (Combo, Expand => False);

      Gtk_New (Render);
      Pack_Start    (+Combo, Render, Expand => True);
      Add_Attribute (+Combo, Render, "markup", Column_1);

      Combo.Set_Entry_Text_Column (Column_0);  --  before Set_Active
      Combo.Set_Active (0);

      --  A slightly different combo box, where the items are on multiple
      --  lines. This doesn't quite replace a tooltip, but might be useful
      --  anyway

      Gtk_New (Model, (Column_0 => GType_String,   --  text for the entry
                       Column_1 => GType_String)); --  text for the popup

      for Choice in 1 .. 10 loop
         Model.Append (Iter);
         Set (Model, Iter, Column_0, "Choice" & Integer'Image (Choice));
         Set (Model, Iter, Column_1,
              "Choice" & Integer'Image (Choice) & ASCII.LF &
              "<small>Some explanation on choice" & Integer'Image (Choice)
              & "</small>");
      end loop;

      Gtk_New_With_Model (Combo, +Model);
      Box.Pack_Start (Combo, Expand => False);

      Gtk_New (Render);
      Pack_Start    (+Combo, Render, Expand => True);
      Add_Attribute (+Combo, Render, "markup", Column_1);

      Combo.Set_Active (0);

      --  Create a model. This is a set of rows, each with two columns in this
      --  specific case.
      Gtk_New (Model, (Column_0 => GType_String,
                       Column_1 => Gdk.Pixbuf.Get_Type,
                       Column_2 => GType_Boolean));

      Append (Model, Iter);
      Set (Model, Iter, Column_0, "Combo From Model 1");
      Set_Color_Pixbuf (Model, Iter, "red");
      Set (Model, Iter, Column_2, True);

      Append (Model, Iter);
      Set (Model, Iter, Column_0, "Combo From Model 2");
      Set_Color_Pixbuf (Model, Iter, "green");
      Set (Model, Iter, Column_2, False);  --  Row 2 will be insensitive

      Append (Model, Iter);
      Set (Model, Iter, Column_0, "Combo From Model 3");
      Set_Color_Pixbuf (Model, Iter, "blue");
      Set (Model, Iter, Column_2, True);

      --  Create the combo. We use both columns of the model to display in the
      --  model, but we could display only one, or even have a display that
      --  doesn't come directly from a column (see create_cell_view for
      --  instance)

      Gtk_New_With_Model (Combo, +Model);
      Pack_Start (Box, Combo, Expand => False);

      Gtk_New (Pix);
      Pack_Start    (+Combo, Pix, Expand => True);
      Add_Attribute (+Combo, Pix, "pixbuf", Column_1);
      Add_Attribute (+Combo, Pix, "sensitive", Column_2);

      Gtk_New (Render);
      Pack_Start    (+Combo, Render, Expand => True);
      Add_Attribute (+Combo, Render, "text", Column_0);
      Add_Attribute (+Combo, Render, "sensitive", Column_2);

      Set_Active (Combo, 0);

      --  A matrix combo now
      Gtk_New (Model, (Column_0 => Gdk.Pixbuf.Get_Type));
      Append_Color_Pixbuf (Model, "red");
      Append_Color_Pixbuf (Model, "green");
      Append_Color_Pixbuf (Model, "blue");
      Append_Color_Pixbuf (Model, "yellow");
      Append_Color_Pixbuf (Model, "black");
      Append_Color_Pixbuf (Model, "white");
      Append_Color_Pixbuf (Model, "cyan");
      Append_Color_Pixbuf (Model, "pink");
      Append_Color_Pixbuf (Model, "magenta");

      Gtk_New_With_Model (Combo, +Model);
      Pack_Start (Box, Combo, Expand => False);
      Set_Wrap_Width (Combo, 3);  --  Make it a matrix

      Gtk_New (Pix);
      Pack_Start    (+Combo, Pix, Expand => True);
      Add_Attribute (+Combo, Pix, "pixbuf", Column_0);

      Set_Active (Combo, 0);

      Show_All (Frame);
   end Run;

end Create_Combo_Box;
