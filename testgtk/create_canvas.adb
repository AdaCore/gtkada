-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2000                            --
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

with Ada.Numerics.Discrete_Random;
with Gdk.Color;           use Gdk.Color;
with Gdk.Drawable;        use Gdk.Drawable;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.GC;              use Gdk.GC;
with Glib;                use Glib;
with Gtk.Arrow;           use Gtk.Arrow;
with Gtk.Box;             use Gtk.Box;
with Gtk.Button;          use Gtk.Button;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtk.Spin_Button;     use Gtk.Spin_Button;
with Gtk.Label;           use Gtk.Label;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Extra.PsFont;    use Gtk.Extra.PsFont;
with Gdk.Font;            use Gdk.Font;

package body Create_Canvas is

   Max_Size : constant := 400;
   --  Size of the canvas;

   Item_Width  : constant Gint := 50;
   Item_Height : constant Gint := 40;

   ----------------------------------------------------------------
   --  Redefine our own item type, since we want to provide our own
   --  graphics.
   ----------------------------------------------------------------

   type Display_Item_Record is new Canvas_Item_Record with record
      Color : Gdk.Color.Gdk_Color;
      W, H : Gint;
      Num : Positive;
   end record;
   type Display_Item is access all Display_Item_Record'Class;

   procedure Initialize (Item : access Display_Item_Record'Class);
   --  Initialize Item with a random size and color.

   procedure Draw (Item : access Display_Item_Record;
                   Canvas : access Interactive_Canvas_Record'Class;
                   Dest : Gdk.Pixmap.Gdk_Pixmap;
                   Xdest, Ydest : Gint);


   -----------------------------
   --  Misc. types and variables
   -----------------------------

   package Canvas_Cb is new Gtk.Handlers.Callback
     (Interactive_Canvas_Record);


   procedure Add_Canvas_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Item1, Item2 : access Canvas_Item_Record'Class; Text : String := "");
   --  Add a link between Item1 and Item2

   Max_Colors : constant := 20;

   Zoom_Levels : constant array (Positive range <>) of Guint :=
     (10, 25, 50, 75, 100, 125, 150, 200, 300, 400);

   Start_Spin, End_Spin, Num_Spin : Gtk_Spin_Button;
   Num_Items_Label, Num_Links_Label : Gtk_Label;
   Font : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;

   type Color_Type is range 1 .. Max_Colors;
   package Color_Random is new Ada.Numerics.Discrete_Random (Color_Type);
   use Color_Random;

   package Items_Random is new Ada.Numerics.Discrete_Random (Positive);
   use Items_Random;

   subtype Coordinate_Type is Gint range Default_Grid_Size + 1 .. Max_Size;
   package Coordinate_Random is new
     Ada.Numerics.Discrete_Random (Coordinate_Type);
   use Coordinate_Random;

   subtype Zoom_Type is Gint range 1 .. 2;
   package Zoom_Random is new Ada.Numerics.Discrete_Random (Zoom_Type);
   use Zoom_Random;

   type String_Access is access String;
   Color_Names : array (Color_Type) of String_Access :=
     (new String' ("forest green"),
      new String' ("red"),
      new String' ("blue"),
      new String' ("yellow"),
      new String' ("peach puff"),
      new String' ("azure"),
      new String' ("seashell"),
      new String' ("lavender"),
      new String' ("grey"),
      new String' ("turquoise"),
      new String' ("khaki"),
      new String' ("tan"),
      new String' ("orange red"),
      new String' ("MediumPurple"),
      new String' ("ivory1"),
      new String' ("DeepSkyBlue1"),
      new String' ("burlywood1"),
      new String' ("wheat1"),
      new String' ("orange1"),
      new String' ("pink"));

   Colors : array (Color_Type) of Gdk_Color;

   Items_List : array (1 .. 500) of Canvas_Item;
   Last_Item : Positive := Items_List'First;
   Last_Link : Positive := 1;
   Green_Gc : Gdk.GC.Gdk_GC;

   Item_Gen : Items_Random.Generator;
   Gen : Coordinate_Random.Generator;
   Color_Gen : Color_Random.Generator;
   Zoom_Gen : Zoom_Random.Generator;
   --  Note: All the generators above are intentionally not reset, so that
   --  we can get the same events every time and thus can reproduce behaviors.

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "An @bInteractive_Canvas@B is an interactive widgets, on which"
        & " you can put items that the user will be able to manipulate"
        & " dynamically with the mouse."
        & ASCII.LF
        & "As you can see in this demo, the items can be linked together, and"
        & " the items remain connected when they are moved."
        & ASCII.LF
        & "The canvas also support scrolling, if put in a "
        & " @bGtk_Scrolled_Window@B, as you can see if you move the items"
        & " outside of the visible part of the canvas."
        & ASCII.LF
        & "It also provides zooming capabilities, as well as a simply layout"
        & " scheme (try inserting items linked to other items for instance)."
        & ASCII.LF
        & "No standard item is currently provided with GtkAda, but you can"
        & " easily create your own items.";
   end Help;

   ----------
   -- Draw --
   ----------

   procedure Draw (Item : access Display_Item_Record;
                   Canvas : access Interactive_Canvas_Record'Class;
                   Dest : Gdk.Pixmap.Gdk_Pixmap;
                   Xdest, Ydest : Gint)
   is
      W : constant Gint := Gint (Get_Coord (Item).Width);
      H : constant Gint := Gint (Get_Coord (Item).Height);
   begin
      Set_Foreground (Green_GC, Display_Item (Item).Color);
      Draw_Rectangle
        (Dest,
         GC     => Green_GC,
         Filled => True,
         X      => Xdest,
         Y      => Ydest,
         Width  => W,
         Height => H);
      if Get_Zoom (Canvas) >= 50 then
         Set_Foreground (Green_GC, Black (Get_Default_Colormap));
         Draw_Text
           (Dest,
            Font,
            Green_GC,
            Xdest + W / 2,
            Ydest + H / 2,
            Positive'Image (Display_Item (Item).Num));
      end if;
   end Draw;

   -------------------
   -- Canvas_Zoomed --
   -------------------

   procedure Canvas_Zoomed (Canvas : access Interactive_Canvas_Record'Class) is
      function Internal
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         It : Display_Item := Display_Item (Item);
      begin
         --  Change the size the item occupies on the screen.
         Set_Screen_Size
           (Item, To_Canvas_Coordinates (Canvas, It.W),
            To_Canvas_Coordinates (Canvas, It.H));
         return True;
      end Internal;

   begin
      For_Each_Item (Canvas, Internal'Unrestricted_Access);
   end Canvas_Zoomed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : access Display_Item_Record'Class) is
   begin
      Item.Color := Colors (Random (Color_Gen));
      Item.W := Item_Width * Random (Zoom_Gen);
      Item.H := Item_Height * Random (Zoom_Gen);
      Item.Num := Last_Item;
      if Last_Item <= Items_List'Last then
         Items_List (Item.Num) := Canvas_Item (Item);
      end if;
      Last_Item := Last_Item + 1;
      Set_Screen_Size (Item, Item.W, Item.H);
      Set_Text (Num_Items_Label, Positive'Image (Last_Item - 1) & " items");
   end Initialize;

   ---------------------
   -- Add_Random_Item --
   ---------------------

   procedure Add_Random_Item
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Item : Display_Item := new Display_Item_Record;
   begin
      Initialize (Item);
      Put (Canvas, Item, Random (Gen), Random (Gen));
      Refresh_Canvas (Canvas);
      Show_Item (Canvas, Item);
  end Add_Random_Item;

   -----------
   -- Clear --
   -----------

   procedure Clear (Canvas : access Interactive_Canvas_Record'Class) is
      function Remove_Internal
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         Remove (Canvas, Item);
         return True;
      end Remove_Internal;

   begin
      For_Each_Item (Canvas, Remove_Internal'Unrestricted_Access);
      Refresh_Canvas (Canvas);
      Last_Item := 1;
      Last_Link := 1;
      Set_Text (Num_Items_Label, Positive'Image (Last_Item - 1) & " items");
      Set_Text (Num_Links_Label, Positive'Image (Last_Link - 1) & " links");
   end Clear;

   ---------------
   -- Add_Items --
   ---------------

   procedure Add_Items
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Max : constant Positive := Last_Item
        + Positive (Get_Value_As_Int (Num_Spin)) - 1;
   begin
      for J in Last_Item .. Max loop
         Add_Random_Item (Canvas);
         Add_Canvas_Link
           (Canvas, Items_List (J), Items_List (Random (Item_Gen) mod J + 1));
         Add_Canvas_Link
           (Canvas, Items_List (J), Items_List (Random (Item_Gen) mod J + 1));
      end loop;
      Refresh_Canvas (Canvas);
   end Add_Items;

   ---------------------
   -- Add_Single_Item --
   ---------------------

   procedure Add_Single_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      With_Link : Boolean)
   is
      Item : Display_Item := new Display_Item_Record;
      Num  : constant Positive := Positive (Get_Value_As_Int (Start_Spin));
   begin
      Initialize (Item);

      if With_Link and then Num < Last_Item then
         Add_Canvas_Link (Canvas, Item, Item, "0");
         Add_Canvas_Link (Canvas, Items_List (Num), Item, "1");
         Add_Canvas_Link (Canvas, Items_List (Num), Item, "2");
      end if;

      Put (Canvas, Item);
      Refresh_Canvas (Canvas);
      Show_Item (Canvas, Item);
   end Add_Single_Item;

   -------------------------------
   -- Add_Single_Item_With_Link --
   -------------------------------

   procedure Add_Single_Item_With_Link
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      Add_Single_Item (Canvas, True);
   end Add_Single_Item_With_Link;

   -----------------------------
   -- Add_Single_Item_No_Link --
   -----------------------------

   procedure Add_Single_Item_No_Link
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      Add_Single_Item (Canvas, False);
   end Add_Single_Item_No_Link;

   ---------------------
   -- Add_Canvas_Link --
   ---------------------

   procedure Add_Canvas_Link
     (Canvas : access Interactive_Canvas_Record'Class;
      Item1, Item2 : access Canvas_Item_Record'Class; Text : String := "")
   is
      Link : Canvas_Link := new Canvas_Link_Record;
   begin
      Add_Link (Canvas, Link, Item1, Item2, Both_Arrow, Text);
      Last_Link := Last_Link + 1;
      Set_Text (Num_Links_Label, Positive'Image (Last_Link - 1) & " links");
   end Add_Canvas_Link;

   -----------------
   -- Remove_Link --
   -----------------

   procedure Remove_Link
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      It1, It2 : Canvas_Item;

      function Remove_Internal
        (Canvas : access Interactive_Canvas_Record'Class;
         Link : access Canvas_Link_Record'Class) return Boolean
      is
         pragma Warnings (Off, Canvas);
      begin
         if (Canvas_Item (Get_Src (Link)) = It1
             and then Canvas_Item (Get_Dest (Link)) = It2)
           or else
           (Canvas_Item (Get_Src (Link)) = It2
            and then Canvas_Item (Get_Dest (Link)) = It1)
         then
            Remove_Link (Canvas, Link);
            return False;
         end if;
         return True;
      end Remove_Internal;

      Num1 : constant Positive := Positive (Get_Value_As_Int (Start_Spin));
      Num2 : constant Positive := Positive (Get_Value_As_Int (End_Spin));
   begin
      if Num1 < Last_Item and then Num2 < Last_Item then
         It1 := Canvas_Item (Items_List (Num1));
         It2 := Canvas_Item (Items_List (Num2));
         For_Each_Link (Canvas, Remove_Internal'Unrestricted_Access);
         Refresh_Canvas (Canvas);
      end if;
   end Remove_Link;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      for J in Zoom_Levels'First .. Zoom_Levels'Last - 1 loop
         if Zoom_Levels (J) = Get_Zoom (Canvas) then
            Zoom (Canvas, Zoom_Levels (J + 1), 5);
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out
     (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      for J in Zoom_Levels'First + 1 .. Zoom_Levels'Last loop
         if Zoom_Levels (J) = Get_Zoom (Canvas) then
            Zoom (Canvas, Zoom_Levels (J - 1), 5);
         end if;
      end loop;
   end Zoom_Out;

   ---------------------
   -- Add_Widget_Item --
   ---------------------

   procedure Add_Widget_Item
     (Canvas : access Interactive_Canvas_Record'Class) is
     pragma Warnings (Off, Canvas);
--        Item : Canvas_Item_Widget := new Canvas_Item_Widget_Record;
--        Button : Gtk_Button;
   begin
--        Gtk_New (Button, "Bar");
--        Add (Item, Canvas, Button);
--        Put (Canvas, Item, Random (Gen), Random (Gen));

--        if Last_Item <= Items_List'Last then
--           Items_List (Last_Item) := Canvas_Item (Item);
--        end if;
--        Last_Item := Last_Item + 1;
--        Set_Screen_Size (Item, Item_Width, Item_Height);
--        Set_Text (Num_Items_Label, Positive'Image (Last_Item - 1) & " items");

--        Refresh_Canvas (Canvas);
      null;
   end Add_Widget_Item;

   -------------------
   -- Initial_Setup --
   -------------------

   procedure Initial_Setup
     (Canvas : access Interactive_Canvas_Record'Class)
   is
      Item1, Item2, Item3,  Item4    : Display_Item;
      Link  : Canvas_Link;
   begin
      Item1 := new Display_Item_Record;
      Initialize (Item1);
      Put (Canvas, Item1, 10, 10);

      Item2 := new Display_Item_Record;
      Initialize (Item2);
      Put (Canvas, Item2, 70, 240);

      Item3 := new Display_Item_Record;
      Initialize (Item3);
      Put (Canvas, Item3, 200, 10);

      Item4 := new Display_Item_Record;
      Initialize (Item4);
      Put (Canvas, Item4, 280, 170);

      Add_Canvas_Link (Canvas, Item1, Item1, "From1->2");
      Add_Canvas_Link (Canvas, Item3, Item1, "From3->2");
      Add_Canvas_Link (Canvas, Item1, Item4, "From1->4");
      Add_Canvas_Link (Canvas, Item1, Item4, "From1->4");
      Add_Canvas_Link (Canvas, Item2, Item3, "From2->3");
      Add_Canvas_Link (Canvas, Item2, Item4, "From2->4");
      Add_Canvas_Link (Canvas, Item3, Item4, "From3->41");

      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item3, Item4, Start_Arrow, "From3->42");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item4, Item3, End_Arrow, "From3->43");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item3, Item4, Both_Arrow, "From3->44");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item4, Item3, Both_Arrow, "From3->45");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item3, Item4, Both_Arrow, "From3->46");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item2, Item2, No_Arrow, "Self");
      Link := new Canvas_Link_Record;
      Add_Link (Canvas, Link, Item2, Item2, Start_Arrow, "Self2");
      Last_Link := Last_Link + 7;

      Set_Text (Num_Links_Label, Positive'Image (Last_Link - 1) & " links");
   end Initial_Setup;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas   : Interactive_Canvas;
      Box, Bbox, Bbox2, Spin_Box, Small : Gtk_Box;
      Button   : Gtk_Button;
      Arrow    : Gtk_Arrow;
      Scrolled : Gtk_Scrolled_Window;
      Label    : Gtk_Label;
      Adj      : Gtk_Adjustment;

   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Gtk_New_Hbox (Bbox, Homogeneous => True);
      Pack_Start (Box, Bbox, Expand => False, Fill => False);

      Gtk_New_Hbox (Bbox2, Homogeneous => True);
      Pack_Start (Box, Bbox2, Expand => False, Fill => False);

      Gtk_New_Hbox (Spin_Box, Homogeneous => True);
      Pack_Start (Box, Spin_Box, Expand => False, Fill => False);

      Gtk_New (Scrolled);
      Pack_Start (Box, Scrolled);

      Gtk_New (Canvas);
      Add (Scrolled, Canvas);
      Align_On_Grid (Canvas, False);

      Gtk_New (Button);
      Gtk_New (Arrow, Arrow_Up, Shadow_Out);
      Add (Button, Arrow);
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked", Canvas_Cb.To_Marshaller (Zoom_In'Access), Canvas);

      Gtk_New (Button);
      Gtk_New (Arrow, Arrow_Down, Shadow_Out);
      Add (Button, Arrow);
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked", Canvas_Cb.To_Marshaller (Zoom_Out'Access), Canvas);

      Gtk_New (Button, "Random");
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Random_Item'Access), Canvas);

      Gtk_New (Button, "Add One");
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Single_Item_No_Link'Access), Canvas);

      Gtk_New (Button, "Clear");
      Pack_Start (Bbox, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Clear'Access), Canvas);

      Gtk_New (Button, "Remove Link Start->End");
      Pack_Start (Bbox2, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Remove_Link'Access), Canvas);

      Gtk_New (Button, "Add multiple Items");
      Pack_Start (Bbox2, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Items'Access), Canvas);

      Gtk_New (Button, "Add with Link ->Start");
      Pack_Start (Bbox2, Button, Expand => False, Fill => True);
      Canvas_Cb.Object_Connect
        (Button, "clicked",
         Canvas_Cb.To_Marshaller (Add_Single_Item_With_Link'Access), Canvas);

--        Gtk_New (Button, "Add Widget");
--        Pack_Start (Bbox2, Button, Expand => False, Fill => True);
--        Canvas_Cb.Object_Connect
--          (Button, "clicked",
--           Canvas_Cb.To_Marshaller (Add_Widget_Item'Access), Canvas);

      Gtk_New (Num_Items_Label, "0 items");
      Pack_Start (Spin_Box, Num_Items_Label, Expand => False, Fill => False);

      Gtk_New (Num_Links_Label, "0 links");
      Pack_Start (Spin_Box, Num_Links_Label, Expand => False, Fill => False);

      Gtk_New_Hbox (Small, Homogeneous => False);
      Gtk_New (Label, "Add:");
      Pack_Start (Small, Label, Expand => False, Fill => False);
      Gtk_New (Adj, 10.0, 1.0, 300.0, 1.0, 30.0, 30.0);
      Gtk_New (Num_Spin, Adj, 0.5, 0);
      Pack_Start (Small, Num_Spin, Expand => False, Fill => False);
      Pack_Start (Spin_Box, Small, Expand => False, Fill => False);

      Gtk_New_Hbox (Small, Homogeneous => False);
      Gtk_New (Label, "Start:");
      Pack_Start (Small, Label, Expand => False, Fill => False);
      Gtk_New (Adj, 1.0, 1.0, 300.0, 1.0, 30.0, 30.0);
      Gtk_New (Start_Spin, Adj, 0.5, 0);
      Pack_Start (Small, Start_Spin, Expand => False, Fill => False);
      Pack_Start (Spin_Box, Small, Expand => False, Fill => False);

      Gtk_New_Hbox (Small, Homogeneous => False);
      Gtk_New (Label, "End:");
      Pack_Start (Small, Label, Expand => False, Fill => False);
      Gtk_New (Adj, 2.0, 1.0, 300.0, 1.0, 30.0, 30.0);
      Gtk_New (End_Spin, Adj, 0.5, 0);
      Pack_Start (Small, End_Spin, Expand => False, Fill => False);
      Pack_Start (Spin_Box, Small, Expand => False, Fill => False);

      Realize (Canvas);

      --  Initialize the colors

      Gdk_New (Green_GC, Get_Window (Canvas));
      for J in Color_Names'Range loop
         Colors (J) := Parse (Color_Names (J).all);
         Alloc (Gtk.Widget.Get_Default_Colormap, Colors (J));
      end loop;
      Font := Get_Gdkfont ("Courier", 8);

      Canvas_Cb.Connect
        (Canvas, "zoomed",
         Canvas_Cb.To_Marshaller (Canvas_Zoomed'Access));

      Initial_Setup (Canvas);
      Show_All (Frame);
   end Run;
end Create_Canvas;

