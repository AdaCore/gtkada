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

with Glib;                use Glib;
with Gtk;                 use Gtk;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gdk.Window;          use Gdk.Window;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gdk.GC;              use Gdk.GC;
with Gdk.Color;           use Gdk.Color;
with Gdk.Drawable;        use Gdk.Drawable;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Frame;           use Gtk.Frame;

package body Create_Canvas is

   type Display_Item1_Record is new Gtkada.Canvas.Canvas_Item_Record
     with null record;
   type Display_Item1 is access all Display_Item1_Record'Class;

   procedure Gtk_New (Item : out Display_Item1;
                      Win  : in Gdk.Window.Gdk_Window);
   procedure Initialize (Item : access Display_Item1_Record'Class;
                         Win  : in Gdk.Window.Gdk_Window);

   type Display_Item2_Record is new Gtkada.Canvas.Canvas_Item_Record
     with null record;
   type Display_Item2 is access all Display_Item2_Record'Class;

   procedure Gtk_New (Item : out Display_Item2;
                      Win  : in Gdk.Window.Gdk_Window);
   procedure Initialize (Item : access Display_Item2_Record'Class;
                         Win  : in Gdk.Window.Gdk_Window);



   White_Gc, Grey_Gc : Gdk.GC.Gdk_GC;

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
        & "No standard item is currently provided with GtkAda, but you can"
        & " easily create your own items.";
   end Help;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Item : out Display_Item1;
                      Win  : in Gdk.Window.Gdk_Window)
   is
   begin
      Item := new Display_Item1_Record;
      Initialize (Item, Win);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Item : out Display_Item2;
                      Win  : in Gdk.Window.Gdk_Window)
   is
   begin
      Item := new Display_Item2_Record;
      Initialize (Item, Win);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : access Display_Item1_Record'Class;
                         Win  : in Gdk.Window.Gdk_Window)
   is
      Width  : constant Gint := 50;
      Height : constant Gint := 40;
   begin
      Gtkada.Canvas.Initialize (Item, Win, Width, Height);
      Draw_Rectangle (Pixmap (Item),
                      GC     => Grey_GC,
                      Filled => True,
                      X      => 0,
                      Y      => 0,
                      Width  => Width - 1,
                      Height => Height - 1);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Item : access Display_Item2_Record'Class;
                         Win  : in Gdk.Window.Gdk_Window)
   is
      Width  : constant Gint := 50;
      Height : constant Gint := 40;
   begin

      Gtkada.Canvas.Initialize (Item, Win, Width, Height);
      Draw_Rectangle (Pixmap (Item),
                      GC     => Grey_GC,
                      Filled => True,
                      X      => 0,
                      Y      => 0,
                      Width  => Width - 1,
                      Height => Height - 1);
      Draw_Arc (Pixmap (Item),
                GC     => White_GC,
                Filled => True,
                X      => 0,
                Y      => 0,
                Width  => Width - 1,
                Height => Height - 1,
                Angle1 => 0,
                Angle2 => 270 * 64);
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Canvas   : Gtkada.Canvas.Interactive_Canvas;
      Scrolled : Gtk_Scrolled_Window;
      Item1,
      Item2    : Display_Item1;
      Item3,
      Item4    : Display_Item2;
      Grey   : Gdk_Color;

      use type Gdk_GC;

   begin
      Gtk.Frame.Set_Label (Frame, "Canvas");

      Gtk_New (Scrolled);
      Add (Frame, Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Gtk_New (Canvas);
      Add (Scrolled, Canvas);
      Set_USize (Canvas, 500, 500);
      Align_On_Grid (Canvas, True);

      Realize (Canvas);

      if White_GC = null then
         Gdk_New (White_GC, Get_Window (Canvas));
         Set_Foreground (White_GC, White (Get_Default_Colormap));

         Grey := Parse ("grey");
         Alloc (Gtk.Widget.Get_Default_Colormap, Grey);

         Gdk_New (Grey_GC, Get_Window (Canvas));
         Set_Foreground (Grey_GC, Grey);

      end if;

      --  Add the items

      Gtk_New (Item1, Get_Window (Canvas));
      Put (Canvas, Item1, 10, 10);

      Gtk_New (Item2, Get_Window (Canvas));
      Put (Canvas, Item2, 70, 240);

      Gtk_New (Item3, Get_Window (Canvas));
      Put (Canvas, Item3, 200, 10);

      Gtk_New (Item4, Get_Window (Canvas));
      Put (Canvas, Item4, 280, 170);

      Add_Link (Canvas, Item1, Item2, Both_Arrow, "From1->2");
      Add_Link (Canvas, Item3, Item1, Start_Arrow, "From3->1");
      Add_Link (Canvas, Item1, Item4, No_Arrow, "From1->4");
      Add_Link (Canvas, Item2, Item3, End_Arrow, "From2->3");
      Add_Link (Canvas, Item2, Item4, Both_Arrow, "From2->4");
      Add_Link (Canvas, Item3, Item4, Start_Arrow, "From3->4");
      Add_Link (Canvas, Item4, Item3, End_Arrow, "From3->41");
      Add_Link (Canvas, Item3, Item4, Both_Arrow, "From3->42");
      Add_Link (Canvas, Item4, Item3, Both_Arrow, "From3->43");
      Add_Link (Canvas, Item3, Item4, Both_Arrow, "From3->44");
      Add_Link (Canvas, Item2, Item2, No_Arrow, "Self");
      Add_Link (Canvas, Item2, Item2, Start_Arrow, "Self2");

      Show_All (Frame);
   end Run;

end Create_Canvas;

