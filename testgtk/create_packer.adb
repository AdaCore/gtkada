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

with Glib;               use Glib;
with Gtk.Button;         use Gtk.Button;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Frame;          use Gtk.Frame;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Object;         use Gtk.Object;
with Gtk.Packer;         use Gtk.Packer;
with Gtk.Table;          use Gtk.Table;
with Gtk.Toggle_Button;  use Gtk.Toggle_Button;
with Gtk.Widget;         use Gtk.Widget;

package body Create_Packer is

   package Side_User_Data is new Gtk.Object.User_Data (Gtk_Side_Type);
   package Anchor_User_Data is new Gtk.Object.User_Data (Gtk_Anchor_Type);
   package Options_User_Data is new
     Gtk.Object.User_Data (Gtk_Packer_Options);

   package Button_Cb is new Gtk.Handlers.Callback (Gtk_Button_Record);
   package Toggle_Cb is new Gtk.Handlers.Callback (Gtk_Toggle_Button_Record);

   type Info_Record is record
      Widgets       : Gtk.Widget.Widget_List.Glist;
      Packer        : Gtk_Packer;
      Current       : Gtk_Toggle_Button;
      Pchild        : Gtk_Packer_Child;

      Button_Top    : Gtk_Toggle_Button;
      Button_Bottom : Gtk_Toggle_Button;
      Button_Left   : Gtk_Toggle_Button;
      Button_Right  : Gtk_Toggle_Button;

      Button_N      : Gtk_Toggle_Button;
      Button_NW     : Gtk_Toggle_Button;
      Button_NE     : Gtk_Toggle_Button;
      Button_E      : Gtk_Toggle_Button;
      Button_W      : Gtk_Toggle_Button;
      Button_Center : Gtk_Toggle_Button;
      Button_S      : Gtk_Toggle_Button;
      Button_SW     : Gtk_Toggle_Button;
      Button_SE     : Gtk_Toggle_Button;

      Button_Fillx  : Gtk_Toggle_Button;
      Button_Filly  : Gtk_Toggle_Button;
      Button_Expand : Gtk_Toggle_Button;
   end record;

   Info : aliased Info_Record;

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A @bGtk_Packer@B is a container that organizes its children"
        & " according to a given location (North, East, South, West or any"
        & " combination of them)."
        & ASCII.LF
        & "Note that this whole demo is completly organized with multiple"
        & " Gtk_Packers, in addition to the one you explicitly see."
        & " Only the ""Anchor"" frame is made of a @bGtk_Table@B and not a"
        & " @bGtk_Packer@B.";
   end Help;

   --------------------
   -- Toggle_Options --
   --------------------

   procedure Toggle_Options (Button : access Gtk_Toggle_Button_Record'Class) is
      Option  : Gtk_Packer_Options;
      Fillx   : Gtk_Packer_Options := Gtk_No_Options;
      Filly   : Gtk_Packer_Options := Gtk_No_Options;
      Expand  : Gtk_Packer_Options := Gtk_No_Options;
      Tbutton : Gtk_Toggle_Button := Gtk_Toggle_Button (Button);

   begin
      if Get_Active (Button) then
         Option := Options_User_Data.Get (Button, "option");

         if Get_Active (Info.Button_Fillx) then
            Fillx := Gtk_Fill_X;
         end if;

         if Get_Active (Info.Button_Filly) then
            Filly := Gtk_Fill_Y;
         end if;

         if Get_Active (Info.Button_Expand) then
            Expand := Gtk_Pack_Expand;
         end if;

         Gtk.Packer.Set_Child_Packing
           (Info.Packer,
            Info.Current,
            Get_Side (Info.Pchild),
            Get_Anchor (Info.Pchild),
            Fillx + Filly + Expand,
            Get_Border_Width (Info.Pchild),
            Get_Pad_X (Info.Pchild),
            Get_Pad_Y (Info.Pchild),
            Get_I_Pad_X (Info.Pchild),
            Get_I_Pad_Y (Info.Pchild));

         if TButton /= Info.Button_Top then
            Set_Active (Info.Button_Top, False);
            Set_Sensitive (Info.Button_Top, True);
         end if;

         if TButton /= Info.Button_Bottom then
            Set_Active (Info.Button_Bottom, False);
            Set_Sensitive (Info.Button_Bottom, True);
         end if;

         if TButton /= Info.Button_Left then
            Set_Active (Info.Button_Left, False);
            Set_Sensitive (Info.Button_Left, True);
         end if;

         if TButton /= Info.Button_Right then
            Set_Active (Info.Button_Right, False);
            Set_Sensitive (Info.Button_Right, True);
         end if;

         Set_Sensitive (Button, False);
      end if;
   end Toggle_Options;

   -------------------
   -- Toggle_Anchor --
   -------------------

   procedure Toggle_Anchor (Button : access Gtk_Toggle_Button_Record'Class) is
      Anchor  : Gtk_Anchor_Type;
      Tbutton : Gtk_Toggle_Button := Gtk_Toggle_Button (Button);

   begin
      if Get_Active (Button) then
         Anchor := Anchor_User_Data.Get (Button, "anchor");
         Gtk.Packer.Set_Child_Packing (Info.Packer,
                                       Info.Current,
                                       Get_Side (Info.Pchild),
                                       Anchor,
                                       Get_Options (Info.Pchild),
                                       Get_Border_Width (Info.Pchild),
                                       Get_Pad_X (Info.Pchild),
                                       Get_Pad_Y (Info.Pchild),
                                       Get_I_Pad_X (Info.Pchild),
                                       Get_I_Pad_Y (Info.Pchild));
         if TButton /= Info.Button_N then
            Set_Active (Info.Button_N, False);
            Set_Sensitive (Info.Button_N, True);
         end if;
         if TButton /= Info.Button_NW then
            Set_Active (Info.Button_NW, False);
            Set_Sensitive (Info.Button_NW, True);
         end if;
         if TButton /= Info.Button_NE then
            Set_Active (Info.Button_NE, False);
            Set_Sensitive (Info.Button_NE, True);
         end if;
         if TButton /= Info.Button_S then
            Set_Active (Info.Button_S, False);
            Set_Sensitive (Info.Button_S, True);
         end if;
         if TButton /= Info.Button_SE then
            Set_Active (Info.Button_SE, False);
            Set_Sensitive (Info.Button_SE, True);
         end if;
         if TButton /= Info.Button_SW then
            Set_Active (Info.Button_SW, False);
            Set_Sensitive (Info.Button_SW, True);
         end if;
         if TButton /= Info.Button_W then
            Set_Active (Info.Button_W, False);
            Set_Sensitive (Info.Button_W, True);
         end if;
         if TButton /= Info.Button_E then
            Set_Active (Info.Button_E, False);
            Set_Sensitive (Info.Button_E, True);
         end if;
         if TButton /= Info.Button_Center then
            Set_Active (Info.Button_Center, False);
            Set_Sensitive (Info.Button_Center, True);
         end if;
         Set_Sensitive (Button, False);
      end if;
   end Toggle_Anchor;

   -----------------
   -- Toggle_Side --
   -----------------

   procedure Toggle_Side (Button : access Gtk_Toggle_Button_Record'Class) is
      Side : Gtk_Side_Type;
      Tbutton : Gtk_Toggle_Button := Gtk_Toggle_Button (Button);
   begin
      if Get_Active (Button) then
         Side := Side_User_Data.Get (Button, "side");
         Gtk.Packer.Set_Child_Packing
           (Info.Packer,
            Info.Current,
            Side,
            Get_Anchor (Info.Pchild),
            Get_Options (Info.Pchild),
            Get_Border_Width (Info.Pchild),
            Get_Pad_X (Info.Pchild),
            Get_Pad_Y (Info.Pchild),
            Get_I_Pad_X (Info.Pchild),
            Get_I_Pad_Y (Info.Pchild));

         if TButton /= Info.Button_Top then
            Set_Active (Info.Button_Top, False);
            Set_Sensitive (Info.Button_Top, True);
         end if;

         if TButton /= Info.Button_Bottom then
            Set_Active (Info.Button_Bottom, False);
            Set_Sensitive (Info.Button_Bottom, True);
         end if;

         if TButton /= Info.Button_Left then
            Set_Active (Info.Button_Left, False);
            Set_Sensitive (Info.Button_Left, True);
         end if;

         if TButton /= Info.Button_Right then
            Set_Active (Info.Button_Right, False);
            Set_Sensitive (Info.Button_Right, True);
         end if;

         Set_Sensitive (Button, False);
      end if;
   end Toggle_Side;

   ----------------
   -- Set_Widget --
   ----------------

   procedure Set_Widget (Button : access Gtk_Toggle_Button_Record'Class) is
      use type Gtk.Widget.Widget_List.Glist;
      Child   : Gtk_Packer_Child;
      Options : Gtk_Packer_Options;
      List    : Gtk.Widget.Widget_List.Glist;
      Toggle  : Gtk_Toggle_Button;
   begin
      if Get_Active (Button) then
         Info.Current := Gtk_Toggle_Button (Button);
         Child := Find_Child (Info.Packer, Info.Current);
         if Child = Null_Packer_Child then
           return;
         end if;

         Info.Pchild := Child;

         case Get_Side (Info.Pchild) is
            when Side_Top    => Set_Active (Info.Button_Top,    True);
            when Side_bottom => Set_Active (Info.Button_Bottom, True);
            when Side_Left   => Set_Active (Info.Button_Left,   True);
            when Side_Right  => Set_Active (Info.Button_Right,  True);
         end case;

         case Get_Anchor (Info.Pchild) is
            when Anchor_N      => Set_Active (Info.Button_N,      True);
            when Anchor_NW     => Set_Active (Info.Button_NW,     True);
            when Anchor_NE     => Set_Active (Info.Button_NE,     True);
            when Anchor_S      => Set_Active (Info.Button_S,      True);
            when Anchor_SW     => Set_Active (Info.Button_SW,     True);
            when Anchor_SE     => Set_Active (Info.Button_SE,     True);
            when Anchor_W      => Set_Active (Info.Button_W,      True);
            when Anchor_E      => Set_Active (Info.Button_E,      True);
            when Anchor_Center => Set_Active (Info.Button_Center, True);
         end case;

         Options := Get_Options (Info.Pchild);
         Set_Active (Info.Button_Expand,
                     (Options and Gtk_Pack_Expand) /= Gtk_No_Options);
         Set_Active (Info.Button_Fillx,
                     (Options and Gtk_Fill_X) /= Gtk_No_Options);
         Set_Active (Info.Button_Filly,
                     (Options and Gtk_Fill_Y) /= Gtk_No_Options);

         Set_Sensitive (Button, False);

         List := Info.Widgets;
         while List /= Widget_List.Null_List loop
            Toggle := Gtk_Toggle_Button (Widget_List.Get_Data (List));
            if Toggle /= Info.Current then
               Set_Active (Toggle, False);
               Set_Sensitive (Toggle, True);
            end if;
            List := Widget_List.Next (List);
         end loop;
      end if;
   end Set_Widget;

   ----------------
   -- Add_Widget --
   ----------------

   N : Natural := 0;

   procedure Add_Widget (Button : access Gtk_Button_Record'Class) is
      pragma Warnings (Off, Button);
      Widget : Gtk_Toggle_Button;
   begin
      Gtk_New (Widget, Natural'Image (N));
      Set_Usize (Widget, 40, 40);
      Add (Info.Packer, Widget);
      Show (Widget);

      Toggle_Cb.Connect (Widget, "toggled",
                         Toggle_Cb.To_Marshaller (Set_Widget'Access));
      Gtk.Widget.Widget_List.Append (Info.Widgets, Gtk_Widget (Widget));
      Set_Active (Widget, True);
      Set_Widget (Widget);
      N := N + 1;
   end Add_Widget;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Window_Pack   : Gtk_Packer;
      Top_Pack      : Gtk_Packer;
      Packer        : Gtk_Packer;
      Button_Pack   : Gtk_Packer;
      Bottom_Pack   : Gtk_Packer;
      Side_Pack     : Gtk_Packer;
      Anchor_Pack   : Gtk_Packer;
      Options_Pack  : Gtk_Packer;
      Frame2        : Gtk_Frame;
      Side_Frame    : Gtk_Frame;
      Anchor_Frame  : Gtk_Frame;
      Options_Frame : Gtk_Frame;
      Button_Add    : Gtk_Button;
      Button_Top    : Gtk_Toggle_Button;
      Button_Bottom : Gtk_Toggle_Button;
      Button_Left   : Gtk_Toggle_Button;
      Button_Right  : Gtk_Toggle_Button;
      Button_N      : Gtk_Toggle_Button;
      Button_S      : Gtk_Toggle_Button;
      Button_W      : Gtk_Toggle_Button;
      Button_E      : Gtk_Toggle_Button;
      Button_NE     : Gtk_Toggle_Button;
      Button_NW     : Gtk_Toggle_Button;
      Button_SE     : Gtk_Toggle_Button;
      Button_SW     : Gtk_Toggle_Button;
      Button_Center : Gtk_Toggle_Button;
      Button_Fillx  : Gtk_Toggle_Button;
      Button_Filly  : Gtk_Toggle_Button;
      Button_Expand : Gtk_Toggle_Button;
      Anchor_Table  : Gtk_Table;

   begin
      Gtk.Frame.Set_Label (Frame, "Packer");

      Gtk_New (Window_Pack);
      Add (Frame, Window_Pack);
      Set_Border_Width (Window_Pack, 4);

      Gtk_New (Top_Pack);
      Gtk.Packer.Add_Defaults
        (Packer  => Window_Pack,
         Child   => Top_Pack,
         Side    => Side_Top,
         Anchor  => Anchor_Center,
         Options => Gtk_Fill_X + Gtk_Fill_Y + Gtk_Pack_Expand);

      Gtk_New (Frame2, "Packing Area");
      Set_Usize (Frame2, 350, 350);
      Gtk.Packer.Add
        (Packer       => Top_Pack,
         Child        => Frame2,
         Side         => Side_Left,
         Anchor       => Anchor_Center,
         Options      => Gtk_Fill_X + Gtk_Fill_Y + Gtk_Pack_Expand,
         Border_Width => 0,
         Pad_X        => 8,
         Pad_Y        => 8,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Gtk_New (Packer);
      Add (Frame2, Packer);

      Gtk_New (Button_Pack);
      Gtk.Packer.Add
        (Packer       => Top_Pack,
         Child        => Button_Pack,
         Side         => Side_Left,
         Anchor       => Anchor_North,
         Options      => Gtk_No_Options,
         Border_Width => 0,
         Pad_X        => 0,
         Pad_Y        => 0,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Gtk_New (Button_Add, "Add Button");
      Gtk.Packer.Add
        (Packer       => Top_Pack,
         Child        => Button_Add,
         Side         => Side_Top,
         Anchor       => Anchor_Center,
         Options      => Gtk_Fill_X,
         Border_Width => 0,
         Pad_X        => 8,
         Pad_Y        => 8,
         I_Pad_X      => 8,
         I_Pad_Y      => 0);
      Button_Cb.Connect (Button_Add, "clicked",
                         Button_Cb.To_Marshaller (Add_Widget'Access));

      Gtk_New (Bottom_Pack);
      Gtk.Packer.Add_Defaults (Window_Pack,
                               Child   => Bottom_Pack,
                               Side    => Side_Top,
                               Anchor  => Anchor_Center,
                               Options => Gtk_Fill_X);

      -----------------------------
      -- The Side Frame + Packer --
      -----------------------------

      Gtk_New (Side_Frame, "Side");
      Gtk.Packer.Add
        (Packer       => Window_Pack,
         Child        => Side_Frame,
         Side         => Side_Left,
         Anchor       => Anchor_West,
         Options      => Gtk_Fill_Y,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 10,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Gtk_New (Side_Pack);
      Add (Side_Frame, Side_Pack);

      Gtk_New (Button_Top, "Top");
      Gtk_New (Button_Bottom, "Bottom");
      Gtk_New (Button_Left, "Left");
      Gtk_New (Button_Right, "Right");

      Side_User_Data.Set (Button_Top,    Side_Top,    "side");
      Side_User_Data.Set (Button_Bottom, Side_Bottom, "side");
      Side_User_Data.Set (Button_Left,   Side_Left,   "side");
      Side_User_Data.Set (Button_Right,  Side_Right,  "side");

      Set_Usize (Button_Top, 50, -1);
      Set_Usize (Button_Bottom, 50, -1);
      Set_Usize (Button_Left, 50, -1);
      Set_Usize (Button_Right, 50, -1);

      Gtk.Packer.Add
        (Packer       => Side_Pack,
         Child        => Button_Top,
         Side         => Side_Top,
         Anchor       => Anchor_Center,
         Options      => Gtk_No_Options,
         Border_Width => 0,
         Pad_X        => 5,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);
      Gtk.Packer.Add
        (Packer       => Side_Pack,
         Child        => Button_Bottom,
         Side         => Side_Bottom,
         Anchor       => Anchor_Center,
         Options      => Gtk_No_Options,
         Border_Width => 0,
         Pad_X        => 5,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);
      Gtk.Packer.Add
        (Packer       => Side_Pack,
         Child        => Button_Left,
         Side         => Side_Left,
         Anchor       => Anchor_Center,
         Options      => Gtk_No_Options,
         Border_Width => 0,
         Pad_X        => 5,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);
      Gtk.Packer.Add
        (Packer       => Side_Pack,
         Child        => Button_Right,
         Side         => Side_Right,
         Anchor       => Anchor_Center,
         Options      => Gtk_No_Options,
         Border_Width => 0,
         Pad_X        => 5,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Toggle_Cb.Connect (Button_Top, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Side'Access));
      Toggle_Cb.Connect (Button_Bottom, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Side'Access));
      Toggle_Cb.Connect (Button_Left, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Side'Access));
      Toggle_Cb.Connect (Button_Right, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Side'Access));

      -------------------------------
      -- The Anchor Frame + Packer --
      -------------------------------

      Gtk_New (Anchor_Frame, "Anchor");
      Gtk.Packer.Add
        (Packer       => Window_Pack,
         Child        => Anchor_Frame,
         Side         => Side_Left,
         Anchor       => Anchor_West,
         Options      => Gtk_Fill_Y,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 10,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Gtk_New (Anchor_Pack);
      Add (Anchor_Frame, Anchor_Pack);

      Gtk_New (Anchor_Table, 3, 3, True);
      Gtk.Packer.Add
        (Packer       => Anchor_Pack,
         Child        => Anchor_Table,
         Side         => Side_Top,
         Anchor       => Anchor_Center,
         Options      => Gtk_Fill_X + Gtk_Fill_Y + Gtk_Pack_Expand,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Gtk_New (Button_N, "N");
      Gtk_New (Button_S, "S");
      Gtk_New (Button_W, "W");
      Gtk_New (Button_E, "E");
      Gtk_New (Button_NE, "NE");
      Gtk_New (Button_NW, "NW");
      Gtk_New (Button_SE, "SE");
      Gtk_New (Button_SW, "SW");
      Gtk_New (Button_Center, "");

      Anchor_User_Data.Set (Button_N,      Anchor_N,      "anchor");
      Anchor_User_Data.Set (Button_S,      Anchor_S,      "anchor");
      Anchor_User_Data.Set (Button_W,      Anchor_W,      "anchor");
      Anchor_User_Data.Set (Button_E,      Anchor_E,      "anchor");
      Anchor_User_Data.Set (Button_NE,     Anchor_NE,     "anchor");
      Anchor_User_Data.Set (Button_NW,     Anchor_NW,     "anchor");
      Anchor_User_Data.Set (Button_SE,     Anchor_SE,     "anchor");
      Anchor_User_Data.Set (Button_SW,     Anchor_SW,     "anchor");
      Anchor_User_Data.Set (Button_Center, Anchor_Center, "anchor");

      Toggle_Cb.Connect (Button_N, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_E, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_S, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_W, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_NE, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_NW, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_SE, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_SW, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));
      Toggle_Cb.Connect (Button_Center, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Anchor'Access));

      Gtk.Table.Attach_Defaults (Anchor_Table, Button_Nw,     0, 1, 0, 1);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_N,      1, 2, 0, 1);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_Ne,     2, 3, 0, 1);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_W,      0, 1, 1, 2);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_Center, 1, 2, 1, 2);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_E,      2, 3, 1, 2);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_SW,     0, 1, 2, 3);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_S,      1, 2, 2, 3);
      Gtk.Table.Attach_Defaults (Anchor_Table, Button_SE,     2, 3, 2, 3);

      --------------------------------
      -- The Options frame + Packer --
      --------------------------------

      Gtk_New (Options_Frame, "Options");
      Gtk.Packer.Add
        (Packer       => Window_Pack,
         Child        => Options_Frame,
         Side         => Side_Left,
         Anchor       => Anchor_West,
         Options      => Gtk_Fill_Y,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 10,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Gtk_New (Options_Pack);
      Add (Options_Frame, Options_Pack);

      Gtk_New (Button_Fillx,  "Fill X");
      Gtk_New (Button_Filly,  "Fill Y");
      Gtk_New (Button_Expand, "Expand");

      Gtk.Packer.Add
        (Packer       => Options_Pack,
         Child        => Button_Fillx,
         Side         => Side_Top,
         Anchor       => Anchor_North,
         Options      => Gtk_Fill_X + Gtk_Pack_Expand,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);
      Gtk.Packer.Add
        (Packer       => Options_Pack,
         Child        => Button_Filly,
         Side         => Side_Top,
         Anchor       => Anchor_Center,
         Options      => Gtk_Fill_X + Gtk_Pack_Expand,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);
      Gtk.Packer.Add
        (Packer       => Options_Pack,
         Child        => Button_Expand,
         Side         => Side_Top,
         Anchor       => Anchor_South,
         Options      => Gtk_Fill_X + Gtk_Pack_Expand,
         Border_Width => 0,
         Pad_X        => 10,
         Pad_Y        => 5,
         I_Pad_X      => 0,
         I_Pad_Y      => 0);

      Options_User_Data.Set (Button_Fillx,  Gtk_Fill_X,      "option");
      Options_User_Data.Set (Button_Filly,  Gtk_Fill_Y,      "option");
      Options_User_Data.Set (Button_Expand, Gtk_Pack_Expand, "option");

      Toggle_Cb.Connect (Button_Fillx, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Options'Access));
      Toggle_Cb.Connect (Button_Filly, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Options'Access));
      Toggle_Cb.Connect (Button_Expand, "toggled",
                         Toggle_Cb.To_Marshaller (Toggle_Options'Access));

      Info := (Widgets       => Gtk.Widget.Widget_List.Null_List,
               Packer        => Packer,
               Current       => null,
               Pchild        => Null_Packer_Child,
               Button_Top    => Button_Top,
               Button_Bottom => Button_Bottom,
               Button_Left   => Button_Left,
               Button_Right  => Button_Right,
               Button_N      => Button_N,
               Button_NW     => Button_NW,
               Button_NE     => Button_NE,
               Button_E      => Button_E,
               Button_W      => Button_W,
               Button_Center => Button_Center,
               Button_S      => Button_S,
               Button_SW     => Button_SW,
               Button_SE     => Button_SE,
               Button_Fillx  => Button_Fillx,
               Button_Filly  => Button_Filly,
               Button_Expand => Button_Expand);

      Add_Widget (Button_Add);

      Show_All (Frame);
   end Run;


end Create_Packer;
