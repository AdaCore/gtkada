-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Gtk; use Gtk;
with Gtk.Adjustment;   use Gtk.Adjustment;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Signal;
with Gtk.Label; use Gtk.Label;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Window; use Gtk.Window;
with Interfaces.C.Strings;

package Common is

    --  This package is created to avoid the instanciation of the
    --  generic packages for callbacks. This provides a much smaller
    --  executable

   package Widget_Cb is new Signal.Object_Callback (Gtk_Widget_Record);
   package Widget2_Cb is new Signal.Callback (Gtk_Widget_Record, Gtk_Widget);
   package Widget3_Cb is new Signal.Void_Callback (Gtk_Widget_Record);
   package Label_Cb is new Signal.Object_Callback (Gtk_Label_Record);
   package Adj_Cb is new Signal.Void_Callback (Gtk_Adjustment_Record);
   package Check_Cb is new Signal.Void_Callback (Gtk_Check_Button_Record);


   type Gtk_Window_Access is access all Gtk_Window;
   package Destroy_Cb is new Signal.Callback (Gtk_Window_Record,
                                              Gtk_Window_Access);
   procedure Destroy_Window (Win : access Gtk.Window.Gtk_Window_Record;
                             Ptr : in Gtk_Window_Access);

   package ICS renames Interfaces.C.Strings;
   Book_Open_Xpm    : ICS.chars_ptr_array :=
     (ICS.New_String ("16 16 4 1"),
      ICS.New_String ("       c None s None"),
      ICS.New_String (".      c black"),
      ICS.New_String ("X      c #808080"),
      ICS.New_String ("o      c white"),
      ICS.New_String ("                "),
      ICS.New_String ("  ..            "),
      ICS.New_String (" .Xo.    ...    "),
      ICS.New_String (" .Xoo. ..oo.    "),
      ICS.New_String (" .Xooo.Xooo...  "),
      ICS.New_String (" .Xooo.oooo.X.  "),
      ICS.New_String (" .Xooo.Xooo.X.  "),
      ICS.New_String (" .Xooo.oooo.X.  "),
      ICS.New_String (" .Xooo.Xooo.X.  "),
      ICS.New_String (" .Xooo.oooo.X.  "),
      ICS.New_String ("  .Xoo.Xoo..X.  "),
      ICS.New_String ("   .Xo.o..ooX.  "),
      ICS.New_String ("    .X..XXXXX.  "),
      ICS.New_String ("    ..X.......  "),
      ICS.New_String ("     ..         "),
      ICS.New_String ("                "));
   Book_Closed_Xpm  : ICS.chars_ptr_array :=
     (ICS.New_String ("16 16 6 1"),
      ICS.New_String ("       c None s None"),
      ICS.New_String (".      c black"),
      ICS.New_String ("X      c red"),
      ICS.New_String ("o      c yellow"),
      ICS.New_String ("O      c #808080"),
      ICS.New_String ("#      c white"),
      ICS.New_String ("                "),
      ICS.New_String ("       ..       "),
      ICS.New_String ("     ..XX.      "),
      ICS.New_String ("   ..XXXXX.     "),
      ICS.New_String (" ..XXXXXXXX.    "),
      ICS.New_String (".ooXXXXXXXXX.   "),
      ICS.New_String ("..ooXXXXXXXXX.  "),
      ICS.New_String (".X.ooXXXXXXXXX. "),
      ICS.New_String (".XX.ooXXXXXX..  "),
      ICS.New_String (" .XX.ooXXX..#O  "),
      ICS.New_String ("  .XX.oo..##OO. "),
      ICS.New_String ("   .XX..##OO..  "),
      ICS.New_String ("    .X.#OO..    "),
      ICS.New_String ("     ..O..      "),
      ICS.New_String ("      ..        "),
      ICS.New_String ("                "));

   Mini_Page_Xpm  : ICS.chars_ptr_array :=
     (ICS.New_String ("16 16 4 1"),
      ICS.New_String ("       c None s None"),
      ICS.New_String (".      c black"),
      ICS.New_String ("X      c white"),
      ICS.New_String ("O      c #808080"),
      ICS.New_String ("                "),
      ICS.New_String ("   .......      "),
      ICS.New_String ("   .XXXXX..     "),
      ICS.New_String ("   .XoooX.X.    "),
      ICS.New_String ("   .XXXXX....   "),
      ICS.New_String ("   .XooooXoo.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   .XooooooX.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   .XooooooX.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   .XooooooX.o  "),
      ICS.New_String ("   .XXXXXXXX.o  "),
      ICS.New_String ("   ..........o  "),
      ICS.New_String ("    oooooooooo  "),
      ICS.New_String ("                "));

   Gtk_Mini_Xpm  : ICS.chars_ptr_array :=
     (ICS.New_String ("15 20 17 1"),
      ICS.New_String ("       c None"),
      ICS.New_String (".      c #14121F"),
      ICS.New_String ("+      c #278828"),
      ICS.New_String ("@      c #9B3334"),
      ICS.New_String ("#      c #284C72"),
      ICS.New_String ("$      c #24692A"),
      ICS.New_String ("%      c #69282E"),
      ICS.New_String ("&      c #37C539"),
      ICS.New_String ("*      c #1D2F4D"),
      ICS.New_String ("=      c #6D7076"),
      ICS.New_String ("-      c #7D8482"),
      ICS.New_String (";      c #E24A49"),
      ICS.New_String (">      c #515357"),
      ICS.New_String (",      c #9B9C9B"),
      ICS.New_String ("'      c #2FA232"),
      ICS.New_String (")      c #3CE23D"),
      ICS.New_String ("!      c #3B6CCB"),
      ICS.New_String ("               "),
      ICS.New_String ("      ***>     "),
      ICS.New_String ("    >.*!!!*    "),
      ICS.New_String ("   ***....#*=  "),
      ICS.New_String ("  *!*.!!!**!!# "),
      ICS.New_String (" .!!#*!#*!!!!# "),
      ICS.New_String (" @%#!.##.*!!$& "),
      ICS.New_String (" @;%*!*.#!#')) "),
      ICS.New_String (" @;;@%!!*$&)'' "),
      ICS.New_String (" @%.%@%$'&)$+' "),
      ICS.New_String (" @;...@$'*'*)+ "),
      ICS.New_String (" @;%..@$+*.')$ "),
      ICS.New_String (" @;%%;;$+..$)# "),
      ICS.New_String (" @;%%;@$$$'.$# "),
      ICS.New_String (" %;@@;;$$+))&* "),
      ICS.New_String ("  %;;;@+$&)&*  "),
      ICS.New_String ("   %;;@'))+>   "),
      ICS.New_String ("    %;@'&#     "),
      ICS.New_String ("     >%$$      "),
      ICS.New_String ("      >=       "));

end Common;
