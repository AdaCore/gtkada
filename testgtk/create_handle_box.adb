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

with Glib; use Glib;
with Gtk.Box; use Gtk.Box;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handle_Box; use Gtk.Handle_Box;
with Gtk.Label; use Gtk.Label;
with Gtk.Separator; use Gtk.Separator;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Style;  use Gtk.Style;
with Gtk.Toolbar; use Gtk.Toolbar;
with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;

with Ada.Text_IO;
with Create_Toolbar;

package body Create_Handle_Box is

   package Handle_Cb is new Signal.Two_Callback
     (Gtk_Handle_Box_Record, String, Gtk_Widget_Record);

   procedure Child_Signal (Handle : access Gtk_Handle_Box_Record;
                           Child  : in Gtk_Widget_Record;
                           Data   : in String) is
      Tmp : aliased Gtk_Widget_Record := Child;
   begin
      Ada.Text_IO.Put_Line (Type_Name (Get_Type (Handle))
                            & ": child <"
                            & Type_Name (Get_Type (Tmp'Access))
                            & "> "
                            & Data);
   end Child_Signal;


   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Id        : Guint;
      Vbox      : Gtk_Box;
      Hbox      : Gtk_Box;
      Label     : Gtk_Label;
      Separator : Gtk_Separator;
      Handle    : Gtk_Handle_Box;
      Handle2   : Gtk_Handle_Box;
      Toolbar   : Gtk_Toolbar;
   begin
      Set_Label (Frame, "Handle Box");
      Gtk_New_Vbox (Vbox,
                    Homogeneous => False,
                    Spacing     => 0);
      Add (Frame, Vbox);

      Gtk_New (Label, "Above");
      Pack_Start (Vbox, Label, False, False);

      Gtk_New_Hseparator (Separator);
      Pack_Start (Vbox, Separator, False, False);

      Gtk_New_Hbox (Hbox,
                    Homogeneous => False,
                    Spacing     => 10);
      Pack_Start (Vbox, Hbox, False, False);

      Gtk_New_Hseparator (Separator);
      Pack_Start (Vbox, Separator, False, False);

      Gtk_New (Label, "Below");
      Pack_Start (Vbox, Label, False, False);

      Gtk_New (Handle);
      Pack_Start (Hbox,
                  Child   => Handle,
                  Expand  => False,
                  Fill    => False,
                  Padding => 0);
      Id := Handle_Cb.Connect (Handle, "child_attached",
                               Child_Signal'Access, "attached");
      Id := Handle_Cb.Connect (Handle, "child_detached",
                               Child_Signal'Access, "detached");

      Create_Toolbar.Make_Toolbar (Toolbar, Get_Window (Frame),
                                   Get_Style (Frame));
      Set_Button_Relief (Toolbar, Relief_Normal);
      Add (Handle, Toolbar);

      Gtk_New (Handle);
      Pack_Start (Hbox,
                  Child   => Handle,
                  Expand  => False,
                  Fill    => False,
                  Padding => 0);
      Id := Handle_Cb.Connect (Handle, "child_attached",
                               Child_Signal'Access, "attached");
      Id := Handle_Cb.Connect (Handle, "child_detached",
                               Child_Signal'Access, "detached");

      Gtk_New (Handle2);
      Add (Handle, Handle2);
      Id := Handle_Cb.Connect (Handle2, "child_attached",
                               Child_Signal'Access, "attached");
      Id := Handle_Cb.Connect (Handle2, "child_detached",
                               Child_Signal'Access, "detached");

      Gtk_New (Label, "Fooo!");
      Add (Handle2, Label);

      Show_All (Frame);
   end Run;

end Create_Handle_Box;
