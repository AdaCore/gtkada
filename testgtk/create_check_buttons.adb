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

with Glib;              use Glib;
with Gtk;               use Gtk;
with Gtk.Box;           use Gtk.Box;
with Gtk.Button;        use Gtk.Button;
with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Enums;
with Gtk.Separator;     use Gtk.Separator;
with Gtk.Signal;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Common;            use Common;

package body Create_Check_Buttons is

   package Exit_Cb is new Signal.Object_Callback
     (Base_Type => Gtk.Widget.Gtk_Widget_Record);
   --  Must be instanciated at library level !


   New_Window : aliased Window.Gtk_Window;


   ----------------------------------------------------------------------

   procedure Run (Widget : access Gtk.Button.Gtk_Button_Record) is
      Box1, Box2 : Box.Gtk_Box;
      A_Button : Button.Gtk_Button;
      A_Check_Button : Check_Button.Gtk_Check_Button;
      A_Separator : Separator.Gtk_Separator;
      Cb_Id : Guint;
   begin

      if New_Window = null then
         Window.Gtk_New (Window => New_Window,
                         The_Type => Enums.Window_Toplevel);
         Cb_Id := Destroy_Cb.Connect (New_Window, "destroy",
                                      Destroy_Window'Access,
                                      New_Window'Access);
         Window.Set_Title (Window => New_Window,
                           Title => "GtkCheckButton");
         Set_Border_Width (Container => New_Window, Border_Width => 0);

         Box.Gtk_New_Vbox (Box1, Homogeneous => False, Spacing => 0);
         Add (Container => New_Window, Widget => Box1);

         Box.Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 10);
         Set_Border_Width (Container => Box2, Border_Width => 10);
         Box.Pack_Start (In_Box => Box1, Child => Box2);

         Check_Button.Gtk_New (A_Check_Button, With_Label => "button1");
         Box.Pack_Start (In_Box => Box2, Child => A_Check_Button);

         Check_Button.Gtk_New (A_Check_Button, With_Label => "button2");
         Box.Pack_Start (In_Box => Box2, Child => A_Check_Button);

         Check_Button.Gtk_New (A_Check_Button, With_Label => "button3");
         Box.Pack_Start (In_Box => Box2, Child => A_Check_Button);

         Gtk.Separator.Gtk_New_Hseparator (A_Separator);
         Box.Pack_Start (In_Box => Box1, Child => A_Separator,
                         Expand => False);

         Box.Gtk_New_Vbox (Box2, Homogeneous => False, Spacing => 10);
         Set_Border_Width (Container => Box2, Border_Width => 10);
         Box.Pack_Start (In_Box => Box1, Child => Box2, Expand => False);

         Button.Gtk_New ( A_Button, Label => "close");
         Cb_Id := Exit_Cb.Connect (Obj => A_Button,
                                   Name => "clicked",
                                   Func => Gtk.Widget.Destroy'Access,
                                   Slot_Object => New_Window);
         Box.Pack_Start (In_Box => Box2, Child => A_Button);
         Set_Flags (Object => A_Button, Flags => Gtk.Widget.Can_Default);
         Grab_Default (A_Button);
         Show_All (New_Window);
      else
         Destroy (New_Window);
      end if;

   end Run;

end Create_Check_Buttons;
