-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Gtk;          use Gtk;
with Gtk.Box;      use Gtk.Box;
with Gtk.Button;   use Gtk.Button;
with Gtk.Frame;    use Gtk.Frame;
with Gtk.Widget;   use Gtk.Widget;
with Gtkada.Multi_Paned; use Gtkada.Multi_Paned;
with Gtk.Menu_Bar; use Gtk.Menu_Bar;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Enums;    use Gtk.Enums;

package body Create_Splittable is

   function Create_Button
      (Bar : Gtk_Menu_Bar; Title : String) return Gtk_Button;
   procedure Destroyed (Button : access Gtk_Widget_Record'Class);
   procedure Toggled (Button : access Gtk_Widget_Record'Class);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "A Gtkada-specific widget, where children can be resized"
        & " interactively by the user, as well as splitted.";
   end Help;

   ---------------
   -- Destroyed --
   ---------------

   procedure Destroyed (Button : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Button);
   end Destroyed;
                                                                                
   -------------
   -- Toggled --
   -------------

   procedure Toggled (Button : access Gtk_Widget_Record'Class) is
   begin
      if Visible_Is_Set (Button) then
         Hide (Button);
      else
         Show (Button);
      end if;
   end Toggled;

   -------------------
   -- Create_Button --
   -------------------

   function Create_Button
      (Bar : Gtk_Menu_Bar; Title : String) return Gtk_Button
   is
      Button : Gtk_Button;
      Item   : Gtk_Menu_Item;
   begin
      Gtk_New (Button, "Destroy" & ASCII.LF & Title);
      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Destroyed'Unrestricted_Access));
                                                                                
      Gtk_New (Item, "Toggle_" & Title);
      Append (Bar, Item);
      Widget_Callback.Object_Connect
        (Item, "activate",
         Widget_Callback.To_Marshaller (Toggled'Unrestricted_Access),
         Button);
                                                                                
      return Button;
   end Create_Button;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Pane   : Gtkada_Multi_Paned;
      Button, Button1, Button2, Button3, Button4 : Gtk_Button;
      Bar : Gtk_Menu_Bar;
      Box : Gtk_Box;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);
                                                                                
      Gtk_New (Bar);
      Pack_Start (Box, Bar, Expand => False);
                                                                                
      Gtk_New (Pane);
      Pack_Start (Box, Pane, Expand => True, Fill => True);
                                                                                
      Button1 := Create_Button (Bar, "First");
      Add_Child (Pane, Button1);
                                                                                
      Button2 := Create_Button (Bar, "Second");
      Add_Child (Pane, Button2);  --  Should split horizontally
                                                                                
      Button3 := Create_Button (Bar, "Third");
      Add_Child (Pane, Button3);  --  Should split horizontally
                                                                                
      Button4 := Create_Button (Bar, "Fourth");
      Split (Pane, Button2, Button4, Orientation_Vertical);
                                                                                
      Button := Create_Button (Bar, "Fifth");
      Split (Pane, Button4, Button, Orientation_Horizontal);
                                                                                
      Show_All (Frame);
   end Run;

end Create_Splittable;

