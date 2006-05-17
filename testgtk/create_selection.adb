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

with Glib;           use Glib;
with Gtk.Frame;      use Gtk.Frame;
with Gtk.Selection;  use Gtk.Selection;
with Gtk.Button;     use Gtk.Button;
with Gtk.Text;       use Gtk.Text;
with Gtk.Box;        use Gtk.Box;
with Gtk.Handlers;   use Gtk.Handlers;
with Gdk.Property;   use Gdk.Property;
with Gtk.Arguments;  use Gtk.Arguments;
with Gtk.Label;      use Gtk.Label;
with Gdk.Types;      use Gdk.Types;

with Ada.Text_IO;    use Ada.Text_IO;

package body Create_Selection is

   type My_Button_Record is new Gtk_Button_Record with record
      Text  : Gtk_Text;
      Label : Gtk_Label;
   end record;
   type My_Button is access all My_Button_Record'Class;

   package My_Button_Handler is new Gtk.Handlers.Callback
     (My_Button_Record);
   package Label_Handler is new Gtk.Handlers.Callback (Gtk_Label_Record);

   ----------
   -- Help --
   ----------

   function Help return String is
   begin
      return "This package demonstrates how you can retrieve the current"
        & " @bselection@B or the contents of the @bclipboard@B in your"
        & " application. Every function is found in the package"
        & " @bGtk.Selection@B."
        & ASCII.LF
        & "When you click on the button at the top of the demo, it will"
        & " request the selection on behalf of the @bGtk_Text@B and"
        & " @bGtk_Label@B widgets."
        & ASCII.LF
        & "As a child of @bGtk_Editable@B, there is nothing to be done for"
        & " the text widget, that automatically knows how to react to the"
        & " selection_received event, and adds the selection to its contents."
        & ASCII.LF
        & "On the other hand, this requires a little bit more work to get"
        & " the selection for the label widget. Since this is a widget with"
        & " no window associated to it, it can not directly receive the events"
        & " for the selection. Thus, the button itself will receive the"
        & " selection and update the label."
        & ASCII.LF
        & "Try selecting some other text outside of this demo, and press"
        & " once again the button at the top.";
   end Help;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click (Button : access My_Button_Record'Class) is
      Success : Boolean;
   begin
      --  Request the selection for the Text widget, as plain text.
      --  The selection should be sent to the text.

      Success := Convert
        (Button.Text,
         Selection => Selection_Primary,
         Target    => Gdk.Property.Atom_Intern ("STRING", True));

      if not Success then
         Put_Line ("Could not request a STRING selection to be sent to the"
                   & " text");
         Put_Line ("This might be because there is already such a request,");
         Put_Line (" or because the current owner of the selection does not");
         Put_Line (" know how to convert it to plain text");
      end if;

      --  Request the selection for the Label widget, as plain text.
      --  The selection should be sent to the text.
      --  Since we can not connect the label directly (it does not have any
      --  Gdk_Window associated with it), we connect the button, through
      --  an Object_Connect callback.

      Success := Convert (Button,
                          Selection => Selection_Primary,
                          Target    => Target_String);  --  same as previously.

      if not Success then
         Put_Line ("Could not request a STRING selection to be sent to the"
                   & " label");
         Put_Line ("This might be because there is already such a request,");
         Put_Line (" or because the current owner of the selection does not");
         Put_Line (" know how to convert it to plain text");
      end if;

   end On_Button_Click;

   ------------------------------
   -- Label_Selection_Received --
   ------------------------------
   --  This is the general form of handlers for the "selection_received"
   --  event.

   procedure Label_Selection_Received (Label : access Gtk_Label_Record'Class;
                                       Args : Gtk_Args)
   is
      Data : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 1));
      Time : Guint := To_Guint (Args, 2);
      pragma Warnings (Off, Time);
   begin
      if Get_Length (Data) > 0 then
         Set_Text (Label, Get_Data_As_String (Data));
      else
         Set_Text (Label, "<no_selection>");
      end if;
   end Label_Selection_Received;

   ---------
   -- Run --
   ---------

   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box    : Gtk_Box;
      Button : My_Button;
   begin
      Set_Label (Frame, "Selection");

      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);

      Button := new My_Button_Record;
      Initialize  (Button, "Paste the selection/clipboard");
      Pack_Start (Box, Button, Fill => False, Expand => False);
      My_Button_Handler.Connect
        (Button, "clicked",
         My_Button_Handler.To_Marshaller (On_Button_Click'Access));

      --  The text

      Gtk_New (Button.Text);
      Pack_Start (Box, Button.Text, Fill => True, Expand => True);

      --  The label

      Gtk_New (Button.Label, "<paste will go here>");
      Pack_Start (Box, Button.Label, Fill => False, Expand => False);

      Label_Handler.Object_Connect (Button, "selection_received",
                                    Label_Selection_Received'Access,
                                    Button.Label);

      Show_All (Frame);
   end Run;

end Create_Selection;
