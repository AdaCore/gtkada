-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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
with Gtk.Button; use Gtk.Button;
with Gtk.Enums; use Gtk.Enums;
with Gtk.File_Selection; use Gtk.File_Selection;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Common; use Common;
with Gtk; use Gtk;

with Ada.Text_IO;

package body Create_File_Selection is

   package Files_Cb is new Signal.Object_Callback (Gtk_File_Selection);

   Window : aliased Gtk_File_Selection;

   procedure Ok (Files : in out Gtk_File_Selection) is
   begin
      Ada.Text_IO.Put_Line ("Selected " & Get_Filename (Files));
      Destroy (Files);
   end Ok;


   procedure Run (Widget : in out Gtk.Button.Gtk_Button) is
      Id     : Guint;
      Button : Gtk_Button;
   begin

      if not Is_Created (Window) then
         Gtk_New (Window, Title => "File Selection Dialog");
         Hide_Fileop_Buttons (Window);
         Position (Window, Win_Pos_Mouse);
         Id := Widget2_Cb.Connect (Window, "destroy", Destroyed'Access,
                                  Window'Access);
         Id := Files_Cb.Connect (Get_Ok_Button (Window), "clicked",
                                 Ok'Access, Window);
         Id := Widget_Cb.Connect (Get_Cancel_Button (Window), "clicked",
                                  Gtk.Widget.Destroy'Access, Window);

         Gtk_New (Button, Label => "Hide Fileops");
         Id := Files_Cb.Connect (Button, "clicked", Hide_Fileop_Buttons'Access,
                                 Window);
         Pack_Start (Get_Action_Area (Window), Button, False, False, 0);
         Show (Button);

         Gtk_New (Button, Label => "Show Fileops");
         Id := Files_Cb.Connect (Button, "clicked", Show_Fileop_Buttons'Access,
                                 Window);
         Pack_Start (Get_Action_Area (Window), Button, False, False, 0);
         Show (Button);

      end if;

      if not Gtk.Widget.Visible_Is_Set (Window) then
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_File_Selection;

