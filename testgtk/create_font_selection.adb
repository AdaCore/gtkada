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
with Gtk.Enums; use Gtk.Enums;
with Gtk.Font_Selection;  use Gtk.Font_Selection;
with Gtk.Signal; use Gtk.Signal;
with Gtk.Widget; use Gtk.Widget;
with Common; use Common;
with Gtk; use Gtk;

with Text_IO; use Text_IO;

package body Create_Font_Selection is

   type Gtk_Font_Selection_Access is access all Gtk_Font_Selection_Dialog;
   package Destroy_Cb is new Signal.Callback
     (Gtk_Font_Selection_Dialog_Record, Gtk_Font_Selection_Access);
   procedure Destroyed (Win : access Gtk_Font_Selection_Dialog_Record;
                        Ptr : in Gtk_Font_Selection_Access);

   procedure Destroyed (Win : access Gtk_Font_Selection_Dialog_Record;
                        Ptr : in Gtk_Font_Selection_Access) is
   begin
      Ptr.all := null;
   end Destroyed;

   package Fs_Cb is new Signal.Object_Callback
     (Gtk_Font_Selection_Dialog_Record);

   Window : aliased Gtk_Font_Selection_Dialog;


   procedure Selection_OK (Fs : access Gtk_Font_Selection_Dialog_Record) is
   begin
      Put_Line (Get_Font_Name (Fs));
      Destroy (Fs);
   end Selection_OK;


   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Id      : Guint;
   begin

      Set_Label (Frame, "Font Selection");

      if Window = null then
         Gtk_New (Window, "Font Selection Dialog");
         Set_Position (Window, Win_Pos_Mouse);

         Id := Destroy_Cb.Connect
           (Window, "destroy", Destroyed'Access, Window'Access);
         Id := Fs_Cb.Connect
           (Get_Ok_Button (Window), "clicked", Selection_Ok'Access, Window);
         Id := Widget_Cb.Connect
           (Get_Cancel_Button (Window), "clicked", Gtk.Widget.Destroy'Access,
            Window);
         Show (Window);
      else
         Destroy (Window);
      end if;

   end Run;

end Create_Font_Selection;

