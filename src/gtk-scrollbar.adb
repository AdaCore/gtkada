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

with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;

package body Gtk.Scrollbar is

   ------------------------
   -- Gtk_New_Hscrollbar --
   ------------------------

   procedure Gtk_New_Hscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget := new Gtk_Scrollbar_Record;
      Initialize_Hscrollbar (Widget, Adjustment);
   end Gtk_New_Hscrollbar;

   ------------------------
   -- Gtk_New_Vscrollbar --
   ------------------------

   procedure Gtk_New_Vscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget := new Gtk_Scrollbar_Record;
      Initialize_Vscrollbar (Widget, Adjustment);
   end Gtk_New_Vscrollbar;

   ---------------------------
   -- Initialize_Hscrollbar --
   ---------------------------

   procedure Initialize_Hscrollbar
     (Widget     : access Gtk_Scrollbar_Record'Class;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_hscrollbar_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Widget);
   end Initialize_Hscrollbar;

   ---------------------------
   -- Initialize_Vscrollbar --
   ---------------------------

   procedure Initialize_Vscrollbar
     (Widget     : access Gtk_Scrollbar_Record'Class;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_vscrollbar_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Widget);
   end Initialize_Vscrollbar;

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      S     : String_Ptr;
      Class : String_Ptr := Get_Field (N, "class");

   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "name");
         Add_Package ("Adjustment");

         if Get_Field (N, "class").all = "GtkHScrollbar" then
            Put_Line
              (File, "   Adjustment.Gtk_New (" & To_Ada (S.all) & "_Adj, " &
               To_Float (Get_Field (N, "hvalue").all) & ", " &
               To_Float (Get_Field (N, "hlower").all) & ", " &
               To_Float (Get_Field (N, "hupper").all) & ", " &
               To_Float (Get_Field (N, "hstep").all)  & ", " &
               To_Float (Get_Field (N, "hpage").all)  & ", " &
               To_Float (Get_Field (N, "hpage_size").all) & ");");

            Gen_New (N, "Scrollbar", S.all & "Adj", "",
                     Class (Class'First + 3) & "scrollbar", File => File);
         else
            Put_Line
              (File, "   Adjustment.Gtk_New (" & To_Ada (S.all) & "_Adj, " &
               To_Float (Get_Field (N, "vvalue").all) & ", " &
               To_Float (Get_Field (N, "vlower").all) & ", " &
               To_Float (Get_Field (N, "vupper").all) & ", " &
               To_Float (Get_Field (N, "vstep").all)  & ", " &
               To_Float (Get_Field (N, "vpage").all)  & ", " &
               To_Float (Get_Field (N, "vpage_size").all) & ");");

            Gen_New (N, "Scrollbar", S.all & "Adj", "",
                     Class (Class'First + 3) & "scrollbar", File => File);
         end if;
      end if;

      GRange.Generate (N, File);
   end Generate;

   procedure Generate
     (Scrollbar : in out Object.Gtk_Object; N : in Node_Ptr)
   is
      Adj : Adjustment.Gtk_Adjustment;
      Class : String_Ptr := Get_Field (N, "class");

   begin
      if not N.Specific_Data.Created then

         if Class (Class'First + 3) = 'H' then
            Adjustment.Gtk_New
              (Adj,
               Gfloat'Value (Get_Field (N, "hvalue").all),
               Gfloat'Value (Get_Field (N, "hlower").all),
               Gfloat'Value (Get_Field (N, "hupper").all),
               Gfloat'Value (Get_Field (N, "hstep").all),
               Gfloat'Value (Get_Field (N, "hpage").all),
               Gfloat'Value (Get_Field (N, "hpage_size").all));
            Gtk_New_Hscrollbar (Gtk_Scrollbar (Scrollbar), Adj);
         else
            Adjustment.Gtk_New
              (Adj,
               Gfloat'Value (Get_Field (N, "vvalue").all),
               Gfloat'Value (Get_Field (N, "vlower").all),
               Gfloat'Value (Get_Field (N, "vupper").all),
               Gfloat'Value (Get_Field (N, "vstep").all),
               Gfloat'Value (Get_Field (N, "vpage").all),
               Gfloat'Value (Get_Field (N, "vpage_size").all));
            Gtk_New_Vscrollbar (Gtk_Scrollbar (Scrollbar), Adj);
         end if;

         Set_Object (Get_Field (N, "name"), Scrollbar);
         N.Specific_Data.Created := True;
      end if;

      GRange.Generate (Scrollbar, N);
   end Generate;

end Gtk.Scrollbar;
