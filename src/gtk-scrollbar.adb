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
     (Widget     : access Gtk_Scrollbar_Record;
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
     (Widget     : access Gtk_Scrollbar_Record;
      Adjustment : in Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : in System.Address)
                         return          System.Address;
      pragma Import (C, Internal, "gtk_vscrollbar_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Adjustment)));
      Initialize_User_Data (Widget);
   end Initialize_Vscrollbar;

end Gtk.Scrollbar;
