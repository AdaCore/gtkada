-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gtk.Widget; use Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Gnome.Number_Entry is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget            : out Gnome_Number_Entry;
      History_Id        : String;
      Calc_Dialog_Title : String)
   is
   begin
      Widget := new Gnome_Number_Entry_Record;
      Initialize (Widget, History_Id, Calc_Dialog_Title);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget            : access Gnome_Number_Entry_Record'Class;
      History_Id        : String;
      Calc_Dialog_Title : String)
   is
      function Internal
        (History_Id        : String;
         Calc_Dialog_Title : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_number_entry_new");
   begin
      Set_Object (Widget, Internal (History_Id & ASCII.NUL,
                                    Calc_Dialog_Title & ASCII.NUL));
   end Initialize;

   -----------------
   -- Gnome_Entry --
   -----------------

   function Gnome_Entry
     (Nentry : access Gnome_Number_Entry_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Nentry : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_number_entry_gnome_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Nentry)));
   end Gnome_Entry;

   ---------------
   -- Gtk_Entry --
   ---------------

   function Gtk_Entry
     (Nentry : access Gnome_Number_Entry_Record) return Gtk.Widget.Gtk_Widget
   is
      function Internal (Nentry : System.Address) return System.Address;
      pragma Import (C, Internal, "gnome_number_entry_gtk_entry");
   begin
      return Widget.Convert (Internal (Get_Object (Nentry)));
   end Gtk_Entry;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number (Nentry : access Gnome_Number_Entry_Record)
                        return Gdouble
   is
      function Internal (Nentry : System.Address)
                         return Gdouble;
      pragma Import (C, Internal, "gnome_number_entry_get_number");
   begin
      return Internal (Get_Object (Nentry));
   end Get_Number;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
     (Nentry            : access Gnome_Number_Entry_Record;
      Calc_Dialog_Title : String)
   is
      procedure Internal
        (Nentry            : System.Address;
         Calc_Dialog_Title : String);
      pragma Import (C, Internal, "gnome_number_entry_set_title");
   begin
      Internal (Get_Object (Nentry),
                Calc_Dialog_Title & ASCII.NUL);
   end Set_Title;

end Gnome.Number_Entry;
