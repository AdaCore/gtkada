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

with Gdk; use Gdk;
with Gtk.Widget;
with Gtk; use Gtk;
with System;

package body Gnome.Proc_Bar is

   use Gdk.Color;

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget : out Gnome_Proc_Bar;
      Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Colors : access Gdk_Color_Array;
      Cb     : Function_Gint) is
   begin
      Widget := new Gnome_Proc_Bar_Record;
      Initialize (Widget, Label, Colors, Cb);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_Proc_Bar_Record'Class;
      Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
      Colors : access Gdk_Color_Array;
      Cb     : Function_Gint)
   is
      function Internal
        (Label  : System.Address;
         N      : Gint;
         Colors : System.Address;
         Cb     : Function_Gint) return System.Address;
      pragma Import (C, Internal, "gnome_proc_bar_new");
   begin
      Set_Object (Widget, Internal
        (Get_Object (Label), Colors'Length, Colors.all'Address, Cb));
   end Initialize;

   ----------------
   -- Set_Orient --
   ----------------

   procedure Set_Orient
     (Pb       : access Gnome_Proc_Bar_Record;
      Vertical : Boolean)
   is
      procedure Internal (Pb : System.Address; Vertical : Gint);
      pragma Import (C, Internal, "gnome_proc_bar_set_orient");
   begin
      Internal (Get_Object (Pb),
                Boolean'Pos (Vertical));
   end Set_Orient;

   ----------------
   -- Set_Values --
   ----------------

   procedure Set_Values
     (Pb  : access Gnome_Proc_Bar_Record;
      Val : out Guint)
   is
      procedure Internal
        (Pb  : System.Address;
         Val : out Guint);
      pragma Import (C, Internal, "gnome_proc_bar_set_values");
   begin
      Internal (Get_Object (Pb),
                Val);
   end Set_Values;

   -----------
   -- Start --
   -----------

   procedure Start
     (Pb    : access Gnome_Proc_Bar_Record;
      Gtime : Gint;
      Data  : System.Address)
   is
      procedure Internal
        (Pb    : System.Address;
         Gtime : Gint;
         Data  : System.Address);
      pragma Import (C, Internal, "gnome_proc_bar_start");
   begin
      Internal (Get_Object (Pb), Gtime, Data);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Pb : access Gnome_Proc_Bar_Record) is
      procedure Internal (Pb : System.Address);
      pragma Import (C, Internal, "gnome_proc_bar_stop");
   begin
      Internal (Get_Object (Pb));
   end Stop;

   ------------
   -- Update --
   ------------

   procedure Update
     (Pb     : access Gnome_Proc_Bar_Record;
      Colors : access Gdk_Color_Array)
   is
      procedure Internal (Pb : System.Address; Colors : System.Address);
      pragma Import (C, Internal, "gnome_proc_bar_update");
   begin
      Internal (Get_Object (Pb), Colors.all'Address);
   end Update;

end Gnome.Proc_Bar;
