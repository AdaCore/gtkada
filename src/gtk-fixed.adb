-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

package body Gtk.Fixed is

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
     (Fixed : access Gtk_Fixed_Record) return Widget.Widget_List.Glist
   is
      function Internal (Widget : System.Address) return System.Address;
      pragma Import (C, Internal, "ada_fixed_get_children");

      use Gtk.Widget.Widget_List;
      Children : Gtk.Widget.Widget_List.Glist;

   begin
      Set_Object (Children, Internal (Get_Object (Fixed)));
      return Children;
   end Get_Children;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Fixed : out Gtk_Fixed) is
   begin
      Fixed := new Gtk_Fixed_Record;
      Gtk.Fixed.Initialize (Fixed);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Fixed : access Gtk_Fixed_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_fixed_new");

   begin
      Set_Object (Fixed, Internal);
      Initialize_User_Data (Fixed);
   end Initialize;

   ----------
   -- Move --
   ----------

   procedure Move
     (Fixed  : access Gtk_Fixed_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint16;
      Y      : Gint16)
   is
      procedure Internal
        (Fixed  : System.Address;
         Widget : System.Address;
         X      : Gint16;
         Y      : Gint16);
      pragma Import (C, Internal, "gtk_fixed_move");

   begin
      Internal (Get_Object (Fixed), Get_Object (Widget), X, Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
     (Fixed  : access Gtk_Fixed_Record;
      Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      X      : Gint16;
      Y      : Gint16)
   is
      procedure Internal
        (Fixed  : System.Address;
         Widget : System.Address;
         X      : Gint16;
         Y      : Gint16);
      pragma Import (C, Internal, "gtk_fixed_put");

   begin
      Internal (Get_Object (Fixed), Get_Object (Widget), X, Y);
   end Put;

   --------------------
   -- Set_Has_Window --
   --------------------

   procedure Set_Has_Window
     (Fixed      : access Gtk_Fixed_Record;
      Has_Window : Boolean := False)
   is
      procedure Set_Has_Window (Fixed : System.Address; Has_Window : Boolean);

      pragma Import (C, Set_Has_Window, "gtk_fixed_set_has_window");
   begin
      Set_Has_Window (Get_Object (Fixed), Has_Window);
   end Set_Has_Window;

end Gtk.Fixed;
