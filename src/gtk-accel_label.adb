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

package body Gtk.Accel_Label is

   ---------------------
   -- Get_Accel_Width --
   ---------------------

   function Get_Accel_Width (Accel_Label : access Gtk_Accel_Label_Record)
                             return               Guint
   is
      function Internal (Accel_Label : in System.Address) return Guint;
      pragma Import (C, Internal, "gtk_accel_label_get_accel_width");
   begin
      return Internal (Get_Object (Accel_Label));
   end Get_Accel_Width;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Accel_Label;
                      Str    : in  String) is
   begin
      Widget := new Gtk_Accel_Label_Record;
      Initialize (Widget, Str);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Accel_Label_Record'Class;
                         Str    : in     String) is
      function Internal (Str : in String) return System.Address;
      pragma Import (C, Internal, "gtk_accel_label_new");
   begin
      Set_Object (Widget, Internal (Str & Ascii.NUL));
      Initialize_User_Data (Widget);
   end Initialize;

   -------------
   -- Refetch --
   -------------

   function Refetch (Accel_Label : access Gtk_Accel_Label_Record)
                     return               Boolean
   is
      function Internal (Accel_Label : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_accel_label_refetch");
   begin
      return Boolean'Val (Internal (Get_Object (Accel_Label)));
   end Refetch;

   ----------------------
   -- Set_Accel_Widget --
   ----------------------

   procedure Set_Accel_Widget
     (Accel_Label  : access Gtk_Accel_Label_Record;
      Accel_Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Accel_Label  : in System.Address;
                          Accel_Widget : in System.Address);
      pragma Import (C, Internal, "gtk_accel_label_set_accel_widget");
   begin
      Internal (Get_Object (Accel_Label), Get_Object (Accel_Widget));
   end Set_Accel_Widget;

end Gtk.Accel_Label;
