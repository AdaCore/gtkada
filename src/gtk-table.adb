-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;

package body Gtk.Table is

   ------------
   -- Attach --
   ------------

   procedure Attach
      (Table         : in Gtk_Table'Class;
       Child         : in Gtk.Widget.Gtk_Widget'Class;
       Left_Attach   : in Gint;
       Right_Attach  : in Gint;
       Top_Attach    : in Gint;
       Bottom_Attach : in Gint;
       Xoptions      : in Gtk_Attach_Options;
       Yoptions      : in Gtk_Attach_Options;
       Xpadding      : in Gint;
       Ypadding      : in Gint)
   is
      procedure Internal
         (Table         : in System.Address;
          Child         : in System.Address;
          Left_Attach   : in Gint;
          Right_Attach  : in Gint;
          Top_Attach    : in Gint;
          Bottom_Attach : in Gint;
          Xoptions      : in Gint;
          Yoptions      : in Gint;
          Xpadding      : in Gint;
          Ypadding      : in Gint);
      pragma Import (C, Internal, "gtk_table_attach");
   begin
      Internal (Get_Object (Table),
                Get_Object (Child),
                Left_Attach,
                Right_Attach,
                Top_Attach,
                Bottom_Attach,
                Gint (Xoptions),
                Gint (Yoptions),
                Xpadding,
                Ypadding);
   end Attach;

   ---------------------
   -- Attach_Defaults --
   ---------------------

   procedure Attach_Defaults
      (Table         : in Gtk_Table'Class;
       Widget        : in Gtk.Widget.Gtk_Widget'Class;
       Left_Attach   : in Gint;
       Right_Attach  : in Gint;
       Top_Attach    : in Gint;
       Bottom_Attach : in Gint)
   is
      procedure Internal
         (Table         : in System.Address;
          Widget        : in System.Address;
          Left_Attach   : in Gint;
          Right_Attach  : in Gint;
          Top_Attach    : in Gint;
          Bottom_Attach : in Gint);
      pragma Import (C, Internal, "gtk_table_attach_defaults");
   begin
      Internal (Get_Object (Table),
                Get_Object (Widget),
                Left_Attach,
                Right_Attach,
                Top_Attach,
                Bottom_Attach);
   end Attach_Defaults;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget      : out Gtk_Table;
       Rows        : in Gint;
       Columns     : in Gint;
       Homogeneous : in Boolean)
   is
      function Internal
         (Rows        : in Gint;
          Columns     : in Gint;
          Homogeneous : in Gint)
          return           System.Address;
      pragma Import (C, Internal, "gtk_table_new");
   begin
      Set_Object (Widget, Internal (Rows,
                                    Columns,
                                    Boolean'Pos (Homogeneous)));
   end Gtk_New;

   ---------------------
   -- Set_Col_Spacing --
   ---------------------

   procedure Set_Col_Spacing
      (Table   : in Gtk_Table'Class;
       Column  : in Gint;
       Spacing : in Gint)
   is
      procedure Internal
         (Table   : in System.Address;
          Column  : in Gint;
          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_table_set_col_spacing");
   begin
      Internal (Get_Object (Table),
                Column,
                Spacing);
   end Set_Col_Spacing;

   ----------------------
   -- Set_Col_Spacings --
   ----------------------

   procedure Set_Col_Spacings
      (Table   : in Gtk_Table'Class;
       Spacing : in Gint)
   is
      procedure Internal
         (Table   : in System.Address;
          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_table_set_col_spacings");
   begin
      Internal (Get_Object (Table),
                Spacing);
   end Set_Col_Spacings;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
      (Table       : in Gtk_Table'Class;
       Homogeneous : in Boolean)
   is
      procedure Internal
         (Table       : in System.Address;
          Homogeneous : in Gint);
      pragma Import (C, Internal, "gtk_table_set_homogeneous");
   begin
      Internal (Get_Object (Table),
                Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
      (Table   : in Gtk_Table'Class;
       Row     : in Gint;
       Spacing : in Gint)
   is
      procedure Internal
         (Table   : in System.Address;
          Row     : in Gint;
          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_table_set_row_spacing");
   begin
      Internal (Get_Object (Table),
                Row,
                Spacing);
   end Set_Row_Spacing;

   ----------------------
   -- Set_Row_Spacings --
   ----------------------

   procedure Set_Row_Spacings
      (Table   : in Gtk_Table'Class;
       Spacing : in Gint)
   is
      procedure Internal
         (Table   : in System.Address;
          Spacing : in Gint);
      pragma Import (C, Internal, "gtk_table_set_row_spacings");
   begin
      Internal (Get_Object (Table),
                Spacing);
   end Set_Row_Spacings;

end Gtk.Table;
