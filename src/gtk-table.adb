-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

package body Gtk.Table is

   use Gtk.Container;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (Table         : access Gtk_Table_Record;
      Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
      Left_Attach   : Guint;
      Right_Attach  : Guint;
      Top_Attach    : Guint;
      Bottom_Attach : Guint;
      Xoptions      : Gtk_Attach_Options := Expand or Fill;
      Yoptions      : Gtk_Attach_Options := Expand or Fill;
      Xpadding      : Guint := 0;
      Ypadding      : Guint := 0)
   is
      procedure Internal
        (Table         : System.Address;
         Child         : System.Address;
         Left_Attach   : Guint;
         Right_Attach  : Guint;
         Top_Attach    : Guint;
         Bottom_Attach : Guint;
         Xoptions      : Gint;
         Yoptions      : Gint;
         Xpadding      : Guint;
         Ypadding      : Guint);
      pragma Import (C, Internal, "gtk_table_attach");

   begin
      Internal
        (Get_Object (Table), Get_Object (Child), Left_Attach, Right_Attach,
         Top_Attach, Bottom_Attach, Gint (Xoptions), Gint (Yoptions),
         Xpadding, Ypadding);
   end Attach;

   ---------------------
   -- Attach_Defaults --
   ---------------------

   procedure Attach_Defaults
     (Table         : access Gtk_Table_Record;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Left_Attach   : Guint;
      Right_Attach  : Guint;
      Top_Attach    : Guint;
      Bottom_Attach : Guint)
   is
      procedure Internal
        (Table         : System.Address;
         Widget        : System.Address;
         Left_Attach   : Guint;
         Right_Attach  : Guint;
         Top_Attach    : Guint;
         Bottom_Attach : Guint);
      pragma Import (C, Internal, "gtk_table_attach_defaults");

   begin
      Internal
        (Get_Object (Table), Get_Object (Widget), Left_Attach, Right_Attach,
         Top_Attach, Bottom_Attach);
   end Attach_Defaults;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
     (Table : access Gtk_Table_Record;
      Row   : Guint) return Guint
   is
      function Internal
        (Table : System.Address;
         Row   : Guint) return Guint;
      pragma Import (C, Internal, "gtk_table_get_row_spacing");

   begin
      return Internal (Get_Object (Table), Row);
   end Get_Row_Spacing;

   ---------------------
   -- Get_Col_Spacing --
   ---------------------

   function Get_Col_Spacing
     (Table  : access Gtk_Table_Record;
      Column : Guint) return Guint
   is
      function Internal
        (Table   : System.Address;
         Column  : Guint) return Guint;
      pragma Import (C, Internal, "gtk_table_get_col_spacing");

   begin
      return Internal (Get_Object (Table), Column);
   end Get_Col_Spacing;

   -----------------------------
   -- Get_Default_Row_Spacing --
   -----------------------------

   function Get_Default_Row_Spacing
     (Table : access Gtk_Table_Record) return Guint
   is
      function Internal (Table : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_table_get_default_row_spacing");

   begin
      return Internal (Get_Object (Table));
   end Get_Default_Row_Spacing;

   -----------------------------
   -- Get_Default_Col_Spacing --
   -----------------------------

   function Get_Default_Col_Spacing
     (Table : access Gtk_Table_Record) return Guint
   is
      function Internal (Table : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_table_get_default_col_spacing");

   begin
      return Internal (Get_Object (Table));
   end Get_Default_Col_Spacing;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous
     (Table : access Gtk_Table_Record) return Boolean
   is
      function Internal (Table : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_table_get_homogeneous");

   begin
      return Internal (Get_Object (Table)) /= 0;
   end Get_Homogeneous;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget      : out Gtk_Table;
      Rows        : Guint;
      Columns     : Guint;
      Homogeneous : Boolean) is
   begin
      Widget := new Gtk_Table_Record;
      Initialize (Widget, Rows, Columns, Homogeneous);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget      : access Gtk_Table_Record'Class;
      Rows        : Guint;
      Columns     : Guint;
      Homogeneous : Boolean)
   is
      function Internal
        (Rows        : Guint;
         Columns     : Guint;
         Homogeneous : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_table_new");

   begin
      Set_Object (Widget, Internal (Rows, Columns, Boolean'Pos (Homogeneous)));
   end Initialize;

   ------------
   -- Resize --
   ------------

   procedure Resize
     (Table   : access Gtk_Table_Record;
      Rows    : Guint;
      Columns : Guint)
   is
      procedure Internal
        (Table   : System.Address;
         Column  : Guint;
         Spacing : Guint);
      pragma Import (C, Internal, "gtk_table_resize");

   begin
      Internal (Get_Object (Table), Rows, Columns);
   end Resize;

   ---------------------
   -- Set_Col_Spacing --
   ---------------------

   procedure Set_Col_Spacing
     (Table   : access Gtk_Table_Record;
      Column  : Guint;
      Spacing : Guint)
   is
      procedure Internal
        (Table   : System.Address;
         Column  : Guint;
         Spacing : Guint);
      pragma Import (C, Internal, "gtk_table_set_col_spacing");

   begin
      Internal (Get_Object (Table), Column, Spacing);
   end Set_Col_Spacing;

   ----------------------
   -- Set_Col_Spacings --
   ----------------------

   procedure Set_Col_Spacings
     (Table : access Gtk_Table_Record; Spacing : Guint)
   is
      procedure Internal (Table : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_table_set_col_spacings");

   begin
      Internal (Get_Object (Table), Spacing);
   end Set_Col_Spacings;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
     (Table       : access Gtk_Table_Record;
      Homogeneous : Boolean)
   is
      procedure Internal (Table : System.Address; Homogeneous : Gint);
      pragma Import (C, Internal, "gtk_table_set_homogeneous");

   begin
      Internal (Get_Object (Table), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
     (Table   : access Gtk_Table_Record;
      Row     : Guint;
      Spacing : Guint)
   is
      procedure Internal
        (Table   : System.Address;
         Row     : Guint;
         Spacing : Guint);
      pragma Import (C, Internal, "gtk_table_set_row_spacing");

   begin
      Internal (Get_Object (Table), Row, Spacing);
   end Set_Row_Spacing;

   ----------------------
   -- Set_Row_Spacings --
   ----------------------

   procedure Set_Row_Spacings
     (Table   : access Gtk_Table_Record;
      Spacing : Guint)
   is
      procedure Internal (Table : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_table_set_row_spacings");

   begin
      Internal (Get_Object (Table), Spacing);
   end Set_Row_Spacings;

end Gtk.Table;
