-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
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

package body Gtk.Table is

   use Gtk.Container;

   ------------
   -- Attach --
   ------------

   procedure Attach (Table         : access Gtk_Table_Record;
                     Child         : access Gtk.Widget.Gtk_Widget_Record'Class;
                     Left_Attach   : in Guint;
                     Right_Attach  : in Guint;
                     Top_Attach    : in Guint;
                     Bottom_Attach : in Guint;
                     Xoptions      : in Gtk.Enums.Gtk_Attach_Options;
                     Yoptions      : in Gtk.Enums.Gtk_Attach_Options;
                     Xpadding      : in Guint;
                     Ypadding      : in Guint)
   is
      procedure Internal (Table         : in System.Address;
                          Child         : in System.Address;
                          Left_Attach   : in Guint;
                          Right_Attach  : in Guint;
                          Top_Attach    : in Guint;
                          Bottom_Attach : in Guint;
                          Xoptions      : in Gint;
                          Yoptions      : in Gint;
                          Xpadding      : in Guint;
                          Ypadding      : in Guint);
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
      Left_Attach   : in Guint;
      Right_Attach  : in Guint;
      Top_Attach    : in Guint;
      Bottom_Attach : in Guint)
   is
      procedure Internal
        (Table         : in System.Address;
         Widget        : in System.Address;
         Left_Attach   : in Guint;
         Right_Attach  : in Guint;
         Top_Attach    : in Guint;
         Bottom_Attach : in Guint);
      pragma Import (C, Internal, "gtk_table_attach_defaults");
   begin
      Internal
        (Get_Object (Table), Get_Object (Widget), Left_Attach, Right_Attach,
         Top_Attach, Bottom_Attach);
   end Attach_Defaults;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget      : out Gtk_Table;
                      Rows        : in Guint;
                      Columns     : in Guint;
                      Homogeneous : in Boolean) is
   begin
      Widget := new Gtk_Table_Record;
      Initialize (Widget, Rows, Columns, Homogeneous);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget      : access Gtk_Table_Record'Class;
                         Rows        : in Guint;
                         Columns     : in Guint;
                         Homogeneous : in Boolean)
   is
      function Internal (Rows        : in Guint;
                         Columns     : in Guint;
                         Homogeneous : in Gint)
         return System.Address;
      pragma Import (C, Internal, "gtk_table_new");
   begin
      Set_Object (Widget, Internal (Rows, Columns, Boolean'Pos (Homogeneous)));
      Initialize_User_Data (Widget);
   end Initialize;

   ------------
   -- Resize --
   ------------

   procedure Resize (Table      : access Gtk_Table_Record;
                     Rows        : in Guint;
                     Columns     : in Guint)
   is
      procedure Internal (Table   : in System.Address;
                          Column  : in Guint;
                          Spacing : in Guint);
      pragma Import (C, Internal, "gtk_table_resize");
   begin
      Internal (Get_Object (Table), Rows, Columns);
   end Resize;

   ---------------------
   -- Set_Col_Spacing --
   ---------------------

   procedure Set_Col_Spacing (Table   : access Gtk_Table_Record;
                              Column  : in Guint;
                              Spacing : in Guint)
   is
      procedure Internal (Table   : in System.Address;
                          Column  : in Guint;
                          Spacing : in Guint);
      pragma Import (C, Internal, "gtk_table_set_col_spacing");
   begin
      Internal (Get_Object (Table), Column, Spacing);
   end Set_Col_Spacing;

   ----------------------
   -- Set_Col_Spacings --
   ----------------------

   procedure Set_Col_Spacings (Table   : access Gtk_Table_Record;
                               Spacing : in Guint)
   is
      procedure Internal (Table   : in System.Address;
                          Spacing : in Guint);
      pragma Import (C, Internal, "gtk_table_set_col_spacings");
   begin
      Internal (Get_Object (Table), Spacing);
   end Set_Col_Spacings;

   ---------------------
   -- Set_Homogeneous --
   ---------------------

   procedure Set_Homogeneous
     (Table       : access Gtk_Table_Record;
      Homogeneous : in Boolean)
   is
      procedure Internal
        (Table       : in System.Address;
         Homogeneous : in Gint);
      pragma Import (C, Internal, "gtk_table_set_homogeneous");
   begin
      Internal (Get_Object (Table), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing (Table   : access Gtk_Table_Record;
                              Row     : in Guint;
                              Spacing : in Guint)
   is
      procedure Internal (Table   : in System.Address;
                          Row     : in Guint;
                          Spacing : in Guint);
      pragma Import (C, Internal, "gtk_table_set_row_spacing");
   begin
      Internal (Get_Object (Table), Row, Spacing);
   end Set_Row_Spacing;

   ----------------------
   -- Set_Row_Spacings --
   ----------------------

   procedure Set_Row_Spacings (Table   : access Gtk_Table_Record;
                               Spacing : in Guint)
   is
      procedure Internal (Table   : in System.Address;
                          Spacing : in Guint);
      pragma Import (C, Internal, "gtk_table_set_row_spacings");
   begin
      Internal (Get_Object (Table), Spacing);
   end Set_Row_Spacings;

   --------------
   -- Generate --
   --------------

   procedure Generate (N      : in Node_Ptr;
                       File   : in File_Type) is
      P   : Node_Ptr;
      Top : constant String_Ptr := Get_Field (Find_Top_Widget (N), "name");

   begin
      if not N.Specific_Data.Created then
         P := Find_Tag (N.Child, "name");

         if P /= null then
            Add_Package ("Table");
            Put (File, "   Gtk_New (" & To_Ada (Top.all) & "." &
              To_Ada (P.Value.all) & ", " &
              To_Ada (Get_Field (N, "rows").all));
            Put (File, ", " & To_Ada (Get_Field (N, "columns").all));
            Put_Line
              (File, ", " & To_Ada (Get_Field (N, "homogeneous").all) & ");");
            N.Specific_Data.Created := True;
         end if;
      end if;

      Container.Generate (N, File);
      Gen_Set (N, "Table", "Row_Spacings", "row_spacing", File);
      Gen_Set (N, "Table", "Col_Spacings", "column_spacing", File);
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (Table : in out Gtk.Object.Gtk_Object;
                       N     : in Node_Ptr) is
      S : String_Ptr;
   begin
      if not N.Specific_Data.Created then
         S := Get_Field (N, "type");
         Gtk_New
           (Gtk_Table (Table), Guint'Value (Get_Field (N, "rows").all),
            Guint'Value (Get_Field (N, "columns").all),
            Boolean'Value (Get_Field (N, "homogeneous").all));
         Set_Object (Get_Field (N, "name"), Table);
         N.Specific_Data.Created := True;
      end if;

      Container.Generate (Table, N);

      S := Get_Field (N, "row_spacing");

      if S /= null then
         Set_Row_Spacings (Gtk_Table (Table), Guint'Value (S.all));
      end if;

      S := Get_Field (N, "column_spacing");

      if S /= null then
         Set_Col_Spacings (Gtk_Table (Table), Guint'Value (S.all));
      end if;
   end Generate;

end Gtk.Table;
