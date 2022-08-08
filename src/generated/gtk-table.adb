------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Table is

   package Type_Conversion_Gtk_Table is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Table_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Table);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Table       : out Gtk_Table;
       Rows        : Guint;
       Columns     : Guint;
       Homogeneous : Boolean)
   is
   begin
      Table := new Gtk_Table_Record;
      Gtk.Table.Initialize (Table, Rows, Columns, Homogeneous);
   end Gtk_New;

   -------------------
   -- Gtk_Table_New --
   -------------------

   function Gtk_Table_New
      (Rows        : Guint;
       Columns     : Guint;
       Homogeneous : Boolean) return Gtk_Table
   is
      Table : constant Gtk_Table := new Gtk_Table_Record;
   begin
      Gtk.Table.Initialize (Table, Rows, Columns, Homogeneous);
      return Table;
   end Gtk_Table_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Table       : not null access Gtk_Table_Record'Class;
       Rows        : Guint;
       Columns     : Guint;
       Homogeneous : Boolean)
   is
      function Internal
         (Rows        : Guint;
          Columns     : Guint;
          Homogeneous : Glib.Gboolean) return System.Address;
      pragma Import (C, Internal, "gtk_table_new");
   begin
      if not Table.Is_Created then
         Set_Object (Table, Internal (Rows, Columns, Boolean'Pos (Homogeneous)));
      end if;
   end Initialize;

   ------------
   -- Attach --
   ------------

   procedure Attach
      (Table         : not null access Gtk_Table_Record;
       Child         : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Left_Attach   : Guint;
       Right_Attach  : Guint;
       Top_Attach    : Guint;
       Bottom_Attach : Guint;
       Xoptions      : Gtk.Enums.Gtk_Attach_Options := Expand or Fill;
       Yoptions      : Gtk.Enums.Gtk_Attach_Options := Expand or Fill;
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
          Xoptions      : Gtk.Enums.Gtk_Attach_Options;
          Yoptions      : Gtk.Enums.Gtk_Attach_Options;
          Xpadding      : Guint;
          Ypadding      : Guint);
      pragma Import (C, Internal, "gtk_table_attach");
   begin
      Internal (Get_Object (Table), Get_Object (Child), Left_Attach, Right_Attach, Top_Attach, Bottom_Attach, Xoptions, Yoptions, Xpadding, Ypadding);
   end Attach;

   ---------------------
   -- Attach_Defaults --
   ---------------------

   procedure Attach_Defaults
      (Table         : not null access Gtk_Table_Record;
       Widget        : not null access Gtk.Widget.Gtk_Widget_Record'Class;
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
      Internal (Get_Object (Table), Get_Object (Widget), Left_Attach, Right_Attach, Top_Attach, Bottom_Attach);
   end Attach_Defaults;

   ---------------------
   -- Get_Col_Spacing --
   ---------------------

   function Get_Col_Spacing
      (Table  : not null access Gtk_Table_Record;
       Column : Guint) return Guint
   is
      function Internal
         (Table  : System.Address;
          Column : Guint) return Guint;
      pragma Import (C, Internal, "gtk_table_get_col_spacing");
   begin
      return Internal (Get_Object (Table), Column);
   end Get_Col_Spacing;

   -----------------------------
   -- Get_Default_Col_Spacing --
   -----------------------------

   function Get_Default_Col_Spacing
      (Table : not null access Gtk_Table_Record) return Guint
   is
      function Internal (Table : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_table_get_default_col_spacing");
   begin
      return Internal (Get_Object (Table));
   end Get_Default_Col_Spacing;

   -----------------------------
   -- Get_Default_Row_Spacing --
   -----------------------------

   function Get_Default_Row_Spacing
      (Table : not null access Gtk_Table_Record) return Guint
   is
      function Internal (Table : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_table_get_default_row_spacing");
   begin
      return Internal (Get_Object (Table));
   end Get_Default_Row_Spacing;

   ---------------------
   -- Get_Homogeneous --
   ---------------------

   function Get_Homogeneous
      (Table : not null access Gtk_Table_Record) return Boolean
   is
      function Internal (Table : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_table_get_homogeneous");
   begin
      return Internal (Get_Object (Table)) /= 0;
   end Get_Homogeneous;

   ---------------------
   -- Get_Row_Spacing --
   ---------------------

   function Get_Row_Spacing
      (Table : not null access Gtk_Table_Record;
       Row   : Guint) return Guint
   is
      function Internal (Table : System.Address; Row : Guint) return Guint;
      pragma Import (C, Internal, "gtk_table_get_row_spacing");
   begin
      return Internal (Get_Object (Table), Row);
   end Get_Row_Spacing;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
      (Table   : not null access Gtk_Table_Record;
       Rows    : out Guint;
       Columns : out Guint)
   is
      procedure Internal
         (Table   : System.Address;
          Rows    : out Guint;
          Columns : out Guint);
      pragma Import (C, Internal, "gtk_table_get_size");
   begin
      Internal (Get_Object (Table), Rows, Columns);
   end Get_Size;

   ------------
   -- Resize --
   ------------

   procedure Resize
      (Table   : not null access Gtk_Table_Record;
       Rows    : Guint;
       Columns : Guint)
   is
      procedure Internal
         (Table   : System.Address;
          Rows    : Guint;
          Columns : Guint);
      pragma Import (C, Internal, "gtk_table_resize");
   begin
      Internal (Get_Object (Table), Rows, Columns);
   end Resize;

   ---------------------
   -- Set_Col_Spacing --
   ---------------------

   procedure Set_Col_Spacing
      (Table   : not null access Gtk_Table_Record;
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
      (Table   : not null access Gtk_Table_Record;
       Spacing : Guint)
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
      (Table       : not null access Gtk_Table_Record;
       Homogeneous : Boolean)
   is
      procedure Internal
         (Table       : System.Address;
          Homogeneous : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_table_set_homogeneous");
   begin
      Internal (Get_Object (Table), Boolean'Pos (Homogeneous));
   end Set_Homogeneous;

   ---------------------
   -- Set_Row_Spacing --
   ---------------------

   procedure Set_Row_Spacing
      (Table   : not null access Gtk_Table_Record;
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
      (Table   : not null access Gtk_Table_Record;
       Spacing : Guint)
   is
      procedure Internal (Table : System.Address; Spacing : Guint);
      pragma Import (C, Internal, "gtk_table_set_row_spacings");
   begin
      Internal (Get_Object (Table), Spacing);
   end Set_Row_Spacings;

end Gtk.Table;
