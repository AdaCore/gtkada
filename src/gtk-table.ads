
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Table is

   type Gtk_Table is new Gtk.Container.Gtk_Container with private;

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
       Ypadding      : in Gint);
   procedure Attach_Defaults
      (Table         : in Gtk_Table'Class;
       Widget        : in Gtk.Widget.Gtk_Widget'Class;
       Left_Attach   : in Gint;
       Right_Attach  : in Gint;
       Top_Attach    : in Gint;
       Bottom_Attach : in Gint);
   procedure Gtk_New
      (Widget      : out Gtk_Table;
       Rows        : in Gint;
       Columns     : in Gint;
       Homogeneous : in Boolean);
   procedure Set_Col_Spacing
      (Table   : in Gtk_Table'Class;
       Column  : in Gint;
       Spacing : in Gint);
   procedure Set_Col_Spacings
      (Table   : in Gtk_Table'Class;
       Spacing : in Gint);
   procedure Set_Homogeneous
      (Table       : in Gtk_Table'Class;
       Homogeneous : in Boolean);
   procedure Set_Row_Spacing
      (Table   : in Gtk_Table'Class;
       Row     : in Gint;
       Spacing : in Gint);
   procedure Set_Row_Spacings
      (Table   : in Gtk_Table'Class;
       Spacing : in Gint);

private
   type Gtk_Table is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Attach gtktable.h gtk_table_attach
   --  mapping: Attach_Defaults gtktable.h gtk_table_attach_defaults
   --  mapping: NOT_IMPLEMENTED gtktable.h gtk_table_get_type
   --  mapping: Gtk_New gtktable.h gtk_table_new
   --  mapping: Set_Col_Spacing gtktable.h gtk_table_set_col_spacing
   --  mapping: Set_Col_Spacings gtktable.h gtk_table_set_col_spacings
   --  mapping: Set_Homogeneous gtktable.h gtk_table_set_homogeneous
   --  mapping: Set_Row_Spacing gtktable.h gtk_table_set_row_spacing
   --  mapping: Set_Row_Spacings gtktable.h gtk_table_set_row_spacings
end Gtk.Table;
