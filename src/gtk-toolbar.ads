
with Gtk.Container;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package Gtk.Toolbar is

   type Gtk_Toolbar is new Gtk.Container.Gtk_Container with private;

   function Append_Element
      (Toolbar              : in Gtk_Toolbar'Class;
       The_Type             : in Gtk_Toolbar_Child_Type;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Widget.Gtk_Widget'Class;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   function Append_Item
      (Toolbar              : in Gtk_Toolbar'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Widget.Gtk_Widget'Class;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   procedure Append_Space (Toolbar : in Gtk_Toolbar'Class);
   procedure Append_Widget
      (Toolbar              : in Gtk_Toolbar'Class;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String);
   procedure Gtk_New
      (Widget      : out Gtk_Toolbar;
       Orientation : in Gtk_Orientation;
       Style       : in Gtk_Toolbar_Style);
   function Insert_Element
      (Toolbar              : in Gtk_Toolbar'Class;
       The_Type             : in Gtk_Toolbar_Child_Type;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class;
       Position             : in Gint)
       return                    Gtk.Widget.Gtk_Widget'Class;
   --  Note : you have to set the callback yourself, as opposed to what is
   --  done in C.

   function Insert_Item
      (Toolbar              : in Gtk_Toolbar'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class;
       Position             : in Gint)
       return                    Gtk.Widget.Gtk_Widget'Class;
   procedure Insert_Space
      (Toolbar  : in Gtk_Toolbar'Class;
       Position : in Gint);
   procedure Insert_Widget
      (Toolbar              : in Gtk_Toolbar'Class;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Position             : in Gint);
   function Prepend_Element
      (Toolbar              : in Gtk_Toolbar'Class;
       The_Type             : in Gtk_Toolbar_Child_Type;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Widget.Gtk_Widget'Class;
   function Prepend_Item
      (Toolbar              : in Gtk_Toolbar'Class;
       Text                 : in String;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String;
       Icon                 : in Gtk.Widget.Gtk_Widget'Class)
       return                    Gtk.Widget.Gtk_Widget'Class;
   procedure Prepend_Space (Toolbar : in Gtk_Toolbar'Class);
   procedure Prepend_Widget
      (Toolbar              : in Gtk_Toolbar'Class;
       Widget               : in Gtk.Widget.Gtk_Widget'Class;
       Tooltip_Text         : in String;
       Tooltip_Private_Text : in String);
   procedure Set_Orientation
      (Toolbar     : in Gtk_Toolbar'Class;
       Orientation : in Gtk_Orientation);
   procedure Set_Space_Size
      (Toolbar    : in Gtk_Toolbar'Class;
       Space_Size : in Gint);
   procedure Set_Style
      (Toolbar : in Gtk_Toolbar'Class;
       Style   : in Gtk_Toolbar_Style);
   procedure Set_Tooltips
      (Toolbar : in Gtk_Toolbar'Class;
       Enable  : in Gint);

private
   type Gtk_Toolbar is new Gtk.Container.Gtk_Container with null record;

   --  mapping: Append_Element gtktoolbar.h gtk_toolbar_append_element
   --  mapping: Append_Item gtktoolbar.h gtk_toolbar_append_item
   --  mapping: Append_Space gtktoolbar.h gtk_toolbar_append_space
   --  mapping: Append_Widget gtktoolbar.h gtk_toolbar_append_widget
   --  mapping: NOT_IMPLEMENTED gtktoolbar.h gtk_toolbar_get_type
   --  mapping: Gtk_New gtktoolbar.h gtk_toolbar_new
   --  mapping: Insert_Element gtktoolbar.h gtk_toolbar_insert_element
   --  mapping: Insert_Item gtktoolbar.h gtk_toolbar_insert_item
   --  mapping: Insert_Space gtktoolbar.h gtk_toolbar_insert_space
   --  mapping: Insert_Widget gtktoolbar.h gtk_toolbar_insert_widget
   --  mapping: Prepend_Element gtktoolbar.h gtk_toolbar_prepend_element
   --  mapping: Prepend_Item gtktoolbar.h gtk_toolbar_prepend_item
   --  mapping: Prepend_Space gtktoolbar.h gtk_toolbar_prepend_space
   --  mapping: Prepend_Widget gtktoolbar.h gtk_toolbar_prepend_widget
   --  mapping: Set_Orientation gtktoolbar.h gtk_toolbar_set_orientation
   --  mapping: Set_Space_Size gtktoolbar.h gtk_toolbar_set_space_size
   --  mapping: Set_Style gtktoolbar.h gtk_toolbar_set_style
   --  mapping: Set_Tooltips gtktoolbar.h gtk_toolbar_set_tooltips
end Gtk.Toolbar;
