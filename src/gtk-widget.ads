
--  The widget is the base of the tree for displayable objects.
--  (A displayable object is one which takes up some amount
--  of screen real estate). It provides a common base and interface
--  which actual widgets must adhere to.

package Gtk.Widget is

   --  Flags used by Gtk_Widget on top of Gtk_Object
   Gtk_TopLevel         : constant := 2 ** 4;
   Gtk_No_Window        : constant := 2 ** 5;
   Gtk_Realized         : constant := 2 ** 6;
   Gtk_Mapped           : constant := 2 ** 7;
   Gtk_Visible          : constant := 2 ** 8;
   Gtk_Sensitive        : constant := 2 ** 9;
   Gtk_Parent_Sensitive : constant := 2 ** 10;
   Gtk_Can_Focus        : constant := 2 ** 11;
   Gtk_Has_Focus        : constant := 2 ** 12;
   Gtk_Can_Default      : constant := 2 ** 13;
   Gtk_Has_Default      : constant := 2 ** 14;
   Gtk_Has_Grab         : constant := 2 ** 15;
   Gtk_Basic            : constant := 2 ** 16;
   Gtk_Reserved_3       : constant := 2 ** 17;
   Gtk_Rc_Style         : constant := 2 ** 18;

   type Gtk_Widget is new Gtk_Object with private;

   procedure Gtk_Show (Widget : in Gtk_Widget'Class);

   procedure Gtk_Destroy (Widget : in Gtk_Widget'Class);

private

   type Gtk_Widget is new Gtk_Object with null record;

   --  mapping: Gtk_TopLevel gtkwidget.h GTK_TOPLEVEL
   --  mapping: Gtk_No_Window gtkwidget.h GTK_NOWINDOW
   --  mapping: Gtk_Realized gtkwidget.h GTK_REALIZED
   --  mapping: Gtk_Mapped gtkwidget.h GTK_MAPPED
   --  mapping: Gtk_Visible gtkwidget.h GTK_VISIBLE
   --  mapping: Gtk_Sensitive gtkwidget.h GTK_SENSITIVE
   --  mapping: Gtk_Parent_Sensitive gtkwidget.h GTK_PARENT_SENSITIVE
   --  mapping: Gtk_Can_Focus gtkwidget.h GTK_CAN_FOCUS
   --  mapping: Gtk_Has_Focus gtkwidget.h GTK_HAS_FOCUS
   --  mapping: Gtk_Can_Default gtkwidget.h GTK_CAN_DEFAULT
   --  mapping: Gtk_Has_Default gtkwidget.h GTK_HAS_DEFAULT
   --  mapping: Gtk_Has_Grab gtkwidget.h GTK_HAS_GRAB
   --  mapping: Gtk_Basic gtkwidget.h GTK_BASIC
   --  mapping: Gtk_Reserved_3 gtkwidget.h GTK_RESERVED_3
   --  mapping: Gtk_Rc_Style gtkwidget.h GTK_RC_STYLE
   --  mapping: Gtk_Show gtkwidget.h gtk_widget_show
   --  mapping: Gtk_Destroy gtkwidget.h gtk_widget_destroy

end Gtk.Widget;
