--  The widget is the base of the tree for displayable objects.
--  (A displayable object is one which takes up some amount
--  of screen real estate). It provides a common base and interface
--  which actual widgets must adhere to.

package Gtk.Widget is

   --  Flags used by Widget on top of Object
   TopLevel         : constant := 2 ** 4;
   No_Window        : constant := 2 ** 5;
   Realized         : constant := 2 ** 6;
   Mapped           : constant := 2 ** 7;
   Visible          : constant := 2 ** 8;
   Sensitive        : constant := 2 ** 9;
   Parent_Sensitive : constant := 2 ** 10;
   Can_Focus        : constant := 2 ** 11;
   Has_Focus        : constant := 2 ** 12;
   Can_Default      : constant := 2 ** 13;
   Has_Default      : constant := 2 ** 14;
   Has_Grab         : constant := 2 ** 15;
   Basic            : constant := 2 ** 16;
   Reserved_3       : constant := 2 ** 17;
   Rc_Style         : constant := 2 ** 18;

   type Gtk_Widget is new Object with private;

   procedure Activate (Widget : in Gtk_Widget'Class);
   --  mapping: Activate gtkwidget.h gtk_widget_activate

   procedure Destroy (Widget : in Gtk_Widget'Class);
   --  mapping: Destroy gtkwidget.h gtk_widget_destroy

   procedure Hide (Widget : in Gtk_Widget'Class);
   --  mapping: Hide gtkwidget.h gtk_widget_hide

   procedure Set_Sensitive (Widget    : in Gtk_Widget'Class;
                            Sensitive : in Boolean);
   --  mapping: Set_Sensitive gtkwidget.h gtk_widget_set_sensitive

   procedure Show (Widget : in Gtk_Widget'Class);
   --  mapping: Show gtkwidget.h gtk_widget_show

private

   type Gtk_Widget is new Object with null record;

   --  mapping: TopLevel gtkwidget.h GTK_TOPLEVEL
   --  mapping: No_Window gtkwidget.h GTK_NOWINDOW
   --  mapping: Realized gtkwidget.h GTK_REALIZED
   --  mapping: Mapped gtkwidget.h GTK_MAPPED
   --  mapping: Visible gtkwidget.h GTK_VISIBLE
   --  mapping: Sensitive gtkwidget.h GTK_SENSITIVE
   --  mapping: Parent_Sensitive gtkwidget.h GTK_PARENT_SENSITIVE
   --  mapping: Can_Focus gtkwidget.h GTK_CAN_FOCUS
   --  mapping: Has_Focus gtkwidget.h GTK_HAS_FOCUS
   --  mapping: Can_Default gtkwidget.h GTK_CAN_DEFAULT
   --  mapping: Has_Default gtkwidget.h GTK_HAS_DEFAULT
   --  mapping: Has_Grab gtkwidget.h GTK_HAS_GRAB
   --  mapping: Basic gtkwidget.h GTK_BASIC
   --  mapping: Reserved_3 gtkwidget.h GTK_RESERVED_3
   --  mapping: Rc_Style gtkwidget.h GTK_RC_STYLE


   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_basic
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_destroyed
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_data_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_drag_add
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_drag_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_dnd_drop_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_draw
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_draw_children
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_draw_default
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_draw_focus
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_ensure_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_event
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_ancestor
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_default_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_default_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_default_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_events
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_extension_events
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_name
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_pointer
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_toplevel
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_type
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_get_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_getv
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_grab_default
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_grab_focus
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_hide_all
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_hide_on_delete
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_install_accelerator
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_intersect
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_is_ancestor
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_is_child
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_map
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_new
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_newv
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_pop_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_pop_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_pop_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_popup
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_push_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_push_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_push_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_queue_draw
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_queue_resize
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_realize
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_ref
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_remove_accelerator
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_reparent
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_reset_rc_styles
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_restore_default_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_default_colormap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_default_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_default_visual
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_events
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_extension_events
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_name
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_parent
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_parent_window
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_rc_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_state
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_style
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_uposition
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_set_usize
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_setv
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_shape_combine_mask
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_show_all
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_show_now
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_size_allocate
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_size_request
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_unmap
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_unparent
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_unrealize
   --  mapping: NOT_IMPLEMENTED gtkwidget.h gtk_widget_unref

   --  mapping: INTERNAL gtkwidget.h gtk_widget_propagate_default_style

end Gtk.Widget;
