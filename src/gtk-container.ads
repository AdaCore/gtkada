with Gtk.Adjustment;
with Gtk.Signal;
with Gtk.Widget;

package Gtk.Container is

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with private;

   procedure Add (Container : in out Gtk_Container'Class;
                  Widget       : in Gtk.Widget.Gtk_Widget'Class);
   --  mapping: Add  gtkcontainer.h gtk_container_add

   procedure Border_Width (Container : in Gtk_Container'Class;
                           Border_Width : in Gint);
   --  mapping: Border_Width gtkcontainer.h gtk_container_border_width

   procedure Remove (Container : in out Gtk_Container'Class;
                     Widget : in Gtk.Widget.Gtk_Widget'Class);
   --  mapping: Remove gtkcontainer.h gtk_container_remove

   procedure Disable_Resize (Container : in out Gtk_Container'Class);
   --  mapping: Disable_Resize gtkcontainer.h gtk_container_disable_resize

   procedure Enable_Resize (Container : in out Gtk_Container'Class);
   --  mapping: Enable_Resize gtkcontainer.h gtk_container_enable_resize

   procedure Block_Resize (Container : in out Gtk_Container'Class);
   --  mapping: Block_Resize gtkcontainer.h gtk_container_block_resize

   procedure Unblock_Resize (Container : in out Gtk_Container'Class);
   --  mapping: Unblock_Resize gtkcontainer.h gtk_container_unblock_resize

   function Need_Resize (Container : in Gtk_Container'Class) return Boolean;
   --  mapping: Need_Resize gtkcontainer.h gtk_container_need_resize

   procedure Set_Focus_Hadjustment
     (Container  : in out Gtk_Container'Class;
      Adjustment : in     Gtk.Adjustment.Gtk_Adjustment'Class);
   --  mapping: Set_Focus_Hadjustment gtkcontainer.h \
   --  mapping:                       gtk_container_set_focus_hadjustment

   procedure Set_Focus_Vadjustment
     (Container  : in out Gtk_Container'Class;
      Adjustment : in     Gtk.Adjustment.Gtk_Adjustment'Class);
   --  mapping: Set_Focus_Vadjustment gtkcontainer.h \
   --  mapping:                       gtk_container_set_focus_vadjustment

private

   type Gtk_Container is new Gtk.Widget.Gtk_Widget with null record;

   --  mapping: USE_OBJECT_ORIENTED gtkcontainer.h gtk_container_get_type

   --  services not mapped because they are probably not needed.
   --
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_foreach
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_foreach_interp
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_foreach_full
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_children
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_register_toplevel
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h \
   --  mapping:                 gtk_container_unregister_toplevel
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_focus
   --  mapping: NOT_IMPLEMENTED gtkcontainer.h gtk_container_set_focus_child

end Gtk.Container;
