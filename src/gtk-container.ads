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
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Adjustment;
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
