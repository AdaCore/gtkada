-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
--         General Public License for more details.                  --
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

with Gdk; use Gdk;

package Gtk.Object is

   type Gtk_Object is new Root_Type with private;

   procedure Destroy (Object : in out Gtk_Object'Class);
   --  mapping: Destroy gtkobject.h gtk_object_destroy

   function Get_Type (Object : in Gtk_Object'Class) return Gint;
   --  mapping Get_Type gtkobject.h GTK_OBJECT_TYPE

   procedure Ref (Object : in out Gtk_Object);
   --  mapping: Ref gtkobject.h gtk_object_ref

   procedure Unref (Object : in out Gtk_Object);
   --  mapping: Unref gtkobject.h gtk_object_unref

   ---------------
   -- User_Data --
   ---------------

   generic
      type Data_Type (<>) is private;
   package User_Data is
      function Get (Object : in Gtk_Object'Class;
                    Id     : in String := "user_data") return Data_Type;
      --  mapping: User_Data.Get gtkobject.h gtk_object_get_user_data
      --  mapping: User_Data_Get gtkobject.h gtk_object_get_data

      procedure Set (Object : in Gtk_Object'Class;
                     Data   : in Data_Type;
                     Id     : in String := "user_data");
      --  mapping: User_Data.Set gtkobject.h gtk_object_set_user_data
      --  mapping: User_Data.Set gtkobject.h gtk_object_set_data
   end User_Data;

   --  The previous package implements the User_Data stuff.
   --  !! Warning !! No type verification is made to check if you are
   --  using the appropriate function Get. This is your own responsability

   -------------
   --  Flags  --
   -------------

   function Flags (Object : in Gtk_Object'Class) return Guint32;

   procedure Set_Flags (Object : in out Gtk_Object'Class;
                        Flags  : in     Guint32);

   procedure Unset_Flags (Object : in out Gtk_Object'Class;
                          Flags  : in     Guint32);


   function Destroyed (Object : in Gtk_Object'Class) return Boolean;

   function Floating (Object : in Gtk_Object'Class) return Boolean;

   function Connected (Object : in Gtk_Object'Class) return Boolean;

private

   type Gtk_Object is new Root_Type with null record;

   --  Functions which are not implemented because they are probably not needed
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_add_arg_type
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_check_cast
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_check_class_cast
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_class_add_signals
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_class_add_user_signal
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_get_arg_type
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_get_type
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_getv
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_new
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_newv
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_query_args
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_set
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_setv
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_sink
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_weakref
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_weakunref
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_trace_referencing

   --  Functions useless because of the object oriented binding
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_remove_data
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_set_data_full

   --  Functions *internal* to gtk, not mapped
   --  mapping: INTERNAL gtkobject.h gtk_object_data_force_id
   --  mapping: INTERNAL gtkobject.h gtk_object_data_try_key
   --  mapping: INTERNAL gtkobject.h gtk_object_get_data_by_id
   --  mapping: INTERNAL gtkobject.h gtk_object_remove_data_by_id
   --  mapping: INTERNAL gtkobject.h gtk_object_set_data_by_id
   --  mapping: INTERNAL gtkobject.h gtk_object_set_data_by_id_full

end Gtk.Object;
