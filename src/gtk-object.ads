package Gtk.Object is

   type Gtk_Object is new Root_Type with private;
   --
   --  FIXME  I wonder wether Gtk_Object should be an abstract
   --  FIXME  type or not...

   procedure Destroy (Object : in out Gtk_Object'Class);
   --  mapping: Destroy gtkobject.h gtk_object_destroy

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
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_get_data
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_get_user_data
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_remove_data
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_set_data
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_set_data_full
   --  mapping: USE_OBJECT_ORIENTED gtkobject.h gtk_object_set_user_data

   --  Functions *internal* to gtk, not mapped
   --  mapping: INTERNAL gtkobject.h gtk_object_data_force_id
   --  mapping: INTERNAL gtkobject.h gtk_object_data_try_key
   --  mapping: INTERNAL gtkobject.h gtk_object_get_data_by_id
   --  mapping: INTERNAL gtkobject.h gtk_object_remove_data_by_id
   --  mapping: INTERNAL gtkobject.h gtk_object_set_data_by_id
   --  mapping: INTERNAL gtkobject.h gtk_object_set_data_by_id_full

end Gtk.Object;
