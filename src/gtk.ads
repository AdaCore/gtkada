
with System;

package Gtk is

   -----------------------
   --  The basic types  --
   -----------------------

   subtype Gint  is Integer;
   subtype Gint32  is Integer range -(2 ** 16) .. (2 ** 16 - 1);

   subtype Guint   is Positive;
   subtype Guint8  is Integer range 0 .. 2 ** 8 - 1;
   subtype Guint32 is Long_Long_Integer range 0 .. 2 ** 32 - 1;

   subtype Gfloat is Float;
   subtype Gdouble is Long_Float;

   type String_Ptr is access String;

   -----------------------
   --  The Object type  --
   -----------------------

   type Gtk_Object is tagged private;

   procedure Destroy (Obj : access Gtk_Object'Class);
   --  mapping: Destroy gtkobject.h gtk_object_destroy


   -------------------------------
   --  The Gtk main procedures  --
   -------------------------------

   procedure Init;
   --  mapping: Init gtkmain.h gtk_init

   procedure Main;
   --  mapping: Main gtkmain.h gtk_main

   procedure Main_Quit;
   --  mapping: Main_Quit gtkmain.h gtk_main_quit

   type Gtk_Update_Type is (Update_Continuous,
                            Update_Discontinuous,
                            Update_Delayed);
   --  mapping: Gtk_Update_Type gtkenums.h GtkUpdateType
   --  FIXME  Should be somewhere else

private

   type Gtk_Object is tagged
      record
         Ptr : System.Address := System.Null_Address;
      end record;

   function Get_Object (Obj : in Gtk_Object'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Obj : in out Gtk_Object'Class;
                         Value  : in     System.Address);
   pragma Inline (Set_Object);

   function To_Boolean (Value : in Gint) return Boolean;
   function To_Gint (Bool : in Boolean) return Gint;

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
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_ref
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_set
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_setv
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_sink
   --  mapping: NOT_IMPLEMENTED gtkobject.h gtk_object_unref
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

end Gtk;
