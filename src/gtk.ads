
with System;

package Gtk is

   subtype Gdouble is Long_Float;
   subtype Gint    is Integer;
   subtype Guint   is Positive;
   subtype Gint32  is Integer range -(2 ** 16) .. (2 ** 16 - 1);
   --  Same for all basic types

   type Gtk_Update_Type is (Update_Continuous,
                            Update_Discontinuous,
                            Update_Delayed);
   --  mapping: Gtk_Update_Type gtkenums.h GtkUpdateType

   type Gtk_Object is tagged private;

   procedure Init;
   pragma Import (C, Init, "ag_gtk_init");
   --  defined in misc-gnat.c
   --  mapping: Init gtkmain.h gtk_init

   procedure Main;
   pragma Import (C, Main, "gtk_main");
   --  mapping: Main gtkmain.h gtk_main

   procedure Main_Quit;
   pragma Import (C, Main_Quit, "gtk_main_quit");
   --  mapping: Main_Quit gtkmain.h gtk_main_quit

   procedure Destroy (Obj : in Gtk_Object'Class);
   --  mapping: Destroy gtkobject.h gtk_object_destroy

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


private

   type Gtk_Object is tagged
      record
         Ptr : System.Address := System.Null_Address;
      end record;

   function Get_Object (Obj : in Gtk_Object'Class)
                        return System.Address;
   pragma Inline (Get_Object);

   procedure Set_Object (Obj   : in out Gtk_Object'Class;
                         Value : in     System.Address);
   pragma Inline (Set_Object);

   function To_Boolean (Value : in Gint) return Boolean;
   pragma Inline (To_Boolean);

   function To_Gint (Bool : in Boolean) return Gint;
   pragma Inline (To_Gint);

end Gtk;
