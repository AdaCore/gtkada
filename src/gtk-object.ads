package Gtk.Object is

   type Gtk_Object is new Root_Type with private;

   procedure Destroy (Object : in out Gtk_Object'Class);
   --  mapping: Destroy gtkobject.h gtk_object_destroy

   function Get_Type (Object : in Gtk_Object'Class) return Gint;
   --  mapping Get_Type gtkobject.h GTK_OBJECT_TYPE

   function Is_Created (Object : in Gtk_Object) return Boolean;

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

   procedure Adjust (Object : in out Gtk_Object);
   procedure Finalize (Object : in out Gtk_Object);
   procedure Initialize (Object : in out Gtk_Object);


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
