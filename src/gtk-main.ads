package Gtk.Main is

   procedure Init;
   --  mapping: Init gtkmain.h gtk_init

   procedure Gtk_Exit (Error_Code : in Gint);
   --  mapping: Gtk_Exit gtkmain.h gtk_exit

   function Set_Locale return String;
   --  mapping: Set_Locale gtkmain.h gtk_set_locale

   procedure Set_Locale;
   --
   --  Drops the string returned by the Set_Locale function.

   function Events_Pending return Gint;
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_events_pending

   procedure Main;
   --  mapping: Main gtkmain.h gtk_main

   function Main_Level return Gint;
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_main_level

   procedure Main_Quit;
   --  mapping: Main_Quit gtkmain.h gtk_main_quit

   function Main_Iteration return Gint;
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_main_iteration


   --  Some services can be directly binded...
   --
   pragma Import (C, Init, "ag_gtk_init");
   pragma Import (C, Gtk_Exit, "gtk_exit");
   pragma Import (C, Events_Pending, "gtk_events_pending");
   pragma Import (C, Main, "gtk_main");
   pragma Import (C, Main_Level, "gtk_main_level");
   pragma Import (C, Main_Quit, "gtk_main_quit");
   pragma Import (C, Main_Iteration, "gtk_main_iteration");

   ----------
   -- Idle --
   ----------

   generic
      type Data_Type (<>) is private;
   package Idle is
      type Callback is access function (D : in Data_Type) return Boolean;

      function Add (Cb : in Callback;  D : in Data_Type) return Guint;
      --  mapping: Idle_Func.Add gtkmain.h gtk_idle_add

   end Idle;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.

   procedure Idle_Remove (Id : in Guint);
   pragma Import (C, Idle_Remove, "gtk_idle_remove");
   --  mapping: Idle_Remove gtkmain.h gtk_idle_remove

   -------------
   -- Timeout --
   -------------

   generic
      type Data_Type (<>) is private;
   package Timeout is
      type Callback is access function (D : in Data_Type) return Boolean;

      function Add (Interval : in Guint32;
                    Func     : in Callback;
                    D        : in Data_Type)
                    return      Guint;
      --  mapping: Timeout.Add gtkmain.h gtk_timeout_add

   end Timeout;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.

   procedure Timeout_Remove (Id : in Guint);
   pragma Import (C, Timeout_Remove, "gtk_timeout_remove");
   --  mapping: Timeout_Remove gtkmain.h gtk_timeout_remove

   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_main_iteration_do
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_true
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_false
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_grab_add
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_grab_get_current
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_grab_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_init_add
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_quit_add_destroy
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_quit_add
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_quit_add_full
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_quit_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_quit_remove_by_data
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_timeout_add_full
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_timeout_add_interp
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add_priority
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add_full
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add_interp
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_remove_by_data
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_input_add_full
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_input_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_key_snooper_install
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_key_snooper_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_get_current_event
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_get_event_widget

end Gtk.Main;
