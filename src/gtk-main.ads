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
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_timeout_add
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_timeout_add_interp
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_timeout_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add_priority
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add_full
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_add_interp
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_idle_remove_by_data
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_input_add_full
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_input_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_key_snooper_install
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_key_snooper_remove
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_get_current_event
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_get_event_widget

end Gtk.Main;
