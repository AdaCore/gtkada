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

with Gtk.Widget;

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

   procedure Grab_Add (Widget : Gtk.Widget.Gtk_Widget'Class);
   --  mapping: Grab_Add gtkmain.h gtk_grab_add

   procedure Grab_Remove (Widget : Gtk.Widget.Gtk_Widget'Class);
   --  mapping: Grab_Remove gtkmain.h gtk_grab_remove

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
   --  mapping: NOT_IMPLEMENTED gtkmain.h gtk_grab_get_current
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
