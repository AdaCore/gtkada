-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
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

   procedure Gtk_Exit (Error_Code : in Gint);

   function Set_Locale return String;

   procedure Set_Locale;
   --
   --  Drops the string returned by the Set_Locale function.

   function Events_Pending return Gint;

   procedure Main;

   function Main_Level return Gint;

   procedure Main_Quit;

   function Main_Iteration return Gint;

   procedure Grab_Add (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   procedure Grab_Remove (Widget : access Gtk.Widget.Gtk_Widget_Record'Class);

   ----------
   -- Idle --
   ----------

   generic
      type Data_Type (<>) is private;
   package Idle is
      type Callback is access function (D : in Data_Type) return Boolean;

      function Add (Cb : in Callback;  D : in Data_Type) return Guint;

   end Idle;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.

   procedure Idle_Remove (Id : in Guint);

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

   end Timeout;
   --  !!Warning!! The instances of this package must be declared at library
   --  level, as they are some accesses to internal functions that happen
   --  when the callback is called.

   procedure Timeout_Remove (Id : in Guint);

private
   --  Some services can be directly binded...

   pragma Import (C, Gtk_Exit, "gtk_exit");
   pragma Import (C, Events_Pending, "gtk_events_pending");
   pragma Import (C, Main, "gtk_main");
   pragma Import (C, Main_Level, "gtk_main_level");
   pragma Import (C, Main_Quit, "gtk_main_quit");
   pragma Import (C, Main_Iteration, "gtk_main_iteration");

   pragma Import (C, Idle_Remove, "gtk_idle_remove");
   pragma Import (C, Timeout_Remove, "gtk_timeout_remove");
end Gtk.Main;
