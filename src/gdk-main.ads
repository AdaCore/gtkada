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

with Glib; use Glib;

package Gdk.Main is

   procedure Set_Use_Xshm (Use_Xshm : in Boolean := True);

   function Get_Use_Xshm return Boolean;

   function Get_Display return String;

   function Time_Get return Guint32;

   function Timer_Get return Guint32;

   procedure Timer_Set (Milliseconds : in Guint32);

   procedure Timer_Enable;

   procedure Timer_Disable;



   function Screen_Width return Gint;

   function Screen_Height return Gint;

   procedure Flush;

   procedure Beep;

   procedure Key_Repeat_Disable;

   procedure Key_Repeat_Restore;


   --  Will probably not be implemented since it does not seem
   --  to be necessary.
   --


   pragma Import (C, Time_Get, "gdk_time_get");
   pragma Import (C, Timer_Get, "gdk_timer_get");
   pragma Import (C, Timer_Set, "gdk_timer_set");
   pragma Import (C, Timer_Enable, "gdk_timer_enable");
   pragma Import (C, Timer_Disable, "gdk_timer_disable");
   pragma Import (C, Screen_Width, "gdk_screen_width");
   pragma Import (C, Screen_Height, "gdk_screen_height");
   pragma Import (C, Flush, "gdk_flush");
   pragma Import (C, Beep, "gdk_beep");
   pragma Import (C, Key_Repeat_Disable, "gdk_key_repeat_disable");
   pragma Import (C, Key_Repeat_Restore, "gdk_key_repeat_restore");

end Gdk.Main;
