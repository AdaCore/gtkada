-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
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
   --  mapping: Set_Use_Xshm gdk.h gdk_set_use_xshm

   function Get_Use_Xshm return Boolean;
   --  mapping: Get_Use_Xshm gdk.h gdk_get_use_xshm

   function Get_Display return String;
   --  mapping: Get_Display gdk.h gdk_get_display

   function Time_Get return Guint32;
   --  mapping: Time_Get gdk.h gdk_time_get

   function Timer_Get return Guint32;
   --  mapping: Timer_Get gdk.h gdk_timer_get

   procedure Timer_Set (Milliseconds : in Guint32);
   --  mapping: Timer_Set gdk.h gdk_timer_set

   procedure Timer_Enable;
   --  mapping: Timer_Enable gdk.h gdk_timer_enable

   procedure Timer_Disable;
   --  mapping: Timer_Disable gdk.h gdk_timer_disable


   --  mapping: NOT_IMPLEMENTED gdk.h gdk_input_add_full
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_input_add
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_input_remove
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pointer_grab
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pointer_ungrab
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_keyboard_grab
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_keyboard_ungrab
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_pointer_is_grabbed

   function Screen_Width return Gint;
   --  mapping: Screen_Width gdk.h gdk_screen_width

   function Screen_Height return Gint;
   --  mapping: Screen_Height gdk.h gdk_screen_height

   procedure Flush;
   --  mapping: Flush gdk.h gdk_flush

   procedure Beep;
   --  mapping: Beep gdk.h gdk_beep

   procedure Key_Repeat_Disable;
   --  mapping: Key_Repeat_Disable gdk.h gdk_key_repeat_disable

   procedure Key_Repeat_Restore;
   --  mapping: Key_Repeat_Restore gdk.h gdk_key_repeat_restore


   --  Will probably not be implemented since it does not seem
   --  to be necessary.
   --
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_init
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_exit
   --  mapping: NOT_IMPLEMENTED gdk.h gdk_set_locale


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
