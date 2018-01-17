/*
-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2000-2018, AdaCore                   --
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
*/

/* This file shows the exported method of misc_osx.m
 * It should be kept 100% C compliand, and not import or define any
 * objective-c object
 */

#ifndef _OSX_EVENT_HANDLER_H_
#define _OSX_EVENT_HANDLER_H_

#include <glib.h>
#include <gtk/gtk.h>

/*
 * callback profile for the open_file event handler.
 *
 * files is to be freed by the user
 */
typedef void (*osx_open_files_cb)
   (GFile** files, gint n_files, gpointer user_data);

/* Registers an event handler for the "open file" event raised by the
 * OS
 */
void
init_osx_open_files_event_handler
  (osx_open_files_cb callback,
   gpointer          user_data);

/* Allows the fullscreen mode for window
 */
void ada_gtk_osx_allow_fullscreen (GtkWindow *window);

/* Activates the app
 */
void ada_activate_app(void);

#endif
