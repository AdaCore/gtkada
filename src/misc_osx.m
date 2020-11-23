/*
-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2020, AdaCore                   --
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

//  Support for opening files from the OS in OSX

#import <Cocoa/Cocoa.h>

#include <glib.h>
#include <gdk/gdkquartz.h>

// gdk_quartz_window_get_nswindow was dropped from headers since gtk 3.24.14
#import <AppKit/AppKit.h>
NSWindow *gdk_quartz_window_get_nswindow(GdkWindow *window);

#include "misc_osx.h"

/****************************************************************************
 * OPEN FILE HANDLING PART
 ****************************************************************************/

/* As OSX Event Handlers are called from a separate thread, we can't call
   any of the glib or gtk functions directly. Instead we build a temporary
   structure with a copy of all the paths to open, and in an idle callback
   we create the final structure containing gfiles
 */
typedef struct {
  char             **paths;
  gint             num_paths;
  gpointer         user_data;
  osx_open_files_cb callback;
} open_files_data;

/*****************
 * _open_file_cb *
 *****************/

/* used to create gfiles from within an idle callback */
static gboolean
_open_files_cb
  (gpointer of_data)
{
  open_files_data   *data      = (open_files_data*)of_data;
  osx_open_files_cb callback   = data->callback;
  gpointer          user_data  = data->user_data;
  gint              n_files    = data->num_paths;
  GFile             **files;

  files = malloc (n_files * sizeof(GFile*));

  for (gint i = 0; i < n_files; i++) {
    files[i] = g_file_new_for_path (data->paths[i]);
    g_free (data->paths[i]);
  }
  g_free (data->paths);
  g_free (data);

  callback (files, n_files, user_data);

  return G_SOURCE_REMOVE;
}

@interface OSXEventHandler : NSObject {
  osx_open_files_cb callback;
  gpointer          user_data;
}

/* Constructor: creates an event handler object calling cb on event, with
 * u_data as user_data parameter
 */
- (id)initWithCallback:(osx_open_files_cb)cb withData:(gpointer)u_data;

/* Actually installs the handler
 */
- (void)installEventHandler;

@end

@implementation OSXEventHandler

/********************
 * initWithCallback *
 ********************/

- (id)initWithCallback:(osx_open_files_cb)cb withData:(gpointer)u_data
{
   callback  = cb;
   user_data = u_data;

   return [super init];
}

/***********************
 * installEventHandler *
 ***********************/

- (void)installEventHandler
{
  NSAppleEventManager *em = [NSAppleEventManager sharedAppleEventManager];
  [em
   setEventHandler:self
   andSelector:@selector(handleOpenDocument:withReplyEvent:)
   forEventClass:kCoreEventClass
   andEventID:kAEOpenDocuments];
}

/**********************
 * handleOpenDocument *
 **********************/

- (void)handleOpenDocument:(NSAppleEventDescriptor*)event
            withReplyEvent:(NSAppleEventDescriptor*)replyEvent
{
  if (!event) return;

  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
  NSAppleEventDescriptor *fileListDescriptor = NULL;
  NSInteger numberOfFiles = 0;

  if ([event eventClass] == kCoreEventClass &&
      [event eventID] == kAEOpenDocuments)
    {
      fileListDescriptor = [event paramDescriptorForKeyword:keyDirectObject];
    }

  if (fileListDescriptor)
    {
      numberOfFiles = [fileListDescriptor numberOfItems];
    }

   if (numberOfFiles > 0)
    {
      open_files_data *of_data = g_malloc (sizeof (open_files_data));

      of_data->user_data = user_data;
      of_data->callback  = callback;
      of_data->num_paths = 0;
      of_data->paths     = (char**)g_malloc(numberOfFiles * sizeof(char*));

      for (NSInteger i = 1; i <= numberOfFiles; i++)
        {
          NSString* urlString =
            [[fileListDescriptor descriptorAtIndex:i] stringValue];
          if (!urlString) continue;

          // String in URL form: file://...
          // so we create an actual NSURL object from it, then retrieve the
          // fs representation that we need
          NSURL* url = [NSURL URLWithString:urlString];
          if (!url) continue;

          const char* path = [url fileSystemRepresentation];

          of_data->paths[of_data->num_paths] =
            (char *)g_malloc(strlen (path) + 1);
          strcpy (of_data->paths[of_data->num_paths], path);

          of_data->num_paths += 1;
        }

      // Cocoa events are processed by a separate thread. So we need to
      // create the gfiles from the main context explicitely
      g_main_context_invoke
        (NULL, _open_files_cb, (gpointer)of_data);
    }

    [pool drain];
}

@end

/*************************************
 * init_osx_open_files_event_handler *
 *************************************/

/* Sets an event handler that reacts to the Open File event and calls
 * ada_gtk_application_open_files.
 */
void
init_osx_open_files_event_handler
  (osx_open_files_cb callback,
   gpointer          user_data)
  {
    OSXEventHandler *handler =
      [[OSXEventHandler alloc] initWithCallback:callback withData:user_data];
    [handler installEventHandler];
  }


/****************************************************************************
 * FULLSCREEN SUPPORT
 ****************************************************************************/

/****************************************
 * _ada_gtk_osx_allow_fullscreen_mapped *
 ****************************************/

/* Enable the full-screen mode for the mapped GtkWindow window
 */
static void
_ada_gtk_osx_allow_fullscreen_mapped (GtkWindow *window)
  {
    GdkWindow *gdk_window = gtk_widget_get_window (GTK_WIDGET (window));
    GdkWindow *toplevel = gdk_window_get_effective_toplevel (gdk_window);
    NSWindow *nswindow = gdk_quartz_window_get_nswindow (toplevel);
    [nswindow
       setCollectionBehavior:NSWindowCollectionBehaviorFullScreenPrimary];
  }

/*******************************************
 * _ada_gtk_osx_allow_fullscreen_on_mapped *
 *******************************************/

/* Called when the 'mapped' event is signalled on window
 */
static void
_ada_gtk_osx_allow_fullscreen_on_mapped
  (GtkWindow *window, GdkEvent *event, gpointer user_data)
{
  _ada_gtk_osx_allow_fullscreen_mapped (window);
  g_signal_handlers_disconnect_by_func
    (window, _ada_gtk_osx_allow_fullscreen_on_mapped, NULL);
}

/********************************
 * ada_gtk_osx_allow_fullscreen *
 ********************************/

/* exported function, setting fullscreen mode on window
 */
void ada_gtk_osx_allow_fullscreen (GtkWindow *window)
{
  GdkWindow *gdk_window = gtk_widget_get_window (GTK_WIDGET (window));
  if (!gdk_window)
    {
      g_signal_connect
        (window,
         "map-event",
         G_CALLBACK (_ada_gtk_osx_allow_fullscreen_on_mapped),
         NULL);
    }
  else
    {
      _ada_gtk_osx_allow_fullscreen_mapped (window);
    }
}

/********************
 * ada_activate_app *
 ********************/

void ada_activate_app (void)
{
  if (![NSApp isActive])
     [NSApp activateIgnoringOtherApps:YES];
}

