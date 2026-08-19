------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Handles launching an application in a graphical context.
--
--  It is an implementation of `GAppLaunchContext` that provides startup
--  notification and allows to launch applications on a specific workspace.
--
--  ## Launching an application
--
--  ```c GdkAppLaunchContext *context;
--
--  context = gdk_display_get_app_launch_context (display);
--
--  gdk_app_launch_context_set_timestamp (gdk_event_get_time (event));
--
--  if (!g_app_info_launch_default_for_uri ("http://www.gtk.org", context,
--  &error)) g_warning ("Launching failed: %s\n", error->message);
--
--  g_object_unref (context); ```

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.App_Launch_Context; use Glib.App_Launch_Context;
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Properties;         use Glib.Properties;

package Gdk.App_Launch_Context is

   type Gdk_App_Launch_Context_Record is new Gapp_Launch_Context_Record with null record;
   type Gdk_App_Launch_Context is access all Gdk_App_Launch_Context_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_app_launch_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Display
      (Self : not null access Gdk_App_Launch_Context_Record)
       return Gdk.Gdk_Display;
   --  Gets the `GdkDisplay` that Context is for.
   --  @return the display of Context
   --  Return has transfer-ownership='none'

   procedure Set_Desktop
      (Self    : not null access Gdk_App_Launch_Context_Record;
       Desktop : Glib.Gint);
   --  Sets the workspace on which applications will be launched.
   --  This only works when running under a window manager that supports
   --  multiple workspaces, as described in the [Extended Window Manager
   --  Hints](http://www.freedesktop.org/Standards/wm-spec). Specifically this
   --  sets the `_NET_WM_DESKTOP` property described in that spec.
   --  This only works when using the X11 backend.
   --  When the workspace is not specified or Desktop is set to -1, it is up
   --  to the window manager to pick one, typically it will be the current
   --  workspace.
   --  @param Desktop the number of a workspace, or -1

   procedure Set_Icon
      (Self : not null access Gdk_App_Launch_Context_Record;
       Icon : Glib.G_Icon.G_Icon);
   --  Sets the icon for applications that are launched with this context.
   --  Window Managers can use this information when displaying startup
   --  notification.
   --  See also [methodGdk.AppLaunchContext.set_icon_name].
   --  @param Icon a `GIcon`

   procedure Set_Icon_Name
      (Self      : not null access Gdk_App_Launch_Context_Record;
       Icon_Name : UTF8_String := "");
   --  Sets the icon for applications that are launched with this context.
   --  The Icon_Name will be interpreted in the same way as the Icon field in
   --  desktop files. See also [methodGdk.AppLaunchContext.set_icon].
   --  If both Icon and Icon_Name are set, the Icon_Name takes priority. If
   --  neither Icon or Icon_Name is set, the icon is taken from either the file
   --  that is passed to launched application or from the `GAppInfo` for the
   --  launched application itself.
   --  @param Icon_Name an icon name

   procedure Set_Timestamp
      (Self      : not null access Gdk_App_Launch_Context_Record;
       Timestamp : Guint32);
   --  Sets the timestamp of Context.
   --  The timestamp should ideally be taken from the event that triggered the
   --  launch.
   --  Window managers can use this information to avoid moving the focus to
   --  the newly launched application when the user is busy typing in another
   --  window. This is also known as 'focus stealing prevention'.
   --  @param Timestamp a timestamp

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Display_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Display
   --  The display that the `GdkAppLaunchContext` is on.

private
   Display_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("display");
end Gdk.App_Launch_Context;
