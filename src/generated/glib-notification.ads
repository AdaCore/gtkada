------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  Glib.Notification.Gnotification is a mechanism for creating a notification
--  to be shown to the user -- typically as a pop-up notification presented by
--  the desktop environment shell.
--
--  The key difference between Glib.Notification.Gnotification and other
--  similar APIs is that, if supported by the desktop environment,
--  notifications sent with Glib.Notification.Gnotification will persist after
--  the application has exited, and even across system reboots.
--
--  Since the user may click on a notification while the application is not
--  running, applications using Glib.Notification.Gnotification should be able
--  to be started as a D-Bus service, using Glib.Application.Gapplication.
--
--  User interaction with a notification (either the default action, or
--  buttons) must be associated with actions on the application (ie: "app."
--  actions). It is not possible to route user interaction through the
--  notification itself, because the object will not exist if the application
--  is autostarted as a result of a notification being clicked.
--
--  A notification can be sent with Glib.Application.Send_Notification.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib.G_Icon;             use Glib.G_Icon;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Variant;            use Glib.Variant;

package Glib.Notification is

   type Gnotification_Record is new GObject_Record with null record;
   type Gnotification is access all Gnotification_Record'Class;

   type GNotification_Priority is (
      G_Notification_Priority_Normal,
      G_Notification_Priority_Low,
      G_Notification_Priority_High,
      G_Notification_Priority_Urgent);
   pragma Convention (C, GNotification_Priority);
   --  Priority levels for GNotifications.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package GNotification_Priority_Properties is
      new Generic_Internal_Discrete_Property (GNotification_Priority);
   type Property_GNotification_Priority is new GNotification_Priority_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure G_New (Self : out Gnotification; Title : UTF8_String);
   --  Creates a new Glib.Notification.Gnotification with Title as its title.
   --  After populating Notification with more details, it can be sent to the
   --  desktop shell with Glib.Application.Send_Notification. Changing any
   --  properties after this call will not have any effect until resending
   --  Notification.
   --  Since: gtk+ 2.40
   --  "title": the title of the notification

   procedure Initialize
      (Self  : not null access Gnotification_Record'Class;
       Title : UTF8_String);
   --  Creates a new Glib.Notification.Gnotification with Title as its title.
   --  After populating Notification with more details, it can be sent to the
   --  desktop shell with Glib.Application.Send_Notification. Changing any
   --  properties after this call will not have any effect until resending
   --  Notification.
   --  Since: gtk+ 2.40
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "title": the title of the notification

   function Gnotification_New (Title : UTF8_String) return Gnotification;
   --  Creates a new Glib.Notification.Gnotification with Title as its title.
   --  After populating Notification with more details, it can be sent to the
   --  desktop shell with Glib.Application.Send_Notification. Changing any
   --  properties after this call will not have any effect until resending
   --  Notification.
   --  Since: gtk+ 2.40
   --  "title": the title of the notification

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_notification_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Button
      (Self            : not null access Gnotification_Record;
       Label           : UTF8_String;
       Detailed_Action : UTF8_String);
   --  Adds a button to Notification that activates the action in
   --  Detailed_Action when clicked. That action must be an application-wide
   --  action (starting with "app."). If Detailed_Action contains a target, the
   --  action will be activated with that target as its parameter.
   --  See g_action_parse_detailed_name for a description of the format for
   --  Detailed_Action.
   --  Since: gtk+ 2.40
   --  "label": label of the button
   --  "detailed_action": a detailed action name

   procedure Add_Button_With_Target_Value
      (Self   : not null access Gnotification_Record;
       Label  : UTF8_String;
       Action : UTF8_String;
       Target : Glib.Variant.Gvariant);
   --  Adds a button to Notification that activates Action when clicked.
   --  Action must be an application-wide action (it must start with "app.").
   --  If Target is non-null, Action will be activated with Target as its
   --  parameter.
   --  Since: gtk+ 2.40
   --  "label": label of the button
   --  "action": an action name
   --  "target": a Glib.Variant.Gvariant to use as Action's parameter, or null

   procedure Set_Body
      (Self     : not null access Gnotification_Record;
       The_Body : UTF8_String := "");
   --  Sets the body of Notification to Body.
   --  Since: gtk+ 2.40
   --  "the_body": the new body for Notification, or null

   procedure Set_Default_Action
      (Self            : not null access Gnotification_Record;
       Detailed_Action : UTF8_String);
   --  Sets the default action of Notification to Detailed_Action. This action
   --  is activated when the notification is clicked on.
   --  The action in Detailed_Action must be an application-wide action (it
   --  must start with "app."). If Detailed_Action contains a target, the given
   --  action will be activated with that target as its parameter. See
   --  g_action_parse_detailed_name for a description of the format for
   --  Detailed_Action.
   --  When no default action is set, the application that the notification
   --  was sent on is activated.
   --  Since: gtk+ 2.40
   --  "detailed_action": a detailed action name

   procedure Set_Default_Action_And_Target_Value
      (Self   : not null access Gnotification_Record;
       Action : UTF8_String;
       Target : Glib.Variant.Gvariant);
   --  Sets the default action of Notification to Action. This action is
   --  activated when the notification is clicked on. It must be an
   --  application-wide action (start with "app.").
   --  If Target is non-null, Action will be activated with Target as its
   --  parameter.
   --  When no default action is set, the application that the notification
   --  was sent on is activated.
   --  Since: gtk+ 2.40
   --  "action": an action name
   --  "target": a Glib.Variant.Gvariant to use as Action's parameter, or null

   procedure Set_Icon
      (Self : not null access Gnotification_Record;
       Icon : Glib.G_Icon.G_Icon);
   --  Sets the icon of Notification to Icon.
   --  Since: gtk+ 2.40
   --  "icon": the icon to be shown in Notification, as a Glib.G_Icon.G_Icon

   procedure Set_Priority
      (Self     : not null access Gnotification_Record;
       Priority : GNotification_Priority);
   --  Sets the priority of Notification to Priority. See
   --  Glib.Notification.GNotification_Priority for possible values.
   --  "priority": a Glib.Notification.GNotification_Priority

   procedure Set_Title
      (Self  : not null access Gnotification_Record;
       Title : UTF8_String);
   --  Sets the title of Notification to Title.
   --  Since: gtk+ 2.40
   --  "title": the new title for Notification

   procedure Set_Urgent
      (Self   : not null access Gnotification_Record;
       Urgent : Boolean);
   pragma Obsolescent (Set_Urgent);
   --  Deprecated in favor of Glib.Notification.Set_Priority.
   --  Since: gtk+ 2.40
   --  Deprecated since 2.42, 1
   --  "urgent": True if Notification is urgent

end Glib.Notification;
