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

with Gdk.Event;
with Gtk.Arguments;
with Gtk.Marshallers;
with Gtk.Notebook;
with Gtk.Object;
with Gtk.Widget;

--  This package is made of four sub-packages.
--
--  They represent the four possible kind of callbacks: they can return values
--  or not, and they can have user specific data associated to them or not.
--
--  They are generic packages. The common parameter is Base_Type, which
--  is the basic widget manipulated. If you do not want to instantiate too
--  many of these packages, you can use Gtk.Object.Gtk_Object_Record. The
--  callbacks will then have to do the conversions.
--  !!Important Note!! These packages must be implemented at library-level.
--
--  The general organization of the packages is the same:
--  * The type "Handler" is the general form of the callbacks.
--    The callbacks in this package are as general as possible: they receive as
--    an argument an array of all the specific values created by gtk+. It is
--    your responsability to extract the values from it and convert them to
--    whatever Ada type they represent. The functions in Gtk.Marshallers will
--    help you to do this job.
--  * A series of Connect function is given. They cover all the cases,
--    from returning or not the "Handler_Id" of the newly create association,
--    connecting either a "Handler" or a "Generic_Marshaller", ...
--    There are four kinds of connect function in gtk+:
--      gtk_signal_connect: connect the widget to the signal
--      gtk_signal_connect_after: the new handler is put last on the list.
--        Since we can have default values for parameters in Ada, this is
--        represented by one more parameter, After, with a default value.
--      gtk_signal_connect_object: the user_data is an Gtk Object, which
--        is passed to the handler instead of the object to which the signal
--        was connected. These functions never have a user data.
--      gtk_signal_connect_object_after: Same as above, but put the handler
--        at the end of the list. This is also implemented as a parameter
--        with a default value.
--
--  You should also look at the package Gtk.Marshallers which gives a
--  more natural way to use callbacks.
--
--  FIXME: The following was taken out of Gtk.Marshallers.
--  FIXME: Add thi to the future documentation of Gtk.Handlers.
--  Here is an example:
--     We want to connect the "delete_event" signal to a widget. The
--     handlers for this signal get an extra new argument that is the
--     Gdk_Event that generated the signal.
--     Here is how to do it:
--
--         with Gtk.Handlers;    use Gtk.Handlers;
--         with Gtk.Marshallers; use Gtk.Marshallers;
--
--         function My_Cb (Widget : access Gtk_Widget_Record'Class;
--                         Event  : Gdk.Event.Gdk_Event)
--                         return Gint;
--         --  your own function
--
--         package Return_Widget_Cb is new Gtk.Handlers.Return_Callback
--            (Gtk.Widget.Gtk_Widget_Record, Gint);
--
--         Return_Widget_Cb.Connect (W, "delete_event",
--            Return_Widget_Cb.To_Event_Marshaller (My_Cb'Access));
--
--  The real handler in gtk+ should expect at least as many arguments as
--  in the marshaller you are using (i.e if your marshaller has one
--  argument (as in the Generic_Marshaller or Generic_Widget_Marshaller
--  below, the C handler must have at least one argument too).
--
--  There are four generic packages here, the same organization as in
--  Gtk.Handlers (i.e there is one for callbacks returning values, but
--  with no
--  user data, for callbacks returning values and with a user callbacks,
--  for
--  callbacks not returning any value and with no user data, and for
--  callbacks
--  returning values and with a user data.

package Gtk.Handlers is
   pragma Elaborate_Body;

   type Handler_Id is new Guint;
   --  This uniquely identifies a connection widget<->signal.

   ---------------------------------------------------------
   --  These handlers should return a value
   --  They do not have a User_Data
   ---------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
      type Return_Type is private;
   package Return_Callback is

      type Handler is access function (Widget : access Widget_Type'Class;
                                       Params : Gtk.Arguments.Gtk_Args)
                                      return Return_Type;

      package Marshallers is new Gtk.Marshallers.Return_Marshallers
        (Widget_Type, Return_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Marsh   : in     Marshallers.Marshaller;
         After   : in     Boolean := False);

      procedure Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Marsh       : in     Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False);

      procedure Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Cb      : in     Handler;
         After   : in     Boolean := False);

      procedure Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Cb          : in     Handler;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False);

      pragma Inline (Connect);
      pragma Inline (Object_Connect);

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Marsh   : in     Marshallers.Marshaller;
         After   : in     Boolean := False)
        return Handler_Id;

      function Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Marsh       : in     Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False)
        return Handler_Id;

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Cb      : in     Handler;
         After   : in     Boolean := False)
        return Handler_Id;

      function Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Cb          : in     Handler;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False)
        return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Gtk.Arguments.To_Gint);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Gtk.Arguments.To_Guint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gtk.Arguments.To_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Arguments.To_Notebook_Page);

      function To_Marshaller (Cb : Gint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Gint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Guint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Guint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Event_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Event_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Widget_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Widget_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Marshallers.Void_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Marshallers.Void_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Notebook_Page_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Notebook_Page_Marshaller.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gint)
                             renames Gint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Guint)
                             renames Guint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gdk.Event.Gdk_Event)
                             renames Event_Marshaller.Emit_By_Name;
      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : in String;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
        renames Widget_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String)
                             renames Marshallers.Void_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gtk.Notebook.Gtk_Notebook_Page)
                             renames Notebook_Page_Marshaller.Emit_By_Name;

   end Return_Callback;


   ---------------------------------------------------------
   --  These handlers should return a value
   --  They require a User_Data
   ---------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
      type Return_Type is private;
      type User_Type (<>) is private;
   package User_Return_Callback is

      type Handler is access function (Widget    : access Widget_Type'Class;
                                       Params    : Gtk.Arguments.Gtk_Args;
                                       User_Data : User_Type)
                                      return Return_Type;

      package Marshallers is new Gtk.Marshallers.User_Return_Marshallers
        (Widget_Type, Return_Type, User_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Marsh     : in     Marshallers.Marshaller;
         User_Data : in     User_Type;
         After     : in     Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Cb        : in     Handler;
         User_Data : in     User_Type;
         After     : in     Boolean := False);

      pragma Inline (Connect);

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Marsh     : in     Marshallers.Marshaller;
         User_Data : in     User_Type;
         After     : in     Boolean := False)
        return Handler_Id;

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Cb        : in     Handler;
         User_Data : in     User_Type;
         After     : in     Boolean := False)
        return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Gtk.Arguments.To_Gint);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Gtk.Arguments.To_Guint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gtk.Arguments.To_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Arguments.To_Notebook_Page);

      function To_Marshaller (Cb : Gint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Gint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Guint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Guint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Event_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Event_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Widget_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Widget_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Marshallers.Void_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Marshallers.Void_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Notebook_Page_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Notebook_Page_Marshaller.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gint)
                             renames Gint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Guint)
                             renames Guint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gdk.Event.Gdk_Event)
                             renames Event_Marshaller.Emit_By_Name;
      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : in String;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
        renames Widget_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String)
                             renames Marshallers.Void_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gtk.Notebook.Gtk_Notebook_Page)
                             renames Notebook_Page_Marshaller.Emit_By_Name;

   end User_Return_Callback;

   ---------------------------------------------------------
   --  These handlers do not return a value
   --  They do not have a User_Data
   ---------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
   package Callback is

      type Handler is access procedure (Widget : access Widget_Type'Class;
                                        Params : Gtk.Arguments.Gtk_Args);

      package Marshallers is
         new Gtk.Marshallers.Void_Marshallers (Widget_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Marsh   : in     Marshallers.Marshaller;
         After   : in     Boolean := False);

      procedure Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Marsh       : in     Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False);

      procedure Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Cb      : in     Handler;
         After   : in     Boolean := False);

      procedure Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Cb          : in     Handler;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False);

      pragma Inline (Connect);
      pragma Inline (Object_Connect);

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Marsh   : in     Marshallers.Marshaller;
         After   : in     Boolean := False)
        return Handler_Id;

      function Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Marsh       : in     Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False)
        return Handler_Id;

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Cb      : in     Handler;
         After   : in     Boolean := False)
        return Handler_Id;

      function Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Cb          : in     Handler;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False)
        return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Gtk.Arguments.To_Gint);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Gtk.Arguments.To_Guint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gtk.Arguments.To_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Arguments.To_Notebook_Page);

      function To_Marshaller (Cb : Gint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Gint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Guint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Guint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Event_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Event_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Widget_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Widget_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Marshallers.Void_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Marshallers.Void_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Notebook_Page_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Notebook_Page_Marshaller.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gint)
                             renames Gint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Guint)
                             renames Guint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gdk.Event.Gdk_Event)
                             renames Event_Marshaller.Emit_By_Name;
      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : in String;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
        renames Widget_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String)
                             renames Marshallers.Void_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gtk.Notebook.Gtk_Notebook_Page)
                             renames Notebook_Page_Marshaller.Emit_By_Name;

   end Callback;


   ---------------------------------------------------------
   --  These handlers do not return a value
   --  They require a User_Data
   ---------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
      type User_Type (<>) is private;
   package User_Callback is

      type Handler is access procedure (Widget    : access Widget_Type'Class;
                                        Params    : Gtk.Arguments.Gtk_Args;
                                        User_Data : User_Type);

      package Marshallers is new Gtk.Marshallers.User_Void_Marshallers
        (Widget_Type, User_Type);

      --  Connecting a handler to an object

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Marsh     : in     Marshallers.Marshaller;
         User_Data : in     User_Type;
         After     : in     Boolean := False);

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Cb        : in     Handler;
         User_Data : in     User_Type;
         After     : in     Boolean := False);

      pragma Inline (Connect);

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Marsh     : in     Marshallers.Marshaller;
         User_Data : in     User_Type;
         After     : in     Boolean := False)
        return Handler_Id;

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : in     String;
         Cb        : in     Handler;
         User_Data : in     User_Type;
         After     : in     Boolean := False)
        return Handler_Id;

      --  Some convenient functions to create marshallers

      package Gint_Marshaller is new Marshallers.Generic_Marshaller
        (Gint, Gtk.Arguments.To_Gint);
      package Guint_Marshaller is new Marshallers.Generic_Marshaller
        (Guint, Gtk.Arguments.To_Guint);
      package Event_Marshaller is new Marshallers.Generic_Marshaller
        (Gdk.Event.Gdk_Event, Gtk.Arguments.To_Event);
      package Widget_Marshaller is new Marshallers.Generic_Widget_Marshaller
        (Gtk.Widget.Gtk_Widget_Record, Gtk.Widget.Gtk_Widget);
      package Notebook_Page_Marshaller is new Marshallers.Generic_Marshaller
        (Gtk.Notebook.Gtk_Notebook_Page, Gtk.Arguments.To_Notebook_Page);

      function To_Marshaller (Cb : Gint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Gint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Guint_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Guint_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Event_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Event_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Widget_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Widget_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Marshallers.Void_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Marshallers.Void_Marshaller.To_Marshaller;
      function To_Marshaller (Cb : Notebook_Page_Marshaller.Handler)
                             return Marshallers.Marshaller
                             renames Notebook_Page_Marshaller.To_Marshaller;

      --  Emitting a signal

      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gint)
                             renames Gint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Guint)
                             renames Guint_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gdk.Event.Gdk_Event)
                             renames Event_Marshaller.Emit_By_Name;
      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : in String;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
        renames Widget_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String)
                             renames Marshallers.Void_Marshaller.Emit_By_Name;
      procedure Emit_By_Name (Object : access Widget_Type'Class;
                              Name   : in String;
                              Param  : in Gtk.Notebook.Gtk_Notebook_Page)
                             renames Notebook_Page_Marshaller.Emit_By_Name;

   end User_Callback;

   ------------------------------------------------------------------
   --  General functions
   ------------------------------------------------------------------

   procedure Disconnect
     (Object : access Gtk.Object.Gtk_Object_Record'Class;
      Id     : in Handler_Id);

   procedure Emit_Stop_By_Name
     (Object : access Gtk.Object.Gtk_Object_Record'Class;
      Name   : in String);

   procedure Handler_Block
     (Obj : access Gtk.Object.Gtk_Object_Record'Class;
      Id  : in Handler_Id);

   procedure Handlers_Destroy
     (Obj : access Object.Gtk_Object_Record'Class);

   procedure Handler_Unblock
     (Obj : access Gtk.Object.Gtk_Object_Record'Class;
      Id  : in Handler_Id);

   function Count_Arguments
     (The_Type : Gtk_Type; Name : in String) return Guint;
   --  Returns the number of arguments used in the handlers for the signal
   --  Note that in the Connect functions, we always test whether the user
   --  has asked for *at most* the number of arguments defined by gtk+ for the
   --  callback. This is because having less argument is authorized (the
   --  extra parameters passed by gtk+ will simply be ignored), whereas having
   --  more arguments is impossible (they would never be set).

   function Argument_Type
     (The_Type : Gtk_Type;
      Name     : in String;
      Num      : in Gint) return Gtk_Type;
   --  Returns the type of the num-th argument for the handlers of signal
   --  name.
   --  If Num is negative, returns the type returned by the handlers for this
   --  signal.

end Gtk.Handlers;
