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

--  <description>
--
--  The aim of this package is to provide some services to connect a
--  handler to a signal emitted by a Gtk Object. To understand the
--  services provided by this package, some definitions are necessary:
--
--    Signal: A signal is a kind of message that an object wants to
--    broadcast. All Gtk_Objects can emit signals. These messages are
--    associated to certain events happening during the life of an
--    object. For instance, when a user clicks on a button, the
--    "clicked" signal is emitted by the button.
--
--    Handler (or callback): A handler is a function or procedure that
--    the user "connects" to a signal for a particular object.
--    Connecting a handler to a signal means associating this handler to
--    the signal.  When the signal is emitted, all connected handlers
--    are called back. Usually, the role of those callbacks is to do
--    some processing triggered by a user action. For instance, when
--    "clicked" signal is emitted by the "OK" button of a dialog, the
--    connected handler can be used to close the dialog or recompute
--    some value.
--
--    In GtkAda, the handlers are defined in a form as general as
--    possible. The first argument is always an access to the object it
--    has been connected to. The second object is a table of arguments
--    (See Gtk.Arguments for more details about this table). It is the
--    responsibility of this handler to extract the values from it, and
--    to convert them to the correct Ada type.
--
--    Because such handlers are not very convenient to use, this package
--    also provides some services to connect a marshaller instead. It
--    will then do the extraction work before calling the more
--    programmer-friendly handler, as defined in Gtk.Marshallers (see
--    Gtk.Marshallers for more details).
--
--  The subdivision of this package is identical to Gtk.Marshallers; it
--  is made of four generic sub-packages, each representing one of the
--  four possible kinds of handlers: they can return a value or not, and
--  they can have some user specific data associated to them or not.
--  Selecting the right package depends on the profile of the handler.
--  For example, the handler for the "delete_event" signal of a
--  Gtk_Window has a return value, and has an extra parameter (a Gint).
--  All handlers also have a user_data field by default, but its usage
--  is optional. To connect a handler to this signal, if the user_data
--  field is not used, the Return_Callback generic should be
--  instantiated. On the other hand, if the user_data field is
--  necessary, then the User_Return_Callback generic should be used.
--
--  Note also that the real handler in Gtk+ should expect at least as
--  many arguments as in the marshaller you are using. If your
--  marshaller has one argument, the C handler must have at least one
--  argument too.
--
--  The common generic parameter to all sub-packages is the widget type,
--  which is the basic widget manipulated. This can be
--  Gtk.Object.Gtk_Object_Record type if you want to reduce the number of
--  instantiations, but the conversion to the original type will have to be
--  done inside the handler.
--
--  All sub-packages are organized in the same way.
--
--    First, the type "Handler" is defined. It represents the general
--    form of the callbacks supported by the sub-package.
--
--    The corresponding sub-package of Gtk.Marshallers is instantiated.
--
--    A series of "Connect" procedures and functions is given. All cases
--    are covered: the functions return the Handler_Id of the newly
--    created association, while the procedures just connect the
--    handler, dropping the Handler_Id; some services allow the user to
--    connect a Handler while some others allow the usage of
--    Marshallers, which are more convenient. Note that more than one
--    handler may be connected to a signal; the handlers will then be
--    invoked in the order of connection.
--
--    Some "Connect_Object" services are also provided. Those services
--    never have a user_data. They accept an additional parameter called
--    Slot_Object. When the callback in invoked, the Gtk Object emitting
--    the signal is substituted by this Slot_Object.
--
--    There are several methods to connect a handler. For each method,
--    although the option of connecting a Handler is provided, the
--    recommended way is to use Marshallers. Each connect service is
--    documented below, in the first sub-package.
--
--    A series of "To_Marshaller" functions are provided. They return
--    some marshallers for the most commonly used types in order to ease
--    the usage of this package. Most of the time, it will not be
--    necessary to use some other marshallers.
--
--    As for the "To_Marshaller" functions, a series of "Emit_By_Name"
--    procedures are also provided for the same most common types, to
--    allow the user to easily emit signals. These procedures are mainly
--    intended for people building new Gtk_Objects.
--
--  At the end of this package, some general services related to the
--  management of signals and handlers are also provided. Each one of
--  them is documented individually below.
--
--  IMPORTANT NOTE: These packages must be instantiated at library-level
--
--  </description>

with Gdk.Event;
with Gtk.Arguments;
with Gtk.Marshallers;
with Gtk.Notebook;
with Gtk.Object;
with Gtk.Widget;

package Gtk.Handlers is

   --  <doc_ignore>

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

      --  In all the Connect services below, the following arguments
      --  will be used:
      --    o Widget, Name: This represents the association (Gtk Object,
      --      Signal_Name) to which the handler is to be connected.
      --    o After: If this boolean is set to True, then the handler
      --      will be connected after all the default handlers. By
      --      default, it is set to False.

      procedure Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Marsh   : in     Marshallers.Marshaller;
         After   : in     Boolean := False);
      --  Connects a Marshaller. The Handler_Id is dropped.

      procedure Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Marsh       : in     Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False);
      --  Connects a Marshaller. The Handler_Id is dropped.

      procedure Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Cb      : in     Handler;
         After   : in     Boolean := False);
      --  Connects a Handler. The Handler_Id is dropped.

      procedure Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Cb          : in     Handler;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False);
      --  Connects a Handler. The Handler_Id is dropped.

      pragma Inline (Connect);
      pragma Inline (Object_Connect);

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Marsh   : in     Marshallers.Marshaller;
         After   : in     Boolean := False)
        return Handler_Id;
      --  Connects a Marshaller. Returns the Handler_Id.

      function Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Marsh       : in     Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False)
        return Handler_Id;
      --  Connects a Marshaller. Returns the Handler_Id.

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : in     String;
         Cb      : in     Handler;
         After   : in     Boolean := False)
        return Handler_Id;
      --  Connects a Handler. Returns the Handler_Id.

      function Object_Connect
        (Widget      : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in     String;
         Cb          : in     Handler;
         Slot_Object : access Widget_Type'Class;
         After       : in     Boolean := False)
        return Handler_Id;
      --  Connects a Handler. Returns the Handler_Id.


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

      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String;
                             Param  : in Gint)
                            return Return_Type
                            renames Gint_Marshaller.Emit_By_Name;
      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String;
                             Param  : in Guint)
                            return Return_Type
                            renames Guint_Marshaller.Emit_By_Name;
      function Emit_By_Name is
         new Event_Marshaller.Emit_By_Name_Generic (Gtk.Arguments.To_Address);
      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : in String;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
        return Return_Type
        renames Widget_Marshaller.Emit_By_Name;
      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String)
                            return Return_Type
                            renames Marshallers.Void_Marshaller.Emit_By_Name;
      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String;
                             Param  : in Gtk.Notebook.Gtk_Notebook_Page)
                            return Return_Type
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

      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String;
                             Param  : in Gint)
                            return Return_Type
                            renames Gint_Marshaller.Emit_By_Name;
      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String;
                             Param  : in Guint)
                            return Return_Type
                            renames Guint_Marshaller.Emit_By_Name;
      function Emit_By_Name is
         new Event_Marshaller.Emit_By_Name_Generic (Gtk.Arguments.To_Address);
      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : in String;
         Param  : access Gtk.Widget.Gtk_Widget_Record'Class)
        return Return_Type
        renames Widget_Marshaller.Emit_By_Name;
      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String)
                            return Return_Type
                            renames Marshallers.Void_Marshaller.Emit_By_Name;
      function Emit_By_Name (Object : access Widget_Type'Class;
                             Name   : in String;
                             Param  : in Gtk.Notebook.Gtk_Notebook_Page)
                            return Return_Type
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
   --  Disconnect the handler identified by the given Handler_Id.

   procedure Emit_Stop_By_Name
     (Object : access Gtk.Object.Gtk_Object_Record'Class;
      Name   : in String);
   --  During a signal emission, invoking this procedure will halt the
   --  emission.

   procedure Handler_Block
     (Obj : access Gtk.Object.Gtk_Object_Record'Class;
      Id  : in Handler_Id);
   --  Blocks temporily the signal. For each call to this procedure,
   --  a call to Handler_Unblock must be performed in order to really
   --  unblock the signal.

   procedure Handlers_Destroy
     (Obj : access Object.Gtk_Object_Record'Class);
   --  Destroys all the handlers associated to the given object.

   procedure Handler_Unblock
     (Obj : access Gtk.Object.Gtk_Object_Record'Class;
      Id  : in Handler_Id);
   --  See Handler_Block.

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

   --  </doc_ignore>

end Gtk.Handlers;

--  <example>
--  --  This example connects the "delete_event" signal to a widget.
--  --  The handlers for this signal get an extra argument which is
--  --  the Gdk_Event that generated the signal.
--
--  with Gtk.Handlers;    use Gtk.Handlers;
--  with Gtk.Marshallers; use Gtk.Marshallers;
--
--  function My_Cb (Widget : access Gtk_Widget_Record'Class;
--                  Event  : Gdk.Event.Gdk_Event)
--                  return Gint;
--  --  your own function
--
--  package Return_Widget_Cb is new Gtk.Handlers.Return_Callback
--     (Gtk.Widget.Gtk_Widget_Record, Gint);
--
--  Return_Widget_Cb.Connect (W, "delete_event",
--     Return_Widget_Cb.To_Marshaller (My_Cb'Access));
--
--  </example>

