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

with Gtk.Object;
with Gtk.Arguments;

--  This package simplifies the use of the callbacks. It has to be used
--  in conjonction with Gtk.Handlers.
--  The idea is that instead of having a general callback that gets its
--  argument in an array, we substitute a Marshaller, that will convert
--  the array into a series of standard parameters to a function.
--
--  This package provides a series of such marshallers. They will cover
--  the most usual cases.
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
--  Each of the generic packages here provides one main function, called
--  To_Marshaller. This returns a record that is a connection between a
--  given marshaller and a user callback.
--
--  Some cases are not covered by this package. This is most notably the case
--  when the callback has more than one extra parameter.
--  In that case, you have two choices: either you use the standard
--  callback mechanism and parse the arguments yourself, or create a new
--  Marshaller package. The second solution is more interesting if you want
--  to have multiple callbacks following the same model.
--  Have a look at the body of this package to find how to write new
--  Marshallers.
--
--  There are four generic packages here, the same organization as in
--  Gtk.Handlers (i.e there is one for callbacks returning values, but with no
--  user data, for callbacks returning values and with a user callbacks, for
--  callbacks not returning any value and with no user data, and for callbacks
--  returning values and with a user data.

--  The packages themselves contain three generic subpackages. The first one,
--  always called "Generic_Marshaller" is for the cases when the extra
--  argument is a simple non-tagged type. The second one
--  "Generic_Widget_Marshaller" is used when the extra parameter is a tagged
--  type, for instance one of the GtkAda widgets.
--  The third subpackage is for the cases when there is no extra parameter.
--
--  The organization of these generic subpackages is always the same:
--  * The type "Marshaller" is defined.
--    This is the general format of the marshaller covered in this generic
--    package.
--  * The type "Generic_Callback" is used to indicate an association between
--    a handler and a marshaller.
--  * The function "To_Marshaller" is the only one that you have to use. It
--    returns an structure that you can pass directly to the Connect functions
--    in Gtk.Handlers. The argument is the callback you want to call.
--  * "Emit_By_Name" is used to emit a signal with the matching profile. You
--    have to give all the expected parameters.

package Gtk.Marshallers is

   type General_Handler is access procedure;
   --  A general access type for a handler.

   --------------------------------------------------------------
   --  Return Marshallers: Return a value, don't have user data
   --------------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
      type Return_Type is private;
   package Return_Marshallers is

      type Marshaller is access function (Widget  : access Widget_Type'Class;
                                          Params  : Gtk.Arguments.Gtk_Args;
                                          Cb      : General_Handler)
                                         return Return_Type;

      type Connection is record
         Func  : General_Handler;   --  User callback
         Marsh : Marshaller;        --  Marshaller for this callback
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion (Args : Gtk.Arguments.Gtk_Args;
                                   Num : Positive)
                                  return Base_Type;
      package Generic_Marshaller is
         type Handler is access function (Widget : access Widget_Type'Class;
                                          Param  : Base_Type)
                                         return Return_Type;
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object : access Widget_Type'Class;
                                 Name   : in String;
                                 Param  : Base_Type);
      private
         function Call (Widget  : access Widget_Type'Class;
                        Params  : Gtk.Arguments.Gtk_Args;
                        Cb      : General_Handler)
                       return Return_Type;
      end Generic_Marshaller;

      --  Widget Marshaller
      generic
         type Base_Type   is new Gtk.Object.Gtk_Object_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access function (Widget : access Widget_Type'Class;
                                          Param  : access Base_Type'Class)
                                         return Return_Type;
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object : access Widget_Type'Class;
                                 Name   : in String;
                                 Param  : access Base_Type'Class);
      private
         function Call (Widget  : access Widget_Type'Class;
                        Params  : Gtk.Arguments.Gtk_Args;
                        Cb      : General_Handler)
                       return Return_Type;
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access function (Widget : access Widget_Type'Class)
                                         return Return_Type;
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object : access Widget_Type'Class;
                                 Name   : in String);
      private
         function Call (Widget  : access Widget_Type'Class;
                        Params  : Gtk.Arguments.Gtk_Args;
                        Cb      : General_Handler)
                       return Return_Type;
      end Void_Marshaller;
   end Return_Marshallers;

   --------------------------------------------------------------
   --  User_Return_Marshallers: Return a value, have a user data
   --------------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
      type Return_Type is private;
      type User_Type (<>) is private;
   package User_Return_Marshallers is

      type Marshaller is access function (Widget    : access Widget_Type'Class;
                                          Params    : Gtk.Arguments.Gtk_Args;
                                          Cb        : General_Handler;
                                          User_Data : User_Type)
                                         return Return_Type;

      type Connection is record
         Func  : General_Handler;
         Marsh : Marshaller;
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion (Args : Gtk.Arguments.Gtk_Args;
                                   Num : Positive)
                                  return Base_Type;
      package Generic_Marshaller is
         type Handler is access function (Widget : access Widget_Type'Class;
                                          Param  : Base_Type;
                                          User_Data : User_Type)
                                         return Return_Type;
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object    : access Widget_Type'Class;
                                 Name      : in String;
                                 Param     : Base_Type);
      private
         function Call (Widget    : access Widget_Type'Class;
                        Params    : Gtk.Arguments.Gtk_Args;
                        Cb        : General_Handler;
                        User_Data : User_Type)
                       return Return_Type;
      end Generic_Marshaller;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Object.Gtk_Object_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access function (Widget    : access Widget_Type'Class;
                                          Param     : access Base_Type'Class;
                                          User_Data : User_Type)
                                         return Return_Type;
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object    : access Widget_Type'Class;
                                 Name      : in String;
                                 Param     : access Base_Type'Class);
      private
         function Call (Widget    : access Widget_Type'Class;
                        Params    : Gtk.Arguments.Gtk_Args;
                        Cb        : General_Handler;
                        User_Data : User_Type)
                       return Return_Type;
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access function (Widget    : access Widget_Type'Class;
                                          User_Data : User_Type)
                                         return Return_Type;
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object    : access Widget_Type'Class;
                                 Name      : in String);
      private
         function Call (Widget    : access Widget_Type'Class;
                        Params    : Gtk.Arguments.Gtk_Args;
                        Cb        : General_Handler;
                        User_Data : User_Type)
                       return Return_Type;
      end Void_Marshaller;

   end User_Return_Marshallers;

   -----------------
   --  Callback_Marshallers: Do not return a value, no user data
   -----------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
   package Void_Marshallers is

      type Marshaller is access procedure (Widget  : access Widget_Type'Class;
                                           Params  : Gtk.Arguments.Gtk_Args;
                                           Cb      : General_Handler);

      type Connection is record
         Func  : General_Handler;
         Marsh : Marshaller;
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion (Args : Gtk.Arguments.Gtk_Args;
                                   Num : Positive)
                                  return Base_Type;
      package Generic_Marshaller is
         type Handler is access procedure (Widget : access Widget_Type'Class;
                                           Param  : Base_Type);
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object : access Widget_Type'Class;
                                 Name   : in String;
                                 Param  : Base_Type);
      private
         procedure Call (Widget  : access Widget_Type'Class;
                         Params  : Gtk.Arguments.Gtk_Args;
                         Cb      : General_Handler);
      end Generic_Marshaller;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Object.Gtk_Object_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access procedure (Widget : access Widget_Type'Class;
                                           Param  : access Base_Type'Class);
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object : access Widget_Type'Class;
                                 Name   : in String;
                                 Param  : access Base_Type'Class);
      private
         procedure Call (Widget  : access Widget_Type'Class;
                         Params  : Gtk.Arguments.Gtk_Args;
                         Cb      : General_Handler);
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access procedure (Widget : access Widget_Type'Class);
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object : access Widget_Type'Class;
                                 Name   : in String);
      private
         procedure Call (Widget  : access Widget_Type'Class;
                         Params  : Gtk.Arguments.Gtk_Args;
                         Cb      : General_Handler);
      end Void_Marshaller;

   end Void_Marshallers;


   ----------------------------------------------------------------------
   --  User_Callback_Marshallers: Do not return a value, have user data
   ----------------------------------------------------------------------

   generic
      type Widget_Type is new Gtk.Object.Gtk_Object_Record with private;
      type User_Type (<>) is private;
   package User_Void_Marshallers is
      type Marshaller is access procedure (Widget   : access Widget_Type'Class;
                                           Params   : Gtk.Arguments.Gtk_Args;
                                           Cb       : General_Handler;
                                           User_Data : User_Type);

      type Connection is record
         Func  : General_Handler;
         Marsh : Marshaller;
      end record;

      --  Basic Marshaller
      generic
         type Base_Type is private;
         with function Conversion (Args : Gtk.Arguments.Gtk_Args;
                                   Num : Positive)
                                  return Base_Type;
      package Generic_Marshaller is
         type Handler is access procedure (Widget : access Widget_Type'Class;
                                           Param  : Base_Type;
                                           User_Data : User_Type);
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object    : access Widget_Type'Class;
                                 Name      : in String;
                                 Param     : Base_Type);
      private
         procedure Call (Widget    : access Widget_Type'Class;
                         Params    : Gtk.Arguments.Gtk_Args;
                         Cb        : General_Handler;
                         User_Data : User_Type);
      end Generic_Marshaller;

      --  Widget Marshaller
      generic
         type Base_Type is new Gtk.Object.Gtk_Object_Record with private;
         type Access_Type is access all Base_Type'Class;
      package Generic_Widget_Marshaller is
         type Handler is access procedure (Widget   : access Widget_Type'Class;
                                           Param     : access Base_Type'Class;
                                           User_Data : User_Type);
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object    : access Widget_Type'Class;
                                 Name      : in String;
                                 Param     : access Base_Type'Class);
      private
         procedure Call (Widget    : access Widget_Type'Class;
                         Params    : Gtk.Arguments.Gtk_Args;
                         Cb        : General_Handler;
                         User_Data : User_Type);
      end Generic_Widget_Marshaller;

      --  Void Marshaller
      package Void_Marshaller is
         type Handler is access procedure (Widget   : access Widget_Type'Class;
                                           User_Data : User_Type);
         function To_Marshaller (Cb : Handler) return Connection;
         procedure Emit_By_Name (Object    : access Widget_Type'Class;
                                 Name      : in String);
      private
         procedure Call (Widget    : access Widget_Type'Class;
                         Params    : Gtk.Arguments.Gtk_Args;
                         Cb        : General_Handler;
                         User_Data : User_Type);
      end Void_Marshaller;

   end User_Void_Marshallers;

end Gtk.Marshallers;
