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

with Gdk;
with Gtk.Object;
with Gtk.Widget;
with Gtk.Tips_Query;
with System;

package Gtk.Signal is

   --  Every Connect function accepts 'null' for the function to call. In
   --  this case, the callback behaves as if you were calling a function
   --  with a null body

   ---------------------------------------------------------------
   --  The following package is for callbacks requiring a data to
   --  be passed to the callback function
   ---------------------------------------------------------------

   generic
      type Base_Type is new Gtk.Object.Gtk_Object_Record with private;

      type Data_Type (<>) is private;
      --  The type of the data for the callback
      --  This type need not be an access type (as opposed as what happens in
      --  C). A new access is created by the connect function.

   package Callback is

      type Callback is access procedure
        (Widget : access Base_Type;
         Data   : in Data_Type);
      --  Callback function for Signal_Connect below
      --  Data is now an 'in' parameter, since you are anyway not
      --  modifying the original data you gave, but a copy of it.

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
        return Guint;

      procedure Emit_By_Name
        (Object : access Base_Type'Class;
         Name   : in String);

   end Callback;

   ---------------------------------------------------------------
   --  The following package is for callbacks requiring two data to
   --  be passed to the callback function
   --  The second data will be given by gtk itself
   ---------------------------------------------------------------

   generic
      type Base_Type is new Gtk.Object.Gtk_Object_Record with private;

      type Data_Type (<>) is private;

      type Cb_Type is new Gdk.Root_Type with private;

   package Two_Callback is

      type Callback is access procedure
        (Widget  : access Base_Type;
         Cb_Data : in Cb_Type;
         Data    : in Data_Type);
      --  Callback function for Signal_Connect below
      --  Data is now an 'in' parameter, since you are anyway not
      --  modifying the original data you gave, but a copy of it.

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
        return Guint;
      --  Intended to be used when Cb_Type is a type from Gdk

      procedure Emit_By_Name
        (Object  : access Base_Type'Class;
         Name    : in String;
         Cb_Data : in Cb_Type);

   end Two_Callback;

   generic
      type Base_Type is new Gtk.Object.Gtk_Object_Record with private;

      type Data_Type (<>) is private;

      type Cb_Type is new Gdk.Root_Type with private;

   package Two_Callback_Gtk is
      type Callback is access procedure
        (Widget  : access Base_Type;
         Cb_Data : access Cb_Type;
         Data    : in Data_Type);

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         Func_Data : in Data_Type;
         After     : in Boolean := False)
         return Guint;
      --  Intended to be used when Cb_Type is a type from Gtk

      procedure Emit_By_Name
        (Object  : access Base_Type'Class;
         Name    : in String;
         Cb_Data : in Cb_Type);

   end Two_Callback_Gtk;

   ------------------------------------------------------------------
   --  The following package should be used in the callbacks for   --
   --  gtk internal functions, such as "draw", "size request",...  --
   --  The second parameter will be set by gtk itself when calling --
   --  your gtk function (no type verification is done!)           --
   --                                                              --
   --  Do not use this package with tagged types, it won't work    --
   ------------------------------------------------------------------

   generic
      type Base_Type is new Gtk.Object.Gtk_Object_Record with private;
      type Simple_Record is private;
   package Record_Callback is
      type Callback is access procedure
        (Widget  : access Base_Type;
         Param   : in out Simple_Record);

      function Connect
        (Obj       : access Base_Type'Class;
         Name      : in String;
         Func      : in Callback;
         After     : in Boolean := False)
        return Guint;

      procedure Emit_By_Name
        (Object  : access Base_Type'Class;
         Name    : in String);

   end Record_Callback;

   -----------------------------------------------------------------
   --  The following functions are for callbacks requiring no data to be
   --  passed to the callback
   -----------------------------------------------------------------

   generic
      type Base_Type is new Gtk.Object.Gtk_Object_Record with private;

   package Void_Callback is

      type Callback is access procedure
        (Widget : access Base_Type);

      function Connect
        (Obj    : access Base_Type'Class;
         Name   : in String;
         Func   : in Callback;
         After  : in Boolean := False)
        return Guint;

      procedure Emit_By_Name
        (Object  : access Base_Type'Class;
         Name    : in String);

   end Void_Callback;

   ------------------------------------------------------------------
   --  The following functions are for callbacks send to another object
   ------------------------------------------------------------------

   generic
      type Base_Type (<>) is tagged private;
      --  This does not need to be a tagged object extending Object

   package Object_Callback is
      type Callback is access procedure
        (Object : access Base_Type);

      function Connect
        (Obj         : access Gtk.Object.Gtk_Object_Record'Class;
         Name        : in String;
         Func        : in Callback;
         Slot_Object : access Base_Type'Class;
         After       : in Boolean := False)
        return Guint;

   end Object_Callback;

   ------------------------------------------------------------------
   --  The following functions are for callbacks for tips query
   ------------------------------------------------------------------

   generic
      type Data_Type is new Object.Gtk_Object_Record with private;

   package Tips_Query_Callback is
      type Callback is access procedure
        (Tips_Query  : access Gtk.Tips_Query.Gtk_Tips_Query_Record;
         Widget      : access Gtk.Widget.Gtk_Widget_Record'Class;
         Tip_Text    : in String;
         Tip_Private : in String;
         Data        : access Data_Type'Class);

      function Connect
        (Obj         : access Gtk.Tips_Query.Gtk_Tips_Query_Record'Class;
         Name        : in String;
         Func        : in Callback;
         Data        : access Data_Type'Class;
         After       : in Boolean := False)
        return Guint;

   end Tips_Query_Callback;

   ----------------------------------------------------------------
   --  The following function for connecting a default C callback
   ----------------------------------------------------------------

   function C_Unsafe_Connect
     (Object      : access Gtk.Object.Gtk_Object_Record'Class;
      Name        : in String;
      Func        : in System.Address;
      Slot_Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Guint;
   --  See testgtk/create_ruler for an example how to use this...
   --  You should avoid using this whenever possible

   ------------------------------------------------------------------
   --  More general functions
   ------------------------------------------------------------------

   procedure Disconnect
     (Object     : access Gtk.Object.Gtk_Object_Record'Class;
      Handler_Id : in Guint);

   procedure Emit_Stop_By_Name
     (Object : access Gtk.Object.Gtk_Object_Record'Class;
      Name   : in String);

   procedure Handler_Block
     (Obj        : access Gtk.Object.Gtk_Object_Record'Class;
      Handler_Id : in Guint);

   procedure Handlers_Destroy
     (Obj : access Object.Gtk_Object_Record'Class);

   procedure Handler_Unblock
     (Obj        : access Gtk.Object.Gtk_Object_Record'Class;
      Handler_Id : in Guint);

end Gtk.Signal;
