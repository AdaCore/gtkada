-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2001 ACT-Europe                 --
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

with Unchecked_Conversion;
with Unchecked_Deallocation;
with Glib.GObjects; use Glib.GObjects;
with Glib.Values; use Glib.Values;
with Gtk.Widget;
with System;

package body Gtk.Handlers is

   function Do_Signal_Connect
     (Object      : Glib.GObjects.GObject;
      Name        : String;
      Marshaller  : System.Address;
      Handler     : System.Address;
      Func_Data   : System.Address;
      Destroy     : System.Address;
      After       : Boolean;
      Slot_Object : System.Address := System.Null_Address) return Handler_Id;
   --  Internal function used to connect the signal.

   type Destroy_Data_Record is record
      Id, Id2, Id3 : Handler_Id;
      Obj1, Obj2   : System.Address;
   end record;

   type Destroy_Data_Access is access Destroy_Data_Record;
   procedure Free is new Unchecked_Deallocation
     (Destroy_Data_Record, Destroy_Data_Access);

   procedure Destroy_Func
     (Object : System.Address;
      Data   : Destroy_Data_Access);
   pragma Convention (C, Destroy_Func);
   --  Handler used to disconnect Object from any signal where it was
   --  connected with Object_Connect and given as the slot object.
   --  Otherwise, a handler could be called with Object as the parameter when
   --  it has been destroyed.

   procedure Disconnect_Internal (Obj : System.Address; Id : Handler_Id);
   pragma Import (C, Disconnect_Internal, "g_signal_handler_disconnect");
   --  Internal version of Disconnect

   function Signal_Lookup (Name : String; IType : GType) return Guint;
   pragma Import (C, Signal_Lookup, "g_signal_lookup");

   procedure Set_Value (Value : GValue; Val : System.Address);
   pragma Import (C, Set_Value, "ada_gvalue_set");
   --  Function used internally to specify the value returned by a callback.

   function To_Address is new Unchecked_Conversion
     (Gtk.Marshallers.General_Handler, System.Address);

   --------------------------------
   -- Glib.Closure small binding --
   --------------------------------

   subtype GClosure is Glib.C_Proxy;

   function CClosure_New
     (Callback  : System.Address;
      User_Data : System.Address;
      Destroy   : System.Address) return GClosure;
   pragma Import (C, CClosure_New, "g_cclosure_new");

   procedure Set_Marshal (Closure : GClosure; Marshaller : System.Address);
   pragma Import (C, Set_Marshal, "g_closure_set_marshal");

   function Get_Data (Closure : GClosure) return System.Address;
   pragma Import (C, Get_Data, "ada_gclosure_get_data");

   ------------------
   -- Destroy_Func --
   ------------------

   procedure Destroy_Func
     (Object : System.Address; Data : Destroy_Data_Access)
   is
      D : Destroy_Data_Access := Data;
   begin
      --  Ensure that this function is only called once by disconnecting
      --  all associated connections to this handler.

      Disconnect_Internal (Data.Obj1, Data.Id);
      Disconnect_Internal (Data.Obj1, Data.Id2);
      Disconnect_Internal (Data.Obj2, Data.Id3);
      Free (D);
   end Destroy_Func;

   -----------------------
   -- Do_Signal_Connect --
   -----------------------

   function Do_Signal_Connect
     (Object      : Glib.GObjects.GObject;
      Name        : String;
      Marshaller  : System.Address;
      Handler     : System.Address;
      Func_Data   : System.Address;
      Destroy     : System.Address;
      After       : Boolean;
      Slot_Object : System.Address := System.Null_Address) return Handler_Id
   is
      function Internal
        (Instance  : System.Address;
         Signal_Id : Guint;
         Detail    : GQuark := Unknown_Quark;
         Closure   : GClosure;
         After     : Gint) return Handler_Id;
      pragma Import (C, Internal, "g_signal_connect_closure_by_id");

      function Get_Type (Object : System.Address) return GType;
      pragma Import (C, Get_Type, "ada_gobject_get_type");

      use type System.Address;
      Id      : Handler_Id;
      Data    : Destroy_Data_Access;
      Closure : GClosure;

   begin
      Closure := CClosure_New (Handler, Func_Data, Destroy);
      Set_Marshal (Closure, Marshaller);
      Id := Internal
        (Get_Object (Object),
         Signal_Id => Signal_Lookup (Name & ASCII.NUL, Get_Type (Object)),
         Closure => Closure, After => Boolean'Pos (After));

      if Slot_Object /= System.Null_Address then
         Data := new Destroy_Data_Record;
         Data.Id := Id;
         Data.Obj1 := Get_Object (Object);
         Data.Obj2 := Slot_Object;

         --  Destroy_Func will remove the following two connections when
         --  called the first time, so that Destroy_Func is only called once.

         Closure := CClosure_New
           (Destroy_Func'Address, Data.all'Address, System.Null_Address);
         Data.Id2 := Internal
           (Instance => Get_Object (Object),
            Signal_Id => Signal_Lookup
                           ("destroy" & ASCII.NUL, Get_Type (Object)),
            Closure => Closure,
            After => 0);

         Closure := CClosure_New
           (Destroy_Func'Address, Data.all'Address, System.Null_Address);
         Data.Id3 := Internal
           (Instance => Slot_Object,
            Signal_Id => Signal_Lookup
                           ("destroy" & ASCII.NUL, Get_Type (Slot_Object)),
            Closure => Closure,
            After => 0);
      end if;

      return Id;
   end Do_Signal_Connect;

   ---------------------
   -- Return_Callback --
   ---------------------

   package body Return_Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type Data_Type_Record is record
         Func     : Handler;
         --  User's callback

         Proxy    : Marshallers.Handler_Proxy := null;
         --  Handler_Proxy to use

         Object   : Acc := null;
         --  Slot Object for Object_Connect
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : String;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Marsh, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Object_Connect (Widget, Name, Marsh, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : String;
         Cb     : Handler;
         After  : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Cb, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Object_Connect (Widget, Name, Cb, Slot_Object, After);
      end Object_Connect;

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Data;

      begin
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         Value  : aliased Return_Type;
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Object = null then
            if Data.Proxy /= null then
               Value :=
                 Data.Proxy
                   (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                    Values, To_General_Handler (Data.Func));
            else
               Value :=
                 Data.Func
                   (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                    Values);
            end if;
         else
            if Data.Proxy /= null then
               Value :=
                 Data.Proxy
                   (Data.Object, Values, To_General_Handler (Data.Func));
            else
               Value := Data.Func (Data.Object, Values);
            end if;
         end if;

         Set_Value (Return_Value, Value'Address);
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : String;
         Marsh   : Marshallers.Marshaller;
         After   : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func     => To_Handler (Marsh.Func),
              Proxy    => Marsh.Proxy,
              Object   => null);

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_None,
            "Handlers for this signal should not return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func     => To_Handler (Marsh.Func),
              Proxy    => Marsh.Proxy,
              Object   => Acc (Slot_Object));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_None,
            "Handlers for this signal should not return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object));
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : String;
         Cb      : Handler;
         After   : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func     => Cb,
              Proxy    => null,
              Object   => null);

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_None,
            "Handlers for this signal should not return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func     => Cb,
              Proxy    => null,
              Object   => Acc (Slot_Object));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_None,
            "Handlers for this signal should not return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object));
      end Object_Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : String;
         Param  : Gdk.Event.Gdk_Event) return Return_Type
      is
         procedure Internal
           (Object : System.Address;
            Name   : String;
            Param  : System.Address;
            Ret    : out Return_Type);
         pragma Import (C, Internal, "g_signal_emit_by_name");

         B : Return_Type;
      begin
         pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param), B);
         return B;
      end Emit_By_Name;

   end Return_Callback;

   --------------------------
   -- User_Return_Callback --
   --------------------------

   package body User_Return_Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type User_Access is access User_Type;
      type Data_Type_Record is record
         Func  : Handler;
         --  User's callback

         Proxy : Marshallers.Handler_Proxy := null;
         --  Handler_Proxy to use

         User  : User_Access := null;
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (User_Type, User_Access);
         D : Data_Type_Access := Data;

      begin
         Internal2 (D.User);
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         Value  : aliased Return_Type;
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Proxy /= null then
            Value :=
              Data.Proxy
                (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                 Values, To_General_Handler (Data.Func), Data.User.all);
         else
            Value :=
              Data.Func
                (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                 Values, Data.User.all);
         end if;

         Set_Value (Return_Value, Value'Address);
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Marsh, User_Data, After);
      end Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Cb, User_Data, After);
      end Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
        return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func     => To_Handler (Marsh.Func),
              Proxy    => Marsh.Proxy,
              User     => new User_Type'(User_Data));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_None,
            "Handlers for this signal should not return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func     => Cb,
              Proxy    => null,
              User     => new User_Type'(User_Data));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_None,
            "Handlers for this signal should not return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      function Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : String;
         Param  : Gdk.Event.Gdk_Event) return Return_Type
      is
         procedure Internal
           (Object : System.Address;
            Name   : String;
            Param  : System.Address;
            Ret    : out Return_Type);
         pragma Import (C, Internal, "g_signal_emit_by_name");

         B : Return_Type;

      begin
         pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param), B);
         return B;
      end Emit_By_Name;

   end User_Return_Callback;

   --------------
   -- Callback --
   --------------

   package body Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type Data_Type_Record is record
         Func   : Handler;             --  User's callback
         Proxy  : Marshallers.Handler_Proxy := null;  --  Handler_Proxy to use
         Object : Acc := null;         --  Slot Object for Object_Connect
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         D : Data_Type_Access := Data;

      begin
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Object = null then
            if Data.Proxy /= null then
               Data.Proxy
                 (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                  Values, To_General_Handler (Data.Func));
            else
               Data.Func
                 (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
                  Values);
            end if;
         else
            if Data.Proxy /= null then
               Data.Proxy
                 (Data.Object, Values, To_General_Handler (Data.Func));
            else
               Data.Func (Data.Object, Values);
            end if;
         end if;
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : String;
         Marsh  : Marshallers.Marshaller;
         After  : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Marsh, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Object_Connect (Widget, Name, Marsh, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget : access Widget_Type'Class;
         Name   : String;
         Cb     : Handler;
         After  : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Cb, After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      procedure Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Object_Connect (Widget, Name, Cb, Slot_Object, After);
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : String;
         Marsh   : Marshallers.Marshaller;
         After   : Boolean := False)
        return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func  => To_Handler (Marsh.Func),
              Proxy => Marsh.Proxy,
              Object => null);

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) = GType_None,
            "Handlers for this signal should return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Marsh       : Marshallers.Marshaller;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func  => To_Handler (Marsh.Func),
              Proxy => Marsh.Proxy,
              Object => Acc (Slot_Object));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) = GType_None,
            "Handlers for this signal should return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object));
      end Object_Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget  : access Widget_Type'Class;
         Name    : String;
         Cb      : Handler;
         After   : Boolean := False)
        return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record' (Func => Cb, Proxy => null, Object => null);

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) = GType_None,
            "Handlers for this signal should return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      --------------------
      -- Object_Connect --
      --------------------

      function Object_Connect
        (Widget      : access Glib.GObjects.GObject_Record'Class;
         Name        : String;
         Cb          : Handler;
         Slot_Object : access Widget_Type'Class;
         After       : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func   => Cb,
              Proxy  => null,
              Object => Acc (Slot_Object));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) = GType_None,
            "Handlers for this signal should return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After,
            Get_Object (Slot_Object));
      end Object_Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : String;
         Param  : Gdk.Event.Gdk_Event)
      is
         procedure Internal
           (Object : System.Address;
            Name   : String;
            Param  : System.Address);
         pragma Import (C, Internal, "g_signal_emit_by_name");
      begin
         pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param));
      end Emit_By_Name;

   end Callback;

   -------------------
   -- User_Callback --
   -------------------

   package body User_Callback is

      function To_Handler is new Unchecked_Conversion
        (Gtk.Marshallers.General_Handler, Handler);
      function To_General_Handler is new Unchecked_Conversion
        (Handler, Gtk.Marshallers.General_Handler);
      function To_Address is new Unchecked_Conversion
        (Handler, System.Address);
      function To_Address is new Unchecked_Conversion
        (Marshallers.Handler_Proxy, System.Address);

      type Acc is access all Widget_Type'Class;
      --  This type has to be declared at library level, otherwise
      --  Program_Error might be raised when trying to cast from the
      --  parameter of Marshaller to another type.

      type User_Access is access User_Type;
      type Data_Type_Record is record
         Func   : Handler;
         --  User's callback

         Proxy  : Marshallers.Handler_Proxy := null;
         --  Handler_Proxy to use

         User   : User_Access := null;
      end record;
      type Data_Type_Access is access all Data_Type_Record;
      pragma Convention (C, Data_Type_Access);
      --  Data passed to the C handler

      function Convert is new Unchecked_Conversion
        (Data_Type_Access, System.Address);
      function Convert is new Unchecked_Conversion
        (System.Address, Data_Type_Access);

      procedure Free_Data (Data : Data_Type_Access);
      pragma Convention (C, Free_Data);
      --  Free the memory associated with the callback's data

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address);
      pragma Convention (C, First_Marshaller);
      --  First level marshaller. This is the function that is actually
      --  called by gtk+. It then calls the Ada functions as required.

      ---------------
      -- Free_Data --
      ---------------

      procedure Free_Data (Data : Data_Type_Access) is
         procedure Internal is new Unchecked_Deallocation
           (Data_Type_Record, Data_Type_Access);
         procedure Internal2 is new Unchecked_Deallocation
           (User_Type, User_Access);
         D : Data_Type_Access := Data;
      begin
         Internal2 (D.User);
         Internal (D);
      end Free_Data;

      ----------------------
      -- First_Marshaller --
      ----------------------

      procedure First_Marshaller
        (Closure         : GClosure;
         Return_Value    : GValue;
         N_Params        : Guint;
         Params          : System.Address;
         Invocation_Hint : System.Address;
         User_Data       : System.Address)
      is
         use type Marshallers.Handler_Proxy;

         Data   : constant Data_Type_Access := Convert (Get_Data (Closure));
         Stub   : Widget_Type;
         Values : GValues;

      begin
         if Data.Func = null then
            return;
         end if;

         Values := Make_Values (N_Params, Params);

         if Data.Proxy /= null then
            Data.Proxy
              (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
               Values, To_General_Handler (Data.Func), Data.User.all);
         else
            Data.Func
              (Acc (Get_User_Data (Get_Address (Nth (Values, 0)), Stub)),
               Values, Data.User.all);
         end if;
      end First_Marshaller;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Marsh, User_Data, After);
      end Connect;

      -------------
      -- Connect --
      -------------

      procedure Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False)
      is
         Id : Handler_Id;
      begin
         Id := Connect (Widget, Name, Cb, User_Data, After);
      end Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Marsh     : Marshallers.Marshaller;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func  => To_Handler (Marsh.Func),
              Proxy => Marsh.Proxy,
              User  => new User_Type'(User_Data));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) = GType_None,
            "Handlers for this signal should return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Marsh.Proxy),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      -------------
      -- Connect --
      -------------

      function Connect
        (Widget    : access Widget_Type'Class;
         Name      : String;
         Cb        : Handler;
         User_Data : User_Type;
         After     : Boolean := False) return Handler_Id
      is
         D : Data_Type_Access :=
           new Data_Type_Record'
             (Func => Cb, Proxy => null, User => new User_Type'(User_Data));

      begin
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) /= GType_Invalid,
            "Invalid signal for this widget");
         pragma Assert
           (Argument_Type (Get_Type (Widget), Name, -1) = GType_None,
            "Handlers for this signal should return a value.");

         return Do_Signal_Connect
           (Glib.GObjects.GObject (Widget),
            Name,
            First_Marshaller'Address,
            To_Address (Cb),
            Convert (D),
            Free_Data'Address,
            After);
      end Connect;

      ------------------
      -- Emit_By_Name --
      ------------------

      procedure Emit_By_Name
        (Object : access Widget_Type'Class;
         Name   : String;
         Param  : Gdk.Event.Gdk_Event)
      is
         procedure Internal
           (Object : System.Address;
            Name   : String;
            Param  : System.Address);
         pragma Import (C, Internal, "g_signal_emit_by_name");

      begin
         pragma Assert (Count_Arguments (Get_Type (Object), Name) = 1);
         Internal
           (Get_Object (Object), Name & ASCII.NUL,
            Gdk.Event.To_Address (Param));
      end Emit_By_Name;

   end User_Callback;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect
     (Object : access Glib.GObjects.GObject_Record'Class;
      Id     : Handler_Id) is
   begin
      Disconnect_Internal (Obj => Get_Object (Object), Id  => Id);
   end Disconnect;

   -----------------------
   -- Emit_Stop_By_Name --
   -----------------------

   procedure Emit_Stop_By_Name
     (Object : access Glib.GObjects.GObject_Record'Class;
      Name   : String)
   is
      procedure Internal
        (Object : System.Address;
         Signal_Id : Guint;
         Detail : GQuark := Unknown_Quark);
      pragma Import (C, Internal, "g_signal_stop_emission");
   begin
      Internal
        (Get_Object (Object),
         Signal_Lookup (Name & ASCII.NUL, Get_Type (Object)));
   end Emit_Stop_By_Name;

   -------------------
   -- Handler_Block --
   -------------------

   procedure Handler_Block
     (Obj : access Glib.GObjects.GObject_Record'Class;
      Id  : Handler_Id)
   is
      procedure Internal (Obj : System.Address; Id : Handler_Id);
      pragma Import (C, Internal, "g_signal_handler_block");

   begin
      Internal (Obj => Get_Object (Obj), Id  => Id);
   end Handler_Block;

   ----------------------
   -- Handlers_Destroy --
   ----------------------

   procedure Handlers_Destroy (Obj : access Glib.GObjects.GObject_Record'Class)
   is
      procedure Internal (Obj : System.Address);
      pragma Import (C, Internal, "g_signal_handlers_destroy");

   begin
      Internal (Obj => Get_Object (Obj));
   end Handlers_Destroy;

   ---------------------
   -- Handler_Unblock --
   ---------------------

   procedure Handler_Unblock
     (Obj : access Glib.GObjects.GObject_Record'Class;
      Id  : Handler_Id)
   is
      procedure Internal (Obj : System.Address; Id : Handler_Id);
      pragma Import (C, Internal, "g_signal_handler_unblock");
   begin
      Internal (Obj => Get_Object (Obj), Id  => Id);
   end Handler_Unblock;

end Gtk.Handlers;

--  Design of the package:
--
--  The callback package should accomodate the following:
--  * Sometimes, the user does not want to get the handler_id.  Thus, we give
--    the two versions of Connect, either as a procedure or a function.
--  * Some gtk+ signals expect a handler that returns a value, whereas others
--    return nothing. The simplest here is to have two kinds of packages, one
--    for each case.  The standard gtk+ signals (up to 1.2) have the following
--    possible return values: gboolean, gchar*, gint, guint, void
--  * The first argument to a handler is always a Gtk Object.  since GtkAda is
--    full object oriented, we could simply have the first parameter be a
--    GObject'Class, but this would require in most cases an explicit cast
--    from the user. It is nice to give the opportunity to have a specific
--    type.
--  * The standard callbacks can have any number of arguments (or even
--    none). These arguments depend on the signal type, and the widget type.
--    The only possibility to cover all the cases is to pass these arguments as
--    an array. The package Marshallers provides a convenient interface between
--    these general callbacks and a more usual implementation.
--  * The user can specify some Data to be passed to the handler.  Or there
--    can be none. Once again, we create two versions of the standard packages.
--
--  How the callbacks work:
--     --------      --------------------      ---------------------
--     | Gtk+ |  ->  | First_Marshaller |  ->  | User's Marshaller |
--     --------      --------------------      ---------------------
--                                   \                 |
--                                    \  or            V
--                                     \       -------------------
--                                      \--->  | User's Callback |
--                                             -------------------
--
--  First_Marshaller is an internal function defined in this package. It
--  creates its own user data to memorize both the User's Marshaller (if any),
--  User's Callback, and the User's Data (if any). It then calls the
--  appropriate function in the user's code.
--  The user_data of First_Marshaller is automatically deallocated when the
--  callback is disconnected or the widget is killed, using some capabilities
--  of gtk+ itself.
