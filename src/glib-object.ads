----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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
--  This package provides a minimal binding to the GObject type in Glib.
--  See Glib.Properties for information on how to manipulate properties
--
--  </description>

with Gtkada.Types;

package Glib.Object is

   type GObject_Record is tagged private;
   type GObject is access all GObject_Record'Class;
   --  The base type for Glib/Gdk/Gtk objects. It basically gives access
   --  to an underlying C object. This is not a controlled type for
   --  efficiency reasons and because glib takes care of the memory
   --  management on its own.

   function Is_Created (Object : in GObject_Record'Class) return Boolean;
   --  Return True if the associated C object has been created, False if
   --  no C object is associated with Object.
   --  This is not the same as testing whether an access type (for instance
   --  any of the widgets) is "null", since this relates to the underlying
   --  C object.

   function Get_Type (Object : access GObject_Record) return GType;
   --  Return the type of Object.
   --  This function is mostly used internally, since in Ada you can simply
   --  test whether an object belong to a class with a statement like:
   --
   --     if Object in Gtk_Button_Record'Class then ...
   --
   --  which is easier.

   procedure G_New (Object : out GObject);
   --  Create a new GObject.
   --  This is only required when you want to create an Ada tagged type to
   --  which you can attach new signals. Most of the time, you only need to
   --  directly create the appropriate Gtk Widget by calling the correct
   --  Gtk_New procedure.

   procedure Initialize (Object : access GObject_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Ref (Object : access GObject_Record);
   --  Increment the reference counter for Object. See Unref below.
   --  Since an object is not deleted while its reference count is not null,
   --  this is a way to keep an object in memory, in particular when you
   --  want to temporarily remove a widget from its parent.

   procedure Unref (Object : access GObject_Record);
   --  Decrement the reference counter for Object. When this reaches 0, the
   --  object is effectively destroy, all the callbacks associated with it are
   --  disconnected.

   ------------------------
   -- Interfacing with C --
   ------------------------
   --  The following functions are made public so that one can easily create
   --  new objects outside the Glib or Gtk package hierarchy.
   --  Only experienced users should make use of these functions.

   function Get_Object
     (Object : access GObject_Record'Class) return System.Address;
   --  Access the underlying C pointer.

   procedure Set_Object
     (Object : access GObject_Record'Class;
      Value  : System.Address);
   --  Modify the underlying C pointer.

   procedure Initialize_User_Data (Obj : access GObject_Record'Class);
   --  Sets a user data field for the C object associated with Obj.
   --  This field will be used so that it is possible, knowing a
   --  C object, to get the full ada object.

   function Get_User_Data
     (Obj  : in System.Address;
      Stub : in GObject_Record'Class) return GObject;
   --  Get the user data that was set by GtkAda.
   --  If the Data is not set, return a new access type, that points to
   --  a structure with the same tag as Stub.

   function Unchecked_Cast
     (Obj  : access GObject_Record'Class;
      Stub : GObject_Record'Class) return GObject;
   --  Cast Obj in an object of tag Stub'Class.
   --  Return the resulting object and free the memory pointed by Obj.

   -------------
   -- Signals --
   -------------
   --  Any child of GObject can be associated with any number of signals. The
   --  mechanism for signals is fully generic, and any number of arguments can
   --  be associated with signals.
   --  See the function Initialize_Class_Record for more information on how
   --  to create new signals for your own new widgets.
   --  The subprograms below are provided for introspection: they make it
   --  possible to query the list of signals defined for a specific widget,
   --  as well as their parameters and return types.

   type Signal_Id_Array is array (Guint range <>) of Glib.Signal_Id;

   type Signal_Query is private;

   function Lookup
     (Object : Glib.GType; Signal : String) return Glib.Signal_Id;
   --  Returns the signal Id associated with a specific Object/Signal pair.
   --  Invalid_Signal_Id is returned if no such signal exists for Object.
   --  You can then use the Query procedure to get more information on the
   --  signal.

   function List_Ids (Typ : Glib.GType) return Signal_Id_Array;
   --  Return the list of signals defined for Typ. You can get more information
   --  on each of this signals by using the Query function below.
   --  See also the function Get_Type above to convert from an object instance
   --  to its type. Using a GType as the parameter makes it easier to find the
   --  signals for a widget and its ancestors (using Glib.Parent).

   procedure Query (Id : Glib.Signal_Id; Result : out Signal_Query);
   --  Return the description associated with the signal Id. You can get the
   --  various fields from Query with one of the functions below.
   --  Result is undefined if Id is Invalid_Signal_Id

   function Id (Q : Signal_Query) return Glib.Signal_Id;
   --  Return the signal Id. Each Id is specific to a widget/signal name pair.
   --  These Ids can then be used to temporarily block a signal for instance,
   --  through the subprograms in Gtk.Handlers.

   function Signal_Name (Q : Signal_Query) return String;
   --  Return the name of the signal, as should be used in a call to Connect.

   function Return_Type (Q : Signal_Query) return Glib.GType;
   --  Return the type of object returned by the handlers for this signal.

   function Params (Q : Signal_Query) return GType_Array;
   --  Return the list of parameters for the handlers for this signal

   --------------------------
   -- Creating new widgets --
   --------------------------
   --  These types and functions are used only when creating new widget types
   --  directly in Ada. These functions initialize the classes so that they are
   --  correctly recognized by gtk+ itself
   --  See the GtkAda user's guide for more information on how to create your
   --  own widget types in Ada.

   type GObject_Class is private;
   Uninitialized_Class : constant GObject_Class;
   --  This type encloses all the informations related to a specific type of
   --  object or widget. All instances of such an object have a pointer to this
   --  structure, that includes the definition of all the signals that exist
   --  for a given object, all its properties,...

   type Signal_Parameter_Types is
     array (Natural range <>, Natural range <>) of GType;
   --  The description of the parameters for each event.
   --  Each event defined with Initialize_Class_Record below should have an
   --  entry in this table. If Gtk_Type_None is found in the table, it is
   --  ignored. For instance, a Signal_Parameter_Type like:
   --    (1 => (1 => Gdk_Type_Gdk_Event, 2 => GType_None),
   --     2 => (1 => GType_Int,          2 => GType_Int));
   --  defines two signals, the first with a single Gdk_Event parameter, the
   --  second with two ints parameters.

   Null_Parameter_Types : constant Signal_Parameter_Types (1 .. 0, 1 .. 0) :=
     (others => (others => GType_None));
   --  An empty array, used as a default parameter in Initialize_Class_Record.

   procedure Initialize_Class_Record
     (Object       : access GObject_Record'Class;
      Signals      : Gtkada.Types.Chars_Ptr_Array;
      Class_Record : in out GObject_Class;
      Type_Name    : String;
      Parameters   : Signal_Parameter_Types := Null_Parameter_Types);
   --  Create the class record for a new object type.
   --  It is associated with Signals'Length new signals. A pointer to the
   --  newly created structure is also returned in Class_Record.
   --  If Class_Record /= System.Null_Address, no memory allocation is
   --  performed, we just reuse it.
   --  Note: The underlying C object must already have been initialized
   --  by a call to its parent's Initialize function.
   --  Parameters'Length should be the same as Signals'Length, or the result
   --  is undefined.
   --  As a special case, if Parameters has its default value, all signals are
   --  created with no argument. This is done for backward compatibility
   --  mainly, and you should instead give it an explicit value.
   --  Type_Name should be a unique name identifying the name of the new type.
   --
   --  Only the signals with no parameter can be connected from C. However,
   --  any signal can be connected from Ada. This is due to the way we define
   --  default marshallers for the signals.

   function Type_From_Class (Class_Record : GObject_Class) return GType;
   --  Return the internal gtk+ type that describes the newly created
   --  Class_Record

   function Class_From_Type (Typ : GType) return GObject_Class;
   --  Return the class record for a specific type

   -------------
   -- Signals --
   -------------
   --  ??? This section is incomplete.

   --  <signals>
   --  The following new signals are defined for this object:
   --
   --  - "notify"
   --    procedure Handler
   --      (Object : access GObject_Record'Class; Name : String);
   --
   --    Emitted when the property Name has been modified
   --  </signals>

   procedure Notify
     (Object        : access GObject_Record;
      Property_Name : String);
   --  Emits the "notify" signal, to signal every listener that the property
   --  has been changed.

   ---------------
   -- User_Data --
   ---------------
   --  This package allow you to associate your own Data to the C widgets. No
   --  type verification is made to check if you are using the correct
   --  matching Get function. This is your own responsability.
   --
   --  We recommend using this package only if you want your data to be
   --  available from your own C code. If you just want to access it from Ada,
   --  you should consider creating a new tagged type instead.

   --  <doc_ignore>

   generic
      type Data_Type (<>) is private;
   package User_Data is
      function Get
        (Object : access GObject_Record'Class;
         Id     : String := "user_data") return Data_Type;
      --  Get the information associated with the key ID.
      --  Raise Gtkada.Types.Data_Error if there is none.

      procedure Set
        (Object : access GObject_Record'Class;
         Data   : Data_Type;
         Id     : String := "user_data");
      --  Associate some new user data with the object.
      --  The strings starting with "gtkada_" are reserved for GtkAda's
      --  internal use, please avoid using them.

      procedure Remove
        (Object : access GObject_Record'Class; Id : String := "user_data");
      --  Remove some data from the object

      function Get
        (Object : access GObject_Record'Class;
         Id     : Glib.GQuark) return Data_Type;
      --  Same function as Get above, but uses directly the Quark associated
      --  with the string, which speeds up the access time significantly.

      procedure Set
        (Object : access GObject_Record'Class;
         Data   : Data_Type;
         Id     : Glib.GQuark);
      --  Same function as Set above, but uses directly the Quark associated
      --  with the string, which speeds up the access time significantly.

      procedure Remove
        (Object : access GObject_Record'Class; Id : Glib.GQuark);
      --  Same function as Remove above, but uses directly the Quark associated
      --  with the string, which speeds up the access time significantly.
   end User_Data;

   --  </doc_ignore>

private

   type GObject_Record is tagged record
      Ptr : System.Address := System.Null_Address;
   end record;

   type GObject_Class is new System.Address;
   Uninitialized_Class : constant GObject_Class :=
     GObject_Class (System.Null_Address);

   type Byte is range 0 .. 255;
   for Byte'Size use 8;
   function C_Signal_Query_Size return Natural;
   pragma Import (C, C_Signal_Query_Size);
   type Signal_Query is array (1 .. C_Signal_Query_Size) of Byte;

   --  <doc_ignore>

   --  Note: the following functions and types should only be used
   --  for internal usage, not in the user's applications.
   --  If you use type inheritance for new widgets, you should not need
   --  these functions.

   GtkAda_String : constant String := "_GtkAda" & ASCII.NUL;
   GtkAda_String_Quark : Glib.GQuark := Glib.Unknown_Quark;
   --  The name for the user data that we set in the objects.
   --  The Quark version is to speed up the string lookup (this is done
   --  only once).

   function Conversion_Function
     (Obj : System.Address; Stub : GObject_Record'Class)
      return GObject;
   --  This function has to convert a C object to an Ada object.
   --  It will first try all the registered functions (in
   --  Glib.Type_Conversion_Hooks) and by default, will create a Stub'Class
   --  object, no matter what the real C type is.
   --  Stub is the expected type.

   --  </doc_ignore>

   pragma Inline (Get_Object);
   pragma Inline (Set_Object);
   pragma Import (C, Type_From_Class, "ada_type_from_class");
   pragma Import (C, Query, "g_signal_query");
   pragma Import (C, Id, "ada_gsignal_query_id");
   pragma Import (C, Return_Type, "ada_gsignal_query_return_type");
   pragma Import (C, Class_From_Type, "gtk_type_class");
end Glib.Object;
