-----------------------------------------------------------------------
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
--
--  </description>

with Gtkada.Types;

package Glib.GObjects is

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

   function Type_Name (Type_Num : in GType) return String;
   --  Return the type name corresponding to a GType.
   --  This might be useful in debug messages.

   function Type_From_Name (Name : in String) return GType;
   --  Convert a string to the matching type.
   --  Name should be the C GObject name rather than the Ada name: thus,
   --  use names such as GtkScrollbar or GtkButton for widgets.

   ------------------------
   -- Interfacing with C --
   ------------------------
   --  The following functions are made public so that one can easily create
   --  new objects outside the Glib or Gtk package hierarchy.
   --  Only experienced users should make use of these functions.

   function Get_Object (Object : access GObject_Record'Class)
                        return System.Address;
   --  Access the underlying C pointer.

   procedure Set_Object
     (Object : access GObject_Record'Class;
      Value  : in     System.Address);
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

   function Count_Arguments
     (The_Type : GType; Name : in String) return Guint;
   --  Return the number of arguments used in the handlers for the signal.
   --  Note that in the Connect functions, we always test whether the user
   --  has asked for *at most* the number of arguments defined by gtk+ for the
   --  callback. This is because having less argument is authorized (the
   --  extra parameters passed by glib will simply be ignored), whereas having
   --  more arguments is impossible (they would never be set).

   function Argument_Type
     (The_Type : GType;
      Name     : in String;
      Num      : in Gint) return GType;
   --  Return the type of the num-th argument for the handlers of signal name.
   --  If Num is negative, return the type returned by the handlers for this
   --  signal.

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

private

   type GObject_Record is tagged record
      Ptr : System.Address := System.Null_Address;
   end record;

   type GObject_Class is new System.Address;
   Uninitialized_Class : constant GObject_Class :=
     GObject_Class (System.Null_Address);

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
end Glib.GObjects;
