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

private

   type GObject_Record is tagged record
      Ptr : System.Address := System.Null_Address;
   end record;

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

end Glib.GObjects;
