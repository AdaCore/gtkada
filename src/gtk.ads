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

--  <description>
--
--  This package provides some basic Gtk+ functionalities such as getting the
--  version number. This is the top level package of the Gtk widget hierarchy.
--  For general GtkAda initializations, @pxref{Package_Gtk.Main}.
--
--  </description>
--  <c_version>1.2.7</c_version>

with Glib;                use Glib;
with Gdk;                 use Gdk;
with System;

package Gtk is

   type Root_Type is tagged private;
   --  The base type of the hierarchy in GtkAda. It basically gives access
   --  to an underlying C object. This is not a controlled type, for efficiency
   --  reasons, and because gtk+ takes care of memory management on its own.

   --  <doc_ignore>
   type Root_Type_Access is access all Root_Type'Class;
   --  </doc_ignore>

   type Gtk_Rc_Style is new Gdk.C_Proxy;
   --  Type used to handle resource styles.
   --  See package Gtk.Rc for more details.

   function Major_Version return Guint;
   --  Return the major version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 1.

   function Minor_Version return Guint;
   --  Return the minor version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 2.

   function Micro_Version return Guint;
   --  Return the micro version number for Gtk+.
   --  Note that this is not necessarily the same as for GtkAda.
   --  If the version is 1.2.6, returns 6.

   subtype Gtk_Type is Glib.GType;
   --  Renaming used for compatiblity.
   --  Note: Gtk_Type_* constants have been replaced by GType_* constants
   --  in Glib.

   type Gtk_Notebook_Page is new Gdk.C_Proxy;
   --  A page of the notebook.
   --  It can contain a single child, and is also associated with a tab
   --  label used to select that page in the notebook.

   function Type_Name (Type_Num : in Gtk_Type) return String;
   --  Return the type name corresponding to a Gtk_Type.
   --  This might be useful in debug messages.

   function Type_From_Name (Name : in String) return Gtk_Type;
   --  Convert a string to the matching type.
   --  Name should be the C widget's name, such as GtkScrollbar or GtkButton,
   --  rather than the Ada name.

   function Is_Created (Object : in Root_Type'Class) return Boolean;
   --  Return True if the associated C object has been created, False if no
   --  C object is associated with Object.
   --  This is not the same as testing whether an access type (for instance
   --  any of the widgets) is "null", since this relates to the underlying
   --  C object.

   ------------------------
   -- Interfacing with C --
   ------------------------
   --  The following functions are made public so that one can easily create
   --  new widgets outside the Gtk package hierarchy.
   --  Only experienced users should make use of these functions.

   function Get_Object (Object : access Root_Type'Class) return System.Address;
   --  Access the underlying C pointer.

   procedure Set_Object
     (Object : access Root_Type'Class;
      Value  : in     System.Address);
   --  Modify the underlying C pointer.

   procedure Initialize_User_Data (Obj : access Root_Type'Class);
   --  Sets a user data field for the C object associated with Obj.
   --  This field will be used so that it is possible, knowing a
   --  C object, to get the full ada object.

   function Get_User_Data
     (Obj  : in System.Address;
      Stub : in Root_Type'Class) return Root_Type_Access;
   --  Get the user data that was set by GtkAda.
   --  If the Data is not set, return a new access type, that points to
   --  a structure with the same tag as Stub.

   function Unchecked_Cast
     (Obj  : access Root_Type'Class;
      Stub : Root_Type'Class) return Root_Type_Access;
   --  Cast Obj in an object of tag Stub'Class.
   --  Return the resulting object and free the memory pointed by Obj.

   function Count_Arguments
     (The_Type : Gtk_Type; Name : in String) return Guint;
   --  Return the number of arguments used in the handlers for the signal.
   --  Note that in the Connect functions, we always test whether the user
   --  has asked for *at most* the number of arguments defined by gtk+ for the
   --  callback. This is because having less argument is authorized (the
   --  extra parameters passed by gtk+ will simply be ignored), whereas having
   --  more arguments is impossible (they would never be set).
   --  Note that we provide this procedure here to avoid circularities.

   function Argument_Type
     (The_Type : Gtk_Type;
      Name     : in String;
      Num      : in Gint) return Gtk_Type;
   --  Return the type of the num-th argument for the handlers of signal name.
   --  If Num is negative, return the type returned by the handlers for this
   --  signal.
   --  Note that we provide this procedure here to avoid circularities.

private

   type Root_Type is tagged record
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
     (Obj : System.Address; Stub : Root_Type'Class) return Root_Type_Access;
   --  This function has to convert a C object to an Ada object.
   --  It will first try all the registered functions (in
   --  Gtk.Type_Conversion_Hooks) and by default, will create a Stub'Class
   --  object, no matter what the real C type is.
   --  Stub is the expected type.

   --  </doc_ignore>

   pragma Inline (Get_Object);
   pragma Inline (Set_Object);

   pragma Import (C, Major_Version, "ada_gtk_major_version");
   pragma Import (C, Minor_Version, "ada_gtk_minor_version");
   pragma Import (C, Micro_Version, "ada_gtk_micro_version");
end Gtk;
