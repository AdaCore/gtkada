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

with Gdk; use Gdk;
with System;

package Gtk.Object is

   type Gtk_Object_Record is new Root_Type with private;
   type Gtk_Object is access all Gtk_Object_Record'Class;

   procedure Destroy (Object : access Gtk_Object_Record);

   function Get_Type (Object : access Gtk_Object_Record) return Gint;

   procedure Ref (Object : access Gtk_Object_Record);

   procedure Unref (Object : access Gtk_Object_Record);

   -------------
   --  Flags  --
   -------------

   --  GtkObject only uses the first 4 bits of the flags field.
   --  Derived objects may use the remaining bits. Though this
   --  is a kinda nasty break up, it does make the size of
   --  derived objects smaller.

   Destroyed   : constant := 2 ** 0;
   Floating    : constant := 2 ** 1;
   Connected   : constant := 2 ** 2;
   Constructed : constant := 2 ** 3;

   function Flags (Object : access Gtk_Object_Record) return Guint32;

   procedure Set_Flags (Object : access Gtk_Object_Record;
                        Flags  : in     Guint32);

   procedure Unset_Flags (Object : access Gtk_Object_Record;
                          Flags  : in     Guint32);

   function Destroyed_Is_Set (Object : in Gtk_Object) return Boolean;

   function Floating_Is_Set (Object : in Gtk_Object) return Boolean;

   function Connected_Is_Set (Object : in Gtk_Object) return Boolean;

   function Constructed_Is_Set (Object : in Gtk_Object) return Boolean;

   ---------------
   -- User_Data --
   ---------------

   --  This package allow you to associate your own Data to the C
   --  widgets. No type verification is made to check if you are using the
   --  corresponding Get function. This is your own responsability.
   --  We recommand using package only if you want your data to be available
   --  from your own C code. If you just want to access it from Ada, you
   --  should consider creating a new tagged type instead.

   generic
      type Data_Type (<>) is private;
   package User_Data is
      function Get (Object : in Gtk_Object;
                    Id     : in String := "user_data") return Data_Type;
      --  Raises Constraint_Error if no Data is associated with this Id

      procedure Set (Object : in Gtk_Object;
                     Data   : in Data_Type;
                     Id     : in String := "user_data");
   end User_Data;

   -----------------------
   --  Support for Gate --
   -----------------------

   procedure Generate (Object : access Gtk_Object_Record;
                       N      : in Node_Ptr;
                       File   : in File_Type);

   procedure Generate (Object : access Gtk_Object_Record;
                       N      : in Node_Ptr);

private
   type Gtk_Object_Record is new Root_Type with null record;

end Gtk.Object;
