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

with Gtk.Widget;
with Glib.Glade; use Glib.Glade; use Glib.Glade.Glib_XML;
with System;

package Gtk.Util is

   procedure Set_Object (Name : String_Ptr; Object : Gtk.Widget.Gtk_Widget);
   --  Associates Object with the specified Name. It is up to the caller to
   --  choose unique names.

   function Get_Object (Name : String_Ptr) return Gtk.Widget.Gtk_Widget;
   --  Return a widget associated (via a call to Set_Object) with Name

   type Private_Object is private;
   --  Internal representation of a widget as given by the low level signal
   --  manager. Use Set_Object (see below) to "convert" this object to an
   --  appropriate widget.

   type Callback is access procedure
     (Object : in Private_Object;
      Data   : in System.Address);
   --  The callback type.

   procedure Set_Object
     (Widget : access Gtk.Widget.Gtk_Widget_Record;
      Object : in     Private_Object);
   --  Sets the "internal" contents of a given widget. This function is similar
   --  to Gdk.Set_Object, but specific to callback functions.
   --  Note that the caller must ensure that Widget has the right type when
   --  calling this procedure.

   procedure Set_Signal
     (Name      : in String;
      Func      : in Callback;
      Func_Data : in System.Address);
   --  Associates a signal with the specified Name. It is up to the caller to
   --  choose unique names. Signal is the address of a callback. Note that
   --  no check is performed on Signal.

   procedure Get_Signal
     (Name      : in     String;
      Func      :    out Callback;
      Func_Data :    out System.Address);
   --  Return a Signal and a Data associated (via a call to Set_Signal) with
   --  Name

private
   type Private_Object is new System.Address;
end Gtk.Util;
