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

with Unchecked_Conversion;
with Interfaces.C.Strings;   use Interfaces.C.Strings;

package body Gtk.Arguments is

   ---------------
   -- Make_Args --
   ---------------

   function Make_Args (Nb : Guint; Args : System.Address) return Gtk_Args is
   begin
      return (Nb => Nb, Arr => Args);
   end Make_Args;

   -------------
   -- Get_Nth --
   -------------

   function Get_Nth (Args : Gtk_Args; Num : Positive) return System.Address is
      function Internal (Args : System.Address; Num : Natural)
                        return System.Address;
      pragma Import (C, Internal, "ada_gtkarg_value_object");
   begin
      pragma Assert (Guint (Num) <= Args.Nb);
      return Internal (Args.Arr, Num - 1);
   end Get_Nth;

   -------------
   -- To_Gint --
   -------------

   function To_Gint (C : System.Address) return Gint is
      function Internal is new Unchecked_Conversion (System.Address, Gint);
   begin
      return Internal (C);
   end To_Gint;

   function To_Gint (Args : Gtk_Args; Num : Positive) return Gint is
   begin
      return To_Gint (Get_Nth (Args, Num));
   end To_Gint;

   --------------
   -- To_Guint --
   --------------

   function To_Guint (C : System.Address) return Guint is
      function Internal is new Unchecked_Conversion (System.Address, Guint);
   begin
      return Internal (C);
   end To_Guint;

   function To_Guint (Args : Gtk_Args; Num : Positive) return Guint is
   begin
      return To_Guint (Get_Nth (Args, Num));
   end To_Guint;

   ----------------
   -- To_Boolean --
   ----------------

   function To_Boolean (C : System.Address) return Boolean is
      function Convert is new Unchecked_Conversion (System.Address, Integer);
   begin
      return Boolean'Val (Convert (C));
   end To_Boolean;

   function To_Boolean (Args : Gtk_Args; Num : Positive) return Boolean is
   begin
      return To_Boolean (Get_Nth (Args, Num));
   end To_Boolean;

   ---------------
   -- To_Object --
   ---------------

   function To_Object  (C : System.Address) return Gtk.Object.Gtk_Object is
      use type System.Address;
      Stub : Gtk.Object.Gtk_Object_Record;

   begin
      if C = System.Null_Address then
         return null;
      else
         return Gtk.Object.Gtk_Object (Get_User_Data (C, Stub));
      end if;
   end To_Object;

   function To_Object (Args : Gtk_Args; Num : Positive)
     return Gtk.Object.Gtk_Object is
   begin
      return To_Object (Get_Nth (Args, Num));
   end To_Object;

   ----------------
   -- To_C_Proxy --
   ----------------

   function To_C_Proxy (C : System.Address) return Gdk.C_Proxy is
      function Internal is new Unchecked_Conversion
        (System.Address, Gdk.C_Proxy);
   begin
      return Internal (C);
   end To_C_Proxy;

   function To_C_Proxy (Args : Gtk_Args; Num : Positive) return Gdk.C_Proxy is
   begin
      return To_C_Proxy (Get_Nth (Args, Num));
   end To_C_Proxy;

   --------------
   -- To_Event --
   --------------

   function To_Event (Args : Gtk_Args; Num : Positive)
     return Gdk.Event.Gdk_Event is
   begin
      return To_Event (Get_Nth (Args, Num));
   end To_Event;

   ----------------------
   -- To_Notebook_Page --
   ----------------------

   function To_Notebook_Page (C : System.Address)
                             return Gtk.Notebook.Gtk_Notebook_Page
   is
   begin
      return Gtk.Notebook.Gtk_Notebook_Page (Gdk.Convert (C));
   end To_Notebook_Page;

   function To_Notebook_Page (Args : Gtk_Args; Num : Positive)
     return Gtk.Notebook.Gtk_Notebook_Page is
   begin
      return To_Notebook_Page (Get_Nth (Args, Num));
   end To_Notebook_Page;

   ----------------
   -- To_Address --
   ----------------

   function To_Address (Args : Gtk_Args; Num : Positive)
     return System.Address is
   begin
      return Get_Nth (Args, Num);
   end To_Address;

   ---------------
   -- To_String --
   ---------------

   function To_String  (C : System.Address) return String is
      use type Interfaces.C.Strings.chars_ptr;
      function Convert is new Unchecked_Conversion (System.Address, chars_ptr);
      Char : chars_ptr := Convert (C);
   begin
      if Char /= Interfaces.C.Strings.Null_Ptr then
         return Value (Char);
      else
         return "";
      end if;
   end To_String;

   function To_String  (Args : Gtk_Args; Num : Positive) return String is
   begin
      return To_String (Get_Nth (Args, Num));
   end To_String;

end Gtk.Arguments;
