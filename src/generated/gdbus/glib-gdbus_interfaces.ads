------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  The Glib.GDBus_Interfaces.Gdbus_Interface type is the base type for D-Bus
--  interfaces both on the service side (see
--  Gdbus.Interface_Skeleton.Gdbus_Interface_Skeleton) and client side (see
--  Gdbus.Proxy.Gdbus_Proxy).

pragma Warnings (Off, "*is already use-visible*");
with Glib.GDBus_Objects; use Glib.GDBus_Objects;
with Glib.GDBus_Types;   use Glib.GDBus_Types;
with Glib.Glist;         use Glib.Glist;
with Glib.Object;        use Glib.Object;
with Glib.Types;         use Glib.Types;
with Glib.Values;        use Glib.Values;

package Glib.GDBus_Interfaces is

   type Gdbus_Interface is new Glib.Types.GType_Interface;
   Null_Gdbus_Interface : constant Gdbus_Interface;

   function Convert (R : Glib.GDBus_Interfaces.Gdbus_Interface) return System.Address;
   function Convert (R : System.Address) return Glib.GDBus_Interfaces.Gdbus_Interface;
   package Gdbus_Interface_List is new Generic_List (Glib.GDBus_Interfaces.Gdbus_Interface);

   subtype Gdbus_Interface_GList is Gdbus_Interface_List.GList;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_dbus_interface_get_type");

   -------------
   -- Methods --
   -------------

   function Dup_Object
      (Self : Gdbus_Interface) return Glib.GDBus_Objects.Gdbus_Object;
   pragma Import (C, Dup_Object, "g_dbus_interface_dup_object");
   --  Gets the Glib.GDBus_Objects.Gdbus_Object that Interface_ belongs to, if
   --  any.
   --  Since: gtk+ 2.32
   --  @return A Glib.GDBus_Objects.Gdbus_Object or null. The returned
   --  reference should be freed with g_object_unref.

   function Get_Info
      (Self : Gdbus_Interface) return Glib.GDBus_Types.Gdbus_Interface_Info;
   pragma Import (C, Get_Info, "g_dbus_interface_get_info");
   --  Gets D-Bus introspection information for the D-Bus interface
   --  implemented by Interface_.
   --  Since: gtk+ 2.30
   --  @return A Gdbus.Interface_Info.Gdbus_Interface_Info. Do not free.

   function Get_Object
      (Self : Gdbus_Interface) return Glib.GDBus_Objects.Gdbus_Object;
   pragma Import (C, Get_Object, "g_dbus_interface_get_object");
   --  Gets the Glib.GDBus_Objects.Gdbus_Object that Interface_ belongs to, if
   --  any.
   --  It is not safe to use the returned object if Interface_ or the returned
   --  object is being used from other threads. See
   --  Glib.GDBus_Interfaces.Dup_Object for a thread-safe alternative.
   --  Since: gtk+ 2.30
   --  @return A Glib.GDBus_Objects.Gdbus_Object or null. The returned
   --  reference belongs to Interface_ and should not be freed.

   procedure Set_Object
      (Self   : Gdbus_Interface;
       Object : Glib.GDBus_Objects.Gdbus_Object);
   pragma Import (C, Set_Object, "g_dbus_interface_set_object");
   --  Sets the Glib.GDBus_Objects.Gdbus_Object for Interface_ to Object.
   --  Note that Interface_ will hold a weak reference to Object.
   --  Since: gtk+ 2.30
   --  @param Object A Glib.GDBus_Objects.Gdbus_Object or null.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Get_Interfaces
     (Self : Glib.GDBus_Objects.Gdbus_Object)
   return Gdbus_Interface_GList;
   pragma Import (C, Get_Interfaces, "g_dbus_object_get_interfaces");
   --  Gets the D-Bus interfaces associated with Object.
   --  Since: gtk+ 2.30
   --  @return A list of Glib.DBus_Interfaces.Gdbus_Interface instances. The
   --  returned list must be freed by g_list_free after each element has been
   --  freed with g_object_unref.

   function Unchecked_To_Gdbus_Interface
     (Args : Glib.Values.C_GValues; Num : Guint)
   return Gdbus_Interface;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdbus_Interface"

   function "+" (W : Gdbus_Interface) return Gdbus_Interface;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Dup_Object is access function
     (Self : Gdbus_Interface) return Glib.GDBus_Objects.Gdbus_Object;
   pragma Convention (C, Virtual_Dup_Object);
   --  Gets the Glib.GDBus_Objects.Gdbus_Object that Interface_ belongs to, if
   --  any.
   --  Since: gtk+ 2.32
   --  @return A Glib.GDBus_Objects.Gdbus_Object or null. The returned
   --  reference should be freed with g_object_unref.

   type Virtual_Get_Info is access function
     (Self : Gdbus_Interface)
   return access Glib.GDBus_Types.Gdbus_Interface_Info;
   pragma Convention (C, Virtual_Get_Info);
   --  Gets D-Bus introspection information for the D-Bus interface
   --  implemented by Interface_.
   --  Since: gtk+ 2.30
   --  @return A Gdbus.Interface_Info.Gdbus_Interface_Info. Do not free.

   type Virtual_Get_Object is access function
     (Self : Gdbus_Interface) return Glib.GDBus_Objects.Gdbus_Object;
   pragma Convention (C, Virtual_Get_Object);
   --  Gets the Glib.GDBus_Objects.Gdbus_Object that Interface_ belongs to, if
   --  any.
   --  It is not safe to use the returned object if Interface_ or the returned
   --  object is being used from other threads. See
   --  Glib.GDBus_Interfaces.Dup_Object for a thread-safe alternative.
   --  Since: gtk+ 2.30
   --  @return A Glib.GDBus_Objects.Gdbus_Object or null. The returned
   --  reference belongs to Interface_ and should not be freed.

   type Virtual_Set_Object is access procedure
     (Self   : Gdbus_Interface;
      Object : Glib.GDBus_Objects.Gdbus_Object);
   pragma Convention (C, Virtual_Set_Object);
   --  Sets the Glib.GDBus_Objects.Gdbus_Object for Interface_ to Object.
   --  Note that Interface_ will hold a weak reference to Object.
   --  Since: gtk+ 2.30
   --  @param Object A Glib.GDBus_Objects.Gdbus_Object or null.

   subtype GDBus_Interfaces_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Dup_Object
     (Self    : GDBus_Interfaces_Interface_Descr;
      Handler : Virtual_Dup_Object);
   pragma Import (C, Set_Dup_Object, "gtkada_GDBus_Interfaces_set_dup_object");

   procedure Set_Get_Info
     (Self    : GDBus_Interfaces_Interface_Descr;
      Handler : Virtual_Get_Info);
   pragma Import (C, Set_Get_Info, "gtkada_GDBus_Interfaces_set_get_info");

   procedure Set_Get_Object
     (Self    : GDBus_Interfaces_Interface_Descr;
      Handler : Virtual_Get_Object);
   pragma Import (C, Set_Get_Object, "gtkada_GDBus_Interfaces_set_get_object");

   procedure Set_Set_Object
     (Self    : GDBus_Interfaces_Interface_Descr;
      Handler : Virtual_Set_Object);
   pragma Import (C, Set_Set_Object, "gtkada_GDBus_Interfaces_set_set_object");
   --  See Glib.Object.Add_Interface

private

   Null_Gdbus_Interface : constant Gdbus_Interface :=
      Gdbus_Interface (Glib.Types.Null_Interface);
end Glib.GDBus_Interfaces;
