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

--  The Glib.GDBus_Objects.Gdbus_Object type is the base type for D-Bus
--  objects on both the service side (see
--  Gdbus.Object_Skeleton.Gdbus_Object_Skeleton) and the client side (see
--  Gdbus.Object_Proxy.Gdbus_Object_Proxy). It is essentially just a container
--  of interfaces.

pragma Warnings (Off, "*is already use-visible*");
limited with Glib.GDBus_Interfaces;
with Glib.Object;           use Glib.Object;
with Glib.Types;            use Glib.Types;
with Gtkada.Types;          use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;       use Gtkada.Bindings;
pragma Warnings(On);

package Glib.GDBus_Objects is

   type Gdbus_Object is new Glib.Types.GType_Interface;
   Null_Gdbus_Object : constant Gdbus_Object;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "g_dbus_object_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Interface
      (Self           : Gdbus_Object;
       Interface_Name : UTF8_String)
       return Glib.GDBus_Interfaces.Gdbus_Interface;
   --  Gets the D-Bus interface with name Interface_Name associated with
   --  Object, if any.
   --  Since: gtk+ 2.30
   --  @param Interface_Name A D-Bus interface name.
   --  @return null if not found, otherwise a
   --  Glib.GDBus_Interfaces.Gdbus_Interface that must be freed with
   --  g_object_unref.

   function Get_Object_Path (Self : Gdbus_Object) return UTF8_String;
   --  Gets the object path for Object.
   --  Since: gtk+ 2.30
   --  @return A string owned by Object. Do not free.

   ----------------------
   -- GtkAda additions --
   ----------------------

   --   function Get_Interfaces
   --      (Self : Gdbus_Object)
   --       return Gdbus_Interface_GList;
   --  This method is moved to Glib.GDBus_Interfaces due to circular dependency

   -------------
   -- Signals --
   -------------

   type Cb_Gdbus_Object_Gdbus_Interface_Void is not null access procedure
     (Self  : Gdbus_Object;
      Iface : Glib.GDBus_Interfaces.Gdbus_Interface);

   type Cb_GObject_Gdbus_Interface_Void is not null access procedure
     (Self  : access Glib.Object.GObject_Record'Class;
      Iface : Glib.GDBus_Interfaces.Gdbus_Interface);

   Signal_Interface_Added : constant Glib.Signal_Name := "interface-added";
   procedure On_Interface_Added
      (Self  : Gdbus_Object;
       Call  : Cb_Gdbus_Object_Gdbus_Interface_Void;
       After : Boolean := False);
   procedure On_Interface_Added
      (Self  : Gdbus_Object;
       Call  : Cb_GObject_Gdbus_Interface_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Interface is added to Object.

   Signal_Interface_Removed : constant Glib.Signal_Name := "interface-removed";
   procedure On_Interface_Removed
      (Self  : Gdbus_Object;
       Call  : Cb_Gdbus_Object_Gdbus_Interface_Void;
       After : Boolean := False);
   procedure On_Interface_Removed
      (Self  : Gdbus_Object;
       Call  : Cb_GObject_Gdbus_Interface_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when Interface is removed from Object.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdbus_Object"

   function "+" (W : Gdbus_Object) return Gdbus_Object;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Interface is access function
     (Self           : Gdbus_Object;
      Interface_Name : Gtkada.Types.Chars_Ptr)
   return Glib.GDBus_Interfaces.Gdbus_Interface;
   pragma Convention (C, Virtual_Get_Interface);
   --  Gets the D-Bus interface with name Interface_Name associated with
   --  Object, if any.
   --  Since: gtk+ 2.30
   --  @param Interface_Name A D-Bus interface name.
   --  @return null if not found, otherwise a
   --  Glib.GDBus_Interfaces.Gdbus_Interface that must be freed with
   --  g_object_unref.

   type Virtual_Get_Interfaces is access function (Self : Gdbus_Object) return System.Address;
   pragma Convention (C, Virtual_Get_Interfaces);
   --  Gets the D-Bus interfaces associated with Object.
   --  Since: gtk+ 2.30
   --  @return A list of Glib.GDBus_Interfaces.Gdbus_Interface instances. The
   --  returned list must be freed by g_list_free after each element has been
   --  freed with g_object_unref.

   type Virtual_Get_Object_Path is access function (Self : Gdbus_Object) return Gtkada.Types.Chars_Ptr;
   pragma Convention (C, Virtual_Get_Object_Path);
   --  Gets the object path for Object.
   --  Since: gtk+ 2.30
   --  @return A string owned by Object. Do not free.

   type Virtual_Interface_Added is access procedure
     (Self  : Gdbus_Object;
      Iface : Glib.GDBus_Interfaces.Gdbus_Interface);
   pragma Convention (C, Virtual_Interface_Added);
   --  Signal handler for the Glib.GDBus_Objects.Gdbus_Object::interface-added
   --  signal.

   type Virtual_Interface_Removed is access procedure
     (Self  : Gdbus_Object;
      Iface : Glib.GDBus_Interfaces.Gdbus_Interface);
   pragma Convention (C, Virtual_Interface_Removed);
   --  Signal handler for the
   --  Glib.GDBus_Objects.Gdbus_Object::interface-removed signal.

   subtype GDBus_Objects_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Interface
     (Self    : GDBus_Objects_Interface_Descr;
      Handler : Virtual_Get_Interface);
   pragma Import (C, Set_Get_Interface, "gtkada_GDBus_Objects_set_get_interface");

   procedure Set_Get_Interfaces
     (Self    : GDBus_Objects_Interface_Descr;
      Handler : Virtual_Get_Interfaces);
   pragma Import (C, Set_Get_Interfaces, "gtkada_GDBus_Objects_set_get_interfaces");

   procedure Set_Get_Object_Path
     (Self    : GDBus_Objects_Interface_Descr;
      Handler : Virtual_Get_Object_Path);
   pragma Import (C, Set_Get_Object_Path, "gtkada_GDBus_Objects_set_get_object_path");

   procedure Set_Interface_Added
     (Self    : GDBus_Objects_Interface_Descr;
      Handler : Virtual_Interface_Added);
   pragma Import (C, Set_Interface_Added, "gtkada_GDBus_Objects_set_interface_added");

   procedure Set_Interface_Removed
     (Self    : GDBus_Objects_Interface_Descr;
      Handler : Virtual_Interface_Removed);
   pragma Import (C, Set_Interface_Removed, "gtkada_GDBus_Objects_set_interface_removed");
   --  See Glib.Object.Add_Interface

private

   Null_Gdbus_Object : constant Gdbus_Object :=
      Gdbus_Object (Glib.Types.Null_Interface);
end Glib.GDBus_Objects;
