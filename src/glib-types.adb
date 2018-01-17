------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Gtkada.Bindings; use Gtkada.Bindings;
with Glib.Object;     use Glib.Object;
with System;          use System;

package body Glib.Types is

   ----------------
   -- Implements --
   ----------------

   package body Implements is

      ---------------
      -- To_Object --
      ---------------

      function To_Object (Interf : Interface_Type) return Object_Type is
         Stub : Object_Type_Record;
      begin
         return Object_Type
           (Get_User_Data (System.Address (Interf), Stub));
      end To_Object;

      ------------------
      -- To_Interface --
      ------------------

      function To_Interface
        (Object : access Object_Type_Record'Class) return Interface_Type is
      begin
         if Object = null then
            return Interface_Type (Null_Interface);
         else
            return Interface_Type (Get_Object (Object));
         end if;
      end To_Interface;
   end Implements;

   ---------------
   -- To_Object --
   ---------------

   function To_Object
     (Interf : GType_Interface) return Glib.Object.GObject
   is
      Stub : GObject_Record;
   begin
      return Get_User_Data (System.Address (Interf), Stub);
   end To_Object;

   ----------------
   -- Interfaces --
   ----------------

   function Interfaces (T : GType) return GType_Array is
      use GType_Arrays;
      function Internal
        (T        : GType;
         N_Ifaces : access Guint) return Unbounded_Array_Access;
      pragma Import (C, Internal, "g_type_interfaces");

      N      : aliased Guint;
      Output : constant Unbounded_Array_Access := Internal (T, N'Access);
      Result : constant GType_Array := To_Array (Output, N);

   begin
      --  Do says we should free, but doing so results in double deallocation
--      G_Free (Output);
      return Result;
   end Interfaces;

   ------------------
   -- Is_Interface --
   ------------------

   function Is_Interface (T : GType) return Boolean is
      function Internal (T : GType) return Gboolean;
      pragma Import (C, Internal, "ada_g_type_is_interface");
   begin
      return Boolean'Val (Internal (T));
   end Is_Interface;

   ----------
   -- Is_A --
   ----------

   function Is_A (T : GType; Is_A_Type : GType) return Boolean is
      function Internal (T1, T2 : GType) return Gboolean;
      pragma Import (C, Internal, "g_type_is_a");
   begin
      return Boolean'Val (Internal (T, Is_A_Type));
   end Is_A;

end Glib.Types;
