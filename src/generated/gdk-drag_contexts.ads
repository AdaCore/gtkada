------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

pragma Ada_2005;

pragma Warnings (Off, "*is already use-visible*");
with Gdk;                     use Gdk;
with Gdk.Device;              use Gdk.Device;
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;

package Gdk.Drag_Contexts is

   type Drag_Context_Record is new GObject_Record with null record;
   type Drag_Context is access all Drag_Context_Record'Class;

   type Gdk_Drag_Action is mod 2 ** Integer'Size;
   pragma Convention (C, Gdk_Drag_Action);
   --  Used in Gdk.Drag_Contexts.Drag_Context to indicate what the destination
   --  should do with the dropped data.

   Action_Default : constant Gdk_Drag_Action := 1;
   Action_Copy : constant Gdk_Drag_Action := 2;
   Action_Move : constant Gdk_Drag_Action := 4;
   Action_Link : constant Gdk_Drag_Action := 8;
   Action_Private : constant Gdk_Drag_Action := 16;
   Action_Ask : constant Gdk_Drag_Action := 32;

   type Gdk_Drag_Protocol is (
      Drag_Proto_None,
      Drag_Proto_Motif,
      Drag_Proto_Xdnd,
      Drag_Proto_Rootwin,
      Drag_Proto_Win32_Dropfiles,
      Drag_Proto_Ole2,
      Drag_Proto_Local);
   pragma Convention (C, Gdk_Drag_Protocol);
   --  Used in Gdk.Drag_Contexts.Drag_Context to indicate the protocol
   --  according to which DND is done.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gdk_Drag_Action_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Drag_Action);
   type Property_Gdk_Drag_Action is new Gdk_Drag_Action_Properties.Property;

   package Gdk_Drag_Protocol_Properties is
      new Generic_Internal_Discrete_Property (Gdk_Drag_Protocol);
   type Property_Gdk_Drag_Protocol is new Gdk_Drag_Protocol_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gdk_drag_context_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Actions
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action;
   --  Determines the bitmask of actions proposed by the source if
   --  Gdk.Drag_Contexts.Get_Suggested_Action returns GDK_ACTION_ASK.
   --  Since: gtk+ 2.22

   function Get_Dest_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window;
   --  Returns the destination windw for the DND operation.
   --  Since: gtk+ 3.0

   function Get_Device
      (Self : not null access Drag_Context_Record)
       return Gdk.Device.Gdk_Device;
   --  Returns the Gdk.Device.Gdk_Device associated to the drag context.

   procedure Set_Device
      (Self   : not null access Drag_Context_Record;
       Device : not null access Gdk.Device.Gdk_Device_Record'Class);
   --  Associates a Gdk.Device.Gdk_Device to Context, so all Drag and Drop
   --  events for Context are emitted as if they came from this device.
   --  "device": a Gdk.Device.Gdk_Device

   function Get_Protocol
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Protocol;
   --  Returns the drag protocol thats used by this context.
   --  Since: gtk+ 3.0

   function Get_Selected_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action;
   --  Determines the action chosen by the drag destination.
   --  Since: gtk+ 2.22

   function Get_Source_Window
      (Self : not null access Drag_Context_Record) return Gdk.Gdk_Window;
   --  Returns the Gdk.Gdk_Window where the DND operation started.
   --  Since: gtk+ 2.22

   function Get_Suggested_Action
      (Self : not null access Drag_Context_Record) return Gdk_Drag_Action;
   --  Determines the suggested drag action of the context.
   --  Since: gtk+ 2.22

end Gdk.Drag_Contexts;
