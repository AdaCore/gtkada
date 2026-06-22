------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2026, AdaCore                          --
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

package body Gdk.Event is

   use type System.Address;

   function Event_Ref (Event : System.Address) return System.Address;
   pragma Import (C, Event_Ref, "gdk_event_ref");

   procedure Event_Unref (Event : System.Address);
   pragma Import (C, Event_Unref, "gdk_event_unref");

   procedure Unreference (Self : in out Gdk_Event'Class);
   pragma Inline (Unreference);

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Gdk_Event) is
   begin
      if Self.Data /= System.Null_Address then
         Self.Data := Event_Ref (Self.Data);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Gdk_Event) is
   begin
      Unreference (Self);
   end Finalize;

   -----------------
   -- From_Object --
   -----------------

   function From_Object (Object : System.Address) return Gdk_Event is
   begin
      return Result : Gdk_Event do
         Set_Object (Result, Object);
      end return;
   end From_Object;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object (Self : Gdk_Event'Class) return System.Address is
   begin
      return Self.Data;
   end Get_Object;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Self : Gdk_Event'Class) return Boolean is
   begin
      return Self.Data = System.Null_Address;
   end Is_Null;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
     (Self   : in out Gdk_Event'Class;
      Object : System.Address)
   is
   begin
      if Self.Data = Object then
         return;
      end if;

      Unreference (Self);

      if Object /= System.Null_Address then
         Self.Data := Event_Ref (Object);
      end if;
   end Set_Object;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Self : in out Gdk_Event'Class) is
   begin
      if Self.Data /= System.Null_Address then
         Event_Unref (Self.Data);
         Self.Data := System.Null_Address;
      end if;
   end Unreference;

end Gdk.Event;