------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Stack_Switcher is

   package Type_Conversion_Gtk_Stack_Switcher is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Stack_Switcher_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Stack_Switcher);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Stack_Switcher) is
   begin
      Self := new Gtk_Stack_Switcher_Record;
      Gtk.Stack_Switcher.Initialize (Self);
   end Gtk_New;

   ----------------------------
   -- Gtk_Stack_Switcher_New --
   ----------------------------

   function Gtk_Stack_Switcher_New return Gtk_Stack_Switcher is
      Self : constant Gtk_Stack_Switcher := new Gtk_Stack_Switcher_Record;
   begin
      Gtk.Stack_Switcher.Initialize (Self);
      return Self;
   end Gtk_Stack_Switcher_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Stack_Switcher_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_stack_switcher_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   ---------------
   -- Get_Stack --
   ---------------

   function Get_Stack
      (Self : not null access Gtk_Stack_Switcher_Record)
       return Gtk.Stack.Gtk_Stack
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_stack_switcher_get_stack");
      Stub_Gtk_Stack : Gtk.Stack.Gtk_Stack_Record;
   begin
      return Gtk.Stack.Gtk_Stack (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Stack));
   end Get_Stack;

   ---------------
   -- Set_Stack --
   ---------------

   procedure Set_Stack
      (Self  : not null access Gtk_Stack_Switcher_Record;
       Stack : access Gtk.Stack.Gtk_Stack_Record'Class)
   is
      procedure Internal (Self : System.Address; Stack : System.Address);
      pragma Import (C, Internal, "gtk_stack_switcher_set_stack");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Stack)));
   end Set_Stack;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Stack_Switcher_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Stack_Switcher_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Stack_Switcher;
