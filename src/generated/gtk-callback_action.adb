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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Object;                use Glib.Object;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Callback_Action is

   function Gtk_Callback_Action_New
     (Callback : System.Address;
      Data     : System.Address;
      Destroy  : System.Address) return System.Address;
   pragma Import (C, Gtk_Callback_Action_New, "gtk_callback_action_new");

   function To_Shortcut_Func is new Ada.Unchecked_Conversion
     (System.Address, Shortcut_Func);
   function To_Address is new Ada.Unchecked_Conversion
     (Shortcut_Func, System.Address);

   function Internal_Shortcut_Func
     (Widget : System.Address;
      Args   : System.Address;
      Data   : System.Address)
   return Glib.Gboolean;
   pragma Convention (C, Internal_Shortcut_Func);

   procedure Dummy_Destroy (Data : System.Address) is null;
   pragma Convention (C, Dummy_Destroy);

   ----------------------------
   -- Internal_Shortcut_Func --
   ----------------------------

   function Internal_Shortcut_Func
     (Widget : System.Address;
      Args   : System.Address;
      Data   : System.Address)
   return Glib.Gboolean
   is
      use type System.Address;
      CB   : constant Shortcut_Func := To_Shortcut_Func (Data);
      Stub : Gtk_Widget_Record;
      A    : GLib.Variant.Gvariant := Null_Gvariant;
   begin
      if Args /= System.Null_Address then
         Set_Object (A, Args);
      end if;
      return CB (Gtk_Widget (Get_User_Data (Widget, Stub)), A);
   end Internal_Shortcut_Func;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self     : out Gtk_Callback_Action;
      Callback : Shortcut_Func) is
   begin
      Self := new Gtk_Callback_Action_Record;
      Gtk.Callback_Action.Initialize (Self, Callback);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Gtk_Callback_Action_Record'Class;
      Callback : Shortcut_Func) is
   begin
      if not Self.Is_Created then
         Set_Object
           (Self,
            Gtk_Callback_Action_New
              (Internal_Shortcut_Func'Address,
               To_Address (Callback),
               Dummy_Destroy'Address));
      end if;
   end Initialize;

   -------------------------------
   -- Callback_Action_With_Data --
   -------------------------------

   package body Callback_Action_With_Data is
      package Users is new Glib.Object.User_Data_Closure
        (User_Data_Type, Destroy);

      function To_Shortcut_Data_Func is new Ada.Unchecked_Conversion
        (System.Address, Shortcut_Data_Func);
      function To_Address is new Ada.Unchecked_Conversion
        (Shortcut_Data_Func, System.Address);

      function Internal_Shortcut_Data_Func
        (Widget : System.Address;
         Args   : System.Address;
         Data   : System.Address) return Glib.Gboolean;
      pragma Convention (C, Internal_Shortcut_Data_Func);

      ---------------------------------
      -- Internal_Shortcut_Data_Func --
      ---------------------------------

      function Internal_Shortcut_Data_Func
        (Widget : System.Address;
         Args   : System.Address;
         Data   : System.Address) return Glib.Gboolean
      is
         use type System.Address;
         D    : constant Users.Internal_Data_Access := Users.Convert (Data);
         Stub : Gtk_Widget_Record;
         A    : GLib.Variant.Gvariant := Null_Gvariant;
      begin
         if Args /= System.Null_Address then
            Set_Object (A, Args);
         end if;
         return To_Shortcut_Data_Func (D.Func)
           (Gtk_Widget (Get_User_Data (Widget, Stub)), A, D.Data.all);
      end Internal_Shortcut_Data_Func;

      -------------
      -- Gtk_New --
      -------------

      procedure Gtk_New
        (Self     : out Gtk_Callback_Action;
         Callback : Shortcut_Data_Func;
         Data     : User_Data_Type) is
      begin
         Self := new Gtk_Callback_Action_Record;
         Gtk.Callback_Action.Callback_Action_With_Data.Initialize
           (Self, Callback, Data);
      end Gtk_New;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Self     : not null access Gtk_Callback_Action_Record'Class;
         Callback : Shortcut_Data_Func;
         Data     : User_Data_Type)
      is
         D : System.Address;
      begin
         if not Self.Is_Created then
            D := Users.Build (To_Address (Callback), Data);
            Set_Object
              (Self,
               Gtk_Callback_Action_New
                 (Internal_Shortcut_Data_Func'Address, D, Users.Free_Data'Address));
         end if;
      end Initialize;

   end Callback_Action_With_Data;

   package Type_Conversion_Gtk_Callback_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Callback_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Callback_Action);

end Gtk.Callback_Action;
