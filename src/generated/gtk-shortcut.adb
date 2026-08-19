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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Shortcut is

   package Type_Conversion_Gtk_Shortcut is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Shortcut_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Shortcut);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self    : out Gtk_Shortcut;
       Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Action  : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class)
   is
   begin
      Self := new Gtk_Shortcut_Record;
      Gtk.Shortcut.Initialize (Self, Trigger, Action);
   end Gtk_New;

   ----------------------
   -- Gtk_Shortcut_New --
   ----------------------

   function Gtk_Shortcut_New
      (Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Action  : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class)
       return Gtk_Shortcut
   is
      Self : constant Gtk_Shortcut := new Gtk_Shortcut_Record;
   begin
      Gtk.Shortcut.Initialize (Self, Trigger, Action);
      return Self;
   end Gtk_Shortcut_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self    : not null access Gtk_Shortcut_Record'Class;
       Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class;
       Action  : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class)
   is
      function Internal
         (Trigger : System.Address;
          Action  : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal (Get_Object_Or_Null (GObject (Trigger)), Get_Object_Or_Null (GObject (Action))));
      end if;
   end Initialize;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
      (Self : not null access Gtk_Shortcut_Record)
       return Gtk.Shortcut_Action.Gtk_Shortcut_Action
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_get_action");
      Stub_Gtk_Shortcut_Action : Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record;
   begin
      return Gtk.Shortcut_Action.Gtk_Shortcut_Action (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Shortcut_Action));
   end Get_Action;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
      (Self : not null access Gtk_Shortcut_Record)
       return Glib.Variant.Gvariant
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_get_arguments");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Get_Arguments;

   -----------------
   -- Get_Trigger --
   -----------------

   function Get_Trigger
      (Self : not null access Gtk_Shortcut_Record)
       return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_get_trigger");
      Stub_Gtk_Shortcut_Trigger : Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record;
   begin
      return Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Shortcut_Trigger));
   end Get_Trigger;

   ----------------
   -- Set_Action --
   ----------------

   procedure Set_Action
      (Self   : not null access Gtk_Shortcut_Record;
       Action : access Gtk.Shortcut_Action.Gtk_Shortcut_Action_Record'Class)
   is
      procedure Internal (Self : System.Address; Action : System.Address);
      pragma Import (C, Internal, "gtk_shortcut_set_action");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Action)));
   end Set_Action;

   -------------------
   -- Set_Arguments --
   -------------------

   procedure Set_Arguments
      (Self : not null access Gtk_Shortcut_Record;
       Args : Glib.Variant.Gvariant)
   is
      procedure Internal (Self : System.Address; Args : System.Address);
      pragma Import (C, Internal, "gtk_shortcut_set_arguments");
   begin
      Internal (Get_Object (Self), Get_Object (Args));
   end Set_Arguments;

   -----------------
   -- Set_Trigger --
   -----------------

   procedure Set_Trigger
      (Self    : not null access Gtk_Shortcut_Record;
       Trigger : access Gtk.Shortcut_Trigger.Gtk_Shortcut_Trigger_Record'Class)
   is
      procedure Internal (Self : System.Address; Trigger : System.Address);
      pragma Import (C, Internal, "gtk_shortcut_set_trigger");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Trigger)));
   end Set_Trigger;

end Gtk.Shortcut;
