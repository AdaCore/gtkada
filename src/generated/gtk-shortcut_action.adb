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
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(On);

package body Gtk.Shortcut_Action is

   package Type_Conversion_Gtk_Shortcut_Action is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Shortcut_Action_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Shortcut_Action);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Shortcut_Action; String : UTF8_String) is
   begin
      Self := new Gtk_Shortcut_Action_Record;
      Gtk.Shortcut_Action.Initialize (Self, String);
   end Gtk_New;

   --------------------------------------
   -- Gtk_Shortcut_Action_Parse_String --
   --------------------------------------

   function Gtk_Shortcut_Action_Parse_String
      (String : UTF8_String) return Gtk_Shortcut_Action
   is
      Self : constant Gtk_Shortcut_Action := new Gtk_Shortcut_Action_Record;
   begin
      Gtk.Shortcut_Action.Initialize (Self, String);
      return Self;
   end Gtk_Shortcut_Action_Parse_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Shortcut_Action_Record'Class;
       String : UTF8_String)
   is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_action_parse_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_String);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_String);
   end Initialize;

   --------------
   -- Activate --
   --------------

   function Activate
      (Self   : not null access Gtk_Shortcut_Action_Record;
       Flags  : Gtk_Shortcut_Action_Flags;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Args   : Glib.Variant.Gvariant) return Boolean
   is
      function Internal
         (Self   : System.Address;
          Flags  : Gtk_Shortcut_Action_Flags;
          Widget : System.Address;
          Args   : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_shortcut_action_activate");
   begin
      return Internal (Get_Object (Self), Flags, Get_Object (Widget), Get_Object (Args)) /= 0;
   end Activate;

   -----------
   -- Print --
   -----------

   procedure Print
      (Self   : not null access Gtk_Shortcut_Action_Record;
       String : access Glib.String.Gstring)
   is
      procedure Internal
         (Self   : System.Address;
          String : access Glib.String.Gstring);
      pragma Import (C, Internal, "gtk_shortcut_action_print");
   begin
      Internal (Get_Object (Self), String);
   end Print;

   ---------------
   -- To_String --
   ---------------

   function To_String
      (Self : not null access Gtk_Shortcut_Action_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_shortcut_action_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

end Gtk.Shortcut_Action;
