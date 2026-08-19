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

package body Gtk.Shortcut_Trigger is

   package Type_Conversion_Gtk_Shortcut_Trigger is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Shortcut_Trigger_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Shortcut_Trigger);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Shortcut_Trigger; String : UTF8_String) is
   begin
      Self := new Gtk_Shortcut_Trigger_Record;
      Gtk.Shortcut_Trigger.Initialize (Self, String);
   end Gtk_New;

   ---------------------------------------
   -- Gtk_Shortcut_Trigger_Parse_String --
   ---------------------------------------

   function Gtk_Shortcut_Trigger_Parse_String
      (String : UTF8_String) return Gtk_Shortcut_Trigger
   is
      Self : constant Gtk_Shortcut_Trigger := new Gtk_Shortcut_Trigger_Record;
   begin
      Gtk.Shortcut_Trigger.Initialize (Self, String);
      return Self;
   end Gtk_Shortcut_Trigger_Parse_String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self   : not null access Gtk_Shortcut_Trigger_Record'Class;
       String : UTF8_String)
   is
      function Internal
         (String : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_shortcut_trigger_parse_string");
      Tmp_String : Gtkada.Types.Chars_Ptr := New_String (String);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_String);
         Free (Tmp_String);
         Set_Object (Self, Tmp_Return);
      else
         Free (Tmp_String);
      end if;
   end Initialize;

   -------------
   -- Compare --
   -------------

   function Compare
      (Self     : not null access Gtk_Shortcut_Trigger_Record;
       Trigger2 : not null access Gtk_Shortcut_Trigger_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Self     : System.Address;
          Trigger2 : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_shortcut_trigger_compare");
   begin
      return Internal (Get_Object (Self), Get_Object (Trigger2));
   end Compare;

   -----------
   -- Equal --
   -----------

   function Equal
      (Self     : not null access Gtk_Shortcut_Trigger_Record;
       Trigger2 : not null access Gtk_Shortcut_Trigger_Record'Class)
       return Boolean
   is
      function Internal
         (Self     : System.Address;
          Trigger2 : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_shortcut_trigger_equal");
   begin
      return Internal (Get_Object (Self), Get_Object (Trigger2)) /= 0;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash
      (Self : not null access Gtk_Shortcut_Trigger_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_shortcut_trigger_hash");
   begin
      return Internal (Get_Object (Self));
   end Hash;

   -----------
   -- Print --
   -----------

   procedure Print
      (Self   : not null access Gtk_Shortcut_Trigger_Record;
       String : access Glib.String.Gstring)
   is
      procedure Internal
         (Self   : System.Address;
          String : access Glib.String.Gstring);
      pragma Import (C, Internal, "gtk_shortcut_trigger_print");
   begin
      Internal (Get_Object (Self), String);
   end Print;

   -----------------
   -- Print_Label --
   -----------------

   function Print_Label
      (Self    : not null access Gtk_Shortcut_Trigger_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class;
       String  : access Glib.String.Gstring) return Boolean
   is
      function Internal
         (Self    : System.Address;
          Display : System.Address;
          String  : access Glib.String.Gstring) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_shortcut_trigger_print_label");
   begin
      return Internal (Get_Object (Self), Get_Object (Display), String) /= 0;
   end Print_Label;

   --------------
   -- To_Label --
   --------------

   function To_Label
      (Self    : not null access Gtk_Shortcut_Trigger_Record;
       Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return UTF8_String
   is
      function Internal
         (Self    : System.Address;
          Display : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_shortcut_trigger_to_label");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self), Get_Object (Display)));
   end To_Label;

   ---------------
   -- To_String --
   ---------------

   function To_String
      (Self : not null access Gtk_Shortcut_Trigger_Record)
       return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_shortcut_trigger_to_string");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end To_String;

   -------------
   -- Trigger --
   -------------

   function Trigger
      (Self             : not null access Gtk_Shortcut_Trigger_Record;
       Event            : Gdk.Event.Gdk_Event;
       Enable_Mnemonics : Boolean) return Gdk.Key_Match.Gdk_Key_Match
   is
      function Internal
         (Self             : System.Address;
          Event            : System.Address;
          Enable_Mnemonics : Glib.Gboolean)
          return Gdk.Key_Match.Gdk_Key_Match;
      pragma Import (C, Internal, "gtk_shortcut_trigger_trigger");
   begin
      return Internal (Get_Object (Self), Get_Object (Event), Boolean'Pos (Enable_Mnemonics));
   end Trigger;

end Gtk.Shortcut_Trigger;
