------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2014, AdaCore                     --
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

with Gtk.Enums; use Gtk.Enums;
with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Extra.Item_Entry is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Item_Entry_Record);
   pragma Warnings (Off, Type_Conversion);
   --  This package is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_IEntry;
                      Max    : Guint16 := 0)
   is
   begin
      Widget := new Gtk_IEntry_Record;
      Initialize (Widget, Max);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_IEntry_Record'Class;
                         Max    : Guint16)
   is
      function Internal (Max    : Guint16)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_item_entry_new_with_max_length");
   begin
      Set_Object (Widget, Internal (Max));
   end Initialize;

   -----------------------
   -- Set_Justification --
   -----------------------

   procedure Set_Justification
      (Item_Entry    : access Gtk_IEntry_Record;
       Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Item_Entry    : System.Address;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_item_entry_set_justification");

   begin
      Internal (Get_Object (Item_Entry), Justification);
   end Set_Justification;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Item_Entry    : access Gtk_IEntry_Record;
       Text          : String;
       Justification : Gtk.Enums.Gtk_Justification)
   is
      procedure Internal
        (Item_Entry    : System.Address;
         Text          : String;
         Justification : Gtk.Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_item_entry_set_text");

   begin
      Internal (Get_Object (Item_Entry),
                Text & ASCII.NUL,
                Justification);
   end Set_Text;

   ------------------------
   -- Set_Cursor_Visible --
   ------------------------

   procedure Set_Cursor_Visible
     (Item_Entry : access Gtk_IEntry_Record; Visible : Boolean)
   is
      procedure Internal (Ent : System.Address; Visible : Gboolean);
      pragma Import (C, Internal, "gtk_item_entry_set_cursor_visible");
   begin
      Internal (Get_Object (Item_Entry), Boolean'Pos (Visible));
   end Set_Cursor_Visible;

   ------------------------
   -- Get_Cursor_Visible --
   ------------------------

   function Get_Cursor_Visible
     (Item_Entry : access Gtk_IEntry_Record) return Boolean
   is
      function Internal (Ent : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_item_entry_get_cursor_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Item_Entry)));
   end Get_Cursor_Visible;

end Gtk.Extra.Item_Entry;
