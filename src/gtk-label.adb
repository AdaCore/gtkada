-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2003 ACT-Europe                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.Label is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return GObject;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   ---------
   -- Get --
   ---------

   function Get_Text (Label : access Gtk_Label_Record) return UTF8_String is
      function Internal (Label : System.Address) return C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_label_get_text");

   begin
      return Value (Internal (Get_Object (Label)));
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Label : out Gtk_Label; Str : UTF8_String := "") is
   begin
      Label := new Gtk_Label_Record;
      Initialize (Label, Str);
   end Gtk_New;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Label : out Gtk_Label; Str : UTF8_String) is
   begin
      Label := new Gtk_Label_Record;
      Initialize_With_Mnemonic (Label, Str);
   end Gtk_New_With_Mnemonic;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String)
   is
      function Internal (Str : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");

   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
   end Initialize;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Label : access Gtk_Label_Record'Class; Str : UTF8_String)
   is
      function Internal (Str : UTF8_String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new_with_mnemonic");

   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
   end Initialize_With_Mnemonic;

   -----------------
   -- Set_Justify --
   -----------------

   procedure Set_Justify
     (Label : access Gtk_Label_Record;
      Jtype : Enums.Gtk_Justification)
   is
      procedure Internal
        (Label : System.Address; Jtype : Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_label_set_justify");

   begin
      Internal (Get_Object (Label), Jtype);
   end Set_Justify;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Label : access Gtk_Label_Record; Str : UTF8_String) is
      procedure Internal (Label : System.Address; Str : UTF8_String);
      pragma Import (C, Internal, "gtk_label_set_text");

   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Text;

   -----------------
   -- Set_Pattern --
   -----------------

   procedure Set_Pattern
     (Label : access Gtk_Label_Record; Pattern : String)
   is
      procedure Internal (Label : System.Address; Pattern : String);
      pragma Import (C, Internal, "gtk_label_set_pattern");

   begin
      Internal (Get_Object (Label), Pattern & ASCII.NUL);
   end Set_Pattern;

   -------------------
   -- Set_Line_Wrap --
   -------------------

   procedure Set_Line_Wrap (Label : access Gtk_Label_Record; Wrap : Boolean) is
      procedure Internal (Label : System.Address; Wrap : Gint);
      pragma Import (C, Internal, "gtk_label_set_line_wrap");

   begin
      Internal (Get_Object (Label), Boolean'Pos (Wrap));
   end Set_Line_Wrap;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return GObject is
   begin
      if Type_Name = "GtkLabel" then
         return new Gtk_Label_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Glib.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.Label;
