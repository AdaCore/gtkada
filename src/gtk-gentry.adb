-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2002 ACT-Europe                 --
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

with System;
with Interfaces.C.Strings;
with Pango.Layout; use Pango.Layout;
with Glib.Type_Conversion_Hooks;
pragma Elaborate_All (Glib.Type_Conversion_Hooks);

package body Gtk.GEntry is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Type_Conversion (Type_Name : String) return GObject;
   --  This function is used to implement a minimal automated type conversion
   --  without having to drag the whole Gtk.Type_Conversion package for the
   --  most common widgets.

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record; Text : String)
   is
      procedure Internal (The_Entry : System.Address; Text : String);
      pragma Import (C, Internal, "gtk_entry_append_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Append_Text;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (The_Entry : access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_visibility");

   begin
      return To_Boolean (Internal (Get_Object (The_Entry)));
   end Get_Visibility;

   ------------------------
   -- Set_Invisible_Char --
   ------------------------

   procedure Set_Invisible_Char
     (The_Entry : access Gtk_Entry_Record; Char : Gunichar)
   is
      procedure Internal (The_Entry : System.Address; Char : Gunichar);
      pragma Import (C, Internal, "gtk_entry_get_invisible_char");

   begin
      Internal (Get_Object (The_Entry), Char);
   end Set_Invisible_Char;

   ------------------------
   -- Get_Invisible_Char --
   ------------------------

   function Get_Invisible_Char
     (The_Entry : access Gtk_Entry_Record) return Gunichar
   is
      function Internal (The_Entry : System.Address) return Gunichar;
      pragma Import (C, Internal, "gtk_entry_get_invisible_char");

   begin
      return Internal (Get_Object (The_Entry));
   end Get_Invisible_Char;

   -------------------
   -- Set_Has_Frame --
   -------------------

   procedure Set_Has_Frame
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean := True)
   is
      procedure Internal (The_Entry : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_has_frame");

   begin
      Internal (Get_Object (The_Entry), To_Gboolean (Setting));
   end Set_Has_Frame;

   -------------------
   -- Get_Has_Frame --
   -------------------

   function Get_Has_Frame
     (The_Entry : access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_has_frame");

   begin
      return To_Boolean (Internal (Get_Object (The_Entry)));
   end Get_Has_Frame;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length (The_Entry : access Gtk_Entry_Record) return Gint is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_max_length");

   begin
      return Internal (Get_Object (The_Entry));
   end Get_Max_Length;

   ---------------------------
   -- Set_Activates_Default --
   ---------------------------

   procedure Set_Activates_Default
     (The_Entry : access Gtk_Entry_Record; Setting : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Setting : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_activates_default");

   begin
      Internal (Get_Object (The_Entry), To_Gboolean (Setting));
   end Set_Activates_Default;

   ---------------------------
   -- Get_Activates_Default --
   ---------------------------

   function Get_Activates_Default
     (The_Entry : access Gtk_Entry_Record) return Boolean
   is
      function Internal (The_Entry : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_entry_get_activates_default");

   begin
      return To_Boolean (Internal (Get_Object (The_Entry)));
   end Get_Activates_Default;

   ---------------------
   -- Get_Width_Chars --
   ---------------------

   function Get_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class) return Gint
   is
      function Internal (The_Entry : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_get_width_chars");

   begin
      return Internal (Get_Object (The_Entry));
   end Get_Width_Chars;

   ----------------
   -- Get_Layout --
   ----------------

   function Get_Layout (The_Entry : access Gtk_Entry_Record)
      return Pango.Layout.Pango_Layout
   is
      function Internal (The_Entry : System.Address) return Pango_Layout;
      pragma Import (C, Internal, "gtk_entry_get_layout");
   begin
      return Internal (Get_Object (The_Entry));
   end Get_Layout;

   ------------------------
   -- Get_Layout_Offsets --
   ------------------------

   procedure Get_Layout_Offsets
     (The_Entry : access Gtk_Entry_Record;
      X         : out Gint;
      Y         : out Gint)
  is
      procedure Internal
        (The_Entry : System.Address;
         X         : out Gint;
         Y         : out Gint);
      pragma Import (C, Internal, "gtk_entry_get_layout_offsets");

   begin
      Internal (Get_Object (The_Entry), X, Y);
   end Get_Layout_Offsets;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (The_Entry : access Gtk_Entry_Record) return String is
      function Internal
        (The_Entry : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_text");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry)));
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry; Max : Gint) is
   begin
      Widget := new Gtk_Entry_Record;
      Initialize (Widget, Max);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry) is
   begin
      Widget := new Gtk_Entry_Record;
      Gtk.GEntry.Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gtk_Entry_Record'Class;
      Max    : Gint)
   is
      function Internal (Max : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_entry_new_with_max_length");

   begin
      Set_Object (Widget, Internal (Max));
      Initialize_User_Data (Widget);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Entry_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_entry_new");

   begin
      Set_Object (Widget, Internal);
      Initialize_User_Data (Widget);
   end Initialize;

   ------------------
   -- Prepend_Text --
   ------------------

   procedure Prepend_Text
     (The_Entry : access Gtk_Entry_Record; Text : String)
   is
      procedure Internal (The_Entry : System.Address; Text : String);
      pragma Import (C, Internal, "gtk_entry_prepend_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Prepend_Text;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record; Editable : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Editable : Gboolean);
      pragma Import (C, Internal, "gtk_entry_set_editable");

   begin
      Internal (Get_Object (The_Entry), To_Gboolean (Editable));
   end Set_Editable;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record;
      Max       : Gint)
   is
      procedure Internal (The_Entry : System.Address; Max : Gint);
      pragma Import (C, Internal, "gtk_entry_set_max_length");

   begin
      Internal (Get_Object (The_Entry), Max);
   end Set_Max_Length;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (The_Entry : access Gtk_Entry_Record; Text : String) is
      procedure Internal (The_Entry : System.Address; Text : String);
      pragma Import (C, Internal, "gtk_entry_set_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Set_Text;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record; Visible : Boolean)
   is
      procedure Internal (The_Entry : System.Address; Visible : Gint);
      pragma Import (C, Internal, "gtk_entry_set_visibility");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Visible));
   end Set_Visibility;

   ---------------------
   -- Set_Width_Chars --
   ---------------------

   procedure Set_Width_Chars
     (The_Entry : access Gtk_Entry_Record'Class; Width : Gint)
   is
      procedure Internal (The_Entry : System.Address; Width : Gint);
      pragma Import (C, Internal, "gtk_entry_set_width_chars");
   begin
      Internal (Get_Object (The_Entry), Width);
   end Set_Width_Chars;

   ---------------------
   -- Type_Conversion --
   ---------------------

   function Type_Conversion (Type_Name : String) return GObject is
   begin
      if Type_Name = "GtkEntry" then
         return new Gtk_Entry_Record;
      else
         return null;
      end if;
   end Type_Conversion;

begin
   Glib.Type_Conversion_Hooks.Add_Hook (Type_Conversion'Access);
end Gtk.GEntry;
