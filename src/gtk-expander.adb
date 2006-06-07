-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2006 AdaCore                         --
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

with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Gtk.Expander is

   ------------------
   -- Get_Expanded --
   ------------------

   function Get_Expanded
     (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_expanded");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Expanded;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Expander : access Gtk_Expander_Record) return String
   is
      function Internal
        (Expander : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_expander_get_label");
      --  Return value must not be modified or freed
   begin
      return Value (Internal (Get_Object (Expander)));
   end Get_Label;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
     (Expander : access Gtk_Expander_Record) return Gtk_Widget
   is
      function Internal (Expander : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_expander_get_label_widget");
      Stub : Gtk_Widget_Record;
   begin
      return Gtk_Widget
        (Get_User_Data (Internal (Get_Object (Expander)), Stub));
   end Get_Label_Widget;

   -----------------
   -- Get_Spacing --
   -----------------

   function Get_Spacing
     (Expander : access Gtk_Expander_Record) return Gint
   is
      function Internal (Expander : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_expander_get_spacing");
   begin
      return Internal (Get_Object (Expander));
   end Get_Spacing;

   --------------------
   -- Get_Use_Markup --
   --------------------

   function Get_Use_Markup
     (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_use_markup");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Use_Markup;

   -----------------------
   -- Get_Use_Underline --
   -----------------------

   function Get_Use_Underline
     (Expander : access Gtk_Expander_Record) return Boolean
   is
      function Internal (Expander : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_expander_get_use_underline");
   begin
      return Boolean'Val (Internal (Get_Object (Expander)));
   end Get_Use_Underline;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Expander : out Gtk_Expander; Label : String) is
   begin
      Expander := new Gtk_Expander_Record;
      Gtk.Expander.Initialize (Expander, Label);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Expander : access Gtk_Expander_Record'Class;
      Label    : String)
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new");
   begin
      Set_Object (Expander, Internal (Label & ASCII.NUL));
   end Initialize;

   ---------------------------
   -- Gtk_New_With_Mnemonic --
   ---------------------------

   procedure Gtk_New_With_Mnemonic
     (Expander : out Gtk_Expander; Label : String) is
   begin
      Expander := new Gtk_Expander_Record;
      Gtk.Expander.Initialize_With_Mnemonic (Expander, Label);
   end Gtk_New_With_Mnemonic;

   ------------------------------
   -- Initialize_With_Mnemonic --
   ------------------------------

   procedure Initialize_With_Mnemonic
     (Expander : access Gtk_Expander_Record'Class;
      Label    : String)
   is
      function Internal (Label : String) return System.Address;
      pragma Import (C, Internal, "gtk_expander_new_with_mnemonic");
   begin
      Set_Object (Expander, Internal (Label & ASCII.NUL));
   end Initialize_With_Mnemonic;

   ------------------
   -- Set_Expanded --
   ------------------

   procedure Set_Expanded
     (Expander : access Gtk_Expander_Record;
      Expanded : Boolean)
   is
      procedure Internal
        (Expander : System.Address; Expanded : Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_expanded");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Expanded));
   end Set_Expanded;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
     (Expander : access Gtk_Expander_Record; Label : String)
   is
      procedure Internal (Expander : System.Address; Label : String);
      pragma Import (C, Internal, "gtk_expander_set_label");
   begin
      Internal (Get_Object (Expander), Label & ASCII.NUL);
   end Set_Label;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
     (Expander     : access Gtk_Expander_Record;
      Label_Widget : access Gtk_Widget_Record'Class)
   is
      procedure Internal
        (Expander     : System.Address;
         Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_expander_set_label_widget");
   begin
      Internal (Get_Object (Expander), Get_Object (Label_Widget));
   end Set_Label_Widget;

   -----------------
   -- Set_Spacing --
   -----------------

   procedure Set_Spacing
     (Expander : access Gtk_Expander_Record;
      Spacing  : Gint)
   is
      procedure Internal
        (Expander : System.Address;
         Spacing  : Gint);
      pragma Import (C, Internal, "gtk_expander_set_spacing");
   begin
      Internal (Get_Object (Expander), Spacing);
   end Set_Spacing;

   --------------------
   -- Set_Use_Markup --
   --------------------

   procedure Set_Use_Markup
     (Expander   : access Gtk_Expander_Record;
      Use_Markup : Boolean)
   is
      procedure Internal
        (Expander   : System.Address;
         Use_Markup : Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_use_markup");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Use_Markup));
   end Set_Use_Markup;

   -----------------------
   -- Set_Use_Underline --
   -----------------------

   procedure Set_Use_Underline
     (Expander      : access Gtk_Expander_Record;
      Use_Underline : Boolean)
   is
      procedure Internal
        (Expander      : System.Address;
         Use_Underline : Gboolean);
      pragma Import (C, Internal, "gtk_expander_set_use_underline");
   begin
      Internal (Get_Object (Expander), Boolean'Pos (Use_Underline));
   end Set_Use_Underline;

end Gtk.Expander;
