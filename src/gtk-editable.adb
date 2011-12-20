------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

with System;
with Interfaces.C.Strings;

with Glib.Type_Conversion_Hooks;

package body Gtk.Editable is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Editable_Record);
   pragma Warnings (Off, Type_Conversion);

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard (Editable : access Gtk_Editable_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_copy_clipboard");

   begin
      Internal (Get_Object (Editable));
   end Copy_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard (Editable : access Gtk_Editable_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_cut_clipboard");

   begin
      Internal (Get_Object (Editable));
   end Cut_Clipboard;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection (Editable : access Gtk_Editable_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");

   begin
      Internal (Get_Object (Editable));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
     (Editable  : access Gtk_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1)
   is
      procedure Internal
        (Editable  : System.Address;
         Start_Pos : Gint;
         End_Pos   : Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");

   begin
      Internal (Get_Object (Editable), Start_Pos, End_Pos);
   end Delete_Text;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Editable  : access Gtk_Editable_Record;
      Start_Pos : Gint := 0;
      End_Pos   : Gint := -1) return UTF8_String
   is
      function Internal
        (Editable  : System.Address;
         Start_Pos : Gint;
         End_Pos   : Gint) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");

      procedure G_Free (Mem : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, G_Free, "g_free");
      --  External binding: g_free

      use type Interfaces.C.Strings.chars_ptr;

      Temp : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Get_Object (Editable), Start_Pos, End_Pos);

   begin
      if Temp /= Interfaces.C.Strings.Null_Ptr then
         declare
            Result : constant String := Interfaces.C.Strings.Value (Temp);
         begin
            G_Free (Temp);
            return Result;
         end;
      else
         return "";
      end if;
   end Get_Chars;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
     (Editable : access Gtk_Editable_Record) return Boolean
   is
      function Internal (Editable : System.Address) return Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");

   begin
      return Internal (Get_Object (Editable)) /= 0;
   end Get_Editable;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position (Editable : access Gtk_Editable_Record) return Gint is
      function Internal (Editable : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");

   begin
      return Internal (Get_Object (Editable));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Widget    : access Gtk_Editable_Record;
      Success   : out Boolean;
      Start_Pos : out Guint;
      End_Pos   : out Guint)
   is
      function Internal
        (Widget  : System.Address;
         Start   : access Guint;
         End_Pos : access Guint) return Gint;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");

      S, E : aliased Guint;

   begin
      Success := Boolean'Val
        (Internal
          (Get_Object (Widget), S'Unchecked_Access, E'Unchecked_Access));

      if Success then
         Start_Pos := S;
         End_Pos   := E;
      end if;
   end Get_Selection_Bounds;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Editable : access Gtk_Editable_Record;
      New_Text : UTF8_String;
      Position : in out Gint)
   is
      procedure Internal
        (Editable        : System.Address;
         New_Text        : UTF8_String;
         New_Text_Length : Gint;
         Position        : in out Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");

   begin
      Internal
        (Get_Object (Editable),
         New_Text & ASCII.NUL,
         New_Text'Length,
         Position);
   end Insert_Text;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard (Editable : access Gtk_Editable_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_paste_clipboard");

   begin
      Internal (Get_Object (Editable));
   end Paste_Clipboard;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Editable : access Gtk_Editable_Record;
      Start    : Gint;
      The_End  : Gint := -1)
   is
      procedure Internal
        (Editable : System.Address;
         Start    : Gint;
         The_End  : Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");

   begin
      Internal (Get_Object (Editable), Start, The_End);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Widget   : access Gtk_Editable_Record;
      Editable : Boolean := True)
   is
      procedure Internal (Widget : System.Address; Editable : Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");

   begin
      Internal (Get_Object (Widget), Boolean'Pos (Editable));
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Editable : access Gtk_Editable_Record;
      Position : Gint)
   is
      procedure Internal (Editable : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");

   begin
      Internal (Get_Object (Editable), Position);
   end Set_Position;

end Gtk.Editable;
