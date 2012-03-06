------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Entry_Buffer is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Entry_Buffer_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self            : out Gtk_Entry_Buffer;
       Initial_Chars   : UTF8_String := "";
       N_Initial_Chars : Gint)
   is
   begin
      Self := new Gtk_Entry_Buffer_Record;
      Gtk.Entry_Buffer.Initialize (Self, Initial_Chars, N_Initial_Chars);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self            : not null access Gtk_Entry_Buffer_Record'Class;
       Initial_Chars   : UTF8_String := "";
       N_Initial_Chars : Gint)
   is
      function Internal
         (Initial_Chars   : Interfaces.C.Strings.chars_ptr;
          N_Initial_Chars : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_entry_buffer_new");
      Tmp_Initial_Chars : Interfaces.C.Strings.chars_ptr;
      Tmp_Return        : System.Address;
   begin
      if Initial_Chars = "" then
         Tmp_Initial_Chars := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Initial_Chars := New_String (Initial_Chars);
      end if;
      Tmp_Return := Internal (Tmp_Initial_Chars, N_Initial_Chars);
      Free (Tmp_Initial_Chars);
      Set_Object (Self, Tmp_Return);
   end Initialize;

   -----------------
   -- Delete_Text --
   -----------------

   function Delete_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       N_Chars  : Gint) return Guint
   is
      function Internal
         (Self     : System.Address;
          Position : Guint;
          N_Chars  : Gint) return Guint;
      pragma Import (C, Internal, "gtk_entry_buffer_delete_text");
   begin
      return Internal (Get_Object (Self), Position, N_Chars);
   end Delete_Text;

   -----------------------
   -- Emit_Deleted_Text --
   -----------------------

   procedure Emit_Deleted_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       N_Chars  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          N_Chars  : Guint);
      pragma Import (C, Internal, "gtk_entry_buffer_emit_deleted_text");
   begin
      Internal (Get_Object (Self), Position, N_Chars);
   end Emit_Deleted_Text;

   ------------------------
   -- Emit_Inserted_Text --
   ------------------------

   procedure Emit_Inserted_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       Chars    : UTF8_String;
       N_Chars  : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Chars    : Interfaces.C.Strings.chars_ptr;
          N_Chars  : Guint);
      pragma Import (C, Internal, "gtk_entry_buffer_emit_inserted_text");
      Tmp_Chars : Interfaces.C.Strings.chars_ptr := New_String (Chars);
   begin
      Internal (Get_Object (Self), Position, Tmp_Chars, N_Chars);
      Free (Tmp_Chars);
   end Emit_Inserted_Text;

   ---------------
   -- Get_Bytes --
   ---------------

   function Get_Bytes
      (Self : not null access Gtk_Entry_Buffer_Record) return gsize
   is
      function Internal (Self : System.Address) return gsize;
      pragma Import (C, Internal, "gtk_entry_buffer_get_bytes");
   begin
      return Internal (Get_Object (Self));
   end Get_Bytes;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
      (Self : not null access Gtk_Entry_Buffer_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_entry_buffer_get_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Length;

   --------------------
   -- Get_Max_Length --
   --------------------

   function Get_Max_Length
      (Self : not null access Gtk_Entry_Buffer_Record) return Gint
   is
      function Internal (Self : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_entry_buffer_get_max_length");
   begin
      return Internal (Get_Object (Self));
   end Get_Max_Length;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
      (Self : not null access Gtk_Entry_Buffer_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_buffer_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Self)));
   end Get_Text;

   -----------------
   -- Insert_Text --
   -----------------

   function Insert_Text
      (Self     : not null access Gtk_Entry_Buffer_Record;
       Position : Guint;
       Chars    : UTF8_String;
       N_Chars  : Gint) return Guint
   is
      function Internal
         (Self     : System.Address;
          Position : Guint;
          Chars    : Interfaces.C.Strings.chars_ptr;
          N_Chars  : Gint) return Guint;
      pragma Import (C, Internal, "gtk_entry_buffer_insert_text");
      Tmp_Chars  : Interfaces.C.Strings.chars_ptr := New_String (Chars);
      Tmp_Return : Guint;
   begin
      Tmp_Return := Internal (Get_Object (Self), Position, Tmp_Chars, N_Chars);
      Free (Tmp_Chars);
      return Tmp_Return;
   end Insert_Text;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
      (Self       : not null access Gtk_Entry_Buffer_Record;
       Max_Length : Gint)
   is
      procedure Internal (Self : System.Address; Max_Length : Gint);
      pragma Import (C, Internal, "gtk_entry_buffer_set_max_length");
   begin
      Internal (Get_Object (Self), Max_Length);
   end Set_Max_Length;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
      (Self    : not null access Gtk_Entry_Buffer_Record;
       Chars   : UTF8_String;
       N_Chars : Gint)
   is
      procedure Internal
         (Self    : System.Address;
          Chars   : Interfaces.C.Strings.chars_ptr;
          N_Chars : Gint);
      pragma Import (C, Internal, "gtk_entry_buffer_set_text");
      Tmp_Chars : Interfaces.C.Strings.chars_ptr := New_String (Chars);
   begin
      Internal (Get_Object (Self), Tmp_Chars, N_Chars);
      Free (Tmp_Chars);
   end Set_Text;

end Gtk.Entry_Buffer;
