-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-2000                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

package body Gtk.Editable is

   -------------
   -- Changed --
   -------------

   procedure Changed (Editable : access Gtk_Editable_Record) is
   begin
      raise Program_Error;
   end Changed;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
      (Editable : access Gtk_Editable_Record;
       Time     : in Guint32)
   is
      procedure Internal
         (Editable : in System.Address;
          Time     : in Guint32);
      pragma Import (C, Internal, "gtk_editable_copy_clipboard");
   begin
      Internal (Get_Object (Editable),
                Time);
   end Copy_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
      (Editable : access Gtk_Editable_Record;
       Time     : in Guint32)
   is
      procedure Internal
         (Editable : in System.Address;
          Time     : in Guint32);
      pragma Import (C, Internal, "gtk_editable_cut_clipboard");
   begin
      Internal (Get_Object (Editable),
                Time);
   end Cut_Clipboard;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection (Editable : access Gtk_Editable_Record)
   is
      procedure Internal (Editable : in System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");
   begin
      Internal (Get_Object (Editable));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
      (Editable  : access Gtk_Editable_Record;
       Start_Pos : in Gint := 0;
       End_Pos   : in Gint := -1)
   is
      procedure Internal
         (Editable  : in System.Address;
          Start_Pos : in Gint;
          End_Pos   : in Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");
   begin
      Internal (Get_Object (Editable),
                Start_Pos,
                End_Pos);
   end Delete_Text;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Editable  : access Gtk_Editable_Record;
       Start_Pos : in Gint := 0;
       End_Pos   : in Gint := -1)
       return         String
   is
      function Internal
         (Editable  : in System.Address;
          Start_Pos : in Gint;
          End_Pos   : in Gint)
          return         Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");

      procedure Internal_G_Free (Mem : in Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal_G_Free, "g_free");
      use type Interfaces.C.Strings.chars_ptr;

      Temp : constant Interfaces.C.Strings.chars_ptr :=
        Internal (Get_Object (Editable), Start_Pos, End_Pos);
   begin
      if Temp /= Interfaces.C.Strings.Null_Ptr then
         declare
            Result : constant String := Interfaces.C.Strings.Value (Temp);
         begin
            Internal_G_Free (Temp);
            return Result;
         end;
      else
         return "";
      end if;
   end Get_Chars;

   ------------------------
   -- Get_Clipboard_Text --
   ------------------------

   function Get_Clipboard_Text
     (Widget : access Gtk_Editable_Record) return String is
   begin
      raise Program_Error;
      return "";
   end Get_Clipboard_Text;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
     (Widget : access Gtk_Editable_Record) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Get_Editable;

   -----------------------
   -- Get_Has_Selection --
   -----------------------

   function Get_Has_Selection
     (Widget : access Gtk_Editable_Record) return Boolean is
   begin
      raise Program_Error;
      return False;
   end Get_Has_Selection;

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
      (Editable        : access Gtk_Editable_Record;
       New_Text        : in String;
       Position        : in out Gint)
   is
      procedure Internal
         (Editable        : in System.Address;
          New_Text        : in String;
          New_Text_Length : in Gint;
          Position        :    in out Gint);
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

   procedure Paste_Clipboard
      (Editable : access Gtk_Editable_Record;
       Time     : in Guint32)
   is
      procedure Internal
         (Editable : in System.Address;
          Time     : in Guint32);
      pragma Import (C, Internal, "gtk_editable_paste_clipboard");
   begin
      Internal (Get_Object (Editable),
                Time);
   end Paste_Clipboard;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Editable : access Gtk_Editable_Record;
       Start    : in Gint;
       The_End  : in Gint := -1)
   is
      procedure Internal
         (Editable : in System.Address;
          Start    : in Gint;
          The_End  : in Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");
   begin
      Internal (Get_Object (Editable),
                Start,
                The_End);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Widget : access Gtk_Editable_Record;
      Editable : in Boolean := True) is
   begin
      raise Program_Error;
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position (Editable : access Gtk_Editable_Record;
                           Position : Gint) is
      procedure Internal (Editable : System.Address;
                          Position : Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");
   begin
      Internal (Get_Object (Editable), Position);
   end Set_Position;

end Gtk.Editable;
