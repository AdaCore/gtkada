-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU Library General Public       --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- Library General Public License for more details.                  --
--                                                                   --
-- You should have received a copy of the GNU Library General Public --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
-----------------------------------------------------------------------

with System;
with Gdk; use Gdk;
with Interfaces.C.Strings;

package body Gtk.Editable is

   -------------
   -- Changed --
   -------------

   procedure Changed (Editable : in Gtk_Editable'Class)
   is
      procedure Internal (Editable : in System.Address);
      pragma Import (C, Internal, "gtk_editable_changed");
   begin
      Internal (Get_Object (Editable));
   end Changed;

   ---------------------
   -- Claim_Selection --
   ---------------------

   procedure Claim_Selection
      (Editable : in Gtk_Editable'Class;
       Claim    : in Boolean;
       Time     : in Guint32)
   is
      procedure Internal
         (Editable : in System.Address;
          Claim    : in Gint;
          Time     : in Guint32);
      pragma Import (C, Internal, "gtk_editable_claim_selection");
   begin
      Internal (Get_Object (Editable),
                Boolean'Pos (Claim),
                Time);
   end Claim_Selection;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
      (Editable : in Gtk_Editable'Class;
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
      (Editable : in Gtk_Editable'Class;
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

   procedure Delete_Selection (Editable : in Gtk_Editable'Class)
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
      (Editable  : in Gtk_Editable'Class;
       Start_Pos : in Gint;
       End_Pos   : in Gint)
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
      (Editable  : in Gtk_Editable'Class;
       Start_Pos : in Gint;
       End_Pos   : in Gint)
       return         String
   is
      function Internal
         (Editable  : in System.Address;
          Start_Pos : in Gint;
          End_Pos   : in Gint)
          return         Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Interfaces.C.Strings.Value
        (Internal (Get_Object (Editable),
                   Start_Pos,
                   End_Pos));
   end Get_Chars;

   ------------------------
   -- Get_Clipboard_Text --
   ------------------------

   function Get_Clipboard_Text (Widget : in Gtk_Editable'Class)
                                 return      String
   is
      function Internal (Widget : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_editable_get_clipboard_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Get_Clipboard_Text;

   ---------------------
   -- Get_Current_Pos --
   ---------------------

   function Get_Current_Pos (Widget : in Gtk_Editable'Class)
                             return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_current_pos");
   begin
      return Internal (Get_Object (Widget));
   end Get_Current_Pos;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable (Widget : in Gtk_Editable'Class)
                          return      Boolean
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_editable");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Editable;

   -----------------------
   -- Get_Has_Selection --
   -----------------------

   function Get_Has_Selection (Widget : in Gtk_Editable'Class)
                               return      Boolean
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_has_selection");
   begin
      return Internal (Get_Object (Widget)) /= 0;
   end Get_Has_Selection;

   ---------------------------
   -- Get_Selection_End_Pos --
   ---------------------------

   function Get_Selection_End_Pos (Widget : in Gtk_Editable'Class)
                                   return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_selection_end_pos");
   begin
      return Internal (Get_Object (Widget));
   end Get_Selection_End_Pos;

   -----------------------------
   -- Get_Selection_Start_Pos --
   -----------------------------

   function Get_Selection_Start_Pos (Widget : in Gtk_Editable'Class)
                                     return      Guint
   is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_editable_get_selection_start_pos");
   begin
      return Internal (Get_Object (Widget));
   end Get_Selection_Start_Pos;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Editable        : in Gtk_Editable'Class;
       New_Text        : in String;
       New_Text_Length : in Gint;
       Position        : in out Gint)
   is
      procedure Internal
         (Editable        : in System.Address;
          New_Text        : in String;
          New_Text_Length : in Gint;
          Position        : in System.Address);
      pragma Import (C, Internal, "gtk_editable_insert_text");
   begin
      Internal (Get_Object (Editable),
                New_Text & Ascii.NUL,
                New_Text_Length,
                Position'Address);
   end Insert_Text;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
      (Editable : in Gtk_Editable'Class;
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
      (Editable : in Gtk_Editable'Class;
       Start    : in Gint;
       The_End  : in Gint)
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

end Gtk.Editable;

