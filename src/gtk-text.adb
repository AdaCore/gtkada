-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
-- Copyright (C) 1998 Emmanuel Briot and Joel Brobecker              --
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
with Gdk; use Gdk;

package body Gtk.Text is

   ---------------------
   -- Backward_Delete --
   ---------------------

   function Backward_Delete
     (Text   : in Gtk_Text;
      Nchars : in Guint)
      return      Gint
   is
      function Internal
        (Text   : in System.Address;
         Nchars : in Guint)
         return      Gint;
      pragma Import (C, Internal, "gtk_text_backward_delete");
   begin
      return Internal (Get_Object (Text),
                       Nchars);
   end Backward_Delete;

   --------------------
   -- Forward_Delete --
   --------------------

   function Forward_Delete
     (Text   : in Gtk_Text;
      Nchars : in Guint)
      return      Gint
   is
      function Internal
        (Text   : in System.Address;
         Nchars : in Guint)
         return      Gint;
      pragma Import (C, Internal, "gtk_text_forward_delete");
   begin
      return Internal (Get_Object (Text),
                       Nchars);
   end Forward_Delete;

   ------------
   -- Freeze --
   ------------

   procedure Freeze (Text : in Gtk_Text) is
      procedure Internal (Text : in System.Address);
      pragma Import (C, Internal, "gtk_text_freeze");
   begin
      Internal (Get_Object (Text));
   end Freeze;

   ----------------------
   -- Get_Gap_Position --
   ----------------------

   function Get_Gap_Position (Widget : in Gtk_Text) return Guint is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_text_get_gap_position");
   begin
      return Internal (Get_Object (Widget));
   end Get_Gap_Position;

   ------------------
   -- Get_Gap_Size --
   ------------------

   function Get_Gap_Size (Widget : in Gtk_Text) return Guint is
      function Internal (Widget : in System.Address)
                         return      Guint;
      pragma Import (C, Internal, "ada_text_get_gap_size");
   begin
      return Internal (Get_Object (Widget));
   end Get_Gap_Size;

   --------------
   -- Get_Hadj --
   --------------

   function Get_Hadj (Widget : in Gtk_Text)
                      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_text_get_hadj");
      Adj : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Adj, Internal (Get_Object (Widget)));
      return Adj;
   end Get_Hadj;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Text   : in Gtk_Text) return Guint is
      function Internal (Text   : in System.Address) return Guint;
      pragma Import (C, Internal, "gtk_text_get_length");
   begin
      return Internal (Get_Object (Text));
   end Get_Length;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point (Text   : in Gtk_Text) return Guint is
      function Internal (Text   : in System.Address) return Guint;
      pragma Import (C, Internal, "gtk_text_get_point");
   begin
      return Internal (Get_Object (Text));
   end Get_Point;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Widget : in Gtk_Text) return String  is
      function Internal (Widget : in System.Address)
                         return      Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "ada_text_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Widget)));
   end Get_Text;

   ------------------
   -- Get_Text_End --
   ------------------

   function Get_Text_End (Widget : in Gtk_Text) return Guint is
      function Internal (Widget : in System.Address) return Guint;
      pragma Import (C, Internal, "ada_text_get_text_end");
   begin
      return Internal (Get_Object (Widget));
   end Get_Text_End;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget : out Gtk_Text;
      Hadj   : in Gtk.Adjustment.Gtk_Adjustment'Class
        := Gtk.Adjustment.Null_Adjustment;
      Vadj   : in Gtk.Adjustment.Gtk_Adjustment'Class
        := Gtk.Adjustment.Null_Adjustment)
   is
      function Internal
        (Hadj   : in System.Address;
         Vadj   : in System.Address)
         return      System.Address;
      pragma Import (C, Internal, "gtk_text_new");
   begin
      Set_Object (Widget, Internal (Get_Object (Hadj),
                                    Get_Object (Vadj)));
   end Gtk_New;

   --------------
   -- Get_Vadj --
   --------------

   function Get_Vadj (Widget : in Gtk_Text)
                      return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_text_get_vadj");
      Adj : Gtk.Adjustment.Gtk_Adjustment;
   begin
      Set_Object (Adj, Internal (Get_Object (Widget)));
      return Adj;
   end Get_Vadj;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Text   : in Gtk_Text;
      Font   : in Gdk.Font.Gdk_Font'Class;
      Fore   : in Gdk.Color.Gdk_Color'Class;
      Back   : in Gdk.Color.Gdk_Color'Class;
      Chars  : in String;
      Length : in Gint)
   is
      procedure Internal
        (Text   : in System.Address;
         Font   : in System.Address;
         Fore   : in System.Address;
         Back   : in System.Address;
         Chars  : in String;
         Length : in Gint);
      pragma Import (C, Internal, "gtk_text_insert");
   begin
      Internal (Get_Object (Text),
                Get_Object (Font),
                Get_Object (Fore),
                Get_Object (Back),
                Chars & Ascii.NUL,
                Length);
   end Insert;

   ---------------------
   -- Set_Adjustments --
   ---------------------

   procedure Set_Adjustments
     (Text : in Gtk_Text;
      Hadj : in Gtk.Adjustment.Gtk_Adjustment'Class;
      Vadj : in Gtk.Adjustment.Gtk_Adjustment'Class)
   is
      procedure Internal
        (Text : in System.Address;
         Hadj : in System.Address;
         Vadj : in System.Address);
      pragma Import (C, Internal, "gtk_text_set_adjustments");
   begin
      Internal (Get_Object (Text),
                Get_Object (Hadj),
                Get_Object (Vadj));
   end Set_Adjustments;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (Text     : in Gtk_Text;
      Editable : in Boolean)
   is
      procedure Internal
        (Text     : in System.Address;
         Editable : in Gint);
      pragma Import (C, Internal, "gtk_text_set_editable");
   begin
      Internal (Get_Object (Text),
                Boolean'Pos (Editable));
   end Set_Editable;

   ---------------
   -- Set_Point --
   ---------------

   procedure Set_Point (Text  : in Gtk_Text; Index : in Guint) is
      procedure Internal
        (Text  : in System.Address;
         Index : in Guint);
      pragma Import (C, Internal, "gtk_text_set_point");
   begin
      Internal (Get_Object (Text),
                Index);
   end Set_Point;

   -------------------
   -- Set_Word_Wrap --
   -------------------

   procedure Set_Word_Wrap
     (Text      : in Gtk_Text;
      Word_Wrap : in Boolean)
   is
      procedure Internal
        (Text      : in System.Address;
         Word_Wrap : in Gint);
      pragma Import (C, Internal, "gtk_text_set_word_wrap");
   begin
      Internal (Get_Object (Text),
                Boolean'Pos (Word_Wrap));
   end Set_Word_Wrap;

   ----------
   -- Thaw --
   ----------

   procedure Thaw (Text : in Gtk_Text)
   is
      procedure Internal (Text : in System.Address);
      pragma Import (C, Internal, "gtk_text_thaw");
   begin
      Internal (Get_Object (Text));
   end Thaw;

end Gtk.Text;
