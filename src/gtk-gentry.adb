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

package body Gtk.GEntry is

   -----------------
   -- Append_Text --
   -----------------

   procedure Append_Text
     (The_Entry : access Gtk_Entry_Record;
      Text      : in String)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Text  : in String);
      pragma Import (C, Internal, "gtk_entry_append_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Append_Text;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (The_Entry : access Gtk_Entry_Record)
     return String
   is
      function Internal (The_Entry : in System.Address)
        return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_entry_get_text");

   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (The_Entry)));
   end Get_Text;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Entry; Max : in Guint16) is
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
      Initialize (Widget);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gtk_Entry_Record'Class;
                         Max : in Guint16)
   is
      function Internal (Max    : in Guint16) return System.Address;
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
     (The_Entry : access Gtk_Entry_Record;
      Text      : in String)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Text      : in String);
      pragma Import (C, Internal, "gtk_entry_prepend_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Prepend_Text;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
     (The_Entry : access Gtk_Entry_Record;
      Max       : in Guint16)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Max       : in Guint16);
      pragma Import (C, Internal, "gtk_entry_set_max_length");

   begin
      Internal (Get_Object (The_Entry), Max);
   end Set_Max_Length;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (The_Entry : access Gtk_Entry_Record;
      Text      : in String)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Text      : in String);
      pragma Import (C, Internal, "gtk_entry_set_text");

   begin
      Internal (Get_Object (The_Entry), Text & ASCII.NUL);
   end Set_Text;

   --------------------
   -- Set_Visibility --
   --------------------

   procedure Set_Visibility
     (The_Entry : access Gtk_Entry_Record;
      Visible   : in Boolean)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Visible   : in Gint);
      pragma Import (C, Internal, "gtk_entry_set_visibility");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Visible));
   end Set_Visibility;

end Gtk.GEntry;
