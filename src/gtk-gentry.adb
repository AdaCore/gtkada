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
with Gtk.Util; use Gtk.Util;
with Gtk.Combo; use Gtk.Combo;
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
      Internal (Get_Object (The_Entry), Text & ASCII.Nul);
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
      Internal (Get_Object (The_Entry), Text & ASCII.Nul);
   end Prepend_Text;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (The_Entry : access Gtk_Entry_Record;
      Start     : in Gint;
      The_End  : in Gint)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Start     : in Gint;
         The_End   : in Gint);
      pragma Import (C, Internal, "gtk_entry_select_region");

   begin
      Internal (Get_Object (The_Entry), Start, The_End);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
     (The_Entry : access Gtk_Entry_Record;
      Editable  : in Boolean)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Editable  : in Gint);
      pragma Import (C, Internal, "gtk_entry_set_editable");

   begin
      Internal (Get_Object (The_Entry), Boolean'Pos (Editable));
   end Set_Editable;

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

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (The_Entry : access Gtk_Entry_Record;
      Position  : in Gint)
   is
      procedure Internal
        (The_Entry : in System.Address;
         Position  : in Gint);
      pragma Import (C, Internal, "gtk_entry_set_position");

   begin
      Internal (Get_Object (The_Entry), Position);
   end Set_Position;

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
      Internal (Get_Object (The_Entry), Text & ASCII.Nul);
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

   --------------
   -- Generate --
   --------------

   procedure Generate (N : in Node_Ptr; File : in File_Type) is
      Child_Name : Node_Ptr := Find_Tag (N.Child, "child_name");
   begin
      if Child_Name = null then
         Gen_New (N, "GEntry", File => File);
      else
         Gen_Child (N, Child_Name, File);
      end if;

      Editable.Generate (N, File);
      Gen_Set (N, "GEntry", "editable", File);
      Gen_Set (N, "GEntry", "Max_Length", "text_max_length", "", "", "", File);
      Gen_Set (N, "GEntry", "position", File);
      Gen_Set (N, "GEntry", "text", File, '"');
      Gen_Set (N, "GEntry", "Visibility", "text_visible", "", "", "", File);
   end Generate;

   procedure Generate
     (The_Entry : in out Object.Gtk_Object;
      N : in Node_Ptr)
   is
      S : String_Ptr;
      Child_Name : String_Ptr := Get_Field (N, "child_name");

   begin
      if Child_Name = null then
         if not N.Specific_Data.Created then
            Gtk_New (Gtk_Entry (The_Entry));
            Set_Object (Get_Field (N, "name"), The_Entry);
            N.Specific_Data.Created := True;
         end if;
      else

         --  Assuming the field is part of a Combo Box

         declare
            Combo : Gtk_Combo;

         begin
            Combo := Gtk_Combo (Get_Object (Find_Tag
               (Find_Parent (N.Parent, Get_Part (Child_Name.all, 1)),
                "name").Value));
            The_Entry := Object.Gtk_Object (Get_Entry (Combo));
         end;
      end if;

      Editable.Generate (The_Entry, N);

      S := Get_Field (N, "editable");

      if S /= null then
         Set_Editable (Gtk_Entry (The_Entry), Boolean'Value (S.all));
      end if;

      S := Get_Field (N, "text_max_length");

      if S /= null then
         Set_Max_Length (Gtk_Entry (The_Entry), Guint16'Value (S.all));
      end if;

      S := Get_Field (N, "position");

      if S /= null then
         Set_Position (Gtk_Entry (The_Entry), Gint'Value (S.all));
      end if;

      S := Get_Field (N, "text");

      if S /= null then
         Set_Text (Gtk_Entry (The_Entry), S.all);
      end if;

      S := Get_Field (N, "text_visible");

      if S /= null then
         Set_Visibility (Gtk_Entry (The_Entry), Boolean'Value (S.all));
      end if;
   end Generate;

end Gtk.GEntry;
