-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
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

with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Gdk; use Gdk;
with Gtk.Util; use Gtk.Util;

package body Gtk.Label is

   -----------
   --  Get  --
   -----------

   function Get (Label : in Gtk_Label) return String is
      procedure Internal (Label : in     System.Address;
                          Str   :    out C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_label_get");
      Temp : chars_ptr;
   begin
      Internal (Get_Object (Label), Temp);
      return Value (Temp);
   end Get;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Label :    out Gtk_Label;
                      Str   : in     String) is
      function Internal (Str : in String) return System.Address;
      pragma Import (C, Internal, "gtk_label_new");
   begin
      Set_Object (Label, Internal (Str & ASCII.NUL));
   end Gtk_New;

   -------------------
   --  Set_Justify  --
   -------------------

   procedure Set_Justify (Label : in Gtk_Label;
                          Jtype : in Enums.Gtk_Justification) is
      procedure Internal (Label : in System.Address;
                          Jtype : in Enums.Gtk_Justification);
      pragma Import (C, Internal, "gtk_label_set_justify");
   begin
      Internal (Get_Object (Label), Jtype);
   end Set_Justify;

   ----------------
   --  Set_Text  --
   ----------------

   procedure Set_Text (Label : in Gtk_Label;
                       Str   : in String) is
      procedure Internal (Label : in System.Address;
                          Str   : in String);
      pragma Import (C, Internal, "gtk_label_set_text");
   begin
      Internal (Get_Object (Label), Str & ASCII.NUL);
   end Set_Text;

   --------------
   -- Generate --
   --------------

   procedure Generate (Label : in Gtk_Label;
                       N     : in Node_Ptr;
                       File  : in File_Type) is
      use Misc;
   begin
      Gen_New (N, "Label", Find_Tag (N.Child, "label").Value.all,
        File => File, Delim => '"');
      Generate (Gtk_Misc (Label), N, File);
      Gen_Set (N, "Label", "justify", File);
   end Generate;

   procedure Generate (Label : in out Gtk_Label;
                       N     : in Node_Ptr) is
      use Misc;

      S : String_Ptr;

   begin
      if not N.Specific_Data.Created then
         Gtk_New (Label, Get_Field (N, "label").all);
         Set_Object (Get_Field (N, "name"), Label'Unchecked_Access);
         N.Specific_Data.Created := True;
      end if;

      Generate (Gtk_Misc (Label), N);

      S := Get_Field (N, "justify");

      if S /= null then
         Set_Justify (Label,
           Enums.Gtk_Justification'Value (S (S'First + 4 .. S'Last)));
      end if;
   end Generate;

end Gtk.Label;
