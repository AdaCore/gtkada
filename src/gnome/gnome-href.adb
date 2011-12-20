------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk; use Gtk;
with Interfaces.C.Strings;
with System;

package body Gnome.HRef is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget : out Gnome_HRef;
      Url    : String;
      Label  : String) is
   begin
      Widget := new Gnome_HRef_Record;
      Initialize (Widget, Url, Label);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Gnome_HRef_Record'Class;
      Url    : String;
      Label  : String)
   is
      function Internal
        (Url    : String;
         Label  : String) return System.Address;
      pragma Import (C, Internal, "gnome_href_new");
   begin
      Set_Object (Widget, Internal (Url & ASCII.NUL, Label & ASCII.NUL));
   end Initialize;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Href : access Gnome_HRef_Record) return String is
      function Internal
        (Href : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_href_get_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Href)));
   end Get_Text;

   -------------
   -- Get_Url --
   -------------

   function Get_Url (Href : access Gnome_HRef_Record) return String is
      function Internal
        (Href : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_href_get_url");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Href)));
   end Get_Url;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Href : access Gnome_HRef_Record;
      Text : String)
   is
      procedure Internal (Href : System.Address; Text : String);
      pragma Import (C, Internal, "gnome_href_set_text");
   begin
      Internal (Get_Object (Href), Text & ASCII.NUL);
   end Set_Text;

   -------------
   -- Set_Url --
   -------------

   procedure Set_Url (Href : access Gnome_HRef_Record; Url : String) is
      procedure Internal (Href : System.Address; Url : String);
      pragma Import (C, Internal, "gnome_href_set_url");
   begin
      Internal (Get_Object (Href), Url & ASCII.NUL);
   end Set_Url;

end Gnome.HRef;
