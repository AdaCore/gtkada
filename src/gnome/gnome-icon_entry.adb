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

package body Gnome.Icon_Entry is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New
     (Widget              : out Gnome_Icon_Entry;
      History_Id          : String;
      Browse_Dialog_Title : String)
   is
   begin
      Widget := new Gnome_Icon_Entry_Record;
      Initialize (Widget, History_Id, Browse_Dialog_Title);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget              : access Gnome_Icon_Entry_Record'Class;
      History_Id          : String;
      Browse_Dialog_Title : String)
   is
      function Internal
        (History_Id          : String;
         Browse_Dialog_Title : String)
         return System.Address;
      pragma Import (C, Internal, "gnome_icon_entry_new");
   begin
      Set_Object (Widget, Internal (History_Id & ASCII.NUL,
                                    Browse_Dialog_Title & ASCII.NUL));
   end Initialize;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Ientry : access Gnome_Icon_Entry_Record) return String
   is
      function Internal
        (Ientry : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_icon_entry_get_filename");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Ientry)));
   end Get_Filename;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Ientry   : access Gnome_Icon_Entry_Record;
      Filename : String)
   is
      procedure Internal
        (Ientry   : System.Address;
         Filename : String);
      pragma Import (C, Internal, "gnome_icon_entry_set_filename");
   begin
      Internal (Get_Object (Ientry), Filename & ASCII.NUL);
   end Set_Filename;

   -----------------------
   -- Set_Pixmap_Subdir --
   -----------------------

   procedure Set_Pixmap_Subdir
     (Ientry : access Gnome_Icon_Entry_Record;
      Subdir : String)
   is
      procedure Internal
        (Ientry : System.Address;
         Subdir : String);
      pragma Import (C, Internal, "gnome_icon_entry_set_pixmap_subdir");
   begin
      Internal (Get_Object (Ientry),
                Subdir & ASCII.NUL);
   end Set_Pixmap_Subdir;

end Gnome.Icon_Entry;
