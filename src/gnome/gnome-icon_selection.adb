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

with Glib; use Glib;
with Gtk; use Gtk;
with System;

package body Gnome.Icon_Selection is

   ---------------
   -- Gnome_New --
   ---------------

   procedure Gnome_New (Widget : out Gnome_Icon_Selection) is
   begin
      Widget := new Gnome_Icon_Selection_Record;
      Gnome.Icon_Selection.Initialize (Widget);
   end Gnome_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Widget : access Gnome_Icon_Selection_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gnome_icon_selection_new");
   begin
      Set_Object (Widget, Internal);
   end Initialize;

   ------------------
   -- Add_Defaults --
   ------------------

   procedure Add_Defaults (Gis : access Gnome_Icon_Selection_Record) is
      procedure Internal (Gis : System.Address);
      pragma Import (C, Internal, "gnome_icon_selection_add_defaults");
   begin
      Internal (Get_Object (Gis));
   end Add_Defaults;

   -------------------
   -- Add_Directory --
   -------------------

   procedure Add_Directory
     (Gis : access Gnome_Icon_Selection_Record;
      Dir : String)
   is
      procedure Internal
        (Gis : System.Address;
         Dir : String);
      pragma Import (C, Internal, "gnome_icon_selection_add_directory");
   begin
      Internal (Get_Object (Gis), Dir & ASCII.NUL);
   end Add_Directory;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Gis       : access Gnome_Icon_Selection_Record;
      Not_Shown : Boolean)
   is
      procedure Internal
        (Gis       : System.Address;
         Not_Shown : Gint);
      pragma Import (C, Internal, "gnome_icon_selection_clear");
   begin
      Internal (Get_Object (Gis),
                Boolean'Pos (Not_Shown));
   end Clear;

   -----------------
   -- Select_Icon --
   -----------------

   procedure Select_Icon
     (Gis      : access Gnome_Icon_Selection_Record;
      Filename : String)
   is
      procedure Internal
        (Gis      : System.Address;
         Filename : String);
      pragma Import (C, Internal, "gnome_icon_selection_select_icon");
   begin
      Internal (Get_Object (Gis), Filename & ASCII.NUL);
   end Select_Icon;

   ----------------
   -- Show_Icons --
   ----------------

   procedure Show_Icons (Gis : access Gnome_Icon_Selection_Record) is
      procedure Internal (Gis : System.Address);
      pragma Import (C, Internal, "gnome_icon_selection_show_icons");
   begin
      Internal (Get_Object (Gis));
   end Show_Icons;

   ------------------
   -- Stop_Loading --
   ------------------

   procedure Stop_Loading (Gis : access Gnome_Icon_Selection_Record) is
      procedure Internal (Gis : System.Address);
      pragma Import (C, Internal, "gnome_icon_selection_stop_loading");
   begin
      Internal (Get_Object (Gis));
   end Stop_Loading;

end Gnome.Icon_Selection;
