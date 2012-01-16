------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with System;

with Glib.Type_Conversion_Hooks;

package body Gtk.Scrollbar is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Scrollbar_Record);
   pragma Warnings (Off, Type_Conversion);

   ------------------------
   -- Gtk_New_Hscrollbar --
   ------------------------

   procedure Gtk_New_Hscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget := new Gtk_Scrollbar_Record;
      Initialize_Hscrollbar (Widget, Adjustment);
   end Gtk_New_Hscrollbar;

   ------------------------
   -- Gtk_New_Vscrollbar --
   ------------------------

   procedure Gtk_New_Vscrollbar
     (Widget     : out Gtk_Scrollbar;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment) is
   begin
      Widget := new Gtk_Scrollbar_Record;
      Initialize_Vscrollbar (Widget, Adjustment);
   end Gtk_New_Vscrollbar;

   ---------------------------
   -- Initialize_Hscrollbar --
   ---------------------------

   procedure Initialize_Hscrollbar
     (Widget     : access Gtk_Scrollbar_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_hscrollbar_new");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Set_Object (Widget, Internal (Adj));
   end Initialize_Hscrollbar;

   ---------------------------
   -- Initialize_Vscrollbar --
   ---------------------------

   procedure Initialize_Vscrollbar
     (Widget     : access Gtk_Scrollbar_Record'Class;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment)
   is
      function Internal (Adjustment : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_vscrollbar_new");

      Adj : System.Address;

      use type Gtk.Adjustment.Gtk_Adjustment;

   begin
      if Adjustment = null then
         Adj := System.Null_Address;
      else
         Adj := Get_Object (Adjustment);
      end if;

      Set_Object (Widget, Internal (Adj));
   end Initialize_Vscrollbar;

end Gtk.Scrollbar;
