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

package body Gnome.Druid_Page is

   ----------
   -- Back --
   ----------

   function Back
     (Druid_Page : access Gnome_Druid_Page_Record) return Boolean
   is
      function Internal (Druid_Page : System.Address) return Gint;
      pragma Import (C, Internal, "gnome_druid_page_back");
   begin
      return Boolean'Val (Internal (Get_Object (Druid_Page)));
   end Back;

   ------------
   -- Cancel --
   ------------

   function Cancel (Druid_Page : access Gnome_Druid_Page_Record)
                    return Boolean
   is
      function Internal (Druid_Page : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gnome_druid_page_cancel");
   begin
      return Boolean'Val (Internal (Get_Object (Druid_Page)));
   end Cancel;

   ------------
   -- Finish --
   ------------

   procedure Finish (Druid_Page : access Gnome_Druid_Page_Record)
   is
      procedure Internal (Druid_Page : System.Address);
      pragma Import (C, Internal, "gnome_druid_page_finish");
   begin
      Internal (Get_Object (Druid_Page));
   end Finish;

   ----------
   -- Next --
   ----------

   function Next (Druid_Page : access Gnome_Druid_Page_Record)
                  return Boolean
   is
      function Internal (Druid_Page : System.Address)
                         return Gint;
      pragma Import (C, Internal, "gnome_druid_page_next");
   begin
      return Boolean'Val (Internal (Get_Object (Druid_Page)));
   end Next;

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Druid_Page : access Gnome_Druid_Page_Record)
   is
      procedure Internal (Druid_Page : System.Address);
      pragma Import (C, Internal, "gnome_druid_page_prepare");
   begin
      Internal (Get_Object (Druid_Page));
   end Prepare;

end Gnome.Druid_Page;
