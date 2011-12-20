------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       ----                     Copyright (C) 1998-2012, AdaCore                     --
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

package body Gtk.Plug is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Plug_Record);
   pragma Warnings (Off, Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Plug : out Gtk_Plug; Socket_Id : Guint32) is
   begin
      Plug := new Gtk_Plug_Record;
      Initialize (Plug, Socket_Id);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Plug : access Gtk_Plug_Record'Class; Socket_Id : Guint32)
   is
      function Internal (Socket_Id : Guint32) return System.Address;
      pragma Import (C, Internal, "gtk_plug_new");

   begin
      Set_Object (Plug, Internal (Socket_Id));
   end Initialize;

   ------------
   -- Get_Id --
   ------------

   function Get_Id (Plug : access Gtk_Plug_Record) return Guint32 is
      function Internal (Plug : System.Address) return Guint32;
      pragma Import (C, Internal, "gtk_plug_get_id");

   begin
      return Internal (Get_Object (Plug));
   end Get_Id;

end Gtk.Plug;
