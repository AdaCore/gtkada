-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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

package body Gtk.Old_Editable is

   -------------
   -- Changed --
   -------------

   procedure Changed (Editable : access Gtk_Old_Editable_Record) is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_old_editable_changed");

   begin
      Internal (Get_Object (Editable));
   end Changed;

   ---------------------
   -- Claim_Selection --
   ---------------------

   procedure Claim_Selection
     (Editable : access Gtk_Old_Editable_Record;
      Claim    : Boolean := True;
      Time     : Guint32)
   is
      procedure Internal
        (Editable : System.Address;
         Claim    : Gint;
         Time     : Guint32);
      pragma Import (C, Internal, "gtk_old_editable_claim_selection");

   begin
      Internal (Get_Object (Editable), Boolean'Pos (Claim), Time);
   end Claim_Selection;

end Gtk.Old_Editable;
