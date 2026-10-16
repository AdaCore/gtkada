------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Symbolic_Paintable is

   -----------------------
   -- Snapshot_Symbolic --
   -----------------------

   procedure Snapshot_Symbolic
     (Self     : Gtk_Symbolic_Paintable;
      Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
      Width    : Gdouble;
      Height   : Gdouble;
      Colors   : array_of_Gdk_RGBA)
   is
      procedure Internal
        (Self     : Gtk_Symbolic_Paintable;
         Snapshot : System.Address;
         Width    : Gdouble;
         Height   : Gdouble;
         Colors   : System.Address;
         N_Colors : Gsize);
      pragma Import (C, Internal, "gtk_symbolic_paintable_snapshot_symbolic");
   begin
      Internal (Self, Get_Object (Snapshot), Width, Height, Colors'Address, Gsize (Colors'Length));
   end Snapshot_Symbolic;

   --------------------------
   -- Snapshot_With_Weight --
   --------------------------

   procedure Snapshot_With_Weight
     (Self     : Gtk_Symbolic_Paintable;
      Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
      Width    : Gdouble;
      Height   : Gdouble;
      Colors   : array_of_Gdk_RGBA;
      Weight   : Gdouble)
   is
      procedure Internal
        (Self     : Gtk_Symbolic_Paintable;
         Snapshot : System.Address;
         Width    : Gdouble;
         Height   : Gdouble;
         Colors   : System.Address;
         N_Colors : Gsize;
         Weight   : Gdouble);
      pragma Import (C, Internal, "gtk_symbolic_paintable_snapshot_with_weight");
   begin
      Internal (Self, Get_Object (Snapshot), Width, Height, Colors'Address, Gsize (Colors'Length), Weight);
   end Snapshot_With_Weight;

   function "+" (W : Gtk_Symbolic_Paintable) return Gtk_Symbolic_Paintable is
   begin
      return W;
   end "+";

end Gtk.Symbolic_Paintable;
