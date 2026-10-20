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

--  An interface that supports symbolic colors in paintables.
--
--  `GdkPaintable`s implementing the interface will have the
--  [vfuncGtk.SymbolicPaintable.snapshot_symbolic] function called and have the
--  colors for drawing symbolic icons passed. At least 5 colors are guaranteed
--  to be passed every time. These 5 colors are the foreground color, and the
--  colors to use for errors, warnings and success information in that order,
--  followed by the system accent color.
--
--  The system accent color has been added in GTK 4.22. More colors may be
--  added in the future.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;     use Gdk.RGBA;
with Gdk.Snapshot; use Gdk.Snapshot;
with Glib;         use Glib;
with Glib.Object;  use Glib.Object;
with Glib.Types;   use Glib.Types;

package Gtk.Symbolic_Paintable is

   type Gtk_Symbolic_Paintable is new Glib.Types.GType_Interface;
   Null_Gtk_Symbolic_Paintable : constant Gtk_Symbolic_Paintable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_symbolic_paintable_get_type");

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Snapshot_Symbolic
     (Self     : Gtk_Symbolic_Paintable;
      Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
      Width    : Gdouble;
      Height   : Gdouble;
      Colors   : array_of_Gdk_RGBA);
   --  Snapshots the paintable with the given colors.
   --  If less than 5 colors are provided, GTK will pad the array with default
   --  colors.
   --  Since: gtk+ 4.6
   --  @param Snapshot a `GdkSnapshot` to snapshot to
   --  @param Width width to snapshot in
   --  @param Height height to snapshot in
   --  @param Colors a pointer to an array of colors

   procedure Snapshot_With_Weight
     (Self     : Gtk_Symbolic_Paintable;
      Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
      Width    : Gdouble;
      Height   : Gdouble;
      Colors   : array_of_Gdk_RGBA;
      Weight   : Gdouble);
   --  Snapshots the paintable with the given colors and weight.
   --  If less than 5 colors are provided, GTK will pad the array with default
   --  colors.
   --  Since: gtk+ 4.22
   --  @param Snapshot a `GdkSnapshot` to snapshot to
   --  @param Width width to snapshot in
   --  @param Height height to snapshot in
   --  @param Colors a pointer to an array of colors
   --  @param Weight The font weight to use (from 1 to 1000, with default 400)

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Symbolic_Paintable"

   function "+" (W : Gtk_Symbolic_Paintable) return Gtk_Symbolic_Paintable;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Snapshot_Symbolic is access procedure
     (Self     : Gtk_Symbolic_Paintable;
      Snapshot : System.Address;
      Width    : Gdouble;
      Height   : Gdouble;
      Colors   : System.Address;
      N_Colors : Gsize);
   pragma Convention (C, Virtual_Snapshot_Symbolic);
   --  Snapshots the paintable with the given colors.
   --  If less than 5 colors are provided, GTK will pad the array with default
   --  colors.
   --  Since: gtk+ 4.6
   --  @param Snapshot a `GdkSnapshot` to snapshot to
   --  @param Width width to snapshot in
   --  @param Height height to snapshot in
   --  @param Colors a pointer to an array of colors
   --  @param N_Colors The number of colors

   type Virtual_Snapshot_With_Weight is access procedure
     (Self     : Gtk_Symbolic_Paintable;
      Snapshot : System.Address;
      Width    : Gdouble;
      Height   : Gdouble;
      Colors   : System.Address;
      N_Colors : Gsize;
      Weight   : Gdouble);
   pragma Convention (C, Virtual_Snapshot_With_Weight);
   --  Snapshots the paintable with the given colors and weight.
   --  If less than 5 colors are provided, GTK will pad the array with default
   --  colors.
   --  Since: gtk+ 4.22
   --  @param Snapshot a `GdkSnapshot` to snapshot to
   --  @param Width width to snapshot in
   --  @param Height height to snapshot in
   --  @param Colors a pointer to an array of colors
   --  @param N_Colors The number of colors
   --  @param Weight The font weight to use (from 1 to 1000, with default 400)

   subtype Symbolic_Paintable_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Snapshot_Symbolic
     (Self    : Symbolic_Paintable_Interface_Descr;
      Handler : Virtual_Snapshot_Symbolic);
   pragma Import (C, Set_Snapshot_Symbolic, "gtkada_Symbolic_Paintable_set_snapshot_symbolic");

   procedure Set_Snapshot_With_Weight
     (Self    : Symbolic_Paintable_Interface_Descr;
      Handler : Virtual_Snapshot_With_Weight);
   pragma Import (C, Set_Snapshot_With_Weight, "gtkada_Symbolic_Paintable_set_snapshot_with_weight");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Symbolic_Paintable : constant Gtk_Symbolic_Paintable :=
      Gtk_Symbolic_Paintable (Glib.Types.Null_Interface);
end Gtk.Symbolic_Paintable;
