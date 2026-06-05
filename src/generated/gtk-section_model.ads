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

--  An interface that adds support for sections to list models.
--
--  A `GtkSectionModel` groups successive items into so-called sections. List
--  widgets like `GtkListView` and `GtkGridView` then allow displaying section
--  headers for these sections by installing a header factory.
--
--  Many GTK list models support sections inherently, or they pass through the
--  sections of a model they are wrapping.
--
--  When the section groupings of a model change, the model will emit the
--  [signalGtk.SectionModel::sections-changed] signal by calling the
--  [methodGtk.SectionModel.sections_changed] function. All sections in the
--  given range then need to be queried again. The
--  [signalGio.ListModel::items-changed] signal has the same effect, all
--  sections in that range are invalidated, too.

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;

package Gtk.Section_Model is

   type Gtk_Section_Model is new Glib.Types.GType_Interface;
   Null_Gtk_Section_Model : constant Gtk_Section_Model;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_section_model_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Section
      (Self      : Gtk_Section_Model;
       Position  : Guint;
       Out_Start : out Guint;
       Out_End   : out Guint);
   pragma Import (C, Get_Section, "gtk_section_model_get_section");
   --  Query the section that covers the given position. The number of items
   --  in the section can be computed by `out_end - out_start`.
   --  If the position is larger than the number of items, a single range from
   --  n_items to G_MAXUINT will be returned.
   --  Since: gtk+ 4.12
   --  @param Position the position of the item to query
   --  @param Out_Start the position of the first item in the section
   --  @param Out_End the position of the first item not part of the section
   --  anymore.

   procedure Sections_Changed
      (Self     : Gtk_Section_Model;
       Position : Guint;
       N_Items  : Guint);
   pragma Import (C, Sections_Changed, "gtk_section_model_sections_changed");
   --  This function emits the [signalGtk.SectionModel::sections-changed]
   --  signal to notify about changes to sections.
   --  It must cover all positions that used to be a section start or that are
   --  now a section start. It does not have to cover all positions for which
   --  the section has changed.
   --  The [signalGio.ListModel::items-changed] implies the effect of the
   --  [signalGtk.SectionModel::sections-changed] signal for all the items it
   --  covers.
   --  It is recommended that when changes to the items cause section changes
   --  in a larger range, that the larger range is included in the emission of
   --  the [signalGio.ListModel::items-changed] instead of emitting two
   --  signals.
   --  Since: gtk+ 4.12
   --  @param Position the first changed item
   --  @param N_Items the number of changed items

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Section_Model_Guint_Guint_Void is not null access procedure
     (Self     : Gtk_Section_Model;
      Position : Guint;
      N_Items  : Guint);

   type Cb_GObject_Guint_Guint_Void is not null access procedure
     (Self     : access Glib.Object.GObject_Record'Class;
      Position : Guint;
      N_Items  : Guint);

   Signal_Sections_Changed : constant Glib.Signal_Name := "sections-changed";
   procedure On_Sections_Changed
      (Self  : Gtk_Section_Model;
       Call  : Cb_Gtk_Section_Model_Guint_Guint_Void;
       After : Boolean := False);
   procedure On_Sections_Changed
      (Self  : Gtk_Section_Model;
       Call  : Cb_GObject_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the start-of-section state of some of the items in Model
   --  changes.
   --
   --  Note that this signal does not specify the new section state of the
   --  items, they need to be queried manually. It is also not necessary for a
   --  model to change the section state of any of the items in the section
   --  model, though it would be rather useless to emit such a signal.
   --
   --  The [signalGio.ListModel::items-changed] implies the effect of the
   --  [signalGtk.SectionModel::sections-changed] signal for all the items it
   --  covers.
   -- 
   --  Callback parameters:
   --    --  @param Position The first item that may have changed
   --    --  @param N_Items number of items with changes

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Section_Model"

   function "+" (W : Gtk_Section_Model) return Gtk_Section_Model;
   pragma Inline ("+");

   ---------------------
   -- Virtual Methods --
   ---------------------

   type Virtual_Get_Section is access procedure
     (Self      : Gtk_Section_Model;
      Position  : Guint;
      Out_Start : out Guint;
      Out_End   : out Guint);
   pragma Convention (C, Virtual_Get_Section);
   --  Query the section that covers the given position. The number of items
   --  in the section can be computed by `out_end - out_start`.
   --  If the position is larger than the number of items, a single range from
   --  n_items to G_MAXUINT will be returned.
   --  Since: gtk+ 4.12
   --  @param Position the position of the item to query
   --  @param Out_Start the position of the first item in the section
   --  @param Out_End the position of the first item not part of the section
   --  anymore.

   subtype Section_Model_Interface_Descr is Glib.Object.Interface_Description;

   procedure Set_Get_Section
     (Self    : Section_Model_Interface_Descr;
      Handler : Virtual_Get_Section);
   pragma Import (C, Set_Get_Section, "gtkada_Section_Model_set_get_section");
   --  See Glib.Object.Add_Interface

private

   Null_Gtk_Section_Model : constant Gtk_Section_Model :=
      Gtk_Section_Model (Glib.Types.Null_Interface);
end Gtk.Section_Model;
