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

--  Places child widgets at fixed positions.
--
--  Most applications should never use this layout manager; fixed positioning
--  and sizing requires constant recalculations on where children need to be
--  positioned and sized. Other layout managers perform this kind of work
--  internally so that application developers don't need to do it.
--  Specifically, widgets positioned in a fixed layout manager will need to
--  take into account:
--
--  - Themes, which may change widget sizes.
--
--  - Fonts other than the one you used to write the app will of course change
--  the size of widgets containing text; keep in mind that users may use a
--  larger font because of difficulty reading the default, or they may be using
--  a different OS that provides different fonts.
--
--  - Translation of text into other languages changes its size. Also, display
--  of non-English text will use a different font in many cases.
--
--  In addition, `GtkFixedLayout` does not pay attention to text direction and
--  thus may produce unwanted results if your app is run under right-to-left
--  languages such as Hebrew or Arabic. That is: normally GTK will order
--  containers appropriately depending on the text direction, e.g. to put
--  labels to the right of the thing they label when using an RTL language;
--  `GtkFixedLayout` won't be able to do that for you.
--
--  Finally, fixed positioning makes it kind of annoying to add/remove UI
--  elements, since you have to reposition all the other elements. This is a
--  long-term maintenance problem for your application.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Properties;    use Glib.Properties;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;

package Gtk.Fixed_Layout is

   type Gtk_Fixed_Layout_Record is new Gtk_Layout_Manager_Record with null record;
   type Gtk_Fixed_Layout is access all Gtk_Fixed_Layout_Record'Class;

   type Gtk_Fixed_Layout_Child_Record is new Gtk_Layout_Child_Record with null record;
   type Gtk_Fixed_Layout_Child is access all Gtk_Fixed_Layout_Child_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Fixed_Layout : out Gtk_Fixed_Layout);
   procedure Initialize
      (Fixed_Layout : not null access Gtk_Fixed_Layout_Record'Class);
   --  Creates a new `GtkFixedLayout`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Fixed_Layout_New return Gtk_Fixed_Layout;
   --  Creates a new `GtkFixedLayout`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_fixed_layout_get_type");

   function Fixed_Layout_Child_Get_Type return Glib.GType;
   pragma Import (C, Fixed_Layout_Child_Get_Type, "gtk_fixed_layout_child_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Transform_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gsk.Transform
   --  The transform of the child.

private
   Transform_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("transform");
end Gtk.Fixed_Layout;
