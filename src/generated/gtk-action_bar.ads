------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

--  <description>
--  GtkActionBar is designed to present contextual actions. It is expected to
--  be displayed below the content and expand horizontally to fill the area.
--
--  It allows placing children at the start or the end. In addition, it
--  contains an internal centered box which is centered with respect to the
--  full width of the box, even if the children at either side take up
--  different amounts of space.
--
--  # CSS nodes
--
--  GtkActionBar has a single CSS node with name actionbar.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Bin;       use Gtk.Bin;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Action_Bar is

   type Gtk_Action_Bar_Record is new Gtk_Bin_Record with null record;
   type Gtk_Action_Bar is access all Gtk_Action_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Action_Bar);
   procedure Initialize (Self : not null access Gtk_Action_Bar_Record'Class);
   --  Creates a new Gtk.Action_Bar.Gtk_Action_Bar widget.
   --  Since: gtk+ 3.12
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Action_Bar_New return Gtk_Action_Bar;
   --  Creates a new Gtk.Action_Bar.Gtk_Action_Bar widget.
   --  Since: gtk+ 3.12

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_action_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Center_Widget
      (Self : not null access Gtk_Action_Bar_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Retrieves the center bar widget of the bar.
   --  Since: gtk+ 3.12

   procedure Set_Center_Widget
      (Self          : not null access Gtk_Action_Bar_Record;
       Center_Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the center widget for the Gtk.Action_Bar.Gtk_Action_Bar.
   --  Since: gtk+ 3.12
   --  "center_widget": a widget to use for the center

   procedure Pack_End
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Child to Action_Bar, packed with reference to the end of the
   --  Action_Bar.
   --  Since: gtk+ 3.12
   --  "child": the Gtk.Widget.Gtk_Widget to be added to Action_Bar

   procedure Pack_Start
      (Self  : not null access Gtk_Action_Bar_Record;
       Child : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Adds Child to Action_Bar, packed with reference to the start of the
   --  Action_Bar.
   --  Since: gtk+ 3.12
   --  "child": the Gtk.Widget.Gtk_Widget to be added to Action_Bar

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Action_Bar_Record, Gtk_Action_Bar);
   function "+"
     (Widget : access Gtk_Action_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Action_Bar
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Action_Bar;
