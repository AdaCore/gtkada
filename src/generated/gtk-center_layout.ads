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

--  Manages up to three children.
--
--  The start widget is allocated at the start of the layout (left in
--  left-to-right locales and right in right-to-left ones), and the end widget
--  at the end.
--
--  The center widget is centered regarding the full width of the layout's.
--
--  <group>Layout containers</group>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Properties;    use Glib.Properties;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Layout_Manager; use Gtk.Layout_Manager;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Center_Layout is

   type Gtk_Center_Layout_Record is new Gtk_Layout_Manager_Record with null record;
   type Gtk_Center_Layout is access all Gtk_Center_Layout_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Center_Layout : out Gtk_Center_Layout);
   procedure Initialize
      (Center_Layout : not null access Gtk_Center_Layout_Record'Class);
   --  Creates a new `GtkCenterLayout`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Center_Layout_New return Gtk_Center_Layout;
   --  Creates a new `GtkCenterLayout`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_center_layout_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Baseline_Position
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Enums.Gtk_Baseline_Position;
   --  Returns the baseline position of the layout.
   --  @return The current baseline position of Self.

   procedure Set_Baseline_Position
      (Center_Layout     : not null access Gtk_Center_Layout_Record;
       Baseline_Position : Gtk.Enums.Gtk_Baseline_Position);
   --  Sets the new baseline position of Self
   --  @param Baseline_Position the new baseline position

   function Get_Center_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the center widget of the layout.
   --  @return the current center widget of Self

   procedure Set_Center_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the new center widget of Self.
   --  To remove the existing center widget, pass null.
   --  @param Widget the new center widget

   function Get_End_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the end widget of the layout.
   --  @return the current end widget of Self

   procedure Set_End_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the new end widget of Self.
   --  To remove the existing center widget, pass null.
   --  @param Widget the new end widget

   function Get_Orientation
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Enums.Gtk_Orientation;
   --  Gets the current orienration of the layout manager.
   --  @return The current orientation of Self

   procedure Set_Orientation
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Orientation   : Gtk.Enums.Gtk_Orientation);
   --  Sets the orientation of Self.
   --  @param Orientation the new orientation

   function Get_Shrink_Center_Last
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Boolean;
   --  Gets whether Self shrinks the center widget after other children.
   --  Since: gtk+ 4.12
   --  @return whether to shrink the center widget after others

   procedure Set_Shrink_Center_Last
      (Center_Layout      : not null access Gtk_Center_Layout_Record;
       Shrink_Center_Last : Boolean);
   --  Sets whether to shrink the center widget after other children.
   --  By default, when there's no space to give all three children their
   --  natural widths, the start and end widgets start shrinking and the center
   --  child keeps natural width until they reach minimum width.
   --  If set to `FALSE`, start and end widgets keep natural width and the
   --  center widget starts shrinking instead.
   --  Since: gtk+ 4.12
   --  @param Shrink_Center_Last whether to shrink the center widget after
   --  others

   function Get_Start_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the start widget of the layout.
   --  @return The current start widget of Self

   procedure Set_Start_Widget
      (Center_Layout : not null access Gtk_Center_Layout_Record;
       Widget        : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Sets the new start widget of Self.
   --  To remove the existing start widget, pass null.
   --  @param Widget the new start widget

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Shrink_Center_Last_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to shrink the center widget after other children.
   --
   --  By default, when there's no space to give all three children their
   --  natural widths, the start and end widgets start shrinking and the center
   --  child keeps natural width until they reach minimum width.
   --
   --  If set to `FALSE`, start and end widgets keep natural width and the
   --  center widget starts shrinking instead.

private
   Shrink_Center_Last_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("shrink-center-last");
end Gtk.Center_Layout;
