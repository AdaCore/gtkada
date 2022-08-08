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
--  Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress renders a numeric
--  value as a progress par in a cell. Additionally, it can display a text on
--  top of the progress bar.
--
--  The Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress cell renderer
--  was added in GTK+ 2.6.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Glib.Types;        use Glib.Types;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Orientable;    use Gtk.Orientable;

package Gtk.Cell_Renderer_Progress is

   type Gtk_Cell_Renderer_Progress_Record is new Gtk_Cell_Renderer_Record with null record;
   type Gtk_Cell_Renderer_Progress is access all Gtk_Cell_Renderer_Progress_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Progress);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Progress_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress.
   --  Since: gtk+ 2.6
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Progress_New return Gtk_Cell_Renderer_Progress;
   --  Creates a new Gtk.Cell_Renderer_Progress.Gtk_Cell_Renderer_Progress.
   --  Since: gtk+ 2.6

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_progress_get_type");

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   function Get_Orientation
      (Self : not null access Gtk_Cell_Renderer_Progress_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Cell_Renderer_Progress_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Inverted_Property : constant Glib.Properties.Property_Boolean;

   Pulse_Property : constant Glib.Properties.Property_Int;
   --  Setting this to a non-negative value causes the cell renderer to enter
   --  "activity mode", where a block bounces back and forth to indicate that
   --  some progress is made, without specifying exactly how much.
   --
   --  Each increment of the property causes the block to move by a little
   --  bit.
   --
   --  To indicate that the activity has not started yet, set the property to
   --  zero. To indicate completion, set the property to G_MAXINT.

   Text_Property : constant Glib.Properties.Property_String;
   --  The "text" property determines the label which will be drawn over the
   --  progress bar. Setting this property to null causes the default label to
   --  be displayed. Setting this property to an empty string causes no label
   --  to be displayed.

   Text_Xalign_Property : constant Glib.Properties.Property_Float;
   --  The "text-xalign" property controls the horizontal alignment of the
   --  text in the progress bar. Valid values range from 0 (left) to 1 (right).
   --  Reserved for RTL layouts.

   Text_Yalign_Property : constant Glib.Properties.Property_Float;
   --  The "text-yalign" property controls the vertical alignment of the text
   --  in the progress bar. Valid values range from 0 (top) to 1 (bottom).

   Value_Property : constant Glib.Properties.Property_Int;
   --  The "value" property determines the percentage to which the progress
   --  bar will be "filled in".

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Orientable"

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Cell_Renderer_Progress_Record, Gtk_Cell_Renderer_Progress);
   function "+"
     (Widget : access Gtk_Cell_Renderer_Progress_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Cell_Renderer_Progress
   renames Implements_Gtk_Orientable.To_Object;

private
   Value_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("value");
   Text_Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("text-yalign");
   Text_Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("text-xalign");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Pulse_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("pulse");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
end Gtk.Cell_Renderer_Progress;
