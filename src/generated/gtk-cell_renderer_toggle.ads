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
--  Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle renders a toggle button
--  in a cell. The button is drawn as a radio or a checkbutton, depending on
--  the Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle:radio property. When
--  activated, it emits the
--  Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle::toggled signal.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Properties;   use Glib.Properties;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Toggle is

   type Gtk_Cell_Renderer_Toggle_Record is new Gtk_Cell_Renderer_Record with null record;
   type Gtk_Cell_Renderer_Toggle is access all Gtk_Cell_Renderer_Toggle_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Toggle);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle. Adjust
   --  rendering parameters using object properties. Object properties can be
   --  set globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "active" property on the cell renderer to a boolean value in the model,
   --  thus causing the check button to reflect the state of the model.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Toggle_New return Gtk_Cell_Renderer_Toggle;
   --  Creates a new Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle. Adjust
   --  rendering parameters using object properties. Object properties can be
   --  set globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "active" property on the cell renderer to a boolean value in the model,
   --  thus causing the check button to reflect the state of the model.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_toggle_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Activatable
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean;
   --  Returns whether the cell renderer is activatable. See
   --  Gtk.Cell_Renderer_Toggle.Set_Activatable.
   --  Since: gtk+ 2.18

   procedure Set_Activatable
      (Self    : not null access Gtk_Cell_Renderer_Toggle_Record;
       Setting : Boolean);
   --  Makes the cell renderer activatable.
   --  Since: gtk+ 2.18
   --  "setting": the value to set.

   function Get_Active
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean;
   --  Returns whether the cell renderer is active. See
   --  Gtk.Cell_Renderer_Toggle.Set_Active.

   procedure Set_Active
      (Self    : not null access Gtk_Cell_Renderer_Toggle_Record;
       Setting : Boolean);
   --  Activates or deactivates a cell renderer.
   --  "setting": the value to set.

   function Get_Radio
      (Self : not null access Gtk_Cell_Renderer_Toggle_Record)
       return Boolean;
   --  Returns whether we're rendering radio toggles rather than checkboxes.

   procedure Set_Radio
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Radio : Boolean);
   --  If Radio is True, the cell renderer renders a radio toggle (i.e. a
   --  toggle in a group of mutually-exclusive toggles). If False, it renders a
   --  check toggle (a standalone boolean option). This can be set globally for
   --  the cell renderer, or changed just before rendering each cell in the
   --  model (for Gtk.Tree_View.Gtk_Tree_View, you set up a per-row setting
   --  using Gtk.Tree_View_Column.Gtk_Tree_View_Column to associate model
   --  columns with cell renderer properties).
   --  "radio": True to make the toggle look like a radio button

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Activatable_Property : constant Glib.Properties.Property_Boolean;

   Active_Property : constant Glib.Properties.Property_Boolean;

   Inconsistent_Property : constant Glib.Properties.Property_Boolean;

   Indicator_Size_Property : constant Glib.Properties.Property_Int;

   Radio_Property : constant Glib.Properties.Property_Boolean;

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void is not null access procedure
     (Self : access Gtk_Cell_Renderer_Toggle_Record'Class;
      Path : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      Path : UTF8_String);

   Signal_Toggled : constant Glib.Signal_Name := "toggled";
   procedure On_Toggled
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Call  : Cb_Gtk_Cell_Renderer_Toggle_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Toggled
      (Self  : not null access Gtk_Cell_Renderer_Toggle_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  The ::toggled signal is emitted when the cell is toggled.
   --
   --  It is the responsibility of the application to update the model with
   --  the correct value to store at Path. Often this is simply the opposite of
   --  the value currently stored at Path.

private
   Radio_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("radio");
   Indicator_Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("indicator-size");
   Inconsistent_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inconsistent");
   Active_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("active");
   Activatable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("activatable");
end Gtk.Cell_Renderer_Toggle;
