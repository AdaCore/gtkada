------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
--  Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel displays a keyboard
--  accelerator (i.e. a key combination like ['Control''a']. If the cell
--  renderer is editable, the accelerator can be changed by simply typing the
--  new combination.
--
--  The Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel cell renderer was
--  added in GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                   use Glib;
with Glib.Properties;        use Glib.Properties;
with Gtk.Cell_Renderer_Text; use Gtk.Cell_Renderer_Text;

package Gtk.Cell_Renderer_Accel is

   type Gtk_Cell_Renderer_Accel_Record is new Gtk_Cell_Renderer_Text_Record with null record;
   type Gtk_Cell_Renderer_Accel is access all Gtk_Cell_Renderer_Accel_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Accel);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Accel_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel.
   --  Since: gtk+ 2.10

   function Gtk_Cell_Renderer_Accel_New return Gtk_Cell_Renderer_Accel;
   --  Creates a new Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel.
   --  Since: gtk+ 2.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_accel_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Accel_Key_Property
   --  Type: Guint
   --  Flags: read-write
   --  The keyval of the accelerator.
   --
   --  Name: Accel_Mode_Property
   --  Type: Cell_Renderer_Accel_Mode
   --  Flags: read-write
   --  Determines if the edited accelerators are GTK+ accelerators. If they
   --  are, consumed modifiers are suppressed, only accelerators accepted by
   --  GTK+ are allowed, and the accelerators are rendered in the same way as
   --  they are in menus.
   --
   --  Name: Accel_Mods_Property
   --  Type: Gdk.Types.Gdk_Modifier_Type
   --  Flags: read-write
   --  The modifier mask of the accelerator.
   --
   --  Name: Keycode_Property
   --  Type: Guint
   --  Flags: read-write
   --  The hardware keycode of the accelerator. Note that the hardware keycode
   --  is only relevant if the key does not have a keyval. Normally, the
   --  keyboard configuration should assign keyvals to all keys.

   Accel_Key_Property : constant Glib.Properties.Property_Uint;
   Accel_Mode_Property : constant Glib.Properties.Property_Boxed;
   Accel_Mods_Property : constant Glib.Properties.Property_Boxed;
   Keycode_Property : constant Glib.Properties.Property_Uint;

   -------------
   -- Signals --
   -------------
   --  The following new signals are defined for this widget:
   --
   --  "accel-cleared"
   --     procedure Handler
   --       (Self        : access Gtk_Cell_Renderer_Accel_Record'Class;
   --        Path_String : UTF8_String);
   --    --  "path_string": the path identifying the row of the edited cell
   --  Gets emitted when the user has removed the accelerator.
   --
   --  "accel-edited"
   --     procedure Handler
   --       (Self             : access Gtk_Cell_Renderer_Accel_Record'Class;
   --        Path_String      : UTF8_String;
   --        Accel_Key        : Guint;
   --        Accel_Mods       : Gdk.Types.Gdk_Modifier_Type;
   --        Hardware_Keycode : Guint);
   --    --  "path_string": the path identifying the row of the edited cell
   --    --  "accel_key": the new accelerator keyval
   --    --  "accel_mods": the new acclerator modifier mask
   --    --  "hardware_keycode": the keycode of the new accelerator
   --  Gets emitted when the user has selected a new accelerator.

   Signal_Accel_Cleared : constant Glib.Signal_Name := "accel-cleared";
   Signal_Accel_Edited : constant Glib.Signal_Name := "accel-edited";

private
   Keycode_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("keycode");
   Accel_Mods_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("accel-mods");
   Accel_Mode_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("accel-mode");
   Accel_Key_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("accel-key");
end Gtk.Cell_Renderer_Accel;
