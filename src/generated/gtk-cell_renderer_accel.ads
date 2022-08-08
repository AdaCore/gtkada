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
--  Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel displays a keyboard
--  accelerator (i.e. a key combination like `Control + a`). If the cell
--  renderer is editable, the accelerator can be changed by simply typing the
--  new combination.
--
--  The Gtk.Cell_Renderer_Accel.Gtk_Cell_Renderer_Accel cell renderer was
--  added in GTK+ 2.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;              use Gdk.Types;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
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
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

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

   Accel_Key_Property : constant Glib.Properties.Property_Uint;
   --  The keyval of the accelerator.

   Accel_Mode_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cell_Renderer_Accel_Mode
   --  Determines if the edited accelerators are GTK+ accelerators. If they
   --  are, consumed modifiers are suppressed, only accelerators accepted by
   --  GTK+ are allowed, and the accelerators are rendered in the same way as
   --  they are in menus.

   Accel_Mods_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Gdk.Types.Gdk_Modifier_Type
   --  The modifier mask of the accelerator.

   Keycode_Property : constant Glib.Properties.Property_Uint;
   --  The hardware keycode of the accelerator. Note that the hardware keycode
   --  is only relevant if the key does not have a keyval. Normally, the
   --  keyboard configuration should assign keyvals to all keys.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void is not null access procedure
     (Self        : access Gtk_Cell_Renderer_Accel_Record'Class;
      Path_String : UTF8_String);

   type Cb_GObject_UTF8_String_Void is not null access procedure
     (Self        : access Glib.Object.GObject_Record'Class;
      Path_String : UTF8_String);

   Signal_Accel_Cleared : constant Glib.Signal_Name := "accel-cleared";
   procedure On_Accel_Cleared
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Void;
       After : Boolean := False);
   procedure On_Accel_Cleared
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_GObject_UTF8_String_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user has removed the accelerator.

   type Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void is not null access procedure
     (Self             : access Gtk_Cell_Renderer_Accel_Record'Class;
      Path_String      : UTF8_String;
      Accel_Key        : Guint;
      Accel_Mods       : Gdk.Types.Gdk_Modifier_Type;
      Hardware_Keycode : Guint);

   type Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void is not null access procedure
     (Self             : access Glib.Object.GObject_Record'Class;
      Path_String      : UTF8_String;
      Accel_Key        : Guint;
      Accel_Mods       : Gdk.Types.Gdk_Modifier_Type;
      Hardware_Keycode : Guint);

   Signal_Accel_Edited : constant Glib.Signal_Name := "accel-edited";
   procedure On_Accel_Edited
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_Gtk_Cell_Renderer_Accel_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       After : Boolean := False);
   procedure On_Accel_Edited
      (Self  : not null access Gtk_Cell_Renderer_Accel_Record;
       Call  : Cb_GObject_UTF8_String_Guint_Gdk_Modifier_Type_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Gets emitted when the user has selected a new accelerator.
   -- 
   --  Callback parameters:
   --    --  "path_string": the path identifying the row of the edited cell
   --    --  "accel_key": the new accelerator keyval
   --    --  "accel_mods": the new acclerator modifier mask
   --    --  "hardware_keycode": the keycode of the new accelerator

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
