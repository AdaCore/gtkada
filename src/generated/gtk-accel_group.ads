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


pragma Warnings (Off, "*is already use-visible*");
with Gdk.Enums; use Gdk.Enums;

package Gtk.Accel_Group is

   ---------------
   -- Functions --
   ---------------

   function Accelerator_Get_Accessible_Label
      (Accelerator_Key  : Guint;
       Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type) return UTF8_String;
   --  Generates an accessible description of an accelerator.
   --  This function is similar to [funcGtk.accelerator_get_label] but it is
   --  meant for accessibility layers labels rather than user-facing labels.
   --  The output of this function is fit for
   --  [enumGtk.AccessibleProperty.KEY_SHORTCUTS].
   --  For more information, see the
   --  [WAI-ARIA](https://www.w3.org/TR/wai-aria/aria-keyshortcuts) reference.
   --  Since: gtk+ 4.22
   --  @param Accelerator_Key accelerator keyval
   --  @param Accelerator_Mods accelerator modifier mask
   --  @return a newly-allocated string representing the accelerator

   function Accelerator_Get_Default_Mod_Mask return Gdk.Enums.Gdk_Modifier_Type;
   pragma Import (C, Accelerator_Get_Default_Mod_Mask, "gtk_accelerator_get_default_mod_mask");
   --  Gets the modifier mask.
   --  The modifier mask determines which modifiers are considered significant
   --  for keyboard accelerators. This includes all keyboard modifiers except
   --  for `GDK_LOCK_MASK`.
   --  @return the modifier mask for accelerators

   function Accelerator_Get_Label
      (Accelerator_Key  : Guint;
       Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type) return UTF8_String;
   --  Converts an accelerator keyval and modifier mask into a string which
   --  can be used to represent the accelerator to the user.
   --  @param Accelerator_Key accelerator keyval
   --  @param Accelerator_Mods accelerator modifier mask
   --  @return a newly-allocated string representing the accelerator

   function Accelerator_Name
      (Accelerator_Key  : Guint;
       Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type) return UTF8_String;
   --  Converts an accelerator keyval and modifier mask into a string that can
   --  be parsed by [funcGtk.accelerator_parse].
   --  For example, if you pass in `GDK_KEY_q` and `GDK_CONTROL_MASK`, this
   --  function returns `<Control>q`.
   --  If you need to display accelerators in the user interface, see
   --  [funcGtk.accelerator_get_label].
   --  @param Accelerator_Key accelerator keyval
   --  @param Accelerator_Mods accelerator modifier mask
   --  @return a newly-allocated accelerator name

   function Accelerator_Parse
      (Accelerator      : UTF8_String;
       Accelerator_Key  : access Guint := null;
       Accelerator_Mods : access Gdk.Enums.Gdk_Modifier_Type := null)
       return Boolean;
   --  Parses a string representing an accelerator.
   --  The format looks like "`<Control>a`" or "`<Shift><Alt>F1`".
   --  The parser is fairly liberal and allows lower or upper case, and also
   --  abbreviations such as "`<Ctl>`" and "`<Ctrl>`".
   --  Key names are parsed using [funcGdk.keyval_from_name]. For character
   --  keys the name is not the symbol, but the lowercase name, e.g. one would
   --  use "`<Ctrl>minus`" instead of "`<Ctrl>-`".
   --  Modifiers are enclosed in angular brackets `<>`, and match the
   --  [flagsGdk.ModifierType] mask:
   --  - `<Shift>` for `GDK_SHIFT_MASK` - `<Ctrl>` for `GDK_CONTROL_MASK` -
   --  `<Alt>` for `GDK_ALT_MASK` - `<Meta>` for `GDK_META_MASK` - `<Super>`
   --  for `GDK_SUPER_MASK` - `<Hyper>` for `GDK_HYPER_MASK`
   --  If the parse operation fails, Accelerator_Key and Accelerator_Mods will
   --  be set to 0 (zero).
   --  @param Accelerator string representing an accelerator
   --  @param Accelerator_Key return location for accelerator keyval
   --  @param Accelerator_Mods return location for accelerator modifier mask
   --  @return whether parsing succeeded

   function Accelerator_Valid
      (Keyval    : Guint;
       Modifiers : Gdk.Enums.Gdk_Modifier_Type) return Boolean;
   --  Determines whether a given keyval and modifier mask constitute a valid
   --  keyboard accelerator.
   --  For example, the `GDK_KEY_a` keyval plus `GDK_CONTROL_MASK` mask is
   --  valid, and matches the "Ctrl+a" accelerator. But, you can't, for
   --  instance, use the `GDK_KEY_Control_L` keyval as an accelerator.
   --  @param Keyval a GDK keyval
   --  @param Modifiers modifier mask
   --  @return true if the accelerator is valid

end Gtk.Accel_Group;
