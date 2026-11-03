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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Accel_Group is

   --------------------------------------
   -- Accelerator_Get_Accessible_Label --
   --------------------------------------

   function Accelerator_Get_Accessible_Label
      (Accelerator_Key  : Guint;
       Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Guint;
          Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accelerator_get_accessible_label");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Get_Accessible_Label;

   ---------------------------
   -- Accelerator_Get_Label --
   ---------------------------

   function Accelerator_Get_Label
      (Accelerator_Key  : Guint;
       Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Guint;
          Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accelerator_get_label");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Get_Label;

   ----------------------
   -- Accelerator_Name --
   ----------------------

   function Accelerator_Name
      (Accelerator_Key  : Guint;
       Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type) return UTF8_String
   is
      function Internal
         (Accelerator_Key  : Guint;
          Accelerator_Mods : Gdk.Enums.Gdk_Modifier_Type)
          return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_accelerator_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Accelerator_Key, Accelerator_Mods));
   end Accelerator_Name;

   -----------------------
   -- Accelerator_Parse --
   -----------------------

   function Accelerator_Parse
      (Accelerator      : UTF8_String;
       Accelerator_Key  : access Guint := null;
       Accelerator_Mods : access Gdk.Enums.Gdk_Modifier_Type := null)
       return Boolean
   is
      function Internal
         (Accelerator      : Gtkada.Types.Chars_Ptr;
          Accelerator_Key  : access Guint;
          Accelerator_Mods : access Gdk.Enums.Gdk_Modifier_Type)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accelerator_parse");
      Tmp_Accelerator : Gtkada.Types.Chars_Ptr := New_String (Accelerator);
      Tmp_Return      : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Tmp_Accelerator, Accelerator_Key, Accelerator_Mods);
      Free (Tmp_Accelerator);
      return Tmp_Return /= 0;
   end Accelerator_Parse;

   -----------------------
   -- Accelerator_Valid --
   -----------------------

   function Accelerator_Valid
      (Keyval    : Guint;
       Modifiers : Gdk.Enums.Gdk_Modifier_Type) return Boolean
   is
      function Internal
         (Keyval    : Guint;
          Modifiers : Gdk.Enums.Gdk_Modifier_Type) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_accelerator_valid");
   begin
      return Internal (Keyval, Modifiers) /= 0;
   end Accelerator_Valid;

end Gtk.Accel_Group;
