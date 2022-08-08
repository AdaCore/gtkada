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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");

package body Gtk.Binding_Set is

   function From_Object_Free (B : access Gtk_Binding_Set) return Gtk_Binding_Set is
      Result : constant Gtk_Binding_Set := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   -------------------------
   -- Gtk_Binding_Set_New --
   -------------------------

   function Gtk_Binding_Set_New
      (Set_Name : UTF8_String) return Gtk_Binding_Set
   is
      function Internal
         (Set_Name : Gtkada.Types.Chars_Ptr) return Gtk_Binding_Set;
      pragma Import (C, Internal, "gtk_binding_set_new");
      Tmp_Set_Name : Gtkada.Types.Chars_Ptr := New_String (Set_Name);
      Tmp_Return   : Gtk_Binding_Set;
      Self         : Gtk_Binding_Set;
   begin
      Tmp_Return := Internal (Tmp_Set_Name);
      Free (Tmp_Set_Name);
      Self := Tmp_Return;
      return Self;
   end Gtk_Binding_Set_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Binding_Set; Set_Name : UTF8_String) is
      function Internal
         (Set_Name : Gtkada.Types.Chars_Ptr) return Gtk_Binding_Set;
      pragma Import (C, Internal, "gtk_binding_set_new");
      Tmp_Set_Name : Gtkada.Types.Chars_Ptr := New_String (Set_Name);
      Tmp_Return   : Gtk_Binding_Set;
   begin
      Tmp_Return := Internal (Tmp_Set_Name);
      Free (Tmp_Set_Name);
      Self := Tmp_Return;
   end Gtk_New;

   --------------
   -- Activate --
   --------------

   function Activate
      (Self      : Gtk_Binding_Set;
       Keyval    : Guint;
       Modifiers : Gdk.Types.Gdk_Modifier_Type;
       Object    : not null access Glib.Object.GObject_Record'Class)
       return Boolean
   is
      function Internal
         (Self      : Gtk_Binding_Set;
          Keyval    : Guint;
          Modifiers : Gdk.Types.Gdk_Modifier_Type;
          Object    : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_binding_set_activate");
   begin
      return Internal (Self, Keyval, Modifiers, Get_Object (Object)) /= 0;
   end Activate;

   --------------
   -- Add_Path --
   --------------

   procedure Add_Path
      (Self         : Gtk_Binding_Set;
       Path_Type    : Gtk.Enums.Gtk_Path_Type;
       Path_Pattern : UTF8_String;
       Priority     : Gtk.Enums.Gtk_Path_Priority_Type)
   is
      procedure Internal
         (Self         : Gtk_Binding_Set;
          Path_Type    : Gtk.Enums.Gtk_Path_Type;
          Path_Pattern : Gtkada.Types.Chars_Ptr;
          Priority     : Gtk.Enums.Gtk_Path_Priority_Type);
      pragma Import (C, Internal, "gtk_binding_set_add_path");
      Tmp_Path_Pattern : Gtkada.Types.Chars_Ptr := New_String (Path_Pattern);
   begin
      Internal (Self, Path_Type, Tmp_Path_Pattern, Priority);
      Free (Tmp_Path_Pattern);
   end Add_Path;

   ----------
   -- Find --
   ----------

   function Find (Set_Name : UTF8_String) return Gtk_Binding_Set is
      function Internal
         (Set_Name : Gtkada.Types.Chars_Ptr) return access Gtk_Binding_Set;
      pragma Import (C, Internal, "gtk_binding_set_find");
      Tmp_Set_Name : Gtkada.Types.Chars_Ptr := New_String (Set_Name);
      Tmp_Return   : access Gtk_Binding_Set;
   begin
      Tmp_Return := Internal (Tmp_Set_Name);
      Free (Tmp_Set_Name);
      return Tmp_Return.all;
   end Find;

end Gtk.Binding_Set;
