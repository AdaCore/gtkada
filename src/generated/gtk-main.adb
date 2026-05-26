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
with Gtkada.Bindings; use Gtkada.Bindings;
with Gtkada.Types;    use Gtkada.Types;

package body Gtk.Main is

   -------------------
   -- Check_Version --
   -------------------

   function Check_Version
      (Required_Major : Guint;
       Required_Minor : Guint;
       Required_Micro : Guint) return UTF8_String
   is
      function Internal
         (Required_Major : Guint;
          Required_Minor : Guint;
          Required_Micro : Guint) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_check_version");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Required_Major, Required_Minor, Required_Micro));
   end Check_Version;

   -----------------------
   -- Disable_Setlocale --
   -----------------------

   procedure Disable_Setlocale is
      procedure Internal;
      pragma Import (C, Internal, "gtk_disable_setlocale");
   begin
      Internal;
   end Disable_Setlocale;

   --------------------
   -- Get_Binary_Age --
   --------------------

   function Get_Binary_Age return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_binary_age");
   begin
      return Internal;
   end Get_Binary_Age;

   --------------------------
   -- Get_Default_Language --
   --------------------------

   function Get_Default_Language return Pango.Language.Pango_Language is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_get_default_language");
   begin
      return From_Object (Internal);
   end Get_Default_Language;

   -----------------------
   -- Get_Interface_Age --
   -----------------------

   function Get_Interface_Age return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_interface_age");
   begin
      return Internal;
   end Get_Interface_Age;

   -----------------------
   -- Get_Major_Version --
   -----------------------

   function Get_Major_Version return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_major_version");
   begin
      return Internal;
   end Get_Major_Version;

   -----------------------
   -- Get_Micro_Version --
   -----------------------

   function Get_Micro_Version return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_micro_version");
   begin
      return Internal;
   end Get_Micro_Version;

   -----------------------
   -- Get_Minor_Version --
   -----------------------

   function Get_Minor_Version return Guint is
      function Internal return Guint;
      pragma Import (C, Internal, "gtk_get_minor_version");
   begin
      return Internal;
   end Get_Minor_Version;

   ----------
   -- Init --
   ----------

   procedure Init is
      procedure Internal;
      pragma Import (C, Internal, "gtk_init");
   begin
      Internal;
   end Init;

   ----------------
   -- Init_Check --
   ----------------

   function Init_Check return Boolean is
      function Internal return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_init_check");
   begin
      return Internal /= 0;
   end Init_Check;

end Gtk.Main;
