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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Separator is

   package Type_Conversion_Gtk_Separator is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Separator_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Separator);

   ------------------------
   -- Gtk_Hseparator_New --
   ------------------------

   function Gtk_Hseparator_New return Gtk_Hseparator is
      Separator : constant Gtk_Hseparator := new Gtk_Hseparator_Record;
   begin
      Gtk.Separator.Initialize_Hseparator (Separator);
      return Separator;
   end Gtk_Hseparator_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Separator   : out Gtk_Separator;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
   begin
      Separator := new Gtk_Separator_Record;
      Gtk.Separator.Initialize (Separator, Orientation);
   end Gtk_New;

   ------------------------
   -- Gtk_New_Hseparator --
   ------------------------

   procedure Gtk_New_Hseparator (Separator : out Gtk_Hseparator) is
   begin
      Separator := new Gtk_Hseparator_Record;
      Gtk.Separator.Initialize_Hseparator (Separator);
   end Gtk_New_Hseparator;

   ------------------------
   -- Gtk_New_Vseparator --
   ------------------------

   procedure Gtk_New_Vseparator (Separator : out Gtk_Vseparator) is
   begin
      Separator := new Gtk_Vseparator_Record;
      Gtk.Separator.Initialize_Vseparator (Separator);
   end Gtk_New_Vseparator;

   -----------------------
   -- Gtk_Separator_New --
   -----------------------

   function Gtk_Separator_New
      (Orientation : Gtk.Enums.Gtk_Orientation) return Gtk_Separator
   is
      Separator : constant Gtk_Separator := new Gtk_Separator_Record;
   begin
      Gtk.Separator.Initialize (Separator, Orientation);
      return Separator;
   end Gtk_Separator_New;

   ------------------------
   -- Gtk_Vseparator_New --
   ------------------------

   function Gtk_Vseparator_New return Gtk_Vseparator is
      Separator : constant Gtk_Vseparator := new Gtk_Vseparator_Record;
   begin
      Gtk.Separator.Initialize_Vseparator (Separator);
      return Separator;
   end Gtk_Vseparator_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Separator   : not null access Gtk_Separator_Record'Class;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      function Internal
         (Orientation : Gtk.Enums.Gtk_Orientation) return System.Address;
      pragma Import (C, Internal, "gtk_separator_new");
   begin
      if not Separator.Is_Created then
         Set_Object (Separator, Internal (Orientation));
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_Hseparator --
   ---------------------------

   procedure Initialize_Hseparator
      (Separator : not null access Gtk_Hseparator_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hseparator_new");
   begin
      if not Separator.Is_Created then
         Set_Object (Separator, Internal);
      end if;
   end Initialize_Hseparator;

   ---------------------------
   -- Initialize_Vseparator --
   ---------------------------

   procedure Initialize_Vseparator
      (Separator : not null access Gtk_Vseparator_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_vseparator_new");
   begin
      if not Separator.Is_Created then
         Set_Object (Separator, Internal);
      end if;
   end Initialize_Vseparator;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Separator_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Separator_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

end Gtk.Separator;
