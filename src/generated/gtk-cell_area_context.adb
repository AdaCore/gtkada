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

package body Gtk.Cell_Area_Context is

   package Type_Conversion_Gtk_Cell_Area_Context is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Cell_Area_Context_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Cell_Area_Context);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
      (Self   : not null access Gtk_Cell_Area_Context_Record;
       Width  : Glib.Gint;
       Height : Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : Glib.Gint;
          Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_allocate");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Allocate;

   --------------------
   -- Get_Allocation --
   --------------------

   procedure Get_Allocation
      (Self   : not null access Gtk_Cell_Area_Context_Record;
       Width  : out Glib.Gint;
       Height : out Glib.Gint)
   is
      procedure Internal
         (Self   : System.Address;
          Width  : out Glib.Gint;
          Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_get_allocation");
   begin
      Internal (Get_Object (Self), Width, Height);
   end Get_Allocation;

   --------------------------
   -- Get_Preferred_Height --
   --------------------------

   procedure Get_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_get_preferred_height");
   begin
      Internal (Get_Object (Self), Minimum_Height, Natural_Height);
   end Get_Preferred_Height;

   ------------------------------------
   -- Get_Preferred_Height_For_Width --
   ------------------------------------

   procedure Get_Preferred_Height_For_Width
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Width          : Glib.Gint;
       Minimum_Height : out Glib.Gint;
       Natural_Height : out Glib.Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Width          : Glib.Gint;
          Minimum_Height : out Glib.Gint;
          Natural_Height : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_get_preferred_height_for_width");
   begin
      Internal (Get_Object (Self), Width, Minimum_Height, Natural_Height);
   end Get_Preferred_Height_For_Width;

   -------------------------
   -- Get_Preferred_Width --
   -------------------------

   procedure Get_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_get_preferred_width");
   begin
      Internal (Get_Object (Self), Minimum_Width, Natural_Width);
   end Get_Preferred_Width;

   ------------------------------------
   -- Get_Preferred_Width_For_Height --
   ------------------------------------

   procedure Get_Preferred_Width_For_Height
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Height        : Glib.Gint;
       Minimum_Width : out Glib.Gint;
       Natural_Width : out Glib.Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Height        : Glib.Gint;
          Minimum_Width : out Glib.Gint;
          Natural_Width : out Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_get_preferred_width_for_height");
   begin
      Internal (Get_Object (Self), Height, Minimum_Width, Natural_Width);
   end Get_Preferred_Width_For_Height;

   ---------------------------
   -- Push_Preferred_Height --
   ---------------------------

   procedure Push_Preferred_Height
      (Self           : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Height : Glib.Gint;
       Natural_Height : Glib.Gint)
   is
      procedure Internal
         (Self           : System.Address;
          Minimum_Height : Glib.Gint;
          Natural_Height : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_push_preferred_height");
   begin
      Internal (Get_Object (Self), Minimum_Height, Natural_Height);
   end Push_Preferred_Height;

   --------------------------
   -- Push_Preferred_Width --
   --------------------------

   procedure Push_Preferred_Width
      (Self          : not null access Gtk_Cell_Area_Context_Record;
       Minimum_Width : Glib.Gint;
       Natural_Width : Glib.Gint)
   is
      procedure Internal
         (Self          : System.Address;
          Minimum_Width : Glib.Gint;
          Natural_Width : Glib.Gint);
      pragma Import (C, Internal, "gtk_cell_area_context_push_preferred_width");
   begin
      Internal (Get_Object (Self), Minimum_Width, Natural_Width);
   end Push_Preferred_Width;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : not null access Gtk_Cell_Area_Context_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_cell_area_context_reset");
   begin
      Internal (Get_Object (Self));
   end Reset;

end Gtk.Cell_Area_Context;
