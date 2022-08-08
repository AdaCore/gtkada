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

package body Gtk.Color_Chooser_Widget is

   package Type_Conversion_Gtk_Color_Chooser_Widget is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Color_Chooser_Widget_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Color_Chooser_Widget);

   ----------------------------------
   -- Gtk_Color_Chooser_Widget_New --
   ----------------------------------

   function Gtk_Color_Chooser_Widget_New return Gtk_Color_Chooser_Widget is
      Self : constant Gtk_Color_Chooser_Widget := new Gtk_Color_Chooser_Widget_Record;
   begin
      Gtk.Color_Chooser_Widget.Initialize (Self);
      return Self;
   end Gtk_Color_Chooser_Widget_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Color_Chooser_Widget) is
   begin
      Self := new Gtk_Color_Chooser_Widget_Record;
      Gtk.Color_Chooser_Widget.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Color_Chooser_Widget_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_color_chooser_widget_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------
   -- Add_Palette --
   -----------------

   procedure Add_Palette
      (Self            : not null access Gtk_Color_Chooser_Widget_Record;
       Orientation     : Gtk.Enums.Gtk_Orientation;
       Colors_Per_Line : Glib.Gint;
       N_Colors        : Glib.Gint;
       Colors          : array_of_Gdk_RGBA)
   is
      procedure Internal
         (Self            : System.Address;
          Orientation     : Gtk.Enums.Gtk_Orientation;
          Colors_Per_Line : Glib.Gint;
          N_Colors        : Glib.Gint;
          Colors          : array_of_Gdk_RGBA);
      pragma Import (C, Internal, "gtk_color_chooser_add_palette");
   begin
      Internal (Get_Object (Self), Orientation, Colors_Per_Line, N_Colors, Colors);
   end Add_Palette;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Color_Chooser_Widget_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   --------------
   -- Get_Rgba --
   --------------

   procedure Get_Rgba
      (Self  : not null access Gtk_Color_Chooser_Widget_Record;
       Color : out Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal
         (Self  : System.Address;
          Color : out Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_color_chooser_get_rgba");
   begin
      Internal (Get_Object (Self), Color);
   end Get_Rgba;

   -------------------
   -- Get_Use_Alpha --
   -------------------

   function Get_Use_Alpha
      (Self : not null access Gtk_Color_Chooser_Widget_Record)
       return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_color_chooser_get_use_alpha");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Use_Alpha;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Color_Chooser_Widget_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   --------------
   -- Set_Rgba --
   --------------

   procedure Set_Rgba
      (Self  : not null access Gtk_Color_Chooser_Widget_Record;
       Color : Gdk.RGBA.Gdk_RGBA)
   is
      procedure Internal (Self : System.Address; Color : Gdk.RGBA.Gdk_RGBA);
      pragma Import (C, Internal, "gtk_color_chooser_set_rgba");
   begin
      Internal (Get_Object (Self), Color);
   end Set_Rgba;

   -------------------
   -- Set_Use_Alpha --
   -------------------

   procedure Set_Use_Alpha
      (Self      : not null access Gtk_Color_Chooser_Widget_Record;
       Use_Alpha : Boolean)
   is
      procedure Internal (Self : System.Address; Use_Alpha : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_color_chooser_set_use_alpha");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Use_Alpha));
   end Set_Use_Alpha;

end Gtk.Color_Chooser_Widget;
