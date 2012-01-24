------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2012, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;        use Interfaces.C.Strings;
with System;                      use System;

with Cairo;                       use Cairo;
with Glib.Values;                 use Glib.Values;
with Gdk.Color;                   use Gdk.Color;
with Gdk.RGBA;                    use Gdk.RGBA;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Visual;                  use Gdk.Visual;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Style;                   use Gtk.Style;
with Gtk.Window;
with Gtkada.Bindings;             use Gtkada.Bindings;
with Gtkada.Types;
with Pango.Context;               use Pango.Context;
with Pango.Layout;                use Pango.Layout;

with Glib.Type_Conversion_Hooks;

package body Gtk.Widget is

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Widget : access Gtk_Widget_Record'Class) is
   begin
      Destroy (Widget);
   end Destroy_Cb;

   ---------------
   -- Intersect --
   ---------------

   function Intersect
     (Widget       : access Gtk_Widget_Record;
      Area         : Gdk.Rectangle.Gdk_Rectangle;
      Intersection : access Gdk.Rectangle.Gdk_Rectangle) return Boolean
   is
      function Internal
        (Widget : System.Address;
         Area   : Gdk.Rectangle.Gdk_Rectangle;
         Inter  : access Gdk.Rectangle.Gdk_Rectangle) return Gint;
      pragma Import (C, Internal, "gtk_widget_intersect");

      Result : Gint;

   begin
      Result := Internal (Get_Object (Widget), Area, Intersection);
      return Boolean'Val (Result);
   end Intersect;

   ----------------------
   -- Region_Intersect --
   ----------------------

   function Region_Intersect
     (Widget : access Gtk_Widget_Record;
      Region : Cairo.Region.Cairo_Region) return Cairo.Region.Cairo_Region
   is
      function Internal
        (Widget : System.Address;
         Region : Cairo.Region.Cairo_Region) return Cairo.Region.Cairo_Region;
      pragma Import (C, Internal, "gtk_widget_region_intersect");
   begin
      return Internal (Get_Object (Widget), Region);
   end Region_Intersect;

   -----------------
   -- Set_Realize --
   -----------------

   package body Realize_Handling is

      procedure Internal_Realize (Widget : System.Address) is
         Dummy : Widget_Type;
         pragma Warnings (Off, Dummy);
      begin
         Realize_Proc (Widget_Type (Get_User_Data (Widget, Dummy).all)'Access);
      end Internal_Realize;

      procedure Set_Realize (Widget : access Gtk_Widget_Record'Class) is
         procedure Internal
           (Widget : System.Address; Realize : System.Address);
         pragma Import (C, Internal, "ada_widget_set_realize");

      begin
         Internal (Get_Object (Widget), Internal_Realize'Address);
      end Set_Realize;

   end Realize_Handling;

   ---------------------------
   -- Translate_Coordinates --
   ---------------------------

   procedure Translate_Coordinates
     (Src_Widget  : Gtk_Widget;
      Dest_Widget : Gtk_Widget;
      Src_X       : Gint;
      Src_Y       : Gint;
      Dest_X      : out Gint;
      Dest_Y      : out Gint;
      Result      : out Boolean)
   is
      function Internal
        (Src_Widget  : System.Address;
         Dest_Widget : System.Address;
         Src_X       : Gint;
         Src_Y       : Gint;
         Dest_X      : access Gint;
         Dest_Y      : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_widget_translate_coordinates");

      X, Y : aliased Gint;

   begin
      Result := Boolean'Val (Internal
        (Get_Object (Src_Widget),
         Get_Object (Dest_Widget),
         Src_X, Src_Y, X'Access, Y'Access));

      if Result then
         Dest_X := X;
         Dest_Y := Y;
      end if;
   end Translate_Coordinates;

end Gtk.Widget;
