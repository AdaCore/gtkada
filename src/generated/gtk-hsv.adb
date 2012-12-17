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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Hsv is

   package Type_Conversion_Gtk_Hsv is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Hsv_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Hsv);

   -----------------
   -- Gtk_Hsv_New --
   -----------------

   function Gtk_Hsv_New return Gtk_Hsv is
      Self : constant Gtk_Hsv := new Gtk_Hsv_Record;
   begin
      Gtk.Hsv.Initialize (Self);
      return Self;
   end Gtk_Hsv_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Hsv) is
   begin
      Self := new Gtk_Hsv_Record;
      Gtk.Hsv.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Hsv_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_hsv_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   ---------------
   -- Get_Color --
   ---------------

   procedure Get_Color
      (Self : not null access Gtk_Hsv_Record;
       H    : out Gdouble;
       S    : out Gdouble;
       V    : out Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          H    : out Gdouble;
          S    : out Gdouble;
          V    : out Gdouble);
      pragma Import (C, Internal, "gtk_hsv_get_color");
   begin
      Internal (Get_Object (Self), H, S, V);
   end Get_Color;

   -----------------
   -- Get_Metrics --
   -----------------

   procedure Get_Metrics
      (Self       : not null access Gtk_Hsv_Record;
       Size       : out Gint;
       Ring_Width : out Gint)
   is
      procedure Internal
         (Self       : System.Address;
          Size       : out Gint;
          Ring_Width : out Gint);
      pragma Import (C, Internal, "gtk_hsv_get_metrics");
   begin
      Internal (Get_Object (Self), Size, Ring_Width);
   end Get_Metrics;

   ------------------
   -- Is_Adjusting --
   ------------------

   function Is_Adjusting
      (Self : not null access Gtk_Hsv_Record) return Boolean
   is
      function Internal (Self : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_hsv_is_adjusting");
   begin
      return Boolean'Val (Internal (Get_Object (Self)));
   end Is_Adjusting;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color
      (Self : not null access Gtk_Hsv_Record;
       H    : Gdouble;
       S    : Gdouble;
       V    : Gdouble)
   is
      procedure Internal
         (Self : System.Address;
          H    : Gdouble;
          S    : Gdouble;
          V    : Gdouble);
      pragma Import (C, Internal, "gtk_hsv_set_color");
   begin
      Internal (Get_Object (Self), H, S, V);
   end Set_Color;

   -----------------
   -- Set_Metrics --
   -----------------

   procedure Set_Metrics
      (Self       : not null access Gtk_Hsv_Record;
       Size       : Gint;
       Ring_Width : Gint)
   is
      procedure Internal
         (Self       : System.Address;
          Size       : Gint;
          Ring_Width : Gint);
      pragma Import (C, Internal, "gtk_hsv_set_metrics");
   begin
      Internal (Get_Object (Self), Size, Ring_Width);
   end Set_Metrics;

   ------------
   -- To_Rgb --
   ------------

   procedure To_Rgb
      (H : Gdouble;
       S : Gdouble;
       V : Gdouble;
       R : out Gdouble;
       G : out Gdouble;
       B : out Gdouble)
   is
      procedure Internal
         (H : Gdouble;
          S : Gdouble;
          V : Gdouble;
          R : out Gdouble;
          G : out Gdouble;
          B : out Gdouble);
      pragma Import (C, Internal, "gtk_hsv_to_rgb");
   begin
      Internal (H, S, V, R, G, B);
   end To_Rgb;

end Gtk.Hsv;
