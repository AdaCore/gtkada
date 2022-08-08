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
--  Gtk.Hsv.Gtk_Hsv is the "color wheel" part of a complete color selector
--  widget. It allows to select a color by determining its HSV components in an
--  intuitive way. Moving the selection around the outer ring changes the hue,
--  and moving the selection point inside the inner triangle changes value and
--  saturation.
--
--  Gtk.Hsv.Gtk_Hsv has been deprecated together with
--  Gtk.Color_Selection.Gtk_Color_Selection, where it was used.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Object;   use Glib.Object;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Hsv is

   type Gtk_Hsv_Record is new Gtk_Widget_Record with null record;
   type Gtk_Hsv is access all Gtk_Hsv_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Hsv);
   procedure Initialize (Self : not null access Gtk_Hsv_Record'Class);
   --  Creates a new HSV color selector.
   --  Since: gtk+ 2.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Hsv_New return Gtk_Hsv;
   --  Creates a new HSV color selector.
   --  Since: gtk+ 2.14

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_hsv_get_type");

   -------------
   -- Methods --
   -------------

   procedure Get_Color
      (Self : not null access Gtk_Hsv_Record;
       H    : out Gdouble;
       S    : out Gdouble;
       V    : out Gdouble);
   --  Queries the current color in an HSV color selector. Returned values
   --  will be in the [0.0, 1.0] range.
   --  Since: gtk+ 2.14
   --  "h": Return value for the hue
   --  "s": Return value for the saturation
   --  "v": Return value for the value

   procedure Set_Color
      (Self : not null access Gtk_Hsv_Record;
       H    : Gdouble;
       S    : Gdouble;
       V    : Gdouble);
   --  Sets the current color in an HSV color selector. Color component values
   --  must be in the [0.0, 1.0] range.
   --  Since: gtk+ 2.14
   --  "h": Hue
   --  "s": Saturation
   --  "v": Value

   procedure Get_Metrics
      (Self       : not null access Gtk_Hsv_Record;
       Size       : out Glib.Gint;
       Ring_Width : out Glib.Gint);
   --  Queries the size and ring width of an HSV color selector.
   --  Since: gtk+ 2.14
   --  "size": Return value for the diameter of the hue ring
   --  "ring_width": Return value for the width of the hue ring

   procedure Set_Metrics
      (Self       : not null access Gtk_Hsv_Record;
       Size       : Glib.Gint;
       Ring_Width : Glib.Gint);
   --  Sets the size and ring width of an HSV color selector.
   --  Since: gtk+ 2.14
   --  "size": Diameter for the hue ring
   --  "ring_width": Width of the hue ring

   function Is_Adjusting
      (Self : not null access Gtk_Hsv_Record) return Boolean;
   --  An HSV color selector can be said to be adjusting if multiple rapid
   --  changes are being made to its value, for example, when the user is
   --  adjusting the value with the mouse. This function queries whether the
   --  HSV color selector is being adjusted or not.
   --  Since: gtk+ 2.14

   ---------------
   -- Functions --
   ---------------

   procedure To_Rgb
      (H : Gdouble;
       S : Gdouble;
       V : Gdouble;
       R : out Gdouble;
       G : out Gdouble;
       B : out Gdouble);
   --  Converts a color from HSV space to RGB.
   --  Input values must be in the [0.0, 1.0] range; output values will be in
   --  the same range.
   --  Since: gtk+ 2.14
   --  "h": Hue
   --  "s": Saturation
   --  "v": Value
   --  "r": Return value for the red component
   --  "g": Return value for the green component
   --  "b": Return value for the blue component

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Hsv_Void is not null access procedure (Self : access Gtk_Hsv_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Changed : constant Glib.Signal_Name := "changed";
   procedure On_Changed
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_Gtk_Hsv_Void;
       After : Boolean := False);
   procedure On_Changed
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   type Cb_Gtk_Hsv_Gtk_Direction_Type_Void is not null access procedure
     (Self   : access Gtk_Hsv_Record'Class;
      Object : Gtk.Enums.Gtk_Direction_Type);

   type Cb_GObject_Gtk_Direction_Type_Void is not null access procedure
     (Self   : access Glib.Object.GObject_Record'Class;
      Object : Gtk.Enums.Gtk_Direction_Type);

   Signal_Move : constant Glib.Signal_Name := "move";
   procedure On_Move
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_Gtk_Hsv_Gtk_Direction_Type_Void;
       After : Boolean := False);
   procedure On_Move
      (Self  : not null access Gtk_Hsv_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Hsv_Record, Gtk_Hsv);
   function "+"
     (Widget : access Gtk_Hsv_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Hsv
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Hsv;
