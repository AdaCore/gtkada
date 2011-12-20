------------------------------------------------------------------------------
--               GtkAda - Ada95 binding for the Gimp Toolkit                --
--                                                                          --
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

--  <description>
--  The Gnome_Color_Picker widget is a simple color picker in a button.
--  The button displays a sample of the currently selected color. When the user
--  clicks on the button, a color selection dialog pops up. The color picker
--  emits the "color_changed" signal when the color is set
--  By default, the color picker does dithering when drawing the color sample
--  box. This can be disabled for cases where it is useful to see the allocated
--  color without dithering.
--  </description>

with Gtk.Button;
with Glib; use Glib;

package Gnome.Color_Picker is

   type Gnome_Color_Picker_Record is new Gtk.Button.Gtk_Button_Record
     with private;
   type Gnome_Color_Picker is access all Gnome_Color_Picker_Record'Class;

   procedure Gnome_New (Color_Picker : out Gnome_Color_Picker);
   --  Create a new Color_Picker

   procedure Initialize
     (Color_Picker : access Gnome_Color_Picker_Record'Class);
   --  Internal initialization function.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with a Color_Picker.

   procedure Set
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : in Gdouble;
      G       : in Gdouble;
      B       : in Gdouble;
      A       : in Gdouble := 0.0);
   --  Set the color in the picker, as doubles range [0.0, 1.0]

   procedure Get
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : out Gdouble;
      G       : out Gdouble;
      B       : out Gdouble;
      A       : out Gdouble);
   --  Get the color in the picker, as doubles range [0.0, 1.0]

   procedure Set
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : in Guint8;
      G       : in Guint8;
      B       : in Guint8;
      A       : in Guint8 := 0);
   --  Set the color in the picker, as guint8s range [0, 255]

   procedure Get
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : out Guint8;
      G       : out Guint8;
      B       : out Guint8;
      A       : out Guint8);
   --  Get the color in the picker, as guint8s range [0, 255]

   procedure Set
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : in Gushort;
      G       : in Gushort;
      B       : in Gushort;
      A       : in Gushort := 0);
   --  Set the color in the picker, as gushorts range [0, 65535]

   procedure Get
     (Cpicker : access Gnome_Color_Picker_Record;
      R       : out Gushort;
      G       : out Gushort;
      B       : out Gushort;
      A       : out Gushort);
   --  Get the color in the picker, as gushorts range [0, 65535]

   procedure Set_Dither
     (Cpicker : access Gnome_Color_Picker_Record;
      Dither  : in Boolean);
   --  Set whether the picker should dither the color sample
   --  or just paint a solid rectangle.

   procedure Set_Use_Alpha
     (Cpicker   : access Gnome_Color_Picker_Record;
      Use_Alpha : in Boolean);
   --  Set whether the picker should use the alpha channel or not.

   procedure Set_Title
     (Cpicker : access Gnome_Color_Picker_Record;
      Title   : in String);
   --  Set the title of the color selection dialog.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "color_set"
   --    procedure Handler (Cpicker : access Gnome_Color_Picker_Record'Class;
   --                       R       : Guint;
   --                       G       : Guint;
   --                       B       : Guint;
   --                       A       : Guint);
   --
   --    The color is set
   --
   --  </signals>

private
   type Gnome_Color_Picker_Record is new Gtk.Button.Gtk_Button_Record
     with null record;
   pragma Import (C, Get_Type, "gnome_color_picker_get_type");
end Gnome.Color_Picker;
