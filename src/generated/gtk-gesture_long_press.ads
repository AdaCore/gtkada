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
--  Gtk.Gesture_Long_Press.Gtk_Gesture_Long_Press is a Gtk.Gesture.Gtk_Gesture
--  implementation able to recognize long presses, triggering the
--  Gtk.Gesture_Long_Press.Gtk_Gesture_Long_Press::pressed after the timeout is
--  exceeded.
--
--  If the touchpoint is lifted before the timeout passes, or if it drifts too
--  far of the initial press point, the
--  Gtk.Gesture_Long_Press.Gtk_Gesture_Long_Press::cancelled signal will be
--  emitted.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Gesture_Long_Press is

   type Gtk_Gesture_Long_Press_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Long_Press is access all Gtk_Gesture_Long_Press_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Gesture_Long_Press;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Gesture_Long_Press_Record'Class;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes long
   --  presses.
   --  Since: gtk+ 3.14
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Gtk_Gesture_Long_Press_New
      (Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk_Gesture_Long_Press;
   --  Returns a newly created Gtk.Gesture.Gtk_Gesture that recognizes long
   --  presses.
   --  Since: gtk+ 3.14
   --  "widget": a Gtk.Widget.Gtk_Widget

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_long_press_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Delay_Factor_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Gesture_Long_Press_Void is not null access procedure
     (Self : access Gtk_Gesture_Long_Press_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Cancelled : constant Glib.Signal_Name := "cancelled";
   procedure On_Cancelled
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_Gtk_Gesture_Long_Press_Void;
       After : Boolean := False);
   procedure On_Cancelled
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever a press moved too far, or was released
   --  before Gtk.Gesture_Long_Press.Gtk_Gesture_Long_Press::pressed happened.

   type Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void is not null access procedure
     (Self : access Gtk_Gesture_Long_Press_Record'Class;
      X    : Gdouble;
      Y    : Gdouble);

   type Cb_GObject_Gdouble_Gdouble_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class;
      X    : Gdouble;
      Y    : Gdouble);

   Signal_Pressed : constant Glib.Signal_Name := "pressed";
   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_Gtk_Gesture_Long_Press_Gdouble_Gdouble_Void;
       After : Boolean := False);
   procedure On_Pressed
      (Self  : not null access Gtk_Gesture_Long_Press_Record;
       Call  : Cb_GObject_Gdouble_Gdouble_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  This signal is emitted whenever a press goes unmoved/unreleased longer
   --  than what the GTK+ defaults tell.
   -- 
   --  Callback parameters:
   --    --  "x": the X coordinate where the press happened, relative to the widget
   --    --  allocation
   --    --  "y": the Y coordinate where the press happened, relative to the widget
   --    --  allocation

private
   Delay_Factor_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("delay-factor");
end Gtk.Gesture_Long_Press;
