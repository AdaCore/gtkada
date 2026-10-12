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

--  Recognizes long press gestures.
--
--  This gesture is also known as "Press and Hold".
--
--  When the timeout is exceeded, the gesture is triggering the
--  [signalGtk.GestureLongPress::pressed] signal.
--
--  If the touchpoint is lifted before the timeout passes, or if it drifts too
--  far of the initial press point, the [signalGtk.GestureLongPress::cancelled]
--  signal will be emitted.
--
--  How long the timeout is before the ::pressed signal gets emitted is
--  determined by the [propertyGtk.Settings:gtk-long-press-time] setting. It
--  can be modified by the [propertyGtk.GestureLongPress:delay-factor]
--  property.

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Properties;    use Glib.Properties;
with Gtk.Gesture_Single; use Gtk.Gesture_Single;

package Gtk.Gesture_Long_Press is

   type Gtk_Gesture_Long_Press_Record is new Gtk_Gesture_Single_Record with null record;
   type Gtk_Gesture_Long_Press is access all Gtk_Gesture_Long_Press_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Gesture_Long_Press);
   procedure Initialize
      (Self : not null access Gtk_Gesture_Long_Press_Record'Class);
   --  Returns a newly created `GtkGesture` that recognizes long presses.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Gesture_Long_Press_New return Gtk_Gesture_Long_Press;
   --  Returns a newly created `GtkGesture` that recognizes long presses.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_gesture_long_press_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Delay_Factor
      (Self : not null access Gtk_Gesture_Long_Press_Record) return Gdouble;
   --  Returns the delay factor.
   --  @return the delay factor

   procedure Set_Delay_Factor
      (Self         : not null access Gtk_Gesture_Long_Press_Record;
       Delay_Factor : Gdouble);
   --  Applies the given delay factor.
   --  The default long press time will be multiplied by this value. Valid
   --  values are in the range [0.5..2.0].
   --  @param Delay_Factor The delay factor to apply

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Delay_Factor_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  Factor by which to modify the default timeout.

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
   --  Emitted whenever a press moved too far, or was released before
   --  [signalGtk.GestureLongPress::pressed] happened.

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
   --  Emitted whenever a press goes unmoved/unreleased longer than what the
   --  GTK defaults tell.
   -- 
   --  Callback parameters:
   --    --  @param X the X coordinate where the press happened, relative to the
   --    --  widget allocation
   --    --  @param Y the Y coordinate where the press happened, relative to the
   --    --  widget allocation

private
   Delay_Factor_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("delay-factor");
end Gtk.Gesture_Long_Press;
