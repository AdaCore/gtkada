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

--  <description>
--  The Gtk.Progress_Bar.Gtk_Progress_Bar is typically used to display the
--  progress of a long running operation. It provides a visual clue that
--  processing is underway. The Gtk.Progress_Bar.Gtk_Progress_Bar can be used
--  in two different modes: percentage mode and activity mode.
--
--  When an application can determine how much work needs to take place (e.g.
--  read a fixed number of bytes from a file) and can monitor its progress, it
--  can use the Gtk.Progress_Bar.Gtk_Progress_Bar in percentage mode and the
--  user sees a growing bar indicating the percentage of the work that has been
--  completed. In this mode, the application is required to call
--  Gtk.Progress_Bar.Set_Fraction periodically to update the progress bar.
--
--  When an application has no accurate way of knowing the amount of work to
--  do, it can use the Gtk.Progress_Bar.Gtk_Progress_Bar in activity mode,
--  which shows activity by a block moving back and forth within the progress
--  area. In this mode, the application is required to call
--  Gtk.Progress_Bar.Pulse periodically to update the progress bar.
--
--  There is quite a bit of flexibility provided to control the appearance of
--  the Gtk.Progress_Bar.Gtk_Progress_Bar. Functions are provided to control
--  the orientation of the bar, optional text can be displayed along with the
--  bar, and the step size used in activity mode can be set.
--
--  </description>
--  <screenshot>gtk-progress_bar</screenshot>
--  <group>Display widgets</group>
--  <testgtk>create_progress.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Buildable;   use Gtk.Buildable;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Orientable;  use Gtk.Orientable;
with Gtk.Widget;      use Gtk.Widget;
with Pango.Layout;    use Pango.Layout;

package Gtk.Progress_Bar is

   type Gtk_Progress_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Progress_Bar is access all Gtk_Progress_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar);
   procedure Initialize
      (Progress_Bar : access Gtk_Progress_Bar_Record'Class);
   --  Creates a new Gtk.Progress_Bar.Gtk_Progress_Bar.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_progress_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Ellipsize
      (Progress_Bar : access Gtk_Progress_Bar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   procedure Set_Ellipsize
      (Progress_Bar : access Gtk_Progress_Bar_Record;
       Mode         : Pango.Layout.Pango_Ellipsize_Mode);
   --  Sets the mode used to ellipsize (add an ellipsis: "...") the text if
   --  there is not enough space to render the entire string.
   --  Since: gtk+ 2.6
   --  "mode": a Pango.Layout.Pango_Ellipsize_Mode

   function Get_Fraction
      (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble;
   procedure Set_Fraction
      (Progress_Bar : access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble);
   --  Causes the progress bar to "fill in" the given fraction of the bar. The
   --  fraction should be between 0.0 and 1.0, inclusive.
   --  "fraction": fraction of the task that's been completed

   function Get_Inverted
      (Progress_Bar : access Gtk_Progress_Bar_Record) return Boolean;
   procedure Set_Inverted
      (Progress_Bar : access Gtk_Progress_Bar_Record;
       Inverted     : Boolean);
   --  Progress bars normally grow from top to bottom or left to right.
   --  Inverted progress bars grow in the opposite direction.
   --  "inverted": True to invert the progress bar

   function Get_Pulse_Step
      (Progress_Bar : access Gtk_Progress_Bar_Record) return Gdouble;
   procedure Set_Pulse_Step
      (Progress_Bar : access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble);
   --  Sets the fraction of total progress bar length to move the bouncing
   --  block for each call to Gtk.Progress_Bar.Pulse.
   --  "fraction": fraction between 0.0 and 1.0

   function Get_Show_Text
      (Progress_Bar : access Gtk_Progress_Bar_Record) return Boolean;
   procedure Set_Show_Text
      (Progress_Bar : access Gtk_Progress_Bar_Record;
       Show_Text    : Boolean);
   --  Sets whether the progressbar will show text superimposed over the bar.
   --  The shown text is either the value of the
   --  Gtk.Progress_Bar.Gtk_Progress_Bar::text property or, if that is null,
   --  the Gtk.Progress_Bar.Gtk_Progress_Bar::fraction value, as a percentage.
   --  To make a progress bar that is styled and sized suitably for containing
   --  text (even if the actual text is blank), set
   --  Gtk.Progress_Bar.Gtk_Progress_Bar:show-text to True and
   --  Gtk.Progress_Bar.Gtk_Progress_Bar:text to the empty string (not null).
   --  Since: gtk+ 3.0
   --  "show_text": whether to show superimposed text

   function Get_Text
      (Progress_Bar : access Gtk_Progress_Bar_Record) return UTF8_String;
   procedure Set_Text
      (Progress_Bar : access Gtk_Progress_Bar_Record;
       Text         : UTF8_String);
   --  Causes the given Text to appear superimposed on the progress bar.
   --  If Text is null and Gtk.Progress_Bar.Gtk_Progress_Bar:show-text is True,
   --  the current value of Gtk.Progress_Bar.Gtk_Progress_Bar:fraction will be
   --  displayed as a percentage.
   --  If Text is non-null and Gtk.Progress_Bar.Gtk_Progress_Bar:show-text is
   --  True, the text will be displayed. In this case, it will not display the
   --  progress percentage. If Text is the empty string, the progress bar will
   --  still be styled and sized suitably for containing text, as long as
   --  Gtk.Progress_Bar.Gtk_Progress_Bar:show-text is True.
   --  "text": a UTF-8 string, or null

   procedure Pulse (Progress_Bar : access Gtk_Progress_Bar_Record);
   --  Indicates that some progress is made, but you don't know how much.
   --  Causes the progress bar to enter "activity mode," where a block bounces
   --  back and forth. Each call to Gtk.Progress_Bar.Pulse causes the block to
   --  move by a little bit (the amount of movement per pulse is determined by
   --  Gtk.Progress_Bar.Set_Pulse_Step).

   ---------------------
   -- Interfaces_Impl --
   ---------------------

   function Get_Orientation
      (Self : access Gtk_Progress_Bar_Record)
       return Gtk.Enums.Gtk_Orientation;
   procedure Set_Orientation
      (Self        : access Gtk_Progress_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"
   --
   --  - "Orientable"

   package Implements_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Progress_Bar_Record, Gtk_Progress_Bar);
   function "+"
     (Widget : access Gtk_Progress_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Progress_Bar
   renames Implements_Buildable.To_Object;

   package Implements_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Progress_Bar_Record, Gtk_Progress_Bar);
   function "+"
     (Widget : access Gtk_Progress_Bar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Progress_Bar
   renames Implements_Orientable.To_Object;

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)
   --
   --  Name: Fraction_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Inverted_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Pulse_Step_Property
   --  Type: Gdouble
   --  Flags: read-write
   --
   --  Name: Show_Text_Property
   --  Type: Boolean
   --  Flags: read-write
   --
   --  Name: Text_Property
   --  Type: UTF8_String
   --  Flags: read-write

   Fraction_Property : constant Glib.Properties.Property_Double;
   Inverted_Property : constant Glib.Properties.Property_Boolean;
   Pulse_Step_Property : constant Glib.Properties.Property_Double;
   Show_Text_Property : constant Glib.Properties.Property_Boolean;
   Text_Property : constant Glib.Properties.Property_String;

private
   Fraction_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("fraction");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
   Pulse_Step_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("pulse-step");
   Show_Text_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-text");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
end Gtk.Progress_Bar;
