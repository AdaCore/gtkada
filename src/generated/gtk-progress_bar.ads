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

--  Displays the progress of a long-running operation.
--
--  `GtkProgressBar` provides a visual clue that processing is underway. It
--  can be used in two different modes: percentage mode and activity mode.
--
--  <picture> <source srcset="progressbar-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example GtkProgressBar"
--  src="progressbar.png"> </picture>
--  When an application can determine how much work needs to take place (e.g.
--  read a fixed number of bytes from a file) and can monitor its progress, it
--  can use the `GtkProgressBar` in percentage mode and the user sees a growing
--  bar indicating the percentage of the work that has been completed. In this
--  mode, the application is required to call
--  [methodGtk.ProgressBar.set_fraction] periodically to update the progress
--  bar.
--
--  When an application has no accurate way of knowing the amount of work to
--  do, it can use the `GtkProgressBar` in activity mode, which shows activity
--  by a block moving back and forth within the progress area. In this mode,
--  the application is required to call [methodGtk.ProgressBar.pulse]
--  periodically to update the progress bar.
--
--  There is quite a bit of flexibility provided to control the appearance of
--  the `GtkProgressBar`. Functions are provided to control the orientation of
--  the bar, optional text can be displayed along with the bar, and the step
--  size used in activity mode can be set.
--
--  # CSS nodes
--
--  ``` progressbar[.osd] ├── [text] ╰── trough[.empty][.full] ╰──
--  progress[.pulse] ```
--
--  `GtkProgressBar` has a main CSS node with name progressbar and subnodes
--  with names text and trough, of which the latter has a subnode named
--  progress. The text subnode is only present if text is shown. The progress
--  subnode has the style class .pulse when in activity mode. It gets the style
--  classes .left, .right, .top or .bottom added when the progress 'touches'
--  the corresponding end of the GtkProgressBar. The .osd class on the
--  progressbar node is for use in overlays like the one Epiphany has for page
--  loading progress.
--
--  # Accessibility
--
--  `GtkProgressBar` uses the [enumGtk.AccessibleRole.progress_bar] role and
--  sets the [enumGtk.AccessibleProperty.value_min],
--  [enumGtk.AccessibleProperty.value_max] and
--  [enumGtk.AccessibleProperty.value_now] properties to reflect the progress.
--
--  <screenshot>gtk-progress_bar</screenshot>
--  <group>Display widgets</group>
--  <gtkada_demo>create_progress.adb</gtkada_demo>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                  use Glib;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Orientable;        use Gtk.Orientable;
with Gtk.Widget;            use Gtk.Widget;
with Pango.Layout;          use Pango.Layout;

package Gtk.Progress_Bar is

   type Gtk_Progress_Bar_Record is new Gtk_Widget_Record with null record;
   type Gtk_Progress_Bar is access all Gtk_Progress_Bar_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Progress_Bar : out Gtk_Progress_Bar);
   procedure Initialize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record'Class);
   --  Creates a new `GtkProgressBar`.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Progress_Bar_New return Gtk_Progress_Bar;
   --  Creates a new `GtkProgressBar`.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_progress_bar_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Ellipsize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode;
   --  Returns the ellipsizing position of the progress bar.
   --  See [methodGtk.ProgressBar.set_ellipsize].
   --  @return `PangoEllipsizeMode`

   procedure Set_Ellipsize
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Mode         : Pango.Layout.Pango_Ellipsize_Mode);
   --  Sets the mode used to ellipsize the text.
   --  The text is ellipsized if there is not enough space to render the
   --  entire string.
   --  @param Mode a `PangoEllipsizeMode`

   function Get_Fraction
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Gdouble;
   --  Returns the current fraction of the task that's been completed.
   --  @return a fraction from 0.0 to 1.0

   procedure Set_Fraction
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble);
   --  Causes the progress bar to "fill in" the given fraction of the bar.
   --  The fraction should be between 0.0 and 1.0, inclusive.
   --  @param Fraction fraction of the task that's been completed

   function Get_Inverted
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Boolean;
   --  Returns whether the progress bar is inverted.
   --  @return True if the progress bar is inverted

   procedure Set_Inverted
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Inverted     : Boolean);
   --  Sets whether the progress bar is inverted.
   --  Progress bars normally grow from top to bottom or left to right.
   --  Inverted progress bars grow in the opposite direction.
   --  @param Inverted True to invert the progress bar

   function Get_Pulse_Step
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Gdouble;
   --  Retrieves the pulse step.
   --  See [methodGtk.ProgressBar.set_pulse_step].
   --  @return a fraction from 0.0 to 1.0

   procedure Set_Pulse_Step
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Fraction     : Gdouble);
   --  Sets the fraction of total progress bar length to move the bouncing
   --  block.
   --  The bouncing block is moved when [methodGtk.ProgressBar.pulse] is
   --  called.
   --  @param Fraction fraction between 0.0 and 1.0

   function Get_Show_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return Boolean;
   --  Returns whether the `GtkProgressBar` shows text.
   --  See [methodGtk.ProgressBar.set_show_text].
   --  @return True if text is shown in the progress bar

   procedure Set_Show_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Show_Text    : Boolean);
   --  Sets whether the progress bar will show text next to the bar.
   --  The shown text is either the value of the
   --  [propertyGtk.ProgressBar:text] property or, if that is null, the
   --  [propertyGtk.ProgressBar:fraction] value, as a percentage.
   --  To make a progress bar that is styled and sized suitably for containing
   --  text (even if the actual text is blank), set
   --  [propertyGtk.ProgressBar:show-text] to True and
   --  [propertyGtk.ProgressBar:text] to the empty string (not null).
   --  @param Show_Text whether to show text

   function Get_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record)
       return UTF8_String;
   --  Retrieves the text that is displayed with the progress bar.
   --  The return value is a reference to the text, not a copy of it, so will
   --  become invalid if you change the text in the progress bar.
   --  @return the text

   procedure Set_Text
      (Progress_Bar : not null access Gtk_Progress_Bar_Record;
       Text         : UTF8_String := "");
   --  Causes the given Text to appear next to the progress bar.
   --  If Text is null and [propertyGtk.ProgressBar:show-text] is True, the
   --  current value of [propertyGtk.ProgressBar:fraction] will be displayed as
   --  a percentage.
   --  If Text is non-null and [propertyGtk.ProgressBar:show-text] is True,
   --  the text will be displayed. In this case, it will not display the
   --  progress percentage. If Text is the empty string, the progress bar will
   --  still be styled and sized suitably for containing text, as long as
   --  [propertyGtk.ProgressBar:show-text] is True.
   --  @param Text a UTF-8 string

   procedure Pulse (Progress_Bar : not null access Gtk_Progress_Bar_Record);
   --  Indicates that some progress has been made, but you don't know how
   --  much.
   --  Causes the progress bar to enter "activity mode," where a block bounces
   --  back and forth. Each call to [methodGtk.ProgressBar.pulse] causes the
   --  block to move by a little bit (the amount of movement per pulse is
   --  determined by [methodGtk.ProgressBar.set_pulse_step]).

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Progress_Bar_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Progress_Bar_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Progress_Bar_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Progress_Bar_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Progress_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Progress_Bar_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Progress_Bar_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Progress_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Progress_Bar_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Progress_Bar_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   function Get_Orientation
      (Self : not null access Gtk_Progress_Bar_Record)
       return Gtk.Enums.Gtk_Orientation;

   procedure Set_Orientation
      (Self        : not null access Gtk_Progress_Bar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode;
   --  Type: Pango.Layout.Pango_Ellipsize_Mode
   --  The preferred place to ellipsize the string.
   --
   --  The text will be ellipsized if the progress bar does not have enough
   --  room to display the entire string, specified as a `PangoEllipsizeMode`.
   --
   --  Note that setting this property to a value other than
   --  Pango.Layout.Ellipsize_None has the side-effect that the progress bar
   --  requests only enough space to display the ellipsis ("..."). Another
   --  means to set a progress bar's width is
   --  [methodGtk.Widget.set_size_request].

   Fraction_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The fraction of total work that has been completed.

   Inverted_Property : constant Glib.Properties.Property_Boolean;
   --  Invert the direction in which the progress bar grows.

   Pulse_Step_Property : constant Glib.Properties.Property_Double;
   --  Type: Gdouble
   --  The fraction of total progress to move the bounding block when pulsed.

   Show_Text_Property : constant Glib.Properties.Property_Boolean;
   --  Sets whether the progress bar will show a text in addition to the bar
   --  itself.
   --
   --  The shown text is either the value of the
   --  [propertyGtk.ProgressBar:text] property or, if that is null, the
   --  [propertyGtk.ProgressBar:fraction] value, as a percentage.
   --
   --  To make a progress bar that is styled and sized suitably for showing
   --  text (even if the actual text is blank), set
   --  [propertyGtk.ProgressBar:show-text] to True and
   --  [propertyGtk.ProgressBar:text] to the empty string (not null).

   Text_Property : constant Glib.Properties.Property_String;
   --  Text to be displayed in the progress bar.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"
   --
   --  - "Gtk.Orientable"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Progress_Bar_Record, Gtk_Progress_Bar);
   function "+"
     (Widget : access Gtk_Progress_Bar_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Progress_Bar
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Progress_Bar_Record, Gtk_Progress_Bar);
   function "+"
     (Widget : access Gtk_Progress_Bar_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Progress_Bar
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Progress_Bar_Record, Gtk_Progress_Bar);
   function "+"
     (Widget : access Gtk_Progress_Bar_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Progress_Bar
   renames Implements_Gtk_Constraint_Target.To_Object;

   package Implements_Gtk_Orientable is new Glib.Types.Implements
     (Gtk.Orientable.Gtk_Orientable, Gtk_Progress_Bar_Record, Gtk_Progress_Bar);
   function "+"
     (Widget : access Gtk_Progress_Bar_Record'Class)
   return Gtk.Orientable.Gtk_Orientable
   renames Implements_Gtk_Orientable.To_Interface;
   function "-"
     (Interf : Gtk.Orientable.Gtk_Orientable)
   return Gtk_Progress_Bar
   renames Implements_Gtk_Orientable.To_Object;

private
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Show_Text_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("show-text");
   Pulse_Step_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("pulse-step");
   Inverted_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("inverted");
   Fraction_Property : constant Glib.Properties.Property_Double :=
     Glib.Properties.Build ("fraction");
   Ellipsize_Property : constant Pango.Layout.Property_Pango_Ellipsize_Mode :=
     Pango.Layout.Build ("ellipsize");
end Gtk.Progress_Bar;
