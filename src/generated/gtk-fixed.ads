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
--  The Gtk.Fixed.Gtk_Fixed widget is a container which can place child
--  widgets at fixed positions and with fixed sizes, given in pixels.
--  Gtk.Fixed.Gtk_Fixed performs no automatic layout management.
--
--  For most applications, you should not use this container! It keeps you
--  from having to learn about the other GTK+ containers, but it results in
--  broken applications. With Gtk.Fixed.Gtk_Fixed, the following things will
--  result in truncated text, overlapping widgets, and other display bugs:
--
--  - Themes, which may change widget sizes.
--
--  - Fonts other than the one you used to write the app will of course change
--  the size of widgets containing text; keep in mind that users may use a
--  larger font because of difficulty reading the default, or they may be using
--  a different OS that provides different fonts.
--
--  - Translation of text into other languages changes its size. Also, display
--  of non-English text will use a different font in many cases.
--
--  In addition, Gtk.Fixed.Gtk_Fixed does not pay attention to text direction
--  and thus may produce unwanted results if your app is run under
--  right-to-left languages such as Hebrew or Arabic. That is: normally GTK+
--  will order containers appropriately for the text direction, e.g. to put
--  labels to the right of the thing they label when using an RTL language, but
--  it can't do that with Gtk.Fixed.Gtk_Fixed. So if you need to reorder
--  widgets depending on the text direction, you would need to manually detect
--  it and adjust child positions accordingly.
--
--  Finally, fixed positioning makes it kind of annoying to add/remove GUI
--  elements, since you have to reposition all the other elements. This is a
--  long-term maintenance problem for your application.
--
--  If you know none of these things are an issue for your application, and
--  prefer the simplicity of Gtk.Fixed.Gtk_Fixed, by all means use the widget.
--  But you should be aware of the tradeoffs.
--
--  See also Gtk.Layout.Gtk_Layout, which shares the ability to perform fixed
--  positioning of child widgets and additionally adds custom drawing and
--  scrollability.
--
--  </description>
--  <screenshot>gtk-fixed</screenshot>
--  <group>Layout containers</group>
--  <testgtk>create_fixed.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Container; use Gtk.Container;
with Gtk.Widget;    use Gtk.Widget;

package Gtk.Fixed is

   type Gtk_Fixed_Record is new Gtk_Container_Record with null record;
   type Gtk_Fixed is access all Gtk_Fixed_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Fixed : out Gtk_Fixed);
   procedure Initialize (Fixed : not null access Gtk_Fixed_Record'Class);
   --  Creates a new Gtk.Fixed.Gtk_Fixed.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Fixed_New return Gtk_Fixed;
   --  Creates a new Gtk.Fixed.Gtk_Fixed.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_fixed_get_type");

   -------------
   -- Methods --
   -------------

   procedure Move
      (Fixed  : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Glib.Gint;
       Y      : Glib.Gint);
   --  Move a child of a GtkFixed container to the given position. X indicates
   --  the horizontal position to place the widget at. Y is the vertical
   --  position to place the widget at.
   --  "widget": the child widget.
   --  "x": the horizontal position to move the widget to.
   --  "y": the vertical position to move the widget to.

   procedure Put
      (Fixed  : not null access Gtk_Fixed_Record;
       Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       X      : Glib.Gint;
       Y      : Glib.Gint);
   --  Add Widget to a Fixed container at the given position. X indicates the
   --  horizontal position to place the widget at. Y is the vertical position
   --  to place the widget at.
   --  "widget": the widget to add.
   --  "x": the horizontal position to place the widget at.
   --  "y": the vertical position to place the widget at.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Fixed_Record, Gtk_Fixed);
   function "+"
     (Widget : access Gtk_Fixed_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Fixed
   renames Implements_Gtk_Buildable.To_Object;

end Gtk.Fixed;
