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
--  The GtkRevealer widget is a container which animates the transition of its
--  child from invisible to visible.
--
--  The style of transition can be controlled with
--  Gtk.Revealer.Set_Transition_Type.
--
--  These animations respect the
--  Gtk.Settings.Gtk_Settings:gtk-enable-animations setting.
--
--  # CSS nodes
--
--  GtkRevealer has a single CSS node with name revealer.
--
--  The GtkRevealer widget was added in GTK+ 3.10.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Bin;                 use Gtk.Bin;
with Gtk.Buildable;           use Gtk.Buildable;

package Gtk.Revealer is

   type Gtk_Revealer_Record is new Gtk_Bin_Record with null record;
   type Gtk_Revealer is access all Gtk_Revealer_Record'Class;

   type Gtk_Revealer_Transition_Type is (
      Revealer_Transition_Type_None,
      Revealer_Transition_Type_Crossfade,
      Revealer_Transition_Type_Slide_Right,
      Revealer_Transition_Type_Slide_Left,
      Revealer_Transition_Type_Slide_Up,
      Revealer_Transition_Type_Slide_Down);
   pragma Convention (C, Gtk_Revealer_Transition_Type);
   --  These enumeration values describe the possible transitions when the
   --  child of a Gtk.Revealer.Gtk_Revealer widget is shown or hidden.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Revealer_Transition_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Revealer_Transition_Type);
   type Property_Gtk_Revealer_Transition_Type is new Gtk_Revealer_Transition_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Revealer);
   procedure Initialize (Self : not null access Gtk_Revealer_Record'Class);
   --  Creates a new Gtk.Revealer.Gtk_Revealer.
   --  Since: gtk+ 3.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Revealer_New return Gtk_Revealer;
   --  Creates a new Gtk.Revealer.Gtk_Revealer.
   --  Since: gtk+ 3.10

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_revealer_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child_Revealed
      (Self : not null access Gtk_Revealer_Record) return Boolean;
   --  Returns whether the child is fully revealed, in other words whether the
   --  transition to the revealed state is completed.
   --  Since: gtk+ 3.10

   function Get_Reveal_Child
      (Self : not null access Gtk_Revealer_Record) return Boolean;
   --  Returns whether the child is currently revealed. See
   --  Gtk.Revealer.Set_Reveal_Child.
   --  This function returns True as soon as the transition is to the revealed
   --  state is started. To learn whether the child is fully revealed (ie the
   --  transition is completed), use Gtk.Revealer.Get_Child_Revealed.
   --  Since: gtk+ 3.10

   procedure Set_Reveal_Child
      (Self         : not null access Gtk_Revealer_Record;
       Reveal_Child : Boolean);
   --  Tells the Gtk.Revealer.Gtk_Revealer to reveal or conceal its child.
   --  The transition will be animated with the current transition type of
   --  Revealer.
   --  Since: gtk+ 3.10
   --  "reveal_child": True to reveal the child

   function Get_Transition_Duration
      (Self : not null access Gtk_Revealer_Record) return Guint;
   --  Returns the amount of time (in milliseconds) that transitions will
   --  take.
   --  Since: gtk+ 3.10

   procedure Set_Transition_Duration
      (Self     : not null access Gtk_Revealer_Record;
       Duration : Guint);
   --  Sets the duration that transitions will take.
   --  Since: gtk+ 3.10
   --  "duration": the new duration, in milliseconds

   function Get_Transition_Type
      (Self : not null access Gtk_Revealer_Record)
       return Gtk_Revealer_Transition_Type;
   --  Gets the type of animation that will be used for transitions in
   --  Revealer.
   --  Since: gtk+ 3.10

   procedure Set_Transition_Type
      (Self       : not null access Gtk_Revealer_Record;
       Transition : Gtk_Revealer_Transition_Type);
   --  Sets the type of animation that will be used for transitions in
   --  Revealer. Available types include various kinds of fades and slides.
   --  Since: gtk+ 3.10
   --  "transition": the new transition type

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Revealed_Property : constant Glib.Properties.Property_Boolean;

   Reveal_Child_Property : constant Glib.Properties.Property_Boolean;

   Transition_Duration_Property : constant Glib.Properties.Property_Uint;

   Transition_Type_Property : constant Gtk.Revealer.Property_Gtk_Revealer_Transition_Type;
   --  Type: Gtk_Revealer_Transition_Type

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Revealer_Record, Gtk_Revealer);
   function "+"
     (Widget : access Gtk_Revealer_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Revealer
   renames Implements_Gtk_Buildable.To_Object;

private
   Transition_Type_Property : constant Gtk.Revealer.Property_Gtk_Revealer_Transition_Type :=
     Gtk.Revealer.Build ("transition-type");
   Transition_Duration_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("transition-duration");
   Reveal_Child_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reveal-child");
   Child_Revealed_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("child-revealed");
end Gtk.Revealer;
