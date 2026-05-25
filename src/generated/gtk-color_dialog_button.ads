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

--  Opens a color chooser dialog to select a color.
--
--  <picture> <source srcset="color-button-dark.png"
--  media="(prefers-color-scheme: dark)"> <img alt="An example
--  GtkColorDialogButton" src="color-button.png"> </picture>
--  It is suitable widget for selecting a color in a preference dialog.
--
--  # CSS nodes
--
--  ``` colorbutton ╰── button.color ╰── [content] ```
--
--  `GtkColorDialogButton` has a single CSS node with name colorbutton which
--  contains a button node. To differentiate it from a plain `GtkButton`, it
--  gets the .color style class.
--
--  <group>Buttons and Toggles</group>

pragma Warnings (Off, "*is already use-visible*");
with Gdk.RGBA;              use Gdk.RGBA;
with Glib;                  use Glib;
with Glib.Object;           use Glib.Object;
with Glib.Properties;       use Glib.Properties;
with Glib.Types;            use Glib.Types;
with Gtk.Accessible;        use Gtk.Accessible;
with Gtk.Atcontext;         use Gtk.Atcontext;
with Gtk.Buildable;         use Gtk.Buildable;
with Gtk.Color_Dialog;      use Gtk.Color_Dialog;
with Gtk.Constraint_Target; use Gtk.Constraint_Target;
with Gtk.Widget;            use Gtk.Widget;

package Gtk.Color_Dialog_Button is

   type Gtk_Color_Dialog_Button_Record is new Gtk_Widget_Record with null record;
   type Gtk_Color_Dialog_Button is access all Gtk_Color_Dialog_Button_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Color_Dialog_Button;
       Dialog : access Gtk.Color_Dialog.Gtk_Color_Dialog_Record'Class);
   procedure Initialize
      (Self   : not null access Gtk_Color_Dialog_Button_Record'Class;
       Dialog : access Gtk.Color_Dialog.Gtk_Color_Dialog_Record'Class);
   --  Creates a new `GtkColorDialogButton` with the given `GtkColorDialog`.
   --  You can pass `NULL` to this function and set a `GtkColorDialog` later.
   --  The button will be insensitive until that happens.
   --  Since: gtk+ 4.10
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Dialog the `GtkColorDialog` to use

   function Gtk_Color_Dialog_Button_New
      (Dialog : access Gtk.Color_Dialog.Gtk_Color_Dialog_Record'Class)
       return Gtk_Color_Dialog_Button;
   --  Creates a new `GtkColorDialogButton` with the given `GtkColorDialog`.
   --  You can pass `NULL` to this function and set a `GtkColorDialog` later.
   --  The button will be insensitive until that happens.
   --  Since: gtk+ 4.10
   --  @param Dialog the `GtkColorDialog` to use

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_color_dialog_button_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Dialog
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gtk.Color_Dialog.Gtk_Color_Dialog;
   --  Returns the `GtkColorDialog` of Self.
   --  Since: gtk+ 4.10
   --  @return the `GtkColorDialog`

   procedure Set_Dialog
      (Self   : not null access Gtk_Color_Dialog_Button_Record;
       Dialog : not null access Gtk.Color_Dialog.Gtk_Color_Dialog_Record'Class);
   --  Sets a `GtkColorDialog` object to use for creating the color chooser
   --  dialog that is presented when the user clicks the button.
   --  Since: gtk+ 4.10
   --  @param Dialog the new `GtkColorDialog`

   function Get_Rgba
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gdk.RGBA.Gdk_RGBA;
   --  Returns the color of the button.
   --  This function is what should be used to obtain the color that was
   --  chosen by the user. To get informed about changes, listen to
   --  "notify::rgba".
   --  Since: gtk+ 4.10
   --  @return the color

   procedure Set_Rgba
      (Self  : not null access Gtk_Color_Dialog_Button_Record;
       Color : Gdk.RGBA.Gdk_RGBA);
   --  Sets the color of the button.
   --  Since: gtk+ 4.10
   --  @param Color the new color

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Color_Dialog_Button_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Color_Dialog_Button_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Color_Dialog_Button_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Color_Dialog_Button_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Color_Dialog_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Color_Dialog_Button_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Color_Dialog_Button_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Color_Dialog_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Color_Dialog_Button_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Color_Dialog_Button_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Dialog_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Color_Dialog.Gtk_Color_Dialog
   --  The `GtkColorDialog` that contains parameters for the color chooser
   --  dialog.

   Rgba_Property : constant Gdk.RGBA.Property_RGBA;
   --  Type: Gdk.RGBA.Gdk_RGBA
   --  The selected color.
   --
   --  This property can be set to give the button its initial color, and it
   --  will be updated to reflect the users choice in the color chooser dialog.
   --
   --  Listen to `notify::rgba` to get informed about changes to the buttons
   --  color.

   -------------
   -- Signals --
   -------------

   type Cb_Gtk_Color_Dialog_Button_Void is not null access procedure
     (Self : access Gtk_Color_Dialog_Button_Record'Class);

   type Cb_GObject_Void is not null access procedure
     (Self : access Glib.Object.GObject_Record'Class);

   Signal_Activate : constant Glib.Signal_Name := "activate";
   procedure On_Activate
      (Self  : not null access Gtk_Color_Dialog_Button_Record;
       Call  : Cb_Gtk_Color_Dialog_Button_Void;
       After : Boolean := False);
   procedure On_Activate
      (Self  : not null access Gtk_Color_Dialog_Button_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False);
   --  Emitted when the color dialog button is activated.
   --
   --  The `::activate` signal on `GtkColorDialogButton` is an action signal
   --  and emitting it causes the button to pop up its dialog.

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

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Color_Dialog_Button_Record, Gtk_Color_Dialog_Button);
   function "+"
     (Widget : access Gtk_Color_Dialog_Button_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Color_Dialog_Button
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Color_Dialog_Button_Record, Gtk_Color_Dialog_Button);
   function "+"
     (Widget : access Gtk_Color_Dialog_Button_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Color_Dialog_Button
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Color_Dialog_Button_Record, Gtk_Color_Dialog_Button);
   function "+"
     (Widget : access Gtk_Color_Dialog_Button_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Color_Dialog_Button
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Rgba_Property : constant Gdk.RGBA.Property_RGBA :=
     Gdk.RGBA.Build ("rgba");
   Dialog_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("dialog");
end Gtk.Color_Dialog_Button;
