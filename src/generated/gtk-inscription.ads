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

--  Shows text in a predefined area.
--
--  You likely want to use `GtkLabel` instead as this widget is intended only
--  for a small subset of use cases. The main scenario envisaged is inside
--  lists such as `GtkColumnView`.
--
--  While a `GtkLabel` sizes itself depending on the text that is displayed,
--  `GtkInscription` is given a size and inscribes the given text into that
--  space as well as it can.
--
--  Users of this widget should take care to plan behaviour for the common
--  case where the text doesn't fit exactly in the allocated space.
--
--  ## CSS nodes
--
--  `GtkInscription` has a single CSS node with the name label.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Properties;         use Glib.Properties;
with Glib.Types;              use Glib.Types;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Accessible_Text;     use Gtk.Accessible_Text;
with Gtk.Atcontext;           use Gtk.Atcontext;
with Gtk.Buildable;           use Gtk.Buildable;
with Gtk.Constraint_Target;   use Gtk.Constraint_Target;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Widget;              use Gtk.Widget;
with Interfaces.C;            use Interfaces.C;
with Pango.Attributes;        use Pango.Attributes;
with Pango.Enums;             use Pango.Enums;

package Gtk.Inscription is

   type Gtk_Inscription_Record is new Gtk_Widget_Record with null record;
   type Gtk_Inscription is access all Gtk_Inscription_Record'Class;

   type Gtk_Inscription_Overflow is (
      Clip,
      Ellipsize_Start,
      Ellipsize_Middle,
      Ellipsize_End);
   pragma Convention (C, Gtk_Inscription_Overflow);
   --  The different methods to handle text in Gtk.Inscription.Gtk_Inscription
   --  when it doesn't fit the available space.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Inscription_Overflow_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Inscription_Overflow);
   type Property_Gtk_Inscription_Overflow is new Gtk_Inscription_Overflow_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Inscription; Text : UTF8_String := "");
   procedure Initialize
      (Self : not null access Gtk_Inscription_Record'Class;
       Text : UTF8_String := "");
   --  Creates a new `GtkInscription` with the given text.
   --  Since: gtk+ 4.8
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Text The text to display.

   function Gtk_Inscription_New
      (Text : UTF8_String := "") return Gtk_Inscription;
   --  Creates a new `GtkInscription` with the given text.
   --  Since: gtk+ 4.8
   --  @param Text The text to display.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_inscription_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Attributes
      (Self : not null access Gtk_Inscription_Record)
       return Pango.Attributes.Pango_Attr_List;
   --  Gets the inscription's attribute list.
   --  Since: gtk+ 4.8
   --  @return the attribute list. Has transfer-ownership='none'.

   procedure Set_Attributes
      (Self  : not null access Gtk_Inscription_Record;
       Attrs : Pango.Attributes.Pango_Attr_List);
   --  Apply attributes to the inscription text.
   --  These attributes will not be evaluated for sizing the inscription.
   --  Since: gtk+ 4.8
   --  @param Attrs a [structPango.AttrList]

   function Get_Min_Chars
      (Self : not null access Gtk_Inscription_Record) return Guint;
   --  Gets the `min-chars` of the inscription.
   --  See the [propertyGtk.Inscription:min-chars] property.
   --  Since: gtk+ 4.8
   --  @return the min-chars property

   procedure Set_Min_Chars
      (Self      : not null access Gtk_Inscription_Record;
       Min_Chars : Guint);
   --  Sets the `min-chars` of the inscription.
   --  See the [propertyGtk.Inscription:min-chars] property.
   --  Since: gtk+ 4.8
   --  @param Min_Chars the minimum number of characters that should fit,
   --  approximately

   function Get_Min_Lines
      (Self : not null access Gtk_Inscription_Record) return Guint;
   --  Gets the `min-lines` of the inscription.
   --  See the [propertyGtk.Inscription:min-lines] property.
   --  Since: gtk+ 4.8
   --  @return the min-lines property

   procedure Set_Min_Lines
      (Self      : not null access Gtk_Inscription_Record;
       Min_Lines : Guint);
   --  Sets the `min-lines` of the inscription.
   --  See the [propertyGtk.Inscription:min-lines] property.
   --  Since: gtk+ 4.8
   --  @param Min_Lines the minimum number of lines that should fit,
   --  approximately

   function Get_Nat_Chars
      (Self : not null access Gtk_Inscription_Record) return Guint;
   --  Gets the `nat-chars` of the inscription.
   --  See the [propertyGtk.Inscription:nat-chars] property.
   --  Since: gtk+ 4.8
   --  @return the nat-chars property

   procedure Set_Nat_Chars
      (Self      : not null access Gtk_Inscription_Record;
       Nat_Chars : Guint);
   --  Sets the `nat-chars` of the inscription.
   --  See the [propertyGtk.Inscription:nat-chars] property.
   --  Since: gtk+ 4.8
   --  @param Nat_Chars the number of characters that should ideally fit,
   --  approximately

   function Get_Nat_Lines
      (Self : not null access Gtk_Inscription_Record) return Guint;
   --  Gets the `nat-lines` of the inscription.
   --  See the [propertyGtk.Inscription:nat-lines] property.
   --  Since: gtk+ 4.8
   --  @return the nat-lines property

   procedure Set_Nat_Lines
      (Self      : not null access Gtk_Inscription_Record;
       Nat_Lines : Guint);
   --  Sets the `nat-lines` of the inscription.
   --  See the [propertyGtk.Inscription:nat-lines] property.
   --  Since: gtk+ 4.8
   --  @param Nat_Lines the number of lines that should ideally fit

   function Get_Text
      (Self : not null access Gtk_Inscription_Record) return UTF8_String;
   --  Gets the text that is displayed.
   --  Since: gtk+ 4.8
   --  @return The displayed text

   procedure Set_Text
      (Self : not null access Gtk_Inscription_Record;
       Text : UTF8_String := "");
   --  Sets the text to be displayed.
   --  Since: gtk+ 4.8
   --  @param Text The text to display

   function Get_Text_Overflow
      (Self : not null access Gtk_Inscription_Record)
       return Gtk_Inscription_Overflow;
   --  Gets the inscription's overflow method.
   --  Since: gtk+ 4.8
   --  @return the overflow method

   procedure Set_Text_Overflow
      (Self     : not null access Gtk_Inscription_Record;
       Overflow : Gtk_Inscription_Overflow);
   --  Sets what to do when the text doesn't fit.
   --  Since: gtk+ 4.8
   --  @param Overflow the overflow method to use

   function Get_Wrap_Mode
      (Self : not null access Gtk_Inscription_Record)
       return Pango.Enums.Wrap_Mode;
   --  Returns line wrap mode used by the inscription.
   --  See [methodGtk.Inscription.set_wrap_mode].
   --  Since: gtk+ 4.8
   --  @return the line wrap mode

   procedure Set_Wrap_Mode
      (Self      : not null access Gtk_Inscription_Record;
       Wrap_Mode : Pango.Enums.Wrap_Mode);
   --  Controls how line wrapping is done.
   --  Since: gtk+ 4.8
   --  @param Wrap_Mode the line wrapping mode

   function Get_Xalign
      (Self : not null access Gtk_Inscription_Record)
       return Interfaces.C.C_float;
   --  Gets the `xalign` of the inscription.
   --  See the [propertyGtk.Inscription:xalign] property.
   --  Since: gtk+ 4.8
   --  @return the xalign property

   procedure Set_Xalign
      (Self   : not null access Gtk_Inscription_Record;
       Xalign : Interfaces.C.C_float);
   --  Sets the `xalign` of the inscription.
   --  See the [propertyGtk.Inscription:xalign] property.
   --  Since: gtk+ 4.8
   --  @param Xalign the new xalign value, between 0 and 1

   function Get_Yalign
      (Self : not null access Gtk_Inscription_Record)
       return Interfaces.C.C_float;
   --  Gets the `yalign` of the inscription.
   --  See the [propertyGtk.Inscription:yalign] property.
   --  Since: gtk+ 4.8
   --  @return the yalign property

   procedure Set_Yalign
      (Self   : not null access Gtk_Inscription_Record;
       Yalign : Interfaces.C.C_float);
   --  Sets the `yalign` of the inscription.
   --  See the [propertyGtk.Inscription:yalign] property.
   --  Since: gtk+ 4.8
   --  @param Yalign the new yalign value, between 0 and 1

   procedure Set_Markup
      (Self   : not null access Gtk_Inscription_Record;
       Markup : UTF8_String := "");
   --  Utility function to set the text and attributes to be displayed.
   --  See the [propertyGtk.Inscription:markup] property.
   --  Since: gtk+ 4.8
   --  @param Markup The markup to display

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------
   --  Methods inherited from the Buildable interface are not duplicated here
   --  since they are meant to be used by tools, mostly. If you need to call
   --  them, use an explicit cast through the "-" operator below.

   procedure Announce
      (Self     : not null access Gtk_Inscription_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Inscription_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Inscription_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Inscription_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Inscription_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Inscription_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Inscription_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Inscription_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Inscription_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Inscription_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Inscription_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Inscription_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Inscription_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Inscription_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Inscription_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   procedure Update_Caret_Position
      (Self : not null access Gtk_Inscription_Record);

   procedure Update_Contents
      (Self    : not null access Gtk_Inscription_Record;
       Change  : Gtk.Enums.Gtk_Accessible_Text_Content_Change;
       Start   : Guint;
       The_End : Guint);

   procedure Update_Selection_Bound
      (Self : not null access Gtk_Inscription_Record);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Attributes_Property : constant Glib.Properties.Property_Object;
   --  Type: Pango.Attributes.Pango_Attr_List
   --  A list of style attributes to apply to the text of the inscription.

   Markup_Property : constant Glib.Properties.Property_String;
   --  Flags: write
   --  Utility property that sets both the [propertyGtk.Inscription:text] and
   --  [propertyGtk.Inscription:attributes] properties, mainly intended for use
   --  in GtkBuilder ui files to ease translation support and bindings.
   --
   --  This function uses [funcPango.parse_markup] to parse the markup into
   --  text and attributes. The markup must be valid. If you cannot ensure
   --  that, consider using [funcPango.parse_markup] and setting the two
   --  properties yourself.

   Min_Chars_Property : constant Glib.Properties.Property_Uint;
   --  The number of characters that should fit into the inscription at
   --  minimum.
   --
   --  This influences the requested width, not the width actually given to
   --  the widget, which might turn out to be larger.
   --
   --  Note that this is an approximate character width, so some characters
   --  might be wider and some might be thinner, so do not expect the number of
   --  characters to exactly match.
   --
   --  If you set this property to 0, the inscription will not request any
   --  width at all and its width will be determined entirely by its
   --  surroundings.

   Min_Lines_Property : constant Glib.Properties.Property_Uint;
   --  The number of lines that should fit into the inscription at minimum.
   --
   --  This influences the requested height, not the height actually given to
   --  the widget, which might turn out to be larger.
   --
   --  Note that this is an approximate line height, so if the text uses
   --  things like fancy Unicode or attribute that influence the height, the
   --  text might not fit.
   --
   --  If you set this property to 0, the inscription will not request any
   --  height at all and its height will be determined entirely by its
   --  surroundings.

   Nat_Chars_Property : constant Glib.Properties.Property_Uint;
   --  The number of characters that should ideally fit into the inscription.
   --
   --  This influences the requested width, not the width actually given to
   --  the widget. The widget might turn out larger as well as smaller.
   --
   --  If this property is set to a value smaller than
   --  [propertyGtk.Inscription:min-chars], that value will be used. In
   --  particular, for the default value of 0, this will always be the case.

   Nat_Lines_Property : constant Glib.Properties.Property_Uint;
   --  The number of lines that should ideally fit into the inscription.
   --
   --  This influences the requested height, not the height actually given to
   --  the widget. The widget might turn out larger as well as smaller.
   --
   --  If this property is set to a value smaller than
   --  [propertyGtk.Inscription:min-lines], that value will be used. In
   --  particular, for the default value of 0, this will always be the case.

   Text_Property : constant Glib.Properties.Property_String;
   --  The displayed text.

   Text_Overflow_Property : constant Gtk.Inscription.Property_Gtk_Inscription_Overflow;
   --  Type: Gtk_Inscription_Overflow
   --  The overflow method to use for the text.

   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode;
   --  Type: Pango.Enums.Wrap_Mode
   --  Controls how the line wrapping is done.
   --
   --  Note that unlike `GtkLabel`, the default here is
   --  Pango.Enums.Pango_Wrap_Word_Char.

   Xalign_Property : constant Glib.Properties.Property_Float;
   --  The horizontal alignment of the text inside the allocated size.
   --
   --  Compare this to [propertyGtk.Widget:halign], which determines how the
   --  inscription's size allocation is positioned in the available space.

   Yalign_Property : constant Glib.Properties.Property_Float;
   --  The vertical alignment of the text inside the allocated size.
   --
   --  Compare this to [propertyGtk.Widget:valign], which determines how the
   --  inscription's size allocation is positioned in the available space.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"
   --
   --  - "Gtk.AccessibleText"
   --
   --  - "Gtk.Buildable"
   --
   --  - "Gtk.ConstraintTarget"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Inscription_Record, Gtk_Inscription);
   function "+"
     (Widget : access Gtk_Inscription_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Inscription
   renames Implements_Gtk_Accessible.To_Object;

   package Implements_Gtk_Accessible_Text is new Glib.Types.Implements
     (Gtk.Accessible_Text.Gtk_Accessible_Text, Gtk_Inscription_Record, Gtk_Inscription);
   function "+"
     (Widget : access Gtk_Inscription_Record'Class)
   return Gtk.Accessible_Text.Gtk_Accessible_Text
   renames Implements_Gtk_Accessible_Text.To_Interface;
   function "-"
     (Interf : Gtk.Accessible_Text.Gtk_Accessible_Text)
   return Gtk_Inscription
   renames Implements_Gtk_Accessible_Text.To_Object;

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Inscription_Record, Gtk_Inscription);
   function "+"
     (Widget : access Gtk_Inscription_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Inscription
   renames Implements_Gtk_Buildable.To_Object;

   package Implements_Gtk_Constraint_Target is new Glib.Types.Implements
     (Gtk.Constraint_Target.Gtk_Constraint_Target, Gtk_Inscription_Record, Gtk_Inscription);
   function "+"
     (Widget : access Gtk_Inscription_Record'Class)
   return Gtk.Constraint_Target.Gtk_Constraint_Target
   renames Implements_Gtk_Constraint_Target.To_Interface;
   function "-"
     (Interf : Gtk.Constraint_Target.Gtk_Constraint_Target)
   return Gtk_Inscription
   renames Implements_Gtk_Constraint_Target.To_Object;

private
   Yalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("yalign");
   Xalign_Property : constant Glib.Properties.Property_Float :=
     Glib.Properties.Build ("xalign");
   Wrap_Mode_Property : constant Pango.Enums.Property_Wrap_Mode :=
     Pango.Enums.Build ("wrap-mode");
   Text_Overflow_Property : constant Gtk.Inscription.Property_Gtk_Inscription_Overflow :=
     Gtk.Inscription.Build ("text-overflow");
   Text_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("text");
   Nat_Lines_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("nat-lines");
   Nat_Chars_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("nat-chars");
   Min_Lines_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("min-lines");
   Min_Chars_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("min-chars");
   Markup_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("markup");
   Attributes_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("attributes");
end Gtk.Inscription;
