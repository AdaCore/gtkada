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
--  GtkArrow should be used to draw simple arrows that need to point in one of
--  the four cardinal directions (up, down, left, or right). The style of the
--  arrow can be one of shadow in, shadow out, etched in, or etched out. Note
--  that these directions and style types may be amended in versions of GTK+ to
--  come.
--
--  GtkArrow will fill any space alloted to it, but since it is inherited from
--  Gtk.Misc.Gtk_Misc, it can be padded and/or aligned, to fill exactly the
--  space the programmer desires.
--
--  Arrows are created with a call to Gtk.Arrow.Gtk_New. The direction or
--  style of an arrow can be changed after creation by using Gtk.Arrow.Set.
--
--  GtkArrow has been deprecated; you can simply use a Gtk.Image.Gtk_Image
--  with a suitable icon name, such as "pan-down-symbolic". When replacing
--  GtkArrow by an image, pay attention to the fact that GtkArrow is doing
--  automatic flipping between GTK_ARROW_LEFT and GTK_ARROW_RIGHT, depending on
--  the text direction. To get the same effect with an image, use the icon
--  names "pan-start-symbolic" and "pan-end-symbolic", which react to the text
--  direction.
--
--  </description>
--  <screenshot>gtk-arrow</screenshot>
--  <testgtk>create_arrow.adb</testgtk>

pragma Warnings (Off, "*is already use-visible*");
with Glib;          use Glib;
with Glib.Types;    use Glib.Types;
with Gtk.Buildable; use Gtk.Buildable;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.Misc;      use Gtk.Misc;

package Gtk.Arrow is

   type Gtk_Arrow_Record is new Gtk_Misc_Record with null record;
   type Gtk_Arrow is access all Gtk_Arrow_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Arrow       : out Gtk_Arrow;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type);
   procedure Initialize
      (Arrow       : not null access Gtk_Arrow_Record'Class;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type);
   --  Creates a new Gtk.Arrow.Gtk_Arrow widget.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  "arrow_type": a valid Gtk.Enums.Gtk_Arrow_Type.
   --  "shadow_type": a valid Gtk.Enums.Gtk_Shadow_Type.

   function Gtk_Arrow_New
      (Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type) return Gtk_Arrow;
   --  Creates a new Gtk.Arrow.Gtk_Arrow widget.
   --  "arrow_type": a valid Gtk.Enums.Gtk_Arrow_Type.
   --  "shadow_type": a valid Gtk.Enums.Gtk_Shadow_Type.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_arrow_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set
      (Arrow       : not null access Gtk_Arrow_Record;
       Arrow_Type  : Gtk.Enums.Gtk_Arrow_Type;
       Shadow_Type : Gtk.Enums.Gtk_Shadow_Type);
   pragma Obsolescent (Set);
   --  Sets the direction and style of the Gtk.Arrow.Gtk_Arrow, Arrow.
   --  Deprecated since 3.14, 1
   --  "arrow_type": a valid Gtk.Enums.Gtk_Arrow_Type.
   --  "shadow_type": a valid Gtk.Enums.Gtk_Shadow_Type.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Arrow_Type_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type;

   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Buildable"

   package Implements_Gtk_Buildable is new Glib.Types.Implements
     (Gtk.Buildable.Gtk_Buildable, Gtk_Arrow_Record, Gtk_Arrow);
   function "+"
     (Widget : access Gtk_Arrow_Record'Class)
   return Gtk.Buildable.Gtk_Buildable
   renames Implements_Gtk_Buildable.To_Interface;
   function "-"
     (Interf : Gtk.Buildable.Gtk_Buildable)
   return Gtk_Arrow
   renames Implements_Gtk_Buildable.To_Object;

private
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow-type");
   Arrow_Type_Property : constant Gtk.Enums.Property_Gtk_Arrow_Type :=
     Gtk.Enums.Build ("arrow-type");
end Gtk.Arrow;
