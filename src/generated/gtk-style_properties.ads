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
--  GtkStyleProperties provides the storage for style information that is used
--  by Gtk.Style_Context.Gtk_Style_Context and other
--  Gtk.Style_Provider.Gtk_Style_Provider implementations.
--
--  Before style properties can be stored in GtkStyleProperties, they must be
--  registered with gtk_style_properties_register_property.
--
--  Unless you are writing a Gtk.Style_Provider.Gtk_Style_Provider
--  implementation, you are unlikely to use this API directly, as
--  gtk_style_context_get and its variants are the preferred way to access
--  styling information from widget implementations and theming engine
--  implementations should use the APIs provided by
--  Gtk.Theming_Engine.Gtk_Theming_Engine instead.
--
--  Gtk.Style_Properties.Gtk_Style_Properties has been deprecated in GTK 3.16.
--  The CSS machinery does not use it anymore and all users of this object have
--  been deprecated.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;               use Glib;
with Glib.Object;        use Glib.Object;
with Glib.Types;         use Glib.Types;
with Glib.Values;        use Glib.Values;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Style_Provider; use Gtk.Style_Provider;
with Gtk.Symbolic_Color; use Gtk.Symbolic_Color;
with Gtk.Widget;         use Gtk.Widget;

package Gtk.Style_Properties is

   type Gtk_Style_Properties_Record is new GObject_Record with null record;
   type Gtk_Style_Properties is access all Gtk_Style_Properties_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Style_Properties);
   procedure Initialize
      (Self : not null access Gtk_Style_Properties_Record'Class);
   --  Returns a newly created Gtk.Style_Properties.Gtk_Style_Properties
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Style_Properties_New return Gtk_Style_Properties;
   --  Returns a newly created Gtk.Style_Properties.Gtk_Style_Properties

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_style_properties_get_type");

   -------------
   -- Methods --
   -------------

   procedure Clear (Self : not null access Gtk_Style_Properties_Record);
   pragma Obsolescent (Clear);
   --  Clears all style information from Props.
   --  Deprecated since 3.16, 1

   procedure Get_Property
      (Self     : not null access Gtk_Style_Properties_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : out Glib.Values.GValue;
       Exists   : out Boolean);
   pragma Obsolescent (Get_Property);
   --  Gets a style property from Props for the given state. When done with
   --  Value, g_value_unset needs to be called to free any allocated memory.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "property": style property name
   --  "state": state to retrieve the property value for
   --  "value": return location for the style property value.

   procedure Set_Property
      (Self     : not null access Gtk_Style_Properties_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags;
       Value    : in out Glib.Values.GValue);
   pragma Obsolescent (Set_Property);
   --  Sets a styling property in Props.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "property": styling property to set
   --  "state": state to set the value for
   --  "value": new value for the property

   function Lookup_Color
      (Self : not null access Gtk_Style_Properties_Record;
       Name : UTF8_String) return Gtk.Symbolic_Color.Gtk_Symbolic_Color;
   pragma Obsolescent (Lookup_Color);
   --  Returns the symbolic color that is mapped to Name.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1
   --  "name": color name to lookup

   procedure Map_Color
      (Self  : not null access Gtk_Style_Properties_Record;
       Name  : UTF8_String;
       Color : Gtk.Symbolic_Color.Gtk_Symbolic_Color);
   pragma Obsolescent (Map_Color);
   --  Maps Color so it can be referenced by Name. See
   --  Gtk.Style_Properties.Lookup_Color
   --  Since: gtk+ 3.0
   --  Deprecated since 3.8, 1
   --  "name": color name
   --  "color": Gtk.Symbolic_Color.Gtk_Symbolic_Color to map Name to

   procedure Merge
      (Self           : not null access Gtk_Style_Properties_Record;
       Props_To_Merge : not null access Gtk_Style_Properties_Record'Class;
       Replace        : Boolean);
   pragma Obsolescent (Merge);
   --  Merges into Props all the style information contained in
   --  Props_To_Merge. If Replace is True, the values will be overwritten, if
   --  it is False, the older values will prevail.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "props_to_merge": a second Gtk.Style_Properties.Gtk_Style_Properties
   --  "replace": whether to replace values or not

   procedure Unset_Property
      (Self     : not null access Gtk_Style_Properties_Record;
       Property : UTF8_String;
       State    : Gtk.Enums.Gtk_State_Flags);
   pragma Obsolescent (Unset_Property);
   --  Unsets a style property in Props.
   --  Since: gtk+ 3.0
   --  Deprecated since 3.16, 1
   --  "property": property to unset
   --  "state": state to unset

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Get_Style_Property
      (Self  : not null access Gtk_Style_Properties_Record;
       Path  : Gtk.Widget.Gtk_Widget_Path;
       State : Gtk.Enums.Gtk_State_Flags;
       Pspec : in out Glib.Param_Spec;
       Value : out Glib.Values.GValue;
       Found : out Boolean);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "StyleProvider"

   package Implements_Gtk_Style_Provider is new Glib.Types.Implements
     (Gtk.Style_Provider.Gtk_Style_Provider, Gtk_Style_Properties_Record, Gtk_Style_Properties);
   function "+"
     (Widget : access Gtk_Style_Properties_Record'Class)
   return Gtk.Style_Provider.Gtk_Style_Provider
   renames Implements_Gtk_Style_Provider.To_Interface;
   function "-"
     (Interf : Gtk.Style_Provider.Gtk_Style_Provider)
   return Gtk_Style_Properties
   renames Implements_Gtk_Style_Provider.To_Object;

end Gtk.Style_Properties;
