-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  Properties are a fully general way to modify the appareance or behavior
--  of widgets. Most of the time, there exists a faster way to modify the
--  widget in the same fashion (for instance a direct call to a primitive
--  subprogram). However, the properties provide a general scheme to modify
--  these attributes.
--  For instance, they can be used to provide introspection on the widget
--  (to automatically retrive the attributes that can be modified), or if
--  you need to implement a tool like a GUI-Builder that is able to
--  manipulate any widget, even those that didn't exist when the tool was
--  written.
--
--  Two functions are provided for each type of property: Set_Property and
--  Get_Property, which allow easy modification of specific widget
--  properties. For instance, you could do the following:
--      declare
--          Button : Gtk_Button;
--      begin
--          Gtk_New (Button, "old label");
--          Set_Property (Button, Property_Label, "new label");
--      end;
--  to modify the label of a button.
--
--  Likewise, you can retrieve the current label with:
--      Current : String := Get_Property (Button, Property_label);
--
--  Dispatching is used ensure type-safety while using properties. The
--  appropriate Set_Property/Get_Property functions are called depending
--  on the type of the property you are trying to use. This is checked
--  statically by the compiler, which provides additional type-safety
--  compared to the C library.
--
--  Note that some properties are read-only, and thus do not have the
--  Set_Property subprogram defined.
--  </description>
--  <c_version>1.3.4</c_version>

with Glib.Object;
with Glib.Generic_Properties; use Glib.Generic_Properties;
pragma Elaborate_All (Glib.Generic_Properties);

package Glib.Properties is

   --  <doc_ignore>

   --  Definition of the types and subprograms.
   --  You can ignore this section.

   package Int_Properties is new
     Generic_Internal_Discrete_Property (Glib.Gint);
   package Uint_Properties is new
     Generic_Internal_Discrete_Property (Glib.Guint);

   --  </doc_ignore>


   --  Predefined types of properties. Additional types are available
   --  for most of the standard enumeration types, and you can create
   --  your own types (see Glib.Properties).

   type Property_Int       is new Int_Properties.Property;
   type Property_Uint      is new Int_Properties.Property;
   type Property_String_RO is new Glib.Property;
   type Property_String    is new Glib.Property;

   --  Special handling of string properties

   procedure Set_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String;
      Value : String);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String) return String;
   pragma Inline (Get_Property);

   function Get_Property
     (Object : access Glib.Object.GObject_Record'Class;
      Name : Property_String_RO) return String;
end Glib.Properties;

