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
--  GtkBuildable allows objects to extend and customize their deserialization
--  from [GtkBuilder UI descriptions][BUILDER-UI]. The interface includes
--  methods for setting names and properties of objects, parsing custom tags
--  and constructing child objects.
--
--  The GtkBuildable interface is implemented by all widgets and many of the
--  non-widget objects that are provided by GTK+. The main user of this
--  interface is Gtk.Builder.Gtk_Builder. There should be very little need for
--  applications to call any of these functions directly.
--
--  An object only needs to implement this interface if it needs to extend the
--  Gtk.Builder.Gtk_Builder format or run any extra routines at deserialization
--  time.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;        use Glib;
with Glib.Error;  use Glib.Error;
with Glib.Object; use Glib.Object;
with Glib.Types;  use Glib.Types;
with Glib.Values; use Glib.Values;
with Gtk.Builder; use Gtk.Builder;

package Gtk.Buildable is

   type Gtk_Buildable is new Glib.Types.GType_Interface;
   Null_Gtk_Buildable : constant Gtk_Buildable;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_buildable_get_type");

   -------------
   -- Methods --
   -------------

   procedure Add_Child
      (Self     : Gtk_Buildable;
       Builder  : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Child    : not null access Glib.Object.GObject_Record'Class;
       The_Type : UTF8_String := "";
       Error    : Glib.Error.GError);
   --  Adds a child to Buildable. Type is an optional string describing how
   --  the child should be added.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "child": child to add
   --  "type": kind of child or null

   function Construct_Child
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String) return Glib.Object.GObject;
   --  Constructs a child of Buildable with the name Name.
   --  Gtk.Builder.Gtk_Builder calls this function if a "constructor" has been
   --  specified in the UI definition.
   --  Since: gtk+ 2.12
   --  "builder": Gtk.Builder.Gtk_Builder used to construct this object
   --  "name": name of child to construct

   procedure Custom_Finished
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : System.Address);
   --  This is similar to Gtk.Buildable.Parser_Finished but is called once for
   --  each custom tag handled by the Buildable.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "child": child object or null for non-child tags
   --  "tagname": the name of the tag
   --  "data": user data created in custom_tag_start

   procedure Custom_Tag_End
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Child   : access Glib.Object.GObject_Record'Class;
       Tagname : UTF8_String;
       Data    : in out System.Address);
   --  This is called at the end of each custom element handled by the
   --  buildable.
   --  Since: gtk+ 2.12
   --  "builder": Gtk.Builder.Gtk_Builder used to construct this object
   --  "child": child object or null for non-child tags
   --  "tagname": name of tag
   --  "data": user data that will be passed in to parser functions

   function Get_Internal_Child
      (Self      : Gtk_Buildable;
       Builder   : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Childname : UTF8_String) return Glib.Object.GObject;
   --  Get the internal child called Childname of the Buildable object.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "childname": name of child

   function Get_Name (Self : Gtk_Buildable) return UTF8_String;
   --  Gets the name of the Buildable object.
   --  Gtk.Builder.Gtk_Builder sets the name based on the [GtkBuilder UI
   --  definition][BUILDER-UI] used to construct the Buildable.
   --  Since: gtk+ 2.12

   procedure Set_Name (Self : Gtk_Buildable; Name : UTF8_String);
   --  Sets the name of the Buildable object.
   --  Since: gtk+ 2.12
   --  "name": name to set

   procedure Parser_Finished
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class);
   --  Called when the builder finishes the parsing of a [GtkBuilder UI
   --  definition][BUILDER-UI]. Note that this will be called once for each
   --  time Gtk.Builder.Add_From_File or Gtk.Builder.Add_From_String is called
   --  on a builder.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder

   procedure Set_Buildable_Property
      (Self    : Gtk_Buildable;
       Builder : not null access Gtk.Builder.Gtk_Builder_Record'Class;
       Name    : UTF8_String;
       Value   : in out Glib.Values.GValue);
   --  Sets the property name Name to Value on the Buildable object.
   --  Since: gtk+ 2.12
   --  "builder": a Gtk.Builder.Gtk_Builder
   --  "name": name of property
   --  "value": value of property

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk_Buildable"

   function "+" (W : Gtk_Buildable) return Gtk_Buildable;
   pragma Inline ("+");

private

Null_Gtk_Buildable : constant Gtk_Buildable :=
   Gtk_Buildable (Glib.Types.Null_Interface);
end Gtk.Buildable;
