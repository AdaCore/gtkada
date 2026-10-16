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

--  Contains information found when looking up an icon in `GtkIconTheme` or
--  loading it from a file.
--
--  `GtkIconPaintable` implements `GdkPaintable` and `GtkSymbolicPaintable`.

pragma Warnings (Off, "*is already use-visible*");
with Gdk.Paintable;          use Gdk.Paintable;
with Gdk.Snapshot;           use Gdk.Snapshot;
with Glib;                   use Glib;
with Glib.GFile;             use Glib.GFile;
with Glib.Object;            use Glib.Object;
with Glib.Properties;        use Glib.Properties;
with Glib.Types;             use Glib.Types;
with Gtk.Symbolic_Paintable; use Gtk.Symbolic_Paintable;

package Gtk.Icon_Paintable is

   type Gtk_Icon_Paintable_Record is new GObject_Record with null record;
   type Gtk_Icon_Paintable is access all Gtk_Icon_Paintable_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New_For_File
      (Self  : out Gtk_Icon_Paintable;
       File  : Glib.GFile.Gfile;
       Size  : Glib.Gint;
       Scale : Glib.Gint);
   procedure Initialize_For_File
      (Self  : not null access Gtk_Icon_Paintable_Record'Class;
       File  : Glib.GFile.Gfile;
       Size  : Glib.Gint;
       Scale : Glib.Gint);
   --  Creates a `GtkIconPaintable` for a file with a given size and scale.
   --  The icon can then be rendered by using it as a `GdkPaintable`.
   --  Initialize_For_File does nothing if the object was already created with
   --  another call to Initialize* or G_New.
   --  @param File a `GFile`
   --  @param Size desired icon size, in application pixels
   --  @param Scale the desired scale

   function Gtk_Icon_Paintable_New_For_File
      (File  : Glib.GFile.Gfile;
       Size  : Glib.Gint;
       Scale : Glib.Gint) return Gtk_Icon_Paintable;
   --  Creates a `GtkIconPaintable` for a file with a given size and scale.
   --  The icon can then be rendered by using it as a `GdkPaintable`.
   --  @param File a `GFile`
   --  @param Size desired icon size, in application pixels
   --  @param Scale the desired scale

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_icon_paintable_get_type");

   -------------
   -- Methods --
   -------------

   function Get_File
      (Self : not null access Gtk_Icon_Paintable_Record)
       return Glib.GFile.Gfile;
   --  Gets the `GFile` that was used to load the icon.
   --  Returns null if the icon was not loaded from a file.
   --  @return the `GFile` for the icon

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Compute_Concrete_Size
      (Self             : not null access Gtk_Icon_Paintable_Record;
       Specified_Width  : Gdouble;
       Specified_Height : Gdouble;
       Default_Width    : Gdouble;
       Default_Height   : Gdouble;
       Concrete_Width   : out Gdouble;
       Concrete_Height  : out Gdouble);

   function Get_Current_Image
      (Self : not null access Gtk_Icon_Paintable_Record)
       return Gdk.Paintable.Gdk_Paintable;

   function Get_Flags
      (Self : not null access Gtk_Icon_Paintable_Record)
       return Gdk.Paintable.Gdk_Paintable_Flags;

   function Get_Intrinsic_Aspect_Ratio
      (Self : not null access Gtk_Icon_Paintable_Record) return Gdouble;

   function Get_Intrinsic_Height
      (Self : not null access Gtk_Icon_Paintable_Record) return Glib.Gint;

   function Get_Intrinsic_Width
      (Self : not null access Gtk_Icon_Paintable_Record) return Glib.Gint;

   procedure Invalidate_Contents
      (Self : not null access Gtk_Icon_Paintable_Record);

   procedure Invalidate_Size
      (Self : not null access Gtk_Icon_Paintable_Record);

   procedure Snapshot
      (Self     : not null access Gtk_Icon_Paintable_Record;
       Snapshot : not null access Gdk.Snapshot.Gdk_Snapshot_Record'Class;
       Width    : Gdouble;
       Height   : Gdouble);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   File_Property : constant Glib.Properties.Property_Interface;
   --  Type: Glib.GFile.Gfile
   --  The file representing the icon, if any.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The icon name that was chosen during lookup.

   Is_Symbolic_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the icon is symbolic or not.

   Scale_Property : constant Glib.Properties.Property_Int;

   Size_Property : constant Glib.Properties.Property_Int;

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gdk.Paintable"
   --
   --  - "Gtk.SymbolicPaintable"

   package Implements_Gdk_Paintable is new Glib.Types.Implements
     (Gdk.Paintable.Gdk_Paintable, Gtk_Icon_Paintable_Record, Gtk_Icon_Paintable);
   function "+"
     (Widget : access Gtk_Icon_Paintable_Record'Class)
   return Gdk.Paintable.Gdk_Paintable
   renames Implements_Gdk_Paintable.To_Interface;
   function "-"
     (Interf : Gdk.Paintable.Gdk_Paintable)
   return Gtk_Icon_Paintable
   renames Implements_Gdk_Paintable.To_Object;

   package Implements_Gtk_Symbolic_Paintable is new Glib.Types.Implements
     (Gtk.Symbolic_Paintable.Gtk_Symbolic_Paintable, Gtk_Icon_Paintable_Record, Gtk_Icon_Paintable);
   function "+"
     (Widget : access Gtk_Icon_Paintable_Record'Class)
   return Gtk.Symbolic_Paintable.Gtk_Symbolic_Paintable
   renames Implements_Gtk_Symbolic_Paintable.To_Interface;
   function "-"
     (Interf : Gtk.Symbolic_Paintable.Gtk_Symbolic_Paintable)
   return Gtk_Icon_Paintable
   renames Implements_Gtk_Symbolic_Paintable.To_Object;

private
   Size_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("size");
   Scale_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("scale");
   Is_Symbolic_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("is-symbolic");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   File_Property : constant Glib.Properties.Property_Interface :=
     Glib.Properties.Build ("file");
end Gtk.Icon_Paintable;
