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

--  Represents a link (i.e. a uri).
--
--  A widget that contains one or more links should implement the
--  [ifaceGtk.AccessibleHypertext] interface and return
--  `GtkAccessibleHyperlink` objects for each of the links.

pragma Warnings (Off, "*is already use-visible*");
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Types;               use Glib.Types;
with Gtk.Accessible;           use Gtk.Accessible;
with Gtk.Accessible_Hypertext; use Gtk.Accessible_Hypertext;
with Gtk.Accessible_Text;      use Gtk.Accessible_Text;
with Gtk.Atcontext;            use Gtk.Atcontext;

package Gtk.Accessible_Hyperlink is

   pragma Elaborate_Body;

   type Gtk_Accessible_Hyperlink_Record is new GObject_Record with null record;
   type Gtk_Accessible_Hyperlink is access all Gtk_Accessible_Hyperlink_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New
      (Self   : out Gtk_Accessible_Hyperlink;
       Parent : Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext;
       Index  : Guint;
       URI    : UTF8_String;
       Bounds : Gtk.Accessible_Text.Gtk_Accessible_Text_Range);
   procedure Initialize
      (Self   : not null access Gtk_Accessible_Hyperlink_Record'Class;
       Parent : Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext;
       Index  : Guint;
       URI    : UTF8_String;
       Bounds : Gtk.Accessible_Text.Gtk_Accessible_Text_Range);
   --  Creates an accessible object that represents a hyperlink.
   --  This is meant to be used with an implementation of the
   --  [ifaceGtk.AccessibleHypertext] interface.
   --  Since: gtk+ 4.22
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.
   --  @param Parent the parent
   --  @param Index the index of this link in the parent
   --  @param URI the uri
   --  @param Bounds the text range that the link occupies (or 0, 0)

   function Gtk_Accessible_Hyperlink_New
      (Parent : Gtk.Accessible_Hypertext.Gtk_Accessible_Hypertext;
       Index  : Guint;
       URI    : UTF8_String;
       Bounds : Gtk.Accessible_Text.Gtk_Accessible_Text_Range)
       return Gtk_Accessible_Hyperlink;
   --  Creates an accessible object that represents a hyperlink.
   --  This is meant to be used with an implementation of the
   --  [ifaceGtk.AccessibleHypertext] interface.
   --  Since: gtk+ 4.22
   --  @param Parent the parent
   --  @param Index the index of this link in the parent
   --  @param URI the uri
   --  @param Bounds the text range that the link occupies (or 0, 0)

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_accessible_hyperlink_get_type");

   -------------
   -- Methods --
   -------------

   procedure Set_Platform_State
      (Self    : not null access Gtk_Accessible_Hyperlink_Record;
       State   : Gtk.Accessible.Gtk_Accessible_Platform_State;
       Enabled : Boolean);
   --  Sets a platform state on the accessible.
   --  Since: gtk+ 4.22
   --  @param State the platform state to change
   --  @param Enabled the new value for the platform state

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Accessible_Hyperlink_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Accessible_Hyperlink_Record)
       return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Accessible_Hyperlink_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Accessible_Hyperlink_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Accessible_Hyperlink_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Accessible_Hyperlink_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Accessible_Hyperlink_Record;
       X      : access Glib.Gint;
       Y      : access Glib.Gint;
       Width  : access Glib.Gint;
       Height : access Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Accessible_Hyperlink_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Accessible_Hyperlink_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Accessible_Hyperlink_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Accessible_Hyperlink_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Accessible_Hyperlink_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Accessible_Hyperlink_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Accessible_Hyperlink_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Accessible_Hyperlink_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Accessible_Hyperlink_Record, Gtk_Accessible_Hyperlink);
   function "+"
     (Widget : access Gtk_Accessible_Hyperlink_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Accessible_Hyperlink
   renames Implements_Gtk_Accessible.To_Object;

end Gtk.Accessible_Hyperlink;
