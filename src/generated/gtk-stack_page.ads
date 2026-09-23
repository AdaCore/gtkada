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

--  An auxiliary class used by `GtkStack`.
--
--  <group>Layout containers</group>
--  <gtkada_demo>create_stack.adb</gtkada_demo>
--  <see>Gtk.Stack</see>

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Glib.Types;      use Glib.Types;
with Gtk.Accessible;  use Gtk.Accessible;
with Gtk.Atcontext;   use Gtk.Atcontext;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Stack_Page is

   type Gtk_Stack_Page_Record is new GObject_Record with null record;
   type Gtk_Stack_Page is access all Gtk_Stack_Page_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_stack_page_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Stack_Page_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the stack child to which Self belongs.
   --  @return the child to which Self belongs. Has transfer-ownership='none'.

   function Get_Icon_Name
      (Self : not null access Gtk_Stack_Page_Record) return UTF8_String;
   --  Returns the icon name of the page.
   --  @return The value of the [propertyGtk.StackPage:icon-name] property

   procedure Set_Icon_Name
      (Self    : not null access Gtk_Stack_Page_Record;
       Setting : UTF8_String);
   --  Sets the icon name of the page.
   --  @param Setting the new value to set

   function Get_Name
      (Self : not null access Gtk_Stack_Page_Record) return UTF8_String;
   --  Returns the name of the page.
   --  @return The value of the [propertyGtk.StackPage:name] property

   procedure Set_Name
      (Self    : not null access Gtk_Stack_Page_Record;
       Setting : UTF8_String);
   --  Sets the name of the page.
   --  @param Setting the new value to set

   function Get_Needs_Attention
      (Self : not null access Gtk_Stack_Page_Record) return Boolean;
   --  Returns whether the page is marked as "needs attention".
   --  @return The value of the [propertyGtk.StackPage:needs-attention]
   --  property.

   procedure Set_Needs_Attention
      (Self    : not null access Gtk_Stack_Page_Record;
       Setting : Boolean);
   --  Sets whether the page is marked as "needs attention".
   --  @param Setting the new value to set

   function Get_Title
      (Self : not null access Gtk_Stack_Page_Record) return UTF8_String;
   --  Gets the page title.
   --  @return The value of the [propertyGtk.StackPage:title] property

   procedure Set_Title
      (Self    : not null access Gtk_Stack_Page_Record;
       Setting : UTF8_String);
   --  Sets the page title.
   --  @param Setting the new value to set

   function Get_Use_Underline
      (Self : not null access Gtk_Stack_Page_Record) return Boolean;
   --  Gets whether underlines in the page title indicate mnemonics.
   --  @return The value of the [propertyGtk.StackPage:use-underline] property

   procedure Set_Use_Underline
      (Self    : not null access Gtk_Stack_Page_Record;
       Setting : Boolean);
   --  Sets whether underlines in the page title indicate mnemonics.
   --  @param Setting the new value to set

   function Get_Visible
      (Self : not null access Gtk_Stack_Page_Record) return Boolean;
   --  Returns whether Page is visible in its `GtkStack`.
   --  This is independent from the [propertyGtk.Widget:visible] property of
   --  its widget.
   --  @return True if Page is visible

   procedure Set_Visible
      (Self    : not null access Gtk_Stack_Page_Record;
       Visible : Boolean);
   --  Sets whether Page is visible in its `GtkStack`.
   --  @param Visible The new property value

   ---------------------------------------------
   -- Inherited subprograms (from interfaces) --
   ---------------------------------------------

   procedure Announce
      (Self     : not null access Gtk_Stack_Page_Record;
       Message  : UTF8_String;
       Priority : Gtk.Accessible.Gtk_Accessible_Announcement_Priority);

   function Get_Accessible_Id
      (Self : not null access Gtk_Stack_Page_Record) return UTF8_String;

   function Get_Accessible_Parent
      (Self : not null access Gtk_Stack_Page_Record)
       return Gtk.Accessible.Gtk_Accessible;

   procedure Set_Accessible_Parent
      (Self         : not null access Gtk_Stack_Page_Record;
       Parent       : Gtk.Accessible.Gtk_Accessible;
       Next_Sibling : Gtk.Accessible.Gtk_Accessible);

   function Get_Accessible_Role
      (Self : not null access Gtk_Stack_Page_Record)
       return Gtk.Accessible.Gtk_Accessible_Role;

   function Get_At_Context
      (Self : not null access Gtk_Stack_Page_Record)
       return Gtk.Atcontext.Gtk_Atcontext;

   function Get_Bounds
      (Self   : not null access Gtk_Stack_Page_Record;
       X      : out Glib.Gint;
       Y      : out Glib.Gint;
       Width  : out Glib.Gint;
       Height : out Glib.Gint) return Boolean;

   function Get_First_Accessible_Child
      (Self : not null access Gtk_Stack_Page_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Next_Accessible_Sibling
      (Self : not null access Gtk_Stack_Page_Record)
       return Gtk.Accessible.Gtk_Accessible;

   function Get_Platform_State
      (Self  : not null access Gtk_Stack_Page_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State) return Boolean;

   procedure Reset_Property
      (Self     : not null access Gtk_Stack_Page_Record;
       Property : Gtk.Accessible.Gtk_Accessible_Property);

   procedure Reset_Relation
      (Self     : not null access Gtk_Stack_Page_Record;
       Relation : Gtk.Accessible.Gtk_Accessible_Relation);

   procedure Reset_State
      (Self  : not null access Gtk_Stack_Page_Record;
       State : Gtk.Accessible.Gtk_Accessible_State);

   procedure Update_Next_Accessible_Sibling
      (Self        : not null access Gtk_Stack_Page_Record;
       New_Sibling : Gtk.Accessible.Gtk_Accessible);

   procedure Update_Platform_State
      (Self  : not null access Gtk_Stack_Page_Record;
       State : Gtk.Accessible.Gtk_Accessible_Platform_State);

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child that this page is for.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The icon name of the child page.

   Name_Property : constant Glib.Properties.Property_String;
   --  The name of the child page.

   Needs_Attention_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the page requires the user attention.
   --
   --  This is used by the [classGtk.StackSwitcher] to change the appearance
   --  of the corresponding button when a page needs attention and it is not
   --  the current one.

   Title_Property : constant Glib.Properties.Property_String;
   --  The title of the child page.

   Use_Underline_Property : constant Glib.Properties.Property_Boolean;
   --  If set, an underline in the title indicates a mnemonic.

   Visible_Property : constant Glib.Properties.Property_Boolean;
   --  Whether this page is visible.

   ----------------
   -- Interfaces --
   ----------------
   --  This class implements several interfaces. See Glib.Types
   --
   --  - "Gtk.Accessible"

   package Implements_Gtk_Accessible is new Glib.Types.Implements
     (Gtk.Accessible.Gtk_Accessible, Gtk_Stack_Page_Record, Gtk_Stack_Page);
   function "+"
     (Widget : access Gtk_Stack_Page_Record'Class)
   return Gtk.Accessible.Gtk_Accessible
   renames Implements_Gtk_Accessible.To_Interface;
   function "-"
     (Interf : Gtk.Accessible.Gtk_Accessible)
   return Gtk_Stack_Page
   renames Implements_Gtk_Accessible.To_Object;

private
   Visible_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("visible");
   Use_Underline_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("use-underline");
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Needs_Attention_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("needs-attention");
   Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("name");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Stack_Page;
