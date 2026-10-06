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

--  `GtkAssistantPage` is an auxiliary object used by `GtkAssistant`.
--
--  <group>Windows</group>
--  <gtkada_demo>create_assistant.adb</gtkada_demo>
--  <see>Gtk.Assistant</see>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Gtk.Widget;              use Gtk.Widget;

package Gtk.Assistant_Page is

   pragma Obsolescent;
   --  This object will be removed in GTK 5

   type Gtk_Assistant_Page_Record is new GObject_Record with null record;
   type Gtk_Assistant_Page is access all Gtk_Assistant_Page_Record'Class;

   type Gtk_Assistant_Page_Type is (
      Content,
      Intro,
      Confirm,
      Summary,
      Progress,
      Custom);
   pragma Convention (C, Gtk_Assistant_Page_Type);
   --  Determines the role of a page inside a `GtkAssistant`.
   --
   --  The role is used to handle buttons sensitivity and visibility.
   --
   --  Note that an assistant needs to end its page flow with a page of type
   --  Gtk.Assistant_Page.Confirm, Gtk.Assistant_Page.Summary or
   --  Gtk.Assistant_Page.Progress to be correct.
   --
   --  The Cancel button will only be shown if the page isn't "committed". See
   --  Gtk.Assistant.Commit for details.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Gtk_Assistant_Page_Type_Properties is
      new Generic_Internal_Discrete_Property (Gtk_Assistant_Page_Type);
   type Property_Gtk_Assistant_Page_Type is new Gtk_Assistant_Page_Type_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_assistant_page_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Assistant_Page_Record)
       return Gtk.Widget.Gtk_Widget;
   pragma Obsolescent (Get_Child);
   --  Returns the child to which Page belongs.
   --  Deprecated since 4.10, 1
   --  @return the child to which Page belongs. Has transfer-ownership='none'.

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child widget.

   Complete_Property : constant Glib.Properties.Property_Boolean;
   --  Whether all required fields are filled in.
   --
   --  GTK uses this information to control the sensitivity of the navigation
   --  buttons.

   Page_Type_Property : constant Gtk.Assistant_Page.Property_Gtk_Assistant_Page_Type;
   --  Type: Gtk_Assistant_Page_Type
   --  The type of the assistant page.

   Title_Property : constant Glib.Properties.Property_String;
   --  The title of the page.

private
   Title_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("title");
   Page_Type_Property : constant Gtk.Assistant_Page.Property_Gtk_Assistant_Page_Type :=
     Gtk.Assistant_Page.Build ("page-type");
   Complete_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("complete");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Assistant_Page;
