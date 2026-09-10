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

--  An auxiliary object used by `GtkNotebook`.

pragma Warnings (Off, "*is already use-visible*");
with Glib;            use Glib;
with Glib.Object;     use Glib.Object;
with Glib.Properties; use Glib.Properties;
with Gtk.Widget;      use Gtk.Widget;

package Gtk.Notebook_Page is

   type Gtk_Notebook_Page_Record is new GObject_Record with null record;
   type Gtk_Notebook_Page is access all Gtk_Notebook_Page_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_notebook_page_get_type");

   -------------
   -- Methods --
   -------------

   function Get_Child
      (Self : not null access Gtk_Notebook_Page_Record)
       return Gtk.Widget.Gtk_Widget;
   --  Returns the notebook child to which Page belongs.
   --  @return the child to which Page belongs
   --  Return has transfer-ownership='none'

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Child_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The child for this page.

   Detachable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the tab is detachable.

   Menu_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The label widget displayed in the child's menu entry.

   Menu_Label_Property : constant Glib.Properties.Property_String;
   --  The text of the menu widget.

   Position_Property : constant Glib.Properties.Property_Int;
   --  The index of the child in the parent.

   Reorderable_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the tab is reorderable by user action.

   Tab_Property : constant Glib.Properties.Property_Object;
   --  Type: Gtk.Widget.Gtk_Widget
   --  The tab widget for this page.

   Tab_Expand_Property : constant Glib.Properties.Property_Boolean;
   --  Whether to expand the child's tab.

   Tab_Fill_Property : constant Glib.Properties.Property_Boolean;
   --  Whether the child's tab should fill the allocated area.

   Tab_Label_Property : constant Glib.Properties.Property_String;
   --  The text of the tab widget.

private
   Tab_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("tab-label");
   Tab_Fill_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tab-fill");
   Tab_Expand_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("tab-expand");
   Tab_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("tab");
   Reorderable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("reorderable");
   Position_Property : constant Glib.Properties.Property_Int :=
     Glib.Properties.Build ("position");
   Menu_Label_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("menu-label");
   Menu_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("menu");
   Detachable_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("detachable");
   Child_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("child");
end Gtk.Notebook_Page;
