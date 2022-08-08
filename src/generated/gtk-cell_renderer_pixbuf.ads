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
--  A Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf can be used to render
--  an image in a cell. It allows to render either a given
--  Gdk.Pixbuf.Gdk_Pixbuf (set via the
--  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf:pixbuf property) or a
--  named icon (set via the
--  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf:icon-name property).
--
--  To support the tree view,
--  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf also supports rendering
--  two alternative pixbufs, when the
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer:is-expander property is True. If the
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer:is-expanded property is True and the
--  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf:pixbuf-expander-open
--  property is set to a pixbuf, it renders that pixbuf, if the
--  Gtk.Cell_Renderer.Gtk_Cell_Renderer:is-expanded property is False and the
--  Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf:pixbuf-expander-closed
--  property is set to a pixbuf, it renders that one.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;              use Glib;
with Glib.Properties;   use Glib.Properties;
with Gtk.Cell_Renderer; use Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Pixbuf is

   type Gtk_Cell_Renderer_Pixbuf_Record is new Gtk_Cell_Renderer_Record with null record;
   type Gtk_Cell_Renderer_Pixbuf is access all Gtk_Cell_Renderer_Pixbuf_Record'Class;

   ------------------
   -- Constructors --
   ------------------

   procedure Gtk_New (Self : out Gtk_Cell_Renderer_Pixbuf);
   procedure Initialize
      (Self : not null access Gtk_Cell_Renderer_Pixbuf_Record'Class);
   --  Creates a new Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf. Adjust
   --  rendering parameters using object properties. Object properties can be
   --  set globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "pixbuf" property on the cell renderer to a pixbuf value in the model,
   --  thus rendering a different image in each row of the
   --  Gtk.Tree_View.Gtk_Tree_View.
   --  Initialize does nothing if the object was already created with another
   --  call to Initialize* or G_New.

   function Gtk_Cell_Renderer_Pixbuf_New return Gtk_Cell_Renderer_Pixbuf;
   --  Creates a new Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf. Adjust
   --  rendering parameters using object properties. Object properties can be
   --  set globally (with g_object_set). Also, with
   --  Gtk.Tree_View_Column.Gtk_Tree_View_Column, you can bind a property to a
   --  value in a Gtk.Tree_Model.Gtk_Tree_Model. For example, you can bind the
   --  "pixbuf" property on the cell renderer to a pixbuf value in the model,
   --  thus rendering a different image in each row of the
   --  Gtk.Tree_View.Gtk_Tree_View.

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gtk_cell_renderer_pixbuf_get_type");

   ----------------
   -- Properties --
   ----------------
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties)

   Follow_State_Property : constant Glib.Properties.Property_Boolean;
   --  Specifies whether the rendered pixbuf should be colorized according to
   --  the Gtk.Cell_Renderer.Gtk_Cell_Renderer_State.

   G_Icon_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Glib.G_Icon.G_Icon
   --  The GIcon representing the icon to display. If the icon theme is
   --  changed, the image will be updated automatically.

   Icon_Name_Property : constant Glib.Properties.Property_String;
   --  The name of the themed icon to display. This property only has an
   --  effect if not overridden by "stock_id" or "pixbuf" properties.

   Pixbuf_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf

   Pixbuf_Expander_Closed_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf

   Pixbuf_Expander_Open_Property : constant Glib.Properties.Property_Object;
   --  Type: Gdk.Pixbuf.Gdk_Pixbuf

   Stock_Detail_Property : constant Glib.Properties.Property_String;

   Stock_Id_Property : constant Glib.Properties.Property_String;

   Stock_Size_Property : constant Glib.Properties.Property_Uint;
   --  The Gtk.Enums.Gtk_Icon_Size value that specifies the size of the
   --  rendered icon.

   Surface_Property : constant Glib.Properties.Property_Boxed;
   --  Type: Cairo.Cairo_Surface

private
   Surface_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("surface");
   Stock_Size_Property : constant Glib.Properties.Property_Uint :=
     Glib.Properties.Build ("stock-size");
   Stock_Id_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-id");
   Stock_Detail_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("stock-detail");
   Pixbuf_Expander_Open_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf-expander-open");
   Pixbuf_Expander_Closed_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf-expander-closed");
   Pixbuf_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("pixbuf");
   Icon_Name_Property : constant Glib.Properties.Property_String :=
     Glib.Properties.Build ("icon-name");
   G_Icon_Property : constant Glib.Properties.Property_Boxed :=
     Glib.Properties.Build ("gicon");
   Follow_State_Property : constant Glib.Properties.Property_Boolean :=
     Glib.Properties.Build ("follow-state");
end Gtk.Cell_Renderer_Pixbuf;
