-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                Copyright (C) 2001-2002 ACT-Europe                 --
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

--  <c_version>1.3.11</c_version>

with Gtk;
with Gtk.Cell_Renderer;

package Gtk.Cell_Renderer_Text is

   type Gtk_Cell_Renderer_Text_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with private;
   type Gtk_Cell_Renderer_Text is
     access all Gtk_Cell_Renderer_Text_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Cell_Renderer_Text);

   procedure Initialize (Widget : access Gtk_Cell_Renderer_Text_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   procedure Set_Fixed_Height_From_Font
     (Renderer       : access Gtk_Cell_Renderer_Text_Record;
      Number_Of_Rows : Gint);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "edited"
   --    procedure Handler
   --     (Widget : access Gtk_Cell_Renderer_Text_Record'Class;
   --       Path : String;
   --       New_Text : String);
   --
   --    Called when the text has been edited interactively . Note that you
   --    also need to set the attribute "editable" for users to be able to
   --    interactively edit the cell. If you want to take into account the
   --    change, you need to change the value in the model appropriately, for
   --    instance through a call to Set_Value
   --  </signals>

   ----------------
   -- Properties --
   ----------------

   --  The following properties are defined for this cell_renderer :
   --
   --   Attribute             Type in Model             Mode
   --   =========             =============             ====
   --
   --   "text"                String                    Read / Write
   --   "markup"              String                    Write
   --   "attributes"          PangoAttrList             Read / Write
   --   "background"          String                    Write
   --   "foreground"          String                    Write
   --   "background-gdk"      Gdk_Color                 Read / Write
   --   "foreground-gdk"      Gdk_Color                 Read / Write
   --   "font"                String                    Read / Write
   --   "font-desc"           Pango_Font_Description    Read / Write
   --   "family"              String                    Read / Write
   --   "style"               PangoStyle                Read / Write
   --   "variant"             PangoVariant              Read / Write
   --   "weight"              Gint                      Read / Write
   --   "stretch"             PangoStretch              Read / Write
   --   "size"                Gint                      Read / Write
   --   "size-points"         Gdouble                   Read / Write
   --   "scale"               Gdouble                   Read / Write
   --   "editable"            Boolean                   Read / Write
   --   "strikethrough"       Boolean                   Read / Write
   --   "underline"           PangoUnderline            Read / Write
   --   "rise"                Gint                      Read / Write
   --   "background-set"      Boolean                   Read / Write
   --   "foreground-set"      Boolean                   Read / Write
   --   "family-set"          Boolean                   Read / Write
   --   "style-set"           Boolean                   Read / Write
   --   "variant-set"         Boolean                   Read / Write
   --   "weight-set"          Boolean                   Read / Write
   --   "stretch-set"         Boolean                   Read / Write
   --   "size-set"            Boolean                   Read / Write
   --   "scale-set"           Boolean                   Read / Write
   --   "editable-set"        Boolean                   Read / Write
   --   "strikethrough-set"   Boolean                   Read / Write
   --   "underline-set"       Boolean                   Read / Write
   --   "rise-set"            Boolean                   Read / Write

private
   type Gtk_Cell_Renderer_Text_Record is
     new Gtk.Cell_Renderer.Gtk_Cell_Renderer_Record with null record;

   pragma Import (C, Get_Type,  "gtk_cell_renderer_text_get_type");
end Gtk.Cell_Renderer_Text;
