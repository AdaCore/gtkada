-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
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

with Gdk.Event;
with Gdk.Rectangle;
with Gdk.Window;
with Gtk;
with Gtk.Cell_Editable;
with Gtk.Object;
with Gtk.Widget;
with Glib.Glist;

with Unchecked_Conversion;

package Gtk.Cell_Renderer is

   type Gtk_Cell_Renderer_Record is
     new Gtk.Object.Gtk_Object_Record with private;
   type Gtk_Cell_Renderer is access all Gtk_Cell_Renderer_Record'Class;

   function Convert is new Unchecked_Conversion
     (Gtk_Cell_Renderer, System.Address);
   function Convert is new Unchecked_Conversion
     (System.Address, Gtk_Cell_Renderer);
   package Cell_Renderer_List is
      new Glib.Glist.Generic_List (Gtk_Cell_Renderer);

   type Gtk_Cell_Renderer_State is mod 2 ** 32;
   Cell_Renderer_Selected    : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Prelit      : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Insensitive : constant Gtk_Cell_Renderer_State;
   Cell_Renderer_Sorted      : constant Gtk_Cell_Renderer_State;

   type Cell_Renderer_Mode is
     (Cell_Renderer_Mode_Inert,
      Cell_Renderer_Mode_Activatable,
      Cell_Renderer_Mode_Editable);

   procedure Render
     (Cell            : access Gtk_Cell_Renderer_Record;
      Window          : Gdk.Window.Gdk_Window;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Expose_Area     : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State);
   --  Invokes the virtual render function of the Gtk_Cell_Renderer. The three
   --  passed-in rectangles are areas of Window. Most renderers will draw
   --  within Cell_Area; the Xalign, Yalign, Xpad, and Ypad fields of the
   --  GtkCellRenderer should be honored with respect to Cell_Area.
   --  Background_Area includes the blank space around the cell, and also the
   --  area containing the tree expander; so the Background_Area rectangles
   --  for all cells tile to cover the entire Window.  Expose_Area is a clip
   --  rectangle.

   function Activate
     (Cell            : access Gtk_Cell_Renderer_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State)
      return Boolean;
   --  Passes an activate event to the cell renderer for possible processing.
   --  Some cell renderers may use events;
   --  for example, Gtk_Cell_Renderer_Toggle toggles when it gets a
   --  mouse click.

   function Start_Editing
     (Cell            : access Gtk_Cell_Renderer_Record;
      Event           : Gdk.Event.Gdk_Event;
      Widget          : access Gtk.Widget.Gtk_Widget_Record'Class;
      Path            : String;
      Background_Area : Gdk.Rectangle.Gdk_Rectangle;
      Cell_Area       : Gdk.Rectangle.Gdk_Rectangle;
      Flags           : Gtk_Cell_Renderer_State)
      return Gtk.Cell_Editable.Gtk_Cell_Editable;
   --  Passes an activate event to the cell renderer for possible processing.
   --  Cell: a Gtk_Cell_Renderer
   --  Event: a Gdk_Event
   --  Widget: widget that received the event
   --  Path: widget-dependent string representation of the event location;
   --  e.g. for Gtk_Tree_View, a string representation of Gtk_Tree_Path
   --  Background_Area: background area as passed to Render
   --  Cell_Area: cell area as passed to Render

   procedure Set_Fixed_Size
     (Cell   : access Gtk_Cell_Renderer_Record;
      Width  : Gint;
      Height : Gint);
   --  Sets the renderer size to be explicit, independent of the
   --  properties set.

   procedure Get_Fixed_Size
     (Cell   : access Gtk_Cell_Renderer_Record;
      Width  : out Gint;
      Height : out Gint);
   --  Fills in Width and Height with the appropriate size of Cell.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "get_size"
   --    procedure Handler (Widget : access Gtk_Cell_Renderer_Record'Class;
   --       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
   --       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       X_Offset : out Gint;
   --       Y_Offset : out Gint;
   --       Width : out Gint;
   --       Height : out Gint);
   --
   --  - "render"
   --    procedure Handler (Widget : access Gtk_Cell_Renderer_Record'Class;
   --       Window : Gdk.Window.Gdk_Window;
   --       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
   --       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Expose_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Flags : Gtk_Cell_Renderer_State);
   --
   --  - "activate"
   --    function Handler (Widget : access Gtk_Cell_Renderer_Record'Class;
   --       Event : Gdk.Event.Gdk_Event;
   --       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
   --       Path : String;
   --       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Flags : Gtk_Cell_Renderer_State)
   --       return Gboolean;
   --
   --  - "start_editing"
   --    function Handler (Widget : access Gtk_Cell_Renderer_Record'Class;
   --       Event : Gdk.Event.Gdk_Event;
   --       Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
   --       Path : String;
   --       Background_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Cell_Area : Gdk.Rectangle.Gdk_Rectangle;
   --       Flags : Gtk_Cell_Renderer_State)
   --       return Cell_Editable *;
   --
   --  </signals>

private
   type Gtk_Cell_Renderer_Record is
     new Gtk.Object.Gtk_Object_Record with null record;

   Cell_Renderer_Selected    : constant Gtk_Cell_Renderer_State := 2 ** 0;
   Cell_Renderer_Prelit      : constant Gtk_Cell_Renderer_State := 2 ** 1;
   Cell_Renderer_Insensitive : constant Gtk_Cell_Renderer_State := 2 ** 2;
   Cell_Renderer_Sorted      : constant Gtk_Cell_Renderer_State := 2 ** 3;

end Gtk.Cell_Renderer;
