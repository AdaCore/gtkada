-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2006 AdaCore                    --
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
--  This widget is an adapter: it can contain any child, and will make it
--  scrollable. Its use is not necessary inside a Gtk_Scrolled_Window, which
--  automatically uses a Gtk_Viewport when necessary.
--  </description>
--  <c_version>2.8.17</c_version>

with Glib.Properties;
with Glib;
with Gdk;
with Gtk.Adjustment;
with Gtk.Bin;
with Gtk.Enums; use Gtk.Enums;

package Gtk.Viewport is

   type Gtk_Viewport_Record is new Gtk.Bin.Gtk_Bin_Record with private;
   type Gtk_Viewport is access all Gtk_Viewport_Record'Class;

   procedure Gtk_New
     (Viewport    : out Gtk_Viewport;
      Hadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment;
      Vadjustment : Adjustment.Gtk_Adjustment := Adjustment.Null_Adjustment);
   procedure Initialize
     (Viewport    : access Gtk_Viewport_Record'Class;
      Hadjustment : Gtk.Adjustment.Gtk_Adjustment;
      Vadjustment : Gtk.Adjustment.Gtk_Adjustment);
   --  Create or initialize a new viewport

   function Get_Type return Glib.GType;
   --  Return the internal value associated with a Gtk_Viewport.

   function Get_Bin_Window
     (Widget : access Gtk_Viewport_Record) return Gdk.Gdk_Window;
   --  Return the window associated with the viewport.
   --  You should use this one rather than Gtk.Widget.Get_Window.

   procedure Set_Hadjustment
     (Viewport   : access Gtk_Viewport_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Hadjustment
     (Viewport : access Gtk_Viewport_Record) return Adjustment.Gtk_Adjustment;
   --  Sets or gets the Gtk_Adjustment used for horizontal scrolling

   procedure Set_Vadjustment
     (Viewport   : access Gtk_Viewport_Record;
      Adjustment : Gtk.Adjustment.Gtk_Adjustment);
   function Get_Vadjustment
     (Viewport : access Gtk_Viewport_Record) return Adjustment.Gtk_Adjustment;
   --  Sets or gets the Gtk_Adjustment used for vertical scrolling

   procedure Set_Shadow_Type
     (Viewport : access Gtk_Viewport_Record;
      The_Type : Gtk_Shadow_Type);
   function Get_Shadow_Type
     (Viewport : access Gtk_Viewport_Record) return Gtk_Shadow_Type;
   --  Sets or gets the visual rendering of the viewport

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "set_scroll_adjustments"
   --    procedure Handler
   --      (Viewport   : access Gtk_Viewport_Record'Class;
   --       Hadj, Vadj : access Gtk_Adjustment_Record'Class);
   --    You should emit this signal to request a change of adjustments for the
   --    viewport. Seldom used, it is simpler to use Set_Vadjusment and
   --    Set_Hadjustment.
   --
   --  </signals>

   Signal_Set_Scroll_Adjustments : constant String := "set_scroll_adjustments";

   ----------------
   -- Properties --
   ----------------

   --  <properties>
   --  The following properties are defined for this widget. See
   --  Glib.Properties for more information on properties.
   --
   --  - Name:  Hadjustment_Property
   --    Type:  Gtk_Adjustment_Record'Class
   --    Flags: read-write
   --    Descr: The Gtk_Adjustment that determines the values of the horizontal
   --           position for this viewport
   --    See also:  Set_Hadjustment and Get_Hadjustment
   --
   --  - Name:  Vadjustment_Property
   --    Type:  Gtk_Adjustment_Record'Class
   --    Flags: read-write
   --    Descr: The Gtk_Adjustment that determines the values of the vertical
   --           position for this viewport
   --    See also:  Set_Vadjustment and Get_Vadjustment
   --
   --  - Name:  Shadow_Type_Property
   --    Type:  Gtk_Shadow_Type
   --    Flags: read-write
   --    Descr: Determines how the shadowed box around the viewport is drawn.
   --    See also:  Set_Shadow_Type
   --
   --  </properties>

   Hadjustment_Property : constant Glib.Properties.Property_Object;
   Vadjustment_Property : constant Glib.Properties.Property_Object;
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type;

private
   type Gtk_Viewport_Record is new Gtk.Bin.Gtk_Bin_Record with null record;

   Hadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("hadjustment");
   Vadjustment_Property : constant Glib.Properties.Property_Object :=
     Glib.Properties.Build ("vadjustment");
   Shadow_Type_Property : constant Gtk.Enums.Property_Gtk_Shadow_Type :=
     Gtk.Enums.Build ("shadow_type");

   pragma Import (C, Get_Type, "gtk_viewport_get_type");
end Gtk.Viewport;
