------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package implements the notion of tab stops, to be used in the
--  context of a pango.layout or its higher level interface Gtk.Text_View.
--
--  This package allows you to define precise location (in pixels) in the
--  layout on which the <tab> character in the text will be aligned. This
--  allows you to easily align columns of text even when using a proportional
--  font for instance. This is similar to the standard interface that most
--  word processing tools provide through tab stops in their ruler bars.

--  <group>Pango, font handling</group>

with Glib;

package Pango.Tabs is

   type Pango_Tab_Array is limited private;
   Null_Tab_Array : constant Pango_Tab_Array;
   --  This type contains an array of tab stops. Each such stop has an
   --  alignment and a position. Several methods are provided to create such
   --  an array, either by spacing out the stops regularly, or by positioning
   --  them by yourself.

   type Pango_Tab_Align is (Pango_Tab_Left);
   pragma Convention (C, Pango_Tab_Align);
   --  This structure specifies where a tab stop appears relative to the text.

   procedure Pango_New
     (Tab_Array           : out Pango_Tab_Array;
      Initial_Size        : Glib.Gint;
      Positions_In_Pixels : Boolean := True);
   --  Create an array of Initial_Size tab stops. The position of these stops
   --  will be specified in pixel units if Position_In_Pixels is True, or in
   --  Pango units otherwise (see Pango.Enums.Pango_Scale).
   --  All stops are initially at position 0.
   --  It is legal to create an array of size 0.

   function Get_Type return Glib.GType;
   --  Return the internal type associated with a Pango_Tab_Array

   procedure Copy
     (Src    : Pango_Tab_Array;
      Target : out Pango_Tab_Array);
   --  Return a newly allocated copy of Src.

   procedure Free (Tab_Array : Pango_Tab_Array);
   --  Free the memory occupied by Tab_Array

   function Get_Size (Tab_Array : Pango_Tab_Array) return Glib.Gint;
   --  Return the number of tab stops in Tab_Array.

   procedure Resize
     (Tab_Array : Pango_Tab_Array;
      New_Size  : Glib.Gint);
   --  Resize Tab_Array. You must then initialize any stop that was added as a
   --  result of growing the array.

   procedure Set_Tab
     (Tab_Array : Pango_Tab_Array;
      Tab_Index : Glib.Gint;
      Alignment : Pango_Tab_Align := Pango_Tab_Left;
      Location  : Glib.Gint);
   --  Set the alignment and location of a tab stop.
   --  Location is either in pixel units or in pango units, depending on how
   --  Tab_Array was created.
   --  Tab_Index starts at 0.

   procedure Get_Tab
     (Tab_Array : Pango_Tab_Array;
      Tab_Index : Glib.Gint;
      Alignment : out Pango_Tab_Align;
      Location  : out Glib.Gint);
   --  Return the alignment and location of a tab stop.
   --  Tab_Index starts at 0.

   function Get_Positions_In_Pixels
     (Tab_Array : Pango_Tab_Array) return Boolean;
   --  Whether the position of tab stops is in pixel units, or in pango units.

private
   type Pango_Tab_Array is new Glib.C_Proxy;

   Null_Tab_Array : constant Pango_Tab_Array := null;

   pragma Import (C, Get_Type, "pango_tab_array_get_type");
   pragma Import (C, Free,     "pango_tab_array_free");
   pragma Import (C, Get_Size, "pango_tab_array_get_size");
   pragma Import (C, Resize,   "pango_tab_array_resize");
   pragma Import (C, Set_Tab,  "pango_tab_array_set_tab");
   pragma Import (C, Get_Tab,  "pango_tab_array_get_tab");
end Pango.Tabs;

--  missing:
--  pango_tab_array_new_with_positions
--  pango_tab_array_get_tabs
