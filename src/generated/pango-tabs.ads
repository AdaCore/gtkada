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
--  A Pango.Tabs.Pango_Tab_Array struct contains an array of tab stops. Each
--  tab stop has an alignment and a position.
--
--  </description>

pragma Warnings (Off, "*is already use-visible*");
with Glib;                    use Glib;
with Glib.Generic_Properties; use Glib.Generic_Properties;

package Pango.Tabs is

   type Pango_Tab_Array is new Glib.C_Boxed with null record;
   Null_Pango_Tab_Array : constant Pango_Tab_Array;

   function From_Object (Object : System.Address) return Pango_Tab_Array;
   function From_Object_Free (B : access Pango_Tab_Array'Class) return Pango_Tab_Array;
   pragma Inline (From_Object_Free, From_Object);

   type Pango_Tab_Align is (
      Pango_Tab_Left);
   pragma Convention (C, Pango_Tab_Align);
   --  A Pango.Tabs.Pango_Tab_Align specifies where a tab stop appears
   --  relative to the text.

   ----------------------------
   -- Enumeration Properties --
   ----------------------------

   package Pango_Tab_Align_Properties is
      new Generic_Internal_Discrete_Property (Pango_Tab_Align);
   type Property_Pango_Tab_Align is new Pango_Tab_Align_Properties.Property;

   ------------------
   -- Constructors --
   ------------------

   procedure Gdk_New
      (Self                : out Pango_Tab_Array;
       Initial_Size        : Glib.Gint;
       Positions_In_Pixels : Boolean);
   --  Creates an array of Initial_Size tab stops. Tab stops are specified in
   --  pixel units if Positions_In_Pixels is True, otherwise in Pango units.
   --  All stops are initially at position 0.
   --  "initial_size": Initial number of tab stops to allocate, can be 0
   --  "positions_in_pixels": whether positions are in pixel units

   function Pango_Tab_Array_New
      (Initial_Size        : Glib.Gint;
       Positions_In_Pixels : Boolean) return Pango_Tab_Array;
   --  Creates an array of Initial_Size tab stops. Tab stops are specified in
   --  pixel units if Positions_In_Pixels is True, otherwise in Pango units.
   --  All stops are initially at position 0.
   --  "initial_size": Initial number of tab stops to allocate, can be 0
   --  "positions_in_pixels": whether positions are in pixel units

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "pango_tab_array_get_type");

   -------------
   -- Methods --
   -------------

   function Copy (Self : Pango_Tab_Array) return Pango_Tab_Array;
   --  Copies a Pango.Tabs.Pango_Tab_Array

   procedure Free (Self : Pango_Tab_Array);
   --  Frees a tab array and associated resources.

   function Get_Positions_In_Pixels (Self : Pango_Tab_Array) return Boolean;
   --  Returns True if the tab positions are in pixels, False if they are in
   --  Pango units.

   function Get_Size (Self : Pango_Tab_Array) return Glib.Gint;
   --  Gets the number of tab stops in Tab_Array.

   procedure Get_Tab
      (Self      : Pango_Tab_Array;
       Tab_Index : Glib.Gint;
       Alignment : out Pango_Tab_Align;
       Location  : out Glib.Gint);
   --  Gets the alignment and position of a tab stop.
   --  "tab_index": tab stop index
   --  "alignment": location to store alignment, or null
   --  "location": location to store tab position, or null

   procedure Set_Tab
      (Self      : Pango_Tab_Array;
       Tab_Index : Glib.Gint;
       Alignment : Pango_Tab_Align;
       Location  : Glib.Gint);
   --  Sets the alignment and location of a tab stop. Alignment must always be
   --  PANGO_TAB_LEFT in the current implementation.
   --  "tab_index": the index of a tab stop
   --  "alignment": tab alignment
   --  "location": tab location in Pango units

   procedure Resize (Self : Pango_Tab_Array; New_Size : Glib.Gint);
   --  Resizes a tab array. You must subsequently initialize any tabs that
   --  were added as a result of growing the array.
   --  "new_size": new size of the array

private

   Null_Pango_Tab_Array : constant Pango_Tab_Array := (Glib.C_Boxed with null record);

end Pango.Tabs;
