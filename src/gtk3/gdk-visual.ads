------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 1998-2022, AdaCore                     --
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

--  <c_version>1.3.6</c_version>
--  <group>Gdk, the low-level API</group>

with Glib; use Glib;
with Glib.Glist;
pragma Elaborate_All (Glib.Glist);

with System;
with Ada.Unchecked_Conversion;

package Gdk.Visual is

   subtype Gdk_Visual is Gdk.Gdk_Visual;
   Null_Visual : constant Gdk_Visual;
   --  This type is not private because we need the full declaration
   --  to instanciate Glib.Glist.Generic_List with it.

   type Gdk_Visual_Type is
     (Visual_Static_Gray,
      Visual_Grayscale,
      Visual_Static_Color,
      Visual_Pseudo_Color,
      Visual_True_Color,
      Visual_Direct_Color);
   pragma Convention (C, Gdk_Visual_Type);

   type Gdk_Visual_Type_Array is array (Natural range <>) of Gdk_Visual_Type;

   function Get_Type return Glib.GType;
   --  Return the internal value associated with Gdk_Visual.

   function Get_Best_Depth return Gint;

   function Get_Best_Type return Gdk_Visual_Type;

   function Get_System return Gdk_Visual;

   function Get_Best return Gdk_Visual;

   procedure Get_Best (Visual : out Gdk_Visual);

   function Get_Best (Depth  : Gint) return Gdk_Visual;

   function Get_Best (Visual_Type : Gdk_Visual_Type) return Gdk_Visual;

   function Get_Best
     (Depth       : Gint;
      Visual_Type : Gdk_Visual_Type) return Gdk_Visual;

   function Query_Depths return Gint_Array;

   function Query_Visual_Types return Gdk_Visual_Type_Array;

   function Convert is
     new Ada.Unchecked_Conversion (Gdk_Visual, System.Address);
   function Convert is
     new Ada.Unchecked_Conversion (System.Address, Gdk_Visual);

   package Gdk_Visual_List is new Glib.Glist.Generic_List (Gdk_Visual);

   function List_Visuals return Gdk_Visual_List.Glist;

private
   Null_Visual : constant Gdk_Visual := null;
   pragma Import (C, Get_Type, "gdk_visual_get_type");
   pragma Import (C, Get_Best_Depth, "gdk_visual_get_best_depth");
   pragma Import (C, Get_Best_Type, "gdk_visual_get_best_type");
   pragma Import (C, Get_System, "gdk_visual_get_system");

end Gdk.Visual;
