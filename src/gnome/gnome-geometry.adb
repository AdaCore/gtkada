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

with Interfaces.C.Strings;

package body Gnome.Geometry is

   ---------------------
   -- Geometry_String --
   ---------------------

   function Geometry_String (Window : Gdk.Window.Gdk_Window) return String is
      function Internal
        (Window : Gdk.Window.Gdk_Window) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gnome_geometry_string");
   begin
      return Interfaces.C.Strings.Value (Internal (Window));
   end Geometry_String;

   --------------------
   -- Parse_Geometry --
   --------------------

   procedure Parse_Geometry
     (Geometry : String;
      Xpos     : out Gint;
      Ypos     : out Gint;
      Width    : out Gint;
      Height   : out Gint;
      Success  : out Boolean)
   is
      function Internal
        (Geometry : String;
         Xpos     : access Gint;
         Ypos     : access Gint;
         Width    : access Gint;
         Height   : access Gint) return Gint;
      pragma Import (C, Internal, "gnome_parse_geometry");

      X, Y, W, H : aliased Gint;

   begin
      Success := Boolean'Val (Internal
        (Geometry & ASCII.NUL,
         X'Unchecked_Access, Y'Unchecked_Access,
         W'Unchecked_Access, H'Unchecked_Access));
      Xpos   := X;
      Ypos   := Y;
      Width  := W;
      Height := H;
   end Parse_Geometry;

end Gnome.Geometry;
