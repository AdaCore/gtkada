-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                     Copyright (C) 1998-1999                       --
--        Emmanuel Briot, Joel Brobecker and Arnaud Charlet          --
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

with System;
with Gdk; use Gdk;
with Gtk.Container; use Gtk.Container;
with Gtk.Util; use Gtk.Util;

package body Gtk.Drawing_Area is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Drawing_Area : out Gtk_Drawing_Area) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_drawing_area_new");
   begin
      Set_Object (Drawing_Area, Internal);
   end Gtk_New;

   ----------
   -- Size --
   ----------

   procedure Size
     (Darea  : in Gtk_Drawing_Area;
      Width  : in Gint;
      Height : in Gint)
   is
      procedure Internal (Darea  : System.Address;
                          Width  : Gint;
                          Height : Gint);
      pragma Import (C, Internal, "gtk_drawing_area_size");
   begin
      Internal (Get_Object (Darea), Width, Height);
   end Size;

   --------------
   -- Generate --
   --------------

   procedure Generate (Drawing_Area : in Gtk_Drawing_Area;
                       N            : in Node_Ptr;
                       File         : in File_Type) is
      use Widget;
   begin
      Gen_New (N, "Drawing_Area", File => File);
      Generate (Gtk_Widget (Drawing_Area), N, File);
      Gen_Call_Child (N, null, "Container", "Add", File => File);
   end Generate;

   procedure Generate (Drawing_Area : in out Gtk_Drawing_Area;
                       N            : in Node_Ptr) is
      use Widget;
   begin
      if not N.Specific_Data.Created then
         Gtk_New (Drawing_Area);
         Set_Object (Get_Field (N, "name"), Drawing_Area'Unchecked_Access);
         N.Specific_Data.Created := True;
      end if;

      Generate (Gtk_Widget (Drawing_Area), N);
      Container.Add
        (Gtk_Container (Get_Object (Get_Field (N.Parent, "name")).all),
         Drawing_Area);
   end Generate;

end Gtk.Drawing_Area;
