-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

--  <description>
--  A special kind of data set that stores three-dimensional data.
--  </description>
--  <c_version>gtk+extra 0.99.14<c_version>


with Gdk.Color;
with Gtk;
with Gtk.Extra.Plot_Data;    use Gtk.Extra.Plot_Data;
with Gtk.Extra.Plot;

package Gtk.Extra.Plot_Surface is

   type Gtk_Plot_Surface_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with private;
   type Gtk_Plot_Surface is access all Gtk_Plot_Surface_Record'Class;

   procedure Gtk_New (Widget : out Gtk_Plot_Surface;
                      Func   : Gtk.Extra.Plot.Plot3D_Function := null);
   --  Create a new surface.
   --  If Func is null, you have to explicitely specify the set of points
   --  found in the data set. Otherwise, the points will be generated
   --  automatically from Func.

   procedure Initialize (Widget : access Gtk_Plot_Surface_Record'Class;
                         Func   : Gtk.Extra.Plot.Plot3D_Function := null);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   function Get_Type return Gtk.Gtk_Type;
   --  Return the internal value associated with this widget.

   --  <doc_ignore>
   --  ??? These duplicate exactly the ones found in Gtk.Extra.Plot_Data
--     function Get_Dx (Data : access Gtk_Plot_Surface_Record) return Gdouble;
--     function Get_Dy (Data : access Gtk_Plot_Surface_Record) return Gdouble;
--     function Get_Dz (Data : access Gtk_Plot_Surface_Record) return Gdouble;
--     procedure Set_Dx
--       (Data : access Gtk_Plot_Surface_Record; Dx : Gdouble_Array_Access);
--     procedure Set_Dy
--       (Data : access Gtk_Plot_Surface_Record; Dy : Gdouble_Array_Access);
--     procedure Set_Dz
--       (Data : access Gtk_Plot_Surface_Record; Dz : Gdouble_Array_Access);
   --  Return or set the coordinates for the points in the set.
   --  This is a direct access to the underlying C array, thus modifying this
   --  array's contents also modifies the graph.
   --  See the corresponding Set_* functions for a definition of the
   --  coordinates
   --  </doc_ignore>

   function Get_Grid_Visible
     (Data : access Gtk_Plot_Surface_Record) return Boolean;
   --  Return True if the grid is currently visible for this data set.

   procedure Set_Grid_Visible
     (Data : access Gtk_Plot_Surface_Record; Visible : Boolean);
   --  Choose whether the grid should be visible

   function Get_Mesh_Visible
     (Data : access Gtk_Plot_Surface_Record) return Boolean;
   --  Return True if the wireframe mesh should be drawn.

   procedure Set_Mesh_Visible
     (Data    : access Gtk_Plot_Surface_Record; Visible : Boolean);
   --  Indicate whether the wireframe mesh should be visible.

   function Get_Nx (Data : access Gtk_Plot_Surface_Record) return Gint;
   function Get_Ny (Data : access Gtk_Plot_Surface_Record) return Gint;
   --  Return the number of points along each coordinate, when drawing a
   --  data set based on a function.

   procedure Set_Nx (Data : access Gtk_Plot_Surface_Record; Nx : Gint);
   procedure Set_Ny (Data : access Gtk_Plot_Surface_Record; Ny : Gint);
   --  Set the number of points along each coordinate

   procedure Set_Points
     (Data : access Gtk_Plot_Surface_Record;
      X    : Gdouble_Array_Access;
      Y    : Gdouble_Array_Access;
      Z    : Gdouble_Array_Access;
      Dx   : Gdouble_Array_Access;
      Dy   : Gdouble_Array_Access;
      Dz   : Gdouble_Array_Access);
   --  Set all the values of the data set at once.

   procedure Get_Points
     (Data : access Gtk_Plot_Surface_Record;
      X    : out Points_Array;
      Y    : out Points_Array;
      Z    : out Points_Array;
      Dx   : out Points_Array;
      Dy   : out Points_Array;
      Dz   : out Points_Array);
   --  Return the values contained in the data set.

   function Get_X (Data : access Gtk_Plot_Surface_Record) return Points_Array;
   function Get_Y (Data : access Gtk_Plot_Surface_Record) return Points_Array;
   function Get_Z (Data : access Gtk_Plot_Surface_Record) return Points_Array;
   --  Return the values contained in the data set.

   --  <doc_ignore>
   --  ??? These duplicate exactly the ones found in Gtk.Extra.Plot_Data
--     procedure Set_X
--       (Data : access Gtk_Plot_Surface_Record; Y : Gdouble_Array_Access);
--     procedure Set_Y
--       (Data : access Gtk_Plot_Surface_Record; Y : Gdouble_Array_Access);
--     procedure Set_Z
--       (Data : access Gtk_Plot_Surface_Record; Z : Gdouble_Array_Access);
   --  Set the values contained in the dataset
   --  </doc_ignore>

   function Get_Xstep (Data : access Gtk_Plot_Surface_Record) return Gdouble;
   function Get_Ystep (Data : access Gtk_Plot_Surface_Record) return Gdouble;
   --  Return the step between two points along one of the axis

   procedure Set_Xstep (Data : access Gtk_Plot_Surface_Record; Step : Gdouble);
   procedure Set_Ystep (Data : access Gtk_Plot_Surface_Record; Step : Gdouble);
   --  Set the step between two points along one of the axis

   procedure Set_Color
     (Data  : access Gtk_Plot_Surface_Record; Color : Gdk.Color.Gdk_Color);
   --  Set the color to use for the surface

   procedure Set_Grid_Background
     (Data       : access Gtk_Plot_Surface_Record;
      Background : Gdk.Color.Gdk_Color);
   --  Set the background color to use for the grid

   procedure Set_Shadow
     (Data  : access Gtk_Plot_Surface_Record; Color : Gdk.Color.Gdk_Color);
   --  Set the color to use for the shadows.

   procedure Set_Grid_Foreground
     (Data       : access Gtk_Plot_Surface_Record;
      Foreground : Gdk.Color.Gdk_Color);
   --  Set the foreground color to use for the grid

   procedure Set_Transparent
     (Data : access Gtk_Plot_Surface_Record; Transparent : Boolean);
   --  Whether the plot is transparent. If Transparent is true, all background
   --  attributes are ignored

   procedure Build_Mesh (Data : access Gtk_Plot_Surface_Record);
   --  ???

   procedure Recalc_Nodes (Data : access Gtk_Plot_Surface_Record);
   --  ???

   ---------------------
   -- Lightning model --
   ---------------------

   procedure Set_Ambient
     (Data : access Gtk_Plot_Surface_Record; Ambient : Gdouble);
   --  Set the ambient

   procedure Set_Light
     (Data : access Gtk_Plot_Surface_Record; X, Y, Z : Gdouble);
   --  Set the orientation of the light vector

   procedure Use_Height_Gradient
     (Data : access Gtk_Plot_Surface_Record; Use_Gradient : Boolean);
   --  ???

   procedure Use_Amplitud
     (Data : access Gtk_Plot_Surface_Record; Amplitud : Boolean);

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --  </signals>

private
   type Gtk_Plot_Surface_Record is new Gtk.Extra.Plot_Data.Gtk_Plot_Data_Record
     with null record;
   pragma Import (C, Get_Type, "gtk_plot_surface_get_type");
end Gtk.Extra.Plot_Surface;
