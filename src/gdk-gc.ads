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

--  <description>
--
--  A graphic context is a structure that describes all the attributes
--  used by the drawing functions in Gdk.
--  The colors, line styles, Fill styles and so on are defined through
--  this structure.
--
--  On X11 systems, this structure is stored directly on the XServer,
--  which speeds up the transfer of the drawing attributes a lot. Instead
--  of transfering all of them everytime you call one of the drawing
--  functions, you simply specify which GC you want to use.
--
--  Thus, it is recommended to create as many GCs as you need, instead
--  of creating a single one that is modified everytime you need to
--  modify one of the attributes.
--
--  </description>
--  <c_version>1.2.6</c_version>

with Glib; use Glib;
with Gdk.Color;
with Gdk.Font;
with Gdk.Rectangle;
with Gdk.Region;
with Gdk.Types;
with Gdk.Window;

package Gdk.GC is

   type Gdk_GC is new Gdk.C_Proxy;
   type Gdk_GC_Values is new Gdk.C_Proxy;
   Null_GC : constant Gdk_GC;
   Null_GC_Values : constant Gdk_GC_Values;

   ------------
   -- Gdk_GC --
   ------------

   procedure Gdk_New (GC     :    out Gdk_GC;
                      Window : in     Gdk.Window.Gdk_Window);
   --  Create a new graphic context.
   --  The window must have been realized first (so that it is associated
   --  with some ressources on the Xserver).
   --  The GC can then be used for any window that has the same root window,
   --  and same color depth as Window.
   --  See the manual page for XCreateGC on Unix systems for more information.

   procedure Gdk_New (GC          :    out Gdk_GC;
                      Window      : in     Gdk.Window.Gdk_Window;
                      Values      : in     Gdk_GC_Values;
                      Values_Mask : in     Types.Gdk_GC_Values_Mask);
   --  Create a new graphic context.
   --  It is directly created with the values set in Values, and whose
   --  associated field has been set in Values_Mask.
   --  This is faster than calling the simple Gdk_New function and each of
   --  other functions in this package, since each of them requires a call
   --  to the server.

   procedure Destroy (GC : in out Gdk_GC);
   --  Free the memory allocated on the server for the graphic context.
   --  Graphic contexts are never freed automatically by GtkAda, this is
   --  the user responsability to do so.

   procedure Ref (GC : in Gdk_GC);
   --  Increment the reference counting for the graphic context.
   --  You should usually not have to use it.

   procedure Unref (GC : in Gdk_GC);
   --  Decrement the reference counting for the graphic context.
   --  When this reaches 0, the graphic context is destroyed.

   procedure Get_Values (GC     : in Gdk_GC;
                         Values : in Gdk_GC_Values);
   --  Get the values set in the GC.
   --  This copies the values from the server to client, allowing faster
   --  modifications. Values can then be copied back to the server by
   --  creating a new graphic context with the function Gdk_New above.
   --  Values should have been allocated first with a call to Gdk_New.

   procedure Set_Foreground (GC    : in Gdk_GC;
                             Color : in Gdk.Color.Gdk_Color);
   --  Set the foreground color for the graphic context.
   --  This color is the one that is used by most drawing functions.

   procedure Set_Background (GC     : in Gdk_GC;
                             Color  : in Gdk.Color.Gdk_Color);
   --  Set the background color for the graphic context.

   procedure Set_Font (GC   : in Gdk_GC;
                       Font : in Gdk.Font.Gdk_Font);
   --  Set the font used by the graphic context.
   --  This font is used by the function Gdk.Drawable.Draw_Text.

   procedure Set_Function (GC   : in Gdk_GC;
                           Func : in Types.Gdk_Function);
   --  Set the function in the graphic context.
   --  This function specifies how the points are put on the screen, ie
   --  if GtkAda how GtkAda should mix the point already on the screen
   --  and the new point being put.
   --  Note that setting the function to Gdk_Xor is not the right way
   --  to do animation. You should instead save the background pixmap,
   --  put the image, and then restore the background.
   --
   --  In general, there are three basic steps to drawing: reading the source
   --  pixels, reading the destination pixels, and writing the destination
   --  pixels.  Some functions only perform the third step (Set and Clear),
   --  some do not need the middle step (Copy), whereas most require the three
   --  steps, and thus can be much slower.

   procedure Set_Fill (GC   : in Gdk_GC;
                       Fill : in Types.Gdk_Fill);
   --  Set the pattern used for filling the polygons.

   --  procedure Set_Tile
   --  procedure Set_Stipple
   --  procedure Set_Clip_Mask
   --
   --  Have been moved to the Gdk.GC.Pixmap child package for
   --  circular dependency reasons.

   procedure Set_Ts_Origin (GC   : in Gdk_GC;
                            X, Y : in Gint);
   --  Set the Tile and Stiple origin in the graphic context.

   procedure Set_Clip_Origin (GC   : in Gdk_GC;
                              X, Y : in Gint);
   --  Set the origin of the clip mask.
   --  See the functions Set_Clip_Rectangle, Set_Clip_Region and
   --  Gdk.Bitmap.Set_Clip_Mask for more explanation.

   procedure Set_Clip_Rectangle
     (GC        : in Gdk_GC;
      Rectangle : in Gdk.Rectangle.Gdk_Rectangle);
   --  Set the clip rectangle.
   --  Only the points that are drawn inside this rectangle will be displayed
   --  on the screen. Note that you might have to modify the Clip origin first
   --  with Set_Clip_Origin.

   procedure Set_Clip_Region (GC     : in Gdk_GC;
                              Region : in Gdk.Region.Gdk_Region);
   --  Define a clip region on the screen.
   --  This is just like Set_Clip_Rectangle, except that a region is a more
   --  complex region, that can be the intersection or union of multiple
   --  rectangles. Note that the Clip_Origin can have an influence on this
   --  function.

   procedure Set_Subwindow (GC   : in Gdk_GC;
                            Mode : in Types.Gdk_Subwindow_Mode);
   --  Set the subwindow mode for the graphic context.
   --  This specifies whether the drawing routines should be clipped to
   --  the specific window they are drawn into, or if they should extend
   --  to subwindows as well.

   procedure Set_Exposures (GC        : in Gdk_GC;
                            Exposures : in Boolean);
   --  Exposures indicates whether you want "expose" and "noexpose" events to
   --  be reported when calling Copy_Area and Copy_Plane with this GC.
   --  You should disable this if you don't need the event and want to optimize
   --  your application.

   procedure Set_Line_Attributes (GC         : in Gdk_GC;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style);
   --  Set the line attributes for this GC.
   --  Line_Width is the width of the line. If its value is 0, the line is as
   --  thin as possible, possibly even more so than if the width is 1. It is
   --  also faster to draw a line with width 0 than any other line width.
   --
   --  Line_Style specifies whether the line should be solid or dashed. If its
   --  value is Line_On_Off_Dash, the colors are alternatively the foreground
   --  color, and blank. If the value is Line_Double_Dash, the colors are
   --  alternatively the foreground and background colors.
   --
   --  Cap_Style specifies how the line should end, either flat or rounded.
   --
   --  Join_Style specifies how two consecutive lines drawn by Draw_Lines are
   --  connected.

   procedure Set_Dashes (Gc          : in Gdk_GC;
                         Dash_Offset : in Gint;
                         Dash_List   : in Guchar_Array);
   --  Specify the dash pattern when the line's style is anything but solid.
   --  The values in the array alternatively give the length (in pixels) of
   --  the plain dash, the empty dash, the second plain dash, ... None of
   --  these values can be 0. If there is an odd number of items in Dash_List,
   --  this is equivalent to giving the array concatenated with itself.
   --  Dash_Offset specifies the phase of the pattern to start with.

   procedure Copy (Dst_GC : in Gdk_GC;
                   Src_GC : in Gdk_GC);
   --  Copy a Src_GC to Dst_GC.


   ----------------------
   -- Gdk_Color_Values --
   ----------------------

   function Gdk_New return Gdk_GC_Values;
   --  Allocate a new Values structure on the client.
   --  Note that this function allocates a C structure, and thus needs to
   --  be freed with a call to Free below.

   procedure Free (Values : in Gdk_GC_Values);
   --  Free the C structure associated with Values.

   procedure Set_Foreground (Values : in Gdk_GC_Values;
                             Color  : in Gdk.Color.Gdk_Color);
   --  Same as Set_Foreground, but on the client side

   procedure Set_Background (Values : in Gdk_GC_Values;
                             Color  : in Gdk.Color.Gdk_Color);
   --  Same as Set_Background, but on the client side

   procedure Set_Font (Values : in Gdk_GC_Values;
                       Font   : in Gdk.Font.Gdk_Font);
   --  Same as Set_Font, but on the client side

   procedure Set_Function (Values : in Gdk_GC_Values;
                           Func   : in Types.Gdk_Function);
   --  Same as Set_Function, but on the client side

   procedure Set_Fill (Values : in Gdk_GC_Values;
                       Fill   : in Types.Gdk_Fill);
   --  Same as Set_Fill, but on the client side

   procedure Set_Ts_Origin (Values : in Gdk_GC_Values;
                            X, Y   : in Gint);
   --  Same as Set_Ts_Origin, but on the client side

   procedure Set_Clip_Origin (Values : in Gdk_GC_Values;
                              X, Y   : in Gint);
   --  Same as Set_Clip_Origin, but on the client side

   procedure Set_Subwindow (Values : in Gdk_GC_Values;
                            Mode   : in Types.Gdk_Subwindow_Mode);
   --  Same as Set_Subwindow, but on the client side

   procedure Set_Exposures (Values    : in Gdk_GC_Values;
                            Exposures : in Boolean);
   --  Same as Set_Exposures, but on the client side

   procedure Set_Line_Attributes (Values     : in Gdk_GC_Values;
                                  Line_Width : in Gint;
                                  Line_Style : in Types.Gdk_Line_Style;
                                  Cap_Style  : in Types.Gdk_Cap_Style;
                                  Join_Style : in Types.Gdk_Join_Style);
   --  Same as Set_Line_Attributes, but on the client side

private
   Null_GC : constant Gdk_GC := null;
   Null_GC_Values : constant Gdk_GC_Values := null;
   pragma Import (C, Copy, "gdk_gc_copy");
   pragma Import (C, Destroy, "gdk_gc_destroy");
   pragma Import (C, Free, "ada_gdk_gc_free_values");
   pragma Import (C, Get_Values, "gdk_gc_get_values");
   pragma Import (C, Ref, "gdk_gc_ref");
   pragma Import (C, Unref, "gdk_gc_unref");
end Gdk.GC;
