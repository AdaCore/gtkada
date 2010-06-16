-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

pragma Ada_2005;

with System;
with Interfaces.C.Strings;

with Glib; use Glib;

package Cairo.Pattern is

   function Create_Rgb
     (Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble)
      return  Cairo_Pattern;
   --  Red: Red component of the color
   --  Green: Green component of the color
   --  Blue: Blue component of the color
   --
   --  Creates a new Cairo_Pattern corresponding to an opaque color.  The
   --  color components are floating point numbers in the range 0 to 1.
   --  If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_Rgba
     (Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble;
      Alpha : Gdouble)
      return  Cairo_Pattern;
   --  Red: Red component of the color
   --  Green: Green component of the color
   --  Blue: Blue component of the color
   --  Alpha: Alpha component of the color
   --
   --  Creates a new Cairo_Pattern corresponding to a translucent color.
   --  The color components are floating point numbers in the range 0 to
   --  1.  If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_For_Surface
     (Surface : Cairo_Surface)
      return    Cairo_Pattern;
   --  Surface: the Surface
   --
   --  Create a new Cairo_Pattern for the given surface.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_Linear
     (X0   : Gdouble;
      Y0   : Gdouble;
      X1   : Gdouble;
      Y1   : Gdouble)
      return Cairo_Pattern;
   --  X0: x coordinate of the start point
   --  Y0: y coordinate of the start point
   --  X1: x coordinate of the end point
   --  Y1: y coordinate of the end point
   --
   --  Create a new linear gradient Cairo_Pattern along the line defined
   --  by (x0, y0) and (x1, y1).  Before using the gradient pattern, a
   --  number of color stops should be defined using
   --  Cairo.Pattern.Add_Color_Stop_Rgb or
   --  Cairo.Pattern.Add_Color_Stop_Rgba.
   --
   --  Note: The coordinates here are in pattern space. For a new pattern,
   --  pattern space is identical to user space, but the relationship
   --  between the spaces can be changed with Cairo.Pattern.Set_Matrix.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Create_Radial
     (Cx0     : Gdouble;
      Cy0     : Gdouble;
      Radius0 : Gdouble;
      Cx1     : Gdouble;
      Cy1     : Gdouble;
      Radius1 : Gdouble)
      return    Cairo_Pattern;
   --  Cx0: x coordinate for the center of the start circle
   --  Cy0: y coordinate for the center of the start circle
   --  Radius0: radius of the start circle
   --  Cx1: x coordinate for the center of the end circle
   --  Cy1: y coordinate for the center of the end circle
   --  Radius1: radius of the end circle
   --
   --  Creates a new radial gradient Cairo_Pattern between the two
   --  circles defined by (cx0, cy0, radius0) and (cx1, cy1, radius1).  Before
   --using the
   --  gradient pattern, a number of color stops should be defined using
   --  Cairo.Pattern.Add_Color_Stop_Rgb or
   --  Cairo.Pattern.Add_Color_Stop_Rgba.
   --
   --  Note: The coordinates here are in pattern space. For a new pattern,
   --  pattern space is identical to user space, but the relationship
   --  between the spaces can be changed with Cairo.Pattern.Set_Matrix.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Cairo.Pattern.Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Cairo.Pattern.Status.

   function Reference (Pattern : Cairo_Pattern) return Cairo_Pattern;
   --  Pattern: a Cairo_Pattern
   --
   --  Increases the reference count on pattern by one. This prevents
   --  pattern from being destroyed until a matching call to
   --  Cairo.Pattern.Destroy is made.
   --
   --  The number of references to a Cairo_Pattern can be get using
   --  Cairo.Pattern.Get_Reference_Count.
   --
   --  Return value: the referenced Cairo_Pattern.

   procedure Destroy (Pattern : Cairo_Pattern);
   --  Pattern: a Cairo_Pattern
   --
   --  Decreases the reference count on pattern by one. If the result is
   --  zero, then pattern and all associated resources are freed.  See
   --  Cairo.Pattern.Reference.

   function Get_Reference_Count (Pattern : Cairo_Pattern) return Guint;
   --  Pattern: a Cairo_Pattern
   --
   --  Returns the current reference count of pattern.
   --
   --  Return value: the current reference count of pattern.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Status (Pattern : Cairo_Pattern) return Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --
   --  Checks whether an error has previously occurred for this
   --  pattern.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, CAIRO_STATUS_NO_MEMORY, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH.

   function Get_User_Data
     (Pattern : Cairo_Pattern;
      Key     : access constant Cairo_User_Data_Key)
      return    System.Address;
   --  Pattern: a Cairo_Pattern
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to pattern using the
   --  specified key.  If no user data has been attached with the given
   --  key this function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4

   function Set_User_Data
     (Pattern   : Cairo_Pattern;
      Key       : access constant Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : access procedure (Arg1 : System.Address))
      return      Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the Cairo_Pattern
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to pattern.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   function Get_Type (Pattern : Cairo_Pattern) return Cairo_Pattern_Type;
   --  Pattern: a Cairo_Pattern
   --
   --  This function returns the type a pattern.
   --  See Cairo_Pattern_Type for available types.
   --
   --  Return value: The type of pattern.
   --
   --  Since: 1.2

   procedure Add_Color_Stop_Rgb
     (Pattern : Cairo_Pattern;
      Offset  : Gdouble;
      Red     : Gdouble;
      Green   : Gdouble;
      Blue    : Gdouble);
   --  Pattern: a Cairo_Pattern
   --  Offset: an Offset in the range [0.0 .. 1.0]
   --  Red: Red component of color
   --  Green: Green component of color
   --  Blue: Blue component of color
   --
   --  Adds an opaque color stop to a gradient pattern. The offset
   --  specifies the location along the gradient's control vector. For
   --  example, a linear gradient's control vector is from (x0,y0) to
   --  (x1,y1) while a radial gradient's control vector is from any point
   --  on the start circle to the corresponding point on the end circle.
   --
   --  The color is specified in the same way as in Cairo_Set_Source_Rgb.
   --
   --  If two (or more) stops are specified with identical offset values,
   --  they will be sorted according to the order in which the stops are
   --  added, (stops added earlier will compare less than stops added
   --  later). This can be useful for reliably making sharp color
   --  transitions instead of the typical blend.
   --
   --
   --  Note: If the pattern is not a gradient pattern, (eg. a linear or
   --  radial pattern), then the pattern will be put into an error status
   --  with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH.

   procedure Add_Color_Stop_Rgba
     (Pattern : Cairo_Pattern;
      Offset  : Gdouble;
      Red     : Gdouble;
      Green   : Gdouble;
      Blue    : Gdouble;
      Alpha   : Gdouble);
   --  Pattern: a Cairo_Pattern
   --  Offset: an Offset in the range [0.0 .. 1.0]
   --  Red: Red component of color
   --  Green: Green component of color
   --  Blue: Blue component of color
   --  Alpha: Alpha component of color
   --
   --  Adds a translucent color stop to a gradient pattern. The offset
   --  specifies the location along the gradient's control vector. For
   --  example, a linear gradient's control vector is from (x0,y0) to
   --  (x1,y1) while a radial gradient's control vector is from any point
   --  on the start circle to the corresponding point on the end circle.
   --
   --  The color is specified in the same way as in Cairo_Set_Source_Rgba.
   --
   --  If two (or more) stops are specified with identical offset values,
   --  they will be sorted according to the order in which the stops are
   --  added, (stops added earlier will compare less than stops added
   --  later). This can be useful for reliably making sharp color
   --  transitions instead of the typical blend.
   --
   --  Note: If the pattern is not a gradient pattern, (eg. a linear or
   --  radial pattern), then the pattern will be put into an error status
   --  with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH.

   procedure Set_Matrix
     (Pattern : Cairo_Pattern;
      Matrix  : access constant Cairo_Matrix);
   --  Pattern: a Cairo_Pattern
   --  Matrix: a Cairo_Matrix
   --
   --  Sets the pattern's transformation matrix to matrix. This matrix is
   --  a transformation from user space to pattern space.
   --
   --  When a pattern is first created it always has the identity matrix
   --  for its transformation matrix, which means that pattern space is
   --  initially identical to user space.
   --
   --  Important: Please note that the direction of this transformation
   --  matrix is from user space to pattern space. This means that if you
   --  imagine the flow from a pattern to user space (and on to device
   --  space), then coordinates in that flow will be transformed by the
   --  inverse of the pattern matrix.
   --
   --  For example, if you want to make a pattern appear twice as large as
   --  it does by default the correct code to use is:
   --
   --  <informalexample><programlisting>
   --  Cairo.Matrix.Init_Scale (&amp;matrix, 0.5, 0.5);
   --  Cairo.Pattern.Set_Matrix (pattern, &amp;matrix);
   --  </programlisting></informalexample>
   --
   --  Meanwhile, using values of 2.0 rather than 0.5 in the code above
   --  would cause the pattern to appear at half of its default size.
   --
   --  Also, please note the discussion of the user-space locking
   --  semantics of Cairo_Set_Source.

   procedure Get_Matrix
     (Pattern : Cairo_Pattern;
      Matrix  : access Cairo_Matrix);
   --  Pattern: a Cairo_Pattern
   --  Matrix: return value for the Matrix
   --
   --  Stores the pattern's transformation matrix into matrix.

   procedure Set_Extend (Pattern : Cairo_Pattern; Extend : Cairo_Extend);
   --  Pattern: a Cairo_Pattern
   --  Extend: a Cairo_Extend describing how the area outside of the
   --  pattern will be drawn
   --
   --  Sets the mode to be used for drawing outside the area of a pattern.
   --  See Cairo_Extend for details on the semantics of each extend
   --  strategy.
   --
   --  The default extend mode is CAIRO_EXTEND_NONE for surface patterns
   --  and CAIRO_EXTEND_PAD for gradient patterns.

   function Get_Extend (Pattern : Cairo_Pattern) return Cairo_Extend;
   --  Pattern: a Cairo_Pattern
   --
   --  Gets the current extend mode for a pattern.  See Cairo_Extend
   --  for details on the semantics of each extend strategy.
   --
   --  Return value: the current extend strategy used for drawing the
   --  pattern.

   procedure Set_Filter (Pattern : Cairo_Pattern; Filter : Cairo_Filter);
   --  Pattern: a Cairo_Pattern
   --  Filter: a Cairo_Filter describing the Filter to use for resizing
   --  the pattern
   --
   --  Sets the filter to be used for resizing when using this pattern.
   --  See Cairo_Filter for details on each filter.
   --
   --  * Note that you might want to control filtering even when you do not
   --  have an explicit Cairo_Pattern object, (for example when using
   --  Cairo_Set_Source_Surface). In these cases, it is convenient to
   --  use Cairo_Get_Source to get access to the pattern that cairo
   --  creates implicitly. For example:
   --
   --  <informalexample><programlisting>
   --  Cairo_Set_Source_Surface (cr, image, x, y);
   --  Cairo.Pattern.Set_Filter (Cairo_Get_Source (cr), CAIRO_FILTER_NEAREST);
   --  </programlisting></informalexample>

   function Get_Filter (Pattern : Cairo_Pattern) return Cairo_Filter;
   --  Pattern: a Cairo_Pattern
   --
   --  Gets the current filter for a pattern.  See Cairo_Filter
   --  for details on each filter.
   --
   --  Return value: the current filter used for resizing the pattern.

   function Get_Rgba
     (Pattern : Cairo_Pattern;
      Red     : access Gdouble;
      Green   : access Gdouble;
      Blue    : access Gdouble;
      Alpha   : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Red: return value for Red component of color, or NULL
   --  Green: return value for Green component of color, or NULL
   --  Blue: return value for Blue component of color, or NULL
   --  Alpha: return value for Alpha component of color, or NULL
   --
   --  Gets the solid color for a solid color pattern.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH if the pattern is not a solid
   --  color pattern.
   --
   --  Since: 1.4

   function Get_Surface
     (Pattern : Cairo_Pattern;
      Surface : System.Address)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Surface: return value for Surface of pattern, or NULL
   --
   --  Gets the surface of a surface pattern.  The reference returned in
   --  surface is owned by the pattern; the caller should call
   --  Cairo.Surface.Reference if the surface is to be retained.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH if the pattern is not a surface
   --  pattern.
   --
   --  Since: 1.4

   function Get_Color_Stop_Rgba
     (Pattern : Cairo_Pattern;
      Index   : Gint;
      Offset  : access Gdouble;
      Red     : access Gdouble;
      Green   : access Gdouble;
      Blue    : access Gdouble;
      Alpha   : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Index: Index of the stop to return data for
   --  Offset: return value for the Offset of the stop, or NULL
   --  Red: return value for Red component of color, or NULL
   --  Green: return value for Green component of color, or NULL
   --  Blue: return value for Blue component of color, or NULL
   --  Alpha: return value for Alpha component of color, or NULL
   --
   --  Gets the color and offset information at the given index for a
   --  gradient pattern.  Values of index are 0 to 1 less than the number
   --  returned by Cairo.Pattern.Get_Color_Stop_Count.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, or CAIRO_STATUS_INVALID_INDEX
   --  if index is not valid for the given pattern.  If the pattern is
   --  not a gradient pattern, CAIRO_STATUS_PATTERN_TYPE_MISMATCH is
   --  returned.
   --
   --  Since: 1.4

   function Get_Color_Stop_Count
     (Pattern : Cairo_Pattern;
      Count   : access Gint)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  Count: return value for the number of color stops, or NULL
   --
   --  Gets the number of color stops specified in the given gradient
   --  pattern.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern is not a gradient
   --  pattern.
   --
   --  Since: 1.4

   function Get_Linear_Points
     (Pattern : Cairo_Pattern;
      X0      : access Gdouble;
      Y0      : access Gdouble;
      X1      : access Gdouble;
      Y1      : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  X0: return value for the x coordinate of the first point, or NULL
   --  Y0: return value for the y coordinate of the first point, or NULL
   --  X1: return value for the x coordinate of the second point, or NULL
   --  Y1: return value for the y coordinate of the second point, or NULL
   --
   --  Gets the gradient endpoints for a linear gradient.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern is not a linear
   --  gradient pattern.
   --
   --  Since: 1.4

   function Get_Radial_Circles
     (Pattern : Cairo_Pattern;
      X0      : access Gdouble;
      Y0      : access Gdouble;
      R0      : access Gdouble;
      X1      : access Gdouble;
      Y1      : access Gdouble;
      R1      : access Gdouble)
      return    Cairo_Status;
   --  Pattern: a Cairo_Pattern
   --  X0: return value for the x coordinate of the center of the first
   --circle, or NULL
   --  Y0: return value for the y coordinate of the center of the first
   --circle, or NULL
   --  R0: return value for the radius of the first circle, or NULL
   --  X1: return value for the x coordinate of the center of the second
   --circle, or NULL
   --  Y1: return value for the y coordinate of the center of the second
   --circle, or NULL
   --  R1: return value for the radius of the second circle, or NULL
   --
   --  Gets the gradient endpoint circles for a radial gradient, each
   --  specified as a center coordinate and a radius.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH if pattern is not a radial
   --  gradient pattern.
   --
   --  Since: 1.4

private

   pragma Import (C, Create_Rgb, "cairo_pattern_create_rgb");
   pragma Import (C, Create_Rgba, "cairo_pattern_create_rgba");
   pragma Import (C, Create_For_Surface, "cairo_pattern_create_for_surface");
   pragma Import (C, Create_Linear, "cairo_pattern_create_linear");
   pragma Import (C, Create_Radial, "cairo_pattern_create_radial");
   pragma Import (C, Reference, "cairo_pattern_reference");
   pragma Import (C, Destroy, "cairo_pattern_destroy");
   pragma Import
     (C,
      Get_Reference_Count,
      "cairo_pattern_get_reference_count");
   pragma Import (C, Status, "cairo_pattern_status");
   pragma Import (C, Get_User_Data, "cairo_pattern_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_pattern_set_user_data");
   pragma Import (C, Get_Type, "cairo_pattern_get_type");
   pragma Import (C, Add_Color_Stop_Rgb, "cairo_pattern_add_color_stop_rgb");
   pragma Import
     (C,
      Add_Color_Stop_Rgba,
      "cairo_pattern_add_color_stop_rgba");
   pragma Import (C, Set_Matrix, "cairo_pattern_set_matrix");
   pragma Import (C, Get_Matrix, "cairo_pattern_get_matrix");
   pragma Import (C, Set_Extend, "cairo_pattern_set_extend");
   pragma Import (C, Get_Extend, "cairo_pattern_get_extend");
   pragma Import (C, Set_Filter, "cairo_pattern_set_filter");
   pragma Import (C, Get_Filter, "cairo_pattern_get_filter");
   pragma Import (C, Get_Rgba, "cairo_pattern_get_rgba");
   pragma Import (C, Get_Surface, "cairo_pattern_get_surface");
   pragma Import
     (C,
      Get_Color_Stop_Rgba,
      "cairo_pattern_get_color_stop_rgba");
   pragma Import
     (C,
      Get_Color_Stop_Count,
      "cairo_pattern_get_color_stop_count");
   pragma Import (C, Get_Linear_Points, "cairo_pattern_get_linear_points");
   pragma Import (C, Get_Radial_Circles, "cairo_pattern_get_radial_circles");

end Cairo.Pattern;
