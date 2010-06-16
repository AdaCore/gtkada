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

package Cairo is

   type Cairo_Bool is new Boolean;

   --   Cairo_Context:
   --
   --   A Cairo_Context contains the current state of the rendering device,
   --   including coordinates of yet to be drawn shapes.
   --
   --   Cairo contexts, as Cairo_Context objects are named, are central to
   --   cairo and all drawing with cairo is always done to a Cairo_Context
   --   object.
   --
   --   Memory management of Cairo_Context is done with subprograms
   --   Reference and Destroy, see below.

   type Cairo_Context is private;

   --   Cairo_Surface:
   --
   --   A Cairo_Surface represents an image, either as the destination
   --   of a drawing operation or as source when drawing onto another
   --   surface.  To draw to a Cairo_Surface, create a cairo context
   --   with the surface as the target, using Create.
   --
   --   There are different subtypes of Cairo_Surface for
   --   different drawing backends; for example, Cairo.Image_Surface.Create
   --   creates a bitmap image in memory.
   --   The type of a surface can be queried with Cairo.Surface.Get_Type.
   --
   --   Memory management of Cairo_Surface is done with
   --   Cairo.Surface.Reference and Cairo.Surface.Destroy.

   type Cairo_Surface is private;

   --   Cairo_Matrix:
   --   Xx: Xx component of the affine transformation
   --   Yx: Yx component of the affine transformation
   --   Xy: Xy component of the affine transformation
   --   Yy: Yy component of the affine transformation
   --   X0: X translation component of the affine transformation
   --   Y0: Y translation component of the affine transformation
   --
   --   A Cairo_Matrix holds an affine transformation, such as a scale,
   --   rotation, shear, or a combination of those. The transformation of
   --   a point (X, Y) is given by:
   --
   --       X_New = Xx * X + Xy * Y + X0;
   --       Y_New = Yx * X + Yy * Y + Y0;

   type Cairo_Matrix is record
      Xx : aliased Gdouble;
      Yx : aliased Gdouble;
      Xy : aliased Gdouble;
      Yy : aliased Gdouble;
      X0 : aliased Gdouble;
      Y0 : aliased Gdouble;
   end record;

   --   Cairo_Pattern:
   --
   --   A Cairo_Pattern represents a source when drawing onto a
   --   surface. There are different subtypes of Cairo_Pattern,
   --   for different types of sources; for example,
   --   Cairo.Pattern.Create_Rgb creates a pattern for a solid
   --   opaque color.
   --
   --   Other than various Cairo.Pattern.Create_<type>
   --   functions, some of the pattern types can be implicitly created
   --   using various Set_Source_<type> functions; for example Set_Source_Rgb.
   --
   --   The type of a pattern can be queried with Cairo.Pattern.Get_Type.
   --
   --   Memory management of Cairo_Pattern is done with
   --   Cairo.Pattern.Reference and Cairo.Pattern.Destroy.

   type Cairo_Pattern is private;

   --   Cairo_Destroy_Func:
   --   Data: The Data element being destroyed.
   --
   --   Cairo_destroy_func the type of function which is called when a
   --   data element is destroyed. It is passed the pointer to the data
   --   element and should free any memory and resources allocated for it.

   type Cairo_Destroy_Func is access procedure (Arg1 : System.Address);
   pragma Convention (C, Cairo_Destroy_Func);

   --   Cairo_User_Data_Key:
   --   Unused: not used; ignore.
   --
   --   Cairo_user_data_key is used for attaching user data to cairo
   --   data structures.  The actual contents of the struct is never used,
   --   and there is no need to initialize the object; only the unique
   --   address of a Cairo_data_key object is used.  Typically, you
   --   would just use the address of a static Cairo_data_key object.
   --

   type Cairo_User_Data_Key is record
      Unused : aliased Gint;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_User_Data_Key);

   --   Cairo_Status:
   --
   --   Cairo_status is used to indicate errors that can occur when
   --   using Cairo. In some cases it is returned directly by functions.
   --   but when using Cairo_T, the last error, if any, is stored in
   --   the context and can be retrieved with Cairo_Status.
   --
   --   New entries may be added in future versions.  Use
   --   Cairo_Status_To_String
   --   to get a human-readable representation of an error message.
   --

   type Cairo_Status is
     (
      Cairo_Status_Success,
      --  no error has occurred

      Cairo_Status_No_Memory,
      --  out of memory

      Cairo_Status_Invalid_Restore,
      --  Cairo_Restore called without matching Cairo_Save

      Cairo_Status_Invalid_Pop_Group,
      --  no saved group to pop

      Cairo_Status_No_Current_Point,
      --  no current point defined

      Cairo_Status_Invalid_Matrix,
      --  invalid matrix (not invertible)

      Cairo_Status_Invalid_Status,
      --  invalid value for an input Cairo_status

      Cairo_Status_Null_Pointer,
      --  NULL pointer

      Cairo_Status_Invalid_String,
      --  input string not valid UTF-8

      Cairo_Status_Invalid_Path_Data,
      --  input path data not valid

      Cairo_Status_Read_Error,
      --  error while reading from input stream

      Cairo_Status_Write_Error,
      --  error while writing to output stream

      Cairo_Status_Surface_Finished,
      --  target surface has been finished

      Cairo_Status_Surface_Type_Mismatch,
      --  the surface type is not appropriate for the operation

      Cairo_Status_Pattern_Type_Mismatch,
      --  the pattern type is not appropriate for the operation

      Cairo_Status_Invalid_Content,
      --  invalid value for an input Cairo_content

      Cairo_Status_Invalid_Format,
      --  invalid value for an input Cairo_format

      Cairo_Status_Invalid_Visual,
      --  invalid value for an input Visual*

      Cairo_Status_File_Not_Found,
      --  file not found

      Cairo_Status_Invalid_Dash,
      --  invalid value for a dash setting

      Cairo_Status_Invalid_Dsc_Comment,
      --  invalid value for a DSC comment (Since 1.2)

      Cairo_Status_Invalid_Index,
      --  invalid index passed to getter (Since 1.4)

      Cairo_Status_Clip_Not_Representable,
      --  clip region not representable in desired format (Since 1.4)

      Cairo_Status_Temp_File_Error,
      --  error creating or writing to a temporary file (Since 1.6)

      Cairo_Status_Invalid_Stride,
      --  invalid value for stride (Since 1.6)

      Cairo_Status_Font_Type_Mismatch,
      --  the font type is not appropriate for the operation (Since 1.8)

      Cairo_Status_User_Font_Immutable,
      --  the user-font is immutable (Since 1.8)

      Cairo_Status_User_Font_Error,
      --  error occurred in a user-font callback function (Since 1.8)

      Cairo_Status_Negative_Count,
      --  negative number used where it is not allowed (Since 1.8)

      Cairo_Status_Invalid_Clusters,
      --  input clusters do not represent the accompanying text and glyph
      --   array (Since 1.8)

      Cairo_Status_Invalid_Slant,
      --  invalid value for an input Cairo_Font_Slant (Since 1.8)

      Cairo_Status_Invalid_Weight
      --  invalid value for an input Cairo_Font_Weight (Since 1.8)
     );

   --   Cairo_Content:
   --
   --   Cairo_content is used to describe the content that a surface will
   --   contain, whether color information, alpha information (translucence
   --   vs. opacity), or both.
   --
   --   Note: The large values here are designed to keep Cairo_Content
   --   values distinct from Cairo_Format values so that the
   --   implementation can detect the error if users confuse the two types.

   subtype Cairo_Content is Guint;

   Cairo_Content_Color       : constant Cairo_Content := 4096;
   --   The surface will hold color content only.

   Cairo_Content_Alpha       : constant Cairo_Content := 8192;
   --   CAIRO_CONTENT_ALPHA: The surface will hold alpha content only.

   Cairo_Content_Color_Alpha : constant Cairo_Content := 12288;
   --   CAIRO_CONTENT_COLOR_ALPHA: The surface will hold color and alpha
   --   content.

   --   Cairo_Write_Func:
   --   Closure: the output Closure
   --   Data: the buffer containing the Data to write
   --   Length: the amount of data to write
   --
   --   Cairo_write_func is the type of function which is called when a
   --   backend needs to write data to an output stream.  It is passed the
   --   closure which was specified by the user at the time the write
   --   function was registered, the data to write and the length of the
   --   data in bytes.  The write function should return
   --   CAIRO_STATUS_SUCCESS if all the data was successfully written,
   --   CAIRO_STATUS_WRITE_ERROR otherwise.
   --
   --   Returns: the status code of the write operation

   type Cairo_Write_Func is access function
     (Arg1 : System.Address;
      Arg2 : access Guchar;
      Arg3 : Guint)
      return Cairo_Status;

   --   Cairo_Read_Func:
   --   Closure: the input Closure
   --   Data: the buffer into which to read the Data
   --   Length: the amount of data to read
   --
   --   Cairo_read_func is the type of function which is called when a
   --   backend needs to read data from an input stream.  It is passed the
   --   closure which was specified by the user at the time the read
   --   function was registered, the buffer to read the data into and the
   --   length of the data in bytes.  The read function should return
   --   CAIRO_STATUS_SUCCESS if all the data was successfully read,
   --   CAIRO_STATUS_READ_ERROR otherwise.
   --
   --   Returns: the status code of the read operation

   type Cairo_Read_Func is access function
     (Arg1 : System.Address;
      Arg2 : access Guchar;
      Arg3 : Guint)
      return Cairo_Status;

   function Create (Target : Cairo_Surface) return Cairo_Context;
   --  Target: Target surface for the context
   --
   --  Creates a new Cairo_Context with all graphics state parameters set to
   --  default values and with target as a target surface. The target
   --  surface should be constructed with a backend-specific function such
   --  as Cairo.Image_Surface.Create.
   --
   --  This function references target, so you can immediately
   --  call Cairo.Surface.Destroy on it if you don't need to
   --  maintain a separate reference to it.
   --
   --  Return value: a newly allocated Cairo_Context with a reference
   --   count of 1. The initial reference count should be released
   --   with Destroy when you are done using the Cairo_Context.
   --   This function never returns NULL. If memory cannot be
   --   allocated, a special Cairo_Context object will be returned on
   --   which Status returns Cairo_Status_No_Memory.
   --   You can use this object normally, but no drawing will
   --   be done.

   function Reference (Cr : Cairo_Context) return Cairo_Context;
   --  Cr: a Cairo_Context
   --
   --  Increases the reference count on cr by one. This prevents
   --  cr from being destroyed until a matching call to Destroy
   --  is made.
   --
   --  The number of references to a Cairo_Context can be get using
   --  Get_Reference_Count.
   --
   --  Return value: the referenced Cairo_Context.

   procedure Destroy (Cr : Cairo_Context);
   --  Cr: a Cairo_Context
   --
   --  Decreases the reference count on cr by one. If the result
   --  is zero, then cr and all associated resources are freed.
   --  See Reference.

   function Get_Reference_Count (Cr : Cairo_Context) return Guint;
   --  Cr: a Cairo_Context
   --
   --  Returns the current reference count of cr.
   --
   --  Return value: the current reference count of cr.  If the
   --  object is a nil object, 0 will be returned.
   --
   --  Since: 1.4

   function Get_User_Data
     (Cr   : Cairo_Context;
      Key  : access constant Cairo_User_Data_Key)
      return System.Address;
   --  Cr: a Cairo_Context
   --  Key: the address of the Cairo_User_Data_Key the user data was
   --  attached to
   --
   --  Return user data previously attached to cr using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4

   function Set_User_Data
     (Cr        : Cairo_Context;
      Key       : access constant Cairo_User_Data_Key;
      User_Data : System.Address;
      Destroy   : Cairo_Destroy_Func)
      return      Cairo_Status;
   --  Cr: a Cairo_Context
   --  Key: the address of a Cairo_User_Data_Key to attach the user data to
   --  User_Data: the user data to attach to the Cairo_Context
   --  Destroy: a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to cr.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   procedure Save (Cr : Cairo_Context);
   --  Cr: a Cairo_Context
   --
   --  Makes a copy of the current state of cr and saves it
   --  on an internal stack of saved states for cr. When
   --  Cairo_Restore is called, cr will be restored to
   --  the saved state. Multiple calls to Cairo_Save and
   --  Cairo_Restore can be nested; each call to Cairo_Restore
   --  restores the state from the matching paired Cairo_Save.
   --
   --  It isn't necessary to clear all saved states before
   --  a Cairo_Context is freed. If the reference count of a Cairo_Context
   --  drops to zero in response to a call to Cairo_Destroy,
   --  any saved states will be freed along with the Cairo_Context.

   procedure Restore (Cr : Cairo_Context);
   --  Cr: a Cairo_Context
   --
   --  Restores cr to the state saved by a preceding call to
   --  Cairo_Save and removes that state from the stack of
   --  saved states.

   procedure Push_Group (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Temporarily redirects drawing to an intermediate surface known as a
   --  group. The redirection lasts until the group is completed by a call
   --  to Cairo_Pop_Group or Cairo_Pop_Group_To_Source. These calls
   --  provide the result of any drawing to the group as a pattern,
   --  (either as an explicit object, or set as the source pattern).
   --
   --  This group functionality can be convenient for performing
   --  intermediate compositing. One common use of a group is to render
   --  objects as opaque within the group, (so that they occlude each
   --  other), and then blend the result with translucence onto the
   --  destination.
   --
   --  Groups can be nested arbitrarily deep by making balanced calls to
   --  Cairo_Push_Group/cairo_Pop_Group. Each call pushes/pops the new
   --  target group onto/from a stack.
   --
   --  The Cairo_Push_Group function calls Cairo_Save so that any
   --  changes to the graphics state will not be visible outside the
   --  group, (the pop_group functions call Cairo_Restore).
   --
   --  By default the intermediate group will have a content type of
   --  CAIRO_CONTENT_COLOR_ALPHA. Other content types can be chosen for
   --  the group by using Cairo_Push_Group_With_Content instead.
   --
   --  As an example, here is how one might fill and stroke a path with
   --  translucence, but without any portion of the fill being visible
   --  under the stroke:
   --
   --  <informalexample><programlisting>
   --  Cairo_Push_Group (cr);
   --  Cairo_Set_Source (cr, fill_pattern);
   --  Cairo_Fill_Preserve (cr);
   --  Cairo_Set_Source (cr, stroke_pattern);
   --  Cairo_Stroke (cr);
   --  Cairo_Pop_Group_To_Source (cr);
   --  Cairo_Paint_With_Alpha (cr, alpha);
   --  </programlisting></informalexample>
   --
   --  Since: 1.2

   procedure Push_Group_With_Content
     (Cr      : Cairo_Context;
      Content : Cairo_Content);
   --  Cr: a cairo context
   --  Content: a Cairo_Content indicating the type of group that
   --            will be created
   --
   --  Temporarily redirects drawing to an intermediate surface known as a
   --  group. The redirection lasts until the group is completed by a call
   --  to Cairo_Pop_Group or Cairo_Pop_Group_To_Source. These calls
   --  provide the result of any drawing to the group as a pattern,
   --  (either as an explicit object, or set as the source pattern).
   --
   --  The group will have a content type of content. The ability to
   --  control this content type is the only distinction between this
   --  function and Cairo_Push_Group which you should see for a more
   --  detailed description of group rendering.
   --
   --  Since: 1.2

   function Pop_Group (Cr : Cairo_Context) return Cairo_Pattern;
   --  Cr: a cairo context
   --
   --  Terminates the redirection begun by a call to Cairo_Push_Group or
   --  Cairo_Push_Group_With_Content and returns a new pattern
   --  containing the results of all drawing operations performed to the
   --  group.
   --
   --  The Cairo_Pop_Group function calls Cairo_Restore, (balancing a
   --  call to Cairo_Save by the push_group function), so that any
   --  changes to the graphics state will not be visible outside the
   --  group.
   --
   --  Return value: a newly created (surface) pattern containing the
   --  results of all drawing operations performed to the group. The
   --  caller owns the returned object and should call
   --  Cairo.Pattern.Destroy when finished with it.
   --
   --  Since: 1.2

   procedure Pop_Group_To_Source (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Terminates the redirection begun by a call to Cairo_Push_Group or
   --  Cairo_Push_Group_With_Content and installs the resulting pattern
   --  as the source pattern in the given cairo context.
   --
   --  The behavior of this function is equivalent to the sequence of
   --  operations:
   --
   --  <informalexample><programlisting>
   --  Cairo_Pattern *group = Cairo_Pop_Group (cr);
   --  Cairo_Set_Source (cr, group);
   --  Cairo.Pattern.Destroy (group);
   --  </programlisting></informalexample>
   --
   --  but is more convenient as their is no need for a variable to store
   --  the short-lived pointer to the pattern.
   --
   --  The Cairo_Pop_Group function calls Cairo_Restore, (balancing a
   --  call to Cairo_Save by the push_group function), so that any
   --  changes to the graphics state will not be visible outside the
   --  group.
   --
   --  Since: 1.2

   --  Modify state

   --   Cairo_Operator:
   --   CAIRO_OPERATOR_CLEAR: clear destination layer (bounded)
   --   CAIRO_OPERATOR_SOURCE: replace destination layer (bounded)
   --   CAIRO_OPERATOR_OVER: draw source layer on top of destination layer
   --   (bounded)
   --   CAIRO_OPERATOR_IN: draw source where there was destination content
   --   (unbounded)
   --   CAIRO_OPERATOR_OUT: draw source where there was no destination
   --   content (unbounded)
   --   CAIRO_OPERATOR_ATOP: draw source on top of destination content and
   --   only there
   --   CAIRO_OPERATOR_DEST: ignore the source
   --   CAIRO_OPERATOR_DEST_OVER: draw destination on top of source
   --   CAIRO_OPERATOR_DEST_IN: leave destination only where there was
   --   source content (unbounded)
   --   CAIRO_OPERATOR_DEST_OUT: leave destination only where there was no
   --   source content
   --   CAIRO_OPERATOR_DEST_ATOP: leave destination on top of source content
   --   and only there (unbounded)
   --   CAIRO_OPERATOR_XOR: source and destination are shown where there is
   --   only
   --   one of them
   --   CAIRO_OPERATOR_ADD: source and destination layers are accumulated
   --   CAIRO_OPERATOR_SATURATE: like over, but assuming source and dest are
   --   disjoint geometries
   --
   --   Cairo_operator is used to set the compositing operator for all cairo
   --   drawing operations.
   --
   --   The default operator is CAIRO_OPERATOR_OVER.
   --
   --   The operators marked as <firstterm>unbounded</firstterm> modify their
   --   destination even outside of the mask layer (that is, their effect is
   --   not
   --   bound by the mask layer).  However, their effect can still be limited
   --   by way of clipping.
   --
   --   To keep things simple, the operator descriptions here
   --   document the behavior for when both source and destination are either
   --   fully
   --   transparent or fully opaque.  The actual implementation works for
   --   translucent layers too.
   --   For a more detailed explanation of the effects of each operator,
   --   including the mathematical definitions, see
   --   <ulink url="http://cairographics.org/operators/">
   --   http://cairographics.org/operators/</ulink>.
   --

   type Cairo_Operator is (
      Cairo_Operator_Clear,
      Cairo_Operator_Source,
      Cairo_Operator_Over,
      Cairo_Operator_In,
      Cairo_Operator_Out,
      Cairo_Operator_Atop,
      Cairo_Operator_Dest,
      Cairo_Operator_Dest_Over,
      Cairo_Operator_Dest_In,
      Cairo_Operator_Dest_Out,
      Cairo_Operator_Dest_Atop,
      Cairo_Operator_Xor,
      Cairo_Operator_Add,
      Cairo_Operator_Saturate);
   pragma Convention (C, Cairo_Operator);

   procedure Set_Operator (Cr : Cairo_Context; Op : Cairo_Operator);
   --  Cr: a Cairo_Context
   --  Op: a compositing Operator, specified as a Cairo_Operator
   --
   --  Sets the compositing operator to be used for all drawing
   --  operations. See Cairo_Operator for details on the semantics of
   --  each available compositing operator.
   --
   --  The default operator is CAIRO_OPERATOR_OVER.

   procedure Set_Source (Cr : Cairo_Context; Source : Cairo_Pattern);
   --  Cr: a cairo context
   --  Source: a Cairo_Pattern to be used as the Source for
   --  subsequent drawing operations.
   --
   --  Sets the source pattern within cr to source. This pattern
   --  will then be used for any subsequent drawing operation until a new
   --  source pattern is set.
   --
   --  Note: The pattern's transformation matrix will be locked to the
   --  user space in effect at the time of Cairo_Set_Source. This means
   --  that further modifications of the current transformation matrix
   --  will not affect the source pattern. See Cairo.Pattern.Set_Matrix.
   --
   --  The default source pattern is a solid pattern that is opaque black,
   --  (that is, it is equivalent to Cairo_Set_Source_Rgb(cr, 0.0, 0.0,
   --  0.0)).

   procedure Set_Source_Rgb
     (Cr    : Cairo_Context;
      Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble);
   --  Cr: a cairo context
   --  Red: Red component of color
   --  Green: Green component of color
   --  Blue: Blue component of color
   --
   --  Sets the source pattern within cr to an opaque color. This opaque
   --  color will then be used for any subsequent drawing operation until
   --  a new source pattern is set.
   --
   --  The color components are floating point numbers in the range 0 to
   --  1. If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  The default source pattern is opaque black, (that is, it is
   --  equivalent to Cairo_Set_Source_Rgb(cr, 0.0, 0.0, 0.0)).

   procedure Set_Source_Rgba
     (Cr    : Cairo_Context;
      Red   : Gdouble;
      Green : Gdouble;
      Blue  : Gdouble;
      Alpha : Gdouble);
   --  Cr: a cairo context
   --  Red: Red component of color
   --  Green: Green component of color
   --  Blue: Blue component of color
   --  Alpha: Alpha component of color
   --
   --  Sets the source pattern within cr to a translucent color. This
   --  color will then be used for any subsequent drawing operation until
   --  a new source pattern is set.
   --
   --  The color and alpha components are floating point numbers in the
   --  range 0 to 1. If the values passed in are outside that range, they
   --  will be clamped.
   --
   --  The default source pattern is opaque black, (that is, it is
   --  equivalent to Cairo_Set_Source_Rgba(cr, 0.0, 0.0, 0.0, 1.0)).

   procedure Set_Source_Surface
     (Cr      : Cairo_Context;
      Surface : Cairo_Surface;
      X       : Gdouble;
      Y       : Gdouble);
   --  Cr: a cairo context
   --  Surface: a Surface to be used to set the source pattern
   --  X: User-space X coordinate for surface origin
   --  Y: User-space Y coordinate for surface origin
   --
   --  This is a convenience function for creating a pattern from surface
   --  and setting it as the source in cr with Cairo_Set_Source.
   --
   --  The x and y parameters give the user-space coordinate at which
   --  the surface origin should appear. (The surface origin is its
   --  upper-left corner before any transformation has been applied.) The
   --  x and y patterns are negated and then set as translation values
   --  in the pattern matrix.
   --
   --  Other than the initial translation pattern matrix, as described
   --  above, all other pattern attributes, (such as its extend mode), are
   --  set to the default values as in Cairo.Pattern.Create_For_Surface.
   --  The resulting pattern can be queried with Cairo_Get_Source so
   --  that these attributes can be modified if desired, (eg. to create a
   --  repeating pattern with Cairo.Pattern.Set_Extend).

   procedure Set_Tolerance (Cr : Cairo_Context; Tolerance : Gdouble);
   --  Cr: a Cairo_Context
   --  Tolerance: the Tolerance, in device units (typically pixels)
   --
   --  Sets the tolerance used when converting paths into trapezoids.
   --  Curved segments of the path will be subdivided until the maximum
   --  deviation between the original path and the polygonal approximation
   --  is less than tolerance. The default value is 0.1. A larger
   --  value will give better performance, a smaller value, better
   --  appearance. (Reducing the value from the default value of 0.1
   --  is unlikely to improve appearance significantly.)  The accuracy of paths
   --  within Cairo is limited by the precision of its internal arithmetic, and
   --  the prescribed tolerance is restricted to the smallest
   --  representable internal value.

   --   Cairo_Antialias:
   --   CAIRO_ANTIALIAS_DEFAULT: Use the default antialiasing for
   --     the subsystem and target device
   --   CAIRO_ANTIALIAS_NONE: Use a bilevel alpha mask
   --   CAIRO_ANTIALIAS_GRAY: Perform single-color antialiasing (using
   --    shades of gray for black text on a white background, for example).
   --   CAIRO_ANTIALIAS_SUBPIXEL: Perform antialiasing by taking
   --    advantage of the order of subpixel elements on devices
   --    such as LCD panels
   --
   --   Specifies the type of antialiasing to do when rendering text or shapes.
   --

   type Cairo_Antialias is (
      Cairo_Antialias_Default,
      Cairo_Antialias_None,
      Cairo_Antialias_Gray,
      Cairo_Antialias_Subpixel);
   pragma Convention (C, Cairo_Antialias);

   procedure Set_Antialias
     (Cr        : Cairo_Context;
      Antialias : Cairo_Antialias);
   --  Cr: a Cairo_Context
   --  Antialias: the new Antialiasing mode
   --
   --  Set the antialiasing mode of the rasterizer used for drawing shapes.
   --  This value is a hint, and a particular backend may or may not support
   --  a particular value.  At the current time, no backend supports
   --  CAIRO_ANTIALIAS_SUBPIXEL when drawing shapes.
   --
   --  Note that this option does not affect text rendering, instead see
   --  Cairo.Font_Options.Set_Antialias.

   --   Cairo_Fill_Rule:
   --   CAIRO_FILL_RULE_WINDING: If the path crosses the ray from
   --   left-to-right, counts +1. If the path crosses the ray
   --   from right to left, counts -1. (Left and right are determined
   --   from the perspective of looking along the ray from the starting
   --   point.) If the total count is non-zero, the point will be filled.
   --   CAIRO_FILL_RULE_EVEN_ODD: Counts the total number of
   --   intersections, without regard to the orientation of the contour. If
   --   the total number of intersections is odd, the point will be
   --   filled.
   --
   --   Cairo_fill_rule is used to select how paths are filled. For both
   --   fill rules, whether or not a point is included in the fill is
   --   determined by taking a ray from that point to infinity and looking
   --   at intersections with the path. The ray can be in any direction,
   --   as long as it doesn't pass through the end point of a segment
   --   or have a tricky intersection such as intersecting tangent to the path.
   --   (Note that filling is not actually implemented in this way. This
   --   is just a description of the rule that is applied.)
   --
   --   The default fill rule is CAIRO_FILL_RULE_WINDING.
   --
   --   New entries may be added in future versions.
   --

   type Cairo_Fill_Rule is (
      Cairo_Fill_Rule_Winding,
      Cairo_Fill_Rule_Even_Odd);
   pragma Convention (C, Cairo_Fill_Rule);

   procedure Set_Fill_Rule
     (Cr        : Cairo_Context;
      Fill_Rule : Cairo_Fill_Rule);
   --  Cr: a Cairo_Context
   --  Fill_Rule: a fill rule, specified as a Cairo_Fill_Rule
   --
   --  Set the current fill rule within the cairo context. The fill rule
   --  is used to determine which regions are inside or outside a complex
   --  (potentially self-intersecting) path. The current fill rule affects
   --  both Cairo_Fill and Cairo_Clip. See Cairo_Fill_Rule for details
   --  on the semantics of each available fill rule.
   --
   --  The default fill rule is CAIRO_FILL_RULE_WINDING.

   procedure Set_Line_Width (Cr : Cairo_Context; Width : Gdouble);
   --  Cr: a Cairo_Context
   --  Width: a line Width
   --
   --  Sets the current line width within the cairo context. The line
   --  width value specifies the diameter of a pen that is circular in
   --  user space, (though device-space pen may be an ellipse in general
   --  due to scaling/shear/rotation of the CTM).
   --
   --  Note: When the description above refers to user space and CTM it
   --  refers to the user space and CTM in effect at the time of the
   --  stroking operation, not the user space and CTM in effect at the
   --  time of the call to Cairo_Set_Line_Width. The simplest usage
   --  makes both of these spaces identical. That is, if there is no
   --  change to the CTM between a call to Cairo_Set_Line_Width and the
   --  stroking operation, then one can just pass user-space values to
   --  Cairo_Set_Line_Width and ignore this note.
   --
   --  As with the other stroke parameters, the current line width is
   --  examined by Cairo_Stroke, Cairo_Stroke_Extents, and
   --  Cairo_Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default line width value is 2.0.

   --   Cairo_Line_Cap:
   --   CAIRO_LINE_CAP_BUTT: start(stop) the line exactly at the start(end)
   --   point
   --   CAIRO_LINE_CAP_ROUND: use a round ending, the center of the circle is
   --   the end point
   --   CAIRO_LINE_CAP_SQUARE: use squared ending, the center of the square is
   --   the end point
   --
   --   Specifies how to render the endpoints of the path when stroking.
   --
   --   The default line cap style is CAIRO_LINE_CAP_BUTT.
   --

   type Cairo_Line_Cap is (
      Cairo_Line_Cap_Butt,
      Cairo_Line_Cap_Round,
      Cairo_Line_Cap_Square);
   pragma Convention (C, Cairo_Line_Cap);

   procedure Set_Line_Cap (Cr : Cairo_Context; Line_Cap : Cairo_Line_Cap);
   --  Cr: a cairo context
   --  Line_Cap: a line cap style
   --
   --  Sets the current line cap style within the cairo context. See
   --  Cairo_Line_Cap for details about how the available line cap
   --  styles are drawn.
   --
   --  As with the other stroke parameters, the current line cap style is
   --  examined by Cairo_Stroke, Cairo_Stroke_Extents, and
   --  Cairo_Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default line cap style is CAIRO_LINE_CAP_BUTT.

   --   Cairo_Line_Join:
   --   CAIRO_LINE_JOIN_MITER: use a sharp (angled) corner, see
   --   Cairo_Set_Miter_Limit
   --   CAIRO_LINE_JOIN_ROUND: use a rounded join, the center of the circle is
   --   the joint point
   --   CAIRO_LINE_JOIN_BEVEL: use a cut-off join, the join is cut off at half
   --   the line width from the joint point
   --
   --   Specifies how to render the junction of two lines when stroking.
   --
   --   The default line join style is CAIRO_LINE_JOIN_MITER.
   --

   type Cairo_Line_Join is (
      Cairo_Line_Join_Miter,
      Cairo_Line_Join_Round,
      Cairo_Line_Join_Bevel);
   pragma Convention (C, Cairo_Line_Join);

   procedure Set_Line_Join
     (Cr        : Cairo_Context;
      Line_Join : Cairo_Line_Join);
   --  Cr: a cairo context
   --  Line_Join: a line join style
   --
   --  Sets the current line join style within the cairo context. See
   --  Cairo_Line_Join for details about how the available line join
   --  styles are drawn.
   --
   --  As with the other stroke parameters, the current line join style is
   --  examined by Cairo_Stroke, Cairo_Stroke_Extents, and
   --  Cairo_Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default line join style is CAIRO_LINE_JOIN_MITER.

   procedure Set_Dash
     (Cr         : Cairo_Context;
      Dashes     : access Gdouble;
      Num_Dashes : Gint;
      Offset     : Gdouble);
   --  Cr: a cairo context
   --  Dashes: an array specifying alternate lengths of on and off stroke
   --  portions
   --  Num_Dashes: the length of the dashes array
   --  Offset: an Offset into the dash pattern at which the stroke should start
   --
   --  Sets the dash pattern to be used by Cairo_Stroke. A dash pattern
   --  is specified by dashes, an array of positive values. Each value
   --  provides the length of alternate "on" and "off" portions of the
   --  stroke. The offset specifies an offset into the pattern at which
   --  the stroke begins.
   --
   --  Each "on" segment will have caps applied as if the segment were a
   --  separate sub-path. In particular, it is valid to use an "on" length
   --  of 0.0 with CAIRO_LINE_CAP_ROUND or CAIRO_LINE_CAP_SQUARE in order
   --  to distributed dots or squares along a path.
   --
   --  Note: The length values are in user-space units as evaluated at the
   --  time of stroking. This is not necessarily the same as the user
   --  space at the time of Cairo_Set_Dash.
   --
   --  If num_dashes is 0 dashing is disabled.
   --
   --  If num_dashes is 1 a symmetric pattern is assumed with alternating
   --  on and off portions of the size specified by the single value in
   --  dashes.
   --
   --  If any value in dashes is negative, or if all values are 0, then
   --  cr will be put into an error state with a status of
   --  CAIRO_STATUS_INVALID_DASH.

   procedure Set_Miter_Limit (Cr : Cairo_Context; Limit : Gdouble);
   --  Cr: a cairo context
   --  Limit: miter Limit to set
   --
   --  Sets the current miter limit within the cairo context.
   --
   --  If the current line join style is set to CAIRO_LINE_JOIN_MITER
   --  (see Cairo_Set_Line_Join), the miter limit is used to determine
   --  whether the lines should be joined with a bevel instead of a miter.
   --  Cairo divides the length of the miter by the line width.
   --  If the result is greater than the miter limit, the style is
   --  converted to a bevel.
   --
   --  As with the other stroke parameters, the current line miter limit is
   --  examined by Cairo_Stroke, Cairo_Stroke_Extents, and
   --  Cairo_Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default miter limit value is 10.0, which will convert joins
   --  with interior angles less than 11 degrees to bevels instead of
   --  miters. For reference, a miter limit of 2.0 makes the miter cutoff
   --  at 60 degrees, and a miter limit of 1.414 makes the cutoff at 90
   --  degrees.
   --
   --  A miter limit for a desired angle can be computed as: miter limit =
   --  1/sin(angle/2)

   procedure Translate (Cr : Cairo_Context; Tx : Gdouble; Ty : Gdouble);
   --  Cr: a cairo context
   --  Tx: amount to translate in the X direction
   --  Ty: amount to translate in the Y direction
   --
   --  Modifies the current transformation matrix (CTM) by translating the
   --  user-space origin by (tx, ty). This offset is interpreted as a
   --  user-space coordinate according to the CTM in place before the new
   --  call to Cairo_Translate. In other words, the translation of the
   --  user-space origin takes place after any existing transformation.

   procedure Scale (Cr : Cairo_Context; Sx : Gdouble; Sy : Gdouble);
   --  Cr: a cairo context
   --  Sx: scale factor for the X dimension
   --  Sy: scale factor for the Y dimension
   --
   --  Modifies the current transformation matrix (CTM) by scaling the X
   --  and Y user-space axes by sx and sy respectively. The scaling of
   --  the axes takes place after any existing transformation of user
   --  space.

   procedure Rotate (Cr : Cairo_Context; Angle : Gdouble);
   --  Cr: a cairo context
   --  Angle: Angle (in radians) by which the user-space axes will be
   --  rotated
   --
   --  Modifies the current transformation matrix (CTM) by rotating the
   --  user-space axes by angle radians. The rotation of the axes takes
   --  places after any existing transformation of user space. The
   --  rotation direction for positive angles is from the positive X axis
   --  toward the positive Y axis.

   procedure Transform
     (Cr     : Cairo_Context;
      Matrix : access constant Cairo_Matrix);
   --  Cr: a cairo context
   --  Matrix: a transformation to be applied to the user-space axes
   --
   --  Modifies the current transformation matrix (CTM) by applying
   --  matrix as an additional transformation. The new transformation of
   --  user space takes place after any existing transformation.

   procedure Set_Matrix
     (Cr     : Cairo_Context;
      Matrix : access constant Cairo_Matrix);
   --  Cr: a cairo context
   --  Matrix: a transformation Matrix from user space to device space
   --
   --  Modifies the current transformation matrix (CTM) by setting it
   --  equal to matrix.

   procedure Identity_Matrix (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Resets the current transformation matrix (CTM) by setting it equal
   --  to the identity matrix. That is, the user-space and device-space
   --  axes will be aligned and one user-space unit will transform to one
   --  device-space unit.

   procedure User_To_Device
     (Cr : Cairo_Context;
      X  : access Gdouble;
      Y  : access Gdouble);
   --  Cr: a cairo context
   --  X: X value of coordinate (in/out parameter)
   --  Y: Y value of coordinate (in/out parameter)
   --
   --  Transform a coordinate from user space to device space by
   --  multiplying the given point by the current transformation matrix
   --  (CTM).

   procedure User_To_Device_Distance
     (Cr : Cairo_Context;
      Dx : access Gdouble;
      Dy : access Gdouble);
   --  Cr: a cairo context
   --  Dx: X component of a distance vector (in/out parameter)
   --  Dy: Y component of a distance vector (in/out parameter)
   --
   --  Transform a distance vector from user space to device space. This
   --  function is similar to Cairo_User_To_Device except that the
   --  translation components of the CTM will be ignored when transforming
   --  (dx,dy).

   procedure Device_To_User
     (Cr : Cairo_Context;
      X  : access Gdouble;
      Y  : access Gdouble);
   --  Cr: a cairo
   --  X: X value of coordinate (in/out parameter)
   --  Y: Y value of coordinate (in/out parameter)
   --
   --  Transform a coordinate from device space to user space by
   --  multiplying the given point by the inverse of the current
   --  transformation matrix (CTM).

   procedure Device_To_User_Distance
     (Cr : Cairo_Context;
      Dx : access Gdouble;
      Dy : access Gdouble);
   --  Cr: a cairo context
   --  Dx: X component of a distance vector (in/out parameter)
   --  Dy: Y component of a distance vector (in/out parameter)
   --
   --  Transform a distance vector from device space to user space. This
   --  function is similar to Cairo_Device_To_User except that the
   --  translation components of the inverse CTM will be ignored when
   --  transforming (dx,dy).

   --  Path creation functions
   procedure New_Path (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Clears the current path. After this call there will be no path and
   --  no current point.

   procedure Move_To (Cr : Cairo_Context; X : Gdouble; Y : Gdouble);
   --  Cr: a cairo context
   --  X: the X coordinate of the new position
   --  Y: the Y coordinate of the new position
   --
   --  Begin a new sub-path. After this call the current point will be (x,
   --  y).

   procedure New_Sub_Path (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Begin a new sub-path. Note that the existing path is not
   --  affected. After this call there will be no current point.
   --
   --  In many cases, this call is not needed since new sub-paths are
   --  frequently started with Cairo_Move_To.
   --
   --  A call to Cairo_New_Sub_Path is particularly useful when
   --  beginning a new sub-path with one of the Cairo_Arc calls. This
   --  makes things easier as it is no longer necessary to manually
   --  compute the arc's initial coordinates for a call to
   --  Cairo_Move_To.
   --
   --  Since: 1.2

   procedure Line_To (Cr : Cairo_Context; X : Gdouble; Y : Gdouble);
   --  Cr: a cairo context
   --  X: the X coordinate of the end of the new line
   --  Y: the Y coordinate of the end of the new line
   --
   --  Adds a line to the path from the current point to position (x, y)
   --  in user-space coordinates. After this call the current point
   --  will be (x, y).
   --
   --  If there is no current point before the call to Cairo_Line_To
   --  this function will behave as Cairo_Move_To(cr, x, y).

   procedure Curve_To
     (Cr : Cairo_Context;
      X1 : Gdouble;
      Y1 : Gdouble;
      X2 : Gdouble;
      Y2 : Gdouble;
      X3 : Gdouble;
      Y3 : Gdouble);
   --  Cr: a cairo context
   --  X1: the X coordinate of the first control point
   --  Y1: the Y coordinate of the first control point
   --  X2: the X coordinate of the second control point
   --  Y2: the Y coordinate of the second control point
   --  X3: the X coordinate of the end of the curve
   --  Y3: the Y coordinate of the end of the curve
   --
   --  Adds a cubic Bézier spline to the path from the current point to
   --  position (x3, y3) in user-space coordinates, using (x1, y1) and
   --  (x2, y2) as the control points. After this call the current point
   --  will be (x3, y3).
   --
   --  If there is no current point before the call to Cairo_Curve_To
   --  this function will behave as if preceded by a call to
   --  Cairo_Move_To(cr, x1, y1).

   procedure Arc
     (Cr     : Cairo_Context;
      Xc     : Gdouble;
      Yc     : Gdouble;
      Radius : Gdouble;
      Angle1 : Gdouble;
      Angle2 : Gdouble);
   --  Cr: a cairo context
   --  Xc: X position of the center of the arc
   --  Yc: Y position of the center of the arc
   --  Radius: the Radius of the arc
   --  Angle1: the start angle, in radians
   --  Angle2: the end angle, in radians
   --
   --  Adds a circular arc of the given radius to the current path.  The
   --  arc is centered at (xc, yc), begins at angle1 and proceeds in
   --  the direction of increasing angles to end at angle2. If angle2 is
   --  less than angle1 it will be progressively increased by 2*M_PI
   --  until it is greater than angle1.
   --
   --  If there is a current point, an initial line segment will be added
   --  to the path to connect the current point to the beginning of the
   --  arc. If this initial line is undesired, it can be avoided by
   --  calling Cairo_New_Sub_Path before calling Cairo_Arc.
   --
   --  Angles are measured in radians. An angle of 0.0 is in the direction
   --  of the positive X axis (in user space). An angle of M_PI/2.0 radians
   --  (90 degrees) is in the direction of the positive Y axis (in
   --  user space). Angles increase in the direction from the positive X
   --  axis toward the positive Y axis. So with the default transformation
   --  matrix, angles increase in a clockwise direction.
   --
   --  (To convert from degrees to radians, use <literal>degrees * (M_PI /
   --  180.)</literal>.)
   --
   --  This function gives the arc in the direction of increasing angles;
   --  see Cairo_Arc_Negative to get the arc in the direction of
   --  decreasing angles.
   --
   --  The arc is circular in user space. To achieve an elliptical arc,
   --  you can scale the current transformation matrix by different
   --  amounts in the X and Y directions. For example, to draw an ellipse
   --  in the box given by X, y, width, height:
   --
   --  <informalexample><programlisting>
   --  Cairo_Save (cr);
   --  Cairo_Translate (cr, x + width / 2., y + height / 2.);
   --  Cairo_Scale (cr, width / 2., height / 2.);
   --  Cairo_Arc (cr, 0., 0., 1., 0., 2 * M_PI);
   --  Cairo_Restore (cr);
   --  </programlisting></informalexample>

   procedure Arc_Negative
     (Cr     : Cairo_Context;
      Xc     : Gdouble;
      Yc     : Gdouble;
      Radius : Gdouble;
      Angle1 : Gdouble;
      Angle2 : Gdouble);
   --  Cr: a cairo context
   --  Xc: X position of the center of the arc
   --  Yc: Y position of the center of the arc
   --  Radius: the Radius of the arc
   --  Angle1: the start angle, in radians
   --  Angle2: the end angle, in radians
   --
   --  Adds a circular arc of the given radius to the current path.  The
   --  arc is centered at (xc, yc), begins at angle1 and proceeds in
   --  the direction of decreasing angles to end at angle2. If angle2 is
   --  greater than angle1 it will be progressively decreased by 2*M_PI
   --  until it is less than angle1.
   --
   --  See Cairo_Arc for more details. This function differs only in the
   --  direction of the arc between the two angles.

   --  XXX: NYI
   --  Cairo_Public void
   --  Cairo_Arc_To (Cairo_Context *cr,
   --         Gdouble x1, Gdouble y1,
   --         Gdouble x2, Gdouble y2,
   --         Gdouble radius);
   --

   procedure Rel_Move_To (Cr : Cairo_Context; Dx : Gdouble; Dy : Gdouble);
   --  Cr: a cairo context
   --  Dx: the X offset
   --  Dy: the Y offset
   --
   --  Begin a new sub-path. After this call the current point will offset
   --  by (x, y).
   --
   --  Given a current point of (x, y), Cairo_Rel_Move_To(cr, dx, dy)
   --  is logically equivalent to Cairo_Move_To(cr, x + dx, y + dy).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause cr to shutdown with a status of
   --  CAIRO_STATUS_NO_CURRENT_POINT.

   procedure Rel_Line_To (Cr : Cairo_Context; Dx : Gdouble; Dy : Gdouble);
   --  Cr: a cairo context
   --  Dx: the X offset to the end of the new line
   --  Dy: the Y offset to the end of the new line
   --
   --  Relative-coordinate version of Cairo_Line_To. Adds a line to the
   --  path from the current point to a point that is offset from the
   --  current point by (dx, dy) in user space. After this call the
   --  current point will be offset by (dx, dy).
   --
   --  Given a current point of (x, y), Cairo_Rel_Line_To(cr, dx, dy)
   --  is logically equivalent to Cairo_Line_To(cr, x + dx, y + dy).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause cr to shutdown with a status of
   --  CAIRO_STATUS_NO_CURRENT_POINT.

   procedure Rel_Curve_To
     (Cr  : Cairo_Context;
      Dx1 : Gdouble;
      Dy1 : Gdouble;
      Dx2 : Gdouble;
      Dy2 : Gdouble;
      Dx3 : Gdouble;
      Dy3 : Gdouble);
   --  Cr: a cairo context
   --  Dx1: the X offset to the first control point
   --  Dy1: the Y offset to the first control point
   --  Dx2: the X offset to the second control point
   --  Dy2: the Y offset to the second control point
   --  Dx3: the X offset to the end of the curve
   --  Dy3: the Y offset to the end of the curve
   --
   --  Relative-coordinate version of Cairo_Curve_To. All offsets are
   --  relative to the current point. Adds a cubic Bézier spline to the
   --  path from the current point to a point offset from the current
   --  point by (dx3, dy3), using points offset by (dx1, dy1) and
   --  (dx2, dy2) as the control points. After this call the current
   --  point will be offset by (dx3, dy3).
   --
   --  Given a current point of (x, y), Cairo_Rel_Curve_To(cr, dx1,
   --  dy1, dx2, dy2, dx3, dy3) is logically equivalent to
   --  Cairo_Curve_To(cr, x+dx1, y+dy1, x+dx2, y+dy2, x+dx3, y+dy3).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause cr to shutdown with a status of
   --  CAIRO_STATUS_NO_CURRENT_POINT.

   procedure Rectangle
     (Cr     : Cairo_Context;
      X      : Gdouble;
      Y      : Gdouble;
      Width  : Gdouble;
      Height : Gdouble);
   --  Cr: a cairo context
   --  X: the X coordinate of the top left corner of the rectangle
   --  Y: the Y coordinate to the top left corner of the rectangle
   --  Width: the Width of the rectangle
   --  Height: the Height of the rectangle
   --
   --  Adds a closed sub-path rectangle of the given size to the current
   --  path at position (x, y) in user-space coordinates.
   --
   --  This function is logically equivalent to:
   --  <informalexample><programlisting>
   --  Cairo_Move_To (cr, x, y);
   --  Cairo_Rel_Line_To (cr, width, 0);
   --  Cairo_Rel_Line_To (cr, 0, height);
   --  Cairo_Rel_Line_To (cr, -width, 0);
   --  Cairo_Close_Path (cr);
   --  </programlisting></informalexample>

   --  XXX: NYI
   --  Cairo_Public void
   --  Cairo_Stroke_To_Path (Cairo_Context *cr);
   --

   procedure Close_Path (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Adds a line segment to the path from the current point to the
   --  beginning of the current sub-path, (the most recent point passed to
   --  Cairo_Move_To), and closes this sub-path. After this call the
   --  current point will be at the joined endpoint of the sub-path.
   --
   --  The behavior of Cairo_Close_Path is distinct from simply calling
   --  Cairo_Line_To with the equivalent coordinate in the case of
   --  stroking. When a closed sub-path is stroked, there are no caps on
   --  the ends of the sub-path. Instead, there is a line join connecting
   --  the final and initial segments of the sub-path.
   --
   --  If there is no current point before the call to Cairo_Close_Path,
   --  this function will have no effect.
   --
   --  Note: As of cairo version 1.2.4 any call to Cairo_Close_Path will
   --  place an explicit MOVE_TO element into the path immediately after
   --  the CLOSE_PATH element, (which can be seen in Cairo_Copy_Path for
   --  example). This can simplify path processing in some cases as it may
   --  not be necessary to save the "last move_to point" during processing
   --  as the MOVE_TO immediately after the CLOSE_PATH will provide that
   --  point.

   procedure Path_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user-space coordinates covering the
   --  points on the current path. If the current path is empty, returns
   --  an empty rectangle ((0,0), (0,0)). Stroke parameters, fill rule,
   --  surface dimensions and clipping are not taken into account.
   --
   --  Contrast with Cairo_Fill_Extents and Cairo_Stroke_Extents which
   --  return the extents of only the area that would be "inked" by
   --  the corresponding drawing operations.
   --
   --  The result of Cairo_Path_Extents is defined as equivalent to the
   --  limit of Cairo_Stroke_Extents with CAIRO_LINE_CAP_ROUND as the
   --  line width approaches 0.0, (but never reaching the empty-rectangle
   --  returned by Cairo_Stroke_Extents for a line width of 0.0).
   --
   --  Specifically, this means that zero-area sub-paths such as
   --  Cairo_Move_To;Cairo_Line_To segments, (even degenerate cases
   --  where the coordinates to both calls are identical), will be
   --  considered as contributing to the extents. However, a lone
   --  Cairo_Move_To will not contribute to the results of
   --  Cairo_Path_Extents.
   --
   --  Since: 1.6

   --  Painting functions
   procedure Paint (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that paints the current source everywhere within
   --  the current clip region.

   procedure Paint_With_Alpha (Cr : Cairo_Context; Alpha : Gdouble);
   --  Cr: a cairo context
   --  Alpha: Alpha value, between 0 (transparent) and 1 (opaque)
   --
   --  A drawing operator that paints the current source everywhere within
   --  the current clip region using a mask of constant alpha value
   --  alpha. The effect is similar to Cairo_Paint, but the drawing
   --  is faded out using the alpha value.

   procedure Mask (Cr : Cairo_Context; Pattern : Cairo_Pattern);
   --  Cr: a cairo context
   --  Pattern: a Cairo_Pattern
   --
   --  A drawing operator that paints the current source
   --  using the alpha channel of pattern as a mask. (Opaque
   --  areas of pattern are painted with the source, transparent
   --  areas are not painted.)

   procedure Mask_Surface
     (Cr        : Cairo_Context;
      Surface   : Cairo_Surface;
      Surface_X : Gdouble;
      Surface_Y : Gdouble);
   --  Cr: a cairo context
   --  Surface: a Cairo_Surface
   --  Surface_X: X coordinate at which to place the origin of surface
   --  Surface_Y: Y coordinate at which to place the origin of surface
   --
   --  A drawing operator that paints the current source
   --  using the alpha channel of surface as a mask. (Opaque
   --  areas of surface are painted with the source, transparent
   --  areas are not painted.)

   procedure Stroke (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that strokes the current path according to the
   --  current line width, line join, line cap, and dash settings. After
   --  Cairo_Stroke, the current path will be cleared from the cairo
   --  context. See Cairo_Set_Line_Width, Cairo_Set_Line_Join,
   --  Cairo_Set_Line_Cap, Cairo_Set_Dash, and
   --  Cairo_Stroke_Preserve.
   --
   --  Note: Degenerate segments and sub-paths are treated specially and
   --  provide a useful result. These can result in two different
   --  situations:
   --
   --  1. Zero-length "on" segments set in Cairo_Set_Dash. If the cap
   --  style is CAIRO_LINE_CAP_ROUND or CAIRO_LINE_CAP_SQUARE then these
   --  segments will be drawn as circular dots or squares respectively. In
   --  the case of CAIRO_LINE_CAP_SQUARE, the orientation of the squares
   --  is determined by the direction of the underlying path.
   --
   --  2. A sub-path created by Cairo_Move_To followed by either a
   --  Cairo_Close_Path or one or more calls to Cairo_Line_To to the
   --  same coordinate as the Cairo_Move_To. If the cap style is
   --  CAIRO_LINE_CAP_ROUND then these sub-paths will be drawn as circular
   --  dots. Note that in the case of CAIRO_LINE_CAP_SQUARE a degenerate
   --  sub-path will not be drawn at all, (since the correct orientation
   --  is indeterminate).
   --
   --  In no case will a cap style of CAIRO_LINE_CAP_BUTT cause anything
   --  to be drawn in the case of either degenerate segments or sub-paths.

   procedure Stroke_Preserve (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that strokes the current path according to the
   --  current line width, line join, line cap, and dash settings. Unlike
   --  Cairo_Stroke, Cairo_Stroke_Preserve preserves the path within the
   --  cairo context.
   --
   --  See Cairo_Set_Line_Width, Cairo_Set_Line_Join,
   --  Cairo_Set_Line_Cap, Cairo_Set_Dash, and
   --  Cairo_Stroke_Preserve.

   procedure Fill (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that fills the current path according to the
   --  current fill rule, (each sub-path is implicitly closed before being
   --  filled). After Cairo_Fill, the current path will be cleared from
   --  the cairo context. See Cairo_Set_Fill_Rule and
   --  Cairo_Fill_Preserve.

   procedure Fill_Preserve (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  A drawing operator that fills the current path according to the
   --  current fill rule, (each sub-path is implicitly closed before being
   --  filled). Unlike Cairo_Fill, Cairo_Fill_Preserve preserves the
   --  path within the cairo context.
   --
   --  See Cairo_Set_Fill_Rule and Cairo_Fill.

   procedure Copy_Page (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Emits the current page for backends that support multiple pages, but
   --  doesn't clear it, so, the contents of the current page will be retained
   --  for the next page too.  Use Cairo_Show_Page if you want to get an
   --  empty page after the emission.
   --
   --  This is a convenience function that simply calls
   --  Cairo.Surface.Copy_Page on cr's target.

   procedure Show_Page (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Emits and clears the current page for backends that support multiple
   --  pages.  Use Cairo_Copy_Page if you don't want to clear the page.
   --
   --  This is a convenience function that simply calls
   --  Cairo.Surface.Show_Page on cr's target.

   --  Insideness testing
   function In_Stroke
     (Cr   : Cairo_Context;
      X    : Gdouble;
      Y    : Gdouble)
      return Cairo_Bool;
   --  Cr: a cairo context
   --  X: X coordinate of the point to test
   --  Y: Y coordinate of the point to test
   --
   --  Tests whether the given point is inside the area that would be
   --  affected by a Cairo_Stroke operation given the current path and
   --  stroking parameters. Surface dimensions and clipping are not taken
   --  into account.
   --
   --  See Cairo_Stroke, Cairo_Set_Line_Width, Cairo_Set_Line_Join,
   --  Cairo_Set_Line_Cap, Cairo_Set_Dash, and
   --  Cairo_Stroke_Preserve.
   --
   --  Return value: A non-zero value if the point is inside, or zero if
   --  outside.

   function In_Fill
     (Cr   : Cairo_Context;
      X    : Gdouble;
      Y    : Gdouble)
      return Cairo_Bool;
   --  Cr: a cairo context
   --  X: X coordinate of the point to test
   --  Y: Y coordinate of the point to test
   --
   --  Tests whether the given point is inside the area that would be
   --  affected by a Cairo_Fill operation given the current path and
   --  filling parameters. Surface dimensions and clipping are not taken
   --  into account.
   --
   --  See Cairo_Fill, Cairo_Set_Fill_Rule and Cairo_Fill_Preserve.
   --
   --  Return value: A non-zero value if the point is inside, or zero if
   --  outside.

   --  Rectangular extents
   procedure Stroke_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user coordinates covering the area that
   --  would be affected, (the "inked" area), by a Cairo_Stroke
   --  operation given the current path and stroke parameters.
   --  If the current path is empty, returns an empty rectangle ((0,0), (0,0)).
   --  Surface dimensions and clipping are not taken into account.
   --
   --  Note that if the line width is set to exactly zero, then
   --  Cairo_Stroke_Extents will return an empty rectangle. Contrast with
   --  Cairo_Path_Extents which can be used to compute the non-empty
   --  bounds as the line width approaches zero.
   --
   --  Note that Cairo_Stroke_Extents must necessarily do more work to
   --  compute the precise inked areas in light of the stroke parameters,
   --  so Cairo_Path_Extents may be more desirable for sake of
   --  performance if non-inked path extents are desired.
   --
   --  See Cairo_Stroke, Cairo_Set_Line_Width, Cairo_Set_Line_Join,
   --  Cairo_Set_Line_Cap, Cairo_Set_Dash, and
   --  Cairo_Stroke_Preserve.

   procedure Fill_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user coordinates covering the area that
   --  would be affected, (the "inked" area), by a Cairo_Fill operation
   --  given the current path and fill parameters. If the current path is
   --  empty, returns an empty rectangle ((0,0), (0,0)). Surface
   --  dimensions and clipping are not taken into account.
   --
   --  Contrast with Cairo_Path_Extents, which is similar, but returns
   --  non-zero extents for some paths with no inked area, (such as a
   --  simple line segment).
   --
   --  Note that Cairo_Fill_Extents must necessarily do more work to
   --  compute the precise inked areas in light of the fill rule, so
   --  Cairo_Path_Extents may be more desirable for sake of performance
   --  if the non-inked path extents are desired.
   --
   --  See Cairo_Fill, Cairo_Set_Fill_Rule and Cairo_Fill_Preserve.

   --  Clipping
   procedure Reset_Clip (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Reset the current clip region to its original, unrestricted
   --  state. That is, set the clip region to an infinitely large shape
   --  containing the target surface. Equivalently, if infinity is too
   --  hard to grasp, one can imagine the clip region being reset to the
   --  exact bounds of the target surface.
   --
   --  Note that code meant to be reusable should not call
   --  Cairo_Reset_Clip as it will cause results unexpected by
   --  higher-level code which calls Cairo_Clip. Consider using
   --  Cairo_Save and Cairo_Restore around Cairo_Clip as a more
   --  robust means of temporarily restricting the clip region.

   procedure Clip (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Establishes a new clip region by intersecting the current clip
   --  region with the current path as it would be filled by Cairo_Fill
   --  and according to the current fill rule (see Cairo_Set_Fill_Rule).
   --
   --  After Cairo_Clip, the current path will be cleared from the cairo
   --  context.
   --
   --  The current clip region affects all drawing operations by
   --  effectively masking out any changes to the surface that are outside
   --  the current clip region.
   --
   --  Calling Cairo_Clip can only make the clip region smaller, never
   --  larger. But the current clip is part of the graphics state, so a
   --  temporary restriction of the clip region can be achieved by
   --  calling Cairo_Clip within a Cairo_Save/cairo_Restore
   --  pair. The only other means of increasing the size of the clip
   --  region is Cairo_Reset_Clip.

   procedure Clip_Preserve (Cr : Cairo_Context);
   --  Cr: a cairo context
   --
   --  Establishes a new clip region by intersecting the current clip
   --  region with the current path as it would be filled by Cairo_Fill
   --  and according to the current fill rule (see Cairo_Set_Fill_Rule).
   --
   --  Unlike Cairo_Clip, Cairo_Clip_Preserve preserves the path within
   --  the cairo context.
   --
   --  The current clip region affects all drawing operations by
   --  effectively masking out any changes to the surface that are outside
   --  the current clip region.
   --
   --  Calling Cairo_Clip_Preserve can only make the clip region smaller, never
   --  larger. But the current clip is part of the graphics state, so a
   --  temporary restriction of the clip region can be achieved by
   --  calling Cairo_Clip_Preserve within a Cairo_Save/cairo_Restore
   --  pair. The only other means of increasing the size of the clip
   --  region is Cairo_Reset_Clip.

   procedure Clip_Extents
     (Cr : Cairo_Context;
      X1 : access Gdouble;
      Y1 : access Gdouble;
      X2 : access Gdouble;
      Y2 : access Gdouble);
   --  Cr: a cairo context
   --  X1: left of the resulting extents
   --  Y1: top of the resulting extents
   --  X2: right of the resulting extents
   --  Y2: bottom of the resulting extents
   --
   --  Computes a bounding box in user coordinates covering the area inside the
   --  current clip.
   --
   --  Since: 1.4

   --   Cairo_Rectangle:
   --   X: X coordinate of the left side of the rectangle
   --   Y: Y coordinate of the the top side of the rectangle
   --   Width: Width of the rectangle
   --   Height: Height of the rectangle
   --
   --   A data structure for holding a rectangle.
   --
   --   Since: 1.4
   --

   type Cairo_Rectangle is record
      X      : aliased Gdouble;
      Y      : aliased Gdouble;
      Width  : aliased Gdouble;
      Height : aliased Gdouble;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Rectangle);

   --   Cairo_Rectangle_List:
   --   Status: Error Status of the rectangle list
   --   Rectangles: Array containing the Rectangles
   --   Num_Rectangles: Number of rectangles in this list
   --
   --   A data structure for holding a dynamically allocated
   --   array of rectangles.
   --
   --   Since: 1.4
   --

   type Cairo_Rectangle_List is record
      Status         : aliased Cairo_Status;
      Rectangles     : access Cairo_Rectangle;
      Num_Rectangles : aliased Gint;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Rectangle_List);

   function Copy_Clip_Rectangle_List
     (Cr   : Cairo_Context)
      return access Cairo_Rectangle_List;
   --  Cr: a cairo context
   --
   --  Gets the current clip region as a list of rectangles in user
   --  coordinates.
   --  Never returns NULL.
   --
   --  The status in the list may be CAIRO_STATUS_CLIP_NOT_REPRESENTABLE to
   --  indicate that the clip region cannot be represented as a list of
   --  user-space rectangles. The status may have other values to indicate
   --  other errors.
   --
   --  Returns: the current clip region as a list of rectangles in user
   --  coordinates,
   --  which should be destroyed using Cairo_Rectangle_List_Destroy.
   --
   --  Since: 1.4

   procedure Rectangle_List_Destroy
     (Rectangle_List : access Cairo_Rectangle_List);

   --  Font/Text functions

   --   Cairo_Scaled_Font:
   --
   --   A Cairo_scaled_font is a font scaled to a particular size and device
   --   resolution. A Cairo_scaled_font is most useful for low-level font
   --   usage where a library or application wants to cache a reference
   --   to a scaled font to speed up the computation of metrics.
   --
   --   There are various types of scaled fonts, depending on the
   --   <firstterm>font backend</firstterm> they use. The type of a
   --   scaled font can be queried using Cairo.Scaled_Font.Get_Type.
   --
   --   Memory management of Cairo_scaled_font is done with
   --   Cairo.Scaled_Font.Reference and Cairo.Scaled_Font.Destroy.
   --

   type Cairo_Scaled_Font is private;

   --   Cairo_Font_Face:
   --
   --   A Cairo_font_face specifies all aspects of a font other
   --   than the size or font matrix (a font matrix is used to distort
   --   a font by sheering it or scaling it unequally in the two
   --   directions) . A font face can be set on a Cairo_Context by using
   --   Cairo_Set_Font_Face; the size and font matrix are set with
   --   Cairo_Set_Font_Size and Cairo_Set_Font_Matrix.
   --
   --   There are various types of font faces, depending on the
   --   <firstterm>font backend</firstterm> they use. The type of a
   --   font face can be queried using Cairo.Font_Face.Get_Type.
   --
   --   Memory management of Cairo_font_face is done with
   --   Cairo.Font_Face.Reference and Cairo.Font_Face.Destroy.
   --

   type Cairo_Font_Face is private;

   --   Cairo_Glyph:
   --   Index: glyph Index in the font. The exact interpretation of the
   --        glyph index depends on the font technology being used.
   --   X: the offset in the X direction between the origin used for
   --       drawing or measuring the string and the origin of this glyph.
   --   Y: the offset in the Y direction between the origin used for
   --       drawing or measuring the string and the origin of this glyph.
   --
   --   The Cairo_glyph structure holds information about a single glyph
   --   when drawing or measuring text. A font is (in simple terms) a
   --   collection of shapes used to draw text. A glyph is one of these
   --   shapes. There can be multiple glyphs for a single character
   --   (alternates to be used in different contexts, for example), or a
   --   glyph can be a <firstterm>ligature</firstterm> of multiple
   --   characters. Cairo doesn't expose any way of converting input text
   --   into glyphs, so in order to use the Cairo interfaces that take
   --   arrays of glyphs, you must directly access the appropriate
   --   underlying font system.
   --
   --   Note that the offsets given by x and y are not cumulative. When
   --   drawing or measuring text, each glyph is individually positioned
   --   with respect to the overall origin
   --

   type Cairo_Glyph is record
      Index : aliased Gulong;
      X     : aliased Gdouble;
      Y     : aliased Gdouble;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Glyph);

   --   Cairo_Text_Cluster:
   --   Num_Bytes: the number of bytes of UTF-8 text covered by cluster
   --   Num_Glyphs: the number of glyphs covered by cluster
   --
   --   The Cairo_text_cluster structure holds information about a single
   --   <firstterm>text cluster</firstterm>.  A text cluster is a minimal
   --   mapping of some glyphs corresponding to some UTF-8 text.
   --
   --   For a cluster to be valid, both num_bytes and num_glyphs should
   --   be non-negative, and at least one should be non-zero.
   --   Note that clusters with zero glyphs are not as well supported as
   --   normal clusters.  For example, PDF rendering applications typically
   --   ignore those clusters when PDF text is being selected.
   --
   --   See Cairo_Show_Text_Glyphs for how clusters are used in advanced
   --   text operations.
   --
   --   Since: 1.8
   --

   type Cairo_Text_Cluster is record
      Num_Bytes  : aliased Gint;
      Num_Glyphs : aliased Gint;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Text_Cluster);

   --   Cairo_Text_Cluster_Flags:
   --   CAIRO_TEXT_CLUSTER_FLAG_BACKWARD: The clusters in the cluster array
   --   map to glyphs in the glyph array from end to start.
   --
   --   Specifies properties of a text cluster mapping.
   --
   --   Since: 1.8
   --

   subtype Cairo_Text_Cluster_Flags is Guint;
   Cairo_Text_Cluster_Flag_Backward : constant Cairo_Text_Cluster_Flags := 1;

   --   Cairo_Text_Extents:
   --   X_Bearing: the horizontal distance from the origin to the
   --     leftmost part of the glyphs as drawn. Positive if the
   --     glyphs lie entirely to the right of the origin.
   --   Y_Bearing: the vertical distance from the origin to the
   --     topmost part of the glyphs as drawn. Positive only if the
   --     glyphs lie completely below the origin; will usually be
   --     negative.
   --   Width: Width of the glyphs as drawn
   --   Height: Height of the glyphs as drawn
   --   X_Advance:distance to advance in the X direction
   --      after drawing these glyphs
   --   Y_Advance: distance to advance in the Y direction
   --     after drawing these glyphs. Will typically be zero except
   --     for vertical text layout as found in East-Asian languages.
   --
   --   The Cairo_text_extents structure stores the extents of a single
   --   glyph or a string of glyphs in user-space coordinates. Because text
   --   extents are in user-space coordinates, they are mostly, but not
   --   entirely, independent of the current transformation matrix. If you call
   --   <literal>Cairo_Scale(cr, 2.0, 2.0)</literal>, text will
   --   be drawn twice as big, but the reported text extents will not be
   --   doubled. They will change slightly due to hinting (so you can't
   --   assume that metrics are independent of the transformation matrix),
   --   but otherwise will remain unchanged.
   --

   type Cairo_Text_Extents is record
      X_Bearing : aliased Gdouble;
      Y_Bearing : aliased Gdouble;
      Width     : aliased Gdouble;
      Height    : aliased Gdouble;
      X_Advance : aliased Gdouble;
      Y_Advance : aliased Gdouble;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Text_Extents);

   --   Cairo_Font_Extents:
   --   Ascent: the distance that the font extends above the baseline.
   --            Note that this is not always exactly equal to the maximum
   --            of the extents of all the glyphs in the font, but rather
   --            is picked to express the font designer's intent as to
   --            how the font should align with elements above it.
   --   Descent: the distance that the font extends below the baseline.
   --             This value is positive for typical fonts that include
   --             portions below the baseline. Note that this is not always
   --             exactly equal to the maximum of the extents of all the
   --             glyphs in the font, but rather is picked to express the
   --             font designer's intent as to how the the font should
   --             align with elements below it.
   --   Height: the recommended vertical distance between baselines when
   --            setting consecutive lines of text with the font. This
   --            is greater than ascent+descent by a
   --            quantity known as the <firstterm>line spacing</firstterm>
   --            or <firstterm>external leading</firstterm>. When space
   --            is at a premium, most fonts can be set with only
   --            a distance of ascent+descent between lines.
   --   Max_X_Advance: the maximum distance in the X direction that
   --           the the origin is advanced for any glyph in the font.
   --   Max_Y_Advance: the maximum distance in the Y direction that
   --           the the origin is advanced for any glyph in the font.
   --           this will be zero for normal fonts used for horizontal
   --           writing. (The scripts of East Asia are sometimes written
   --           vertically.)
   --
   --   The Cairo_font_extents structure stores metric information for
   --   a font. Values are given in the current user-space coordinate
   --   system.
   --
   --   Because font metrics are in user-space coordinates, they are
   --   mostly, but not entirely, independent of the current transformation
   --   matrix. If you call <literal>Cairo_Scale(cr, 2.0, 2.0)</literal>,
   --   text will be drawn twice as big, but the reported text extents will
   --   not be doubled. They will change slightly due to hinting (so you
   --   can't assume that metrics are independent of the transformation
   --   matrix), but otherwise will remain unchanged.
   --

   type Cairo_Font_Extents is record
      Ascent        : aliased Gdouble;
      Descent       : aliased Gdouble;
      Height        : aliased Gdouble;
      Max_X_Advance : aliased Gdouble;
      Max_Y_Advance : aliased Gdouble;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Font_Extents);

   --   Cairo_Font_Slant:
   --   CAIRO_FONT_SLANT_NORMAL: Upright font style
   --   CAIRO_FONT_SLANT_ITALIC: Italic font style
   --   CAIRO_FONT_SLANT_OBLIQUE: Oblique font style
   --
   --   Specifies variants of a font face based on their slant.
   --

   type Cairo_Font_Slant is (
      Cairo_Font_Slant_Normal,
      Cairo_Font_Slant_Italic,
      Cairo_Font_Slant_Oblique);
   pragma Convention (C, Cairo_Font_Slant);

   --   Cairo_Font_Weight:
   --   CAIRO_FONT_WEIGHT_NORMAL: Normal font weight
   --   CAIRO_FONT_WEIGHT_BOLD: Bold font weight
   --
   --   Specifies variants of a font face based on their weight.
   --

   type Cairo_Font_Weight is (
      Cairo_Font_Weight_Normal,
      Cairo_Font_Weight_Bold);
   pragma Convention (C, Cairo_Font_Weight);

   --   Cairo_Subpixel_Order:
   --   CAIRO_SUBPIXEL_ORDER_DEFAULT: Use the default subpixel order for
   --     for the target device
   --   CAIRO_SUBPIXEL_ORDER_RGB: Subpixel elements are arranged horizontally
   --     with red at the left
   --   CAIRO_SUBPIXEL_ORDER_BGR:  Subpixel elements are arranged horizontally
   --     with blue at the left
   --   CAIRO_SUBPIXEL_ORDER_VRGB: Subpixel elements are arranged vertically
   --     with red at the top
   --   CAIRO_SUBPIXEL_ORDER_VBGR: Subpixel elements are arranged vertically
   --     with blue at the top
   --
   --   The subpixel order specifies the order of color elements within
   --   each pixel on the display device when rendering with an
   --   antialiasing mode of CAIRO_ANTIALIAS_SUBPIXEL.
   --

   type Cairo_Subpixel_Order is (
      Cairo_Subpixel_Order_Default,
      Cairo_Subpixel_Order_Rgb,
      Cairo_Subpixel_Order_Bgr,
      Cairo_Subpixel_Order_Vrgb,
      Cairo_Subpixel_Order_Vbgr);
   pragma Convention (C, Cairo_Subpixel_Order);

   --   Cairo_Hint_Style:
   --   CAIRO_HINT_STYLE_DEFAULT: Use the default hint style for
   --     font backend and target device
   --   CAIRO_HINT_STYLE_NONE: Do not hint outlines
   --   CAIRO_HINT_STYLE_SLIGHT: Hint outlines slightly to improve
   --     contrast while retaining good fidelity to the original
   --     shapes.
   --   CAIRO_HINT_STYLE_MEDIUM: Hint outlines with medium strength
   --     giving a compromise between fidelity to the original shapes
   --     and contrast
   --   CAIRO_HINT_STYLE_FULL: Hint outlines to maximize contrast
   --
   --   Specifies the type of hinting to do on font outlines. Hinting
   --   is the process of fitting outlines to the pixel grid in order
   --   to improve the appearance of the result. Since hinting outlines
   --   involves distorting them, it also reduces the faithfulness
   --   to the original outline shapes. Not all of the outline hinting
   --   styles are supported by all font backends.
   --
   --   New entries may be added in future versions.
   --

   type Cairo_Hint_Style is (
      Cairo_Hint_Style_Default,
      Cairo_Hint_Style_None,
      Cairo_Hint_Style_Slight,
      Cairo_Hint_Style_Medium,
      Cairo_Hint_Style_Full);
   pragma Convention (C, Cairo_Hint_Style);

   --   Cairo_Hint_Metrics:
   --   CAIRO_HINT_METRICS_DEFAULT: Hint metrics in the default
   --    manner for the font backend and target device
   --   CAIRO_HINT_METRICS_OFF: Do not hint font metrics
   --   CAIRO_HINT_METRICS_ON: Hint font metrics
   --
   --   Specifies whether to hint font metrics; hinting font metrics
   --   means quantizing them so that they are integer values in
   --   device space. Doing this improves the consistency of
   --   letter and line spacing, however it also means that text
   --   will be laid out differently at different zoom factors.
   --

   type Cairo_Hint_Metrics is (
      Cairo_Hint_Metrics_Default,
      Cairo_Hint_Metrics_Off,
      Cairo_Hint_Metrics_On);
   pragma Convention (C, Cairo_Hint_Metrics);

   --   Cairo_Font_Options:
   --
   --   An opaque structure holding all options that are used when
   --   rendering fonts.
   --
   --   Individual features of a Cairo_font_options can be set or
   --   accessed using functions named
   --   Cairo.Font_Options.Set_<emphasis>feature_Name</emphasis> and
   --   Cairo.Font_Options.Get_<emphasis>feature_Name</emphasis>, like
   --   Cairo.Font_Options.Set_Antialias and
   --   Cairo.Font_Options.Get_Antialias.
   --
   --   New features may be added to a Cairo_font_options in the
   --   future.  For this reason, Cairo.Font_Options.Copy,
   --   Cairo.Font_Options.Equal, Cairo.Font_Options.Merge, and
   --   Cairo.Font_Options.Hash should be used to copy, check
   --   for equality, merge, or compute a hash value of
   --   Cairo_font_options objects.
   --

   type Cairo_Font_Options is private;

   --  This interface is for dealing with text as text, not caring about the
   --   font object inside the the Cairo_Context.

   procedure Select_Font_Face
     (Cr     : Cairo_Context;
      Family : Interfaces.C.Strings.chars_ptr;
      Slant  : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight);
   --  Cr: a Cairo_Context
   --  Family: a font Family name, encoded in UTF-8
   --  Slant: the Slant for the font
   --  Weight: the Weight for the font
   --
   --  Note: The Cairo_Select_Font_Face function call is part of what
   --  the cairo designers call the "toy" text API. It is convenient for
   --  short demos and simple programs, but it is not expected to be
   --  adequate for serious text-using applications.
   --
   --  Selects a family and style of font from a simplified description as
   --  a family name, slant and weight. Cairo provides no operation to
   --  list available family names on the system (this is a "toy",
   --  remember), but the standard CSS2 generic family names, ("serif",
   --  "sans-serif", "cursive", "fantasy", "monospace"), are likely to
   --  work as expected.
   --
   --  For "real" font selection, see the font-backend-specific
   --  font_face_create functions for the font backend you are using. (For
   --  example, if you are using the freetype-based cairo-ft font backend,
   --  see Cairo_Ft_Font_Face_Create_For_Ft_Face or
   --  Cairo_Ft_Font_Face_Create_For_Pattern.) The resulting font face
   --  could then be used with Cairo.Scaled_Font.Create and
   --  Cairo_Set_Scaled_Font.
   --
   --  Similarly, when using the "real" font support, you can call
   --  directly into the underlying font system, (such as fontconfig or
   --  freetype), for operations such as listing available fonts, etc.
   --
   --  It is expected that most applications will need to use a more
   --  comprehensive font handling and text layout library, (for example,
   --  pango), in conjunction with cairo.
   --
   --  If text is drawn without a call to Cairo_Select_Font_Face, (nor
   --  Cairo_Set_Font_Face nor Cairo_Set_Scaled_Font), the default
   --  family is platform-specific, but is essentially "sans-serif".
   --  Default slant is CAIRO_FONT_SLANT_NORMAL, and default weight is
   --  CAIRO_FONT_WEIGHT_NORMAL.
   --
   --  This function is equivalent to a call to Cairo_Toy_Font_Face_Create
   --  followed by Cairo_Set_Font_Face.

   procedure Set_Font_Size (Cr : Cairo_Context; Size : Gdouble);
   --  Cr: a Cairo_Context
   --  Size: the new font Size, in user space units
   --
   --  Sets the current font matrix to a scale by a factor of size, replacing
   --  any font matrix previously set with Cairo_Set_Font_Size or
   --  Cairo_Set_Font_Matrix. This results in a font size of size user space
   --  units. (More precisely, this matrix will result in the font's
   --  em-square being a size by size square in user space.)
   --
   --  If text is drawn without a call to Cairo_Set_Font_Size, (nor
   --  Cairo_Set_Font_Matrix nor Cairo_Set_Scaled_Font), the default
   --  font size is 10.0.

   procedure Set_Font_Matrix
     (Cr     : Cairo_Context;
      Matrix : access constant Cairo_Matrix);
   --  Cr: a Cairo_Context
   --  Matrix: a Cairo_Matrix describing a transform to be applied to
   --  the current font.
   --
   --  Sets the current font matrix to matrix. The font matrix gives a
   --  transformation from the design space of the font (in this space,
   --  the em-square is 1 unit by 1 unit) to user space. Normally, a
   --  simple scale is used (see Cairo_Set_Font_Size), but a more
   --  complex font matrix can be used to shear the font
   --  or stretch it unequally along the two axes

   procedure Get_Font_Matrix
     (Cr     : Cairo_Context;
      Matrix : access Cairo_Matrix);
   --  Cr: a Cairo_Context
   --  Matrix: return value for the Matrix
   --
   --  Stores the current font matrix into matrix. See
   --  Cairo_Set_Font_Matrix.

   procedure Set_Font_Options
     (Cr      : Cairo_Context;
      Options : access constant Cairo_Font_Options);
   --  Cr: a Cairo_Context
   --  Options: font Options to use
   --
   --  Sets a set of custom font rendering options for the Cairo_Context.
   --  Rendering options are derived by merging these options with the
   --  options derived from underlying surface; if the value in options
   --  has a default value (like CAIRO_ANTIALIAS_DEFAULT), then the value
   --  from the surface is used.

   procedure Get_Font_Options
     (Cr      : Cairo_Context;
      Options : Cairo_Font_Options);
   --  Cr: a Cairo_Context
   --  Options: a Cairo_Font_Options object into which to store
   --    the retrieved options. All existing values are overwritten
   --
   --  Retrieves font rendering options set via Cairo_Set_Font_Options.
   --  Note that the returned options do not include any options derived
   --  from the underlying surface; they are literally the options
   --  passed to Cairo_Set_Font_Options.

   procedure Set_Font_Face
     (Cr        : Cairo_Context;
      Font_Face : Cairo_Font_Face);
   --  Cr: a Cairo_Context
   --  Font_Face: a Cairo.Font_Face.T, or NULL to restore to the default font
   --
   --  Replaces the current Cairo_Font_Face object in the Cairo_Context with
   --  font_face. The replaced font face in the Cairo_Context will be
   --  destroyed if there are no other references to it.

   function Get_Font_Face (Cr : Cairo_Context) return Cairo_Font_Face;
   --  Cr: a Cairo_Context
   --
   --  Gets the current font face for a Cairo_Context.
   --
   --  Return value: the current font face.  This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Cairo.Font_Face.Reference.
   --
   --  This function never returns NULL. If memory cannot be allocated, a
   --  special "nil" Cairo_Font_Face object will be returned on which
   --  Cairo.Font_Face.Status returns CAIRO_STATUS_NO_MEMORY. Using
   --  this nil object will cause its error state to propagate to other
   --  objects it is passed to, (for example, calling
   --  Cairo_Set_Font_Face with a nil font will trigger an error that
   --  will shutdown the Cairo_Context object).

   procedure Set_Scaled_Font
     (Cr          : Cairo_Context;
      Scaled_Font : access constant Cairo_Scaled_Font);
   --  Cr: a Cairo_Context
   --  Scaled_Font: a Cairo_Scaled_Font
   --
   --  Replaces the current font face, font matrix, and font options in
   --  the Cairo_Context with those of the Cairo_Scaled_Font.  Except for
   --  some translation, the current CTM of the Cairo_Context should be the
   --  same as that of the Cairo.Scaled_Font.T, which can be accessed
   --  using Cairo.Scaled_Font.Get_Ctm.
   --
   --  Since: 1.2

   function Get_Scaled_Font (Cr : Cairo_Context) return Cairo_Scaled_Font;
   --  Cr: a Cairo_Context
   --
   --  Gets the current scaled font for a Cairo_Context.
   --
   --  Return value: the current scaled font. This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Cairo.Scaled_Font.Reference.
   --
   --  This function never returns NULL. If memory cannot be allocated, a
   --  special "nil" Cairo_Scaled_Font object will be returned on which
   --  Cairo.Scaled_Font.Status returns CAIRO_STATUS_NO_MEMORY. Using
   --  this nil object will cause its error state to propagate to other
   --  objects it is passed to, (for example, calling
   --  Cairo_Set_Scaled_Font with a nil font will trigger an error that
   --  will shutdown the Cairo_Context object).
   --
   --  Since: 1.4

   procedure Show_Text
     (Cr   : Cairo_Context;
      Utf8 : Interfaces.C.Strings.chars_ptr);
   --  Cr: a cairo context
   --  Utf8: a NUL-terminated string of text encoded in UTF-8, or NULL
   --
   --  A drawing operator that generates the shape from a string of UTF-8
   --  characters, rendered according to the current font_face, font_size
   --  (font_matrix), and font_options.
   --
   --  This function first computes a set of glyphs for the string of
   --  text. The first glyph is placed so that its origin is at the
   --  current point. The origin of each subsequent glyph is offset from
   --  that of the previous glyph by the advance values of the previous
   --  glyph.
   --
   --  After this call the current point is moved to the origin of where
   --  the next glyph would be placed in this same progression. That is,
   --  the current point will be at the origin of the final glyph offset
   --  by its advance values. This allows for easy display of a single
   --  logical string with multiple calls to Cairo_Show_Text.
   --
   --  Note: The Cairo_Show_Text function call is part of what the cairo
   --  designers call the "toy" text API. It is convenient for short demos
   --  and simple programs, but it is not expected to be adequate for
   --  serious text-using applications. See Cairo_Show_Glyphs for the
   --  "real" text display API in cairo.

   procedure Show_Glyphs
     (Cr         : Cairo_Context;
      Glyphs     : access constant Cairo_Glyph;
      Num_Glyphs : Gint);
   --  Cr: a cairo context
   --  Glyphs: array of Glyphs to show
   --  Num_Glyphs: number of glyphs to show
   --
   --  A drawing operator that generates the shape from an array of glyphs,
   --  rendered according to the current font face, font size
   --  (font matrix), and font options.

   procedure Show_Text_Glyphs
     (Cr            : Cairo_Context;
      Utf8          : Interfaces.C.Strings.chars_ptr;
      Utf8_Len      : Gint;
      Glyphs        : access constant Cairo_Glyph;
      Num_Glyphs    : Gint;
      Clusters      : access constant Cairo_Text_Cluster;
      Num_Clusters  : Gint;
      Cluster_Flags : Cairo_Text_Cluster_Flags);
   --  Cr: a cairo context
   --  Utf8: a string of text encoded in UTF-8
   --  Utf8_Len: length of utf8 in bytes, or -1 if it is NUL-terminated
   --  Glyphs: array of Glyphs to show
   --  Num_Glyphs: number of glyphs to show
   --  Clusters: array of cluster mapping information
   --  Num_Clusters: number of clusters in the mapping
   --  Cluster_Flags: cluster mapping flags
   --
   --  This operation has rendering effects similar to Cairo_Show_Glyphs
   --  but, if the target surface supports it, uses the provided text and
   --  cluster mapping to embed the text for the glyphs shown in the output.
   --  If the target does not support the extended attributes, this function
   --  acts like the basic Cairo_Show_Glyphs as if it had been passed
   --  glyphs and num_glyphs.
   --
   --  The mapping between utf8 and glyphs is provided by an array of
   --  <firstterm>clusters</firstterm>.  Each cluster covers a number of
   --  text bytes and glyphs, and neighboring clusters cover neighboring
   --  areas of utf8 and glyphs.  The clusters should collectively cover utf8
   --  and glyphs in entirety.
   --
   --  The first cluster always covers bytes from the beginning of utf8.
   --  If cluster_flags do not have the CAIRO_TEXT_CLUSTER_FLAG_BACKWARD
   --  set, the first cluster also covers the beginning
   --  of glyphs, otherwise it covers the end of the glyphs array and
   --  following clusters move backward.
   --
   --  See Cairo_Text_Cluster for constraints on valid clusters.
   --
   --  Since: 1.8

   procedure Text_Path
     (Cr   : Cairo_Context;
      Utf8 : Interfaces.C.Strings.chars_ptr);
   --  Cr: a cairo context
   --  Utf8: a NUL-terminated string of text encoded in UTF-8, or NULL
   --
   --  Adds closed paths for text to the current path.  The generated
   --  path if filled, achieves an effect similar to that of
   --  Cairo_Show_Text.
   --
   --  Text conversion and positioning is done similar to Cairo_Show_Text.
   --
   --  Like Cairo_Show_Text, After this call the current point is
   --  moved to the origin of where the next glyph would be placed in
   --  this same progression.  That is, the current point will be at
   --  the origin of the final glyph offset by its advance values.
   --  This allows for chaining multiple calls to to Cairo_Text_Path
   --  without having to set current point in between.
   --
   --  Note: The Cairo_Text_Path function call is part of what the cairo
   --  designers call the "toy" text API. It is convenient for short demos
   --  and simple programs, but it is not expected to be adequate for
   --  serious text-using applications. See Cairo_Glyph_Path for the
   --  "real" text path API in cairo.

   procedure Glyph_Path
     (Cr         : Cairo_Context;
      Glyphs     : access constant Cairo_Glyph;
      Num_Glyphs : Gint);
   --  Cr: a cairo context
   --  Glyphs: array of Glyphs to show
   --  Num_Glyphs: number of glyphs to show
   --
   --  Adds closed paths for the glyphs to the current path.  The generated
   --  path if filled, achieves an effect similar to that of
   --  Cairo_Show_Glyphs.

   procedure Text_Extents
     (Cr      : Cairo_Context;
      Utf8    : Interfaces.C.Strings.chars_ptr;
      Extents : Cairo_Text_Extents);
   --  Cr: a Cairo_Context
   --  Utf8: a NUL-terminated string of text encoded in UTF-8, or NULL
   --  Extents: a Cairo_Text_Extents object into which the results
   --  will be stored
   --
   --  Gets the extents for a string of text. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the text,
   --  (as it would be drawn by Cairo_Show_Text). Additionally, the
   --  x_advance and y_advance values indicate the amount by which the
   --  current point would be advanced by Cairo_Show_Text.
   --
   --  Note that whitespace characters do not directly contribute to the
   --  size of the rectangle (extents.width and extents.height). They do
   --  contribute indirectly by changing the position of non-whitespace
   --  characters. In particular, trailing whitespace characters are
   --  likely to not affect the size of the rectangle, though they will
   --  affect the x_advance and y_advance values.

   procedure Glyph_Extents
     (Cr         : Cairo_Context;
      Glyphs     : access constant Cairo_Glyph;
      Num_Glyphs : Gint;
      Extents    : Cairo_Text_Extents);
   --  Cr: a Cairo_Context
   --  Glyphs: an array of Cairo_Glyph objects
   --  Num_Glyphs: the number of elements in glyphs
   --  Extents: a Cairo_Text_Extents object into which the results
   --  will be stored
   --
   --  Gets the extents for an array of glyphs. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the
   --  glyphs, (as they would be drawn by Cairo_Show_Glyphs).
   --  Additionally, the x_advance and y_advance values indicate the
   --  amount by which the current point would be advanced by
   --  Cairo_Show_Glyphs.
   --
   --  Note that whitespace glyphs do not contribute to the size of the
   --  rectangle (extents.width and extents.height).

   procedure Font_Extents
     (Cr      : Cairo_Context;
      Extents : access Cairo_Font_Extents);
   --  Cr: a Cairo_Context
   --  Extents: a Cairo_Font_Extents object into which the results
   --  will be stored.
   --
   --  Gets the font extents for the currently selected font.

   --  Generic identifier for a font style

   --   Cairo_Font_Type:
   --   CAIRO_FONT_TYPE_TOY: The font was created using cairo's toy font api
   --   CAIRO_FONT_TYPE_FT: The font is of type FreeType
   --   CAIRO_FONT_TYPE_WIN32: The font is of type Win32
   --   CAIRO_FONT_TYPE_QUARTZ: The font is of type Quartz (Since: 1.6)
   --   CAIRO_FONT_TYPE_USER: The font was create using cairo's user font api
   --   Since: 1.8)
   --
   --   Cairo_font_type is used to describe the type of a given font
   --   face or scaled font. The font types are also known as "font
   --   backends" within cairo.
   --
   --   The type of a font face is determined by the function used to
   --   create it, which will generally be of the form
   --   Cairo_<emphasis>type</emphasis>_Font_Face_Create. The font face type
   --   can be queried
   --   with Cairo.Font_Face.Get_Type
   --
   --   The various Cairo_font_face functions can be used with a font face
   --   of any type.
   --
   --   The type of a scaled font is determined by the type of the font
   --   face passed to Cairo.Scaled_Font.Create. The scaled font type can
   --   be queried with Cairo.Scaled_Font.Get_Type
   --
   --   The various Cairo_scaled_font functions can be used with scaled
   --   fonts of any type, but some font backends also provide
   --   type-specific functions that must only be called with a scaled font
   --   of the appropriate type. These functions have names that begin with
   --   Cairo_<emphasis>type</emphasis>_Scaled_Font such as
   --   Cairo_Ft_Scaled_Font_Lock_Face.
   --
   --   The behavior of calling a type-specific function with a scaled font
   --   of the wrong type is undefined.
   --
   --   New entries may be added in future versions.
   --
   --   Since: 1.2
   --

   type Cairo_Font_Type is (
      Cairo_Font_Type_Toy,
      Cairo_Font_Type_Ft,
      Cairo_Font_Type_Win32,
      Cairo_Font_Type_Quartz,
      Cairo_Font_Type_User);
   pragma Convention (C, Cairo_Font_Type);

   --  Portable interface to general font features.

   --  Toy fonts

   --  User fonts

   --  User-font method signatures

   --   Cairo_User_Scaled_Font_Init_Func:
   --   Scaled_Font: the scaled-font being created
   --   Cr: a cairo context, in font space
   --   Extents: font Extents to fill in, in font space
   --
   --   Cairo_user_scaled_font_init_func is the type of function which is
   --   called when a scaled-font needs to be created for a user font-face.
   --
   --   The cairo context cr is not used by the caller, but is prepared in font
   --   space, similar to what the cairo contexts passed to the render_glyph
   --   method will look like.  The callback can use this context for extents
   --   computation for example.  After the callback is called, cr is checked
   --   for any error status.
   --
   --   The extents argument is where the user font sets the font extents for
   --   scaled_font.  It is in font space, which means that for most cases its
   --   ascent and descent members should add to 1.0.  extents is preset to
   --   hold a value of 1.0 for ascent, height, and max_x_advance, and 0.0 for
   --   descent and max_y_advance members.
   --
   --   The callback is optional.  If not set, default font extents as
   --   described
   --   in the previous paragraph will be used.
   --
   --   Note that scaled_font is not fully initialized at this
   --   point and trying to use it for text operations in the callback will
   --   result
   --   in deadlock.
   --
   --   Returns: CAIRO_STATUS_SUCCESS upon success, or
   --   CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
   --
   --   Since: 1.8
   --

   type Cairo_User_Scaled_Font_Init_Func is access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Cairo_Context;
      Arg3 : access Cairo_Font_Extents)
      return Cairo_Status;

   --   Cairo_User_Scaled_Font_Render_Glyph_Func:
   --   Scaled_Font: user scaled-font
   --   Glyph: Glyph code to render
   --   Cr: cairo context to draw to, in font space
   --   Extents: glyph Extents to fill in, in font space
   --
   --   Cairo_user_scaled_font_render_glyph_func is the type of function which
   --   is called when a user scaled-font needs to render a glyph.
   --
   --   The callback is mandatory, and expected to draw the glyph with code
   --   glyph to
   --   the cairo context cr.  cr is prepared such that the glyph drawing is
   --   done in
   --   font space.  That is, the matrix set on cr is the scale matrix of
   --   scaled_font,
   --   The extents argument is where the user font sets the font extents for
   --   scaled_font.  However, if user prefers to draw in user space, they can
   --   achieve that by changing the matrix on cr.  All cairo rendering
   --   operations
   --   to cr are permitted, however, the result is undefined if any source
   --   other
   --   than the default source on cr is used.  That means, glyph bitmaps
   --   should
   --   be rendered using Cairo_Mask instead of Cairo_Paint.
   --
   --   Other non-default settings on cr include a font size of 1.0 (given that
   --   it is set up to be in font space), and font options corresponding to
   --   scaled_font.
   --
   --   The extents argument is preset to have <literal>x_bearing</literal>,
   --   <literal>width</literal>, and <literal>y_advance</literal> of zero,
   --   <literal>y_bearing</literal> set to
   --   literal>-font_extents.ascent</literal>,
   --   <literal>height</literal> to
   --   literal>font_extents.ascent+font_extents.descent</literal>,
   --   and <literal>x_advance</literal> to
   --   literal>font_extents.max_x_advance</literal>.
   --   The only field user needs to set in majority of cases is
   --   <literal>x_advance</literal>.
   --   If the <literal>width</literal> field is zero upon the callback
   --   returning
   --   (which is its preset value), the glyph extents are automatically
   --   computed
   --   based on the drawings done to cr.  This is in most cases exactly what
   --   the
   --   desired behavior is.  However, if for any reason the callback sets the
   --   extents, it must be ink extents, and include the extents of all drawing
   --   done to cr in the callback.
   --
   --   Returns: CAIRO_STATUS_SUCCESS upon success, or
   --   CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
   --
   --   Since: 1.8
   --

   type Cairo_User_Scaled_Font_Render_Glyph_Func is access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Gulong;
      Arg3 : Cairo_Context;
      Arg4 : Cairo_Text_Extents)
      return Cairo_Status;

   --   Cairo_User_Scaled_Font_Text_To_Glyphs_Func:
   --   Scaled_Font: the scaled-font being created
   --   Utf8: a string of text encoded in UTF-8
   --   Utf8_Len: length of utf8 in bytes
   --   Glyphs: pointer to array of Glyphs to fill, in font space
   --   Num_Glyphs: pointer to number of glyphs
   --   Clusters: pointer to array of cluster mapping information to fill, or
   --   NULL
   --   Num_Clusters: pointer to number of clusters
   --   Cluster_Flags: pointer to location to store cluster flags
   --   corresponding to the
   --                   output clusters
   --
   --   Cairo_user_scaled_font_text_to_glyphs_func is the type of function
   --   which
   --   is called to convert input text to an array of glyphs.  This is used
   --   by the
   --   Cairo_Show_Text operation.
   --
   --   Using this callback the user-font has full control on glyphs and their
   --   positions.  That means, it allows for features like ligatures and
   --   kerning,
   --   as well as complex <firstterm>shaping</firstterm> required for scripts
   --   like
   --   Arabic and Indic.
   --
   --   The num_glyphs argument is preset to the number of glyph entries
   --   available
   --   in the glyphs buffer. If the glyphs buffer is NULL, the value of
   --   num_glyphs will be zero.  If the provided glyph array is too short for
   --   the conversion (or for convenience), a new glyph array may be allocated
   --   using Cairo_Glyph_Allocate and placed in glyphs.  Upon return,
   --   num_glyphs should contain the number of generated glyphs.  If the value
   --   glyphs points at has changed after the call, the caller will free the
   --   allocated glyph array using Cairo_Glyph_Free.
   --   The callback should populate the glyph indices and positions (in font
   --   space)
   --   assuming that the text is to be shown at the origin.
   --
   --   If clusters is not NULL, num_clusters and cluster_flags are also
   --   non-NULL, and cluster mapping should be computed. The semantics of how
   --   cluster array allocation works is similar to the glyph array.  That is,
   --   if clusters initially points to a non-NULL value, that array may be
   --   used
   --   as a cluster buffer, and num_clusters points to the number of cluster
   --   entries available there.  If the provided cluster array is too short
   --   for
   --   the conversion (or for convenience), a new cluster array may be
   --   allocated
   --   using Cairo_Text_Cluster_Allocate and placed in clusters.  Upon return,
   --   num_clusters should contain the number of generated clusters.
   --   If the value clusters points at has changed after the call, the caller
   --   will free the allocated cluster array using Cairo_Text_Cluster_Free.
   --
   --   The callback is optional.  If num_glyphs is negative upon
   --   the callback returning, the unicode_to_glyph callback
   --   is tried.  See Cairo_user_scaled_font_unicode_to_glyph_func.
   --
   --   Note: While cairo does not impose any limitation on glyph indices,
   --   some applications may assume that a glyph index fits in a 16-bit
   --   Guint integer.  As such, it is advised that user-fonts keep their
   --   glyphs in the 0 to 65535 range.  Furthermore, some applications may
   --   assume that glyph 0 is a special glyph-not-found glyph.  User-fonts
   --   are advised to use glyph 0 for such purposes and do not use that
   --   glyph value for other purposes.
   --
   --   Returns: CAIRO_STATUS_SUCCESS upon success, or
   --   CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
   --
   --   Since: 1.8
   --

   type Cairo_User_Scaled_Font_Text_To_Glyphs_Func is access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Interfaces.C.Strings.chars_ptr;
      Arg3 : Gint;
      Arg4 : System.Address;
      Arg5 : access Gint;
      Arg6 : System.Address;
      Arg7 : access Gint;
      Arg8 : Cairo_Text_Cluster_Flags)
      return Cairo_Status;

   --   Cairo_User_Scaled_Font_Unicode_To_Glyph_Func:
   --   Scaled_Font: the scaled-font being created
   --   Unicode: input Unicode character code-point
   --   Glyph_Index: output glyph index
   --
   --   Cairo_user_scaled_font_unicode_to_glyph_func is the type of function
   --   which
   --   is called to convert an input Unicode character to a single glyph.
   --   This is used by the Cairo_Show_Text operation.
   --
   --   This callback is used to provide the same functionality as the
   --   text_to_glyphs callback does (see
   --   Cairo_user_scaled_font_text_to_glyphs_func)
   --   but has much less control on the output,
   --   in exchange for increased ease of use.  The inherent assumption to
   --   using
   --   this callback is that each character maps to one glyph, and that the
   --   mapping is context independent.  It also assumes that glyphs are
   --   positioned
   --   according to their advance width.  These mean no ligatures, kerning, or
   --   complex scripts can be implemented using this callback.
   --
   --   The callback is optional, and only used if text_to_glyphs callback is
   --   not
   --   set or fails to return glyphs.  If this callback is not set, an
   --   identity
   --   mapping from Unicode code-points to glyph indices is assumed.
   --
   --   Note: While cairo does not impose any limitation on glyph indices,
   --   some applications may assume that a glyph index fits in a 16-bit
   --   Guint integer.  As such, it is advised that user-fonts keep their
   --   glyphs in the 0 to 65535 range.  Furthermore, some applications may
   --   assume that glyph 0 is a special glyph-not-found glyph.  User-fonts
   --   are advised to use glyph 0 for such purposes and do not use that
   --   glyph value for other purposes.
   --
   --   Returns: CAIRO_STATUS_SUCCESS upon success, or
   --   CAIRO_STATUS_USER_FONT_ERROR or any other error status on error.
   --
   --   Since: 1.8
   --

   type Cairo_User_Scaled_Font_Unicode_To_Glyph_Func is access function
     (Arg1 : Cairo_Scaled_Font;
      Arg2 : Gulong;
      Arg3 : access Gulong)
      return Cairo_Status;

   --  User-font method setters

   --  User-font method getters

   --  Query functions
   function Get_Operator (Cr : Cairo_Context) return Cairo_Operator;
   --  Cr: a cairo context
   --
   --  Gets the current compositing operator for a cairo context.
   --
   --  Return value: the current compositing operator.

   function Get_Source (Cr : Cairo_Context) return Cairo_Pattern;
   --  Cr: a cairo context
   --
   --  Gets the current source pattern for cr.
   --
   --  Return value: the current source pattern. This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Cairo.Pattern.Reference.

   function Get_Tolerance (Cr : Cairo_Context) return Gdouble;
   --  Cr: a cairo context
   --
   --  Gets the current tolerance value, as set by Cairo_Set_Tolerance.
   --
   --  Return value: the current tolerance value.

   function Get_Antialias (Cr : Cairo_Context) return Cairo_Antialias;
   --  Cr: a cairo context
   --
   --  Gets the current shape antialiasing mode, as set by Cairo_Set_Antialias.
   --
   --  Return value: the current shape antialiasing mode.

   function Has_Current_Point (Cr : Cairo_Context) return Cairo_Bool;
   --  Cr: a cairo context
   --
   --  Returns whether a current point is defined on the current path.
   --  See Cairo_Get_Current_Point for details on the current point.
   --
   --  Return value: whether a current point is defined.
   --
   --  Since: 1.6

   procedure Get_Current_Point
     (Cr : Cairo_Context;
      X  : access Gdouble;
      Y  : access Gdouble);
   --  Cr: a cairo context
   --  X: return value for X coordinate of the current point
   --  Y: return value for Y coordinate of the current point
   --
   --  Gets the current point of the current path, which is
   --  conceptually the final point reached by the path so far.
   --
   --  The current point is returned in the user-space coordinate
   --  system. If there is no defined current point or if cr is in an
   --  error status, x and y will both be set to 0.0. It is possible to
   --  check this in advance with Cairo_Has_Current_Point.
   --
   --  Most path construction functions alter the current point. See the
   --  following for details on how they affect the current point:
   --  Cairo_New_Path, Cairo_New_Sub_Path,
   --  Cairo_Append_Path, Cairo_Close_Path,
   --  Cairo_Move_To, Cairo_Line_To, Cairo_Curve_To,
   --  Cairo_Rel_Move_To, Cairo_Rel_Line_To, Cairo_Rel_Curve_To,
   --  Cairo_Arc, Cairo_Arc_Negative, Cairo_Rectangle,
   --  Cairo_Text_Path, Cairo_Glyph_Path, Cairo_Stroke_To_Path.
   --
   --  Some functions use and alter the current point but do not
   --  otherwise change current path:
   --  Cairo_Show_Text.
   --
   --  Some functions unset the current path and as a result, current point:
   --  Cairo_Fill, Cairo_Stroke.

   function Get_Fill_Rule (Cr : Cairo_Context) return Cairo_Fill_Rule;
   --  Cr: a cairo context
   --
   --  Gets the current fill rule, as set by Cairo_Set_Fill_Rule.
   --
   --  Return value: the current fill rule.

   function Get_Line_Width (Cr : Cairo_Context) return Gdouble;
   --  Cr: a cairo context
   --
   --  This function returns the current line width value exactly as set by
   --  Cairo_Set_Line_Width. Note that the value is unchanged even if
   --  the CTM has changed between the calls to Cairo_Set_Line_Width and
   --  Cairo_Get_Line_Width.
   --
   --  Return value: the current line width.

   function Get_Line_Cap (Cr : Cairo_Context) return Cairo_Line_Cap;
   --  Cr: a cairo context
   --
   --  Gets the current line cap style, as set by Cairo_Set_Line_Cap.
   --
   --  Return value: the current line cap style.

   function Get_Line_Join (Cr : Cairo_Context) return Cairo_Line_Join;
   --  Cr: a cairo context
   --
   --  Gets the current line join style, as set by Cairo_Set_Line_Join.
   --
   --  Return value: the current line join style.

   function Get_Miter_Limit (Cr : Cairo_Context) return Gdouble;
   --  Cr: a cairo context
   --
   --  Gets the current miter limit, as set by Cairo_Set_Miter_Limit.
   --
   --  Return value: the current miter limit.

   function Get_Dash_Count (Cr : Cairo_Context) return Gint;
   --  Cr: a Cairo_Context
   --
   --  This function returns the length of the dash array in cr (0 if dashing
   --  is not currently in effect).
   --
   --  See also Cairo_Set_Dash and Cairo_Get_Dash.
   --
   --  Return value: the length of the dash array, or 0 if no dash array set.
   --
   --  Since: 1.4

   procedure Get_Dash
     (Cr     : Cairo_Context;
      Dashes : access Gdouble;
      Offset : access Gdouble);
   --  Cr: a Cairo_Context
   --  Dashes: return value for the dash array, or NULL
   --  Offset: return value for the current dash Offset, or NULL
   --
   --  Gets the current dash array.  If not NULL, dashes should be big
   --  enough to hold at least the number of values returned by
   --  Cairo_Get_Dash_Count.
   --
   --  Since: 1.4

   procedure Get_Matrix (Cr : Cairo_Context; Matrix : access Cairo_Matrix);
   --  Cr: a cairo context
   --  Matrix: return value for the Matrix
   --
   --  Stores the current transformation matrix (CTM) into matrix.

   function Get_Target (Cr : Cairo_Context) return Cairo_Surface;
   --  Cr: a cairo context
   --
   --  Gets the target surface for the cairo context as passed to
   --  Cairo_Create.
   --
   --  This function will always return a valid pointer, but the result
   --  can be a "nil" surface if cr is already in an error state,
   --  (ie. Cairo_Status <literal>!=</literal> CAIRO_STATUS_SUCCESS).
   --  A nil surface is indicated by Cairo.Surface.Status
   --  <literal>!=</literal> CAIRO_STATUS_SUCCESS.
   --
   --  Return value: the target surface. This object is owned by cairo. To
   --  keep a reference to it, you must call Cairo.Surface.Reference.

   function Get_Group_Target (Cr : Cairo_Context) return Cairo_Surface;
   --  Cr: a cairo context
   --
   --  Gets the current destination surface for the context. This is either
   --  the original target surface as passed to Cairo_Create or the target
   --  surface for the current group as started by the most recent call to
   --  Cairo_Push_Group or Cairo_Push_Group_With_Content.
   --
   --  This function will always return a valid pointer, but the result
   --  can be a "nil" surface if cr is already in an error state,
   --  (ie. Cairo_Status <literal>!=</literal> CAIRO_STATUS_SUCCESS).
   --  A nil surface is indicated by Cairo.Surface.Status
   --  <literal>!=</literal> CAIRO_STATUS_SUCCESS.
   --
   --  Return value: the target surface. This object is owned by cairo. To
   --  keep a reference to it, you must call Cairo.Surface.Reference.
   --
   --  Since: 1.2

   --   Cairo_Path_Data_Type:
   --   CAIRO_PATH_MOVE_TO: A move-to operation
   --   CAIRO_PATH_LINE_TO: A line-to operation
   --   CAIRO_PATH_CURVE_TO: A curve-to operation
   --   CAIRO_PATH_CLOSE_PATH: A close-path operation
   --
   --   Cairo_path_data is used to describe the type of one portion
   --   of a path when represented as a Cairo_path.
   --   See Cairo_path_data for details.
   --

   type Cairo_Path_Data_Type is (
      Cairo_Path_Move_To,
      Cairo_Path_Line_To,
      Cairo_Path_Curve_To,
      Cairo_Path_Close_Path);
   pragma Convention (C, Cairo_Path_Data_Type);

   --   Cairo_Path_Data:
   --
   --   Cairo_path_data is used to represent the path data inside a
   --   Cairo_path.
   --
   --   The data structure is designed to try to balance the demands of
   --   efficiency and ease-of-use. A path is represented as an array of
   --   Cairo_Path_Data_T, which is a union of headers and points.
   --
   --   Each portion of the path is represented by one or more elements in
   --   the array, (one header followed by 0 or more points). The length
   --   value of the header is the number of array elements for the current
   --   portion including the header, (ie. length == 1 +  of points), and
   --   where the number of points for each element type is as follows:
   --
   --   <programlisting>
   --       CAIRO_PATH_MOVE_TO:     1 point
   --       CAIRO_PATH_LINE_TO:     1 point
   --       CAIRO_PATH_CURVE_TO:    3 points
   --       CAIRO_PATH_CLOSE_PATH:  0 points
   --   </programlisting>
   --
   --   The semantics and ordering of the coordinate values are consistent
   --   with Cairo_Move_To, Cairo_Line_To, Cairo_Curve_To, and
   --   Cairo_Close_Path.
   --
   --   Here is sample code for iterating through a Cairo_Path:
   --
   --   <informalexample><programlisting>
   --        Gint i;
   --        Cairo_path *path;
   --        Cairo_path_data *data;
   --   &nbsp;
   --        path = Cairo_Copy_Path (cr);
   --   &nbsp;
   --        for (i=0; i < path->num_data; i += path->data[i].header.length) {
   --            data = &amp;path->data[i];
   --            switch (data->header.type) {
   --            case CAIRO_PATH_MOVE_TO:
   --                do_move_to_things (data[1].point.x, data[1].point.y);
   --                break;
   --            case CAIRO_PATH_LINE_TO:
   --                do_line_to_things (data[1].point.x, data[1].point.y);
   --                break;
   --            case CAIRO_PATH_CURVE_TO:
   --                do_curve_to_things (data[1].point.x, data[1].point.y,
   --                                    data[2].point.x, data[2].point.y,
   --                                    data[3].point.x, data[3].point.y);
   --                break;
   --            case CAIRO_PATH_CLOSE_PATH:
   --                do_close_path_things ;
   --                break;
   --            }
   --        }
   --        Cairo_Path_Destroy (path);
   --   </programlisting></informalexample>
   --
   --   As of cairo 1.4, cairo does not mind if there are more elements in
   --   a portion of the path than needed.  Such elements can be used by
   --   users of the cairo API to hold extra values in the path data
   --   structure.  For this reason, it is recommended that applications
   --   always use <literal>data->header.length</literal> to
   --   iterate over the path data, instead of hardcoding the number of
   --   elements for each element type.
   --

   type U_Cairo_Path_Data;
   type Anon_27 is record
      C_Type : aliased Cairo_Path_Data_Type;
      Length : aliased Gint;
   end record;
   pragma Convention (C_Pass_By_Copy, Anon_27);
   type Anon_28 is record
      X : aliased Gdouble;
      Y : aliased Gdouble;
   end record;
   pragma Convention (C_Pass_By_Copy, Anon_28);
   subtype Cairo_Path_Data is U_Cairo_Path_Data;

   type U_Cairo_Path_Data (Discr : Guint := 0) is record
      case Discr is
         when 0 =>
            Header : aliased Anon_27;
         when others =>
            Point : aliased Anon_28;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, U_Cairo_Path_Data);
   pragma Unchecked_Union (U_Cairo_Path_Data);

   --   Cairo_Path:
   --   Status: the current error Status
   --   Data: the elements in the path
   --   Num_Data: the number of elements in the data array
   --
   --   A data structure for holding a path. This data structure serves as
   --   the return value for Cairo_Copy_Path and
   --   Cairo_Copy_Path_Flat as well the input value for
   --   Cairo_Append_Path.
   --
   --   See Cairo_path_data for hints on how to iterate over the
   --   actual data within the path.
   --
   --   The num_data member gives the number of elements in the data
   --   array. This number is larger than the number of independent path
   --   portions (defined in Cairo_path_data_type), since the data
   --   includes both headers and coordinates for each portion.
   --

   type Cairo_Path is record
      Status   : aliased Cairo_Status;
      Data     : access Cairo_Path_Data;
      Num_Data : aliased Gint;
   end record;
   pragma Convention (C_Pass_By_Copy, Cairo_Path);

   function Copy_Path (Cr : Cairo_Context) return access Cairo_Path;
   --  Cr: a cairo context
   --
   --  Creates a copy of the current path and returns it to the user as a
   --  Cairo_Path. See Cairo_Path_Data for hints on how to iterate
   --  over the returned data structure.
   --
   --  This function will always return a valid pointer, but the result
   --  will have no data (<literal>data==NULL</literal> and
   --  <literal>num_data==0</literal>), if either of the following
   --  conditions hold:
   --
   --  <orderedlist>
   --  <listitem>If there is insufficient memory to copy the path. In this
   --      case <literal>path->status</literal> will be set to
   --      CAIRO_STATUS_NO_MEMORY.</listitem>
   --  <listitem>If cr is already in an error state. In this case
   --     <literal>path->status</literal> will contain the same status that
   --     would be returned by Cairo_Status.</listitem>
   --  </orderedlist>
   --
   --  Return value: the copy of the current path. The caller owns the
   --  returned object and should call Cairo_Path_Destroy when finished
   --  with it.

   function Copy_Path_Flat (Cr : Cairo_Context) return access Cairo_Path;
   --  Cr: a cairo context
   --
   --  Gets a flattened copy of the current path and returns it to the
   --  user as a Cairo_Path. See Cairo_Path_Data for hints on
   --  how to iterate over the returned data structure.
   --
   --  This function is like Cairo_Copy_Path except that any curves
   --  in the path will be approximated with piecewise-linear
   --  approximations, (accurate to within the current tolerance
   --  value). That is, the result is guaranteed to not have any elements
   --  of type CAIRO_PATH_CURVE_TO which will instead be replaced by a
   --  series of CAIRO_PATH_LINE_TO elements.
   --
   --  This function will always return a valid pointer, but the result
   --  will have no data (<literal>data==NULL</literal> and
   --  <literal>num_data==0</literal>), if either of the following
   --  conditions hold:
   --
   --  <orderedlist>
   --  <listitem>If there is insufficient memory to copy the path. In this
   --      case <literal>path->status</literal> will be set to
   --      CAIRO_STATUS_NO_MEMORY.</listitem>
   --  <listitem>If cr is already in an error state. In this case
   --     <literal>path->status</literal> will contain the same status that
   --     would be returned by Cairo_Status.</listitem>
   --  </orderedlist>
   --
   --  Return value: the copy of the current path. The caller owns the
   --  returned object and should call Cairo_Path_Destroy when finished
   --  with it.

   procedure Append_Path
     (Cr   : Cairo_Context;
      Path : access constant Cairo_Path);
   --  Cr: a cairo context
   --  Path: Path to be appended
   --
   --  Append the path onto the current path. The path may be either the
   --  return value from one of Cairo_Copy_Path or
   --  Cairo_Copy_Path_Flat or it may be constructed manually.  See
   --  Cairo_Path for details on how the path data structure should be
   --  initialized, and note that <literal>path->status</literal> must be
   --  initialized to CAIRO_STATUS_SUCCESS.

   procedure Path_Destroy (Path : access Cairo_Path);

   --  Error status queries
   function Status (Cr : Cairo_Context) return Cairo_Status;
   --  Cr: a cairo context
   --
   --  Checks whether an error has previously occurred for this context.
   --
   --  Returns: the current status of this context, see Cairo_Status

   --  Surface manipulation

   --   Cairo_Surface_Type:
   --   CAIRO_SURFACE_TYPE_IMAGE: The surface is of type image
   --   CAIRO_SURFACE_TYPE_PDF: The surface is of type pdf
   --   CAIRO_SURFACE_TYPE_PS: The surface is of type ps
   --   CAIRO_SURFACE_TYPE_XLIB: The surface is of type xlib
   --   CAIRO_SURFACE_TYPE_XCB: The surface is of type xcb
   --   CAIRO_SURFACE_TYPE_GLITZ: The surface is of type glitz
   --   CAIRO_SURFACE_TYPE_QUARTZ: The surface is of type quartz
   --   CAIRO_SURFACE_TYPE_WIN32: The surface is of type win32
   --   CAIRO_SURFACE_TYPE_BEOS: The surface is of type beos
   --   CAIRO_SURFACE_TYPE_DIRECTFB: The surface is of type directfb
   --   CAIRO_SURFACE_TYPE_SVG: The surface is of type svg
   --   CAIRO_SURFACE_TYPE_OS2: The surface is of type os2
   --   CAIRO_SURFACE_TYPE_WIN32_PRINTING: The surface is a win32 printing
   --   surface
   --   CAIRO_SURFACE_TYPE_QUARTZ_IMAGE: The surface is of type quartz_image
   --
   --   Cairo_surface_type is used to describe the type of a given
   --   surface. The surface types are also known as "backends" or "surface
   --   backends" within cairo.
   --
   --   The type of a surface is determined by the function used to create
   --   it, which will generally be of the form
   --   Cairo_<emphasis>type</emphasis>_Surface_Create,
   --   (though see Cairo.Surface.Create_Similar as well).
   --
   --   The surface type can be queried with Cairo.Surface.Get_Type
   --
   --   The various Cairo_surface functions can be used with surfaces of
   --   any type, but some backends also provide type-specific functions
   --   that must only be called with a surface of the appropriate
   --   type. These functions have names that begin with
   --   Cairo_<emphasis>type</emphasis>_Surface<!-- --> such as
   --   Cairo.Image_Surface.Get_Width.
   --
   --   The behavior of calling a type-specific function with a surface of
   --   the wrong type is undefined.
   --
   --   New entries may be added in future versions.
   --
   --   Since: 1.2
   --

   type Cairo_Surface_Type is (
      Cairo_Surface_Type_Image,
      Cairo_Surface_Type_Pdf,
      Cairo_Surface_Type_Ps,
      Cairo_Surface_Type_Xlib,
      Cairo_Surface_Type_Xcb,
      Cairo_Surface_Type_Glitz,
      Cairo_Surface_Type_Quartz,
      Cairo_Surface_Type_Win32,
      Cairo_Surface_Type_Beos,
      Cairo_Surface_Type_Directfb,
      Cairo_Surface_Type_Svg,
      Cairo_Surface_Type_Os2,
      Cairo_Surface_Type_Win32_Printing,
      Cairo_Surface_Type_Quartz_Image);
   pragma Convention (C, Cairo_Surface_Type);

   --  Image-surface functions

   --   Cairo_Format:
   --   CAIRO_FORMAT_ARGB32: each pixel is a 32-bit quantity, with
   --     alpha in the upper 8 bits, then red, then green, then blue.
   --     The 32-bit quantities are stored native-endian. Pre-multiplied
   --     alpha is used. (That is, 50 transparent red is 0x80800000,
   --     not 0x80ff0000.)
   --   CAIRO_FORMAT_RGB24: each pixel is a 32-bit quantity, with
   --     the upper 8 bits unused. Red, Green, and Blue are stored
   --     in the remaining 24 bits in that order.
   --   CAIRO_FORMAT_A8: each pixel is a 8-bit quantity holding
   --     an alpha value.
   --   CAIRO_FORMAT_A1: each pixel is a 1-bit quantity holding
   --     an alpha value. Pixels are packed together into 32-bit
   --     quantities. The ordering of the bits matches the
   --     endianess of the platform. On a big-endian machine, the
   --     first pixel is in the uppermost bit, on a little-endian
   --     machine the first pixel is in the least-significant bit.
   --   CAIRO_FORMAT_RGB16_565: This format value is deprecated. It has
   --     never been properly implemented in cairo and should not be used
   --     by applications. (since 1.2)
   --
   --   Cairo_format is used to identify the memory format of
   --   image data.
   --
   --   New entries may be added in future versions.
   --

   --  The value of 4 is reserved by a deprecated enum value.
   --     * The next format added must have an explicit value of 5.
   --    CAIRO_FORMAT_RGB16_565 = 4,
   --

   type Cairo_Format is (
      Cairo_Format_Argb32,
      Cairo_Format_Rgb24,
      Cairo_Format_A8,
      Cairo_Format_A1);
   pragma Convention (C, Cairo_Format);

   --  Pattern creation functions

   --   Cairo_Pattern_Type:
   --   CAIRO_PATTERN_TYPE_SOLID: The pattern is a solid (uniform)
   --   color. It may be opaque or translucent.
   --   CAIRO_PATTERN_TYPE_SURFACE: The pattern is a based on a surface (an
   --   image).
   --   CAIRO_PATTERN_TYPE_LINEAR: The pattern is a linear gradient.
   --   CAIRO_PATTERN_TYPE_RADIAL: The pattern is a radial gradient.
   --
   --   Cairo_pattern_type is used to describe the type of a given pattern.
   --
   --   The type of a pattern is determined by the function used to create
   --   it. The Cairo.Pattern.Create_Rgb and Cairo.Pattern.Create_Rgba
   --   functions create SOLID patterns. The remaining
   --   Cairo.Pattern.Create<!-- --> functions map to pattern types in obvious
   --   ways.
   --
   --   The pattern type can be queried with Cairo.Pattern.Get_Type
   --
   --   Most Cairo_pattern functions can be called with a pattern of any
   --   type, (though trying to change the extend or filter for a solid
   --   pattern will have no effect). A notable exception is
   --   Cairo.Pattern.Add_Color_Stop_Rgb and
   --   Cairo.Pattern.Add_Color_Stop_Rgba which must only be called with
   --   gradient patterns (either LINEAR or RADIAL). Otherwise the pattern
   --   will be shutdown and put into an error state.
   --
   --   New entries may be added in future versions.
   --
   --   Since: 1.2
   --

   type Cairo_Pattern_Type is (
      Cairo_Pattern_Type_Solid,
      Cairo_Pattern_Type_Surface,
      Cairo_Pattern_Type_Linear,
      Cairo_Pattern_Type_Radial);
   pragma Convention (C, Cairo_Pattern_Type);

   --   Cairo_Extend:
   --   CAIRO_EXTEND_NONE: pixels outside of the source pattern
   --     are fully transparent
   --   CAIRO_EXTEND_REPEAT: the pattern is tiled by repeating
   --   CAIRO_EXTEND_REFLECT: the pattern is tiled by reflecting
   --     at the edges (Implemented for surface patterns since 1.6)
   --   CAIRO_EXTEND_PAD: pixels outside of the pattern copy
   --     the closest pixel from the source (Since 1.2; but only
   --     implemented for surface patterns since 1.6)
   --
   --   Cairo_extend is used to describe how pattern color/alpha will be
   --   determined for areas "outside" the pattern's natural area, (for
   --   example, outside the surface bounds or outside the gradient
   --   geometry).
   --
   --   The default extend mode is CAIRO_EXTEND_NONE for surface patterns
   --   and CAIRO_EXTEND_PAD for gradient patterns.
   --
   --   New entries may be added in future versions.
   --

   type Cairo_Extend is (
      Cairo_Extend_None,
      Cairo_Extend_Repeat,
      Cairo_Extend_Reflect,
      Cairo_Extend_Pad);
   pragma Convention (C, Cairo_Extend);

   --   Cairo_Filter:
   --   CAIRO_FILTER_FAST: A high-performance filter, with quality similar
   --       to CAIRO_FILTER_NEAREST
   --   CAIRO_FILTER_GOOD: A reasonable-performance filter, with quality
   --       similar to CAIRO_FILTER_BILINEAR
   --   CAIRO_FILTER_BEST: The highest-quality available, performance may
   --       not be suitable for interactive use.
   --   CAIRO_FILTER_NEAREST: Nearest-neighbor filtering
   --   CAIRO_FILTER_BILINEAR: Linear interpolation in two dimensions
   --   CAIRO_FILTER_GAUSSIAN: This filter value is currently
   --       unimplemented, and should not be used in current code.
   --
   --   Cairo_filter is used to indicate what filtering should be
   --   applied when reading pixel values from patterns. See
   --   Cairo.Pattern.Set_Source for indicating the desired filter to be
   --   used with a particular pattern.
   --

   type Cairo_Filter is (
      Cairo_Filter_Fast,
      Cairo_Filter_Good,
      Cairo_Filter_Best,
      Cairo_Filter_Nearest,
      Cairo_Filter_Bilinear,
      Cairo_Filter_Gaussian);
   pragma Convention (C, Cairo_Filter);

   --  Matrix functions

   Null_Context      : constant Cairo_Context;
   Null_Surface      : constant Cairo_Surface;
   Null_Pattern      : constant Cairo_Pattern;
   Null_Scaled_Font  : constant Cairo_Scaled_Font;
   Null_Font_Face    : constant Cairo_Font_Face;
   Null_Font_Options : constant Cairo_Font_Options;

private

   pragma Convention (C, Cairo_Bool);
   pragma Convention (C_Pass_By_Copy, Cairo_Matrix);
   pragma Convention (C, Cairo_Status);

   type Cairo_Context is new System.Address;
   Null_Context : constant Cairo_Context :=
      Cairo_Context (System.Null_Address);
   type Cairo_Surface is new System.Address;
   Null_Surface : constant Cairo_Surface :=
      Cairo_Surface (System.Null_Address);
   type Cairo_Pattern is new System.Address;
   Null_Pattern : constant Cairo_Pattern :=
      Cairo_Pattern (System.Null_Address);
   type Cairo_Scaled_Font is new System.Address;
   Null_Scaled_Font : constant Cairo_Scaled_Font :=
      Cairo_Scaled_Font (System.Null_Address);
   type Cairo_Font_Face is new System.Address;
   Null_Font_Face : constant Cairo_Font_Face :=
      Cairo_Font_Face (System.Null_Address);
   type Cairo_Font_Options is new System.Address;
   Null_Font_Options : constant Cairo_Font_Options :=
      Cairo_Font_Options (System.Null_Address);
   pragma Import (C, Create, "cairo_create");
   pragma Import (C, Reference, "cairo_reference");
   pragma Import (C, Destroy, "cairo_destroy");
   pragma Import (C, Get_Reference_Count, "cairo_get_reference_count");
   pragma Import (C, Get_User_Data, "cairo_get_user_data");
   pragma Import (C, Set_User_Data, "cairo_set_user_data");
   pragma Import (C, Save, "cairo_save");
   pragma Import (C, Restore, "cairo_restore");
   pragma Import (C, Push_Group, "cairo_push_group");
   pragma Import
     (C,
      Push_Group_With_Content,
      "cairo_push_group_with_content");
   pragma Import (C, Pop_Group, "cairo_pop_group");
   pragma Import (C, Pop_Group_To_Source, "cairo_pop_group_to_source");
   pragma Import (C, Set_Operator, "cairo_set_operator");
   pragma Import (C, Set_Source, "cairo_set_source");
   pragma Import (C, Set_Source_Rgb, "cairo_set_source_rgb");
   pragma Import (C, Set_Source_Rgba, "cairo_set_source_rgba");
   pragma Import (C, Set_Source_Surface, "cairo_set_source_surface");
   pragma Import (C, Set_Tolerance, "cairo_set_tolerance");
   pragma Import (C, Set_Antialias, "cairo_set_antialias");
   pragma Import (C, Set_Fill_Rule, "cairo_set_fill_rule");
   pragma Import (C, Set_Line_Width, "cairo_set_line_width");
   pragma Import (C, Set_Line_Cap, "cairo_set_line_cap");
   pragma Import (C, Set_Line_Join, "cairo_set_line_join");
   pragma Import (C, Set_Dash, "cairo_set_dash");
   pragma Import (C, Set_Miter_Limit, "cairo_set_miter_limit");
   pragma Import (C, Translate, "cairo_translate");
   pragma Import (C, Scale, "cairo_scale");
   pragma Import (C, Rotate, "cairo_rotate");
   pragma Import (C, Transform, "cairo_transform");
   pragma Import (C, Set_Matrix, "cairo_set_matrix");
   pragma Import (C, Identity_Matrix, "cairo_identity_matrix");
   pragma Import (C, User_To_Device, "cairo_user_to_device");
   pragma Import
     (C,
      User_To_Device_Distance,
      "cairo_user_to_device_distance");
   pragma Import (C, Device_To_User, "cairo_device_to_user");
   pragma Import
     (C,
      Device_To_User_Distance,
      "cairo_device_to_user_distance");
   pragma Import (C, New_Path, "cairo_new_path");
   pragma Import (C, Move_To, "cairo_move_to");
   pragma Import (C, New_Sub_Path, "cairo_new_sub_path");
   pragma Import (C, Line_To, "cairo_line_to");
   pragma Import (C, Curve_To, "cairo_curve_to");
   pragma Import (C, Arc, "cairo_arc");
   pragma Import (C, Arc_Negative, "cairo_arc_negative");
   pragma Import (C, Rel_Move_To, "cairo_rel_move_to");
   pragma Import (C, Rel_Line_To, "cairo_rel_line_to");
   pragma Import (C, Rel_Curve_To, "cairo_rel_curve_to");
   pragma Import (C, Rectangle, "cairo_rectangle");
   pragma Import (C, Close_Path, "cairo_close_path");
   pragma Import (C, Path_Extents, "cairo_path_extents");
   pragma Import (C, Paint, "cairo_paint");
   pragma Import (C, Paint_With_Alpha, "cairo_paint_with_alpha");
   pragma Import (C, Mask, "cairo_mask");
   pragma Import (C, Mask_Surface, "cairo_mask_surface");
   pragma Import (C, Stroke, "cairo_stroke");
   pragma Import (C, Stroke_Preserve, "cairo_stroke_preserve");
   pragma Import (C, Fill, "cairo_fill");
   pragma Import (C, Fill_Preserve, "cairo_fill_preserve");
   pragma Import (C, Copy_Page, "cairo_copy_page");
   pragma Import (C, Show_Page, "cairo_show_page");
   pragma Import (C, In_Stroke, "cairo_in_stroke");
   pragma Import (C, In_Fill, "cairo_in_fill");
   pragma Import (C, Stroke_Extents, "cairo_stroke_extents");
   pragma Import (C, Fill_Extents, "cairo_fill_extents");
   pragma Import (C, Reset_Clip, "cairo_reset_clip");
   pragma Import (C, Clip, "cairo_clip");
   pragma Import (C, Clip_Preserve, "cairo_clip_preserve");
   pragma Import (C, Clip_Extents, "cairo_clip_extents");
   pragma Import
     (C,
      Copy_Clip_Rectangle_List,
      "cairo_copy_clip_rectangle_list");
   pragma Import (C, Rectangle_List_Destroy, "cairo_rectangle_list_destroy");
   pragma Import (C, Select_Font_Face, "cairo_select_font_face");
   pragma Import (C, Set_Font_Size, "cairo_set_font_size");
   pragma Import (C, Set_Font_Matrix, "cairo_set_font_matrix");
   pragma Import (C, Get_Font_Matrix, "cairo_get_font_matrix");
   pragma Import (C, Set_Font_Options, "cairo_set_font_options");
   pragma Import (C, Get_Font_Options, "cairo_get_font_options");
   pragma Import (C, Set_Font_Face, "cairo_set_font_face");
   pragma Import (C, Get_Font_Face, "cairo_get_font_face");
   pragma Import (C, Set_Scaled_Font, "cairo_set_scaled_font");
   pragma Import (C, Get_Scaled_Font, "cairo_get_scaled_font");
   pragma Import (C, Show_Text, "cairo_show_text");
   pragma Import (C, Show_Glyphs, "cairo_show_glyphs");
   pragma Import (C, Show_Text_Glyphs, "cairo_show_text_glyphs");
   pragma Import (C, Text_Path, "cairo_text_path");
   pragma Import (C, Glyph_Path, "cairo_glyph_path");
   pragma Import (C, Text_Extents, "cairo_text_extents");
   pragma Import (C, Glyph_Extents, "cairo_glyph_extents");
   pragma Import (C, Font_Extents, "cairo_font_extents");
   pragma Import (C, Get_Operator, "cairo_get_operator");
   pragma Import (C, Get_Source, "cairo_get_source");
   pragma Import (C, Get_Tolerance, "cairo_get_tolerance");
   pragma Import (C, Get_Antialias, "cairo_get_antialias");
   pragma Import (C, Has_Current_Point, "cairo_has_current_point");
   pragma Import (C, Get_Current_Point, "cairo_get_current_point");
   pragma Import (C, Get_Fill_Rule, "cairo_get_fill_rule");
   pragma Import (C, Get_Line_Width, "cairo_get_line_width");
   pragma Import (C, Get_Line_Cap, "cairo_get_line_cap");
   pragma Import (C, Get_Line_Join, "cairo_get_line_join");
   pragma Import (C, Get_Miter_Limit, "cairo_get_miter_limit");
   pragma Import (C, Get_Dash_Count, "cairo_get_dash_count");
   pragma Import (C, Get_Dash, "cairo_get_dash");
   pragma Import (C, Get_Matrix, "cairo_get_matrix");
   pragma Import (C, Get_Target, "cairo_get_target");
   pragma Import (C, Get_Group_Target, "cairo_get_group_target");
   pragma Import (C, Copy_Path, "cairo_copy_path");
   pragma Import (C, Copy_Path_Flat, "cairo_copy_path_flat");
   pragma Import (C, Append_Path, "cairo_append_path");
   pragma Import (C, Path_Destroy, "cairo_path_destroy");
   pragma Import (C, Status, "cairo_status");

end Cairo;
