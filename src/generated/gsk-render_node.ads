------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

--  The basic block in a scene graph to be rendered using [classGsk.Renderer].
--
--  Each node has a parent, except the top-level node; each node may have
--  children nodes.
--
--  Each node has an associated drawing surface, which has the size of the
--  rectangle set when creating it.
--
--  Render nodes are meant to be transient; once they have been associated to
--  a [classGsk.Renderer] it's safe to release any reference you have on them.
--  All [classGsk.RenderNode]s are immutable, you can only specify their
--  properties during construction.

pragma Warnings (Off, "*is already use-visible*");
with Ada.Finalization; use Ada.Finalization;
with Cairo;            use Cairo;
with Gdk.RGBA;         use Gdk.RGBA;
with Glib;             use Glib;
with Glib.Bytes;       use Glib.Bytes;
with Glib.Error;       use Glib.Error;
with Graphene.Rect;    use Graphene.Rect;
with Gsk.Enums;        use Gsk.Enums;
with Interfaces.C;     use Interfaces.C;
with System;           use System;

package Gsk.Render_Node is

   type Gsk_Render_Node_Record is abstract new Ada.Finalization.Controlled with private;
   type Gsk_Render_Node is access all Gsk_Render_Node_Record'Class;

   type Gsk_Parse_Location is record
      Bytes : Gsize;
      Chars : Gsize;
      Lines : Gsize;
      Line_Bytes : Gsize;
      Line_Chars : Gsize;
   end record;
   pragma Convention (C, Gsk_Parse_Location);
   --  A location in a parse buffer.

   type Gsk_Color_Stop is record
      Offset : Interfaces.C.C_float;
      Color : Gdk.RGBA.Gdk_RGBA;
   end record;
   pragma Convention (C, Gsk_Color_Stop);
   --  A color stop in a gradient node.

   type Gsk_Color_Stop_Array is array (Natural range <>) of Gsk_Color_Stop
   with Convention => C;

   type Gsk_Render_Node_Array is array (Natural range <>) of Gsk_Render_Node
   with Convention => C;

   ---------------
   -- Callbacks --
   ---------------

   type Gsk_Parse_Error_Func is access procedure
     (Start   : Gsk_Parse_Location;
      The_End : Gsk_Parse_Location;
      Error   : Glib.Error.GError);
   --  Type of callback that is called when an error occurs during node
   --  deserialization.
   --  @param Start start of the error location
   --  @param The_End end of the error location
   --  @param Error the error

   ------------------
   -- Constructors --
   ------------------

   function Get_Type return Glib.GType;
   pragma Import (C, Get_Type, "gsk_render_node_get_type");

   -------------
   -- Methods --
   -------------

   procedure Draw (Self : Gsk_Render_Node; Cr : Cairo.Cairo_Context);
   --  Draws the contents of a render node on a cairo context.
   --  Typically, you'll use this function to implement fallback rendering of
   --  render nodes on an intermediate Cairo context, instead of using the
   --  drawing context associated to a [classGdk.Surface]'s rendering buffer.
   --  For advanced nodes that cannot be supported using Cairo, in particular
   --  for nodes doing 3D operations, this function may fail.
   --  @param Cr cairo context to draw to

   procedure Get_Bounds
      (Self   : Gsk_Render_Node;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T);
   --  Retrieves the boundaries of the Node.
   --  The node will not draw outside of its boundaries.
   --  @param Bounds return location for the boundaries

   function Get_Children
      (Self : Gsk_Render_Node) return Gsk_Render_Node_Array;
   --  Gets a list of all children nodes of the rendernode.
   --  Keep in mind that for various rendernodes, their children have
   --  different semantics, like the mask vs the source of a mask node. If you
   --  care about thse semantics, don't use this function, use the specific
   --  getters instead.
   --  Since: gtk+ 4.22
   --  @return The children

   function Get_Node_Type
      (Self : Gsk_Render_Node) return Gsk.Enums.Gsk_Render_Node_Type;
   --  Returns the type of the render node.
   --  @return the type of Node

   function Get_Opaque_Rect
      (Self       : Gsk_Render_Node;
       Out_Opaque : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean;
   --  Gets an opaque rectangle inside the node that GTK can determine to be
   --  fully opaque.
   --  There is no guarantee that this is indeed the largest opaque rectangle
   --  or that regions outside the rectangle are not opaque. This function is a
   --  best effort with that goal.
   --  The rectangle will be fully contained in the bounds of the node.
   --  Since: gtk+ 4.16
   --  @param Out_Opaque return location for the opaque rect
   --  @return true if part or all of the rendernode is opaque, false if no
   --  opaque region could be found.

   function Serialize (Self : Gsk_Render_Node) return Glib.Bytes.Gbytes;
   --  Serializes the Node for later deserialization via
   --  Gsk.Render_Node.Deserialize. No guarantees are made about the format
   --  used other than that the same version of GTK will be able to deserialize
   --  the result of a call to Gsk.Render_Node.Serialize and
   --  Gsk.Render_Node.Deserialize will correctly reject files it cannot open
   --  that were created with previous versions of GTK.
   --  The intended use of this functions is testing, benchmarking and
   --  debugging. The format is not meant as a permanent storage format.
   --  @return a `GBytes` representing the node. Has
   --  transfer-ownership='full'.

   function Write_To_File
      (Self     : Gsk_Render_Node;
       Filename : UTF8_String;
       Error    : out Glib.Error.GError) return Boolean;
   --  This function is equivalent to calling [methodGsk.RenderNode.serialize]
   --  followed by [funcGlib.file_set_contents].
   --  See those two functions for details on the arguments.
   --  It is mostly intended for use inside a debugger to quickly dump a
   --  render node to a file for later inspection.
   --  @param Filename the file to save it to
   --  @param Error the return location for a recoverable error
   --  @return true if saving was successful





   function Deserialize
      (Bytes      : Glib.Bytes.Gbytes;
       Error_Func : Gsk_Parse_Error_Func) return Gsk_Render_Node;
   --  Loads data previously created via [methodGsk.RenderNode.serialize].
   --  For a discussion of the supported format, see that function.
   --  @param Bytes the bytes containing the data
   --  @param Error_Func callback on parsing errors
   --  @return a new render node. Has transfer-ownership='full'.

   ----------------------
   -- GtkAda additions --
   ----------------------

   function Create (Ptr: not null access System.Address) return Gsk_Render_Node_Record is abstract;

   type Dummy_Gsk_Render_Node_Record is new Gsk_Render_Node_Record with private;
   function Create (Ptr: not null access System.Address) return Dummy_Gsk_Render_Node_Record;

   function From_Object_Free
     (B : not null access Gsk_Parse_Location) return Gsk_Parse_Location;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function From_Object_Free
     (B : not null access Gsk_Color_Stop) return Gsk_Color_Stop;
   pragma Inline (From_Object_Free);
   --  Return the underlying object and free the pointer.
   --  This is meant to be used internally by GtkAda,
   --  and should not in general be called by user code.

   function Is_Created
      (Self : not null access Gsk_Render_Node_Record'Class) return Boolean;
   --  Retrn True if Self is initialized with Gtk object

   function Get_Object
      (Self : access Gsk_Render_Node_Record'Class) return System.Address;
   --  For internal usage. Do not call manualy.

   procedure Set_Object
      (Self   : not null access Gsk_Render_Node_Record'Class;
       Object : System.Address);
   --  For internal usage. Do not call manualy.

   function From_Object_Full_Ownership
      (Object : System.Address) return Gsk_Render_Node;
   --  For internal usage. Do not call manualy.

   function From_Object_None_Ownership
      (Object : System.Address) return Gsk_Render_Node;
   --  For internal usage. Do not call manualy.

private

   type Gsk_Render_Node_Record is abstract new Ada.Finalization.Controlled with record
      Ptr : System.Address := System.Null_Address;
   end record;

   overriding procedure Adjust (Object : in out Gsk_Render_Node_Record);
   overriding procedure Finalize (Object : in out Gsk_Render_Node_Record);

   type Dummy_Gsk_Render_Node_Record is new Gsk_Render_Node_Record with null record;

end Gsk.Render_Node;
