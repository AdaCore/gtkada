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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Tags.Generic_Dispatching_Constructor;
with Ada.Unchecked_Conversion;
with Glib.Error;
with Interfaces.C.Pointers;
pragma Warnings(Off);  --  might be unused
with Gsk.Blend_Node;
with Gsk.Blur_Node;
with Gsk.Border_Node;
with Gsk.Cairo_Node;
with Gsk.Clip_Node;
with Gsk.Color_Matrix_Node;
with Gsk.Color_Node;
with Gsk.Component_Transfer_Node;
with Gsk.Composite_Node;
with Gsk.Conic_Gradient_Node;
with Gsk.Container_Node;
with Gsk.Copy_Node;
with Gsk.Cross_Fade_Node;
with Gsk.Debug_Node;
with Gsk.Fill_Node;
with Gsk.Glshader_Node;
with Gsk.Inset_Shadow_Node;
with Gsk.Isolation_Node;
with Gsk.Linear_Gradient_Node;
with Gsk.Mask_Node;
with Gsk.Opacity_Node;
with Gsk.Outset_Shadow_Node;
with Gsk.Paste_Node;
with Gsk.Radial_Gradient_Node;
with Gsk.Repeat_Node;
with Gsk.Repeating_Linear_Gradient_Node;
with Gsk.Repeating_Radial_Gradient_Node;
with Gsk.Rounded_Clip_Node;
with Gsk.Shadow_Node;
with Gsk.Stroke_Node;
with Gsk.Subsurface_Node;
with Gsk.Text_Node;
with Gsk.Texture_Node;
with Gsk.Texture_Scale_Node;
with Gsk.Transform_Node;
with Gtkada.Types;                             use Gtkada.Types;
pragma Warnings(On);

use type Glib.Error.GError;

package body Gsk.Render_Node is

   function C_Gsk_Render_Node_Deserialize
      (Bytes      : System.Address;
       Error_Func : System.Address;
       User_Data  : System.Address) return System.Address;
   pragma Import (C, C_Gsk_Render_Node_Deserialize, "gsk_render_node_deserialize");
   --  Loads data previously created via [methodGsk.RenderNode.serialize].
   --  For a discussion of the supported format, see that function.
   --  @param Bytes the bytes containing the data
   --  @param Error_Func callback on parsing errors
   --  @param User_Data user_data for Error_Func
   --  @return a new render node. Has transfer-ownership='full'.

   function To_Gsk_Parse_Error_Func is new Ada.Unchecked_Conversion
     (System.Address, Gsk_Parse_Error_Func);

   function To_Address is new Ada.Unchecked_Conversion
     (Gsk_Parse_Error_Func, System.Address);

   procedure Internal_Gsk_Parse_Error_Func
      (Start     : access Gsk.Render_Node.Gsk_Parse_Location;
       The_End   : access Gsk.Render_Node.Gsk_Parse_Location;
       Error     : Glib.Error.GError;
       User_Data : System.Address);
   pragma Convention (C, Internal_Gsk_Parse_Error_Func);
   --  @param Start start of the error location
   --  @param The_End end of the error location
   --  @param Error the error
   --  @param User_Data user data

   -----------------------------------
   -- Internal_Gsk_Parse_Error_Func --
   -----------------------------------

   procedure Internal_Gsk_Parse_Error_Func
      (Start     : access Gsk.Render_Node.Gsk_Parse_Location;
       The_End   : access Gsk.Render_Node.Gsk_Parse_Location;
       Error     : Glib.Error.GError;
       User_Data : System.Address)
   is
      Func : constant Gsk_Parse_Error_Func := To_Gsk_Parse_Error_Func (User_Data);
   begin
      Func (Start.all, The_End.all, Error);
   end Internal_Gsk_Parse_Error_Func;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Object : in out Gsk_Render_Node_Record) is
      function Ref (Self : System.Address) return System.Address;
      pragma Import (C, Ref, "gsk_render_node_ref");
   begin
      if Object.Ptr /= System.Null_Address then
         Object.Ptr := Ref (Object.Ptr);
      end if;
   end Adjust;

   -----------------
   -- Deserialize --
   -----------------

   function Deserialize
      (Bytes      : Glib.Bytes.Gbytes;
       Error_Func : Gsk_Parse_Error_Func) return Gsk_Render_Node
   is
   begin
      if Error_Func = null then
         return From_Object_Full_Ownership (C_Gsk_Render_Node_Deserialize (Get_Object (Bytes), System.Null_Address, System.Null_Address));
      else
         return From_Object_Full_Ownership (C_Gsk_Render_Node_Deserialize (Get_Object (Bytes), Internal_Gsk_Parse_Error_Func'Address, To_Address (Error_Func)));
      end if;
   end Deserialize;

   ----------
   -- Draw --
   ----------

   procedure Draw (Self : Gsk_Render_Node; Cr : Cairo.Cairo_Context) is
      procedure Internal (Self : System.Address; Cr : Cairo.Cairo_Context);
      pragma Import (C, Internal, "gsk_render_node_draw");
   begin
      Internal (Get_Object (Self), Cr);
   end Draw;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Gsk_Render_Node_Record) is
      procedure Unref (Self : System.Address);
      pragma Import (C, Unref, "gsk_render_node_unref");
   begin
      if Object.Ptr /= System.Null_Address then
         Unref (Object.Ptr);
         Object.Ptr := System.Null_Address;
      end if;
   end Finalize;

   ----------------
   -- Get_Bounds --
   ----------------

   procedure Get_Bounds
      (Self   : Gsk_Render_Node;
       Bounds : not null access Graphene.Rect.Graphene_Rect_T)
   is
      procedure Internal
         (Self   : System.Address;
          Bounds : not null access Graphene.Rect.Graphene_Rect_T);
      pragma Import (C, Internal, "gsk_render_node_get_bounds");
   begin
      Internal (Get_Object (Self), Bounds);
   end Get_Bounds;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children
      (Self : Gsk_Render_Node) return Gsk_Render_Node_Array
   is
      use Interfaces.C;
      type Aliased_Address_Array is
        array (Positive range <>) of aliased System.Address
        with Convention => C;
      package P is new Interfaces.C.Pointers
        (Positive, System.Address, Aliased_Address_Array, System.Null_Address);

      function Internal
         (Self           : System.Address;
          Acc_N_Children : access Gsize) return P.Pointer;
      pragma Import (C, Internal, "gsk_render_node_get_children");
      Acc_N_Children : aliased Gsize;
      Tmp_Return     : P.Pointer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Acc_N_Children'Access);
      declare
         Result : Gsk_Render_Node_Array (1 .. Natural (Acc_N_Children));
         Arr    : constant Aliased_Address_Array :=
           P.Value (Tmp_Return, Result'Length);
         I      : Positive := 1;
      begin
         for Addr of Arr loop
            Result (I) := From_Object_None_Ownership (Addr);
            I := I + 1;
         end loop;
         return Result;
      end;
   end Get_Children;

   -------------------
   -- Get_Node_Type --
   -------------------

   function Get_Node_Type
      (Self : Gsk_Render_Node) return Gsk.Enums.Gsk_Render_Node_Type
   is
      function Internal
         (Self : System.Address) return Gsk.Enums.Gsk_Render_Node_Type;
      pragma Import (C, Internal, "gsk_render_node_get_node_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Node_Type;

   ---------------------
   -- Get_Opaque_Rect --
   ---------------------

   function Get_Opaque_Rect
      (Self       : Gsk_Render_Node;
       Out_Opaque : not null access Graphene.Rect.Graphene_Rect_T)
       return Boolean
   is
      function Internal
         (Self       : System.Address;
          Out_Opaque : access Graphene.Rect.Graphene_Rect_T)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_render_node_get_opaque_rect");
   begin
      return Internal (Get_Object (Self), Out_Opaque) /= 0;
   end Get_Opaque_Rect;

   ---------------
   -- Serialize --
   ---------------

   function Serialize (Self : Gsk_Render_Node) return Glib.Bytes.Gbytes is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gsk_render_node_serialize");
   begin
      return From_Object (Internal (Get_Object (Self)));
   end Serialize;

   -------------------
   -- Write_To_File --
   -------------------

   function Write_To_File
      (Self     : Gsk_Render_Node;
       Filename : UTF8_String;
       Error    : out Glib.Error.GError) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Filename  : Gtkada.Types.Chars_Ptr;
          Acc_Error : access Glib.Error.GError) return Glib.Gboolean;
      pragma Import (C, Internal, "gsk_render_node_write_to_file");
      Acc_Error    : aliased Glib.Error.GError;
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Filename, Acc_Error'Access);
      Error := Acc_Error;
      Free (Tmp_Filename);
      return Tmp_Return /= 0;
   end Write_To_File;

   function Dispatching_Constructor is
      new Ada.Tags.Generic_Dispatching_Constructor
     (Gsk_Render_Node_Record, System.Address, Create);

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gsk_Parse_Location) return Gsk_Parse_Location
   is
      Result : constant Gsk_Parse_Location := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ----------------------
   -- From_Object_Free --
   ----------------------

   function From_Object_Free
     (B : not null access Gsk_Color_Stop) return Gsk_Color_Stop
   is
      Result : constant Gsk_Color_Stop := B.all;
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   ------------
   -- Create --
   ------------

   function Create
      (Ptr : not null access System.Address)
       return Dummy_Gsk_Render_Node_Record
   is
      pragma Unreferenced (Ptr);
      Result : Dummy_Gsk_Render_Node_Record;
   begin
      return Result;
   end Create;

   --------------------------------
   -- From_Object_Full_Ownership --
   --------------------------------

   function From_Object_Full_Ownership
      (Object : System.Address) return Gsk_Render_Node
   is
      T      : Glib.GType;
      O      : aliased System.Address := System.Null_Address;
      Result : Gsk_Render_Node;
   begin
      if Object /= System.Null_Address then
         T := Glib.Instance_Get_Type (Object);
         Result := new Gsk_Render_Node_Record'Class'
           (Dispatching_Constructor
              (Ada.Tags.Internal_Tag (Glib.Type_Name (T)), O'Access));
         Set_Object (Result, Object);
         return Result;
      else
         return null;
      end if;
   exception
      when Ada.Tags.Tag_Error =>
         Result := new Dummy_Gsk_Render_Node_Record'(Create (O'Access));
         Set_Object (Result, Object);
         return Result;
   end From_Object_Full_Ownership;

   --------------------------------
   -- From_Object_None_Ownership --
   --------------------------------

   function From_Object_None_Ownership
      (Object : System.Address) return Gsk_Render_Node
   is
      Result : Gsk_Render_Node;
   begin
      Result := From_Object_Full_Ownership (Object);
      if Result /= null then
         --  To call Ref
         Adjust (Result.all);
      end if;
      return Result;
   end From_Object_None_Ownership;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
      (Self : access Gsk_Render_Node_Record'Class) return System.Address
   is
   begin
      if Self = null then
         return System.Null_Address;
      else
         return Self.Ptr;
      end if;
   end Get_Object;

   ----------------
   -- Is_Created --
   ----------------

   function Is_Created
      (Self : not null access Gsk_Render_Node_Record'Class) return Boolean
   is
   begin
      return Self.Ptr /= System.Null_Address;
   end Is_Created;

   ----------------
   -- Set_Object --
   ----------------

   procedure Set_Object
      (Self   : not null access Gsk_Render_Node_Record'Class;
       Object : System.Address)
   is
   begin
      Self.Ptr := Object;
   end Set_Object;

end Gsk.Render_Node;
