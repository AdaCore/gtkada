--  Exercise Gsk.Texture_Scale_Node construction, initialization, and getters.
--  There is no focused C testsuite case for gsk_texture_scale_node_*; the
--  tests check the documented Ada binding behavior directly.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Gdk.Memory_Texture;
with Gdk.Texture;
with Glib.Bytes;
with Glib.Object;
with Glib.Test;
with Graphene.Rect;
with Gsk.Enums;
with Gsk.Render_Node;
with Gsk.Texture_Scale_Node;
with Gtk.Main;
with System;

procedure Main is
   use Glib.Test;
   use type Glib.Gsize;
   use type Gsk.Enums.Gsk_Scaling_Filter;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  A tiny in-memory texture keeps this node test independent of files.
   Pixels : constant Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
     ((16#11#, 16#22#, 16#33#, 16#FF#, 16#44#, 16#55#, 16#66#, 16#FF#), 8);

   --  Render-node wrapper deallocation releases its owned GSK reference.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);
   procedure Free_Scale_Node is new Ada.Unchecked_Deallocation
     (Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node_Record'Class,
      Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node);

   procedure Test_Constructors_And_Getters with Convention => C;
   procedure Test_Deserialize_Create with Convention => C;

   --  Both constructors initialize a new node; a repeated Initialize must
   --  preserve the original filter and texture on this immutable node.
   procedure Test_Constructors_And_Getters is
      Texture : Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (2, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixels, 8);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 4.0), (Width => 20.0, Height => 10.0));
      Other_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 5.0, Height => 5.0));
      Function_Node : Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node :=
        Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node_New
          (Texture, Bounds'Access, Gsk.Enums.Gsk_Scaling_Filter_Nearest);
      Procedure_Node : Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node;
      Actual_Texture : Gdk.Texture.Gdk_Texture;
   begin
      --  The function constructor records its filter and source texture.
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Function_Node));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
             Gsk.Enums.Gsk_Texture_Scale_Node);
      Assert_True
        (Gsk.Texture_Scale_Node.Get_Filter (Function_Node) =
           Gsk.Enums.Gsk_Scaling_Filter_Nearest);
      Actual_Texture := Gsk.Texture_Scale_Node.Get_Texture (Function_Node);
      Assert_True
        (Glib.Object.Get_Object (Actual_Texture) =
           Glib.Object.Get_Object (Texture));

      --  Initialize is a no-op after construction, even with new parameters.
      Gsk.Texture_Scale_Node.Initialize
        (Function_Node, Texture, Other_Bounds'Access,
         Gsk.Enums.Gsk_Scaling_Filter_Trilinear);
      Assert_True
        (Gsk.Texture_Scale_Node.Get_Filter (Function_Node) =
           Gsk.Enums.Gsk_Scaling_Filter_Nearest);

      --  The procedure constructor is a second public initialization path.
      Gsk.Texture_Scale_Node.Gtk_New
        (Procedure_Node, Texture, Bounds'Access,
         Gsk.Enums.Gsk_Scaling_Filter_Linear);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Procedure_Node));
      Assert_True
        (Gsk.Texture_Scale_Node.Get_Filter (Procedure_Node) =
           Gsk.Enums.Gsk_Scaling_Filter_Linear);
      Actual_Texture := Gsk.Texture_Scale_Node.Get_Texture (Procedure_Node);
      Assert_True
        (Glib.Object.Get_Object (Actual_Texture) =
           Glib.Object.Get_Object (Texture));

      --  The texture belongs to its Ada wrapper; release the node wrappers
      --  explicitly so their finalizers unref each render node once.
      Free_Scale_Node (Procedure_Node);
      Free_Scale_Node (Function_Node);
      Glib.Object.Unref (Glib.Object.GObject (Texture));
   end Test_Constructors_And_Getters;

   --  Deserialization selects a concrete Ada wrapper by the GSK node type,
   --  exercising this node's Create override as well as its public getters.
   procedure Test_Deserialize_Create is
      Texture : Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (2, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixels, 8);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 1.0, Y => 2.0), (Width => 12.0, Height => 6.0));
      Node : Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node :=
        Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node_New
          (Texture, Bounds'Access, Gsk.Enums.Gsk_Scaling_Filter_Nearest);
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Scale : Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node;
      Restored_Texture : Gdk.Texture.Gdk_Texture;
   begin
      --  Texture-scale nodes serialize their texture data along with geometry.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Texture_Scale_Node);

      --  The concrete cast confirms the generic dispatching constructor chose
      --  Gsk.Texture_Scale_Node.Create for the deserialized GSK object.
      Restored_Scale :=
        Gsk.Texture_Scale_Node.Gsk_Texture_Scale_Node (Restored);
      Assert_True
        (Gsk.Texture_Scale_Node.Get_Filter (Restored_Scale) =
           Gsk.Enums.Gsk_Scaling_Filter_Nearest);
      Restored_Texture := Gsk.Texture_Scale_Node.Get_Texture (Restored_Scale);
      Assert_Nonnull (Glib.Object.Get_Object (Restored_Texture));

      Free_Render_Node (Restored);
      Free_Scale_Node (Node);
      Glib.Object.Unref (Glib.Object.GObject (Texture));
   end Test_Deserialize_Create;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/texture-scale-node/constructors-getters",
      Test_Constructors_And_Getters'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/texture-scale-node/deserialize-create",
      Test_Deserialize_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
