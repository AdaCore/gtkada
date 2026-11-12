--  Exercise Gsk.Texture_Node's constructors, immutable initialization, and
--  texture getter. No focused C testsuite case covers gsk_texture_node_*.

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
with Gsk.Texture_Node;
with Gtk.Main;
with System;

procedure Main is
   use Glib.Test;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;
   use type System.Address;

   --  Small in-memory textures make this test independent of image files.
   Pixels : constant Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
     ((16#11#, 16#22#, 16#33#, 16#FF#), 4);
   Other_Pixels : constant Glib.Bytes.Gbytes := Glib.Bytes.Gbytes_New
     ((16#AA#, 16#BB#, 16#CC#, 16#FF#), 4);

   --  Deallocating these wrappers releases their owned GSK node references.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Gsk.Texture_Node.Gsk_Texture_Node_Record'Class,
      Gsk.Texture_Node.Gsk_Texture_Node);

   procedure Test_Constructors_And_Get_Texture with Convention => C;

   --  Check both public construction paths and the documented Initialize no-op.
   procedure Test_Constructors_And_Get_Texture is
      Texture : Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (1, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Pixels, 4);
      Replacement : Gdk.Memory_Texture.Gdk_Memory_Texture :=
        Gdk.Memory_Texture.Gdk_Memory_Texture_New
          (1, 1, Gdk.Texture.Gdk_Memory_R8G8B8A8, Other_Pixels, 4);
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 3.0, Y => 4.0), (Width => 20.0, Height => 10.0));
      Other_Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 0.0, Y => 0.0), (Width => 5.0, Height => 5.0));
      Function_Node : Gsk.Texture_Node.Gsk_Texture_Node :=
        Gsk.Texture_Node.Gsk_Texture_Node_New (Texture, Bounds'Access);
      Procedure_Node : Gsk.Texture_Node.Gsk_Texture_Node;
      Actual_Texture : Gdk.Texture.Gdk_Texture;
   begin
      --  The function constructor returns a live GSK texture node.
      Assert_True
        (Gsk.Render_Node.Is_Created
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Function_Node)) =
             Gsk.Enums.Gsk_Texture_Node);
      Actual_Texture := Gsk.Texture_Node.Get_Texture (Function_Node);
      Assert_True
        (Glib.Object.Get_Object (Actual_Texture) =
           Glib.Object.Get_Object (Texture));

      --  Gtk_New is the procedure constructor and initializes its out value.
      Gsk.Texture_Node.Gtk_New
        (Procedure_Node, Texture, Bounds'Access);
      Assert_True
        (Gsk.Render_Node.Is_Created
           (Gsk.Render_Node.Gsk_Render_Node (Procedure_Node)));
      Actual_Texture := Gsk.Texture_Node.Get_Texture (Procedure_Node);
      Assert_True
        (Glib.Object.Get_Object (Actual_Texture) =
           Glib.Object.Get_Object (Texture));

      --  Reinitializing an existing node leaves its original texture intact.
      Gsk.Texture_Node.Initialize
        (Procedure_Node, Replacement, Other_Bounds'Access);
      Actual_Texture := Gsk.Texture_Node.Get_Texture (Procedure_Node);
      Assert_True
        (Glib.Object.Get_Object (Actual_Texture) =
           Glib.Object.Get_Object (Texture));

      --  Drop nodes before their source textures, whose references they hold.
      Free_Node (Procedure_Node);
      Free_Node (Function_Node);
      Glib.Object.Unref (Glib.Object.GObject (Replacement));
      Glib.Object.Unref (Glib.Object.GObject (Texture));
   end Test_Constructors_And_Get_Texture;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Glib.Test.Add_Func
     ("/gsk/texture-node/constructors-get-texture",
      Test_Constructors_And_Get_Texture'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
