--  Exercise Gsk.Component_Transfer values and Gsk.Component_Transfer_Node
--  construction, accessors, ownership, and deserialization.  No matching C
--  testsuite case covers these APIs to port.

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Gdk.Enums;
with Gdk.RGBA;
with Glib.Bytes;
with Interfaces.C;
with Graphene.Rect;
with Gsk.Color_Node;
with Gsk.Component_Transfer_Node;
with Gsk.Enums;
with Gsk.Render_Node;
with System;

with Glib;      use Glib;
with Glib.Test; use Glib.Test;
with Gsk.Component_Transfer; use Gsk.Component_Transfer;

procedure Main is
   use type System.Address;
   use type Interfaces.C.C_float;
   use type Gsk.Enums.Gsk_Render_Node_Type;
   use type Gsk.Render_Node.Gsk_Render_Node;

   procedure Test_Constructors_And_Ownership
   with Convention => C;

   --  Ada storage for From_Object_Free must come from GLib's allocator because
   --  that helper releases the wrapper record with g_free after copying it.
   type Transfer_Access is access all Gsk_Component_Transfer;
   function To_Transfer_Access is new Ada.Unchecked_Conversion
     (System.Address, Transfer_Access);

   --  Child accessors return a new Ada wrapper that owns one GSK reference.
   procedure Free_Render_Node is new Ada.Unchecked_Deallocation
     (Gsk.Render_Node.Gsk_Render_Node_Record'Class,
      Gsk.Render_Node.Gsk_Render_Node);

   procedure Test_Node_Constructors_And_Accessors
   with Convention => C;
   procedure Test_Node_Deserialize_Create with Convention => C;

   procedure Test_Constructors_And_Ownership is
      function G_Malloc (Size : Gsize) return System.Address;
      pragma Import (C, G_Malloc, "g_malloc");

      Values : constant Gfloat_Array := (0 => 0.0, 1 => 0.5, 2 => 1.0);
      By_Function_Discrete : Gsk_Component_Transfer;
      By_Procedure_Discrete : Gsk_Component_Transfer;
      By_Function_Gamma    : Gsk_Component_Transfer;
      By_Procedure_Gamma   : Gsk_Component_Transfer;
      By_Function_Identity : Gsk_Component_Transfer;
      By_Procedure_Identity : Gsk_Component_Transfer;
      By_Function_Levels   : Gsk_Component_Transfer;
      By_Procedure_Levels  : Gsk_Component_Transfer;
      By_Function_Linear   : Gsk_Component_Transfer;
      By_Procedure_Linear  : Gsk_Component_Transfer;
      By_Function_Table    : Gsk_Component_Transfer;
      By_Procedure_Table   : Gsk_Component_Transfer;
      Duplicate            : Gsk_Component_Transfer;
      Converted            : Gsk_Component_Transfer;
   begin
      --  The function and procedure forms of each constructor must describe
      --  equal transfer functions for the same parameters.
      By_Function_Discrete := Gsk_Component_Transfer_New_Discrete (3, Values);
      Gtk_New_Discrete (By_Procedure_Discrete, 3, Values);
      Assert_True (Equal (Get_Object (By_Function_Discrete),
                          Get_Object (By_Procedure_Discrete)));

      By_Function_Gamma := Gsk_Component_Transfer_New_Gamma (1.0, 2.0, 0.1);
      Gtk_New_Gamma (By_Procedure_Gamma, 1.0, 2.0, 0.1);
      Assert_True (Equal (Get_Object (By_Function_Gamma),
                          Get_Object (By_Procedure_Gamma)));

      By_Function_Identity := Gsk_Component_Transfer_New_Identity;
      Gtk_New_Identity (By_Procedure_Identity);
      Assert_True (Equal (Get_Object (By_Function_Identity),
                          Get_Object (By_Procedure_Identity)));

      By_Function_Levels := Gsk_Component_Transfer_New_Levels (4.0);
      Gtk_New_Levels (By_Procedure_Levels, 4.0);
      Assert_True (Equal (Get_Object (By_Function_Levels),
                          Get_Object (By_Procedure_Levels)));

      By_Function_Linear := Gsk_Component_Transfer_New_Linear (1.5, -0.1);
      Gtk_New_Linear (By_Procedure_Linear, 1.5, -0.1);
      Assert_True (Equal (Get_Object (By_Function_Linear),
                          Get_Object (By_Procedure_Linear)));

      By_Function_Table := Gsk_Component_Transfer_New_Table (3, Values);
      Gtk_New_Table (By_Procedure_Table, 3, Values);
      Assert_True (Equal (Get_Object (By_Function_Table),
                          Get_Object (By_Procedure_Table)));

      --  Copy creates an equal independent value; a different transform is
      --  unequal, which checks both outcomes of the equality wrapper.
      Duplicate := By_Function_Gamma.Copy;
      Assert_True (Equal (Get_Object (By_Function_Gamma),
                          Get_Object (Duplicate)));
      Assert_False (Equal (Get_Object (By_Function_Gamma),
                           Get_Object (By_Function_Linear)));

      --  From_Object_Free consumes a GLib-allocated Ada wrapper record while
      --  preserving the boxed pointer held in the returned value.
      declare
         Wrapper_Bytes : constant Gsize :=
           Gsize ((Gsk_Component_Transfer'Size + System.Storage_Unit - 1)
                  / System.Storage_Unit);
         Wrapper : constant Transfer_Access :=
           To_Transfer_Access (G_Malloc (Wrapper_Bytes));
      begin
         Wrapper.all.Set_Object (Get_Object (By_Function_Identity));
         Converted := From_Object_Free (Wrapper);
      end;
      Assert_True (Equal (Get_Object (By_Function_Identity),
                          Get_Object (Converted)));

      --  Free releases every owned C transfer exactly once, including the
      --  copied value and the wrapper converted above.
      Free (By_Function_Discrete);
      Free (By_Procedure_Discrete);
      Free (By_Function_Gamma);
      Free (By_Procedure_Gamma);
      Free (By_Function_Identity);
      Free (By_Function_Levels);
      Free (By_Procedure_Levels);
      Free (By_Function_Linear);
      Free (By_Procedure_Linear);
      Free (By_Function_Table);
      Free (By_Procedure_Table);
      Free (Duplicate);
   end Test_Constructors_And_Ownership;

   --  Both public constructors must retain the child and channel transfers.
   procedure Test_Node_Constructors_And_Accessors is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 2.0, Y => 3.0), (Width => 24.0, Height => 18.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.2, 0.4, 0.6, 1.0);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Red : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Linear (1.2, 0.1);
      Green : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Gamma (1.0, 2.0, 0.05);
      Blue : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Levels (4.0);
      Alpha : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Identity;
      Other : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Linear (0.5, 0.0);
      Node : Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node;
      Gtk_Node : Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node;
      Actual_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  Distinct transfer values reveal swapped or ignored channel inputs.
      Node := Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node_New
        (Child, Red, Green, Blue, Alpha);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Node)) =
             Gsk.Enums.Gsk_Component_Transfer_Node);
      Actual_Child := Gsk.Component_Transfer_Node.Get_Child (Node);
      Assert_True
        (Gsk.Render_Node.Get_Object (Actual_Child) =
           Gsk.Render_Node.Get_Object (Child));
      Free_Render_Node (Actual_Child);

      --  Transfer getters are borrowed boxed values; compare them while the
      --  node remains alive and do not free the returned Ada wrappers.
      --  Equal compares the transfer values rather than wrapper addresses.
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Node, Gdk.Enums.Gdk_Color_Channel_Red)), Get_Object (Red)));
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Node, Gdk.Enums.Gdk_Color_Channel_Green)), Get_Object (Green)));
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Node, Gdk.Enums.Gdk_Color_Channel_Blue)), Get_Object (Blue)));
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Node, Gdk.Enums.Gdk_Color_Channel_Alpha)), Get_Object (Alpha)));

      --  Reinitializing an immutable node must leave its first transfer set.
      Gsk.Component_Transfer_Node.Initialize
        (Node, Child, Other, Other, Other, Other);
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Node, Gdk.Enums.Gdk_Color_Channel_Red)), Get_Object (Red)));

      --  Gtk_New exercises the procedure constructor through Initialize too.
      Gsk.Component_Transfer_Node.Gtk_New
        (Gtk_Node, Child, Other, Other, Other, Other);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type
           (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node)) =
             Gsk.Enums.Gsk_Component_Transfer_Node);
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Gtk_Node, Gdk.Enums.Gdk_Color_Channel_Blue)), Get_Object (Other)));

      --  Release node references before freeing caller-owned transfer values.
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Gtk_Node));
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Render_Node (Child);
      Free (Red);
      Free (Green);
      Free (Blue);
      Free (Alpha);
      Free (Other);
   end Test_Node_Constructors_And_Accessors;

   --  Round-tripping through deserialization exercises the concrete Create
   --  hook that wraps a generic GSK node as a GtkAda component-transfer node.
   procedure Test_Node_Deserialize_Create is
      Bounds : aliased Graphene.Rect.Graphene_Rect_T :=
        ((X => 4.0, Y => 5.0), (Width => 32.0, Height => 20.0));
      Color : aliased Gdk.RGBA.Gdk_RGBA := (0.7, 0.3, 0.1, 0.8);
      Child_Node : Gsk.Color_Node.Gsk_Color_Node :=
        Gsk.Color_Node.Gsk_Color_Node_New (Color'Access, Bounds'Access);
      Child : Gsk.Render_Node.Gsk_Render_Node :=
        Gsk.Render_Node.Gsk_Render_Node (Child_Node);
      Red : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Linear (1.1, 0.05);
      Green : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Gamma (1.0, 1.5, 0.0);
      Blue : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Levels (5.0);
      Alpha : Gsk_Component_Transfer :=
        Gsk_Component_Transfer_New_Identity;
      Node : Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node :=
        Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node_New
          (Child, Red, Green, Blue, Alpha);
      Serialized : Glib.Bytes.Gbytes;
      Restored : Gsk.Render_Node.Gsk_Render_Node;
      Restored_Node : Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node;
      Restored_Child : Gsk.Render_Node.Gsk_Render_Node;
   begin
      --  The in-memory byte round trip needs neither a renderer nor files.
      Serialized := Gsk.Render_Node.Serialize
        (Gsk.Render_Node.Gsk_Render_Node (Node));
      Assert_True (Serialized.Get_Size > 0);
      Restored := Gsk.Render_Node.Deserialize (Serialized, null);
      Assert_Nonnull (Gsk.Render_Node.Get_Object (Restored));
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored) =
           Gsk.Enums.Gsk_Component_Transfer_Node);

      --  The cast verifies the dynamic wrapper type and the restored getters.
      Restored_Node :=
        Gsk.Component_Transfer_Node.Gsk_Component_Transfer_Node (Restored);
      Restored_Child := Gsk.Component_Transfer_Node.Get_Child (Restored_Node);
      Assert_True
        (Gsk.Render_Node.Get_Node_Type (Restored_Child) =
           Gsk.Enums.Gsk_Color_Node);
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Restored_Node, Gdk.Enums.Gdk_Color_Channel_Red)), Get_Object (Red)));
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Restored_Node, Gdk.Enums.Gdk_Color_Channel_Green)), Get_Object (Green)));
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Restored_Node, Gdk.Enums.Gdk_Color_Channel_Blue)), Get_Object (Blue)));
      Assert_True (Equal
        (Get_Object (Gsk.Component_Transfer_Node.Get_Transfer
           (Restored_Node, Gdk.Enums.Gdk_Color_Channel_Alpha)), Get_Object (Alpha)));

      --  Deserialization and Get_Child each yield an owned Ada node reference.
      Free_Render_Node (Restored_Child);
      Free_Render_Node (Restored);
      Free_Render_Node (Gsk.Render_Node.Gsk_Render_Node (Node));
      Free_Render_Node (Child);
      Free (Red);
      Free (Green);
      Free (Blue);
      Free (Alpha);
   end Test_Node_Deserialize_Create;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/gsk/component-transfer/constructors-and-ownership",
      Test_Constructors_And_Ownership'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/component-transfer-node/constructors-and-accessors",
      Test_Node_Constructors_And_Accessors'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/gsk/component-transfer-node/deserialize-create",
      Test_Node_Deserialize_Create'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
