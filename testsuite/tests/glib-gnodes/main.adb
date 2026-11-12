--  Exercise GtkAda's Glib.Gnodes wrappers using a small GLib-owned tree.
--  There is no matching GNode test in testsuite/c_tests/ to port.

with Ada.Command_Line;
with System;

with Glib;
with Glib.Enums; use Glib.Enums;
with Glib.Gnodes; use Glib.Gnodes;
with Glib.Test; use Glib.Test;

procedure Main is
   use type Glib.Guint;
   use type Glib.Gint;
   use type Glib.C_Proxy;
   use type Glib.Gnodes.Gnode;
   use type System.Address;

   --  Use GLib's constructor because Glib.Gnodes intentionally exposes tree
   --  navigation and ownership, but does not bind g_node_new itself.
   function New_Node (Data : System.Address) return Gnode;
   pragma Import (C, New_Node, "g_node_new");

   function Insert_Node
     (Parent : Gnode; Position : Glib.Gint; Node : Gnode) return Gnode;
   pragma Import (C, Insert_Node, "g_node_insert");

   type Payload is record
      Value : Integer;
   end record;
   type Payload_Access is access all Payload;

   --  This generic's public operation only converts an Ada access value into
   --  the opaque proxy. Keep the payload separate from the real C tree nodes.
   package Payload_Nodes is new Glib.Gnodes.Gnode_Data
     (Payload, Payload_Access);

   procedure Test_Node_Wrappers with Convention => C;

   --  Check pointer conversion and the root, ancestor, leaf, count, and destroy
   --  behavior on a tree with a root, one child, and one grandchild.
   procedure Test_Node_Wrappers is
      Data        : aliased Payload := (Value => 42);
      Data_Handle : Gnode;
      Root        : Gnode := New_Node (System.Null_Address);
      Child       : Gnode := New_Node (System.Null_Address);
      Leaf        : Gnode := New_Node (System.Null_Address);
   begin
      --  The conversion is checked immediately and the address is never
      --  retained or dereferenced, so Unchecked_Access is bounded here.
      Payload_Nodes.Glib_New (Data_Handle, Data'Unchecked_Access);
      Assert_True (Data_Handle /= null);
      Assert_True
        (Glib.To_Address (Glib.C_Proxy (Data_Handle)) = Data'Address);

      --  Appending transfers the newly allocated nodes into the root's tree.
      Child := Insert_Node (Root, -1, Child);
      Leaf := Insert_Node (Child, -1, Leaf);

      Assert_True (Is_Root (Root));
      Assert_False (Is_Root (Child));
      Assert_True (Is_Ancestor (Root, Leaf));
      Assert_False (Is_Ancestor (Child, Root));
      Assert_False (Is_Leaf (Root));
      Assert_True (Is_Leaf (Leaf));
      Assert_Cmpuint_Eq (N_Nodes (Root, Traverse_All), 3);
      Assert_Cmpuint_Eq (N_Nodes (Root, Traverse_Leafs), 1);

      --  Destroy releases the entire tree and resets the Ada handle to null.
      Destroy (Root);
      Assert_True (Root = null);
   end Test_Node_Wrappers;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/glib-gnodes/node-wrappers", Test_Node_Wrappers'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
