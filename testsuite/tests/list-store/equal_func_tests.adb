with Glib;               use Glib;
with Glib.List_Store;    use Glib.List_Store;
with Glib.Object;        use Glib.Object;
with Glib.Simple_Action; use Glib.Simple_Action;
with Glib.Test;          use Glib.Test;
with System;

package body Equal_Func_Tests is
   use type System.Address;

   First, Match, Duplicate, Needle : Gsimple_Action;
   Callback_Count : Guint := 0;
   Callback_Arguments_Valid : Boolean := True;

   --  Use the public Equal_Func profile. Keeping this callback in a library
   --  package avoids a nested-subprogram trampoline, so a failure diagnoses
   --  the binding's object conversion rather than a captured Ada stack frame.
   function Equal_Action_Names
     (A, B : not null access GObject_Record'Class) return Gboolean
   is
   begin
      --  glib-list_store.equal-func-callback-abi: C supplies raw GObjects,
      --  whereas this Ada API promises GtkAda wrappers. Compare addresses
      --  before reading any object fields (even converting to a named access
      --  type can inspect the tag). Return safely on invalid arguments so the
      --  caller can assert the failure and the other test cases can run.
      if B.all'Address /= Needle.all'Address
        or else (A.all'Address /= First.all'Address
                 and then A.all'Address /= Match.all'Address
                 and then A.all'Address /= Duplicate.all'Address)
      then
         Callback_Arguments_Valid := False;
         return 0;
      end if;
      Callback_Count := Callback_Count + 1;

      --  Custom equality must compare values, not just object identity.
      if Get_Name (Gsimple_Action (A)) = Get_Name (Gsimple_Action (B)) then
         return 1;
      else
         return 0;
      end if;
   end Equal_Action_Names;

   --  Search with an independently allocated object having the same name as
   --  two entries. GLib promises the first equal position and puts the search
   --  object second in every callback; also check a miss and optional position.
   procedure Test_Find_With_Equal_Func is
      Store : constant Glist_Store := Glist_Store_New
        (Glib.Simple_Action.Get_Type);
      Position : aliased Guint := Guint'Last;
      Found : Boolean;
   begin
      First := Gsimple_Action_New ("first", null);
      Match := Gsimple_Action_New ("match", null);
      Duplicate := Gsimple_Action_New ("match", null);
      Needle := Gsimple_Action_New ("match", null);
      Store.Append (First);
      Store.Append (Match);
      Store.Append (Duplicate);

      Assert_False (Store.Find (Needle));
      Callback_Count := 0;
      Callback_Arguments_Valid := True;
      Found := Store.Find_With_Equal_Func
        (Needle, Equal_Action_Names'Access, Position'Access);
      Assert_True
        (Callback_Arguments_Valid,
         "Find_With_Equal_Func must pass Ada wrappers to the equality callback");

      --  Once the bridge supplies valid wrappers, check the search contract.
      --  Skip these dependent checks after an argument failure to keep the
      --  diagnostic focused on the callback ABI.
      if Callback_Arguments_Valid then
         Assert_True
           (Found,
            "Find_With_Equal_Func must find a distinct object with an equal name");
         Assert_Cmpuint_Eq (Position, 1);
         Assert_Cmpuint_Eq (Callback_Count, 2);

         Assert_True
           (Store.Find_With_Equal_Func (Needle, Equal_Action_Names'Access),
            "Find_With_Equal_Func must accept an omitted position");
         Glib.Object.Unref (GObject_Record (Needle.all)'Access);
         Needle := Gsimple_Action_New ("absent", null);
         Callback_Count := 0;
         Assert_False
           (Store.Find_With_Equal_Func
              (Needle, Equal_Action_Names'Access, Position'Access),
            "Find_With_Equal_Func must return False when no names match");
         Assert_Cmpuint_Eq (Callback_Count, 3);
      end if;

      --  Append takes its own reference; release both the store's references
      --  and the constructor-owned references when the search checks pass.
      --  The record view selects the base GObject Unref for derived handles.
      Glib.Object.Unref (GObject_Record (Store.all)'Access);
      Glib.Object.Unref (GObject_Record (First.all)'Access);
      Glib.Object.Unref (GObject_Record (Match.all)'Access);
      Glib.Object.Unref (GObject_Record (Duplicate.all)'Access);
      Glib.Object.Unref (GObject_Record (Needle.all)'Access);
   end Test_Find_With_Equal_Func;
end Equal_Func_Tests;
