--  Exercise Gtk.Tree_Row_Reference, including the proxy notifications used
--  by Gtk's tree model machinery.  The proxy checks port the row-reference
--  behavior used by ref_count_row_ref in testsuite/c_tests/filtermodel.c and
--  testsuite/c_tests/sortmodel.c; no standalone C test covers this API.

pragma Warnings (Off, "*obsolescent*");

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with System;

with Glib;                  use Glib;
with Glib.Test;             use Glib.Test;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Row_Reference; use Gtk.Tree_Row_Reference;
with Gtk.Tree_Store;        use Gtk.Tree_Store;

procedure Main is
   use type Gtk.Tree_Model.Gtk_Tree_Model;
   use type System.Address;

   procedure Test_Model_References with Convention => C;
   procedure Test_Proxy_Notifications with Convention => C;

   --  From_Object_Free releases its temporary Ada wrapper with g_free, so the
   --  wrapper storage must come from GLib's allocator rather than `new`.
   type Row_Reference_Access is access all Gtk_Tree_Row_Reference;
   function To_Row_Reference_Access is new Ada.Unchecked_Conversion
     (System.Address, Row_Reference_Access);
   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   ----------------------------
   -- Test_Model_References --
   ----------------------------

   --  The model-backed constructors follow the same row across insertion and
   --  invalidate it when that row is removed.  Both overloads and ownership
   --  helpers are checked against the observable path and model.
   procedure Test_Model_References is
      Store       : Gtk_Tree_Store;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      First       : Gtk_Tree_Iter;
      Second      : Gtk_Tree_Iter;
      Inserted    : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
      Current     : Gtk_Tree_Path;
      By_Proc     : Gtk_Tree_Row_Reference;
      By_Func     : Gtk_Tree_Row_Reference;
      Duplicate   : Gtk_Tree_Row_Reference;
      Invalid     : Gtk_Tree_Row_Reference;
      Converted   : Gtk_Tree_Row_Reference;
      Wrapper     : Row_Reference_Access;
   begin
      Gtk_New (Store, (0 => GType_Int));
      Store.Append (First, Null_Iter);
      Store.Append (Second, Null_Iter);
      Model := +Store;

      Path := Gtk_Tree_Path_New_From_String ("1");
      Gtk_New (By_Proc, Model, Path);
      By_Func := Gtk_Tree_Row_Reference_New (Model, Path);
      Path_Free (Path);

      Assert_True (Valid (By_Proc));
      Assert_True (Valid (By_Func));
      Assert_True (Get_Model (By_Proc) = Model);
      Current := Get_Path (By_Proc);
      Assert_Cmpstr_Eq (To_String (Current), "1");
      Path_Free (Current);

      --  Copy returns an independently freeable reference to the same row.
      Duplicate := Copy (By_Func);
      Assert_True (Valid (Duplicate));
      Current := Get_Path (Duplicate);
      Assert_Cmpstr_Eq (To_String (Current), "1");
      Path_Free (Current);

      --  The generated ownership converter copies the boxed handle out of a
      --  GLib-allocated wrapper and releases only that wrapper allocation.
      declare
         Bytes : constant Gsize :=
           Gsize ((Gtk_Tree_Row_Reference'Size + System.Storage_Unit - 1)
                  / System.Storage_Unit);
      begin
         Wrapper := To_Row_Reference_Access (G_Malloc (Bytes));
      end;
      Wrapper.all := Copy (By_Proc);
      Converted := From_Object_Free (Wrapper);
      Current := Get_Path (Converted);
      Assert_Cmpstr_Eq (To_String (Current), "1");
      Path_Free (Current);
      Free (Converted);

      --  Creating a reference for a nonexistent path returns an invalid
      --  reference; Get_Path then returns the null boxed path.
      Path := Gtk_Tree_Path_New_From_String ("9");
      Gtk_New (Invalid, Model, Path);
      Assert_False (Valid (Invalid));
      --  GTK returns a null reference here; Get_Path has a non-null-reference
      --  precondition, so only Valid and Free are applicable to this result.
      Free (Invalid);
      Path_Free (Path);

      --  Inserting before the referenced row shifts its path; deleting that
      --  row invalidates every model-connected reference to it.
      Store.Insert (Inserted, Null_Iter, 0);
      Current := Get_Path (By_Proc);
      Assert_Cmpstr_Eq (To_String (Current), "2");
      Path_Free (Current);
      Current := Get_Path (Duplicate);
      Assert_Cmpstr_Eq (To_String (Current), "2");
      Path_Free (Current);

      Store.Remove (Second);
      Assert_False (Valid (By_Proc));
      Assert_False (Valid (By_Func));
      Assert_False (Valid (Duplicate));
      Current := Get_Path (By_Proc);
      Assert_True (Get_Object (Current) = System.Null_Address);
      Path_Free (Current);

      --  The public null constant is accepted by Valid and Free.
      Assert_False (Valid (Null_Gtk_Tree_Row_Reference));
      Free (Null_Gtk_Tree_Row_Reference);
      Free (By_Proc);
      Free (By_Func);
      Free (Duplicate);
   end Test_Model_References;

   ---------------------------------
   -- Test_Proxy_Notifications --
   ---------------------------------

   --  Proxy references do not listen to model signals themselves.  The caller
   --  reports each insert, reorder, and deletion exactly once through the
   --  matching notification function, as GTK's model proxies do.
   procedure Test_Proxy_Notifications is
      Store       : Gtk_Tree_Store;
      Proxy       : Gtk_Tree_Store;
      Model       : Gtk.Tree_Model.Gtk_Tree_Model;
      Parent      : Gtk_Tree_Iter;
      First       : Gtk_Tree_Iter;
      Second      : Gtk_Tree_Iter;
      Inserted    : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;
      Current     : Gtk_Tree_Path;
      By_Proc     : Gtk_Tree_Row_Reference;
      By_Func     : Gtk_Tree_Row_Reference;
   begin
      Gtk_New (Store, (0 => GType_Int));
      Gtk_New (Proxy, (0 => GType_Int));
      Store.Append (Parent, Null_Iter);
      Store.Append (First, Parent);
      Store.Append (Second, Parent);
      Model := +Store;

      Path := Gtk_Tree_Path_New_From_String ("0:1");
      Gtk_New_Proxy (By_Proc, Proxy, Model, Path);
      By_Func := Gtk_Tree_Row_Reference_New_Proxy (Proxy, Model, Path);
      Path_Free (Path);
      Assert_True (Valid (By_Proc));
      Assert_True (Valid (By_Func));

      --  Insert a new first child, then report its path so both references
      --  continue to identify the original second child.
      Store.Insert (Inserted, Parent, 0);
      Path := Gtk_Tree_Path_New_From_String ("0:0");
      Gtk.Tree_Row_Reference.Inserted (Proxy, Path);
      Path_Free (Path);
      Current := Get_Path (By_Proc);
      Assert_Cmpstr_Eq (To_String (Current), "0:2");
      Path_Free (Current);

      --  Deleting the tracked row and reporting the deletion invalidates both
      --  proxy references, matching the model notification contract.
      Path := Gtk_Tree_Path_New_From_String ("0:2");
      Store.Remove (Second);
      Gtk.Tree_Row_Reference.Deleted (Proxy, Path);
      Path_Free (Path);
      Assert_False (Valid (By_Proc));
      Assert_False (Valid (By_Func));

      Free (By_Proc);
      Free (By_Func);
   end Test_Proxy_Notifications;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func
     ("/tree-row-reference/model", Test_Model_References'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/tree-row-reference/proxy", Test_Proxy_Notifications'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
