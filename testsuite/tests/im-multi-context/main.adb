--  Exercises Gtk.IM_Multi_Context constructors and context-id operations.
--  GtkIMMulticontext has no dedicated C test; testsuite/c_tests/imcontext.c
--  covers only the parent GtkIMContext interface.

with Ada.Command_Line;
with Glib.Test;
with Gtk.IM_Multi_Context;
with Gtk.Main;

procedure Main is
   use Glib.Test;
   use Gtk.IM_Multi_Context;

   procedure Test_Constructors with Convention => C;
   procedure Test_Context_Id with Convention => C;

   procedure Test_Constructors is
      Via_Function  : constant Gtk_IM_Multi_Context :=
        Gtk_IM_Multi_Context_New;
      Via_Procedure : Gtk_IM_Multi_Context;
   begin
      --  Both public constructors create usable objects with a readable id.
      Gtk_New (Via_Procedure);
      Assert_True (Via_Function /= null);
      Assert_True (Via_Procedure /= null);
      Assert_Cmpstr_Eq
        (Via_Function.Get_Context_Id, Via_Procedure.Get_Context_Id);

      --  Initialize is documented as a no-op for an already-created object.
      declare
         Original_Id : constant Glib.UTF8_String :=
           Via_Procedure.Get_Context_Id;
      begin
         Gtk.IM_Multi_Context.Initialize (Via_Procedure);
         Assert_Cmpstr_Eq (Via_Procedure.Get_Context_Id, Original_Id);
      end;
   end Test_Constructors;

   procedure Test_Context_Id is
      Context : constant Gtk_IM_Multi_Context := Gtk_IM_Multi_Context_New;
   begin
      --  GTK's built-in simple input method uses this context id.
      Context.Set_Context_Id ("gtk-im-context-simple");
      Assert_Cmpstr_Eq (Context.Get_Context_Id, "gtk-im-context-simple");

      --  An empty id selects GTK's configured default and uses the null-pointer
      --  branch in the binding. Read it twice to verify the reset is stable.
      Context.Set_Context_Id;
      declare
         Default_Id : constant Glib.UTF8_String := Context.Get_Context_Id;
      begin
         Assert_Cmpstr_Eq (Default_Id, "gtk-im-context-simple");
         Assert_Cmpstr_Eq (Context.Get_Context_Id, Default_Id);
      end;
   end Test_Context_Id;

begin
   Glib.Test.Init;
   Gtk.Main.Init;
   Add_Func ("/im-multi-context/constructors", Test_Constructors'Unrestricted_Access);
   Add_Func ("/im-multi-context/context-id", Test_Context_Id'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
