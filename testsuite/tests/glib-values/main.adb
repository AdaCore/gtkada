--  Tests Glib.Values string accessors and the lifetime and bounds handling
--  of its GValues array wrapper.  No C test is dedicated to this GtkAda unit.

with Ada.Command_Line;

with Glib;
with Glib.Test;   use Glib.Test;
with Glib.Values; use Glib.Values;

procedure Main is

   procedure Test_String_Getters with Convention => C;
   procedure Test_Values_Array with Convention => C;

   ------------------------
   -- Test_String_Getters --
   ------------------------

   --  Both overloads return an empty string for a null GValue string and the
   --  stored text for a non-null string.
   procedure Test_String_Getters is
      Value : GValue;
   begin
      Init (Value, Glib.GType_String);
      Assert_Cmpstr_Eq (Get_String (Value), "");
      Assert_Cmpstr_Eq (Get_String (Value, 0), "");

      Set_String (Value, "GtkAda values");
      Assert_Cmpstr_Eq (Get_String (Value), "GtkAda values");
      Assert_Cmpstr_Eq (Get_String (Value, 13), "GtkAda values");
      Unset (Value);
   end Test_String_Getters;

   ----------------------
   -- Test_Values_Array --
   ----------------------

   --  An allocated array can be freed, and requesting an element beyond the
   --  wrapper's declared count raises Constraint_Error before C access.
   procedure Test_Values_Array is
      Values : GValues := Make_Values (1);
      Raised : Boolean := False;
   begin
      begin
         declare
            Ignored : constant GValue := Nth (Values, 2);
            pragma Unreferenced (Ignored);
         begin
            null;
         end;
      exception
         when Constraint_Error =>
            Raised := True;
      end;
      Assert_True (Raised);

      Free (Values);
   end Test_Values_Array;

begin
   Glib.Test.Init;
   Glib.Test.Add_Func ("/glib-values/string-getters",
                       Test_String_Getters'Unrestricted_Access);
   Glib.Test.Add_Func ("/glib-values/values-array",
                       Test_Values_Array'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
