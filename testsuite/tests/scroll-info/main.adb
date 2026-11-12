--  Check Gtk.Scroll_Info construction, axis settings, reference management,
--  and the wrapper conversion helper. There is no matching C test to port.

with Ada.Command_Line;
with Glib; use Glib;
with Glib.Test; use Glib.Test;
with Gtk.Scroll_Info; use Gtk.Scroll_Info;
with System;
with System.Address_To_Access_Conversions;

procedure Main is
   use type System.Address;

   --  From_Object_Free takes a GLib-allocated Ada wrapper and frees that
   --  wrapper after copying it; allocate its storage with the same allocator.
   function G_Malloc (Size : Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");
   package Scroll_Info_Conversion is new
     System.Address_To_Access_Conversions (Gtk_Scroll_Info);

   procedure Test_Scroll_Info with Convention => C;

   --  Both constructors should produce usable objects, and each setter should
   --  round trip both Boolean values through its matching getter.
   procedure Test_Scroll_Info is
      By_Procedure : Gtk_Scroll_Info;
      By_Function  : Gtk_Scroll_Info := Gtk_Scroll_Info_New;
      Held         : Gtk_Scroll_Info;
   begin
      Gtk_New (By_Procedure);
      Assert_True (Get_Type /= 0);
      Assert_Nonnull (Get_Object (By_Procedure));
      Assert_Nonnull (Get_Object (By_Function));

      --  Exercise both directions for each axis and check through an extra
      --  reference that the objects still identify the same native value.
      Set_Enable_Horizontal (By_Function, False);
      Assert_False (Get_Enable_Horizontal (By_Function));
      Set_Enable_Horizontal (By_Function, True);
      Assert_True (Get_Enable_Horizontal (By_Function));
      Set_Enable_Vertical (By_Function, True);
      Assert_True (Get_Enable_Vertical (By_Function));
      Set_Enable_Vertical (By_Function, False);
      Assert_False (Get_Enable_Vertical (By_Function));

      --  Ref returns an owned reference to the same object; balance it below.
      Held := Ref (By_Function);
      Assert_True (Get_Object (Held) = Get_Object (By_Function));
      Assert_False (Get_Enable_Vertical (Held));

      --  Copy the Ada wrapper into GLib memory to exercise the documented
      --  conversion helper's copy-and-free ownership behavior safely.
      declare
         Address : constant System.Address :=
           G_Malloc
             (Gsize
                ((Gtk_Scroll_Info'Size + System.Storage_Unit - 1)
                 / System.Storage_Unit));
         Wrapper : constant Scroll_Info_Conversion.Object_Pointer :=
           Scroll_Info_Conversion.To_Pointer (Address);
         Converted : Gtk_Scroll_Info;
      begin
         Wrapper.all := By_Function;
         Converted := From_Object_Free (Wrapper);
         Assert_True (Get_Object (Converted) = Get_Object (By_Function));
         Assert_True (Get_Enable_Horizontal (Converted));
      end;

      --  The conversion result borrows the source pointer, so release only
      --  the references actually acquired from the GTK constructors and Ref.
      Unref (Held);
      Unref (By_Function);
      Unref (By_Procedure);
   end Test_Scroll_Info;

begin
   Glib.Test.Init;
   Add_Func ("/gtk/scroll-info", Test_Scroll_Info'Unrestricted_Access);
   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Main;
