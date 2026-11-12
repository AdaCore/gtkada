--  Headless tests for Gtk.Color_Dialog and Gtk.Color_Dialog_Button.
--
--  The asynchronous Gtk.Color_Dialog.Choose_Rgba flow needs user interaction,
--  so these cases cover dialog properties, button construction and settings,
--  accessible methods and interfaces, and the activate signal.
--  The constructor check ports misc.c:test_color_dialog_button_new, which
--  verifies that gtk_color_dialog_button_new accepts a null dialog.

with Ada.Command_Line;
with Ada.Unchecked_Conversion;
with Glib;                    use Glib;
with Glib.Cancellable;        use Glib.Cancellable;
with Glib.Error;              use Glib.Error;
with Glib.Test;               use Glib.Test;
with Glib.Main;
with Glib.Object;             use Glib.Object;
with Glib.Properties;         use Glib.Properties;
with Gdk.RGBA;                use Gdk.RGBA;
with Gtk.Accessible;          use Gtk.Accessible;
with Gtk.Atcontext;
with Gtk.Buildable;
with Gtk.Color_Dialog;        use Gtk.Color_Dialog;
with Gtk.Color_Dialog_Button; use Gtk.Color_Dialog_Button;
with Gtk.Constraint_Target;
with Gtk.Main;
with Gtkada.Types;            use Gtkada.Types;
with System;

use type Gtk.Accessible.Gtk_Accessible;
use type Gtk.Atcontext.Gtk_Atcontext;
use type Gtk.Buildable.Gtk_Buildable;
use type Gtk.Constraint_Target.Gtk_Constraint_Target;
use type Glib.Error.GError;
use type System.Address;

procedure Color_Dialog is
   Typed_Activations : Natural := 0;
   Slot_Activations  : Natural := 0;
   --  The slot overload receives a separate object supplied to On_Activate.
   Slot_Object       : Gtk_Color_Dialog_Button;

   Async_Dialog : Gtk_Color_Dialog;
   Async_Done   : Boolean := False;

   --  GtkAda supplies a C helper for emitting void signals by name.
   procedure Emit_Void (Object : System.Address; Name : Chars_Ptr);
   pragma Import (C, Emit_Void, "ada_g_signal_emit_by_name");

   --  A completed GTask with GtkColorDialog's source tag lets the finish
   --  method be checked without presenting an interactive chooser window.
   --  GtkAda keeps GTask and GAsyncResult opaque, so this fixture uses their
   --  C entry points to create the same kind of result the chooser returns.
   procedure Choose_Rgba_Tag
     (Self          : System.Address;
      Parent        : System.Address;
      Initial_Color : System.Address;
      Cancellable   : System.Address;
      Callback      : System.Address;
      User_Data     : System.Address);
   pragma Import (C, Choose_Rgba_Tag, "gtk_color_dialog_choose_rgba");

   function G_Task_New
     (Source_Object : System.Address;
      Cancellable   : System.Address;
      Callback      : System.Address;
      User_Data     : System.Address) return System.Address;
   pragma Import (C, G_Task_New, "g_task_new");

   procedure G_Task_Set_Source_Tag
     (Task_Handle, Source_Tag : System.Address);
   pragma Import (C, G_Task_Set_Source_Tag, "g_task_set_source_tag");

   procedure G_Task_Return_Pointer
     (Task_Handle, Result, Result_Destroy : System.Address);
   pragma Import (C, G_Task_Return_Pointer, "g_task_return_pointer");

   procedure G_Object_Unref (Object : System.Address);
   pragma Import (C, G_Object_Unref, "g_object_unref");

   function G_Malloc (Size : Glib.Gsize) return System.Address;
   pragma Import (C, G_Malloc, "g_malloc");

   type RGBA_Access is access all Gdk_RGBA;
   function To_RGBA_Access is new Ada.Unchecked_Conversion
     (System.Address, RGBA_Access);
   function To_Async_Result is new Ada.Unchecked_Conversion
     (System.Address, G_Async_Result);

   procedure Test_Dialog_Properties
   with Convention => C;

   procedure Test_Button_Rgba_Round_Trip
   with Convention => C;

   procedure Test_Button_Constructors_And_Dialog
   with Convention => C;

   procedure Test_Button_Accessibility_And_Interfaces
   with Convention => C;

   procedure Test_Button_Activate_Signal
   with Convention => C;

   procedure Test_Dialog_Cancelled_Choose
   with Convention => C;

   procedure Test_Dialog_Finish_Success
   with Convention => C;

   procedure On_Choose_Rgba
     (Source : access GObject_Record'Class;
      Result : G_Async_Result);

   procedure On_Activate_Typed
     (Self : access Gtk_Color_Dialog_Button_Record'Class);

   procedure On_Activate_Slot
     (Self : access GObject_Record'Class);

   --  Check the typed marshaller supplies the button which emitted activate.
   procedure On_Activate_Typed
     (Self : access Gtk_Color_Dialog_Button_Record'Class) is
   begin
      Assert_Nonnull (Self.all'Address);
      Typed_Activations := Typed_Activations + 1;
   end On_Activate_Typed;

   --  Check the slot marshaller supplies the object registered with the slot.
   procedure On_Activate_Slot
     (Self : access GObject_Record'Class) is
   begin
      Assert_True (Self = GObject (Slot_Object));
      Slot_Activations := Slot_Activations + 1;
   end On_Activate_Slot;

   --  Finishing a cancelled chooser must return the documented empty colour
   --  and an error; forwarding this result also tests the generated callback
   --  trampoline's source-object and async-result arguments.
   procedure On_Choose_Rgba
     (Source : access GObject_Record'Class;
      Result : G_Async_Result)
   is
      Error : GError;
      Color : Gdk_RGBA;
   begin
      Assert_True (Source.Get_Object = Async_Dialog.Get_Object);
      Color := Async_Dialog.Choose_Rgba_Finish (Result, Error);
      Assert_True (Error /= null);
      Assert_Cmpfloat_Eq (Gdouble (Color.Red), 0.0);
      Assert_Cmpfloat_Eq (Gdouble (Color.Green), 0.0);
      Assert_Cmpfloat_Eq (Gdouble (Color.Blue), 0.0);
      Assert_Cmpfloat_Eq (Gdouble (Color.Alpha), 0.0);
      Error_Free (Error);
      Async_Done := True;
   end On_Choose_Rgba;

   ------------------------------
   -- Test_Dialog_Properties --
   ------------------------------

   procedure Test_Dialog_Properties is
      Dialog : Gtk_Color_Dialog;
      Initial_Modal : Boolean;
   begin
      Gtk_New (Dialog);
      Assert_Nonnull (Dialog.all'Address);

      Dialog.Set_Title ("Choose a colour");
      Assert_Cmpstr_Eq (Dialog.Get_Title, "Choose a colour");

      --  Toggle the platform-provided default in both directions and verify
      --  that the getter reports each value set by the Ada binding.
      Initial_Modal := Dialog.Get_Modal;
      Dialog.Set_Modal (not Initial_Modal);
      Assert_True (Dialog.Get_Modal = not Initial_Modal);
      Dialog.Set_Modal (Initial_Modal);
      Assert_True (Dialog.Get_Modal = Initial_Modal);

      Dialog.Set_With_Alpha (True);
      Assert_True (Dialog.Get_With_Alpha);

      Dialog.Set_With_Alpha (False);
      Assert_False (Dialog.Get_With_Alpha);
   end Test_Dialog_Properties;

   ----------------------------------
   -- Test_Button_Rgba_Round_Trip --
   ----------------------------------

   procedure Test_Button_Rgba_Round_Trip is
      Dialog : Gtk_Color_Dialog;
      Button : Gtk_Color_Dialog_Button;
      Color  : constant Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
      Got    : Gdk_RGBA;
   begin
      Gtk_New (Dialog);
      Gtk_New (Button, Dialog);

      --  The button must report the dialog it was created with.
      Assert_True (Button.Get_Dialog = Dialog);

      Button.Set_Rgba (Color);
      Got := Button.Get_Rgba;

      Assert_Cmpfloat_Eq (Gdouble (Got.Red), Gdouble (Color.Red));
      Assert_Cmpfloat_Eq (Gdouble (Got.Green), Gdouble (Color.Green));
      Assert_Cmpfloat_Eq (Gdouble (Got.Blue), Gdouble (Color.Blue));
      Assert_Cmpfloat_Eq (Gdouble (Got.Alpha), Gdouble (Color.Alpha));
   end Test_Button_Rgba_Round_Trip;

   --  Port of misc.c:test_color_dialog_button_new, with both Ada constructors.
   --  The supplied dialog is null, matching the original C regression check.
   procedure Test_Button_Constructors_And_Dialog is
      By_Procedure : Gtk_Color_Dialog_Button;
      By_Function  : constant Gtk_Color_Dialog_Button :=
        Gtk_Color_Dialog_Button_New (null);
      Dialog       : constant Gtk_Color_Dialog := Gtk_Color_Dialog_New;
      Dialog2      : constant Gtk_Color_Dialog := Gtk_Color_Dialog_New;
   begin
      Gtk_New (By_Procedure, null);
      Ref_Sink (By_Procedure);
      Ref_Sink (By_Function);

      --  Constructors return initialized widgets with no dialog attached.
      Assert_Nonnull (By_Procedure.all'Address);
      Assert_Nonnull (By_Function.all'Address);
      Assert_True (By_Procedure.Get_Dialog = null);
      Assert_True (By_Function.Get_Dialog = null);

      --  Initialize is a no-op after Gtk_New has already created the object.
      By_Procedure.Initialize (null);
      By_Procedure.Set_Dialog (Dialog);
      Assert_True (By_Procedure.Get_Dialog = Dialog);
      By_Procedure.Set_Dialog (Dialog2);
      Assert_True (By_Procedure.Get_Dialog = Dialog2);

      Unref (By_Procedure);
      Unref (By_Function);
      Unref (Dialog);
      Unref (Dialog2);
   end Test_Button_Constructors_And_Dialog;

   --  Accessibility queries on an unrooted button have stable empty results.
   procedure Test_Button_Accessibility_And_Interfaces is
      Button : constant Gtk_Color_Dialog_Button :=
        Gtk_Color_Dialog_Button_New (null);
      Parent  : Gtk_Accessible;
      Next    : Gtk_Accessible;
      First   : Gtk_Accessible;
      Context : Gtk.Atcontext.Gtk_Atcontext;
      X, Y, Width, Height : Gint;
      Has_Bounds : Boolean;
      Accessible : constant Gtk_Accessible := +Button;
      Buildable : constant Gtk.Buildable.Gtk_Buildable := +Button;
      Constraint : constant Gtk.Constraint_Target.Gtk_Constraint_Target := +Button;
   begin
      Ref_Sink (Button);
      Assert_True (Button.Get_Accessible_Role /= Accessible_Role_None);
      Assert_Cmpstr_Eq (Button.Get_Accessible_Id, "");
      Assert_True (Button.Get_Accessible_Parent = Null_Gtk_Accessible);
      Assert_True (Button.Get_Next_Accessible_Sibling = Null_Gtk_Accessible);

      --  The native button has an accessible child, even while unrooted.
      First := Button.Get_First_Accessible_Child;
      Assert_True (First /= Null_Gtk_Accessible);
      Context := Button.Get_At_Context;
      Assert_True (Context = null);
      Has_Bounds := Button.Get_Bounds (X, Y, Width, Height);
      Assert_False (Has_Bounds);
      Assert_False
        (Button.Get_Platform_State (Accessible_Platform_State_Focused));

      --  Null relationships keep these calls independent of an accessibility
      --  backend or a widget tree; the updates are valid for an unrooted one.
      Button.Announce ("color button test", Accessible_Announcement_Priority_Low);
      Button.Reset_Property (Accessible_Property_Label);
      Button.Reset_Relation (Accessible_Relation_Labelled_By);
      Button.Reset_State (Accessible_State_Busy);
      Button.Update_Platform_State (Accessible_Platform_State_Focusable);
      Button.Set_Accessible_Parent (Null_Gtk_Accessible, Null_Gtk_Accessible);
      Parent := Button.Get_Accessible_Parent;
      Assert_True (Parent = Null_Gtk_Accessible);
      Button.Update_Next_Accessible_Sibling (Null_Gtk_Accessible);
      Next := Button.Get_Next_Accessible_Sibling;
      Assert_True (Next = Null_Gtk_Accessible);

      --  Round-trip all interface conversions declared by the generated type.
      Assert_True (-Accessible = Button);
      Assert_True (-Buildable = Button);
      Assert_True (-Constraint = Button);
      Unref (Button);
   end Test_Button_Accessibility_And_Interfaces;

   --  Emitting activate directly exercises both callback marshallers without
   --  opening a chooser or waiting for user input.
   procedure Test_Button_Activate_Signal is
      Button : constant Gtk_Color_Dialog_Button :=
        Gtk_Color_Dialog_Button_New (null);
      Signal_Name : Chars_Ptr;
   begin
      Ref_Sink (Button);
      Slot_Object := Button;
      Typed_Activations := 0;
      Slot_Activations := 0;
      Button.On_Activate (On_Activate_Typed'Unrestricted_Access);
      Button.On_Activate (On_Activate_Slot'Unrestricted_Access, Button);

      Signal_Name := New_String ("activate");
      Emit_Void (Button.Get_Object, Signal_Name);
      Free (Signal_Name);
      Assert_Cmpuint_Eq (Guint (Typed_Activations), 1);
      Assert_Cmpuint_Eq (Guint (Slot_Activations), 1);
      Unref (Button);
   end Test_Button_Activate_Signal;

   --  Cancellation completes asynchronously under Xvfb without opening an
   --  interactive color chooser, so the result can be checked headlessly.
   procedure Test_Dialog_Cancelled_Choose is
      Dialog : constant Gtk_Color_Dialog := Gtk_Color_Dialog_New;
      Cancel : constant Gcancellable := Gcancellable_New;
      Initial : constant Gdk_RGBA := (0.25, 0.5, 0.75, 1.0);
   begin
      Async_Dialog := Dialog;
      Async_Done := False;
      Dialog.Choose_Rgba
        (Parent => null,
         Initial_Color => Initial,
         Cancellable => Cancel,
         Callback => On_Choose_Rgba'Unrestricted_Access);
      Cancel.Cancel;

      --  Async callbacks run only after control returns to the main context.
      for Attempt in 1 .. 5_000 loop
         exit when Async_Done;
         if not Glib.Main.Main_Context_Iteration (May_Block => False) then
            delay 0.001;
         end if;
      end loop;
      Assert_True (Async_Done);

      --  A pre-cancelled operation with no callback safely exercises the
      --  nullable callback path without leaving a chooser waiting for input.
      Cancel.Cancel;
      Dialog.Choose_Rgba
        (Parent => null,
         Initial_Color => Initial,
         Cancellable => Cancel,
         Callback => null);
      Unref (GObject (Cancel));
      Unref (Dialog);
      Async_Dialog := null;
   end Test_Dialog_Cancelled_Choose;

   --  Check the successful transfer-full result path. Gtk's finish method
   --  accepts a GTask from this dialog when its source tag matches choose_rgba.
   procedure Test_Dialog_Finish_Success is
      Dialog : constant Gtk_Color_Dialog := Gtk_Color_Dialog_New;
      Chosen : constant Gdk_RGBA := (0.125, 0.375, 0.625, 0.875);
      Bytes  : constant Glib.Gsize :=
        Glib.Gsize
          ((Gdk_RGBA'Size + System.Storage_Unit - 1) / System.Storage_Unit);
      Buffer : constant RGBA_Access := To_RGBA_Access (G_Malloc (Bytes));
      Task_Handle : constant System.Address :=
        G_Task_New (Dialog.Get_Object, System.Null_Address,
                    System.Null_Address, System.Null_Address);
      Error  : GError;
      Actual : Gdk_RGBA;
   begin
      --  The result buffer uses GLib allocation because the Ada binding frees
      --  the transfer-full GdkRGBA with g_free after copying its fields.
      Buffer.all := Chosen;
      G_Task_Set_Source_Tag (Task_Handle, Choose_Rgba_Tag'Address);
      G_Task_Return_Pointer
        (Task_Handle, Buffer.all'Address, System.Null_Address);

      Actual := Dialog.Choose_Rgba_Finish
        (To_Async_Result (Task_Handle), Error);
      Assert_True (Error = null);
      Assert_Cmpfloat_Eq (Gdouble (Actual.Red), Gdouble (Chosen.Red));
      Assert_Cmpfloat_Eq (Gdouble (Actual.Green), Gdouble (Chosen.Green));
      Assert_Cmpfloat_Eq (Gdouble (Actual.Blue), Gdouble (Chosen.Blue));
      Assert_Cmpfloat_Eq (Gdouble (Actual.Alpha), Gdouble (Chosen.Alpha));

      G_Object_Unref (Task_Handle);
      Unref (Dialog);
   end Test_Dialog_Finish_Success;

begin
   Glib.Test.Init;

   --  Widgets cannot be created until GTK is initialized.
   Gtk.Main.Init;

   Glib.Test.Add_Func
     ("/color-dialog/properties", Test_Dialog_Properties'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/button-rgba-round-trip",
      Test_Button_Rgba_Round_Trip'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/button-constructors-dialog",
      Test_Button_Constructors_And_Dialog'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/button-accessibility-interfaces",
      Test_Button_Accessibility_And_Interfaces'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/button-activate-signal",
      Test_Button_Activate_Signal'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/cancelled-choose",
      Test_Dialog_Cancelled_Choose'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/color-dialog/finish-success",
      Test_Dialog_Finish_Success'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Color_Dialog;
