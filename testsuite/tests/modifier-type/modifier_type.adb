with Ada.Command_Line;

with Glib;                     use Glib;
with Glib.Properties.Creation;
with Glib.Test;                use Glib.Test;

with Gdk.Enums;                use Gdk.Enums;

procedure Modifier_Type is

   function Ada_Constant
     (C_Name : String;
      Found  : out Boolean) return Gdk_Modifier_Type;
   --  Return the Ada constant bound from the GDK_*_MASK member named C_Name,
   --  setting Found to False if this test knows no such member.

   procedure Test_Values with Convention => C;
   procedure Test_Combinations with Convention => C;
   procedure Test_Runtime_Class with Convention => C;

   ------------------
   -- Ada_Constant --
   ------------------

   function Ada_Constant
     (C_Name : String;
      Found  : out Boolean) return Gdk_Modifier_Type
   is
   begin
      Found := True;
      if C_Name = "GDK_NO_MODIFIER_MASK" then
         return Gdk_No_Modifier_Mask;
      elsif C_Name = "GDK_SHIFT_MASK" then
         return Gdk_Shift_Mask;
      elsif C_Name = "GDK_LOCK_MASK" then
         return Gdk_Lock_Mask;
      elsif C_Name = "GDK_CONTROL_MASK" then
         return Gdk_Control_Mask;
      elsif C_Name = "GDK_ALT_MASK" then
         return Gdk_Alt_Mask;
      elsif C_Name = "GDK_BUTTON1_MASK" then
         return Gdk_Button1_Mask;
      elsif C_Name = "GDK_BUTTON2_MASK" then
         return Gdk_Button2_Mask;
      elsif C_Name = "GDK_BUTTON3_MASK" then
         return Gdk_Button3_Mask;
      elsif C_Name = "GDK_BUTTON4_MASK" then
         return Gdk_Button4_Mask;
      elsif C_Name = "GDK_BUTTON5_MASK" then
         return Gdk_Button5_Mask;
      elsif C_Name = "GDK_SUPER_MASK" then
         return Gdk_Super_Mask;
      elsif C_Name = "GDK_HYPER_MASK" then
         return Gdk_Hyper_Mask;
      elsif C_Name = "GDK_META_MASK" then
         return Gdk_Meta_Mask;
      else
         Found := False;
         return Gdk_No_Modifier_Mask;
      end if;
   end Ada_Constant;

   -----------------
   -- Test_Values --
   -----------------

   procedure Test_Values is
   begin
      Assert_Cmphex_Eq (Guint (Gdk_No_Modifier_Mask), 16#0000_0000#);
      Assert_Cmphex_Eq (Guint (Gdk_Shift_Mask),       16#0000_0001#);
      Assert_Cmphex_Eq (Guint (Gdk_Lock_Mask),        16#0000_0002#);
      Assert_Cmphex_Eq (Guint (Gdk_Control_Mask),     16#0000_0004#);
      Assert_Cmphex_Eq (Guint (Gdk_Alt_Mask),         16#0000_0008#);
      Assert_Cmphex_Eq (Guint (Gdk_Button1_Mask),     16#0000_0100#);
      Assert_Cmphex_Eq (Guint (Gdk_Button2_Mask),     16#0000_0200#);
      Assert_Cmphex_Eq (Guint (Gdk_Button3_Mask),     16#0000_0400#);
      Assert_Cmphex_Eq (Guint (Gdk_Button4_Mask),     16#0000_0800#);
      Assert_Cmphex_Eq (Guint (Gdk_Button5_Mask),     16#0000_1000#);
      Assert_Cmphex_Eq (Guint (Gdk_Super_Mask),       16#0400_0000#);
      Assert_Cmphex_Eq (Guint (Gdk_Hyper_Mask),       16#0800_0000#);
      Assert_Cmphex_Eq (Guint (Gdk_Meta_Mask),        16#1000_0000#);
      Assert_Cmphex_Eq (Guint (Gdk_Modifier_Mask),    16#1C00_1F0F#);
   end Test_Values;

   -----------------------
   -- Test_Combinations --
   -----------------------

   procedure Test_Combinations is
      Accel : constant Gdk_Modifier_Type :=
        Gdk_Control_Mask or Gdk_Shift_Mask;

      --  A state as GDK reports it: an accelerator plus a private bit
      --  outside Gdk_Modifier_Mask.
      State : constant Gdk_Modifier_Type := Accel or 2 ** 30;
   begin
      Assert_Cmphex_Eq (Guint (Accel), 16#0000_0005#);

      --  Membership tests
      Assert_True ((Accel and Gdk_Control_Mask) /= Gdk_No_Modifier_Mask);
      Assert_True ((Accel and Gdk_Alt_Mask) = Gdk_No_Modifier_Mask);

      --  Masking the private bits out makes the comparison succeed
      Assert_True (State /= Accel);
      Assert_Cmphex_Eq (Guint (State and Gdk_Modifier_Mask), Guint (Accel));

      --  Every declared member is covered by Gdk_Modifier_Mask
      Assert_Cmphex_Eq
        (Guint ((Gdk_Shift_Mask or Gdk_Lock_Mask or Gdk_Control_Mask
                 or Gdk_Alt_Mask or Gdk_Button1_Mask or Gdk_Button2_Mask
                 or Gdk_Button3_Mask or Gdk_Button4_Mask or Gdk_Button5_Mask
                 or Gdk_Super_Mask or Gdk_Hyper_Mask or Gdk_Meta_Mask)
                and not Gdk_Modifier_Mask),
         16#0000_0000#);
   end Test_Combinations;

   ------------------------
   -- Test_Runtime_Class --
   ------------------------

   procedure Test_Runtime_Class is
      use Glib.Properties.Creation;

      Klass : constant Flags_Class :=
        Flags_Class_From_Type (Gdk_Modifier_Type_Get_Type);
      Nth   : Guint := 0;
      Seen  : Natural := 0;
   begin
      Assert_True (Klass /= null);

      loop
         declare
            Val : constant Flags_Value := Nth_Value (Klass, Nth);
         begin
            exit when Val = null;

            declare
               C_Name  : constant String :=
                 Glib.Properties.Creation.Name (Val);
               Found   : Boolean;
               Ada_Val : constant Gdk_Modifier_Type :=
                 Ada_Constant (C_Name, Found);
            begin
               if Found then
                  Assert_Cmphex_Eq
                    (Guint (Glib.Properties.Creation.Value (Val)),
                     Guint (Ada_Val));
               else
                  Message
                    ("GdkModifierType member not bound in Gdk.Enums: "
                     & C_Name);
                  Fail;
               end if;
            end;
         end;

         Seen := Seen + 1;
         Nth := Nth + 1;
      end loop;

      --  Fails if a future GTK4 drops a member, which the loop above cannot
      --  see.
      Assert_Cmpuint_Eq (Guint (Seen), 13);
   end Test_Runtime_Class;

begin
   Glib.Test.Init;

   Glib.Test.Add_Func
     ("/modifier-type/values", Test_Values'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/modifier-type/combinations", Test_Combinations'Unrestricted_Access);
   Glib.Test.Add_Func
     ("/modifier-type/runtime-class", Test_Runtime_Class'Unrestricted_Access);

   Ada.Command_Line.Set_Exit_Status (Glib.Test.Run);
end Modifier_Type;
