package body UG_My_Button is

   -----------------------
   -- My_Primitive_Func --
   -----------------------

   procedure My_Primitive_Func (Myb : access My_Button_Record) is
   begin
      Myb.Count := Myb.Count + 1;
   end My_Primitive_Func;

   ------------
   -- Create --
   ------------

   function Create return My_Button is
      Myb : My_Button;
   begin
      --  START create
      Myb := new My_Button_Record;
      Initialize (Myb, Label => "Hello");  --  from Gtk.Button
      --  END create
      return Myb;
   end Create;

end UG_My_Button;
