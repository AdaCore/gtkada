with Interfaces.C.Strings;

package body Gdk.Main is

   -------------------
   --  Get_Display  --
   -------------------

   function Get_Display return String is
      function Internal return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gdk_get_display");
   begin
      return Interfaces.C.Strings.Value (Internal);
   end Get_Display;


   --------------------
   --  Get_Use_Xshm  --
   --------------------

   function Get_Use_Xshm return Boolean is
      function Internal return Gint;
      pragma Import (C, Internal, "gdk_get_use_xshm");
   begin
      return To_Boolean (Internal);
   end Get_Use_Xshm;


   --------------------
   --  Set_Use_Xshm  --
   --------------------

   procedure Set_Use_Xshm (Use_Xshm : in Boolean := True) is
      procedure Internal (Use_Xshm : in Gint);
      pragma Import (C, Internal, "gdk_set_use_xshm");
   begin
      Internal (To_Gint (Use_Xshm));
   end Set_Use_Xshm;

end Gdk.Main;
