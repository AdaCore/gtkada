package body Gdk.Drawable is

   --------------------
   -- Draw_Rectangle --
   --------------------

   procedure Draw_Rectangle (Drawable : in Gdk_Drawable'Class;
                             Gc       : in Gdk.GC.Gdk_GC'Class;
                             Filled   : in Boolean := False;
                             X, Y     : in Gint;
                             Width    : in Gint;
                             Height   : in Gint) is
      procedure Internal (Drawable : in System.Address;
                          Gc       : in System.Address;
                          Filled   : in Gint;
                          X, Y     : in Gint;
                          Width    : in Gint;
                          Height   : in Gint);
      pragma Import (C, Internal, "gdk_draw_rectangle");
   begin
      Internal (Get_Object (Drawable),
                Get_Object (Gc),
                To_Gint (Filled),
                X, Y, Width, Height);
   end Draw_Rectangle;

end Gdk.Drawable;
