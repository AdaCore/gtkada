package body Gdk.Bitmap is

   ------------------------
   --  Create_From_Data  --
   ------------------------

   procedure Create_From_Data (Bitmap :    out Gdk_Bitmap;
                               Window : in     Gdk.Window.Gdk_Window;
                               Data   : in     String;
                               Width  : in     Gint;
                               Height : in     Gint) is
      function Internal (Window        : in System.Address;
                         Data          : in String;
                         Width, Height : in Gint) return System.Address;
      pragma Import (C, Internal, "gdk_bitmap_create_from_data");
   begin
      Set_Object (Bitmap, Internal (Get_Object (Window), Data & ASCII.NUL,
                                    Width, Height));
   end Create_From_Data;


end Gdk.Bitmap;
