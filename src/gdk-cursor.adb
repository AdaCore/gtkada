package body Gdk.Cursor is

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Cursor : in out Gdk_Cursor'Class)
   is
      procedure Internal (Cursor : in System.Address);
      pragma Import (C, Internal, "gdk_cursor_destroy");
   begin
      Internal (Get_Object (Cursor));
      Set_Object (Cursor, System.Null_Address);
   end Destroy;


   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New (Widget      : out Gdk_Cursor;
                      Cursor_Type : in Gdk.Types.Gdk_Cursor_Type)
   is
      function Internal (Cursor_Type : in Gdk.Types.Gdk_Cursor_Type)
                         return           System.Address;
      pragma Import (C, Internal, "ada_gdk_cursor_new");
   begin
      Set_Object (Widget, Internal (Cursor_Type));
   end Gdk_New;


   -------------
   -- Gdk_New --
   -------------

   procedure Gdk_New
      (Widget : out Gdk_Cursor;
       Source : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Mask   : in Gdk.Pixmap.Gdk_Pixmap'Class;
       Fg     : in Gdk.Color.Gdk_Color'Class;
       Bg     : in Gdk.Color.Gdk_Color'Class;
       X      : in Gint;
       Y      : in Gint)
   is
      function Internal
         (Source : in System.Address;
          Mask   : in System.Address;
          Fg     : in System.Address;
          Bg     : in System.Address;
          X      : in Gint;
          Y      : in Gint)
          return      System.Address;
      pragma Import (C, Internal, "gdk_cursor_new_from_pixmap");
   begin
      Set_Object (Widget, Internal (Get_Object (Source),
                                    Get_Object (Mask),
                                    Get_Object (Fg),
                                    Get_Object (Bg),
                                    X,
                                    Y));
   end Gdk_New;

end Gdk.Cursor;
