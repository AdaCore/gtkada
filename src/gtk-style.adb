package body Gtk.Style is


   --------------
   --  Attach  --
   --------------

   function Attach (Style  : in Gtk_Style;
                    Window : in Gdk.Types.Gdk_Window) return Gtk_Style is
      function Internal (Style : in System.Address;
                         Window : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_attach");
      Result : Gtk_Style;
   begin
      Set_Object (Result, Internal (Get_Object (Style),
                                    Gdk.Get_Object (Window)));
      return Result;
   end Attach;


   ------------
   --  Copy  --
   ------------

   procedure Copy (Source : in Gtk_Style;
                   Destination : out Gtk_Style) is
      function Internal (Style : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_style_copy");
   begin
      Set_Object (Destination, Internal (Get_Object (Source)));
   end Copy;


   --------------
   --  Detach  --
   --------------

   procedure Detach (Style : in out Gtk_Style) is
      procedure Internal (Style : in System.Address);
      pragma Import (C, Internal, "gtk_style_detach");
   begin
      Internal (Get_Object (Style));
   end Detach;


   ------------------
   --  Draw_Arrow  --
   ------------------

   procedure Draw_Arrow (Style       : in Gtk_Style;
                         Window      : in Gdk.Types.Gdk_Window;
                         State_Type  : in Enums.Gtk_State_Type;
                         Shadow_Type : in Enums.Gtk_Shadow_Type;
                         Arrow_Type  : in Enums.Gtk_Arrow_Type;
                         Fill        : in Gint;
                         X, Y        : in Gint;
                         Width       : in Gint;
                         Height      : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          Arrow_Type  : in Enums.Gtk_Arrow_Type;
                          Fill        : in Gint;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_arrow");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window), State_Type,
                Shadow_Type, Arrow_Type, Fill, X, Y, Width, Height);
   end Draw_Arrow;



   --------------------
   --  Draw_Diamond  --
   --------------------

   procedure Draw_Diamond (Style       : in Gtk_Style;
                           Window      : in Gdk.Types.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           X, Y        : in Gint;
                           Width       : in Gint;
                           Height      : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_diamond");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window), State_Type,
                Shadow_Type, X, Y, Width, Height);
   end Draw_Diamond;


   ------------------
   --  Draw_Hline  --
   ------------------

   procedure Draw_Hline (Style      : in Gtk_Style;
                         Window     : in Gdk.Types.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         X1, X2     : in Gint;
                         Y          : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          X1, X2, Y     : in Gint);
      pragma Import (C, Internal, "gtk_draw_hline");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window),
                State_Type, X1, X2, Y);
   end Draw_Hline;


   -----------------
   --  Draw_Oval  --
   -----------------

   procedure Draw_Oval (Style       : in Gtk_Style;
                        Window      : in Gdk.Types.Gdk_Window;
                        State_Type  : in Enums.Gtk_State_Type;
                        Shadow_Type : in Enums.Gtk_Shadow_Type;
                        X, Y        : in Gint;
                        Width       : in Gint;
                        Height      : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint);
      pragma Import (C, Internal, "gtk_draw_oval");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window), State_Type,
                Shadow_Type, X, Y, Width, Height);
   end Draw_Oval;


   --------------------
   --  Draw_Polygon  --
   --------------------

   procedure Draw_Polygon (Style       : in Gtk_Style;
                           Window      : in Gdk.Types.Gdk_Window;
                           State_Type  : in Enums.Gtk_State_Type;
                           Shadow_Type : in Enums.Gtk_Shadow_Type;
                           Points      : in Points_Array;
                           Fill        : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          Shadow_Type   : in Enums.Gtk_Shadow_Type;
                          Points        : in Points_Array;
                          Npoints       : in Gint;
                          Fill          : in Gint);
      pragma Import (C, Internal, "gtk_draw_polygon");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window), State_Type,
                Shadow_Type, Points, Points'Length, Fill);
   end Draw_Polygon;


   -------------------
   --  Draw_Shadow  --
   -------------------

   procedure Draw_Shadow (Style       : in Gtk_Style;
                          Window      : in Gdk.Types.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          Shadow_Type : in Enums.Gtk_Shadow_Type;
                          X, Y        : in Gint;
                          Width       : in Gint;
                          Height      : in Gint) is
      procedure Internal (Style, Window       : in System.Address;
                          State_Type          : in Enums.Gtk_State_Type;
                          Shadow_Type         : in Enums.Gtk_Shadow_Type;
                          X, Y, Width, Height : Gint);
      pragma Import (C, Internal, "gtk_draw_shadow");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window),
                State_Type, Shadow_Type, X, Y, Width, Height);
   end Draw_Shadow;


   -------------------
   --  Draw_String  --
   -------------------

   procedure Draw_String (Style       : in Gtk_Style;
                          Window      : in Gdk.Types.Gdk_Window;
                          State_Type  : in Enums.Gtk_State_Type;
                          X, Y        : in Gint;
                          Str         : in String) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          X, Y          : in Gint;
                          Str           : in String);
      pragma Import (C, Internal, "gtk_draw_string");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window),
                State_Type, X,  Y, Str & ASCII.NUL);
   end Draw_String;


   ------------------
   --  Draw_Vline  --
   ------------------

   procedure Draw_Vline (Style      : in Gtk_Style;
                         Window     : in Gdk.Types.Gdk_Window;
                         State_Type : in Enums.Gtk_State_Type;
                         Y1, Y2     : in Gint;
                         X          : in Gint) is
      procedure Internal (Style, Window : in System.Address;
                          State_Type    : in Enums.Gtk_State_Type;
                          Y1, Y2, X     : in Gint);
      pragma Import (C, Internal, "gtk_draw_vline");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window),
                State_Type, Y1, Y2, X);
   end Draw_Vline;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Style : out Gtk_Style) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_style_new");
   begin
      Set_Object (Style, Internal);
   end Gtk_New;


   ----------------------
   --  Set_Background  --
   ----------------------

   procedure Set_Background (Style      : in out Gtk_Style;
                             Window     : in     Gdk.Types.Gdk_Window;
                             State_Type : in     Enums.Gtk_State_Type) is
      procedure Internal (Style      : in System.Address;
                          Window     : in System.Address;
                          State_Type : in Enums.Gtk_State_Type);
      pragma Import (C, Internal, "gtk_style_set_background");
   begin
      Internal (Get_Object (Style), Gdk.Get_Object (Window), State_Type);
   end Set_Background;

end Gtk.Style;
