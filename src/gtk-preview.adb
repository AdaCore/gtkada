with Gdk; use Gdk;

package body Gtk.Preview is

   --------------
   -- Draw_Row --
   --------------

   procedure Draw_Row
      (Preview : in Gtk_Preview'Class;
       Data    : in Guchar_Array;
       X       : in Gint;
       Y       : in Gint;
       W       : in Gint)
   is
      procedure Internal
         (Preview : in System.Address;
          Data    : in System.Address;
          X       : in Gint;
          Y       : in Gint;
          W       : in Gint);
      pragma Import (C, Internal, "gtk_preview_draw_row");
   begin
      Internal (Get_Object (Preview),
                Data'Address,
                X,
                Y,
                W);
   end Draw_Row;

   --------------
   -- Get_Cmap --
   --------------

   function Get_Cmap return Gdk.Color.Gdk_Colormap'Class
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_cmap");
      Widget : Gdk.Color.Gdk_Colormap;
   begin
      Set_Object (Widget, Internal);
      return Widget;
   end Get_Cmap;

   --------------
   -- Get_Info --
   --------------

   function Get_Info return Gtk_Preview_Info
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_info");
      Widget : Gtk_Preview_Info;
   begin
      Set_Object (Widget, Internal);
      return Widget;
   end Get_Info;

   ----------------
   -- Get_Visual --
   ----------------

   function Get_Visual return Gdk.Visual.Gdk_Visual'Class
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_preview_get_visual");
      Widget : Gdk.Visual.Gdk_Visual;
   begin
      Set_Object (Widget, Internal);
      return Widget;
   end Get_Visual;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget   : out Gtk_Preview;
                      The_Type : in Gtk_Preview_Type)
   is
      function Internal (The_Type : in Gint)
                         return        System.Address;
      pragma Import (C, Internal, "gtk_preview_new");
   begin
      Set_Object (Widget, Internal (Gtk_Preview_Type'Pos (The_Type)));
   end Gtk_New;

   ---------
   -- Put --
   ---------

   procedure Put
      (Preview : in Gtk_Preview'Class;
       Window  : in Gdk.Window.Gdk_Window'Class;
       Gc      : in Gdk.GC.Gdk_GC'Class;
       Srcx    : in Gint;
       Srcy    : in Gint;
       Destx   : in Gint;
       Desty   : in Gint;
       Width   : in Gint;
       Height  : in Gint)
   is
      procedure Internal
         (Preview : in System.Address;
          Window  : in System.Address;
          Gc      : in System.Address;
          Srcx    : in Gint;
          Srcy    : in Gint;
          Destx   : in Gint;
          Desty   : in Gint;
          Width   : in Gint;
          Height  : in Gint);
      pragma Import (C, Internal, "gtk_preview_put");
   begin
      Internal (Get_Object (Preview),
                Get_Object (Window),
                Get_Object (Gc),
                Srcx,
                Srcy,
                Destx,
                Desty,
                Width,
                Height);
   end Put;

   -------------
   -- Put_Row --
   -------------

   procedure Put_Row
      (Preview : in Gtk_Preview'Class;
       Src     : in Guchar_Array;
       Dest    : in Guchar_Array;
       X       : in Gint;
       Y       : in Gint;
       W       : in Gint)
   is
      procedure Internal
         (Preview : in System.Address;
          Src     : in System.Address;
          Dest    : in System.Address;
          X       : in Gint;
          Y       : in Gint;
          W       : in Gint);
      pragma Import (C, Internal, "gtk_preview_put_row");
   begin
      Internal (Get_Object (Preview),
                Src'Address,
                Dest'Address,
                X,
                Y,
                W);
   end Put_Row;

   -----------
   -- Reset --
   -----------

   procedure Reset
   is
      procedure Internal;
      pragma Import (C, Internal, "gtk_preview_reset");
   begin
      Internal;
   end Reset;

   --------------------
   -- Set_Color_Cube --
   --------------------

   procedure Set_Color_Cube
      (Nred_Shades   : in Guint;
       Ngreen_Shades : in Guint;
       Nblue_Shades  : in Guint;
       Ngray_Shades  : in Guint)
   is
      procedure Internal
         (Nred_Shades   : in Guint;
          Ngreen_Shades : in Guint;
          Nblue_Shades  : in Guint;
          Ngray_Shades  : in Guint);
      pragma Import (C, Internal, "gtk_preview_set_color_cube");
   begin
      Internal (Nred_Shades,
                Ngreen_Shades,
                Nblue_Shades,
                Ngray_Shades);
   end Set_Color_Cube;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
      (Preview : in Gtk_Preview'Class;
       Expand  : in Gint)
   is
      procedure Internal
         (Preview : in System.Address;
          Expand  : in Gint);
      pragma Import (C, Internal, "gtk_preview_set_expand");
   begin
      Internal (Get_Object (Preview),
                Expand);
   end Set_Expand;

   ---------------
   -- Set_Gamma --
   ---------------

   procedure Set_Gamma (Gamma : in Gdouble)
   is
      procedure Internal (Gamma : in Gdouble);
      pragma Import (C, Internal, "gtk_preview_set_gamma");
   begin
      Internal (Gamma);
   end Set_Gamma;

   ----------------------
   -- Set_Install_Cmap --
   ----------------------

   procedure Set_Install_Cmap (Install_Cmap : in Gint)
   is
      procedure Internal (Install_Cmap : in Gint);
      pragma Import (C, Internal, "gtk_preview_set_install_cmap");
   begin
      Internal (Install_Cmap);
   end Set_Install_Cmap;

   ------------------
   -- Set_Reserved --
   ------------------

   procedure Set_Reserved (Nreserved : in Gint)
   is
      procedure Internal (Nreserved : in Gint);
      pragma Import (C, Internal, "gtk_preview_set_reserved");
   begin
      Internal (Nreserved);
   end Set_Reserved;

   ----------
   -- Size --
   ----------

   procedure Size
      (Preview : in Gtk_Preview'Class;
       Width   : in Gint;
       Height  : in Gint)
   is
      procedure Internal
         (Preview : in System.Address;
          Width   : in Gint;
          Height  : in Gint);
      pragma Import (C, Internal, "gtk_preview_size");
   begin
      Internal (Get_Object (Preview),
                Width,
                Height);
   end Size;

   ------------
   -- Uninit --
   ------------

   procedure Uninit
   is
      procedure Internal;
      pragma Import (C, Internal, "gtk_preview_uninit");
   begin
      Internal;
   end Uninit;

end Gtk.Preview;
