

package body Gtk.Aspect_Frame is

   ---------------
   -- Get_Ratio --
   ---------------

   function Get_Ratio (Widget : in Gtk_Aspect_Frame'Class)
                       return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_ratio");
   begin
      return Internal (Get_Object (Widget));
   end Get_Ratio;

   ----------------
   -- Get_Xalign --
   ----------------

   function Get_Xalign (Widget : in Gtk_Aspect_Frame'Class)
                        return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_xalign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Xalign;

   ----------------
   -- Get_Yalign --
   ----------------

   function Get_Yalign (Widget : in Gtk_Aspect_Frame'Class)
                        return      Gfloat
   is
      function Internal (Widget : in System.Address)
                         return      Gfloat;
      pragma Import (C, Internal, "ada_aspect_frame_get_yalign");
   begin
      return Internal (Get_Object (Widget));
   end Get_Yalign;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Widget     : out Gtk_Aspect_Frame;
       Label      : in String;
       Xalign     : in Gfloat;
       Yalign     : in Gfloat;
       Ratio      : in Gfloat;
       Obey_Child : in Gint)
   is
      function Internal
         (Label      : in String;
          Xalign     : in Gfloat;
          Yalign     : in Gfloat;
          Ratio      : in Gfloat;
          Obey_Child : in Gint)
          return          System.Address;
      pragma Import (C, Internal, "gtk_aspect_frame_new");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL,
                                    Xalign,
                                    Yalign,
                                    Ratio,
                                    Obey_Child));
   end Gtk_New;

   ---------
   -- Set --
   ---------

   procedure Set
      (Aspect_Frame : in Gtk_Aspect_Frame'Class;
       Xalign       : in Gfloat;
       Yalign       : in Gfloat;
       Ratio        : in Gfloat;
       Obey_Child   : in Gint)
   is
      procedure Internal
         (Aspect_Frame : in System.Address;
          Xalign       : in Gfloat;
          Yalign       : in Gfloat;
          Ratio        : in Gfloat;
          Obey_Child   : in Gint);
      pragma Import (C, Internal, "gtk_aspect_frame_set");
   begin
      Internal (Get_Object (Aspect_Frame),
                Xalign,
                Yalign,
                Ratio,
                Obey_Child);
   end Set;

end Gtk.Aspect_Frame;

