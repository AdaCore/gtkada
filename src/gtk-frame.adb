

package body Gtk.Frame is

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Frame;
                      Label  : in String)
   is
      function Internal (Label  : in String)
                         return      System.Address;
      pragma Import (C, Internal, "gtk_frame_new");
   begin
      Set_Object (Widget, Internal (Label & Ascii.NUL));
   end Gtk_New;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Frame : in Gtk_Frame'Class;
       Label : in String)
   is
      procedure Internal
         (Frame : in System.Address;
          Label : in String);
      pragma Import (C, Internal, "gtk_frame_set_label");
   begin
      Internal (Get_Object (Frame),
                Label & Ascii.NUL);
   end Set_Label;

   ---------------------
   -- Set_Label_Align --
   ---------------------

   procedure Set_Label_Align
      (Frame  : in Gtk_Frame'Class;
       Xalign : in Gfloat;
       Yalign : in Gfloat)
   is
      procedure Internal
         (Frame  : in System.Address;
          Xalign : in Gfloat;
          Yalign : in Gfloat);
      pragma Import (C, Internal, "gtk_frame_set_label_align");
   begin
      Internal (Get_Object (Frame),
                Xalign,
                Yalign);
   end Set_Label_Align;

   ---------------------
   -- Set_Shadow_Type --
   ---------------------

   procedure Set_Shadow_Type
      (Frame    : in Gtk_Frame'Class;
       The_Type : in Gtk_Shadow_Type)
   is
      procedure Internal
         (Frame    : in System.Address;
          The_Type : in Gint);
      pragma Import (C, Internal, "gtk_frame_set_shadow_type");
   begin
      Internal (Get_Object (Frame),
                Gtk_Shadow_Type'Pos (The_Type));
   end Set_Shadow_Type;

end Gtk.Frame;
