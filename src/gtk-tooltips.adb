
package body Gtk.Tooltips is

   ------------
   -- Enable --
   ------------

   procedure Enable (Tooltips : in Gtk_Tooltips'Class) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_enable");
   begin
      Internal (Get_Object (Tooltips));
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Tooltips : in Gtk_Tooltips'Class) is
      procedure Internal (Tooltips : System.Address);
      pragma Import (C, Internal, "gtk_tooltips_disable");
   begin
      Internal (Get_Object (Tooltips));
   end Disable;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Tooltips) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tooltips_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ---------------
   -- Set_Delay --
   ---------------

   procedure Set_Delay
     (Tooltips : in Gtk_Tooltips'Class;
      duration : in Guint)
   is
      procedure Internal (Tooltips : System.Address;
                          Duration : Guint);
      pragma Import (C, Internal, "gtk_tooltips_set_delay");
   begin
      Internal (Get_Object (Tooltips), Duration);
   end Set_Delay;

   -------------
   -- Set_Tip --
   -------------

   procedure Set_Tip
     (Tooltips    : in Gtk_Tooltips'Class;
      Widget      : in Gtk.Widget.Gtk_Widget'Class;
      Tip_Text    : in String;
      Tip_Private : in String)
   is
      procedure Internal (Tooltips : System.Address;
                          Widget   : System.Address;
                          Tip_Text : String;
                          Tip_Private : String);
      pragma Import (C, Internal, "gtk_tooltips_set_tip");
   begin
      Internal (Get_Object (Tooltips), Get_Object (Widget),
                Tip_Text & Ascii.NUL,
                Tip_Private & Ascii.NUL);
   end Set_Tip;

end Gtk.Tooltips;
