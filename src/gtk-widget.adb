package body Gtk.Widget is

   ----------------
   --  Activate  --
   ----------------

   procedure Activate (Widget : in Gtk_Widget'Class)
   is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_activate");
   begin
      Internal (Get_Object (Widget));
   end Activate;

   ---------------
   --  Destroy  --
   ---------------

   procedure Destroy (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_destroy");
   begin
      Internal (Get_Object (Widget));
   end Destroy;


   ------------------
   --  Get_Object  --
   ------------------

   function Get_Events (Widget : in Gtk_Widget'Class) return Gint is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "gtk_widget_get_events");
   begin
      return Internal (Get_Object (Widget));
   end Get_Events;


   --------------------
   --  Get_Toplevel  --
   --------------------

   procedure Get_Toplevel (Widget : in Gtk_Widget'Class;
                           Result : out Gtk_Widget'Class) is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_widget_get_toplevel");
   begin
      Set_Object (Result, Internal (Get_Object (Widget)));
   end Get_Toplevel;


   --------------------
   --  Grab_Default  --
   --------------------

   procedure Grab_Default (Widget : in out Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_default");
   begin
      Internal (Get_Object (Widget));
   end Grab_Default;


   ------------------
   --  Grab_Focus  --
   ------------------

   procedure Grab_Focus (Widget : in out Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_grab_focus");
   begin
      Internal (Get_Object (Widget));
   end Grab_Focus;


   ------------
   --  Hide  --
   ------------

   procedure Hide (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_hide");
   begin
      Internal (Get_Object (Widget));
   end Hide;


   -----------
   --  Map  --
   -----------

   procedure Map (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_map");
   begin
      Internal (Get_Object (Widget));
   end Map;


   -------------
   --  Popup  --
   -------------

   procedure Popup (Widget : in out Gtk_Widget'Class;
                    X, Y : in Gint) is
      procedure Internal (Widget : in System.Address; X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_popup");
   begin
      Internal (Get_Object (Widget), X, Y);
   end Popup;



   --------------
   -- Realize  --
   -------------

   procedure Realize (Widget : in out Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_realize");
   begin
      Internal (Get_Object (Widget));
   end Realize;


   ----------------
   --  Reparent  --
   ----------------

   procedure Reparent (Widget : in out Gtk_Widget'Class;
                       New_Parent : in Gtk_Widget'Class) is
      procedure Internal (Widget, New_Parent : in System.Address);
      pragma Import (C, Internal, "gtk_widget_reparent");
   begin
      Internal (Get_Object (Widget), Get_Object (New_Parent));
   end Reparent;


   ------------------
   --  Set_Events  --
   ------------------

   procedure Set_Events (Widget : in out Gtk_Widget'Class;
                         Events : in     Gint) is
      procedure Internal (Widget : in System.Address;
                          Events : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_events");
   begin
      Internal (Get_Object (Widget), Events);
   end Set_Events;


   ----------------
   --  Set_Name  --
   ----------------

   procedure Set_Name (Widget : in out Gtk_Widget'Class;
                       Name : in String) is
      procedure Internal (Widget : in System.Address;
                          Name : in String);
      pragma Import (C, Internal, "gtk_widget_set_name");
   begin
      Internal (Get_Object (Widget), Name & Ascii.NUL);
   end Set_Name;


   ------------------
   --  Set_Parent  --
   ------------------

   procedure Set_Parent (Widget : in out Gtk_Widget'Class;
                         Parent : in     Gtk_Widget'Class) is
      procedure Internal (Widget, Parent : in System.Address);
      pragma Import (C, Internal, "gtk_widget_set_parent");
   begin
      Internal (Get_Object (Widget), Get_Object (Parent));
   end Set_Parent;

   ---------------------
   --  Set_Sensitive  --
   ---------------------

   procedure Set_Sensitive (Widget    : in Gtk_Widget'Class;
                            Sensitive : in Boolean := True)
   is
      procedure Internal (Widget      : in System.Address;
                          Sensitive : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_sensitive");
   begin
      Internal (Get_Object (Widget), To_Gint (Sensitive));
   end Set_Sensitive;


   ---------------------
   --  Set_UPosition  --
   ---------------------

   procedure Set_UPosition (Widget : in Gtk_Widget'Class;
                            X, Y   : in Gint) is
      procedure Internal (Widget : System.Address;
                          X, Y : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_uposition");
   begin
      Internal (Get_Object (Widget), X, Y);
   end Set_UPosition;


   -----------------
   --  Set_USize  --
   -----------------

   procedure Set_USize (Widget : in Gtk_Widget'Class;
                        Width  : in Gint;
                        Height : in Gint) is
      procedure Internal (Widget : System.Address;
                          Width : in Gint;
                          Height : in Gint);
      pragma Import (C, Internal, "gtk_widget_set_usize");
   begin
      Internal (Get_Object (Widget), Width, Height);
   end Set_USize;

   ------------
   --  Show  --
   ------------

   procedure Show (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show");
   begin
      Internal (Get_Object (Widget));
   end Show;


   ----------------
   --  Show_All  --
   ----------------

   procedure Show_All (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : System.Address);
      pragma Import (C, Internal, "gtk_widget_show_all");
   begin
      Internal (Get_Object (Widget));
   end Show_All;


   -------------
   --  Unmap  --
   -------------

   procedure Unmap (Widget : in Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_unmap");
   begin
      Internal (Get_Object (Widget));
   end Unmap;

   --------------
   -- Unrealize  --
   -------------

   procedure Unrealize (Widget : in out Gtk_Widget'Class) is
      procedure Internal (Widget : in System.Address);
      pragma Import (C, Internal, "gtk_widget_unrealize");
   begin
      Internal (Get_Object (Widget));
   end Unrealize;

end Gtk.Widget;
