
package body Gtk.Fixed is

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Widget : in Gtk.Fixed.Gtk_Fixed'Class)
                          return      Enums.Widget_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_fixed_get_children");
      use Enums.Widget_List;
      Children : Glist;
   begin
      Set_Object (Children, Internal (Get_Object (Widget)));
      return Children;
   end Get_Children;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Fixed)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_fixed_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ----------
   -- Move --
   ----------

   procedure Move
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16)
   is
      procedure Internal
         (Fixed  : in System.Address;
          Widget : in System.Address;
          X      : in Gint16;
          Y      : in Gint16);
      pragma Import (C, Internal, "gtk_fixed_move");
   begin
      Internal (Get_Object (Fixed),
                Get_Object (Widget),
                X,
                Y);
   end Move;

   ---------
   -- Put --
   ---------

   procedure Put
      (Fixed  : in Gtk_Fixed'Class;
       Widget : in Gtk.Widget.Gtk_Widget'Class;
       X      : in Gint16;
       Y      : in Gint16)
   is
      procedure Internal
         (Fixed  : in System.Address;
          Widget : in System.Address;
          X      : in Gint16;
          Y      : in Gint16);
      pragma Import (C, Internal, "gtk_fixed_put");
   begin
      Internal (Get_Object (Fixed),
                Get_Object (Widget),
                X,
                Y);
   end Put;

end Gtk.Fixed;
