with Interfaces.C;

package body Gtk.Menu_Item is

   package C renames Interfaces.C;

   ------------------------
   --  Accelerator_Size  --
   ------------------------

   procedure Accelerator_Size (Menu_Item : in out Gtk_Menu_Item'Class) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_accelerator_size");
   begin
      Internal (Get_Object (Menu_Item));
   end Accelerator_Size;


   ------------------------
   --  Accelerator_Text  --
   ------------------------

   function Accelerator_Text (Menu_Item : in Gtk_Menu_Item'Class)
                              return String is
      procedure Internal (Menu_Item : in System.Address;
                          Buffer : in out C.char_array);
      pragma Import (C, Internal, "gtk_menu_item_accelerator_text");
      Buffer : C.char_array (1 .. 32);
      --
      --  The size of the buffer has been taken from the source of gtk.
      --  (see gtk_menu_item_accelerator_size for confirmation).
   begin
      Internal (Get_Object (Menu_Item), Buffer);
      return C.To_Ada (Buffer);
   end Accelerator_Text;


   ----------------
   --  Activate  --
   ----------------

   procedure Activate (Menu_Item : in out Gtk_Menu_Item'Class) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_activate");
   begin
      Internal (Get_Object (Menu_Item));
   end Activate;


   -----------------
   --  Configure  --
   -----------------

   procedure Configure (Menu_Item              : in out Gtk_Menu_Item'Class;
                        Show_Toggle_Indicator  : in     Boolean;
                        Show_Submenu_Indicator : in     Boolean) is
      procedure Internal (Menu_Item : System.Address;
                          Show_Toggle_Indicator : in Boolean;
                          Show_Submenu_Indicator : in Boolean);
      pragma Import (C, Internal, "gtk_menu_item_configure");
   begin
      Internal (Get_Object (Menu_Item), Show_Toggle_Indicator,
                Show_Submenu_Indicator);
   end Configure;


   ----------------
   --  Deselect  --
   ----------------

   procedure Deselect (Menu_Item : in out Gtk_Menu_Item'Class) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_deselect");
   begin
      Internal (Get_Object (Menu_Item));
   end Deselect;

   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new");
   begin
      Set_Object (Menu_Item, Internal);
   end Gtk_New;


   ---------------
   --  Gtk_New  --
   ---------------

   procedure Gtk_New (Menu_Item : out Gtk_Menu_Item;
                      Label     : in  String) is
      function Internal (Label : in String) return System.Address;
      pragma Import (C, Internal, "gtk_menu_item_new_with_label");
   begin
      Set_Object (Menu_Item, Internal (Label & ASCII.NUL));
   end Gtk_New;


   ------------------
   --  Gtk_Select  --
   ------------------

   procedure Gtk_Select (Menu_Item : in out Gtk_Menu_Item'Class) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_select");
   begin
      Internal (Get_Object (Menu_Item));
   end Gtk_Select;


   ----------------------
   --  Remove_Submenu  --
   ----------------------

   procedure Remove_Submenu (Menu_Item : in out Gtk_Menu_Item'Class) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_remove_submenu");
   begin
      Internal (Get_Object (Menu_Item));
   end Remove_Submenu;


   ---------------------
   --  Right_Justify  --
   ---------------------

   procedure Right_Justify (Menu_Item : in out Gtk_Menu_Item'Class) is
      procedure Internal (Menu_Item : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_right_justify");
   begin
      Internal (Get_Object (Menu_Item));
   end Right_Justify;


   ---------------------
   --  Set_Placement  --
   ---------------------

   procedure Set_Placement (Menu_Item : in out Gtk_Menu_Item'Class;
                            Placement : in     Enums.Submenu_Placement) is
      procedure Internal (Menu_Item : in System.Address;
                          Placement : in Enums.Submenu_Placement);
      pragma Import (C, Internal, "gtk_menu_item_set_placement");
   begin
      Internal (Get_Object (Menu_Item), Placement);
   end Set_Placement;


   -------------------
   --  Set_Submenu  --
   -------------------

   procedure Set_Submenu (Menu_Item : in out Gtk_Menu_Item'Class;
                          Submenu   : in     Widget.Gtk_Widget'Class) is
      procedure Internal (Menu_Item : in System.Address;
                          Submenu   : in System.Address);
      pragma Import (C, Internal, "gtk_menu_item_set_submenu");
   begin
      Internal (Get_Object (Menu_Item), Get_Object (Submenu));
   end Set_Submenu;

end Gtk.Menu_Item;
