
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget;

package body Gtk.List is

   use Children_List;


   ------------------
   -- Append_Items --
   ------------------

   procedure Append_Items
      (List  : in Gtk_List'Class;
       Items : in Children_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_append_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Append_Items;

   --------------------
   -- Child_Position --
   --------------------

   function Child_Position
      (List   : in Gtk_List'Class;
       Child  : in Gtk.Widget.Gtk_Widget'Class)
       return      Gint
   is
      function Internal
         (List   : in System.Address;
          Child  : in System.Address)
          return      Gint;
      pragma Import (C, Internal, "gtk_list_child_position");
   begin
      return Internal (Get_Object (List),
                       Get_Object (Child));
   end Child_Position;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items
      (List    : in Gtk_List'Class;
       Start   : in Gint;
       The_End : in Gint)
   is
      procedure Internal
         (List    : in System.Address;
          Start   : in Gint;
          The_End : in Gint);
      pragma Import (C, Internal, "gtk_list_clear_items");
   begin
      Internal (Get_Object (List),
                Start,
                The_End);
   end Clear_Items;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Widget : in Gtk.List.Gtk_List'Class)
                          return      Children_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_list_get_children");
      List : Children_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Children;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection (Widget : in Gtk.List.Gtk_List'Class)
                           return      Children_List.Glist
   is
      function Internal (Widget : in System.Address)
                         return      System.Address;
      pragma Import (C, Internal, "ada_list_get_selection");
      List : Children_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Selection;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_List)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_list_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   ------------------
   -- Insert_Items --
   ------------------

   procedure Insert_Items
      (List     : in Gtk_List'Class;
       Items    : in Children_List.Glist;
       Position : in Gint)
   is
      procedure Internal
         (List     : in System.Address;
          Items    : in System.Address;
          Position : in Gint);
      pragma Import (C, Internal, "gtk_list_insert_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items),
                Position);
   end Insert_Items;

   -------------------
   -- Prepend_Items --
   -------------------

   procedure Prepend_Items
      (List  : in Gtk_List'Class;
       Items : in Children_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_prepend_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Prepend_Items;

   ------------------
   -- Remove_Items --
   ------------------

   procedure Remove_Items
      (List  : in Gtk_List'Class;
       Items : in Children_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_remove_items");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Remove_Items;

   ---------------------------
   -- Remove_Items_No_Unref --
   ---------------------------

   procedure Remove_Items_No_Unref
      (List  : in Gtk_List'Class;
       Items : in Children_List.Glist)
   is
      procedure Internal
         (List  : in System.Address;
          Items : in System.Address);
      pragma Import (C, Internal, "gtk_list_remove_items_no_unref");
   begin
      Internal (Get_Object (List),
                Get_Object (Items));
   end Remove_Items_No_Unref;

   ------------------
   -- Select_Child --
   ------------------

   procedure Select_Child
      (List  : in Gtk_List'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (List  : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_list_select_child");
   begin
      Internal (Get_Object (List),
                Get_Object (Child));
   end Select_Child;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
      (List : in Gtk_List'Class;
       Item : in Gint)
   is
      procedure Internal
         (List : in System.Address;
          Item : in Gint);
      pragma Import (C, Internal, "gtk_list_select_item");
   begin
      Internal (Get_Object (List),
                Item);
   end Select_Item;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (List : in Gtk_List'Class;
       Mode : in Gtk_Selection_Mode)
   is
      procedure Internal
         (List : in System.Address;
          Mode : in Gint);
      pragma Import (C, Internal, "gtk_list_set_selection_mode");
   begin
      Internal (Get_Object (List),
                Gtk_Selection_Mode'Pos (Mode));
   end Set_Selection_Mode;

   --------------------
   -- Unselect_Child --
   --------------------

   procedure Unselect_Child
      (List  : in Gtk_List'Class;
       Child : in Gtk.Widget.Gtk_Widget'Class)
   is
      procedure Internal
         (List  : in System.Address;
          Child : in System.Address);
      pragma Import (C, Internal, "gtk_list_unselect_child");
   begin
      Internal (Get_Object (List),
                Get_Object (Child));
   end Unselect_Child;

   -------------------
   -- Unselect_Item --
   -------------------

   procedure Unselect_Item
      (List : in Gtk_List'Class;
       Item : in Gint)
   is
      procedure Internal
         (List : in System.Address;
          Item : in Gint);
      pragma Import (C, Internal, "gtk_list_unselect_item");
   begin
      Internal (Get_Object (List),
                Item);
   end Unselect_Item;

end Gtk.List;

