

package body Gtk.Notebook is

   -----------------
   -- Append_Page --
   -----------------

   procedure Append_Page
      (Notebook  : in Gtk_Notebook'Class;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class)
   is
      procedure Internal
         (Notebook  : in System.Address;
          Child     : in System.Address;
          Tab_Label : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_append_page");
   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label));
   end Append_Page;

   ----------------------
   -- Append_Page_Menu --
   ----------------------

   procedure Append_Page_Menu
      (Notebook   : in Gtk_Notebook'Class;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class)
   is
      procedure Internal
         (Notebook   : in System.Address;
          Child      : in System.Address;
          Tab_Label  : in System.Address;
          Menu_Label : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_append_page_menu");
   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Get_Object (Menu_Label));
   end Append_Page_Menu;

   ------------------
   -- Current_Page --
   ------------------

   function Current_Page (Notebook : in Gtk_Notebook'Class)
                          return        Gint
   is
      function Internal (Notebook : in System.Address)
                         return        Gint;
      pragma Import (C, Internal, "gtk_notebook_current_page");
   begin
      return Internal (Get_Object (Notebook));
   end Current_Page;

   ------------------
   -- Get_Children --
   ------------------

   function Get_Children (Widget : in Gtk_Notebook'Class)
                          return      Children_List.Glist
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_notebook_get_children");
      List : Children_List.Glist;
   begin
      Children_List.Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Children;

   ------------------
   -- Get_Cur_Page --
   ------------------

   function Get_Cur_Page (Widget : in Gtk_Notebook'Class)
                          return      Gtk_Notebook_Page
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_notebook_get_cur_page");
      Page : Gtk_Notebook_Page;
   begin
      Set_Object (Page, Internal (Get_Object (Widget)));
      return Page;
   end Get_Cur_Page;

   --------------------
   -- Get_Menu_Label --
   --------------------

   function Get_Menu_Label (Page : in Gtk_Notebook_Page)
                            return Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_notebook_get_menu_label");
      Box : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Box, Internal (Get_Object (Page)));
      return Box;
   end Get_Menu_Label;

   -------------------
   -- Get_Tab_Label --
   -------------------

   function Get_Tab_Label (Page : in Gtk_Notebook_Page)
                           return Gtk.Box.Gtk_Box
   is
      function Internal (Widget : in System.Address) return System.Address;
      pragma Import (C, Internal, "ada_notebook_get_tab_label");
      Box : Gtk.Box.Gtk_Box;
   begin
      Set_Object (Box, Internal (Get_Object (Page)));
      return Box;
   end Get_Tab_Label;

   -----------------
   -- Get_Tab_Pos --
   -----------------

   function Get_Tab_Pos (Widget : in Gtk_Notebook'Class)
                         return      Gtk_Position_Type
   is
      function Internal (Widget : in System.Address) return Gint;
      pragma Import (C, Internal, "ada_notebook_get_tab_pos");
   begin
      return Gtk_Position_Type'Val (Internal (Get_Object (Widget)));
   end Get_Tab_Pos;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Widget : out Gtk_Notebook)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_notebook_new");
   begin
      Set_Object (Widget, Internal);
   end Gtk_New;

   -----------------
   -- Insert_Page --
   -----------------

   procedure Insert_Page
      (Notebook  : in Gtk_Notebook'Class;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class;
       Position  : in Gint)
   is
      procedure Internal
         (Notebook  : in System.Address;
          Child     : in System.Address;
          Tab_Label : in System.Address;
          Position  : in Gint);
      pragma Import (C, Internal, "gtk_notebook_insert_page");
   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Position);
   end Insert_Page;

   ----------------------
   -- Insert_Page_Menu --
   ----------------------

   procedure Insert_Page_Menu
      (Notebook   : in Gtk_Notebook'Class;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class;
       Position   : in Gint)
   is
      procedure Internal
         (Notebook   : in System.Address;
          Child      : in System.Address;
          Tab_Label  : in System.Address;
          Menu_Label : in System.Address;
          Position   : in Gint);
      pragma Import (C, Internal, "gtk_notebook_insert_page_menu");
   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Get_Object (Menu_Label),
                Position);
   end Insert_Page_Menu;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Notebook : in out Gtk_Notebook'Class)
   is
      procedure Internal (Notebook : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_next_page");
   begin
      Internal (Get_Object (Notebook));
   end Next_Page;

   -------------------
   -- Popup_Disable --
   -------------------

   procedure Popup_Disable (Notebook : in Gtk_Notebook'Class)
   is
      procedure Internal (Notebook : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_popup_disable");
   begin
      Internal (Get_Object (Notebook));
   end Popup_Disable;

   ------------------
   -- Popup_Enable --
   ------------------

   procedure Popup_Enable (Notebook : in Gtk_Notebook'Class)
   is
      procedure Internal (Notebook : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_popup_enable");
   begin
      Internal (Get_Object (Notebook));
   end Popup_Enable;

   ------------------
   -- Prepend_Page --
   ------------------

   procedure Prepend_Page
      (Notebook  : in Gtk_Notebook'Class;
       Child     : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label : in Gtk.Box.Gtk_Box'Class)
   is
      procedure Internal
         (Notebook  : in System.Address;
          Child     : in System.Address;
          Tab_Label : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_prepend_page");
   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label));
   end Prepend_Page;

   -----------------------
   -- Prepend_Page_Menu --
   -----------------------

   procedure Prepend_Page_Menu
      (Notebook   : in Gtk_Notebook'Class;
       Child      : in Gtk.Widget.Gtk_Widget'Class;
       Tab_Label  : in Gtk.Box.Gtk_Box'Class;
       Menu_Label : in Gtk.Box.Gtk_Box'Class)
   is
      procedure Internal
         (Notebook   : in System.Address;
          Child      : in System.Address;
          Tab_Label  : in System.Address;
          Menu_Label : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_prepend_page_menu");
   begin
      Internal (Get_Object (Notebook),
                Get_Object (Child),
                Get_Object (Tab_Label),
                Get_Object (Menu_Label));
   end Prepend_Page_Menu;

   ---------------
   -- Prev_Page --
   ---------------

   procedure Prev_Page (Notebook : in out Gtk_Notebook'Class)
   is
      procedure Internal (Notebook : in System.Address);
      pragma Import (C, Internal, "gtk_notebook_prev_page");
   begin
      Internal (Get_Object (Notebook));
   end Prev_Page;

   -----------------
   -- Remove_Page --
   -----------------

   procedure Remove_Page
      (Notebook : in Gtk_Notebook'Class;
       Page_Num : in Gint)
   is
      procedure Internal
         (Notebook : in System.Address;
          Page_Num : in Gint);
      pragma Import (C, Internal, "gtk_notebook_remove_page");
   begin
      Internal (Get_Object (Notebook),
                Page_Num);
   end Remove_Page;

   --------------
   -- Set_Page --
   --------------

   procedure Set_Page
      (Notebook : in Gtk_Notebook'Class;
       Page_Num : in Gint)
   is
      procedure Internal
         (Notebook : in System.Address;
          Page_Num : in Gint);
      pragma Import (C, Internal, "gtk_notebook_set_page");
   begin
      Internal (Get_Object (Notebook),
                Page_Num);
   end Set_Page;

   --------------------
   -- Set_Scrollable --
   --------------------

   procedure Set_Scrollable
      (Notebook   : in Gtk_Notebook'Class;
       Scrollable : in Boolean)
   is
      procedure Internal
         (Notebook   : in System.Address;
          Scrollable : in Gint);
      pragma Import (C, Internal, "gtk_notebook_set_scrollable");
   begin
      Internal (Get_Object (Notebook),
                Boolean'Pos (Scrollable));
   end Set_Scrollable;

   ---------------------
   -- Set_Show_Border --
   ---------------------

   procedure Set_Show_Border
      (Notebook    : in Gtk_Notebook'Class;
       Show_Border : in Boolean)
   is
      procedure Internal
         (Notebook    : in System.Address;
          Show_Border : in Gint);
      pragma Import (C, Internal, "gtk_notebook_set_show_border");
   begin
      Internal (Get_Object (Notebook),
                Boolean'Pos (Show_Border));
   end Set_Show_Border;

   -------------------
   -- Set_Show_Tabs --
   -------------------

   procedure Set_Show_Tabs
      (Notebook  : in Gtk_Notebook'Class;
       Show_Tabs : in Boolean)
   is
      procedure Internal
         (Notebook  : in System.Address;
          Show_Tabs : in Gint);
      pragma Import (C, Internal, "gtk_notebook_set_show_tabs");
   begin
      Internal (Get_Object (Notebook),
                Boolean'Pos (Show_Tabs));
   end Set_Show_Tabs;

   --------------------
   -- Set_Tab_Border --
   --------------------

   procedure Set_Tab_Border
      (Notebook     : in Gtk_Notebook'Class;
       Border_Width : in Gint)
   is
      procedure Internal
         (Notebook     : in System.Address;
          Border_Width : in Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_border");
   begin
      Internal (Get_Object (Notebook),
                Border_Width);
   end Set_Tab_Border;

   -----------------
   -- Set_Tab_Pos --
   -----------------

   procedure Set_Tab_Pos
      (Notebook : in Gtk_Notebook'Class;
       Pos      : in Gtk_Position_Type)
   is
      procedure Internal
         (Notebook : in System.Address;
          Pos      : in Gint);
      pragma Import (C, Internal, "gtk_notebook_set_tab_pos");
   begin
      Internal (Get_Object (Notebook),
                Gtk_Position_Type'Pos (Pos));
   end Set_Tab_Pos;

end Gtk.Notebook;
