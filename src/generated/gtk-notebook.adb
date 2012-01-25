------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_05;
pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Notebook is

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Ignored : Gint;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Append_Page (Notebook, Child, null);
   end Append_Page;

   procedure Append_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Ignored : Gint;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Append_Page (Notebook, Child, Tab_Label);
   end Append_Page;

   procedure Prepend_Page
     (Notebook  : access Gtk_Notebook_Record;
      Child     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Ignored : Gint;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Append_Page (Notebook, Child, Tab_Label);
   end Prepend_Page;

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Notebook_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Notebook : out Gtk_Notebook) is
   begin
      Notebook := new Gtk_Notebook_Record;
      Gtk.Notebook.Initialize (Notebook);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Notebook : access Gtk_Notebook_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_notebook_new");
   begin
      Set_Object (Notebook, Internal);
   end Initialize;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_append_page");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)));
   end Append_Page;

   ----------------------
   -- Append_Page_Menu --
   ----------------------

   procedure Append_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Tab_Label  : System.Address;
          Menu_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_append_page_menu");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)), Get_Object_Or_Null (GObject (Menu_Label)));
   end Append_Page_Menu;

   -----------------------
   -- Get_Action_Widget --
   -----------------------

   function Get_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Notebook  : System.Address;
          Pack_Type : Gtk.Enums.Gtk_Pack_Type) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_action_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Notebook), Pack_Type), Stub_Gtk_Widget));
   end Get_Action_Widget;

   ----------------------
   -- Get_Current_Page --
   ----------------------

   function Get_Current_Page
      (Notebook : not null access Gtk_Notebook_Record) return Gint
   is
      function Internal (Notebook : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_get_current_page");
   begin
      return Internal (Get_Object (Notebook));
   end Get_Current_Page;

   --------------------
   -- Get_Group_Name --
   --------------------

   function Get_Group_Name
      (Notebook : not null access Gtk_Notebook_Record) return UTF8_String
   is
      function Internal
         (Notebook : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_notebook_get_group_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Notebook)));
   end Get_Group_Name;

   --------------------
   -- Get_Menu_Label --
   --------------------

   function Get_Menu_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_menu_label");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Notebook), Get_Object (Child)), Stub_Gtk_Widget));
   end Get_Menu_Label;

   -------------------------
   -- Get_Menu_Label_Text --
   -------------------------

   function Get_Menu_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_notebook_get_menu_label_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Menu_Label_Text;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
      (Notebook : not null access Gtk_Notebook_Record) return Gint
   is
      function Internal (Notebook : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_get_n_pages");
   begin
      return Internal (Get_Object (Notebook));
   end Get_N_Pages;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Notebook : System.Address;
          Page_Num : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_nth_page");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Notebook), Page_Num), Stub_Gtk_Widget));
   end Get_Nth_Page;

   --------------------
   -- Get_Scrollable --
   --------------------

   function Get_Scrollable
      (Notebook : not null access Gtk_Notebook_Record) return Boolean
   is
      function Internal (Notebook : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_notebook_get_scrollable");
   begin
      return Boolean'Val (Internal (Get_Object (Notebook)));
   end Get_Scrollable;

   ---------------------
   -- Get_Show_Border --
   ---------------------

   function Get_Show_Border
      (Notebook : not null access Gtk_Notebook_Record) return Boolean
   is
      function Internal (Notebook : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_notebook_get_show_border");
   begin
      return Boolean'Val (Internal (Get_Object (Notebook)));
   end Get_Show_Border;

   -------------------
   -- Get_Show_Tabs --
   -------------------

   function Get_Show_Tabs
      (Notebook : not null access Gtk_Notebook_Record) return Boolean
   is
      function Internal (Notebook : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_notebook_get_show_tabs");
   begin
      return Boolean'Val (Internal (Get_Object (Notebook)));
   end Get_Show_Tabs;

   ------------------------
   -- Get_Tab_Detachable --
   ------------------------

   function Get_Tab_Detachable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_notebook_get_tab_detachable");
   begin
      return Boolean'Val (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Tab_Detachable;

   ---------------------
   -- Get_Tab_Hborder --
   ---------------------

   function Get_Tab_Hborder
      (Notebook : not null access Gtk_Notebook_Record) return Guint16
   is
      function Internal (Notebook : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_notebook_get_tab_hborder");
   begin
      return Internal (Get_Object (Notebook));
   end Get_Tab_Hborder;

   -------------------
   -- Get_Tab_Label --
   -------------------

   function Get_Tab_Label
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_notebook_get_tab_label");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Notebook), Get_Object (Child)), Stub_Gtk_Widget));
   end Get_Tab_Label;

   ------------------------
   -- Get_Tab_Label_Text --
   ------------------------

   function Get_Tab_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return UTF8_String
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_notebook_get_tab_label_text");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Tab_Label_Text;

   -----------------
   -- Get_Tab_Pos --
   -----------------

   function Get_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record)
       return Gtk.Enums.Gtk_Position_Type
   is
      function Internal
         (Notebook : System.Address) return Gtk.Enums.Gtk_Position_Type;
      pragma Import (C, Internal, "gtk_notebook_get_tab_pos");
   begin
      return Internal (Get_Object (Notebook));
   end Get_Tab_Pos;

   -------------------------
   -- Get_Tab_Reorderable --
   -------------------------

   function Get_Tab_Reorderable
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Boolean
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_notebook_get_tab_reorderable");
   begin
      return Boolean'Val (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Tab_Reorderable;

   ---------------------
   -- Get_Tab_Vborder --
   ---------------------

   function Get_Tab_Vborder
      (Notebook : not null access Gtk_Notebook_Record) return Guint16
   is
      function Internal (Notebook : System.Address) return Guint16;
      pragma Import (C, Internal, "gtk_notebook_get_tab_vborder");
   begin
      return Internal (Get_Object (Notebook));
   end Get_Tab_Vborder;

   -----------------
   -- Insert_Page --
   -----------------

   function Insert_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position  : Gint) return Gint
   is
      function Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address;
          Position  : Gint) return Gint;
      pragma Import (C, Internal, "gtk_notebook_insert_page");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)), Position);
   end Insert_Page;

   ----------------------
   -- Insert_Page_Menu --
   ----------------------

   function Insert_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class;
       Position   : Gint) return Gint
   is
      function Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Tab_Label  : System.Address;
          Menu_Label : System.Address;
          Position   : Gint) return Gint;
      pragma Import (C, Internal, "gtk_notebook_insert_page_menu");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)), Get_Object_Or_Null (GObject (Menu_Label)), Position);
   end Insert_Page_Menu;

   ---------------
   -- Next_Page --
   ---------------

   procedure Next_Page (Notebook : not null access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_next_page");
   begin
      Internal (Get_Object (Notebook));
   end Next_Page;

   --------------
   -- Page_Num --
   --------------

   function Page_Num
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
       return Gint
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_page_num");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child));
   end Page_Num;

   -------------------
   -- Popup_Disable --
   -------------------

   procedure Popup_Disable (Notebook : not null access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_popup_disable");
   begin
      Internal (Get_Object (Notebook));
   end Popup_Disable;

   ------------------
   -- Popup_Enable --
   ------------------

   procedure Popup_Enable (Notebook : not null access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_popup_enable");
   begin
      Internal (Get_Object (Notebook));
   end Popup_Enable;

   ------------------
   -- Prepend_Page --
   ------------------

   function Prepend_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_prepend_page");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)));
   end Prepend_Page;

   -----------------------
   -- Prepend_Page_Menu --
   -----------------------

   function Prepend_Page_Menu
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label  : access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Tab_Label  : System.Address;
          Menu_Label : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_notebook_prepend_page_menu");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)), Get_Object_Or_Null (GObject (Menu_Label)));
   end Prepend_Page_Menu;

   ---------------
   -- Prev_Page --
   ---------------

   procedure Prev_Page (Notebook : not null access Gtk_Notebook_Record) is
      procedure Internal (Notebook : System.Address);
      pragma Import (C, Internal, "gtk_notebook_prev_page");
   begin
      Internal (Get_Object (Notebook));
   end Prev_Page;

   -----------------
   -- Remove_Page --
   -----------------

   procedure Remove_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Gint)
   is
      procedure Internal (Notebook : System.Address; Page_Num : Gint);
      pragma Import (C, Internal, "gtk_notebook_remove_page");
   begin
      Internal (Get_Object (Notebook), Page_Num);
   end Remove_Page;

   -------------------
   -- Reorder_Child --
   -------------------

   procedure Reorder_Child
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Position : Gint)
   is
      procedure Internal
         (Notebook : System.Address;
          Child    : System.Address;
          Position : Gint);
      pragma Import (C, Internal, "gtk_notebook_reorder_child");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Position);
   end Reorder_Child;

   -----------------------
   -- Set_Action_Widget --
   -----------------------

   procedure Set_Action_Widget
      (Notebook  : not null access Gtk_Notebook_Record;
       Widget    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Pack_Type : Gtk.Enums.Gtk_Pack_Type)
   is
      procedure Internal
         (Notebook  : System.Address;
          Widget    : System.Address;
          Pack_Type : Gtk.Enums.Gtk_Pack_Type);
      pragma Import (C, Internal, "gtk_notebook_set_action_widget");
   begin
      Internal (Get_Object (Notebook), Get_Object (Widget), Pack_Type);
   end Set_Action_Widget;

   ----------------------
   -- Set_Current_Page --
   ----------------------

   procedure Set_Current_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Gint := -1)
   is
      procedure Internal (Notebook : System.Address; Page_Num : Gint);
      pragma Import (C, Internal, "gtk_notebook_set_current_page");
   begin
      Internal (Get_Object (Notebook), Page_Num);
   end Set_Current_Page;

   --------------------
   -- Set_Group_Name --
   --------------------

   procedure Set_Group_Name
      (Notebook   : not null access Gtk_Notebook_Record;
       Group_Name : UTF8_String := "")
   is
      procedure Internal
         (Notebook   : System.Address;
          Group_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_notebook_set_group_name");
      Tmp_Group_Name : Interfaces.C.Strings.chars_ptr;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Group_Name := New_String (Group_Name);
      end if;
      Internal (Get_Object (Notebook), Tmp_Group_Name);
      Free (Tmp_Group_Name);
   end Set_Group_Name;

   --------------------
   -- Set_Menu_Label --
   --------------------

   procedure Set_Menu_Label
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Menu_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_set_menu_label");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Menu_Label)));
   end Set_Menu_Label;

   -------------------------
   -- Set_Menu_Label_Text --
   -------------------------

   procedure Set_Menu_Label_Text
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Menu_Text : UTF8_String)
   is
      procedure Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Menu_Text : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_notebook_set_menu_label_text");
      Tmp_Menu_Text : Interfaces.C.Strings.chars_ptr := New_String (Menu_Text);
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Tmp_Menu_Text);
      Free (Tmp_Menu_Text);
   end Set_Menu_Label_Text;

   --------------------
   -- Set_Scrollable --
   --------------------

   procedure Set_Scrollable
      (Notebook   : not null access Gtk_Notebook_Record;
       Scrollable : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Scrollable : Integer);
      pragma Import (C, Internal, "gtk_notebook_set_scrollable");
   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Scrollable));
   end Set_Scrollable;

   ---------------------
   -- Set_Show_Border --
   ---------------------

   procedure Set_Show_Border
      (Notebook    : not null access Gtk_Notebook_Record;
       Show_Border : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Show_Border : Integer);
      pragma Import (C, Internal, "gtk_notebook_set_show_border");
   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Show_Border));
   end Set_Show_Border;

   -------------------
   -- Set_Show_Tabs --
   -------------------

   procedure Set_Show_Tabs
      (Notebook  : not null access Gtk_Notebook_Record;
       Show_Tabs : Boolean := True)
   is
      procedure Internal (Notebook : System.Address; Show_Tabs : Integer);
      pragma Import (C, Internal, "gtk_notebook_set_show_tabs");
   begin
      Internal (Get_Object (Notebook), Boolean'Pos (Show_Tabs));
   end Set_Show_Tabs;

   ------------------------
   -- Set_Tab_Detachable --
   ------------------------

   procedure Set_Tab_Detachable
      (Notebook   : not null access Gtk_Notebook_Record;
       Child      : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Detachable : Boolean := True)
   is
      procedure Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Detachable : Integer);
      pragma Import (C, Internal, "gtk_notebook_set_tab_detachable");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Boolean'Pos (Detachable));
   end Set_Tab_Detachable;

   -------------------
   -- Set_Tab_Label --
   -------------------

   procedure Set_Tab_Label
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address);
      pragma Import (C, Internal, "gtk_notebook_set_tab_label");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Get_Object_Or_Null (GObject (Tab_Label)));
   end Set_Tab_Label;

   ------------------------
   -- Set_Tab_Label_Text --
   ------------------------

   procedure Set_Tab_Label_Text
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Text : UTF8_String)
   is
      procedure Internal
         (Notebook : System.Address;
          Child    : System.Address;
          Tab_Text : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_notebook_set_tab_label_text");
      Tmp_Tab_Text : Interfaces.C.Strings.chars_ptr := New_String (Tab_Text);
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Tmp_Tab_Text);
      Free (Tmp_Tab_Text);
   end Set_Tab_Label_Text;

   -----------------
   -- Set_Tab_Pos --
   -----------------

   procedure Set_Tab_Pos
      (Notebook : not null access Gtk_Notebook_Record;
       Pos      : Gtk.Enums.Gtk_Position_Type)
   is
      procedure Internal
         (Notebook : System.Address;
          Pos      : Gtk.Enums.Gtk_Position_Type);
      pragma Import (C, Internal, "gtk_notebook_set_tab_pos");
   begin
      Internal (Get_Object (Notebook), Pos);
   end Set_Tab_Pos;

   -------------------------
   -- Set_Tab_Reorderable --
   -------------------------

   procedure Set_Tab_Reorderable
      (Notebook    : not null access Gtk_Notebook_Record;
       Child       : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Reorderable : Boolean := True)
   is
      procedure Internal
         (Notebook    : System.Address;
          Child       : System.Address;
          Reorderable : Integer);
      pragma Import (C, Internal, "gtk_notebook_set_tab_reorderable");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Boolean'Pos (Reorderable));
   end Set_Tab_Reorderable;

end Gtk.Notebook;
