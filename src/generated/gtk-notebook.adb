------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

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

   package Type_Conversion_Gtk_Notebook is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Notebook_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Notebook);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Notebook : out Gtk_Notebook) is
   begin
      Notebook := new Gtk_Notebook_Record;
      Gtk.Notebook.Initialize (Notebook);
   end Gtk_New;

   ----------------------
   -- Gtk_Notebook_New --
   ----------------------

   function Gtk_Notebook_New return Gtk_Notebook is
      Notebook : constant Gtk_Notebook := new Gtk_Notebook_Record;
   begin
      Gtk.Notebook.Initialize (Notebook);
      return Notebook;
   end Gtk_Notebook_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Notebook : not null access Gtk_Notebook_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_notebook_new");
   begin
      if not Notebook.Is_Created then
         Set_Object (Notebook, Internal);
      end if;
   end Initialize;

   -----------------
   -- Append_Page --
   -----------------

   function Append_Page
      (Notebook  : not null access Gtk_Notebook_Record;
       Child     : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address) return Glib.Gint;
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

   ----------------
   -- Detach_Tab --
   ----------------

   procedure Detach_Tab
      (Notebook : not null access Gtk_Notebook_Record;
       Child    : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Notebook : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_notebook_detach_tab");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child));
   end Detach_Tab;

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
      (Notebook : not null access Gtk_Notebook_Record) return Glib.Gint
   is
      function Internal (Notebook : System.Address) return Glib.Gint;
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
         (Notebook : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_notebook_get_group_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Notebook)));
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
          Child    : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_notebook_get_menu_label_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Notebook), Get_Object (Child)));
   end Get_Menu_Label_Text;

   -----------------
   -- Get_N_Pages --
   -----------------

   function Get_N_Pages
      (Notebook : not null access Gtk_Notebook_Record) return Glib.Gint
   is
      function Internal (Notebook : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_notebook_get_n_pages");
   begin
      return Internal (Get_Object (Notebook));
   end Get_N_Pages;

   ------------------
   -- Get_Nth_Page --
   ------------------

   function Get_Nth_Page
      (Notebook : not null access Gtk_Notebook_Record;
       Page_Num : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Notebook : System.Address;
          Page_Num : Glib.Gint) return System.Address;
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
      function Internal (Notebook : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_scrollable");
   begin
      return Internal (Get_Object (Notebook)) /= 0;
   end Get_Scrollable;

   ---------------------
   -- Get_Show_Border --
   ---------------------

   function Get_Show_Border
      (Notebook : not null access Gtk_Notebook_Record) return Boolean
   is
      function Internal (Notebook : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_show_border");
   begin
      return Internal (Get_Object (Notebook)) /= 0;
   end Get_Show_Border;

   -------------------
   -- Get_Show_Tabs --
   -------------------

   function Get_Show_Tabs
      (Notebook : not null access Gtk_Notebook_Record) return Boolean
   is
      function Internal (Notebook : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_show_tabs");
   begin
      return Internal (Get_Object (Notebook)) /= 0;
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
          Child    : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_tab_detachable");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child)) /= 0;
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
          Child    : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_notebook_get_tab_label_text");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Notebook), Get_Object (Child)));
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
          Child    : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_notebook_get_tab_reorderable");
   begin
      return Internal (Get_Object (Notebook), Get_Object (Child)) /= 0;
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
       Position  : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address;
          Position  : Glib.Gint) return Glib.Gint;
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
       Position   : Glib.Gint) return Glib.Gint
   is
      function Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Tab_Label  : System.Address;
          Menu_Label : System.Address;
          Position   : Glib.Gint) return Glib.Gint;
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
       return Glib.Gint
   is
      function Internal
         (Notebook : System.Address;
          Child    : System.Address) return Glib.Gint;
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
       Tab_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Notebook  : System.Address;
          Child     : System.Address;
          Tab_Label : System.Address) return Glib.Gint;
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
       Menu_Label : access Gtk.Widget.Gtk_Widget_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Notebook   : System.Address;
          Child      : System.Address;
          Tab_Label  : System.Address;
          Menu_Label : System.Address) return Glib.Gint;
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
       Page_Num : Glib.Gint)
   is
      procedure Internal (Notebook : System.Address; Page_Num : Glib.Gint);
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
       Position : Glib.Gint)
   is
      procedure Internal
         (Notebook : System.Address;
          Child    : System.Address;
          Position : Glib.Gint);
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
       Page_Num : Glib.Gint := -1)
   is
      procedure Internal (Notebook : System.Address; Page_Num : Glib.Gint);
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
          Group_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_notebook_set_group_name");
      Tmp_Group_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Group_Name = "" then
         Tmp_Group_Name := Gtkada.Types.Null_Ptr;
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
          Menu_Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_notebook_set_menu_label_text");
      Tmp_Menu_Text : Gtkada.Types.Chars_Ptr := New_String (Menu_Text);
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
      procedure Internal
         (Notebook   : System.Address;
          Scrollable : Glib.Gboolean);
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
      procedure Internal
         (Notebook    : System.Address;
          Show_Border : Glib.Gboolean);
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
      procedure Internal
         (Notebook  : System.Address;
          Show_Tabs : Glib.Gboolean);
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
          Detachable : Glib.Gboolean);
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
          Tab_Text : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_notebook_set_tab_label_text");
      Tmp_Tab_Text : Gtkada.Types.Chars_Ptr := New_String (Tab_Text);
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
          Reorderable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_notebook_set_tab_reorderable");
   begin
      Internal (Get_Object (Notebook), Get_Object (Child), Boolean'Pos (Reorderable));
   end Set_Tab_Reorderable;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Notebook_Tab_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Notebook_Tab_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Gtk_Widget_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Gtk_Widget_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Widget_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Widget_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gtk_Direction_Type_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gtk_Direction_Type_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Notebook_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Notebook_Boolean_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Boolean_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Boolean_Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gint_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Direction_Type_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Boolean_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Notebook_Tab_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Boolean_Boolean);

   procedure Marsh_GObject_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Boolean);

   procedure Marsh_GObject_Gtk_Direction_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Boolean_Boolean);

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Direction_Type_Void);

   procedure Marsh_GObject_Gtk_Notebook_Tab_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Notebook_Tab_Boolean);

   procedure Marsh_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook);

   procedure Marsh_GObject_Gtk_Widget_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gtk_Widget_Guint_Void);

   procedure Marsh_Gtk_Notebook_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Boolean_Boolean);

   procedure Marsh_Gtk_Notebook_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Gint_Boolean);

   procedure Marsh_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean);

   procedure Marsh_Gtk_Notebook_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Gtk_Direction_Type_Void);

   procedure Marsh_Gtk_Notebook_Gtk_Notebook_Tab_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Gtk_Notebook_Tab_Boolean);

   procedure Marsh_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook);

   procedure Marsh_Gtk_Notebook_Gtk_Widget_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Notebook_Gtk_Widget_Guint_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Gtk_Notebook_Tab_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Direction_Type_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Gtk_Widget_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Notebook_Boolean_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Notebook_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Notebook_Tab_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Notebook_Tab_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Direction_Type_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Widget_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Widget_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gtk_Direction_Type_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gtk_Direction_Type_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Notebook_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Boolean_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Boolean_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   -----------------------------------
   -- Marsh_GObject_Boolean_Boolean --
   -----------------------------------

   procedure Marsh_GObject_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Boolean_Boolean;

   --------------------------------
   -- Marsh_GObject_Gint_Boolean --
   --------------------------------

   procedure Marsh_GObject_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Boolean;

   ------------------------------------------------------
   -- Marsh_GObject_Gtk_Direction_Type_Boolean_Boolean --
   ------------------------------------------------------

   procedure Marsh_GObject_Gtk_Direction_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Direction_Type_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1), Unchecked_To_Boolean (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Direction_Type_Boolean_Boolean;

   -------------------------------------------
   -- Marsh_GObject_Gtk_Direction_Type_Void --
   -------------------------------------------

   procedure Marsh_GObject_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Direction_Type_Void;

   --------------------------------------------
   -- Marsh_GObject_Gtk_Notebook_Tab_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gtk_Notebook_Tab_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Notebook_Tab_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Notebook_Tab (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Notebook_Tab_Boolean;

   -----------------------------------------------------
   -- Marsh_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook --
   -----------------------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased not null access Gtk_Notebook_Record'Class := H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook;

   -----------------------------------------
   -- Marsh_GObject_Gtk_Widget_Guint_Void --
   -----------------------------------------

   procedure Marsh_GObject_Gtk_Widget_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gtk_Widget_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gtk_Widget_Guint_Void;

   ----------------------------------------
   -- Marsh_Gtk_Notebook_Boolean_Boolean --
   ----------------------------------------

   procedure Marsh_Gtk_Notebook_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Boolean (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Boolean_Boolean;

   -------------------------------------
   -- Marsh_Gtk_Notebook_Gint_Boolean --
   -------------------------------------

   procedure Marsh_Gtk_Notebook_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Gint_Boolean;

   -----------------------------------------------------------
   -- Marsh_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean --
   -----------------------------------------------------------

   procedure Marsh_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1), Unchecked_To_Boolean (Params, 2));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean;

   ------------------------------------------------
   -- Marsh_Gtk_Notebook_Gtk_Direction_Type_Void --
   ------------------------------------------------

   procedure Marsh_Gtk_Notebook_Gtk_Direction_Type_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Gtk_Direction_Type_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Gtk_Direction_Type (Params, 1));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Gtk_Direction_Type_Void;

   -------------------------------------------------
   -- Marsh_Gtk_Notebook_Gtk_Notebook_Tab_Boolean --
   -------------------------------------------------

   procedure Marsh_Gtk_Notebook_Gtk_Notebook_Tab_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gtk_Notebook_Tab (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Gtk_Notebook_Tab_Boolean;

   ----------------------------------------------------------
   -- Marsh_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook --
   ----------------------------------------------------------

   procedure Marsh_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
      V   : aliased not null access Gtk_Notebook_Record'Class := H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Gint (Params, 2), Unchecked_To_Gint (Params, 3));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook;

   ----------------------------------------------
   -- Marsh_Gtk_Notebook_Gtk_Widget_Guint_Void --
   ----------------------------------------------

   procedure Marsh_Gtk_Notebook_Gtk_Widget_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Notebook_Gtk_Widget_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Notebook := Gtk_Notebook (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Gtk.Widget.Gtk_Widget (Unchecked_To_Object (Params, 1)), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Notebook_Gtk_Widget_Guint_Void;

   ----------------------------
   -- On_Change_Current_Page --
   ----------------------------

   procedure On_Change_Current_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "change-current-page" & ASCII.NUL, Call, After);
   end On_Change_Current_Page;

   ----------------------------
   -- On_Change_Current_Page --
   ----------------------------

   procedure On_Change_Current_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "change-current-page" & ASCII.NUL, Call, After, Slot);
   end On_Change_Current_Page;

   ----------------------
   -- On_Create_Window --
   ----------------------

   procedure On_Create_Window
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       After : Boolean := False)
   is
   begin
      Connect (Self, "create-window" & ASCII.NUL, Call, After);
   end On_Create_Window;

   ----------------------
   -- On_Create_Window --
   ----------------------

   procedure On_Create_Window
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Gint_Gint_Gtk_Notebook;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "create-window" & ASCII.NUL, Call, After, Slot);
   end On_Create_Window;

   ------------------
   -- On_Focus_Tab --
   ------------------

   procedure On_Focus_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Notebook_Tab_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "focus-tab" & ASCII.NUL, Call, After);
   end On_Focus_Tab;

   ------------------
   -- On_Focus_Tab --
   ------------------

   procedure On_Focus_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Notebook_Tab_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "focus-tab" & ASCII.NUL, Call, After, Slot);
   end On_Focus_Tab;

   -----------------------
   -- On_Move_Focus_Out --
   -----------------------

   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Direction_Type_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "move-focus-out" & ASCII.NUL, Call, After);
   end On_Move_Focus_Out;

   -----------------------
   -- On_Move_Focus_Out --
   -----------------------

   procedure On_Move_Focus_Out
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "move-focus-out" & ASCII.NUL, Call, After, Slot);
   end On_Move_Focus_Out;

   -------------------
   -- On_Page_Added --
   -------------------

   procedure On_Page_Added
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "page-added" & ASCII.NUL, Call, After);
   end On_Page_Added;

   -------------------
   -- On_Page_Added --
   -------------------

   procedure On_Page_Added
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "page-added" & ASCII.NUL, Call, After, Slot);
   end On_Page_Added;

   ---------------------
   -- On_Page_Removed --
   ---------------------

   procedure On_Page_Removed
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "page-removed" & ASCII.NUL, Call, After);
   end On_Page_Removed;

   ---------------------
   -- On_Page_Removed --
   ---------------------

   procedure On_Page_Removed
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "page-removed" & ASCII.NUL, Call, After, Slot);
   end On_Page_Removed;

   -----------------------
   -- On_Page_Reordered --
   -----------------------

   procedure On_Page_Reordered
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "page-reordered" & ASCII.NUL, Call, After);
   end On_Page_Reordered;

   -----------------------
   -- On_Page_Reordered --
   -----------------------

   procedure On_Page_Reordered
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "page-reordered" & ASCII.NUL, Call, After, Slot);
   end On_Page_Reordered;

   --------------------
   -- On_Reorder_Tab --
   --------------------

   procedure On_Reorder_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Direction_Type_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "reorder-tab" & ASCII.NUL, Call, After);
   end On_Reorder_Tab;

   --------------------
   -- On_Reorder_Tab --
   --------------------

   procedure On_Reorder_Tab
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Direction_Type_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "reorder-tab" & ASCII.NUL, Call, After, Slot);
   end On_Reorder_Tab;

   --------------------
   -- On_Select_Page --
   --------------------

   procedure On_Select_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Boolean_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "select-page" & ASCII.NUL, Call, After);
   end On_Select_Page;

   --------------------
   -- On_Select_Page --
   --------------------

   procedure On_Select_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Boolean_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "select-page" & ASCII.NUL, Call, After, Slot);
   end On_Select_Page;

   --------------------
   -- On_Switch_Page --
   --------------------

   procedure On_Switch_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_Gtk_Notebook_Gtk_Widget_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "switch-page" & ASCII.NUL, Call, After);
   end On_Switch_Page;

   --------------------
   -- On_Switch_Page --
   --------------------

   procedure On_Switch_Page
      (Self  : not null access Gtk_Notebook_Record;
       Call  : Cb_GObject_Gtk_Widget_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "switch-page" & ASCII.NUL, Call, After, Slot);
   end On_Switch_Page;

end Gtk.Notebook;
