-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--   Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet   --
--                Copyright (C) 2000-2011, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.List is

   package Type_Conversion is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_List_Record);
   pragma Unreferenced (Type_Conversion);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_List) is
   begin
      Self := new Gtk_List_Record;
      Gtk.List.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : access Gtk_List_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_list_new");
   begin
      Set_Object (Self, Internal);
   end Initialize;

   ------------------
   -- Append_Items --
   ------------------

   procedure Append_Items
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist)
   is
      procedure Internal (Self : System.Address; Items : Widget_List.Glist);
      pragma Import (C, Internal, "gtk_list_append_items");
   begin
      Internal (Get_Object (Self), Items);
   end Append_Items;

   --------------------
   -- Child_Position --
   --------------------

   function Child_Position
      (Self  : access Gtk_List_Record;
       Child : access Gtk_Widget_Record'Class) return Gint
   is
      function Internal
         (Self  : System.Address;
          Child : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_list_child_position");
   begin
      return Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Child_Position;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items
      (Self    : access Gtk_List_Record;
       Start   : Gint;
       The_End : Gint)
   is
      procedure Internal
         (Self    : System.Address;
          Start   : Gint;
          The_End : Gint);
      pragma Import (C, Internal, "gtk_list_clear_items");
   begin
      Internal (Get_Object (Self), Start, The_End);
   end Clear_Items;

   ------------------------
   -- End_Drag_Selection --
   ------------------------

   procedure End_Drag_Selection (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_end_drag_selection");
   begin
      Internal (Get_Object (Self));
   end End_Drag_Selection;

   -------------------
   -- End_Selection --
   -------------------

   procedure End_Selection (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_end_selection");
   begin
      Internal (Get_Object (Self));
   end End_Selection;

   ----------------------
   -- Extend_Selection --
   ----------------------

   procedure Extend_Selection
      (Self                 : access Gtk_List_Record;
       Scroll_Type          : Gtk.Enums.Gtk_Scroll_Type;
       Position             : Gfloat;
       Auto_Start_Selection : Boolean)
   is
      procedure Internal
         (Self                 : System.Address;
          Scroll_Type          : Integer;
          Position             : Gfloat;
          Auto_Start_Selection : Integer);
      pragma Import (C, Internal, "gtk_list_extend_selection");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Scroll_Type'Pos (Scroll_Type), Position, Boolean'Pos (Auto_Start_Selection));
   end Extend_Selection;

   ------------------
   -- Insert_Items --
   ------------------

   procedure Insert_Items
      (Self     : access Gtk_List_Record;
       Items    : Widget_List.Glist;
       Position : Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Items    : Widget_List.Glist;
          Position : Gint);
      pragma Import (C, Internal, "gtk_list_insert_items");
   begin
      Internal (Get_Object (Self), Items, Position);
   end Insert_Items;

   -------------------
   -- Prepend_Items --
   -------------------

   procedure Prepend_Items
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist)
   is
      procedure Internal (Self : System.Address; Items : Widget_List.Glist);
      pragma Import (C, Internal, "gtk_list_prepend_items");
   begin
      Internal (Get_Object (Self), Items);
   end Prepend_Items;

   ------------------
   -- Remove_Items --
   ------------------

   procedure Remove_Items
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist)
   is
      procedure Internal (Self : System.Address; Items : Widget_List.Glist);
      pragma Import (C, Internal, "gtk_list_remove_items");
   begin
      Internal (Get_Object (Self), Items);
   end Remove_Items;

   ---------------------------
   -- Remove_Items_No_Unref --
   ---------------------------

   procedure Remove_Items_No_Unref
      (Self  : access Gtk_List_Record;
       Items : Widget_List.Glist)
   is
      procedure Internal (Self : System.Address; Items : Widget_List.Glist);
      pragma Import (C, Internal, "gtk_list_remove_items_no_unref");
   begin
      Internal (Get_Object (Self), Items);
   end Remove_Items_No_Unref;

   -----------------------
   -- Scroll_Horizontal --
   -----------------------

   procedure Scroll_Horizontal
      (Self        : access Gtk_List_Record;
       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
       Position    : Gfloat)
   is
      procedure Internal
         (Self        : System.Address;
          Scroll_Type : Integer;
          Position    : Gfloat);
      pragma Import (C, Internal, "gtk_list_scroll_horizontal");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Scroll_Type'Pos (Scroll_Type), Position);
   end Scroll_Horizontal;

   ---------------------
   -- Scroll_Vertical --
   ---------------------

   procedure Scroll_Vertical
      (Self        : access Gtk_List_Record;
       Scroll_Type : Gtk.Enums.Gtk_Scroll_Type;
       Position    : Gfloat)
   is
      procedure Internal
         (Self        : System.Address;
          Scroll_Type : Integer;
          Position    : Gfloat);
      pragma Import (C, Internal, "gtk_list_scroll_vertical");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Scroll_Type'Pos (Scroll_Type), Position);
   end Scroll_Vertical;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_select_all");
   begin
      Internal (Get_Object (Self));
   end Select_All;

   ------------------
   -- Select_Child --
   ------------------

   procedure Select_Child
      (Self  : access Gtk_List_Record;
       Child : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_list_select_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Select_Child;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item (Self : access Gtk_List_Record; Item : Gint) is
      procedure Internal (Self : System.Address; Item : Gint);
      pragma Import (C, Internal, "gtk_list_select_item");
   begin
      Internal (Get_Object (Self), Item);
   end Select_Item;

   ------------------------
   -- Set_Selection_Mode --
   ------------------------

   procedure Set_Selection_Mode
      (Self : access Gtk_List_Record;
       Mode : Gtk.Enums.Gtk_Selection_Mode)
   is
      procedure Internal (Self : System.Address; Mode : Integer);
      pragma Import (C, Internal, "gtk_list_set_selection_mode");
   begin
      Internal (Get_Object (Self), Gtk.Enums.Gtk_Selection_Mode'Pos (Mode));
   end Set_Selection_Mode;

   ---------------------
   -- Start_Selection --
   ---------------------

   procedure Start_Selection (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_start_selection");
   begin
      Internal (Get_Object (Self));
   end Start_Selection;

   ---------------------
   -- Toggle_Add_Mode --
   ---------------------

   procedure Toggle_Add_Mode (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_toggle_add_mode");
   begin
      Internal (Get_Object (Self));
   end Toggle_Add_Mode;

   ----------------------
   -- Toggle_Focus_Row --
   ----------------------

   procedure Toggle_Focus_Row (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_toggle_focus_row");
   begin
      Internal (Get_Object (Self));
   end Toggle_Focus_Row;

   ----------------
   -- Toggle_Row --
   ----------------

   procedure Toggle_Row
      (Self : access Gtk_List_Record;
       Item : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Item : System.Address);
      pragma Import (C, Internal, "gtk_list_toggle_row");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Item)));
   end Toggle_Row;

   --------------------
   -- Undo_Selection --
   --------------------

   procedure Undo_Selection (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_undo_selection");
   begin
      Internal (Get_Object (Self));
   end Undo_Selection;

   ------------------
   -- Unselect_All --
   ------------------

   procedure Unselect_All (Self : access Gtk_List_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_list_unselect_all");
   begin
      Internal (Get_Object (Self));
   end Unselect_All;

   --------------------
   -- Unselect_Child --
   --------------------

   procedure Unselect_Child
      (Self  : access Gtk_List_Record;
       Child : access Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_list_unselect_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Unselect_Child;

   -------------------
   -- Unselect_Item --
   -------------------

   procedure Unselect_Item (Self : access Gtk_List_Record; Item : Gint) is
      procedure Internal (Self : System.Address; Item : Gint);
      pragma Import (C, Internal, "gtk_list_unselect_item");
   begin
      Internal (Get_Object (Self), Item);
   end Unselect_Item;

   function Get_Selection (Widget : access Gtk.List.Gtk_List_Record)
   return Widget_List.Glist
   is
      use Widget_List;
      function Internal (Widget : in System.Address)
      return      System.Address;
      pragma Import (C, Internal, "ada_list_get_selection");
      List : Gtk.Widget.Widget_List.Glist;
   begin
      Set_Object (List, Internal (Get_Object (Widget)));
      return List;
   end Get_Selection;

end Gtk.List;
