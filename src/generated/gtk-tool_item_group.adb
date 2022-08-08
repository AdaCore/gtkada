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
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Gtkada.Bindings;            use Gtkada.Bindings;
pragma Warnings(Off);  --  might be unused
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Tool_Item_Group is

   package Type_Conversion_Gtk_Tool_Item_Group is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tool_Item_Group_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tool_Item_Group);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Tool_Item_Group; Label : UTF8_String) is
   begin
      Self := new Gtk_Tool_Item_Group_Record;
      Gtk.Tool_Item_Group.Initialize (Self, Label);
   end Gtk_New;

   -----------------------------
   -- Gtk_Tool_Item_Group_New --
   -----------------------------

   function Gtk_Tool_Item_Group_New
      (Label : UTF8_String) return Gtk_Tool_Item_Group
   is
      Self : constant Gtk_Tool_Item_Group := new Gtk_Tool_Item_Group_Record;
   begin
      Gtk.Tool_Item_Group.Initialize (Self, Label);
      return Self;
   end Gtk_Tool_Item_Group_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self  : not null access Gtk_Tool_Item_Group_Record'Class;
       Label : UTF8_String)
   is
      function Internal
         (Label : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_group_new");
      Tmp_Label  : Gtkada.Types.Chars_Ptr := New_String (Label);
      Tmp_Return : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Return := Internal (Tmp_Label);
         Free (Tmp_Label);
         Set_Object (Self, Tmp_Return);
      end if;
   end Initialize;

   -------------------
   -- Get_Collapsed --
   -------------------

   function Get_Collapsed
      (Self : not null access Gtk_Tool_Item_Group_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tool_item_group_get_collapsed");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Collapsed;

   -------------------
   -- Get_Drop_Item --
   -------------------

   function Get_Drop_Item
      (Self : not null access Gtk_Tool_Item_Group_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Tool_Item.Gtk_Tool_Item
   is
      function Internal
         (Self : System.Address;
          X    : Glib.Gint;
          Y    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_group_get_drop_item");
      Stub_Gtk_Tool_Item : Gtk.Tool_Item.Gtk_Tool_Item_Record;
   begin
      return Gtk.Tool_Item.Gtk_Tool_Item (Get_User_Data (Internal (Get_Object (Self), X, Y), Stub_Gtk_Tool_Item));
   end Get_Drop_Item;

   -------------------
   -- Get_Ellipsize --
   -------------------

   function Get_Ellipsize
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Self : System.Address) return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_tool_item_group_get_ellipsize");
   begin
      return Internal (Get_Object (Self));
   end Get_Ellipsize;

   -----------------------
   -- Get_Header_Relief --
   -----------------------

   function Get_Header_Relief
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_tool_item_group_get_header_relief");
   begin
      return Internal (Get_Object (Self));
   end Get_Header_Relief;

   -----------------------
   -- Get_Item_Position --
   -----------------------

   function Get_Item_Position
      (Self : not null access Gtk_Tool_Item_Group_Record;
       Item : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Self : System.Address;
          Item : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tool_item_group_get_item_position");
   begin
      return Internal (Get_Object (Self), Get_Object (Item));
   end Get_Item_Position;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
      (Self : not null access Gtk_Tool_Item_Group_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_tool_item_group_get_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Label;

   ----------------------
   -- Get_Label_Widget --
   ----------------------

   function Get_Label_Widget
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_group_get_label_widget");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Label_Widget;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Self : not null access Gtk_Tool_Item_Group_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_tool_item_group_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   ------------------
   -- Get_Nth_Item --
   ------------------

   function Get_Nth_Item
      (Self  : not null access Gtk_Tool_Item_Group_Record;
       Index : Guint) return Gtk.Tool_Item.Gtk_Tool_Item
   is
      function Internal
         (Self  : System.Address;
          Index : Guint) return System.Address;
      pragma Import (C, Internal, "gtk_tool_item_group_get_nth_item");
      Stub_Gtk_Tool_Item : Gtk.Tool_Item.Gtk_Tool_Item_Record;
   begin
      return Gtk.Tool_Item.Gtk_Tool_Item (Get_User_Data (Internal (Get_Object (Self), Index), Stub_Gtk_Tool_Item));
   end Get_Nth_Item;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Self     : not null access Gtk_Tool_Item_Group_Record;
       Item     : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Item     : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_tool_item_group_insert");
   begin
      Internal (Get_Object (Self), Get_Object (Item), Position);
   end Insert;

   -------------------
   -- Set_Collapsed --
   -------------------

   procedure Set_Collapsed
      (Self      : not null access Gtk_Tool_Item_Group_Record;
       Collapsed : Boolean)
   is
      procedure Internal (Self : System.Address; Collapsed : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tool_item_group_set_collapsed");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Collapsed));
   end Set_Collapsed;

   -------------------
   -- Set_Ellipsize --
   -------------------

   procedure Set_Ellipsize
      (Self      : not null access Gtk_Tool_Item_Group_Record;
       Ellipsize : Pango.Layout.Pango_Ellipsize_Mode)
   is
      procedure Internal
         (Self      : System.Address;
          Ellipsize : Pango.Layout.Pango_Ellipsize_Mode);
      pragma Import (C, Internal, "gtk_tool_item_group_set_ellipsize");
   begin
      Internal (Get_Object (Self), Ellipsize);
   end Set_Ellipsize;

   -----------------------
   -- Set_Header_Relief --
   -----------------------

   procedure Set_Header_Relief
      (Self  : not null access Gtk_Tool_Item_Group_Record;
       Style : Gtk.Enums.Gtk_Relief_Style)
   is
      procedure Internal
         (Self  : System.Address;
          Style : Gtk.Enums.Gtk_Relief_Style);
      pragma Import (C, Internal, "gtk_tool_item_group_set_header_relief");
   begin
      Internal (Get_Object (Self), Style);
   end Set_Header_Relief;

   -----------------------
   -- Set_Item_Position --
   -----------------------

   procedure Set_Item_Position
      (Self     : not null access Gtk_Tool_Item_Group_Record;
       Item     : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Item     : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_tool_item_group_set_item_position");
   begin
      Internal (Get_Object (Self), Get_Object (Item), Position);
   end Set_Item_Position;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label
      (Self  : not null access Gtk_Tool_Item_Group_Record;
       Label : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_tool_item_group_set_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr := New_String (Label);
   begin
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Label;

   ----------------------
   -- Set_Label_Widget --
   ----------------------

   procedure Set_Label_Widget
      (Self         : not null access Gtk_Tool_Item_Group_Record;
       Label_Widget : not null access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal
         (Self         : System.Address;
          Label_Widget : System.Address);
      pragma Import (C, Internal, "gtk_tool_item_group_set_label_widget");
   begin
      Internal (Get_Object (Self), Get_Object (Label_Widget));
   end Set_Label_Widget;

   ------------------------
   -- Get_Ellipsize_Mode --
   ------------------------

   function Get_Ellipsize_Mode
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Self : System.Address) return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_tool_shell_get_ellipsize_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Ellipsize_Mode;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_tool_shell_get_icon_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Icon_Size;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_tool_shell_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ----------------------
   -- Get_Relief_Style --
   ----------------------

   function Get_Relief_Style
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_tool_shell_get_relief_style");
   begin
      return Internal (Get_Object (Self));
   end Get_Relief_Style;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Toolbar_Style
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Toolbar_Style;
      pragma Import (C, Internal, "gtk_tool_shell_get_style");
   begin
      return Internal (Get_Object (Self));
   end Get_Style;

   ------------------------
   -- Get_Text_Alignment --
   ------------------------

   function Get_Text_Alignment
      (Self : not null access Gtk_Tool_Item_Group_Record) return Gfloat
   is
      function Internal (Self : System.Address) return Gfloat;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_alignment");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Alignment;

   --------------------------
   -- Get_Text_Orientation --
   --------------------------

   function Get_Text_Orientation
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Text_Orientation;

   -------------------------
   -- Get_Text_Size_Group --
   -------------------------

   function Get_Text_Size_Group
      (Self : not null access Gtk_Tool_Item_Group_Record)
       return Gtk.Size_Group.Gtk_Size_Group
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tool_shell_get_text_size_group");
      Stub_Gtk_Size_Group : Gtk.Size_Group.Gtk_Size_Group_Record;
   begin
      return Gtk.Size_Group.Gtk_Size_Group (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Size_Group));
   end Get_Text_Size_Group;

   ------------------
   -- Rebuild_Menu --
   ------------------

   procedure Rebuild_Menu
      (Self : not null access Gtk_Tool_Item_Group_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tool_shell_rebuild_menu");
   begin
      Internal (Get_Object (Self));
   end Rebuild_Menu;

end Gtk.Tool_Item_Group;
