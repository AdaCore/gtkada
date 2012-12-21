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

pragma Style_Checks (Off);
pragma Warnings (Off, "*is already use-visible*");
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;

package body Gtk.Toolbar is

   package Type_Conversion_Gtk_Toolbar is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Toolbar_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Toolbar);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Toolbar : out Gtk_Toolbar) is
   begin
      Toolbar := new Gtk_Toolbar_Record;
      Gtk.Toolbar.Initialize (Toolbar);
   end Gtk_New;

   ---------------------
   -- Gtk_Toolbar_New --
   ---------------------

   function Gtk_Toolbar_New return Gtk_Toolbar is
      Toolbar : constant Gtk_Toolbar := new Gtk_Toolbar_Record;
   begin
      Gtk.Toolbar.Initialize (Toolbar);
      return Toolbar;
   end Gtk_Toolbar_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Toolbar : not null access Gtk_Toolbar_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_new");
   begin
      Set_Object (Toolbar, Internal);
   end Initialize;

   --------------------
   -- Get_Drop_Index --
   --------------------

   function Get_Drop_Index
      (Toolbar : not null access Gtk_Toolbar_Record;
       X       : Gint;
       Y       : Gint) return Gint
   is
      function Internal
         (Toolbar : System.Address;
          X       : Gint;
          Y       : Gint) return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_drop_index");
   begin
      return Internal (Get_Object (Toolbar), X, Y);
   end Get_Drop_Index;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal
         (Toolbar : System.Address) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_toolbar_get_icon_size");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Icon_Size;

   --------------------
   -- Get_Item_Index --
   --------------------

   function Get_Item_Index
      (Toolbar : not null access Gtk_Toolbar_Record;
       Item    : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class)
       return Gint
   is
      function Internal
         (Toolbar : System.Address;
          Item    : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_item_index");
   begin
      return Internal (Get_Object (Toolbar), Get_Object (Item));
   end Get_Item_Index;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Toolbar : not null access Gtk_Toolbar_Record) return Gint
   is
      function Internal (Toolbar : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_toolbar_get_n_items");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_N_Items;

   ------------------
   -- Get_Nth_Item --
   ------------------

   function Get_Nth_Item
      (Toolbar : not null access Gtk_Toolbar_Record;
       N       : Gint) return Gtk.Tool_Item.Gtk_Tool_Item
   is
      function Internal
         (Toolbar : System.Address;
          N       : Gint) return System.Address;
      pragma Import (C, Internal, "gtk_toolbar_get_nth_item");
      Stub_Gtk_Tool_Item : Gtk.Tool_Item.Gtk_Tool_Item_Record;
   begin
      return Gtk.Tool_Item.Gtk_Tool_Item (Get_User_Data (Internal (Get_Object (Toolbar), N), Stub_Gtk_Tool_Item));
   end Get_Nth_Item;

   ----------------------
   -- Get_Relief_Style --
   ----------------------

   function Get_Relief_Style
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Relief_Style
   is
      function Internal
         (Toolbar : System.Address) return Gtk.Enums.Gtk_Relief_Style;
      pragma Import (C, Internal, "gtk_toolbar_get_relief_style");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Relief_Style;

   --------------------
   -- Get_Show_Arrow --
   --------------------

   function Get_Show_Arrow
      (Toolbar : not null access Gtk_Toolbar_Record) return Boolean
   is
      function Internal (Toolbar : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_toolbar_get_show_arrow");
   begin
      return Boolean'Val (Internal (Get_Object (Toolbar)));
   end Get_Show_Arrow;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
      (Toolbar : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Toolbar_Style
   is
      function Internal
         (Toolbar : System.Address) return Gtk.Enums.Gtk_Toolbar_Style;
      pragma Import (C, Internal, "gtk_toolbar_get_style");
   begin
      return Internal (Get_Object (Toolbar));
   end Get_Style;

   ------------
   -- Insert --
   ------------

   procedure Insert
      (Toolbar : not null access Gtk_Toolbar_Record;
       Item    : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Pos     : Gint := -1)
   is
      procedure Internal
         (Toolbar : System.Address;
          Item    : System.Address;
          Pos     : Gint);
      pragma Import (C, Internal, "gtk_toolbar_insert");
   begin
      Internal (Get_Object (Toolbar), Get_Object (Item), Pos);
   end Insert;

   -----------------------------
   -- Set_Drop_Highlight_Item --
   -----------------------------

   procedure Set_Drop_Highlight_Item
      (Toolbar   : not null access Gtk_Toolbar_Record;
       Tool_Item : access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
       Index     : Gint)
   is
      procedure Internal
         (Toolbar   : System.Address;
          Tool_Item : System.Address;
          Index     : Gint);
      pragma Import (C, Internal, "gtk_toolbar_set_drop_highlight_item");
   begin
      Internal (Get_Object (Toolbar), Get_Object_Or_Null (GObject (Tool_Item)), Index);
   end Set_Drop_Highlight_Item;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
      (Toolbar   : not null access Gtk_Toolbar_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Toolbar   : System.Address;
          Icon_Size : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_toolbar_set_icon_size");
   begin
      Internal (Get_Object (Toolbar), Icon_Size);
   end Set_Icon_Size;

   --------------------
   -- Set_Show_Arrow --
   --------------------

   procedure Set_Show_Arrow
      (Toolbar    : not null access Gtk_Toolbar_Record;
       Show_Arrow : Boolean := True)
   is
      procedure Internal (Toolbar : System.Address; Show_Arrow : Integer);
      pragma Import (C, Internal, "gtk_toolbar_set_show_arrow");
   begin
      Internal (Get_Object (Toolbar), Boolean'Pos (Show_Arrow));
   end Set_Show_Arrow;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
      (Toolbar : not null access Gtk_Toolbar_Record;
       Style   : Gtk.Enums.Gtk_Toolbar_Style)
   is
      procedure Internal
         (Toolbar : System.Address;
          Style   : Gtk.Enums.Gtk_Toolbar_Style);
      pragma Import (C, Internal, "gtk_toolbar_set_style");
   begin
      Internal (Get_Object (Toolbar), Style);
   end Set_Style;

   ---------------------
   -- Unset_Icon_Size --
   ---------------------

   procedure Unset_Icon_Size (Toolbar : not null access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_icon_size");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Icon_Size;

   -----------------
   -- Unset_Style --
   -----------------

   procedure Unset_Style (Toolbar : not null access Gtk_Toolbar_Record) is
      procedure Internal (Toolbar : System.Address);
      pragma Import (C, Internal, "gtk_toolbar_unset_style");
   begin
      Internal (Get_Object (Toolbar));
   end Unset_Style;

   ------------------------
   -- Get_Ellipsize_Mode --
   ------------------------

   function Get_Ellipsize_Mode
      (Self : not null access Gtk_Toolbar_Record)
       return Pango.Layout.Pango_Ellipsize_Mode
   is
      function Internal
         (Self : System.Address) return Pango.Layout.Pango_Ellipsize_Mode;
      pragma Import (C, Internal, "gtk_tool_shell_get_ellipsize_mode");
   begin
      return Internal (Get_Object (Self));
   end Get_Ellipsize_Mode;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Toolbar_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ------------------------
   -- Get_Text_Alignment --
   ------------------------

   function Get_Text_Alignment
      (Self : not null access Gtk_Toolbar_Record) return Gfloat
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
      (Self : not null access Gtk_Toolbar_Record)
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
      (Self : not null access Gtk_Toolbar_Record)
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

   procedure Rebuild_Menu (Self : not null access Gtk_Toolbar_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tool_shell_rebuild_menu");
   begin
      Internal (Get_Object (Self));
   end Rebuild_Menu;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Toolbar_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   --------------------------
   -- On_Focus_Home_Or_End --
   --------------------------

   procedure On_Focus_Home_Or_End
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access function
         (Self       : access Gtk_Toolbar_Record'Class;
          Focus_Home : Boolean) return Boolean)
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Focus_Home_Or_End;

   --------------------------
   -- On_Focus_Home_Or_End --
   --------------------------

   procedure On_Focus_Home_Or_End
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access function
         (Self       : access Glib.Object.GObject_Record'Class;
          Focus_Home : Boolean) return Boolean;
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Focus_Home_Or_End;

   ----------------------------
   -- On_Orientation_Changed --
   ----------------------------

   procedure On_Orientation_Changed
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access procedure
         (Self        : access Gtk_Toolbar_Record'Class;
          Orientation : Gtk.Enums.Gtk_Orientation))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Orientation_Changed;

   ----------------------------
   -- On_Orientation_Changed --
   ----------------------------

   procedure On_Orientation_Changed
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access procedure
         (Self        : access Glib.Object.GObject_Record'Class;
          Orientation : Gtk.Enums.Gtk_Orientation);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Orientation_Changed;

   ---------------------------
   -- On_Popup_Context_Menu --
   ---------------------------

   procedure On_Popup_Context_Menu
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access function
         (Self   : access Gtk_Toolbar_Record'Class;
          X      : Gint;
          Y      : Gint;
          Button : Gint) return Boolean)
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Popup_Context_Menu;

   ---------------------------
   -- On_Popup_Context_Menu --
   ---------------------------

   procedure On_Popup_Context_Menu
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access function
         (Self   : access Glib.Object.GObject_Record'Class;
          X      : Gint;
          Y      : Gint;
          Button : Gint) return Boolean;
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Popup_Context_Menu;

   ----------------------
   -- On_Style_Changed --
   ----------------------

   procedure On_Style_Changed
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access procedure
         (Self  : access Gtk_Toolbar_Record'Class;
          Style : Gtk.Enums.Gtk_Toolbar_Style))
   is
      pragma Unreferenced (Self, Call);
   begin
      null;
   end On_Style_Changed;

   ----------------------
   -- On_Style_Changed --
   ----------------------

   procedure On_Style_Changed
      (Self : not null access Gtk_Toolbar_Record;
       Call : not null access procedure
         (Self  : access Glib.Object.GObject_Record'Class;
          Style : Gtk.Enums.Gtk_Toolbar_Style);
       Slot : not null access Glib.Object.GObject_Record'Class)
   is
      pragma Unreferenced (Self, Call, Slot);
   begin
      null;
   end On_Style_Changed;

end Gtk.Toolbar;
