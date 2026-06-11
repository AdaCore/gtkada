------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2026, AdaCore                     --
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

package body Gtk.List_Item is

   package Type_Conversion_Gtk_List_Item is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_List_Item_Record);
   pragma Unreferenced (Type_Conversion_Gtk_List_Item);

   --------------------------------
   -- Get_Accessible_Description --
   --------------------------------

   function Get_Accessible_Description
      (Self : not null access Gtk_List_Item_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_list_item_get_accessible_description");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Accessible_Description;

   --------------------------
   -- Get_Accessible_Label --
   --------------------------

   function Get_Accessible_Label
      (Self : not null access Gtk_List_Item_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_list_item_get_accessible_label");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Accessible_Label;

   ---------------------
   -- Get_Activatable --
   ---------------------

   function Get_Activatable
      (Self : not null access Gtk_List_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_item_get_activatable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Activatable;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
      (Self : not null access Gtk_List_Item_Record)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_item_get_child");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Widget));
   end Get_Child;

   -------------------
   -- Get_Focusable --
   -------------------

   function Get_Focusable
      (Self : not null access Gtk_List_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_item_get_focusable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Focusable;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self : not null access Gtk_List_Item_Record) return System.Address
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_list_item_get_item");
   begin
      return Internal (Get_Object (Self));
   end Get_Item;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Self : not null access Gtk_List_Item_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "gtk_list_item_get_position");
   begin
      return Internal (Get_Object (Self));
   end Get_Position;

   --------------------
   -- Get_Selectable --
   --------------------

   function Get_Selectable
      (Self : not null access Gtk_List_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_item_get_selectable");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Selectable;

   ------------------
   -- Get_Selected --
   ------------------

   function Get_Selected
      (Self : not null access Gtk_List_Item_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_list_item_get_selected");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Selected;

   --------------------------------
   -- Set_Accessible_Description --
   --------------------------------

   procedure Set_Accessible_Description
      (Self        : not null access Gtk_List_Item_Record;
       Description : UTF8_String)
   is
      procedure Internal
         (Self        : System.Address;
          Description : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_list_item_set_accessible_description");
      Tmp_Description : Gtkada.Types.Chars_Ptr := New_String (Description);
   begin
      Internal (Get_Object (Self), Tmp_Description);
      Free (Tmp_Description);
   end Set_Accessible_Description;

   --------------------------
   -- Set_Accessible_Label --
   --------------------------

   procedure Set_Accessible_Label
      (Self  : not null access Gtk_List_Item_Record;
       Label : UTF8_String)
   is
      procedure Internal
         (Self  : System.Address;
          Label : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_list_item_set_accessible_label");
      Tmp_Label : Gtkada.Types.Chars_Ptr := New_String (Label);
   begin
      Internal (Get_Object (Self), Tmp_Label);
      Free (Tmp_Label);
   end Set_Accessible_Label;

   ---------------------
   -- Set_Activatable --
   ---------------------

   procedure Set_Activatable
      (Self        : not null access Gtk_List_Item_Record;
       Activatable : Boolean)
   is
      procedure Internal
         (Self        : System.Address;
          Activatable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_list_item_set_activatable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Activatable));
   end Set_Activatable;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
      (Self  : not null access Gtk_List_Item_Record;
       Child : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      procedure Internal (Self : System.Address; Child : System.Address);
      pragma Import (C, Internal, "gtk_list_item_set_child");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Child)));
   end Set_Child;

   -------------------
   -- Set_Focusable --
   -------------------

   procedure Set_Focusable
      (Self      : not null access Gtk_List_Item_Record;
       Focusable : Boolean)
   is
      procedure Internal (Self : System.Address; Focusable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_list_item_set_focusable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Focusable));
   end Set_Focusable;

   --------------------
   -- Set_Selectable --
   --------------------

   procedure Set_Selectable
      (Self       : not null access Gtk_List_Item_Record;
       Selectable : Boolean)
   is
      procedure Internal (Self : System.Address; Selectable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_list_item_set_selectable");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Selectable));
   end Set_Selectable;

end Gtk.List_Item;
