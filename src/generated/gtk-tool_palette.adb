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

package body Gtk.Tool_Palette is

   package Type_Conversion_Gtk_Tool_Palette is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Tool_Palette_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Tool_Palette);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Tool_Palette) is
   begin
      Self := new Gtk_Tool_Palette_Record;
      Gtk.Tool_Palette.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Gtk_Tool_Palette_New --
   --------------------------

   function Gtk_Tool_Palette_New return Gtk_Tool_Palette is
      Self : constant Gtk_Tool_Palette := new Gtk_Tool_Palette_Record;
   begin
      Gtk.Tool_Palette.Initialize (Self);
      return Self;
   end Gtk_Tool_Palette_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Tool_Palette_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_tool_palette_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -------------------
   -- Add_Drag_Dest --
   -------------------

   procedure Add_Drag_Dest
      (Self    : not null access Gtk_Tool_Palette_Record;
       Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
       Flags   : Gtk_Dest_Defaults;
       Targets : Gtk_Tool_Palette_Drag_Targets;
       Actions : Gdk.Drag_Contexts.Gdk_Drag_Action)
   is
      procedure Internal
         (Self    : System.Address;
          Widget  : System.Address;
          Flags   : Gtk_Dest_Defaults;
          Targets : Gtk_Tool_Palette_Drag_Targets;
          Actions : Gdk.Drag_Contexts.Gdk_Drag_Action);
      pragma Import (C, Internal, "gtk_tool_palette_add_drag_dest");
   begin
      Internal (Get_Object (Self), Get_Object (Widget), Flags, Targets, Actions);
   end Add_Drag_Dest;

   -------------------
   -- Get_Drag_Item --
   -------------------

   function Get_Drag_Item
      (Self      : not null access Gtk_Tool_Palette_Record;
       Selection : Gtk.Selection_Data.Gtk_Selection_Data)
       return Gtk.Widget.Gtk_Widget
   is
      function Internal
         (Self      : System.Address;
          Selection : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_tool_palette_get_drag_item");
      Stub_Gtk_Widget : Gtk.Widget.Gtk_Widget_Record;
   begin
      return Gtk.Widget.Gtk_Widget (Get_User_Data (Internal (Get_Object (Self), Get_Object (Selection)), Stub_Gtk_Widget));
   end Get_Drag_Item;

   --------------------
   -- Get_Drop_Group --
   --------------------

   function Get_Drop_Group
      (Self : not null access Gtk_Tool_Palette_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Tool_Item_Group.Gtk_Tool_Item_Group
   is
      function Internal
         (Self : System.Address;
          X    : Glib.Gint;
          Y    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_tool_palette_get_drop_group");
      Stub_Gtk_Tool_Item_Group : Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record;
   begin
      return Gtk.Tool_Item_Group.Gtk_Tool_Item_Group (Get_User_Data (Internal (Get_Object (Self), X, Y), Stub_Gtk_Tool_Item_Group));
   end Get_Drop_Group;

   -------------------
   -- Get_Drop_Item --
   -------------------

   function Get_Drop_Item
      (Self : not null access Gtk_Tool_Palette_Record;
       X    : Glib.Gint;
       Y    : Glib.Gint) return Gtk.Tool_Item.Gtk_Tool_Item
   is
      function Internal
         (Self : System.Address;
          X    : Glib.Gint;
          Y    : Glib.Gint) return System.Address;
      pragma Import (C, Internal, "gtk_tool_palette_get_drop_item");
      Stub_Gtk_Tool_Item : Gtk.Tool_Item.Gtk_Tool_Item_Record;
   begin
      return Gtk.Tool_Item.Gtk_Tool_Item (Get_User_Data (Internal (Get_Object (Self), X, Y), Stub_Gtk_Tool_Item));
   end Get_Drop_Item;

   -------------------
   -- Get_Exclusive --
   -------------------

   function Get_Exclusive
      (Self  : not null access Gtk_Tool_Palette_Record;
       Group : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class)
       return Boolean
   is
      function Internal
         (Self  : System.Address;
          Group : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tool_palette_get_exclusive");
   begin
      return Internal (Get_Object (Self), Get_Object (Group)) /= 0;
   end Get_Exclusive;

   ----------------
   -- Get_Expand --
   ----------------

   function Get_Expand
      (Self  : not null access Gtk_Tool_Palette_Record;
       Group : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class)
       return Boolean
   is
      function Internal
         (Self  : System.Address;
          Group : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_tool_palette_get_expand");
   begin
      return Internal (Get_Object (Self), Get_Object (Group)) /= 0;
   end Get_Expand;

   ------------------------
   -- Get_Group_Position --
   ------------------------

   function Get_Group_Position
      (Self  : not null access Gtk_Tool_Palette_Record;
       Group : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class)
       return Glib.Gint
   is
      function Internal
         (Self  : System.Address;
          Group : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_tool_palette_get_group_position");
   begin
      return Internal (Get_Object (Self), Get_Object (Group));
   end Get_Group_Position;

   -------------------
   -- Get_Icon_Size --
   -------------------

   function Get_Icon_Size
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Icon_Size
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Icon_Size;
      pragma Import (C, Internal, "gtk_tool_palette_get_icon_size");
   begin
      return Internal (Get_Object (Self));
   end Get_Icon_Size;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Toolbar_Style
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Toolbar_Style;
      pragma Import (C, Internal, "gtk_tool_palette_get_style");
   begin
      return Internal (Get_Object (Self));
   end Get_Style;

   ---------------------
   -- Set_Drag_Source --
   ---------------------

   procedure Set_Drag_Source
      (Self    : not null access Gtk_Tool_Palette_Record;
       Targets : Gtk_Tool_Palette_Drag_Targets)
   is
      procedure Internal
         (Self    : System.Address;
          Targets : Gtk_Tool_Palette_Drag_Targets);
      pragma Import (C, Internal, "gtk_tool_palette_set_drag_source");
   begin
      Internal (Get_Object (Self), Targets);
   end Set_Drag_Source;

   -------------------
   -- Set_Exclusive --
   -------------------

   procedure Set_Exclusive
      (Self      : not null access Gtk_Tool_Palette_Record;
       Group     : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class;
       Exclusive : Boolean)
   is
      procedure Internal
         (Self      : System.Address;
          Group     : System.Address;
          Exclusive : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tool_palette_set_exclusive");
   begin
      Internal (Get_Object (Self), Get_Object (Group), Boolean'Pos (Exclusive));
   end Set_Exclusive;

   ----------------
   -- Set_Expand --
   ----------------

   procedure Set_Expand
      (Self   : not null access Gtk_Tool_Palette_Record;
       Group  : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class;
       Expand : Boolean)
   is
      procedure Internal
         (Self   : System.Address;
          Group  : System.Address;
          Expand : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_tool_palette_set_expand");
   begin
      Internal (Get_Object (Self), Get_Object (Group), Boolean'Pos (Expand));
   end Set_Expand;

   ------------------------
   -- Set_Group_Position --
   ------------------------

   procedure Set_Group_Position
      (Self     : not null access Gtk_Tool_Palette_Record;
       Group    : not null access Gtk.Tool_Item_Group.Gtk_Tool_Item_Group_Record'Class;
       Position : Glib.Gint)
   is
      procedure Internal
         (Self     : System.Address;
          Group    : System.Address;
          Position : Glib.Gint);
      pragma Import (C, Internal, "gtk_tool_palette_set_group_position");
   begin
      Internal (Get_Object (Self), Get_Object (Group), Position);
   end Set_Group_Position;

   -------------------
   -- Set_Icon_Size --
   -------------------

   procedure Set_Icon_Size
      (Self      : not null access Gtk_Tool_Palette_Record;
       Icon_Size : Gtk.Enums.Gtk_Icon_Size)
   is
      procedure Internal
         (Self      : System.Address;
          Icon_Size : Gtk.Enums.Gtk_Icon_Size);
      pragma Import (C, Internal, "gtk_tool_palette_set_icon_size");
   begin
      Internal (Get_Object (Self), Icon_Size);
   end Set_Icon_Size;

   ---------------
   -- Set_Style --
   ---------------

   procedure Set_Style
      (Self  : not null access Gtk_Tool_Palette_Record;
       Style : Gtk.Enums.Gtk_Toolbar_Style)
   is
      procedure Internal
         (Self  : System.Address;
          Style : Gtk.Enums.Gtk_Toolbar_Style);
      pragma Import (C, Internal, "gtk_tool_palette_set_style");
   begin
      Internal (Get_Object (Self), Style);
   end Set_Style;

   ---------------------
   -- Unset_Icon_Size --
   ---------------------

   procedure Unset_Icon_Size
      (Self : not null access Gtk_Tool_Palette_Record)
   is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tool_palette_unset_icon_size");
   begin
      Internal (Get_Object (Self));
   end Unset_Icon_Size;

   -----------------
   -- Unset_Style --
   -----------------

   procedure Unset_Style (Self : not null access Gtk_Tool_Palette_Record) is
      procedure Internal (Self : System.Address);
      pragma Import (C, Internal, "gtk_tool_palette_unset_style");
   begin
      Internal (Get_Object (Self));
   end Unset_Style;

   ----------------
   -- Get_Border --
   ----------------

   function Get_Border
      (Self   : not null access Gtk_Tool_Palette_Record;
       Border : access Gtk.Style.Gtk_Border) return Boolean
   is
      function Internal
         (Self       : System.Address;
          Acc_Border : access Gtk.Style.Gtk_Border) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_scrollable_get_border");
      Acc_Border     : aliased Gtk.Style.Gtk_Border;
      Tmp_Acc_Border : aliased Gtk.Style.Gtk_Border;
      Tmp_Return     : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Acc_Border'Access);
      Acc_Border := Tmp_Acc_Border;
      Border.all := Acc_Border;
      return Tmp_Return /= 0;
   end Get_Border;

   ---------------------
   -- Get_Hadjustment --
   ---------------------

   function Get_Hadjustment
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_hadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Hadjustment;

   ------------------------
   -- Get_Hscroll_Policy --
   ------------------------

   function Get_Hscroll_Policy
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_hscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Hscroll_Policy;

   ---------------------
   -- Get_Orientation --
   ---------------------

   function Get_Orientation
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Orientation
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Orientation;
      pragma Import (C, Internal, "gtk_orientable_get_orientation");
   begin
      return Internal (Get_Object (Self));
   end Get_Orientation;

   ---------------------
   -- Get_Vadjustment --
   ---------------------

   function Get_Vadjustment
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Adjustment.Gtk_Adjustment
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_scrollable_get_vadjustment");
      Stub_Gtk_Adjustment : Gtk.Adjustment.Gtk_Adjustment_Record;
   begin
      return Gtk.Adjustment.Gtk_Adjustment (Get_User_Data (Internal (Get_Object (Self)), Stub_Gtk_Adjustment));
   end Get_Vadjustment;

   ------------------------
   -- Get_Vscroll_Policy --
   ------------------------

   function Get_Vscroll_Policy
      (Self : not null access Gtk_Tool_Palette_Record)
       return Gtk.Enums.Gtk_Scrollable_Policy
   is
      function Internal
         (Self : System.Address) return Gtk.Enums.Gtk_Scrollable_Policy;
      pragma Import (C, Internal, "gtk_scrollable_get_vscroll_policy");
   begin
      return Internal (Get_Object (Self));
   end Get_Vscroll_Policy;

   ---------------------
   -- Set_Hadjustment --
   ---------------------

   procedure Set_Hadjustment
      (Self        : not null access Gtk_Tool_Palette_Record;
       Hadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Hadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_hadjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Hadjustment)));
   end Set_Hadjustment;

   ------------------------
   -- Set_Hscroll_Policy --
   ------------------------

   procedure Set_Hscroll_Policy
      (Self   : not null access Gtk_Tool_Palette_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk.Enums.Gtk_Scrollable_Policy);
      pragma Import (C, Internal, "gtk_scrollable_set_hscroll_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Hscroll_Policy;

   ---------------------
   -- Set_Orientation --
   ---------------------

   procedure Set_Orientation
      (Self        : not null access Gtk_Tool_Palette_Record;
       Orientation : Gtk.Enums.Gtk_Orientation)
   is
      procedure Internal
         (Self        : System.Address;
          Orientation : Gtk.Enums.Gtk_Orientation);
      pragma Import (C, Internal, "gtk_orientable_set_orientation");
   begin
      Internal (Get_Object (Self), Orientation);
   end Set_Orientation;

   ---------------------
   -- Set_Vadjustment --
   ---------------------

   procedure Set_Vadjustment
      (Self        : not null access Gtk_Tool_Palette_Record;
       Vadjustment : access Gtk.Adjustment.Gtk_Adjustment_Record'Class)
   is
      procedure Internal
         (Self        : System.Address;
          Vadjustment : System.Address);
      pragma Import (C, Internal, "gtk_scrollable_set_vadjustment");
   begin
      Internal (Get_Object (Self), Get_Object_Or_Null (GObject (Vadjustment)));
   end Set_Vadjustment;

   ------------------------
   -- Set_Vscroll_Policy --
   ------------------------

   procedure Set_Vscroll_Policy
      (Self   : not null access Gtk_Tool_Palette_Record;
       Policy : Gtk.Enums.Gtk_Scrollable_Policy)
   is
      procedure Internal
         (Self   : System.Address;
          Policy : Gtk.Enums.Gtk_Scrollable_Policy);
      pragma Import (C, Internal, "gtk_scrollable_set_vscroll_policy");
   begin
      Internal (Get_Object (Self), Policy);
   end Set_Vscroll_Policy;

   ---------------------------
   -- Get_Drag_Target_Group --
   ---------------------------

   function Get_Drag_Target_Group return Gtk.Target_Entry.Gtk_Target_Entry is
      function Internal return access Gtk.Target_Entry.Gtk_Target_Entry;
      pragma Import (C, Internal, "gtk_tool_palette_get_drag_target_group");
   begin
      return Internal.all;
   end Get_Drag_Target_Group;

   --------------------------
   -- Get_Drag_Target_Item --
   --------------------------

   function Get_Drag_Target_Item return Gtk.Target_Entry.Gtk_Target_Entry is
      function Internal return access Gtk.Target_Entry.Gtk_Target_Entry;
      pragma Import (C, Internal, "gtk_tool_palette_get_drag_target_item");
   begin
      return Internal.all;
   end Get_Drag_Target_Item;

end Gtk.Tool_Palette;
