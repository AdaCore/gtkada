-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
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

--  <description>
--  These functions provide an applications programmer with default
--  images and buttons for toolbars, menu pixmaps, etc.
--  </description>

--  <c_version>1.3.6</c_version>

with Gdk.Types;
with Gtkada.Types;

package Gtk.Stock is

   type Gtk_Stock_Item is record
      Stock_Id           : Gtkada.Types.Chars_Ptr;
      Label              : Gtkada.Types.Chars_Ptr;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : Gtkada.Types.Chars_Ptr;
   end record;
   pragma Convention (C, Gtk_Stock_Item);

   type Gtk_Stock_Item_Access is access all Gtk_Stock_Item;
   pragma Convention (C, Gtk_Stock_Item_Access);

   type Gtk_Stock_Item_Array is array (Natural range <>) of Gtk_Stock_Item;

   procedure Gtk_New
     (Item               : out Gtk_Stock_Item;
      Stock_Id           : String;
      Label              : String;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : String);
   --  Create a new stock item.

   procedure Add (Item : Gtk_Stock_Item);
   --  Register Item.
   --  If an item already exists with the same stock ID as one of the items,
   --  the old item gets replaced. The stock item is copied, so GtkAda does
   --  not hold any pointer into item and item can be freed. Use
   --  Add_Static if item is persistent and GtkAda need not copy the array.

   procedure Add (Items : Gtk_Stock_Item_Array);
   --  Register each of the stock items in Items.

   procedure Add_Static (Item : Gtk_Stock_Item);
   --  Same as Add, but do not copy Item, so Item must persist until
   --  application exit.

   procedure Add_Static (Items : Gtk_Stock_Item_Array);
   --  Same as Add, but do not copy Items, so Items must persist until
   --  application exit.

   procedure Lookup
     (Stock_Id : String;
      Item     : out Gtk_Stock_Item;
      Success  : out Boolean);
   --  Fill Item with the registered values for Stock_Id.
   --  Success if set to True of Stock_Id was known.

   --  function List_Ids return GSList*;
   --  Should free the list (and free each string in it also).
   --  This function is only useful for GUI builders and such.

   procedure Free (Item : in out Gtk_Stock_Item);
   --  Free memory allocated in Item.

   --  Stock IDs (not all are stock items; some are images only)

   Stock_Dialog_Info      : constant String := "gtk-dialog-info";
   Stock_Dialog_Warning   : constant String := "gtk-dialog-warning";
   Stock_Dialog_Error     : constant String := "gtk-dialog-error";
   Stock_Dialog_Question  : constant String := "gtk-dialog-question";

   Stock_Add              : constant String := "gtk-add";
   Stock_Apply            : constant String := "gtk-apply";
   Stock_Bold             : constant String := "gtk-bold";
   Stock_Cancel           : constant String := "gtk-cancel";
   Stock_Cdrom            : constant String := "gtk-cdrom";
   Stock_Clear            : constant String := "gtk-clear";
   Stock_Close            : constant String := "gtk-close";
   Stock_Convert          : constant String := "gtk-convert";
   Stock_Copy             : constant String := "gtk-copy";
   Stock_Cut              : constant String := "gtk-cut";
   Stock_Delete           : constant String := "gtk-delete";
   Stock_Execute          : constant String := "gtk-execute";
   Stock_Find             : constant String := "gtk-find";
   Stock_Find_And_Replace : constant String := "gtk-find-and-replace";
   Stock_Floppy           : constant String := "gtk-floppy";
   Stock_Goto_Bottom      : constant String := "gtk-goto-bottom";
   Stock_Goto_First       : constant String := "gtk-goto-first";
   Stock_Goto_Last        : constant String := "gtk-goto-last";
   Stock_Goto_Top         : constant String := "gtk-goto-top";
   Stock_Go_Back          : constant String := "gtk-go-back";
   Stock_Go_Down          : constant String := "gtk-go-down";
   Stock_Go_Forward       : constant String := "gtk-go-forward";
   Stock_Go_Up            : constant String := "gtk-go-up";
   Stock_Help             : constant String := "gtk-help";
   Stock_Home             : constant String := "gtk-home";
   Stock_Index            : constant String := "gtk-index";
   Stock_Italic           : constant String := "gtk-italic";
   Stock_Jump_To          : constant String := "gtk-jump-to";
   Stock_Justify_Center   : constant String := "gtk-justify-center";
   Stock_Justify_Fill     : constant String := "gtk-justify-fill";
   Stock_Justify_Left     : constant String := "gtk-justify-left";
   Stock_Justify_Right    : constant String := "gtk-justify-right";
   Stock_Missing_Image    : constant String := "gtk-missing-image";
   Stock_New              : constant String := "gtk-new";
   Stock_No               : constant String := "gtk-no";
   Stock_Ok               : constant String := "gtk-ok";
   Stock_Open             : constant String := "gtk-open";
   Stock_Paste            : constant String := "gtk-paste";
   Stock_Preferences      : constant String := "gtk-preferences";
   Stock_Print            : constant String := "gtk-print";
   Stock_Print_Preview    : constant String := "gtk-print-preview";
   Stock_Properties       : constant String := "gtk-properties";
   Stock_Quit             : constant String := "gtk-quit";
   Stock_Redo             : constant String := "gtk-redo";
   Stock_Refresh          : constant String := "gtk-refresh";
   Stock_Remove           : constant String := "gtk-remove";
   Stock_Revert_To_Saved  : constant String := "gtk-revert-to-saved";
   Stock_Save             : constant String := "gtk-save";
   Stock_Save_As          : constant String := "gtk-save-as";
   Stock_Select_Color     : constant String := "gtk-select-color";
   Stock_Select_Font      : constant String := "gtk-select-font";
   Stock_Sort_Ascending   : constant String := "gtk-sort-ascending";
   Stock_Sort_Descending  : constant String := "gtk-sort-descending";
   Stock_Spell_Check      : constant String := "gtk-spell-check";
   Stock_Stop             : constant String := "gtk-stop";
   Stock_Strikethrough    : constant String := "gtk-strikethrough";
   Stock_Undelete         : constant String := "gtk-undelete";
   Stock_Underline        : constant String := "gtk-underline";
   Stock_Undo             : constant String := "gtk-undo";
   Stock_Yes              : constant String := "gtk-yes";
   Stock_Zoom_100         : constant String := "gtk-zoom-100";
   Stock_Zoom_Fit         : constant String := "gtk-zoom-fit";
   Stock_Zoom_In          : constant String := "gtk-zoom-in";
   Stock_Zoom_Out         : constant String := "gtk-zoom-out";

end Gtk.Stock;
