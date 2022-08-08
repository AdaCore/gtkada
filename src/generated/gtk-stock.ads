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


pragma Warnings (Off, "*is already use-visible*");
with Gdk.Types;    use Gdk.Types;
with Gtkada.Types; use Gtkada.Types;

package Gtk.Stock is

   Stock_About : constant UTF8_String := "gtk-about";
   Stock_Add : constant UTF8_String := "gtk-add";
   Stock_Apply : constant UTF8_String := "gtk-apply";
   Stock_Bold : constant UTF8_String := "gtk-bold";
   Stock_Cancel : constant UTF8_String := "gtk-cancel";
   Stock_Caps_Lock_Warning : constant UTF8_String := "gtk-caps-lock-warning";
   Stock_Cdrom : constant UTF8_String := "gtk-cdrom";
   Stock_Clear : constant UTF8_String := "gtk-clear";
   Stock_Close : constant UTF8_String := "gtk-close";
   Stock_Color_Picker : constant UTF8_String := "gtk-color-picker";
   Stock_Connect : constant UTF8_String := "gtk-connect";
   Stock_Convert : constant UTF8_String := "gtk-convert";
   Stock_Copy : constant UTF8_String := "gtk-copy";
   Stock_Cut : constant UTF8_String := "gtk-cut";
   Stock_Delete : constant UTF8_String := "gtk-delete";
   Stock_Dialog_Authentication : constant UTF8_String := "gtk-dialog-authentication";
   Stock_Dialog_Error : constant UTF8_String := "gtk-dialog-error";
   Stock_Dialog_Info : constant UTF8_String := "gtk-dialog-info";
   Stock_Dialog_Question : constant UTF8_String := "gtk-dialog-question";
   Stock_Dialog_Warning : constant UTF8_String := "gtk-dialog-warning";
   Stock_Directory : constant UTF8_String := "gtk-directory";
   Stock_Discard : constant UTF8_String := "gtk-discard";
   Stock_Disconnect : constant UTF8_String := "gtk-disconnect";
   Stock_Dnd : constant UTF8_String := "gtk-dnd";
   Stock_Dnd_Multiple : constant UTF8_String := "gtk-dnd-multiple";
   Stock_Edit : constant UTF8_String := "gtk-edit";
   Stock_Execute : constant UTF8_String := "gtk-execute";
   Stock_File : constant UTF8_String := "gtk-file";
   Stock_Find : constant UTF8_String := "gtk-find";
   Stock_Find_And_Replace : constant UTF8_String := "gtk-find-and-replace";
   Stock_Floppy : constant UTF8_String := "gtk-floppy";
   Stock_Fullscreen : constant UTF8_String := "gtk-fullscreen";
   Stock_Go_Back : constant UTF8_String := "gtk-go-back";
   Stock_Go_Down : constant UTF8_String := "gtk-go-down";
   Stock_Go_Forward : constant UTF8_String := "gtk-go-forward";
   Stock_Go_Up : constant UTF8_String := "gtk-go-up";
   Stock_Goto_Bottom : constant UTF8_String := "gtk-goto-bottom";
   Stock_Goto_First : constant UTF8_String := "gtk-goto-first";
   Stock_Goto_Last : constant UTF8_String := "gtk-goto-last";
   Stock_Goto_Top : constant UTF8_String := "gtk-goto-top";
   Stock_Harddisk : constant UTF8_String := "gtk-harddisk";
   Stock_Help : constant UTF8_String := "gtk-help";
   Stock_Home : constant UTF8_String := "gtk-home";
   Stock_Indent : constant UTF8_String := "gtk-indent";
   Stock_Index : constant UTF8_String := "gtk-index";
   Stock_Info : constant UTF8_String := "gtk-info";
   Stock_Italic : constant UTF8_String := "gtk-italic";
   Stock_Jump_To : constant UTF8_String := "gtk-jump-to";
   Stock_Justify_Center : constant UTF8_String := "gtk-justify-center";
   Stock_Justify_Fill : constant UTF8_String := "gtk-justify-fill";
   Stock_Justify_Left : constant UTF8_String := "gtk-justify-left";
   Stock_Justify_Right : constant UTF8_String := "gtk-justify-right";
   Stock_Leave_Fullscreen : constant UTF8_String := "gtk-leave-fullscreen";
   Stock_Media_Forward : constant UTF8_String := "gtk-media-forward";
   Stock_Media_Next : constant UTF8_String := "gtk-media-next";
   Stock_Media_Pause : constant UTF8_String := "gtk-media-pause";
   Stock_Media_Play : constant UTF8_String := "gtk-media-play";
   Stock_Media_Previous : constant UTF8_String := "gtk-media-previous";
   Stock_Media_Record : constant UTF8_String := "gtk-media-record";
   Stock_Media_Rewind : constant UTF8_String := "gtk-media-rewind";
   Stock_Media_Stop : constant UTF8_String := "gtk-media-stop";
   Stock_Missing_Image : constant UTF8_String := "gtk-missing-image";
   Stock_Network : constant UTF8_String := "gtk-network";
   Stock_New : constant UTF8_String := "gtk-new";
   Stock_No : constant UTF8_String := "gtk-no";
   Stock_Ok : constant UTF8_String := "gtk-ok";
   Stock_Open : constant UTF8_String := "gtk-open";
   Stock_Orientation_Landscape : constant UTF8_String := "gtk-orientation-landscape";
   Stock_Orientation_Portrait : constant UTF8_String := "gtk-orientation-portrait";
   Stock_Orientation_Reverse_Landscape : constant UTF8_String := "gtk-orientation-reverse-landscape";
   Stock_Orientation_Reverse_Portrait : constant UTF8_String := "gtk-orientation-reverse-portrait";
   Stock_Page_Setup : constant UTF8_String := "gtk-page-setup";
   Stock_Paste : constant UTF8_String := "gtk-paste";
   Stock_Preferences : constant UTF8_String := "gtk-preferences";
   Stock_Print : constant UTF8_String := "gtk-print";
   Stock_Print_Error : constant UTF8_String := "gtk-print-error";
   Stock_Print_Paused : constant UTF8_String := "gtk-print-paused";
   Stock_Print_Preview : constant UTF8_String := "gtk-print-preview";
   Stock_Print_Report : constant UTF8_String := "gtk-print-report";
   Stock_Print_Warning : constant UTF8_String := "gtk-print-warning";
   Stock_Properties : constant UTF8_String := "gtk-properties";
   Stock_Quit : constant UTF8_String := "gtk-quit";
   Stock_Redo : constant UTF8_String := "gtk-redo";
   Stock_Refresh : constant UTF8_String := "gtk-refresh";
   Stock_Remove : constant UTF8_String := "gtk-remove";
   Stock_Revert_To_Saved : constant UTF8_String := "gtk-revert-to-saved";
   Stock_Save : constant UTF8_String := "gtk-save";
   Stock_Save_As : constant UTF8_String := "gtk-save-as";
   Stock_Select_All : constant UTF8_String := "gtk-select-all";
   Stock_Select_Color : constant UTF8_String := "gtk-select-color";
   Stock_Select_Font : constant UTF8_String := "gtk-select-font";
   Stock_Sort_Ascending : constant UTF8_String := "gtk-sort-ascending";
   Stock_Sort_Descending : constant UTF8_String := "gtk-sort-descending";
   Stock_Spell_Check : constant UTF8_String := "gtk-spell-check";
   Stock_Stop : constant UTF8_String := "gtk-stop";
   Stock_Strikethrough : constant UTF8_String := "gtk-strikethrough";
   Stock_Undelete : constant UTF8_String := "gtk-undelete";
   Stock_Underline : constant UTF8_String := "gtk-underline";
   Stock_Undo : constant UTF8_String := "gtk-undo";
   Stock_Unindent : constant UTF8_String := "gtk-unindent";
   Stock_Yes : constant UTF8_String := "gtk-yes";
   Stock_Zoom_100 : constant UTF8_String := "gtk-zoom-100";
   Stock_Zoom_Fit : constant UTF8_String := "gtk-zoom-fit";
   Stock_Zoom_In : constant UTF8_String := "gtk-zoom-in";
   Stock_Zoom_Out : constant UTF8_String := "gtk-zoom-out";

   type Gtk_Stock_Item is private;
   function From_Object_Free (B : access Gtk_Stock_Item) return Gtk_Stock_Item;
   pragma Inline (From_Object_Free);


   type Gtk_Stock_Item_Array is array (Natural range <>) of Gtk_Stock_Item;

   -------------
   -- Methods --
   -------------

   function Copy (Self : Gtk_Stock_Item) return Gtk_Stock_Item;
   pragma Import (C, Copy, "gtk_stock_item_copy");
   pragma Obsolescent (Copy);
   --  Copies a stock item, mostly useful for language bindings and not in
   --  applications.
   --  Deprecated since 3.10, 1

   ----------------------
   -- GtkAda additions --
   ----------------------

   procedure Gtk_New
     (Item               : out Gtk_Stock_Item;
      Stock_Id           : String;
      Label              : UTF8_String;
      Modifier           : Gdk.Types.Gdk_Modifier_Type;
      Keyval             : Gdk.Types.Gdk_Key_Type;
      Translation_Domain : String);
   --  Create a new stock item.

   procedure Free (Item : in out Gtk_Stock_Item);
   --  Free memory allocated in Item.

   procedure Add (Item : Gtk_Stock_Item);
   --  Register Item.
   --  If an item already exists with the same stock ID as one of the items,
   --  the old item gets replaced. The stock item is copied, so GtkAda does
   --  not hold any pointer into item and item can be freed. Use
   --  Add_Static if item is persistent and GtkAda need not copy the array.

   ---------------
   -- Functions --
   ---------------

   procedure Add (Items : Gtk_Stock_Item_Array);
   pragma Obsolescent (Add);
   --  Registers each of the stock items in Items. If an item already exists
   --  with the same stock ID as one of the Items, the old item gets replaced.
   --  The stock items are copied, so GTK+ does not hold any pointer into Items
   --  and Items can be freed. Use Gtk.Stock.Add_Static if Items is persistent
   --  and GTK+ need not copy the array.
   --  Deprecated since 3.10, 1
   --  "items": a Gtk.Stock.Gtk_Stock_Item or array of items

   procedure Add_Static (Items : Gtk_Stock_Item_Array);
   pragma Obsolescent (Add_Static);
   --  Same as Gtk.Stock.Add, but doesn't copy Items, so Items must persist
   --  until application exit.
   --  Deprecated since 3.10, 1
   --  "items": a Gtk.Stock.Gtk_Stock_Item or array of
   --  Gtk.Stock.Gtk_Stock_Item

   procedure Lookup
      (Stock_Id : UTF8_String;
       Item     : out Gtk_Stock_Item;
       Success  : out Boolean);
   pragma Obsolescent (Lookup);
   --  Fills Item with the registered values for Stock_Id, returning True if
   --  Stock_Id was known.
   --  Deprecated since 3.10, 1
   --  "stock_id": a stock item name
   --  "item": stock item to initialize with values

private
type Gtk_Stock_Item is record
   Stock_Id : Gtkada.Types.Chars_Ptr;
   Label : Gtkada.Types.Chars_Ptr;
   Modifier : Gdk.Types.Gdk_Modifier_Type;
   Keyval : Guint;
   Translation_Domain : Gtkada.Types.Chars_Ptr;
end record;
pragma Convention (C, Gtk_Stock_Item);

end Gtk.Stock;
