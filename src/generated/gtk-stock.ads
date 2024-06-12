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
   pragma Obsolescent (Stock_About, Message => "Deprecated since 3.10");

   Stock_Add : constant UTF8_String := "gtk-add";
   pragma Obsolescent (Stock_Add, Message => "Deprecated since 3.10");

   Stock_Apply : constant UTF8_String := "gtk-apply";
   pragma Obsolescent (Stock_Apply, Message => "Deprecated since 3.10");

   Stock_Bold : constant UTF8_String := "gtk-bold";
   pragma Obsolescent (Stock_Bold, Message => "Deprecated since 3.10");

   Stock_Cancel : constant UTF8_String := "gtk-cancel";
   pragma Obsolescent (Stock_Cancel, Message => "Deprecated since 3.10");

   Stock_Caps_Lock_Warning : constant UTF8_String := "gtk-caps-lock-warning";
   pragma Obsolescent (Stock_Caps_Lock_Warning, Message => "Deprecated since 3.10");

   Stock_Cdrom : constant UTF8_String := "gtk-cdrom";
   pragma Obsolescent (Stock_Cdrom, Message => "Deprecated since 3.10");

   Stock_Clear : constant UTF8_String := "gtk-clear";
   pragma Obsolescent (Stock_Clear, Message => "Deprecated since 3.10");

   Stock_Close : constant UTF8_String := "gtk-close";
   pragma Obsolescent (Stock_Close, Message => "Deprecated since 3.10");

   Stock_Color_Picker : constant UTF8_String := "gtk-color-picker";
   pragma Obsolescent (Stock_Color_Picker, Message => "Deprecated since 3.10");

   Stock_Connect : constant UTF8_String := "gtk-connect";
   pragma Obsolescent (Stock_Connect, Message => "Deprecated since 3.10");

   Stock_Convert : constant UTF8_String := "gtk-convert";
   pragma Obsolescent (Stock_Convert, Message => "Deprecated since 3.10");

   Stock_Copy : constant UTF8_String := "gtk-copy";
   pragma Obsolescent (Stock_Copy, Message => "Deprecated since 3.10");

   Stock_Cut : constant UTF8_String := "gtk-cut";
   pragma Obsolescent (Stock_Cut, Message => "Deprecated since 3.10");

   Stock_Delete : constant UTF8_String := "gtk-delete";
   pragma Obsolescent (Stock_Delete, Message => "Deprecated since 3.10");

   Stock_Dialog_Authentication : constant UTF8_String := "gtk-dialog-authentication";
   pragma Obsolescent (Stock_Dialog_Authentication, Message => "Deprecated since 3.10");

   Stock_Dialog_Error : constant UTF8_String := "gtk-dialog-error";
   pragma Obsolescent (Stock_Dialog_Error, Message => "Deprecated since 3.10");

   Stock_Dialog_Info : constant UTF8_String := "gtk-dialog-info";
   pragma Obsolescent (Stock_Dialog_Info, Message => "Deprecated since 3.10");

   Stock_Dialog_Question : constant UTF8_String := "gtk-dialog-question";
   pragma Obsolescent (Stock_Dialog_Question, Message => "Deprecated since 3.10");

   Stock_Dialog_Warning : constant UTF8_String := "gtk-dialog-warning";
   pragma Obsolescent (Stock_Dialog_Warning, Message => "Deprecated since 3.10");

   Stock_Directory : constant UTF8_String := "gtk-directory";
   pragma Obsolescent (Stock_Directory, Message => "Deprecated since 3.10");

   Stock_Discard : constant UTF8_String := "gtk-discard";
   pragma Obsolescent (Stock_Discard, Message => "Deprecated since 3.10");

   Stock_Disconnect : constant UTF8_String := "gtk-disconnect";
   pragma Obsolescent (Stock_Disconnect, Message => "Deprecated since 3.10");

   Stock_Dnd : constant UTF8_String := "gtk-dnd";
   pragma Obsolescent (Stock_Dnd, Message => "Deprecated since 3.10");

   Stock_Dnd_Multiple : constant UTF8_String := "gtk-dnd-multiple";
   pragma Obsolescent (Stock_Dnd_Multiple, Message => "Deprecated since 3.10");

   Stock_Edit : constant UTF8_String := "gtk-edit";
   pragma Obsolescent (Stock_Edit, Message => "Deprecated since 3.10");

   Stock_Execute : constant UTF8_String := "gtk-execute";
   pragma Obsolescent (Stock_Execute, Message => "Deprecated since 3.10");

   Stock_File : constant UTF8_String := "gtk-file";
   pragma Obsolescent (Stock_File, Message => "Deprecated since 3.10");

   Stock_Find : constant UTF8_String := "gtk-find";
   pragma Obsolescent (Stock_Find, Message => "Deprecated since 3.10");

   Stock_Find_And_Replace : constant UTF8_String := "gtk-find-and-replace";
   pragma Obsolescent (Stock_Find_And_Replace, Message => "Deprecated since 3.10");

   Stock_Floppy : constant UTF8_String := "gtk-floppy";
   pragma Obsolescent (Stock_Floppy, Message => "Deprecated since 3.10");

   Stock_Fullscreen : constant UTF8_String := "gtk-fullscreen";
   pragma Obsolescent (Stock_Fullscreen, Message => "Deprecated since 3.10");

   Stock_Go_Back : constant UTF8_String := "gtk-go-back";
   pragma Obsolescent (Stock_Go_Back, Message => "Deprecated since 3.10");

   Stock_Go_Down : constant UTF8_String := "gtk-go-down";
   pragma Obsolescent (Stock_Go_Down, Message => "Deprecated since 3.10");

   Stock_Go_Forward : constant UTF8_String := "gtk-go-forward";
   pragma Obsolescent (Stock_Go_Forward, Message => "Deprecated since 3.10");

   Stock_Go_Up : constant UTF8_String := "gtk-go-up";
   pragma Obsolescent (Stock_Go_Up, Message => "Deprecated since 3.10");

   Stock_Goto_Bottom : constant UTF8_String := "gtk-goto-bottom";
   pragma Obsolescent (Stock_Goto_Bottom, Message => "Deprecated since 3.10");

   Stock_Goto_First : constant UTF8_String := "gtk-goto-first";
   pragma Obsolescent (Stock_Goto_First, Message => "Deprecated since 3.10");

   Stock_Goto_Last : constant UTF8_String := "gtk-goto-last";
   pragma Obsolescent (Stock_Goto_Last, Message => "Deprecated since 3.10");

   Stock_Goto_Top : constant UTF8_String := "gtk-goto-top";
   pragma Obsolescent (Stock_Goto_Top, Message => "Deprecated since 3.10");

   Stock_Harddisk : constant UTF8_String := "gtk-harddisk";
   pragma Obsolescent (Stock_Harddisk, Message => "Deprecated since 3.10");

   Stock_Help : constant UTF8_String := "gtk-help";
   pragma Obsolescent (Stock_Help, Message => "Deprecated since 3.10");

   Stock_Home : constant UTF8_String := "gtk-home";
   pragma Obsolescent (Stock_Home, Message => "Deprecated since 3.10");

   Stock_Indent : constant UTF8_String := "gtk-indent";
   pragma Obsolescent (Stock_Indent, Message => "Deprecated since 3.10");

   Stock_Index : constant UTF8_String := "gtk-index";
   pragma Obsolescent (Stock_Index, Message => "Deprecated since 3.10");

   Stock_Info : constant UTF8_String := "gtk-info";
   pragma Obsolescent (Stock_Info, Message => "Deprecated since 3.10");

   Stock_Italic : constant UTF8_String := "gtk-italic";
   pragma Obsolescent (Stock_Italic, Message => "Deprecated since 3.10");

   Stock_Jump_To : constant UTF8_String := "gtk-jump-to";
   pragma Obsolescent (Stock_Jump_To, Message => "Deprecated since 3.10");

   Stock_Justify_Center : constant UTF8_String := "gtk-justify-center";
   pragma Obsolescent (Stock_Justify_Center, Message => "Deprecated since 3.10");

   Stock_Justify_Fill : constant UTF8_String := "gtk-justify-fill";
   pragma Obsolescent (Stock_Justify_Fill, Message => "Deprecated since 3.10");

   Stock_Justify_Left : constant UTF8_String := "gtk-justify-left";
   pragma Obsolescent (Stock_Justify_Left, Message => "Deprecated since 3.10");

   Stock_Justify_Right : constant UTF8_String := "gtk-justify-right";
   pragma Obsolescent (Stock_Justify_Right, Message => "Deprecated since 3.10");

   Stock_Leave_Fullscreen : constant UTF8_String := "gtk-leave-fullscreen";
   pragma Obsolescent (Stock_Leave_Fullscreen, Message => "Deprecated since 3.10");

   Stock_Media_Forward : constant UTF8_String := "gtk-media-forward";
   pragma Obsolescent (Stock_Media_Forward, Message => "Deprecated since 3.10");

   Stock_Media_Next : constant UTF8_String := "gtk-media-next";
   pragma Obsolescent (Stock_Media_Next, Message => "Deprecated since 3.10");

   Stock_Media_Pause : constant UTF8_String := "gtk-media-pause";
   pragma Obsolescent (Stock_Media_Pause, Message => "Deprecated since 3.10");

   Stock_Media_Play : constant UTF8_String := "gtk-media-play";
   pragma Obsolescent (Stock_Media_Play, Message => "Deprecated since 3.10");

   Stock_Media_Previous : constant UTF8_String := "gtk-media-previous";
   pragma Obsolescent (Stock_Media_Previous, Message => "Deprecated since 3.10");

   Stock_Media_Record : constant UTF8_String := "gtk-media-record";
   pragma Obsolescent (Stock_Media_Record, Message => "Deprecated since 3.10");

   Stock_Media_Rewind : constant UTF8_String := "gtk-media-rewind";
   pragma Obsolescent (Stock_Media_Rewind, Message => "Deprecated since 3.10");

   Stock_Media_Stop : constant UTF8_String := "gtk-media-stop";
   pragma Obsolescent (Stock_Media_Stop, Message => "Deprecated since 3.10");

   Stock_Missing_Image : constant UTF8_String := "gtk-missing-image";
   pragma Obsolescent (Stock_Missing_Image, Message => "Deprecated since 3.10");

   Stock_Network : constant UTF8_String := "gtk-network";
   pragma Obsolescent (Stock_Network, Message => "Deprecated since 3.10");

   Stock_New : constant UTF8_String := "gtk-new";
   pragma Obsolescent (Stock_New, Message => "Deprecated since 3.10");

   Stock_No : constant UTF8_String := "gtk-no";
   pragma Obsolescent (Stock_No, Message => "Deprecated since 3.10");

   Stock_Ok : constant UTF8_String := "gtk-ok";
   pragma Obsolescent (Stock_Ok, Message => "Deprecated since 3.10");

   Stock_Open : constant UTF8_String := "gtk-open";
   pragma Obsolescent (Stock_Open, Message => "Deprecated since 3.10");

   Stock_Orientation_Landscape : constant UTF8_String := "gtk-orientation-landscape";
   pragma Obsolescent (Stock_Orientation_Landscape, Message => "Deprecated since 3.10");

   Stock_Orientation_Portrait : constant UTF8_String := "gtk-orientation-portrait";
   pragma Obsolescent (Stock_Orientation_Portrait, Message => "Deprecated since 3.10");

   Stock_Orientation_Reverse_Landscape : constant UTF8_String := "gtk-orientation-reverse-landscape";
   pragma Obsolescent (Stock_Orientation_Reverse_Landscape, Message => "Deprecated since 3.10");

   Stock_Orientation_Reverse_Portrait : constant UTF8_String := "gtk-orientation-reverse-portrait";
   pragma Obsolescent (Stock_Orientation_Reverse_Portrait, Message => "Deprecated since 3.10");

   Stock_Page_Setup : constant UTF8_String := "gtk-page-setup";
   pragma Obsolescent (Stock_Page_Setup, Message => "Deprecated since 3.10");

   Stock_Paste : constant UTF8_String := "gtk-paste";
   pragma Obsolescent (Stock_Paste, Message => "Deprecated since 3.10");

   Stock_Preferences : constant UTF8_String := "gtk-preferences";
   pragma Obsolescent (Stock_Preferences, Message => "Deprecated since 3.10");

   Stock_Print : constant UTF8_String := "gtk-print";
   pragma Obsolescent (Stock_Print, Message => "Deprecated since 3.10");

   Stock_Print_Error : constant UTF8_String := "gtk-print-error";
   pragma Obsolescent (Stock_Print_Error, Message => "Deprecated since 3.10");

   Stock_Print_Paused : constant UTF8_String := "gtk-print-paused";
   pragma Obsolescent (Stock_Print_Paused, Message => "Deprecated since 3.10");

   Stock_Print_Preview : constant UTF8_String := "gtk-print-preview";
   pragma Obsolescent (Stock_Print_Preview, Message => "Deprecated since 3.10");

   Stock_Print_Report : constant UTF8_String := "gtk-print-report";
   pragma Obsolescent (Stock_Print_Report, Message => "Deprecated since 3.10");

   Stock_Print_Warning : constant UTF8_String := "gtk-print-warning";
   pragma Obsolescent (Stock_Print_Warning, Message => "Deprecated since 3.10");

   Stock_Properties : constant UTF8_String := "gtk-properties";
   pragma Obsolescent (Stock_Properties, Message => "Deprecated since 3.10");

   Stock_Quit : constant UTF8_String := "gtk-quit";
   pragma Obsolescent (Stock_Quit, Message => "Deprecated since 3.10");

   Stock_Redo : constant UTF8_String := "gtk-redo";
   pragma Obsolescent (Stock_Redo, Message => "Deprecated since 3.10");

   Stock_Refresh : constant UTF8_String := "gtk-refresh";
   pragma Obsolescent (Stock_Refresh, Message => "Deprecated since 3.10");

   Stock_Remove : constant UTF8_String := "gtk-remove";
   pragma Obsolescent (Stock_Remove, Message => "Deprecated since 3.10");

   Stock_Revert_To_Saved : constant UTF8_String := "gtk-revert-to-saved";
   pragma Obsolescent (Stock_Revert_To_Saved, Message => "Deprecated since 3.10");

   Stock_Save : constant UTF8_String := "gtk-save";
   pragma Obsolescent (Stock_Save, Message => "Deprecated since 3.10");

   Stock_Save_As : constant UTF8_String := "gtk-save-as";
   pragma Obsolescent (Stock_Save_As, Message => "Deprecated since 3.10");

   Stock_Select_All : constant UTF8_String := "gtk-select-all";
   pragma Obsolescent (Stock_Select_All, Message => "Deprecated since 3.10");

   Stock_Select_Color : constant UTF8_String := "gtk-select-color";
   pragma Obsolescent (Stock_Select_Color, Message => "Deprecated since 3.10");

   Stock_Select_Font : constant UTF8_String := "gtk-select-font";
   pragma Obsolescent (Stock_Select_Font, Message => "Deprecated since 3.10");

   Stock_Sort_Ascending : constant UTF8_String := "gtk-sort-ascending";
   pragma Obsolescent (Stock_Sort_Ascending, Message => "Deprecated since 3.10");

   Stock_Sort_Descending : constant UTF8_String := "gtk-sort-descending";
   pragma Obsolescent (Stock_Sort_Descending, Message => "Deprecated since 3.10");

   Stock_Spell_Check : constant UTF8_String := "gtk-spell-check";
   pragma Obsolescent (Stock_Spell_Check, Message => "Deprecated since 3.10");

   Stock_Stop : constant UTF8_String := "gtk-stop";
   pragma Obsolescent (Stock_Stop, Message => "Deprecated since 3.10");

   Stock_Strikethrough : constant UTF8_String := "gtk-strikethrough";
   pragma Obsolescent (Stock_Strikethrough, Message => "Deprecated since 3.10");

   Stock_Undelete : constant UTF8_String := "gtk-undelete";
   pragma Obsolescent (Stock_Undelete, Message => "Deprecated since 3.10");

   Stock_Underline : constant UTF8_String := "gtk-underline";
   pragma Obsolescent (Stock_Underline, Message => "Deprecated since 3.10");

   Stock_Undo : constant UTF8_String := "gtk-undo";
   pragma Obsolescent (Stock_Undo, Message => "Deprecated since 3.10");

   Stock_Unindent : constant UTF8_String := "gtk-unindent";
   pragma Obsolescent (Stock_Unindent, Message => "Deprecated since 3.10");

   Stock_Yes : constant UTF8_String := "gtk-yes";
   pragma Obsolescent (Stock_Yes, Message => "Deprecated since 3.10");

   Stock_Zoom_100 : constant UTF8_String := "gtk-zoom-100";
   pragma Obsolescent (Stock_Zoom_100, Message => "Deprecated since 3.10");

   Stock_Zoom_Fit : constant UTF8_String := "gtk-zoom-fit";
   pragma Obsolescent (Stock_Zoom_Fit, Message => "Deprecated since 3.10");

   Stock_Zoom_In : constant UTF8_String := "gtk-zoom-in";
   pragma Obsolescent (Stock_Zoom_In, Message => "Deprecated since 3.10");

   Stock_Zoom_Out : constant UTF8_String := "gtk-zoom-out";
   pragma Obsolescent (Stock_Zoom_Out, Message => "Deprecated since 3.10");

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
