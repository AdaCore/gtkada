------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with Interfaces.C.Strings;       use Interfaces.C.Strings;
pragma Warnings(On);

package body Gtk.Search_Entry is

   package Type_Conversion_Gtk_Search_Entry is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Search_Entry_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Search_Entry);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Search_Entry) is
   begin
      Self := new Gtk_Search_Entry_Record;
      Gtk.Search_Entry.Initialize (Self);
   end Gtk_New;

   --------------------------
   -- Gtk_Search_Entry_New --
   --------------------------

   function Gtk_Search_Entry_New return Gtk_Search_Entry is
      Self : constant Gtk_Search_Entry := new Gtk_Search_Entry_Record;
   begin
      Gtk.Search_Entry.Initialize (Self);
      return Self;
   end Gtk_Search_Entry_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self : not null access Gtk_Search_Entry_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_search_entry_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_copy_clipboard");
   begin
      Internal (Get_Object (Editable));
   end Copy_Clipboard;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_cut_clipboard");
   begin
      Internal (Get_Object (Editable));
   end Cut_Clipboard;

   ----------------------
   -- Delete_Selection --
   ----------------------

   procedure Delete_Selection
      (Editable : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_delete_selection");
   begin
      Internal (Get_Object (Editable));
   end Delete_Selection;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Gint;
          End_Pos   : Gint);
      pragma Import (C, Internal, "gtk_editable_delete_text");
   begin
      Internal (Get_Object (Editable), Start_Pos, End_Pos);
   end Delete_Text;

   ------------------
   -- Editing_Done --
   ------------------

   procedure Editing_Done
      (Cell_Editable : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_editing_done");
   begin
      Internal (Get_Object (Cell_Editable));
   end Editing_Done;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1) return UTF8_String
   is
      function Internal
         (Editable  : System.Address;
          Start_Pos : Gint;
          End_Pos   : Gint) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_editable_get_chars");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Editable), Start_Pos, End_Pos));
   end Get_Chars;

   ------------------
   -- Get_Editable --
   ------------------

   function Get_Editable
      (Editable : not null access Gtk_Search_Entry_Record) return Boolean
   is
      function Internal (Editable : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Get_Object (Editable)) /= 0;
   end Get_Editable;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Editable : not null access Gtk_Search_Entry_Record) return Gint
   is
      function Internal (Editable : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Editable));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Search_Entry_Record;
       Start_Pos     : out Gint;
       End_Pos       : out Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Editable      : System.Address;
          Acc_Start_Pos : access Gint;
          Acc_End_Pos   : access Gint) return Integer;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Gint;
      Acc_End_Pos   : aliased Gint;
      Tmp_Return    : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Editable), Acc_Start_Pos'Access, Acc_End_Pos'Access);
      Start_Pos := Acc_Start_Pos;
      End_Pos := Acc_End_Pos;
      Has_Selection := Tmp_Return /= 0;
   end Get_Selection_Bounds;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
      (Editable        : not null access Gtk_Search_Entry_Record;
       New_Text        : UTF8_String;
       New_Text_Length : Gint;
       Position        : in out Gint)
   is
      procedure Internal
         (Editable        : System.Address;
          New_Text        : Interfaces.C.Strings.chars_ptr;
          New_Text_Length : Gint;
          Position        : in out Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_New_Text : Interfaces.C.Strings.chars_ptr := New_String (New_Text);
   begin
      Internal (Get_Object (Editable), Tmp_New_Text, New_Text_Length, Position);
      Free (Tmp_New_Text);
   end Insert_Text;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
      (Editable : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Editable : System.Address);
      pragma Import (C, Internal, "gtk_editable_paste_clipboard");
   begin
      Internal (Get_Object (Editable));
   end Paste_Clipboard;

   -------------------
   -- Remove_Widget --
   -------------------

   procedure Remove_Widget
      (Cell_Editable : not null access Gtk_Search_Entry_Record)
   is
      procedure Internal (Cell_Editable : System.Address);
      pragma Import (C, Internal, "gtk_cell_editable_remove_widget");
   begin
      Internal (Get_Object (Cell_Editable));
   end Remove_Widget;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
      (Editable  : not null access Gtk_Search_Entry_Record;
       Start_Pos : Gint;
       End_Pos   : Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Gint;
          End_Pos   : Gint);
      pragma Import (C, Internal, "gtk_editable_select_region");
   begin
      Internal (Get_Object (Editable), Start_Pos, End_Pos);
   end Select_Region;

   ------------------
   -- Set_Editable --
   ------------------

   procedure Set_Editable
      (Editable    : not null access Gtk_Search_Entry_Record;
       Is_Editable : Boolean)
   is
      procedure Internal (Editable : System.Address; Is_Editable : Integer);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Editable), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Editable : not null access Gtk_Search_Entry_Record;
       Position : Gint)
   is
      procedure Internal (Editable : System.Address; Position : Gint);
      pragma Import (C, Internal, "gtk_editable_set_position");
   begin
      Internal (Get_Object (Editable), Position);
   end Set_Position;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing
      (Cell_Editable : not null access Gtk_Search_Entry_Record;
       Event         : Gdk.Event.Gdk_Event)
   is
      procedure Internal
         (Cell_Editable : System.Address;
          Event         : Gdk.Event.Gdk_Event);
      pragma Import (C, Internal, "gtk_cell_editable_start_editing");
   begin
      Internal (Get_Object (Cell_Editable), Event);
   end Start_Editing;

end Gtk.Search_Entry;
