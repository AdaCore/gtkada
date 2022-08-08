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

   ------------------
   -- Handle_Event --
   ------------------

   function Handle_Event
      (Self  : not null access Gtk_Search_Entry_Record;
       Event : Gdk.Event.Gdk_Event) return Boolean
   is
      function Internal
         (Self  : System.Address;
          Event : Gdk.Event.Gdk_Event) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_search_entry_handle_event");
   begin
      return Internal (Get_Object (Self), Event) /= 0;
   end Handle_Event;

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
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
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
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1) return UTF8_String
   is
      function Internal
         (Editable  : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint) return Gtkada.Types.Chars_Ptr;
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
      function Internal (Editable : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_editable");
   begin
      return Internal (Get_Object (Editable)) /= 0;
   end Get_Editable;

   ------------------
   -- Get_Position --
   ------------------

   function Get_Position
      (Editable : not null access Gtk_Search_Entry_Record) return Glib.Gint
   is
      function Internal (Editable : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_editable_get_position");
   begin
      return Internal (Get_Object (Editable));
   end Get_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
      (Editable      : not null access Gtk_Search_Entry_Record;
       Start_Pos     : out Glib.Gint;
       End_Pos       : out Glib.Gint;
       Has_Selection : out Boolean)
   is
      function Internal
         (Editable      : System.Address;
          Acc_Start_Pos : access Glib.Gint;
          Acc_End_Pos   : access Glib.Gint) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_editable_get_selection_bounds");
      Acc_Start_Pos : aliased Glib.Gint;
      Acc_End_Pos   : aliased Glib.Gint;
      Tmp_Return    : Glib.Gboolean;
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
       New_Text_Length : Glib.Gint;
       Position        : in out Glib.Gint)
   is
      procedure Internal
         (Editable        : System.Address;
          New_Text        : Gtkada.Types.Chars_Ptr;
          New_Text_Length : Glib.Gint;
          Position        : in out Glib.Gint);
      pragma Import (C, Internal, "gtk_editable_insert_text");
      Tmp_New_Text : Gtkada.Types.Chars_Ptr := New_String (New_Text);
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
       Start_Pos : Glib.Gint;
       End_Pos   : Glib.Gint := -1)
   is
      procedure Internal
         (Editable  : System.Address;
          Start_Pos : Glib.Gint;
          End_Pos   : Glib.Gint);
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
      procedure Internal
         (Editable    : System.Address;
          Is_Editable : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_editable_set_editable");
   begin
      Internal (Get_Object (Editable), Boolean'Pos (Is_Editable));
   end Set_Editable;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
      (Editable : not null access Gtk_Search_Entry_Record;
       Position : Glib.Gint)
   is
      procedure Internal (Editable : System.Address; Position : Glib.Gint);
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

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Search_Entry_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Search_Entry_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Search_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Search_Entry_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Search_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Search_Entry_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Search_Entry_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Search_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Search_Entry_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Search_Entry_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Search_Entry_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------------
   -- Marsh_GObject_Void --
   ------------------------

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   ---------------------------------
   -- Marsh_Gtk_Search_Entry_Void --
   ---------------------------------

   procedure Marsh_Gtk_Search_Entry_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Search_Entry_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Search_Entry := Gtk_Search_Entry (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Search_Entry_Void;

   -------------------
   -- On_Next_Match --
   -------------------

   procedure On_Next_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "next-match" & ASCII.NUL, Call, After);
   end On_Next_Match;

   -------------------
   -- On_Next_Match --
   -------------------

   procedure On_Next_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "next-match" & ASCII.NUL, Call, After, Slot);
   end On_Next_Match;

   -----------------------
   -- On_Previous_Match --
   -----------------------

   procedure On_Previous_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "previous-match" & ASCII.NUL, Call, After);
   end On_Previous_Match;

   -----------------------
   -- On_Previous_Match --
   -----------------------

   procedure On_Previous_Match
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "previous-match" & ASCII.NUL, Call, After, Slot);
   end On_Previous_Match;

   -----------------------
   -- On_Search_Changed --
   -----------------------

   procedure On_Search_Changed
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "search-changed" & ASCII.NUL, Call, After);
   end On_Search_Changed;

   -----------------------
   -- On_Search_Changed --
   -----------------------

   procedure On_Search_Changed
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "search-changed" & ASCII.NUL, Call, After, Slot);
   end On_Search_Changed;

   --------------------
   -- On_Stop_Search --
   --------------------

   procedure On_Stop_Search
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_Gtk_Search_Entry_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "stop-search" & ASCII.NUL, Call, After);
   end On_Stop_Search;

   --------------------
   -- On_Stop_Search --
   --------------------

   procedure On_Stop_Search
      (Self  : not null access Gtk_Search_Entry_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "stop-search" & ASCII.NUL, Call, After, Slot);
   end On_Stop_Search;

end Gtk.Search_Entry;
