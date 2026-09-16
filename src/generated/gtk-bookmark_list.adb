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
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Bookmark_List is

   package Type_Conversion_Gtk_Bookmark_List is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Bookmark_List_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Bookmark_List);

   ---------------------------
   -- Gtk_Bookmark_List_New --
   ---------------------------

   function Gtk_Bookmark_List_New
      (Filename   : UTF8_String := "";
       Attributes : UTF8_String := "") return Gtk_Bookmark_List
   is
      Self : constant Gtk_Bookmark_List := new Gtk_Bookmark_List_Record;
   begin
      Gtk.Bookmark_List.Initialize (Self, Filename, Attributes);
      return Self;
   end Gtk_Bookmark_List_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gtk_Bookmark_List;
       Filename   : UTF8_String := "";
       Attributes : UTF8_String := "")
   is
   begin
      Self := new Gtk_Bookmark_List_Record;
      Gtk.Bookmark_List.Initialize (Self, Filename, Attributes);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gtk_Bookmark_List_Record'Class;
       Filename   : UTF8_String := "";
       Attributes : UTF8_String := "")
   is
      function Internal
         (Filename   : Gtkada.Types.Chars_Ptr;
          Attributes : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_bookmark_list_new");
      Tmp_Filename   : Gtkada.Types.Chars_Ptr;
      Tmp_Attributes : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Filename :=
           (if Filename = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Filename));
         Tmp_Attributes :=
           (if Attributes = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Attributes));
         Tmp_Return := Internal (Tmp_Filename, Tmp_Attributes);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Attributes);
      Free (Tmp_Filename);
   end Initialize;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Self : not null access Gtk_Bookmark_List_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_bookmark_list_get_attributes");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Attributes;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
      (Self : not null access Gtk_Bookmark_List_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_bookmark_list_get_filename");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Filename;

   ---------------------
   -- Get_Io_Priority --
   ---------------------

   function Get_Io_Priority
      (Self : not null access Gtk_Bookmark_List_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_bookmark_list_get_io_priority");
   begin
      return Internal (Get_Object (Self));
   end Get_Io_Priority;

   ----------------
   -- Is_Loading --
   ----------------

   function Is_Loading
      (Self : not null access Gtk_Bookmark_List_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_bookmark_list_is_loading");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Loading;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Self       : not null access Gtk_Bookmark_List_Record;
       Attributes : UTF8_String := "")
   is
      procedure Internal
         (Self       : System.Address;
          Attributes : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_bookmark_list_set_attributes");
      Tmp_Attributes : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Attributes :=
        (if Attributes = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Attributes));
      Internal (Get_Object (Self), Tmp_Attributes);
      Free (Tmp_Attributes);
   end Set_Attributes;

   ---------------------
   -- Set_Io_Priority --
   ---------------------

   procedure Set_Io_Priority
      (Self        : not null access Gtk_Bookmark_List_Record;
       Io_Priority : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Io_Priority : Glib.Gint);
      pragma Import (C, Internal, "gtk_bookmark_list_set_io_priority");
   begin
      Internal (Get_Object (Self), Io_Priority);
   end Set_Io_Priority;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Bookmark_List_Record;
       Position : Guint) return Glib.Object.GObject
   is
      function Internal
         (Self     : System.Address;
          Position : Guint) return System.Address;
      pragma Import (C, Internal, "g_list_model_get_object");
      Stub_GObject : Glib.Object.GObject_Record;
   begin
      return Get_User_Data (Internal (Get_Object (Self), Position), Stub_GObject);
   end Get_Item;

   -------------------
   -- Get_Item_Type --
   -------------------

   function Get_Item_Type
      (Self : not null access Gtk_Bookmark_List_Record) return GType
   is
      function Internal (Self : System.Address) return GType;
      pragma Import (C, Internal, "g_list_model_get_item_type");
   begin
      return Internal (Get_Object (Self));
   end Get_Item_Type;

   -----------------
   -- Get_N_Items --
   -----------------

   function Get_N_Items
      (Self : not null access Gtk_Bookmark_List_Record) return Guint
   is
      function Internal (Self : System.Address) return Guint;
      pragma Import (C, Internal, "g_list_model_get_n_items");
   begin
      return Internal (Get_Object (Self));
   end Get_N_Items;

   -------------------
   -- Items_Changed --
   -------------------

   procedure Items_Changed
      (Self     : not null access Gtk_Bookmark_List_Record;
       Position : Guint;
       Removed  : Guint;
       Added    : Guint)
   is
      procedure Internal
         (Self     : System.Address;
          Position : Guint;
          Removed  : Guint;
          Added    : Guint);
      pragma Import (C, Internal, "g_list_model_items_changed");
   begin
      Internal (Get_Object (Self), Position, Removed, Added);
   end Items_Changed;

end Gtk.Bookmark_List;
