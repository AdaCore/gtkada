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

package body Gtk.Directory_List is

   package Type_Conversion_Gtk_Directory_List is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Directory_List_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Directory_List);

   ----------------------------
   -- Gtk_Directory_List_New --
   ----------------------------

   function Gtk_Directory_List_New
      (Attributes : UTF8_String := "";
       File       : Glib.GFile.Gfile) return Gtk_Directory_List
   is
      Self : constant Gtk_Directory_List := new Gtk_Directory_List_Record;
   begin
      Gtk.Directory_List.Initialize (Self, Attributes, File);
      return Self;
   end Gtk_Directory_List_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
      (Self       : out Gtk_Directory_List;
       Attributes : UTF8_String := "";
       File       : Glib.GFile.Gfile)
   is
   begin
      Self := new Gtk_Directory_List_Record;
      Gtk.Directory_List.Initialize (Self, Attributes, File);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self       : not null access Gtk_Directory_List_Record'Class;
       Attributes : UTF8_String := "";
       File       : Glib.GFile.Gfile)
   is
      function Internal
         (Attributes : Gtkada.Types.Chars_Ptr;
          File       : Glib.GFile.Gfile) return System.Address;
      pragma Import (C, Internal, "gtk_directory_list_new");
      Tmp_Attributes : Gtkada.Types.Chars_Ptr;
      Tmp_Return     : System.Address;
   begin
      if not Self.Is_Created then
         Tmp_Attributes :=
           (if Attributes = ""
            then Gtkada.Types.Null_Ptr
            else New_String (Attributes));
         Tmp_Return := Internal (Tmp_Attributes, File);
         Set_Object (Self, Tmp_Return);
      end if;
      Free (Tmp_Attributes);
   end Initialize;

   --------------------
   -- Get_Attributes --
   --------------------

   function Get_Attributes
      (Self : not null access Gtk_Directory_List_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_directory_list_get_attributes");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Self)));
   end Get_Attributes;

   ---------------
   -- Get_Error --
   ---------------

   function Get_Error
      (Self : not null access Gtk_Directory_List_Record)
       return Glib.Error.GError
   is
      function Internal (Self : System.Address) return Glib.Error.GError;
      pragma Import (C, Internal, "gtk_directory_list_get_error");
   begin
      return Internal (Get_Object (Self));
   end Get_Error;

   --------------
   -- Get_File --
   --------------

   function Get_File
      (Self : not null access Gtk_Directory_List_Record)
       return Glib.GFile.Gfile
   is
      function Internal (Self : System.Address) return Glib.GFile.Gfile;
      pragma Import (C, Internal, "gtk_directory_list_get_file");
   begin
      return Internal (Get_Object (Self));
   end Get_File;

   ---------------------
   -- Get_Io_Priority --
   ---------------------

   function Get_Io_Priority
      (Self : not null access Gtk_Directory_List_Record) return Glib.Gint
   is
      function Internal (Self : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_directory_list_get_io_priority");
   begin
      return Internal (Get_Object (Self));
   end Get_Io_Priority;

   -------------------
   -- Get_Monitored --
   -------------------

   function Get_Monitored
      (Self : not null access Gtk_Directory_List_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_directory_list_get_monitored");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Get_Monitored;

   ----------------
   -- Is_Loading --
   ----------------

   function Is_Loading
      (Self : not null access Gtk_Directory_List_Record) return Boolean
   is
      function Internal (Self : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_directory_list_is_loading");
   begin
      return Internal (Get_Object (Self)) /= 0;
   end Is_Loading;

   --------------------
   -- Set_Attributes --
   --------------------

   procedure Set_Attributes
      (Self       : not null access Gtk_Directory_List_Record;
       Attributes : UTF8_String := "")
   is
      procedure Internal
         (Self       : System.Address;
          Attributes : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_directory_list_set_attributes");
      Tmp_Attributes : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Attributes :=
        (if Attributes = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Attributes));
      Internal (Get_Object (Self), Tmp_Attributes);
      Free (Tmp_Attributes);
   end Set_Attributes;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
      (Self : not null access Gtk_Directory_List_Record;
       File : Glib.GFile.Gfile)
   is
      procedure Internal (Self : System.Address; File : Glib.GFile.Gfile);
      pragma Import (C, Internal, "gtk_directory_list_set_file");
   begin
      Internal (Get_Object (Self), File);
   end Set_File;

   ---------------------
   -- Set_Io_Priority --
   ---------------------

   procedure Set_Io_Priority
      (Self        : not null access Gtk_Directory_List_Record;
       Io_Priority : Glib.Gint)
   is
      procedure Internal (Self : System.Address; Io_Priority : Glib.Gint);
      pragma Import (C, Internal, "gtk_directory_list_set_io_priority");
   begin
      Internal (Get_Object (Self), Io_Priority);
   end Set_Io_Priority;

   -------------------
   -- Set_Monitored --
   -------------------

   procedure Set_Monitored
      (Self      : not null access Gtk_Directory_List_Record;
       Monitored : Boolean)
   is
      procedure Internal (Self : System.Address; Monitored : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_directory_list_set_monitored");
   begin
      Internal (Get_Object (Self), Boolean'Pos (Monitored));
   end Set_Monitored;

   --------------
   -- Get_Item --
   --------------

   function Get_Item
      (Self     : not null access Gtk_Directory_List_Record;
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
      (Self : not null access Gtk_Directory_List_Record) return GType
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
      (Self : not null access Gtk_Directory_List_Record) return Guint
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
      (Self     : not null access Gtk_Directory_List_Record;
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

end Gtk.Directory_List;
