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
with Ada.Unchecked_Conversion;
with Glib.Type_Conversion_Hooks; use Glib.Type_Conversion_Hooks;
with Glib.Values;                use Glib.Values;
with Gtk.Arguments;              use Gtk.Arguments;
with Interfaces.C.Pointers;
pragma Warnings(Off);  --  might be unused
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.Types;               use Gtkada.Types;
pragma Warnings(On);

package body Gtk.Icon_Theme is

   package Type_Conversion_Gtk_Icon_Theme is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Theme_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_Theme);

   ------------------------
   -- Gtk_Icon_Theme_New --
   ------------------------

   function Gtk_Icon_Theme_New return Gtk_Icon_Theme is
      Self : constant Gtk_Icon_Theme := new Gtk_Icon_Theme_Record;
   begin
      Gtk.Icon_Theme.Initialize (Self);
      return Self;
   end Gtk_Icon_Theme_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Self : out Gtk_Icon_Theme) is
   begin
      Self := new Gtk_Icon_Theme_Record;
      Gtk.Icon_Theme.Initialize (Self);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Gtk_Icon_Theme_Record'Class) is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_new");
   begin
      if not Self.Is_Created then
         Set_Object (Self, Internal);
      end if;
   end Initialize;

   -----------------------
   -- Add_Resource_Path --
   -----------------------

   procedure Add_Resource_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_add_resource_path");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
   end Add_Resource_Path;

   ---------------------
   -- Add_Search_Path --
   ---------------------

   procedure Add_Search_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : UTF8_String)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_add_search_path");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path);
      Free (Tmp_Path);
   end Add_Search_Path;

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
      (Self : not null access Gtk_Icon_Theme_Record) return Gdk.Gdk_Display
   is
      function Internal (Self : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_get_display");
      Stub_Gdk_Display : Gdk.Display.Gdk_Display_Record;
   begin
      return Gdk.Gdk_Display (Get_User_Data (Internal (Get_Object (Self)), Stub_Gdk_Display));
   end Get_Display;

   --------------------
   -- Get_Icon_Names --
   --------------------

   function Get_Icon_Names
      (Self : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_icon_theme_get_icon_names");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end Get_Icon_Names;

   --------------------
   -- Get_Icon_Sizes --
   --------------------

   function Get_Icon_Sizes
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon_Name : UTF8_String) return Gint_Array
   is
      use Interfaces.C;
      type Aliased_Gint_Array is array (Natural range <>) of aliased Gint;
      pragma Convention (C, Aliased_Gint_Array);
      package P is new Interfaces.C.Pointers
        (Natural, Gint, Aliased_Gint_Array, 0);
      use P;
      
      function Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr) return P.Pointer;
      pragma Import (C, Internal, "gtk_icon_theme_get_icon_sizes");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Tmp_Return    : P.Pointer;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
      declare
         Arr : constant Gint_Array := Gint_Array (P.Value (Tmp_Return));
      begin
         Glib.g_free (Tmp_Return.all'Address);
         return Arr;
      end;
   end Get_Icon_Sizes;

   -----------------------
   -- Get_Resource_Path --
   -----------------------

   function Get_Resource_Path
      (Self : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_icon_theme_get_resource_path");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end Get_Resource_Path;

   ---------------------
   -- Get_Search_Path --
   ---------------------

   function Get_Search_Path
      (Self : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List
   is
      function Internal
         (Self : System.Address) return chars_ptr_array_access;
      pragma Import (C, Internal, "gtk_icon_theme_get_search_path");
   begin
      return To_String_List_And_Free (Internal (Get_Object (Self)));
   end Get_Search_Path;

   --------------------
   -- Get_Theme_Name --
   --------------------

   function Get_Theme_Name
      (Self : not null access Gtk_Icon_Theme_Record) return UTF8_String
   is
      function Internal
         (Self : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_theme_get_theme_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Self)));
   end Get_Theme_Name;

   ---------------
   -- Has_Gicon --
   ---------------

   function Has_Gicon
      (Self   : not null access Gtk_Icon_Theme_Record;
       G_Icon : Glib.G_Icon.G_Icon) return Boolean
   is
      function Internal
         (Self   : System.Address;
          G_Icon : Glib.G_Icon.G_Icon) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_theme_has_gicon");
   begin
      return Internal (Get_Object (Self), G_Icon) /= 0;
   end Has_Gicon;

   --------------
   -- Has_Icon --
   --------------

   function Has_Icon
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon_Name : UTF8_String) return Boolean
   is
      function Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_theme_has_icon");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
      return Tmp_Return /= 0;
   end Has_Icon;

   ---------------------
   -- Lookup_By_Gicon --
   ---------------------

   function Lookup_By_Gicon
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon      : Glib.G_Icon.G_Icon;
       Size      : Glib.Gint;
       Scale     : Glib.Gint;
       Direction : Gtk.Enums.Gtk_Text_Direction;
       Flags     : Gtk_Icon_Lookup_Flags)
       return Gtk.Icon_Paintable.Gtk_Icon_Paintable
   is
      function Internal
         (Self      : System.Address;
          Icon      : Glib.G_Icon.G_Icon;
          Size      : Glib.Gint;
          Scale     : Glib.Gint;
          Direction : Gtk.Enums.Gtk_Text_Direction;
          Flags     : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_by_gicon");
      Stub_Gtk_Icon_Paintable : Gtk.Icon_Paintable.Gtk_Icon_Paintable_Record;
   begin
      return Gtk.Icon_Paintable.Gtk_Icon_Paintable (Get_User_Data (Internal (Get_Object (Self), Icon, Size, Scale, Direction, Flags), Stub_Gtk_Icon_Paintable));
   end Lookup_By_Gicon;

   -----------------
   -- Lookup_Icon --
   -----------------

   function Lookup_Icon
      (Self      : not null access Gtk_Icon_Theme_Record;
       Icon_Name : UTF8_String;
       Fallbacks : GNAT.Strings.String_List;
       Size      : Glib.Gint;
       Scale     : Glib.Gint;
       Direction : Gtk.Enums.Gtk_Text_Direction;
       Flags     : Gtk_Icon_Lookup_Flags)
       return Gtk.Icon_Paintable.Gtk_Icon_Paintable
   is
      function Internal
         (Self      : System.Address;
          Icon_Name : Gtkada.Types.Chars_Ptr;
          Fallbacks : Gtkada.Types.chars_ptr_array;
          Size      : Glib.Gint;
          Scale     : Glib.Gint;
          Direction : Gtk.Enums.Gtk_Text_Direction;
          Flags     : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_icon");
      Tmp_Icon_Name           : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Tmp_Fallbacks           : Gtkada.Types.chars_ptr_array := From_String_List (Fallbacks);
      Stub_Gtk_Icon_Paintable : Gtk.Icon_Paintable.Gtk_Icon_Paintable_Record;
      Tmp_Return              : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Self), Tmp_Icon_Name, Tmp_Fallbacks, Size, Scale, Direction, Flags);
      Gtkada.Types.Free (Tmp_Fallbacks);
      Free (Tmp_Icon_Name);
      return Gtk.Icon_Paintable.Gtk_Icon_Paintable (Get_User_Data (Tmp_Return, Stub_Gtk_Icon_Paintable));
   end Lookup_Icon;

   -----------------------
   -- Set_Resource_Path --
   -----------------------

   procedure Set_Resource_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_icon_theme_set_resource_path");
      Tmp_Path : Gtkada.Types.chars_ptr_array := From_String_List (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path);
      Gtkada.Types.Free (Tmp_Path);
   end Set_Resource_Path;

   ---------------------
   -- Set_Search_Path --
   ---------------------

   procedure Set_Search_Path
      (Self : not null access Gtk_Icon_Theme_Record;
       Path : GNAT.Strings.String_List)
   is
      procedure Internal
         (Self : System.Address;
          Path : Gtkada.Types.chars_ptr_array);
      pragma Import (C, Internal, "gtk_icon_theme_set_search_path");
      Tmp_Path : Gtkada.Types.chars_ptr_array := From_String_List (Path);
   begin
      Internal (Get_Object (Self), Tmp_Path);
      Gtkada.Types.Free (Tmp_Path);
   end Set_Search_Path;

   --------------------
   -- Set_Theme_Name --
   --------------------

   procedure Set_Theme_Name
      (Self       : not null access Gtk_Icon_Theme_Record;
       Theme_Name : UTF8_String := "")
   is
      procedure Internal
         (Self       : System.Address;
          Theme_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_set_theme_name");
      Tmp_Theme_Name : Gtkada.Types.Chars_Ptr;
   begin
      Tmp_Theme_Name :=
        (if Theme_Name = ""
         then Gtkada.Types.Null_Ptr
         else New_String (Theme_Name));
      Internal (Get_Object (Self), Tmp_Theme_Name);
      Free (Tmp_Theme_Name);
   end Set_Theme_Name;

   ---------------------
   -- Get_For_Display --
   ---------------------

   function Get_For_Display
      (Display : not null access Gdk.Display.Gdk_Display_Record'Class)
       return Gtk_Icon_Theme
   is
      function Internal (Display : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_get_for_display");
      Stub_Gtk_Icon_Theme : Gtk_Icon_Theme_Record;
   begin
      return Gtk.Icon_Theme.Gtk_Icon_Theme (Get_User_Data (Internal (Get_Object (Display)), Stub_Gtk_Icon_Theme));
   end Get_For_Display;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Icon_Theme_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Icon_Theme_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   procedure Connect
      (Object  : access Gtk_Icon_Theme_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_Theme_Void;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Icon_Theme_Record'Class;
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

   procedure Marsh_Gtk_Icon_Theme_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Icon_Theme_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Icon_Theme_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Icon_Theme_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Icon_Theme_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Icon_Theme_Record'Class;
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
   exception
      when E : others => Process_Exception (E);
   end Marsh_GObject_Void;

   -------------------------------
   -- Marsh_Gtk_Icon_Theme_Void --
   -------------------------------

   procedure Marsh_Gtk_Icon_Theme_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Icon_Theme_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Icon_Theme := Gtk_Icon_Theme (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
   exception
      when E : others => Process_Exception (E);
   end Marsh_Gtk_Icon_Theme_Void;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Icon_Theme_Record;
       Call  : Cb_Gtk_Icon_Theme_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "changed" & ASCII.NUL, Call, After);
   end On_Changed;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
      (Self  : not null access Gtk_Icon_Theme_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "changed" & ASCII.NUL, Call, After, Slot);
   end On_Changed;

end Gtk.Icon_Theme;
