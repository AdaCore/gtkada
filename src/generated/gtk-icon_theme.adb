------------------------------------------------------------------------------
--                                                                          --
--      Copyright (C) 1998-2000 E. Briot, J. Brobecker and A. Charlet       --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with GtkAda.Types;               use GtkAda.Types;
with Gtkada.Bindings;            use Gtkada.Bindings;
with Gtkada.C;                   use Gtkada.C;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Icon_Theme is

   function From_Object_Free
     (B : access Gtk_Icon_Info'Class) return Gtk_Icon_Info
   is
      Result : constant Gtk_Icon_Info := Gtk_Icon_Info (B.all);
   begin
      Glib.g_free (B.all'Address);
      return Result;
   end From_Object_Free;

   function From_Object (Object : System.Address) return Gtk_Icon_Info is
      S : Gtk_Icon_Info;
   begin
      S.Set_Object (Object);
      return S;
   end From_Object;

   package Points_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Point, (0, 0), Positive, Gdk.Types.Gdk_Points_Array);

   package Type_Conversion_Gtk_Icon_Theme is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Theme_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_Theme);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Icon_Theme : out Gtk_Icon_Theme) is
   begin
      Icon_Theme := new Gtk_Icon_Theme_Record;
      Gtk.Icon_Theme.Initialize (Icon_Theme);
   end Gtk_New;

   ------------------------
   -- Gtk_New_For_Pixbuf --
   ------------------------

   procedure Gtk_New_For_Pixbuf
      (Icon_Info  : out Gtk_Icon_Info;
       Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal
         (Icon_Theme : System.Address;
          Pixbuf     : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_new_for_pixbuf");
   begin
      Icon_Info.Set_Object (Internal (Get_Object (Icon_Theme), Get_Object (Pixbuf)));
   end Gtk_New_For_Pixbuf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_new");
   begin
      Set_Object (Icon_Theme, Internal);
   end Initialize;

   ------------------------
   -- Append_Search_Path --
   ------------------------

   procedure Append_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Path       : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_icon_theme_append_search_path");
      Tmp_Path : Interfaces.C.Strings.chars_ptr := New_String (Path);
   begin
      Internal (Get_Object (Icon_Theme), Tmp_Path);
      Free (Tmp_Path);
   end Append_Search_Path;

   -----------------
   -- Choose_Icon --
   -----------------

   function Choose_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon_Names : GNAT.Strings.String_List;
       Size       : Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Names : Interfaces.C.Strings.chars_ptr_array;
          Size       : Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_choose_icon");
      Tmp_Icon_Names : Interfaces.C.Strings.chars_ptr_array := From_String_List (Icon_Names);
      Tmp_Return     : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Names, Size, Flags);
      GtkAda.Types.Free (Tmp_Icon_Names);
      return From_Object (Tmp_Return);
   end Choose_Icon;

   ----------
   -- Copy --
   ----------

   function Copy (Icon_Info : Gtk_Icon_Info) return Gtk_Icon_Info is
      function Internal (Icon_Info : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_copy");
   begin
      return From_Object (Internal (Get_Object (Icon_Info)));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Icon_Info : Gtk_Icon_Info) is
      procedure Internal (Icon_Info : System.Address);
      pragma Import (C, Internal, "gtk_icon_info_free");
   begin
      Internal (Get_Object (Icon_Info));
   end Free;

   -----------------------
   -- Get_Attach_Points --
   -----------------------

   function Get_Attach_Points
      (Icon_Info : Gtk_Icon_Info) return Gdk.Types.Gdk_Points_Array
   is
      use Points_Arrays;
      function Internal
        (Icon_Info : System.Address;
         Result    : access Unbounded_Array_Access;
         N_Points  : access Gint) return Gboolean;
      pragma Import (C, Internal, "gtk_icon_info_get_attach_points");

      R : aliased Unbounded_Array_Access;
      N : aliased Gint;
   begin
      if Internal (Icon_Info.Get_Object, R'Unchecked_Access,
                   N'Unchecked_Access) = 0
      then
         R := null;
      end if;

      declare
         Result : constant Gdk_Points_Array := To_Array (R, Integer (N));
      begin
         G_Free (R);
         return Result;
      end;
   end Get_Attach_Points;

   -------------------
   -- Get_Base_Size --
   -------------------

   function Get_Base_Size (Icon_Info : Gtk_Icon_Info) return Gint is
      function Internal (Icon_Info : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_icon_info_get_base_size");
   begin
      return Internal (Get_Object (Icon_Info));
   end Get_Base_Size;

   ------------------------
   -- Get_Builtin_Pixbuf --
   ------------------------

   function Get_Builtin_Pixbuf
      (Icon_Info : Gtk_Icon_Info) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Icon_Info : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_get_builtin_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Icon_Info)), Stub_Gdk_Pixbuf));
   end Get_Builtin_Pixbuf;

   ----------------------
   -- Get_Display_Name --
   ----------------------

   function Get_Display_Name (Icon_Info : Gtk_Icon_Info) return UTF8_String is
      function Internal
         (Icon_Info : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_icon_info_get_display_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Icon_Info)));
   end Get_Display_Name;

   -----------------------
   -- Get_Embedded_Rect --
   -----------------------

   procedure Get_Embedded_Rect
      (Icon_Info              : Gtk_Icon_Info;
       Rectangle              : out Gdk.Rectangle.Gdk_Rectangle;
       Has_Embedded_Rectangle : out Boolean)
   is
      function Internal
         (Icon_Info     : System.Address;
          Acc_Rectangle : access Gdk.Rectangle.Gdk_Rectangle) return Integer;
      pragma Import (C, Internal, "gtk_icon_info_get_embedded_rect");
      Acc_Rectangle : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return    : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Acc_Rectangle'Access);
      Rectangle := Acc_Rectangle;
      Has_Embedded_Rectangle := Boolean'Val (Tmp_Return);
   end Get_Embedded_Rect;

   ---------------------------
   -- Get_Example_Icon_Name --
   ---------------------------

   function Get_Example_Icon_Name
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return UTF8_String
   is
      function Internal
         (Icon_Theme : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_icon_theme_get_example_icon_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Icon_Theme)));
   end Get_Example_Icon_Name;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Icon_Info : Gtk_Icon_Info) return UTF8_String is
      function Internal
         (Icon_Info : System.Address) return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_icon_info_get_filename");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Icon_Info)));
   end Get_Filename;

   --------------------
   -- Get_Icon_Sizes --
   --------------------

   function Get_Icon_Sizes
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String) return Gint_Array
   is
      use Gint_Arrays;
      function Internal
        (Icon_Theme : System.Address;
         Icon_Name  : String) return Unbounded_Array_Access;
      pragma Import (C, Internal, "gtk_icon_theme_get_icon_sizes");

      Res    : constant Unbounded_Array_Access := Internal
        (Get_Object (Icon_Theme), Icon_Name & ASCII.NUL);
      Result : constant Gint_Array := To_Gint_Array_Zero_Terminated (Res);
   begin
      G_Free (Res);
      return Result;
   end Get_Icon_Sizes;

   ---------------------
   -- Get_Search_Path --
   ---------------------

   function Get_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return GNAT.Strings.String_List
   is
      procedure Internal
        (Icon_Theme : System.Address;
         Path       : out chars_ptr_array_access;
         N_Elements : out Gint);
      pragma Import (C, Internal, "gtk_icon_theme_get_search_path");

      P : chars_ptr_array_access;
      N : Gint;
   begin
      Internal (Get_Object (Icon_Theme), P, N);
      declare
         Result : constant GNAT.Strings.String_List :=
           To_String_List (P.all, N);
      begin
         Free (P.all);
         return Result;
      end;
   end Get_Search_Path;

   --------------
   -- Has_Icon --
   --------------

   function Has_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String) return Boolean
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Interfaces.C.Strings.chars_ptr) return Integer;
      pragma Import (C, Internal, "gtk_icon_theme_has_icon");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
      Tmp_Return    : Integer;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
      return Boolean'Val (Tmp_Return);
   end Has_Icon;

   -------------------
   -- List_Contexts --
   -------------------

   function List_Contexts
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return Gtk.Enums.String_List.Glist
   is
      function Internal (Icon_Theme : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_list_contexts");
      Tmp_Return : Gtk.Enums.String_List.Glist;
   begin
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Get_Object (Icon_Theme)));
      return Tmp_Return;
   end List_Contexts;

   ----------------
   -- List_Icons --
   ----------------

   function List_Icons
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Context    : UTF8_String := "") return Gtk.Enums.String_List.Glist
   is
      function Internal
         (Icon_Theme : System.Address;
          Context    : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_list_icons");
      Tmp_Context : Interfaces.C.Strings.chars_ptr;
      Tmp_Return  : Gtk.Enums.String_List.Glist;
   begin
      if Context = "" then
         Tmp_Context := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Context := New_String (Context);
      end if;
      Gtk.Enums.String_List.Set_Object (Tmp_Return, Internal (Get_Object (Icon_Theme), Tmp_Context));
      Free (Tmp_Context);
      return Tmp_Return;
   end List_Icons;

   ---------------
   -- Load_Icon --
   ---------------

   function Load_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Interfaces.C.Strings.chars_ptr;
          Size       : Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_load_icon");
      Tmp_Icon_Name   : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Flags);
      Free (Tmp_Icon_Name);
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Icon;

   ---------------
   -- Load_Icon --
   ---------------

   function Load_Icon
      (Icon_Info : Gtk_Icon_Info) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Icon_Info : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_icon");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Icon_Info)), Stub_Gdk_Pixbuf));
   end Load_Icon;

   -------------------
   -- Load_Symbolic --
   -------------------

   function Load_Symbolic
      (Icon_Info     : Gtk_Icon_Info;
       Fg            : Gdk.RGBA.Gdk_RGBA;
       Success_Color : Gdk.RGBA.Gdk_RGBA;
       Warning_Color : Gdk.RGBA.Gdk_RGBA;
       Error_Color   : Gdk.RGBA.Gdk_RGBA;
       Was_Symbolic  : access Boolean) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Info        : System.Address;
          Fg               : Gdk.RGBA.Gdk_RGBA;
          Success_Color    : System.Address;
          Warning_Color    : System.Address;
          Error_Color      : System.Address;
          Acc_Was_Symbolic : access Integer) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_symbolic");
      Acc_Was_Symbolic     : aliased Boolean;
      Tmp_Acc_Was_Symbolic : aliased Integer;
      Stub_Gdk_Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Fg, Gdk.RGBA.Gdk_RGBA_Or_Null (Success_Color'Address), Gdk.RGBA.Gdk_RGBA_Or_Null (Warning_Color'Address), Gdk.RGBA.Gdk_RGBA_Or_Null (Error_Color'Address), Tmp_Acc_Was_Symbolic'Access);
      Acc_Was_Symbolic := Boolean'Val (Tmp_Acc_Was_Symbolic);
      if Was_Symbolic /= null then
         Was_Symbolic.all := Acc_Was_Symbolic;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Symbolic;

   -------------------------------
   -- Load_Symbolic_For_Context --
   -------------------------------

   function Load_Symbolic_For_Context
      (Icon_Info    : Gtk_Icon_Info;
       Context      : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Was_Symbolic : access Boolean) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Info        : System.Address;
          Context          : System.Address;
          Acc_Was_Symbolic : access Integer) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_symbolic_for_context");
      Acc_Was_Symbolic     : aliased Boolean;
      Tmp_Acc_Was_Symbolic : aliased Integer;
      Stub_Gdk_Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Get_Object (Context), Tmp_Acc_Was_Symbolic'Access);
      Acc_Was_Symbolic := Boolean'Val (Tmp_Acc_Was_Symbolic);
      if Was_Symbolic /= null then
         Was_Symbolic.all := Acc_Was_Symbolic;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Symbolic_For_Context;

   -----------------------------
   -- Load_Symbolic_For_Style --
   -----------------------------

   function Load_Symbolic_For_Style
      (Icon_Info    : Gtk_Icon_Info;
       Style        : not null access Gtk.Style.Gtk_Style_Record'Class;
       State        : Gtk.Enums.Gtk_State_Type;
       Was_Symbolic : access Boolean) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Info        : System.Address;
          Style            : System.Address;
          State            : Gtk.Enums.Gtk_State_Type;
          Acc_Was_Symbolic : access Integer) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_symbolic_for_style");
      Acc_Was_Symbolic     : aliased Boolean;
      Tmp_Acc_Was_Symbolic : aliased Integer;
      Stub_Gdk_Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Get_Object (Style), State, Tmp_Acc_Was_Symbolic'Access);
      Acc_Was_Symbolic := Boolean'Val (Tmp_Acc_Was_Symbolic);
      if Was_Symbolic /= null then
         Was_Symbolic.all := Acc_Was_Symbolic;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Symbolic_For_Style;

   ---------------------
   -- Lookup_By_Gicon --
   ---------------------

   function Lookup_By_Gicon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon       : Glib.G_Icon.G_Icon;
       Size       : Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon       : Glib.G_Icon.G_Icon;
          Size       : Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_by_gicon");
   begin
      return From_Object (Internal (Get_Object (Icon_Theme), Icon, Size, Flags));
   end Lookup_By_Gicon;

   -----------------
   -- Lookup_Icon --
   -----------------

   function Lookup_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon_Name  : UTF8_String;
       Size       : Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Interfaces.C.Strings.chars_ptr;
          Size       : Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_icon");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Flags);
      Free (Tmp_Icon_Name);
      return From_Object (Tmp_Return);
   end Lookup_Icon;

   -------------------------
   -- Prepend_Search_Path --
   -------------------------

   procedure Prepend_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Path       : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_icon_theme_prepend_search_path");
      Tmp_Path : Interfaces.C.Strings.chars_ptr := New_String (Path);
   begin
      Internal (Get_Object (Icon_Theme), Tmp_Path);
      Free (Tmp_Path);
   end Prepend_Search_Path;

   ----------------------
   -- Rescan_If_Needed --
   ----------------------

   function Rescan_If_Needed
      (Icon_Theme : not null access Gtk_Icon_Theme_Record) return Boolean
   is
      function Internal (Icon_Theme : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_icon_theme_rescan_if_needed");
   begin
      return Boolean'Val (Internal (Get_Object (Icon_Theme)));
   end Rescan_If_Needed;

   ----------------------
   -- Set_Custom_Theme --
   ----------------------

   procedure Set_Custom_Theme
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Theme_Name : UTF8_String := "")
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Theme_Name : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_icon_theme_set_custom_theme");
      Tmp_Theme_Name : Interfaces.C.Strings.chars_ptr;
   begin
      if Theme_Name = "" then
         Tmp_Theme_Name := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Theme_Name := New_String (Theme_Name);
      end if;
      Internal (Get_Object (Icon_Theme), Tmp_Theme_Name);
      Free (Tmp_Theme_Name);
   end Set_Custom_Theme;

   -------------------------
   -- Set_Raw_Coordinates --
   -------------------------

   procedure Set_Raw_Coordinates
      (Icon_Info       : Gtk_Icon_Info;
       Raw_Coordinates : Boolean)
   is
      procedure Internal
         (Icon_Info       : System.Address;
          Raw_Coordinates : Integer);
      pragma Import (C, Internal, "gtk_icon_info_set_raw_coordinates");
   begin
      Internal (Get_Object (Icon_Info), Boolean'Pos (Raw_Coordinates));
   end Set_Raw_Coordinates;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Screen     : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Screen     : System.Address);
      pragma Import (C, Internal, "gtk_icon_theme_set_screen");
   begin
      Internal (Get_Object (Icon_Theme), Get_Object (Screen));
   end Set_Screen;

   ---------------------
   -- Set_Search_Path --
   ---------------------

   procedure Set_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : GNAT.Strings.String_List)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Path       : Interfaces.C.Strings.chars_ptr_array;
          N_Elements : Gint);
      pragma Import (C, Internal, "gtk_icon_theme_set_search_path");
      Tmp_Path : Interfaces.C.Strings.chars_ptr_array := From_String_List (Path);
   begin
      Internal (Get_Object (Icon_Theme), Tmp_Path, Path'Length);
      GtkAda.Types.Free (Tmp_Path);
   end Set_Search_Path;

   ----------------------
   -- Add_Builtin_Icon --
   ----------------------

   procedure Add_Builtin_Icon
      (Icon_Name : UTF8_String;
       Size      : Gint;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Icon_Name : Interfaces.C.Strings.chars_ptr;
          Size      : Gint;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_icon_theme_add_builtin_icon");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
   begin
      Internal (Tmp_Icon_Name, Size, Get_Object (Pixbuf));
      Free (Tmp_Icon_Name);
   end Add_Builtin_Icon;

   -----------------
   -- Get_Default --
   -----------------

   function Get_Default return Gtk_Icon_Theme is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_get_default");
      Stub_Gtk_Icon_Theme : Gtk_Icon_Theme_Record;
   begin
      return Gtk.Icon_Theme.Gtk_Icon_Theme (Get_User_Data (Internal, Stub_Gtk_Icon_Theme));
   end Get_Default;

   --------------------
   -- Get_For_Screen --
   --------------------

   function Get_For_Screen
      (Screen : not null access Gdk.Screen.Gdk_Screen_Record'Class)
       return Gtk_Icon_Theme
   is
      function Internal (Screen : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_get_for_screen");
      Stub_Gtk_Icon_Theme : Gtk_Icon_Theme_Record;
   begin
      return Gtk.Icon_Theme.Gtk_Icon_Theme (Get_User_Data (Internal (Get_Object (Screen)), Stub_Gtk_Icon_Theme));
   end Get_For_Screen;

end Gtk.Icon_Theme;
