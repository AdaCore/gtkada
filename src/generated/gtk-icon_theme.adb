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
with Gtkada.C;                   use Gtkada.C;
with Gtkada.Types;               use Gtkada.Types;

package body Gtk.Icon_Theme is

   package Points_Arrays is new Gtkada.C.Unbounded_Arrays
     (Gdk.Types.Gdk_Point, (0, 0), Positive, Gdk.Types.Gdk_Points_Array);

   package Type_Conversion_Gtk_Icon_Theme is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Icon_Theme_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_Theme);

   package Type_Conversion_Gtk_Icon_Info is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Icon_Info_Get_Type'Access, Gtk_Icon_Info_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Icon_Info);

   ----------------------------------
   -- Gtk_Icon_Info_New_For_Pixbuf --
   ----------------------------------

   function Gtk_Icon_Info_New_For_Pixbuf
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Icon_Info
   is
      Icon_Info : constant Gtk_Icon_Info := new Gtk_Icon_Info_Record;
   begin
      Gtk.Icon_Theme.Initialize_For_Pixbuf (Icon_Info, Icon_Theme, Pixbuf);
      return Icon_Info;
   end Gtk_Icon_Info_New_For_Pixbuf;

   ------------------------
   -- Gtk_Icon_Theme_New --
   ------------------------

   function Gtk_Icon_Theme_New return Gtk_Icon_Theme is
      Icon_Theme : constant Gtk_Icon_Theme := new Gtk_Icon_Theme_Record;
   begin
      Gtk.Icon_Theme.Initialize (Icon_Theme);
      return Icon_Theme;
   end Gtk_Icon_Theme_New;

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
   begin
      Icon_Info := new Gtk_Icon_Info_Record;
      Gtk.Icon_Theme.Initialize_For_Pixbuf (Icon_Info, Icon_Theme, Pixbuf);
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
      if not Icon_Theme.Is_Created then
         Set_Object (Icon_Theme, Internal);
      end if;
   end Initialize;

   ---------------------------
   -- Initialize_For_Pixbuf --
   ---------------------------

   procedure Initialize_For_Pixbuf
      (Icon_Info  : not null access Gtk_Icon_Info_Record'Class;
       Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Pixbuf     : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal
         (Icon_Theme : System.Address;
          Pixbuf     : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_new_for_pixbuf");
   begin
      if not Icon_Info.Is_Created then
         Set_Object (Icon_Info, Internal (Get_Object (Icon_Theme), Get_Object (Pixbuf)));
      end if;
   end Initialize_For_Pixbuf;

   -----------------------
   -- Add_Resource_Path --
   -----------------------

   procedure Add_Resource_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Path       : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_add_resource_path");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
   begin
      Internal (Get_Object (Icon_Theme), Tmp_Path);
      Free (Tmp_Path);
   end Add_Resource_Path;

   ------------------------
   -- Append_Search_Path --
   ------------------------

   procedure Append_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Path       : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_append_search_path");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
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
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Names : Gtkada.Types.chars_ptr_array;
          Size       : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_choose_icon");
      Tmp_Icon_Names     : Gtkada.Types.chars_ptr_array := From_String_List (Icon_Names);
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Names, Size, Flags);
      Gtkada.Types.Free (Tmp_Icon_Names);
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Tmp_Return, Stub_Gtk_Icon_Info));
   end Choose_Icon;

   ---------------------------
   -- Choose_Icon_For_Scale --
   ---------------------------

   function Choose_Icon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Names : GNAT.Strings.String_List;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Names : Gtkada.Types.chars_ptr_array;
          Size       : Glib.Gint;
          Scale      : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_choose_icon_for_scale");
      Tmp_Icon_Names     : Gtkada.Types.chars_ptr_array := From_String_List (Icon_Names);
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Names, Size, Scale, Flags);
      Gtkada.Types.Free (Tmp_Icon_Names);
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Tmp_Return, Stub_Gtk_Icon_Info));
   end Choose_Icon_For_Scale;

   ----------
   -- Copy --
   ----------

   function Copy
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gtk_Icon_Info
   is
      function Internal (Icon_Info : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_copy");
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
   begin
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Internal (Get_Object (Icon_Info)), Stub_Gtk_Icon_Info));
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Icon_Info : not null access Gtk_Icon_Info_Record) is
      procedure Internal (Icon_Info : System.Address);
      pragma Import (C, Internal, "gtk_icon_info_free");
   begin
      Internal (Get_Object (Icon_Info));
   end Free;

   -----------------------
   -- Get_Attach_Points --
   -----------------------

   function Get_Attach_Points
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gdk.Types.Gdk_Points_Array
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

   --------------------
   -- Get_Base_Scale --
   --------------------

   function Get_Base_Scale
      (Icon_Info : not null access Gtk_Icon_Info_Record) return Glib.Gint
   is
      function Internal (Icon_Info : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_info_get_base_scale");
   begin
      return Internal (Get_Object (Icon_Info));
   end Get_Base_Scale;

   -------------------
   -- Get_Base_Size --
   -------------------

   function Get_Base_Size
      (Icon_Info : not null access Gtk_Icon_Info_Record) return Glib.Gint
   is
      function Internal (Icon_Info : System.Address) return Glib.Gint;
      pragma Import (C, Internal, "gtk_icon_info_get_base_size");
   begin
      return Internal (Get_Object (Icon_Info));
   end Get_Base_Size;

   ------------------------
   -- Get_Builtin_Pixbuf --
   ------------------------

   function Get_Builtin_Pixbuf
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
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

   function Get_Display_Name
      (Icon_Info : not null access Gtk_Icon_Info_Record) return UTF8_String
   is
      function Internal
         (Icon_Info : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_info_get_display_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Icon_Info)));
   end Get_Display_Name;

   -----------------------
   -- Get_Embedded_Rect --
   -----------------------

   procedure Get_Embedded_Rect
      (Icon_Info              : not null access Gtk_Icon_Info_Record;
       Rectangle              : out Gdk.Rectangle.Gdk_Rectangle;
       Has_Embedded_Rectangle : out Boolean)
   is
      function Internal
         (Icon_Info     : System.Address;
          Acc_Rectangle : access Gdk.Rectangle.Gdk_Rectangle)
          return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_info_get_embedded_rect");
      Acc_Rectangle : aliased Gdk.Rectangle.Gdk_Rectangle;
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Acc_Rectangle'Access);
      Rectangle := Acc_Rectangle;
      Has_Embedded_Rectangle := Tmp_Return /= 0;
   end Get_Embedded_Rect;

   ---------------------------
   -- Get_Example_Icon_Name --
   ---------------------------

   function Get_Example_Icon_Name
      (Icon_Theme : not null access Gtk_Icon_Theme_Record)
       return UTF8_String
   is
      function Internal
         (Icon_Theme : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_icon_theme_get_example_icon_name");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Icon_Theme)));
   end Get_Example_Icon_Name;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
      (Icon_Info : not null access Gtk_Icon_Info_Record) return UTF8_String
   is
      function Internal
         (Icon_Info : System.Address) return Gtkada.Types.Chars_Ptr;
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
          Icon_Name  : Gtkada.Types.Chars_Ptr) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_theme_has_icon");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Tmp_Return    : Glib.Gboolean;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
      return Tmp_Return /= 0;
   end Has_Icon;

   -----------------
   -- Is_Symbolic --
   -----------------

   function Is_Symbolic
      (Icon_Info : not null access Gtk_Icon_Info_Record) return Boolean
   is
      function Internal (Icon_Info : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_info_is_symbolic");
   begin
      return Internal (Get_Object (Icon_Info)) /= 0;
   end Is_Symbolic;

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
          Context    : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_list_icons");
      Tmp_Context : Gtkada.Types.Chars_Ptr;
      Tmp_Return  : Gtk.Enums.String_List.Glist;
   begin
      if Context = "" then
         Tmp_Context := Gtkada.Types.Null_Ptr;
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
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags;
       Error      : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Gtkada.Types.Chars_Ptr;
          Size       : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags;
          Acc_Error  : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_load_icon");
      Acc_Error       : aliased Glib.Error.GError;
      Tmp_Icon_Name   : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Flags, Acc_Error'Access);
      Free (Tmp_Icon_Name);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Icon;

   ---------------
   -- Load_Icon --
   ---------------

   function Load_Icon
      (Icon_Info : not null access Gtk_Icon_Info_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Icon_Info : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_icon");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Icon_Info)), Stub_Gdk_Pixbuf));
   end Load_Icon;

   -------------------------
   -- Load_Icon_For_Scale --
   -------------------------

   function Load_Icon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags;
       Error      : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Gtkada.Types.Chars_Ptr;
          Size       : Glib.Gint;
          Scale      : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags;
          Acc_Error  : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_load_icon_for_scale");
      Acc_Error       : aliased Glib.Error.GError;
      Tmp_Icon_Name   : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return      : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Scale, Flags, Acc_Error'Access);
      Free (Tmp_Icon_Name);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Icon_For_Scale;

   ------------------
   -- Load_Surface --
   ------------------

   function Load_Surface
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       For_Window : Gdk.Gdk_Window;
       Flags      : Gtk_Icon_Lookup_Flags;
       Error      : access Glib.Error.GError) return Cairo.Cairo_Surface
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Gtkada.Types.Chars_Ptr;
          Size       : Glib.Gint;
          Scale      : Glib.Gint;
          For_Window : Gdk.Gdk_Window;
          Flags      : Gtk_Icon_Lookup_Flags;
          Acc_Error  : access Glib.Error.GError) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gtk_icon_theme_load_surface");
      Acc_Error     : aliased Glib.Error.GError;
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Tmp_Return    : Cairo.Cairo_Surface;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Scale, For_Window, Flags, Acc_Error'Access);
      Free (Tmp_Icon_Name);
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Tmp_Return;
   end Load_Surface;

   ------------------
   -- Load_Surface --
   ------------------

   function Load_Surface
      (Icon_Info  : not null access Gtk_Icon_Info_Record;
       For_Window : Gdk.Gdk_Window) return Cairo.Cairo_Surface
   is
      function Internal
         (Icon_Info  : System.Address;
          For_Window : Gdk.Gdk_Window) return Cairo.Cairo_Surface;
      pragma Import (C, Internal, "gtk_icon_info_load_surface");
   begin
      return Internal (Get_Object (Icon_Info), For_Window);
   end Load_Surface;

   -------------------
   -- Load_Symbolic --
   -------------------

   function Load_Symbolic
      (Icon_Info     : not null access Gtk_Icon_Info_Record;
       Fg            : Gdk.RGBA.Gdk_RGBA;
       Success_Color : Gdk.RGBA.Gdk_RGBA;
       Warning_Color : Gdk.RGBA.Gdk_RGBA;
       Error_Color   : Gdk.RGBA.Gdk_RGBA;
       Was_Symbolic  : access Boolean;
       Error         : access Glib.Error.GError)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Info        : System.Address;
          Fg               : Gdk.RGBA.Gdk_RGBA;
          Success_Color    : System.Address;
          Warning_Color    : System.Address;
          Error_Color      : System.Address;
          Acc_Was_Symbolic : access Glib.Gboolean;
          Acc_Error        : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_symbolic");
      Acc_Was_Symbolic     : aliased Boolean;
      Acc_Error            : aliased Glib.Error.GError;
      Tmp_Acc_Was_Symbolic : aliased Glib.Gboolean;
      Stub_Gdk_Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Fg, Gdk.RGBA.Gdk_RGBA_Or_Null (Success_Color'Address), Gdk.RGBA.Gdk_RGBA_Or_Null (Warning_Color'Address), Gdk.RGBA.Gdk_RGBA_Or_Null (Error_Color'Address), Tmp_Acc_Was_Symbolic'Access, Acc_Error'Access);
      Acc_Was_Symbolic := Tmp_Acc_Was_Symbolic /= 0;
      if Was_Symbolic /= null then
         Was_Symbolic.all := Acc_Was_Symbolic;
      end if;
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Symbolic;

   -------------------------------
   -- Load_Symbolic_For_Context --
   -------------------------------

   function Load_Symbolic_For_Context
      (Icon_Info    : not null access Gtk_Icon_Info_Record;
       Context      : not null access Gtk.Style_Context.Gtk_Style_Context_Record'Class;
       Was_Symbolic : access Boolean;
       Error        : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Info        : System.Address;
          Context          : System.Address;
          Acc_Was_Symbolic : access Glib.Gboolean;
          Acc_Error        : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_symbolic_for_context");
      Acc_Was_Symbolic     : aliased Boolean;
      Acc_Error            : aliased Glib.Error.GError;
      Tmp_Acc_Was_Symbolic : aliased Glib.Gboolean;
      Stub_Gdk_Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Get_Object (Context), Tmp_Acc_Was_Symbolic'Access, Acc_Error'Access);
      Acc_Was_Symbolic := Tmp_Acc_Was_Symbolic /= 0;
      if Was_Symbolic /= null then
         Was_Symbolic.all := Acc_Was_Symbolic;
      end if;
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Symbolic_For_Context;

   -----------------------------
   -- Load_Symbolic_For_Style --
   -----------------------------

   function Load_Symbolic_For_Style
      (Icon_Info    : not null access Gtk_Icon_Info_Record;
       Style        : not null access Gtk.Style.Gtk_Style_Record'Class;
       State        : Gtk.Enums.Gtk_State_Type;
       Was_Symbolic : access Boolean;
       Error        : access Glib.Error.GError) return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal
         (Icon_Info        : System.Address;
          Style            : System.Address;
          State            : Gtk.Enums.Gtk_State_Type;
          Acc_Was_Symbolic : access Glib.Gboolean;
          Acc_Error        : access Glib.Error.GError) return System.Address;
      pragma Import (C, Internal, "gtk_icon_info_load_symbolic_for_style");
      Acc_Was_Symbolic     : aliased Boolean;
      Acc_Error            : aliased Glib.Error.GError;
      Tmp_Acc_Was_Symbolic : aliased Glib.Gboolean;
      Stub_Gdk_Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf_Record;
      Tmp_Return           : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Info), Get_Object (Style), State, Tmp_Acc_Was_Symbolic'Access, Acc_Error'Access);
      Acc_Was_Symbolic := Tmp_Acc_Was_Symbolic /= 0;
      if Was_Symbolic /= null then
         Was_Symbolic.all := Acc_Was_Symbolic;
      end if;
      if Error /= null then
         Error.all := Acc_Error;
      end if;
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Tmp_Return, Stub_Gdk_Pixbuf));
   end Load_Symbolic_For_Style;

   ---------------------
   -- Lookup_By_Gicon --
   ---------------------

   function Lookup_By_Gicon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon       : Glib.G_Icon.G_Icon;
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon       : Glib.G_Icon.G_Icon;
          Size       : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_by_gicon");
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
   begin
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Internal (Get_Object (Icon_Theme), Icon, Size, Flags), Stub_Gtk_Icon_Info));
   end Lookup_By_Gicon;

   -------------------------------
   -- Lookup_By_Gicon_For_Scale --
   -------------------------------

   function Lookup_By_Gicon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon       : Glib.G_Icon.G_Icon;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon       : Glib.G_Icon.G_Icon;
          Size       : Glib.Gint;
          Scale      : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_by_gicon_for_scale");
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
   begin
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Internal (Get_Object (Icon_Theme), Icon, Size, Scale, Flags), Stub_Gtk_Icon_Info));
   end Lookup_By_Gicon_For_Scale;

   -----------------
   -- Lookup_Icon --
   -----------------

   function Lookup_Icon
      (Icon_Theme : not null access Gtk_Icon_Theme_Record'Class;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Gtkada.Types.Chars_Ptr;
          Size       : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_icon");
      Tmp_Icon_Name      : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Flags);
      Free (Tmp_Icon_Name);
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Tmp_Return, Stub_Gtk_Icon_Info));
   end Lookup_Icon;

   ---------------------------
   -- Lookup_Icon_For_Scale --
   ---------------------------

   function Lookup_Icon_For_Scale
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Icon_Name  : UTF8_String;
       Size       : Glib.Gint;
       Scale      : Glib.Gint;
       Flags      : Gtk_Icon_Lookup_Flags) return Gtk_Icon_Info
   is
      function Internal
         (Icon_Theme : System.Address;
          Icon_Name  : Gtkada.Types.Chars_Ptr;
          Size       : Glib.Gint;
          Scale      : Glib.Gint;
          Flags      : Gtk_Icon_Lookup_Flags) return System.Address;
      pragma Import (C, Internal, "gtk_icon_theme_lookup_icon_for_scale");
      Tmp_Icon_Name      : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Stub_Gtk_Icon_Info : Gtk_Icon_Info_Record;
      Tmp_Return         : System.Address;
   begin
      Tmp_Return := Internal (Get_Object (Icon_Theme), Tmp_Icon_Name, Size, Scale, Flags);
      Free (Tmp_Icon_Name);
      return Gtk.Icon_Theme.Gtk_Icon_Info (Get_User_Data (Tmp_Return, Stub_Gtk_Icon_Info));
   end Lookup_Icon_For_Scale;

   -------------------------
   -- Prepend_Search_Path --
   -------------------------

   procedure Prepend_Search_Path
      (Icon_Theme : not null access Gtk_Icon_Theme_Record;
       Path       : UTF8_String)
   is
      procedure Internal
         (Icon_Theme : System.Address;
          Path       : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_prepend_search_path");
      Tmp_Path : Gtkada.Types.Chars_Ptr := New_String (Path);
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
      function Internal (Icon_Theme : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_icon_theme_rescan_if_needed");
   begin
      return Internal (Get_Object (Icon_Theme)) /= 0;
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
          Theme_Name : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_icon_theme_set_custom_theme");
      Tmp_Theme_Name : Gtkada.Types.Chars_Ptr;
   begin
      if Theme_Name = "" then
         Tmp_Theme_Name := Gtkada.Types.Null_Ptr;
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
      (Icon_Info       : not null access Gtk_Icon_Info_Record;
       Raw_Coordinates : Boolean)
   is
      procedure Internal
         (Icon_Info       : System.Address;
          Raw_Coordinates : Glib.Gboolean);
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
          Path       : Gtkada.Types.chars_ptr_array;
          N_Elements : Glib.Gint);
      pragma Import (C, Internal, "gtk_icon_theme_set_search_path");
      Tmp_Path : Gtkada.Types.chars_ptr_array := From_String_List (Path);
   begin
      Internal (Get_Object (Icon_Theme), Tmp_Path, Path'Length);
      Gtkada.Types.Free (Tmp_Path);
   end Set_Search_Path;

   ----------------------
   -- Add_Builtin_Icon --
   ----------------------

   procedure Add_Builtin_Icon
      (Icon_Name : UTF8_String;
       Size      : Glib.Gint;
       Pixbuf    : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Icon_Name : Gtkada.Types.Chars_Ptr;
          Size      : Glib.Gint;
          Pixbuf    : System.Address);
      pragma Import (C, Internal, "gtk_icon_theme_add_builtin_icon");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
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

   use type System.Address;

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
      exception when E : others => Process_Exception (E);
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
      exception when E : others => Process_Exception (E);
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
