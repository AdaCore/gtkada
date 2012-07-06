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
with Gtkada.Bindings;            use Gtkada.Bindings;
with Interfaces.C.Strings;       use Interfaces.C.Strings;

package body Gtk.Status_Icon is

   package Type_Conversion_Gtk_Status_Icon is new Glib.Type_Conversion_Hooks.Hook_Registrator
     (Get_Type'Access, Gtk_Status_Icon_Record);
   pragma Unreferenced (Type_Conversion_Gtk_Status_Icon);

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Status_Icon : out Gtk_Status_Icon) is
   begin
      Status_Icon := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize (Status_Icon);
   end Gtk_New;

   -----------------------
   -- Gtk_New_From_File --
   -----------------------

   procedure Gtk_New_From_File
      (Status_Icon : out Gtk_Status_Icon;
       Filename    : UTF8_String)
   is
   begin
      Status_Icon := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize_From_File (Status_Icon, Filename);
   end Gtk_New_From_File;

   ------------------------
   -- Gtk_New_From_Gicon --
   ------------------------

   procedure Gtk_New_From_Gicon
      (Status_Icon : out Gtk_Status_Icon;
       Icon        : Glib.G_Icon.G_Icon)
   is
   begin
      Status_Icon := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize_From_Gicon (Status_Icon, Icon);
   end Gtk_New_From_Gicon;

   ----------------------------
   -- Gtk_New_From_Icon_Name --
   ----------------------------

   procedure Gtk_New_From_Icon_Name
      (Status_Icon : out Gtk_Status_Icon;
       Icon_Name   : UTF8_String)
   is
   begin
      Status_Icon := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize_From_Icon_Name (Status_Icon, Icon_Name);
   end Gtk_New_From_Icon_Name;

   -------------------------
   -- Gtk_New_From_Pixbuf --
   -------------------------

   procedure Gtk_New_From_Pixbuf
      (Status_Icon : out Gtk_Status_Icon;
       Pixbuf      : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
   begin
      Status_Icon := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize_From_Pixbuf (Status_Icon, Pixbuf);
   end Gtk_New_From_Pixbuf;

   ------------------------
   -- Gtk_New_From_Stock --
   ------------------------

   procedure Gtk_New_From_Stock
      (Status_Icon : out Gtk_Status_Icon;
       Stock_Id    : UTF8_String)
   is
   begin
      Status_Icon := new Gtk_Status_Icon_Record;
      Gtk.Status_Icon.Initialize_From_Stock (Status_Icon, Stock_Id);
   end Gtk_New_From_Stock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new");
   begin
      Set_Object (Status_Icon, Internal);
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Filename    : UTF8_String)
   is
      function Internal
         (Filename : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_file");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Filename);
      Free (Tmp_Filename);
      Set_Object (Status_Icon, Tmp_Return);
   end Initialize_From_File;

   ---------------------------
   -- Initialize_From_Gicon --
   ---------------------------

   procedure Initialize_From_Gicon
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Icon        : Glib.G_Icon.G_Icon)
   is
      function Internal (Icon : Glib.G_Icon.G_Icon) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_gicon");
   begin
      Set_Object (Status_Icon, Internal (Icon));
   end Initialize_From_Gicon;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Icon_Name   : UTF8_String)
   is
      function Internal
         (Icon_Name : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
      Tmp_Return    : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
      Set_Object (Status_Icon, Tmp_Return);
   end Initialize_From_Icon_Name;

   ----------------------------
   -- Initialize_From_Pixbuf --
   ----------------------------

   procedure Initialize_From_Pixbuf
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Pixbuf      : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      function Internal (Pixbuf : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_pixbuf");
   begin
      Set_Object (Status_Icon, Internal (Get_Object (Pixbuf)));
   end Initialize_From_Pixbuf;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Stock_Id    : UTF8_String)
   is
      function Internal
         (Stock_Id : Interfaces.C.Strings.chars_ptr) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      Tmp_Return := Internal (Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
      Set_Object (Status_Icon, Tmp_Return);
   end Initialize_From_Stock;

   ---------------
   -- Get_Gicon --
   ---------------

   function Get_Gicon
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Glib.G_Icon.G_Icon
   is
      function Internal
         (Status_Icon : System.Address) return Glib.G_Icon.G_Icon;
      pragma Import (C, Internal, "gtk_status_icon_get_gicon");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_Gicon;

   ---------------------
   -- Get_Has_Tooltip --
   ---------------------

   function Get_Has_Tooltip
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_status_icon_get_has_tooltip");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Get_Has_Tooltip;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_icon_name");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Status_Icon)));
   end Get_Icon_Name;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Gdk.Pixbuf.Gdk_Pixbuf
   is
      function Internal (Status_Icon : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_get_pixbuf");
      Stub_Gdk_Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf_Record;
   begin
      return Gdk.Pixbuf.Gdk_Pixbuf (Get_User_Data (Internal (Get_Object (Status_Icon)), Stub_Gdk_Pixbuf));
   end Get_Pixbuf;

   ----------------
   -- Get_Screen --
   ----------------

   function Get_Screen
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Gdk.Screen.Gdk_Screen
   is
      function Internal (Status_Icon : System.Address) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_get_screen");
      Stub_Gdk_Screen : Gdk.Screen.Gdk_Screen_Record;
   begin
      return Gdk.Screen.Gdk_Screen (Get_User_Data (Internal (Get_Object (Status_Icon)), Stub_Gdk_Screen));
   end Get_Screen;

   --------------
   -- Get_Size --
   --------------

   function Get_Size
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Gint
   is
      function Internal (Status_Icon : System.Address) return Gint;
      pragma Import (C, Internal, "gtk_status_icon_get_size");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_Size;

   ---------------
   -- Get_Stock --
   ---------------

   function Get_Stock
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_stock");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Status_Icon)));
   end Get_Stock;

   ----------------------
   -- Get_Storage_Type --
   ----------------------

   function Get_Storage_Type
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Gtk.Image.Gtk_Image_Type
   is
      function Internal
         (Status_Icon : System.Address) return Gtk.Image.Gtk_Image_Type;
      pragma Import (C, Internal, "gtk_status_icon_get_storage_type");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_Storage_Type;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_title");
   begin
      return Interfaces.C.Strings.Value (Internal (Get_Object (Status_Icon)));
   end Get_Title;

   ------------------------
   -- Get_Tooltip_Markup --
   ------------------------

   function Get_Tooltip_Markup
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_tooltip_markup");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Status_Icon)));
   end Get_Tooltip_Markup;

   ----------------------
   -- Get_Tooltip_Text --
   ----------------------

   function Get_Tooltip_Text
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address)
          return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_tooltip_text");
   begin
      return Gtkada.Bindings.Value_And_Free (Internal (Get_Object (Status_Icon)));
   end Get_Tooltip_Text;

   -----------------
   -- Get_Visible --
   -----------------

   function Get_Visible
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_status_icon_get_visible");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Get_Visible;

   -----------------------
   -- Get_X11_Window_Id --
   -----------------------

   function Get_X11_Window_Id
      (Status_Icon : not null access Gtk_Status_Icon_Record) return guint32
   is
      function Internal (Status_Icon : System.Address) return guint32;
      pragma Import (C, Internal, "gtk_status_icon_get_x11_window_id");
   begin
      return Internal (Get_Object (Status_Icon));
   end Get_X11_Window_Id;

   -----------------
   -- Is_Embedded --
   -----------------

   function Is_Embedded
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Boolean
   is
      function Internal (Status_Icon : System.Address) return Integer;
      pragma Import (C, Internal, "gtk_status_icon_is_embedded");
   begin
      return Boolean'Val (Internal (Get_Object (Status_Icon)));
   end Is_Embedded;

   -------------------
   -- Set_From_File --
   -------------------

   procedure Set_From_File
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Filename    : UTF8_String)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Filename    : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_from_file");
      Tmp_Filename : Interfaces.C.Strings.chars_ptr := New_String (Filename);
   begin
      Internal (Get_Object (Status_Icon), Tmp_Filename);
      Free (Tmp_Filename);
   end Set_From_File;

   --------------------
   -- Set_From_Gicon --
   --------------------

   procedure Set_From_Gicon
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Icon        : Glib.G_Icon.G_Icon)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Icon        : Glib.G_Icon.G_Icon);
      pragma Import (C, Internal, "gtk_status_icon_set_from_gicon");
   begin
      Internal (Get_Object (Status_Icon), Icon);
   end Set_From_Gicon;

   ------------------------
   -- Set_From_Icon_Name --
   ------------------------

   procedure Set_From_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Icon_Name   : UTF8_String)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Icon_Name   : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_from_icon_name");
      Tmp_Icon_Name : Interfaces.C.Strings.chars_ptr := New_String (Icon_Name);
   begin
      Internal (Get_Object (Status_Icon), Tmp_Icon_Name);
      Free (Tmp_Icon_Name);
   end Set_From_Icon_Name;

   ---------------------
   -- Set_From_Pixbuf --
   ---------------------

   procedure Set_From_Pixbuf
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Pixbuf      : access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Pixbuf      : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_set_from_pixbuf");
   begin
      Internal (Get_Object (Status_Icon), Get_Object_Or_Null (GObject (Pixbuf)));
   end Set_From_Pixbuf;

   --------------------
   -- Set_From_Stock --
   --------------------

   procedure Set_From_Stock
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Stock_Id    : UTF8_String)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Stock_Id    : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_from_stock");
      Tmp_Stock_Id : Interfaces.C.Strings.chars_ptr := New_String (Stock_Id);
   begin
      Internal (Get_Object (Status_Icon), Tmp_Stock_Id);
      Free (Tmp_Stock_Id);
   end Set_From_Stock;

   ---------------------
   -- Set_Has_Tooltip --
   ---------------------

   procedure Set_Has_Tooltip
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Has_Tooltip : Boolean)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Has_Tooltip : Integer);
      pragma Import (C, Internal, "gtk_status_icon_set_has_tooltip");
   begin
      Internal (Get_Object (Status_Icon), Boolean'Pos (Has_Tooltip));
   end Set_Has_Tooltip;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Name        : UTF8_String)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Name        : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_name");
      Tmp_Name : Interfaces.C.Strings.chars_ptr := New_String (Name);
   begin
      Internal (Get_Object (Status_Icon), Tmp_Name);
      Free (Tmp_Name);
   end Set_Name;

   ----------------
   -- Set_Screen --
   ----------------

   procedure Set_Screen
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Screen      : not null access Gdk.Screen.Gdk_Screen_Record'Class)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Screen      : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_set_screen");
   begin
      Internal (Get_Object (Status_Icon), Get_Object (Screen));
   end Set_Screen;

   ---------------
   -- Set_Title --
   ---------------

   procedure Set_Title
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Title       : UTF8_String)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Title       : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_title");
      Tmp_Title : Interfaces.C.Strings.chars_ptr := New_String (Title);
   begin
      Internal (Get_Object (Status_Icon), Tmp_Title);
      Free (Tmp_Title);
   end Set_Title;

   ------------------------
   -- Set_Tooltip_Markup --
   ------------------------

   procedure Set_Tooltip_Markup
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Markup      : UTF8_String := "")
   is
      procedure Internal
         (Status_Icon : System.Address;
          Markup      : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip_markup");
      Tmp_Markup : Interfaces.C.Strings.chars_ptr;
   begin
      if Markup = "" then
         Tmp_Markup := Interfaces.C.Strings.Null_Ptr;
      else
         Tmp_Markup := New_String (Markup);
      end if;
      Internal (Get_Object (Status_Icon), Tmp_Markup);
      Free (Tmp_Markup);
   end Set_Tooltip_Markup;

   ----------------------
   -- Set_Tooltip_Text --
   ----------------------

   procedure Set_Tooltip_Text
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Text        : UTF8_String)
   is
      procedure Internal
         (Status_Icon : System.Address;
          Text        : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip_text");
      Tmp_Text : Interfaces.C.Strings.chars_ptr := New_String (Text);
   begin
      Internal (Get_Object (Status_Icon), Tmp_Text);
      Free (Tmp_Text);
   end Set_Tooltip_Text;

   -----------------
   -- Set_Visible --
   -----------------

   procedure Set_Visible
      (Status_Icon : not null access Gtk_Status_Icon_Record;
       Visible     : Boolean)
   is
      procedure Internal (Status_Icon : System.Address; Visible : Integer);
      pragma Import (C, Internal, "gtk_status_icon_set_visible");
   begin
      Internal (Get_Object (Status_Icon), Boolean'Pos (Visible));
   end Set_Visible;

   -------------------
   -- Position_Menu --
   -------------------

   procedure Position_Menu
      (Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class;
       X       : out Gint;
       Y       : out Gint;
       Push_In : out Boolean;
       Icon    : Glib.Object.GObject)
   is
      procedure Internal
         (Menu    : System.Address;
          X       : out Gint;
          Y       : out Gint;
          Push_In : out Integer;
          Icon    : System.Address);
      pragma Import (C, Internal, "gtk_status_icon_position_menu");
      Tmp_Push_In : aliased Integer;
   begin
      Internal (Get_Object (Menu), X, Y, Tmp_Push_In, Get_Object (Icon));
      Push_In := Boolean'Val (Tmp_Push_In);
   end Position_Menu;

end Gtk.Status_Icon;
