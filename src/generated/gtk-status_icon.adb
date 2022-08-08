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

   -------------------------
   -- Gtk_Status_Icon_New --
   -------------------------

   function Gtk_Status_Icon_New return Gtk_Status_Icon is
      Status_Icon : constant Gtk_Status_Icon := new Gtk_Status_Icon_Record;
   begin
      Gtk.Status_Icon.Initialize (Status_Icon);
      return Status_Icon;
   end Gtk_Status_Icon_New;

   -----------------------------------
   -- Gtk_Status_Icon_New_From_File --
   -----------------------------------

   function Gtk_Status_Icon_New_From_File
      (Filename : UTF8_String) return Gtk_Status_Icon
   is
      Status_Icon : constant Gtk_Status_Icon := new Gtk_Status_Icon_Record;
   begin
      Gtk.Status_Icon.Initialize_From_File (Status_Icon, Filename);
      return Status_Icon;
   end Gtk_Status_Icon_New_From_File;

   ------------------------------------
   -- Gtk_Status_Icon_New_From_Gicon --
   ------------------------------------

   function Gtk_Status_Icon_New_From_Gicon
      (Icon : Glib.G_Icon.G_Icon) return Gtk_Status_Icon
   is
      Status_Icon : constant Gtk_Status_Icon := new Gtk_Status_Icon_Record;
   begin
      Gtk.Status_Icon.Initialize_From_Gicon (Status_Icon, Icon);
      return Status_Icon;
   end Gtk_Status_Icon_New_From_Gicon;

   ----------------------------------------
   -- Gtk_Status_Icon_New_From_Icon_Name --
   ----------------------------------------

   function Gtk_Status_Icon_New_From_Icon_Name
      (Icon_Name : UTF8_String) return Gtk_Status_Icon
   is
      Status_Icon : constant Gtk_Status_Icon := new Gtk_Status_Icon_Record;
   begin
      Gtk.Status_Icon.Initialize_From_Icon_Name (Status_Icon, Icon_Name);
      return Status_Icon;
   end Gtk_Status_Icon_New_From_Icon_Name;

   -------------------------------------
   -- Gtk_Status_Icon_New_From_Pixbuf --
   -------------------------------------

   function Gtk_Status_Icon_New_From_Pixbuf
      (Pixbuf : not null access Gdk.Pixbuf.Gdk_Pixbuf_Record'Class)
       return Gtk_Status_Icon
   is
      Status_Icon : constant Gtk_Status_Icon := new Gtk_Status_Icon_Record;
   begin
      Gtk.Status_Icon.Initialize_From_Pixbuf (Status_Icon, Pixbuf);
      return Status_Icon;
   end Gtk_Status_Icon_New_From_Pixbuf;

   ------------------------------------
   -- Gtk_Status_Icon_New_From_Stock --
   ------------------------------------

   function Gtk_Status_Icon_New_From_Stock
      (Stock_Id : UTF8_String) return Gtk_Status_Icon
   is
      Status_Icon : constant Gtk_Status_Icon := new Gtk_Status_Icon_Record;
   begin
      Gtk.Status_Icon.Initialize_From_Stock (Status_Icon, Stock_Id);
      return Status_Icon;
   end Gtk_Status_Icon_New_From_Stock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class)
   is
      function Internal return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new");
   begin
      if not Status_Icon.Is_Created then
         Set_Object (Status_Icon, Internal);
      end if;
   end Initialize;

   --------------------------
   -- Initialize_From_File --
   --------------------------

   procedure Initialize_From_File
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Filename    : UTF8_String)
   is
      function Internal
         (Filename : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
      Tmp_Return   : System.Address;
   begin
      if not Status_Icon.Is_Created then
         Tmp_Return := Internal (Tmp_Filename);
         Free (Tmp_Filename);
         Set_Object (Status_Icon, Tmp_Return);
      end if;
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
      if not Status_Icon.Is_Created then
         Set_Object (Status_Icon, Internal (Icon));
      end if;
   end Initialize_From_Gicon;

   -------------------------------
   -- Initialize_From_Icon_Name --
   -------------------------------

   procedure Initialize_From_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Icon_Name   : UTF8_String)
   is
      function Internal
         (Icon_Name : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
      Tmp_Return    : System.Address;
   begin
      if not Status_Icon.Is_Created then
         Tmp_Return := Internal (Tmp_Icon_Name);
         Free (Tmp_Icon_Name);
         Set_Object (Status_Icon, Tmp_Return);
      end if;
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
      if not Status_Icon.Is_Created then
         Set_Object (Status_Icon, Internal (Get_Object (Pixbuf)));
      end if;
   end Initialize_From_Pixbuf;

   ---------------------------
   -- Initialize_From_Stock --
   ---------------------------

   procedure Initialize_From_Stock
      (Status_Icon : not null access Gtk_Status_Icon_Record'Class;
       Stock_Id    : UTF8_String)
   is
      function Internal
         (Stock_Id : Gtkada.Types.Chars_Ptr) return System.Address;
      pragma Import (C, Internal, "gtk_status_icon_new_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
      Tmp_Return   : System.Address;
   begin
      if not Status_Icon.Is_Created then
         Tmp_Return := Internal (Tmp_Stock_Id);
         Free (Tmp_Stock_Id);
         Set_Object (Status_Icon, Tmp_Return);
      end if;
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
      function Internal (Status_Icon : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_get_has_tooltip");
   begin
      return Internal (Get_Object (Status_Icon)) /= 0;
   end Get_Has_Tooltip;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_icon_name");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Status_Icon)));
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
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return Glib.Gint
   is
      function Internal (Status_Icon : System.Address) return Glib.Gint;
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
         (Status_Icon : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_stock");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Status_Icon)));
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
         (Status_Icon : System.Address) return Gtkada.Types.Chars_Ptr;
      pragma Import (C, Internal, "gtk_status_icon_get_title");
   begin
      return Gtkada.Bindings.Value_Allowing_Null (Internal (Get_Object (Status_Icon)));
   end Get_Title;

   ------------------------
   -- Get_Tooltip_Markup --
   ------------------------

   function Get_Tooltip_Markup
      (Status_Icon : not null access Gtk_Status_Icon_Record)
       return UTF8_String
   is
      function Internal
         (Status_Icon : System.Address) return Gtkada.Types.Chars_Ptr;
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
         (Status_Icon : System.Address) return Gtkada.Types.Chars_Ptr;
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
      function Internal (Status_Icon : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_get_visible");
   begin
      return Internal (Get_Object (Status_Icon)) /= 0;
   end Get_Visible;

   -----------------------
   -- Get_X11_Window_Id --
   -----------------------

   function Get_X11_Window_Id
      (Status_Icon : not null access Gtk_Status_Icon_Record) return Guint32
   is
      function Internal (Status_Icon : System.Address) return Guint32;
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
      function Internal (Status_Icon : System.Address) return Glib.Gboolean;
      pragma Import (C, Internal, "gtk_status_icon_is_embedded");
   begin
      return Internal (Get_Object (Status_Icon)) /= 0;
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
          Filename    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_from_file");
      Tmp_Filename : Gtkada.Types.Chars_Ptr := New_String (Filename);
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
          Icon_Name   : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_from_icon_name");
      Tmp_Icon_Name : Gtkada.Types.Chars_Ptr := New_String (Icon_Name);
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
          Stock_Id    : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_from_stock");
      Tmp_Stock_Id : Gtkada.Types.Chars_Ptr := New_String (Stock_Id);
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
          Has_Tooltip : Glib.Gboolean);
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
          Name        : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_name");
      Tmp_Name : Gtkada.Types.Chars_Ptr := New_String (Name);
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
          Title       : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_title");
      Tmp_Title : Gtkada.Types.Chars_Ptr := New_String (Title);
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
          Markup      : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip_markup");
      Tmp_Markup : Gtkada.Types.Chars_Ptr;
   begin
      if Markup = "" then
         Tmp_Markup := Gtkada.Types.Null_Ptr;
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
          Text        : Gtkada.Types.Chars_Ptr);
      pragma Import (C, Internal, "gtk_status_icon_set_tooltip_text");
      Tmp_Text : Gtkada.Types.Chars_Ptr := New_String (Text);
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
      procedure Internal
         (Status_Icon : System.Address;
          Visible     : Glib.Gboolean);
      pragma Import (C, Internal, "gtk_status_icon_set_visible");
   begin
      Internal (Get_Object (Status_Icon), Boolean'Pos (Visible));
   end Set_Visible;

   -------------------
   -- Position_Menu --
   -------------------

   procedure Position_Menu
      (Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class;
       X       : out Glib.Gint;
       Y       : out Glib.Gint;
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
      --  Custom body because we need to initialize to 0 here (on OSX).
      Tmp_Push_In : aliased Integer := 0;
   begin
      Internal (Get_Object (Menu), X, Y, Tmp_Push_In, Get_Object (Icon));
      Push_In := Tmp_Push_In /= 0;
   end Position_Menu;

   use type System.Address;

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Icon_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Icon_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Button_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Button_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Icon_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Icon_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Guint_Guint_Void, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Guint_Guint_Void);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gdk_Event_Scroll_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gdk_Event_Scroll_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_Gtk_Status_Icon_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_Gtk_Status_Icon_Gint_Boolean);

   function Cb_To_Address is new Ada.Unchecked_Conversion
     (Cb_GObject_Gint_Boolean, System.Address);
   function Address_To_Cb is new Ada.Unchecked_Conversion
     (System.Address, Cb_GObject_Gint_Boolean);

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Guint_Guint_Void;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean;
       After   : Boolean);

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gint_Boolean;
       After   : Boolean);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Button_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Scroll_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null);

   procedure Marsh_GObject_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Button_Boolean);

   procedure Marsh_GObject_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gdk_Event_Scroll_Boolean);

   procedure Marsh_GObject_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Boolean);

   procedure Marsh_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean);

   procedure Marsh_GObject_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Guint_Guint_Void);

   procedure Marsh_GObject_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_GObject_Void);

   procedure Marsh_Gtk_Status_Icon_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Icon_Gdk_Event_Button_Boolean);

   procedure Marsh_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean);

   procedure Marsh_Gtk_Status_Icon_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Icon_Gint_Boolean);

   procedure Marsh_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean);

   procedure Marsh_Gtk_Status_Icon_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Icon_Guint_Guint_Void);

   procedure Marsh_Gtk_Status_Icon_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address);
   pragma Convention (C, Marsh_Gtk_Status_Icon_Void);

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Icon_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Icon_Gdk_Event_Button_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Guint_Guint_Void;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Icon_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   -------------
   -- Connect --
   -------------

   procedure Connect
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_Gtk_Status_Icon_Gint_Boolean;
       After   : Boolean)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_Gtk_Status_Icon_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         After       => After);
   end Connect;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
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

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Button_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Button_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Guint_Guint_Void;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Guint_Guint_Void'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gdk_Event_Scroll_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gdk_Event_Scroll_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   ------------------
   -- Connect_Slot --
   ------------------

   procedure Connect_Slot
      (Object  : access Gtk_Status_Icon_Record'Class;
       C_Name  : Glib.Signal_Name;
       Handler : Cb_GObject_Gint_Boolean;
       After   : Boolean;
       Slot    : access Glib.Object.GObject_Record'Class := null)
   is
   begin
      Unchecked_Do_Signal_Connect
        (Object      => Object,
         C_Name      => C_Name,
         Marshaller  => Marsh_GObject_Gint_Boolean'Access,
         Handler     => Cb_To_Address (Handler),--  Set in the closure
         Slot_Object => Slot,
         After       => After);
   end Connect_Slot;

   --------------------------------------------
   -- Marsh_GObject_Gdk_Event_Button_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Button_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Button (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Button_Boolean;

   --------------------------------------------
   -- Marsh_GObject_Gdk_Event_Scroll_Boolean --
   --------------------------------------------

   procedure Marsh_GObject_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gdk_Event_Scroll_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Scroll (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gdk_Event_Scroll_Boolean;

   --------------------------------
   -- Marsh_GObject_Gint_Boolean --
   --------------------------------

   procedure Marsh_GObject_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Boolean;

   ---------------------------------------------------------
   -- Marsh_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean --
   ---------------------------------------------------------

   procedure Marsh_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3), Gtk.Tooltip.Gtk_Tooltip (Unchecked_To_Object (Params, 4)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;

   ------------------------------------
   -- Marsh_GObject_Guint_Guint_Void --
   ------------------------------------

   procedure Marsh_GObject_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_GObject_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Glib.Object.GObject := Glib.Object.Convert (Get_Data (Closure));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_GObject_Guint_Guint_Void;

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

   ----------------------------------------------------
   -- Marsh_Gtk_Status_Icon_Gdk_Event_Button_Boolean --
   ----------------------------------------------------

   procedure Marsh_Gtk_Status_Icon_Gdk_Event_Button_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Icon := Gtk_Status_Icon (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Button (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Icon_Gdk_Event_Button_Boolean;

   ----------------------------------------------------
   -- Marsh_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean --
   ----------------------------------------------------

   procedure Marsh_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Icon := Gtk_Status_Icon (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gdk_Event_Scroll (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean;

   ----------------------------------------
   -- Marsh_Gtk_Status_Icon_Gint_Boolean --
   ----------------------------------------

   procedure Marsh_Gtk_Status_Icon_Gint_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Icon_Gint_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Icon := Gtk_Status_Icon (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Icon_Gint_Boolean;

   -----------------------------------------------------------------
   -- Marsh_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean --
   -----------------------------------------------------------------

   procedure Marsh_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Icon := Gtk_Status_Icon (Unchecked_To_Object (Params, 0));
      V   : aliased Boolean := H (Obj, Unchecked_To_Gint (Params, 1), Unchecked_To_Gint (Params, 2), Unchecked_To_Boolean (Params, 3), Gtk.Tooltip.Gtk_Tooltip (Unchecked_To_Object (Params, 4)));
   begin
      Set_Value (Return_Value, V'Address);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;

   --------------------------------------------
   -- Marsh_Gtk_Status_Icon_Guint_Guint_Void --
   --------------------------------------------

   procedure Marsh_Gtk_Status_Icon_Guint_Guint_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Icon_Guint_Guint_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Icon := Gtk_Status_Icon (Unchecked_To_Object (Params, 0));
   begin
      H (Obj, Unchecked_To_Guint (Params, 1), Unchecked_To_Guint (Params, 2));
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Icon_Guint_Guint_Void;

   --------------------------------
   -- Marsh_Gtk_Status_Icon_Void --
   --------------------------------

   procedure Marsh_Gtk_Status_Icon_Void
      (Closure         : GClosure;
       Return_Value    : Glib.Values.GValue;
       N_Params        : Glib.Guint;
       Params          : Glib.Values.C_GValues;
       Invocation_Hint : System.Address;
       User_Data       : System.Address)
   is
      pragma Unreferenced (Return_Value, N_Params, Invocation_Hint, User_Data);
      H   : constant Cb_Gtk_Status_Icon_Void := Address_To_Cb (Get_Callback (Closure));
      Obj : constant Gtk_Status_Icon := Gtk_Status_Icon (Unchecked_To_Object (Params, 0));
   begin
      H (Obj);
      exception when E : others => Process_Exception (E);
   end Marsh_Gtk_Status_Icon_Void;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "activate" & ASCII.NUL, Call, After);
   end On_Activate;

   -----------------
   -- On_Activate --
   -----------------

   procedure On_Activate
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "activate" & ASCII.NUL, Call, After, Slot);
   end On_Activate;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   procedure On_Button_Press_Event
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "button-press-event" & ASCII.NUL, Call, After);
   end On_Button_Press_Event;

   ---------------------------
   -- On_Button_Press_Event --
   ---------------------------

   procedure On_Button_Press_Event
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Gdk_Event_Button_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "button-press-event" & ASCII.NUL, Call, After, Slot);
   end On_Button_Press_Event;

   -----------------------------
   -- On_Button_Release_Event --
   -----------------------------

   procedure On_Button_Release_Event
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Gdk_Event_Button_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "button-release-event" & ASCII.NUL, Call, After);
   end On_Button_Release_Event;

   -----------------------------
   -- On_Button_Release_Event --
   -----------------------------

   procedure On_Button_Release_Event
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Gdk_Event_Button_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "button-release-event" & ASCII.NUL, Call, After, Slot);
   end On_Button_Release_Event;

   -------------------
   -- On_Popup_Menu --
   -------------------

   procedure On_Popup_Menu
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Guint_Guint_Void;
       After : Boolean := False)
   is
   begin
      Connect (Self, "popup-menu" & ASCII.NUL, Call, After);
   end On_Popup_Menu;

   -------------------
   -- On_Popup_Menu --
   -------------------

   procedure On_Popup_Menu
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Guint_Guint_Void;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "popup-menu" & ASCII.NUL, Call, After, Slot);
   end On_Popup_Menu;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   procedure On_Query_Tooltip
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "query-tooltip" & ASCII.NUL, Call, After);
   end On_Query_Tooltip;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   procedure On_Query_Tooltip
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Gint_Gint_Boolean_Gtk_Tooltip_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "query-tooltip" & ASCII.NUL, Call, After, Slot);
   end On_Query_Tooltip;

   ---------------------
   -- On_Scroll_Event --
   ---------------------

   procedure On_Scroll_Event
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Gdk_Event_Scroll_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "scroll-event" & ASCII.NUL, Call, After);
   end On_Scroll_Event;

   ---------------------
   -- On_Scroll_Event --
   ---------------------

   procedure On_Scroll_Event
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Gdk_Event_Scroll_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "scroll-event" & ASCII.NUL, Call, After, Slot);
   end On_Scroll_Event;

   ---------------------
   -- On_Size_Changed --
   ---------------------

   procedure On_Size_Changed
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_Gtk_Status_Icon_Gint_Boolean;
       After : Boolean := False)
   is
   begin
      Connect (Self, "size-changed" & ASCII.NUL, Call, After);
   end On_Size_Changed;

   ---------------------
   -- On_Size_Changed --
   ---------------------

   procedure On_Size_Changed
      (Self  : not null access Gtk_Status_Icon_Record;
       Call  : Cb_GObject_Gint_Boolean;
       Slot  : not null access Glib.Object.GObject_Record'Class;
       After : Boolean := False)
   is
   begin
      Connect_Slot (Self, "size-changed" & ASCII.NUL, Call, After, Slot);
   end On_Size_Changed;

end Gtk.Status_Icon;
